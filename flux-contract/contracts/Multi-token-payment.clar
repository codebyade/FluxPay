;; Multi-Token Payment Streaming Contract
;; Supports STX, SIP-010 tokens, atomic swaps, and token management

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_STREAM_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_STREAM_ALREADY_EXISTS (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))
(define-constant ERR_INVALID_DURATION (err u105))
(define-constant ERR_TOKEN_NOT_WHITELISTED (err u106))
(define-constant ERR_TOKEN_BLACKLISTED (err u107))
(define-constant ERR_STREAM_ENDED (err u108))
(define-constant ERR_SWAP_FAILED (err u109))
(define-constant ERR_INVALID_TOKEN (err u110))

;; Data Variables
(define-data-var stream-id-nonce uint u0)
(define-data-var contract-paused bool false)

;; Token Management Maps
(define-map whitelisted-tokens principal bool)
(define-map blacklisted-tokens principal bool)

;; Stream Data Structure
(define-map streams
  uint
  {
    sender: principal,
    recipient: principal,
    token-contract: (optional principal),
    total-amount: uint,
    amount-per-block: uint,
    start-block: uint,
    end-block: uint,
    withdrawn-amount: uint,
    is-active: bool,
    allow-swaps: bool,
    is-stx: bool
  }
)

;; User Stream Lists
(define-map user-outgoing-streams principal (list 50 uint))
(define-map user-incoming-streams principal (list 50 uint))

;; Atomic Swap Structures
(define-map swap-proposals
  uint
  {
    stream-id: uint,
    proposer: principal,
    from-token: (optional principal),
    to-token: (optional principal),
    exchange-rate: uint,
    expiry-block: uint,
    is-active: bool
  }
)

(define-data-var swap-id-nonce uint u0)

;; SIP-010 Token Trait
(define-trait sip010-token
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 10) uint))
    (get-decimals () (response uint uint))
  )
)

;; Authorization Functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT_OWNER)
)

(define-private (is-stream-participant (stream-id uint))
  (let ((stream-data (unwrap! (map-get? streams stream-id) false)))
    (or 
      (is-eq tx-sender (get sender stream-data))
      (is-eq tx-sender (get recipient stream-data))
    )
  )
)

;; Token Management Functions
(define-public (whitelist-token (token-contract principal))
  (begin
    (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
    (map-set whitelisted-tokens token-contract true)
    (ok true)
  )
)

(define-public (blacklist-token (token-contract principal))
  (begin
    (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
    (map-set blacklisted-tokens token-contract true)
    (ok true)
  )
)

(define-public (remove-from-whitelist (token-contract principal))
  (begin
    (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
    (map-delete whitelisted-tokens token-contract)
    (ok true)
  )
)

(define-public (remove-from-blacklist (token-contract principal))
  (begin
    (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
    (map-delete blacklisted-tokens token-contract)
    (ok true)
  )
)

(define-private (is-token-allowed (token-contract (optional principal)))
  (match token-contract
    some-token
    (and
      (default-to false (map-get? whitelisted-tokens some-token))
      (not (default-to false (map-get? blacklisted-tokens some-token)))
    )
    true ;; STX is always allowed
  )
)

;; Stream Creation Functions
(define-public (create-stx-stream 
  (recipient principal)
  (total-amount uint)
  (duration-blocks uint)
  (allow-swaps bool)
)
  (let (
    (stream-id (var-get stream-id-nonce))
    (amount-per-block (/ total-amount duration-blocks))
    (start-block stacks-block-height)
    (end-block (+ stacks-block-height duration-blocks))
  )
    (asserts! (> total-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> duration-blocks u0) ERR_INVALID_DURATION)
    (asserts! (>= (stx-get-balance tx-sender) total-amount) ERR_INSUFFICIENT_BALANCE)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    
    ;; Create stream record
    (map-set streams stream-id {
      sender: tx-sender,
      recipient: recipient,
      token-contract: none,
      total-amount: total-amount,
      amount-per-block: amount-per-block,
      start-block: start-block,
      end-block: end-block,
      withdrawn-amount: u0,
      is-active: true,
      allow-swaps: allow-swaps,
      is-stx: true
    })
    
    ;; Update user stream lists
    (update-user-streams tx-sender recipient stream-id)
    
    ;; Increment nonce
    (var-set stream-id-nonce (+ stream-id u1))
    
    (ok stream-id)
  )
)

(define-public (create-token-stream
  (token-contract <sip010-token>)
  (recipient principal)
  (total-amount uint)
  (duration-blocks uint)
  (allow-swaps bool)
)
  (let (
    (stream-id (var-get stream-id-nonce))
    (amount-per-block (/ total-amount duration-blocks))
    (start-block stacks-block-height)
    (end-block (+ stacks-block-height duration-blocks))
    (token-principal (contract-of token-contract))
  )
    (asserts! (> total-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> duration-blocks u0) ERR_INVALID_DURATION)
    (asserts! (is-token-allowed (some token-principal)) ERR_TOKEN_NOT_WHITELISTED)
    (asserts! (not (default-to false (map-get? blacklisted-tokens token-principal))) ERR_TOKEN_BLACKLISTED)
    
    ;; Check balance and transfer tokens to contract
    (let ((sender-balance (unwrap! (contract-call? token-contract get-balance tx-sender) ERR_INVALID_TOKEN)))
      (asserts! (>= sender-balance total-amount) ERR_INSUFFICIENT_BALANCE)
      (try! (contract-call? token-contract transfer total-amount tx-sender (as-contract tx-sender) none))
    )
    
    ;; Create stream record
    (map-set streams stream-id {
      sender: tx-sender,
      recipient: recipient,
      token-contract: (some token-principal),
      total-amount: total-amount,
      amount-per-block: amount-per-block,
      start-block: start-block,
      end-block: end-block,
      withdrawn-amount: u0,
      is-active: true,
      allow-swaps: allow-swaps,
      is-stx: false
    })
    
    ;; Update user stream lists
    (update-user-streams tx-sender recipient stream-id)
    
    ;; Increment nonce
    (var-set stream-id-nonce (+ stream-id u1))
    
    (ok stream-id)
  )
)

;; Stream Management Functions
(define-private (update-user-streams (sender principal) (recipient principal) (stream-id uint))
  (let (
    (sender-streams (default-to (list) (map-get? user-outgoing-streams sender)))
    (recipient-streams (default-to (list) (map-get? user-incoming-streams recipient)))
  )
    (map-set user-outgoing-streams sender (unwrap-panic (as-max-len? (append sender-streams stream-id) u50)))
    (map-set user-incoming-streams recipient (unwrap-panic (as-max-len? (append recipient-streams stream-id) u50)))
  )
)

(define-private (calculate-withdrawable-amount (stream-id uint))
  (let ((stream-data (unwrap! (map-get? streams stream-id) u0)))
    (let (
      (blocks-elapsed (- stacks-block-height (get start-block stream-data)))
      (total-blocks (- (get end-block stream-data) (get start-block stream-data)))
      (amount-per-block (get amount-per-block stream-data))
      (withdrawn (get withdrawn-amount stream-data))
    )
      (if (<= stacks-block-height (get end-block stream-data))
        (- (* blocks-elapsed amount-per-block) withdrawn)
        (- (get total-amount stream-data) withdrawn)
      )
    )
  )
)

(define-public (withdraw-from-stx-stream (stream-id uint))
  (let ((stream-data (unwrap! (map-get? streams stream-id) ERR_STREAM_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get recipient stream-data)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active stream-data) ERR_STREAM_ENDED)
    (asserts! (get is-stx stream-data) ERR_INVALID_TOKEN)
    
    (let ((withdrawable-amount (calculate-withdrawable-amount stream-id)))
      (asserts! (> withdrawable-amount u0) ERR_INSUFFICIENT_BALANCE)
      
      ;; Transfer STX
      (try! (as-contract (stx-transfer? withdrawable-amount tx-sender (get recipient stream-data))))
      
      ;; Update withdrawn amount
      (map-set streams stream-id 
        (merge stream-data {withdrawn-amount: (+ (get withdrawn-amount stream-data) withdrawable-amount)})
      )
      
      (ok withdrawable-amount)
    )
  )
)

(define-public (withdraw-from-token-stream (stream-id uint) (token-contract <sip010-token>))
  (let ((stream-data (unwrap! (map-get? streams stream-id) ERR_STREAM_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get recipient stream-data)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active stream-data) ERR_STREAM_ENDED)
    (asserts! (not (get is-stx stream-data)) ERR_INVALID_TOKEN)
    (asserts! (is-eq (some (contract-of token-contract)) (get token-contract stream-data)) ERR_INVALID_TOKEN)
    
    (let ((withdrawable-amount (calculate-withdrawable-amount stream-id)))
      (asserts! (> withdrawable-amount u0) ERR_INSUFFICIENT_BALANCE)
      
      ;; Transfer tokens
      (try! (as-contract (contract-call? token-contract transfer withdrawable-amount tx-sender (get recipient stream-data) none)))
      
      ;; Update withdrawn amount
      (map-set streams stream-id 
        (merge stream-data {withdrawn-amount: (+ (get withdrawn-amount stream-data) withdrawable-amount)})
      )
      
      (ok withdrawable-amount)
    )
  )
)

(define-public (cancel-stx-stream (stream-id uint))
  (let ((stream-data (unwrap! (map-get? streams stream-id) ERR_STREAM_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get sender stream-data)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active stream-data) ERR_STREAM_ENDED)
    (asserts! (get is-stx stream-data) ERR_INVALID_TOKEN)
    
    (let (
      (withdrawable-amount (calculate-withdrawable-amount stream-id))
      (remaining-amount (- (get total-amount stream-data) (get withdrawn-amount stream-data) withdrawable-amount))
    )
      ;; Transfer withdrawable amount to recipient
      (if (> withdrawable-amount u0)
        (try! (as-contract (stx-transfer? withdrawable-amount tx-sender (get recipient stream-data))))
        true
      )
      
      ;; Return remaining amount to sender
      (if (> remaining-amount u0)
        (try! (as-contract (stx-transfer? remaining-amount tx-sender (get sender stream-data))))
        true
      )
      
      ;; Mark stream as inactive
      (map-set streams stream-id (merge stream-data {is-active: false}))
      
      (ok true)
    )
  )
)

(define-public (cancel-token-stream (stream-id uint) (token-contract <sip010-token>))
  (let ((stream-data (unwrap! (map-get? streams stream-id) ERR_STREAM_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get sender stream-data)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active stream-data) ERR_STREAM_ENDED)
    (asserts! (not (get is-stx stream-data)) ERR_INVALID_TOKEN)
    (asserts! (is-eq (some (contract-of token-contract)) (get token-contract stream-data)) ERR_INVALID_TOKEN)
    
    (let (
      (withdrawable-amount (calculate-withdrawable-amount stream-id))
      (remaining-amount (- (get total-amount stream-data) (get withdrawn-amount stream-data) withdrawable-amount))
    )
      ;; Transfer withdrawable amount to recipient
      (if (> withdrawable-amount u0)
        (try! (as-contract (contract-call? token-contract transfer withdrawable-amount tx-sender (get recipient stream-data) none)))
        true
      )
      
      ;; Return remaining amount to sender
      (if (> remaining-amount u0)
        (try! (as-contract (contract-call? token-contract transfer remaining-amount tx-sender (get sender stream-data) none)))
        true
      )
      
      ;; Mark stream as inactive
      (map-set streams stream-id (merge stream-data {is-active: false}))
      
      (ok true)
    )
  )
)

;; Atomic Swap Functions
(define-public (propose-swap
  (stream-id uint)
  (from-token (optional principal))
  (to-token (optional principal))
  (exchange-rate uint)
  (expiry-blocks uint)
)
  (let (
    (stream-data (unwrap! (map-get? streams stream-id) ERR_STREAM_NOT_FOUND))
    (swap-id (var-get swap-id-nonce))
  )
    (asserts! (is-stream-participant stream-id) ERR_NOT_AUTHORIZED)
    (asserts! (get allow-swaps stream-data) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active stream-data) ERR_STREAM_ENDED)
    (asserts! (> exchange-rate u0) ERR_INVALID_AMOUNT)
    
    (map-set swap-proposals swap-id {
      stream-id: stream-id,
      proposer: tx-sender,
      from-token: from-token,
      to-token: to-token,
      exchange-rate: exchange-rate,
      expiry-block: (+ stacks-block-height expiry-blocks),
      is-active: true
    })
    
    (var-set swap-id-nonce (+ swap-id u1))
    (ok swap-id)
  )
)

(define-public (execute-swap (swap-id uint))
  (let (
    (swap-data (unwrap! (map-get? swap-proposals swap-id) ERR_STREAM_NOT_FOUND))
    (stream-data (unwrap! (map-get? streams (get stream-id swap-data)) ERR_STREAM_NOT_FOUND))
  )
    (asserts! (get is-active swap-data) ERR_SWAP_FAILED)
    (asserts! (< stacks-block-height (get expiry-block swap-data)) ERR_SWAP_FAILED)
    (asserts! (is-stream-participant (get stream-id swap-data)) ERR_NOT_AUTHORIZED)
    (asserts! (not (is-eq tx-sender (get proposer swap-data))) ERR_NOT_AUTHORIZED)
    
    ;; Mark swap as executed
    (map-set swap-proposals swap-id (merge swap-data {is-active: false}))
    
    ;; Update stream token contract
    (map-set streams (get stream-id swap-data) 
      (merge stream-data {token-contract: (get to-token swap-data)})
    )
    
    (ok true)
  )
)

;; Read-only Functions
(define-read-only (get-stream-info (stream-id uint))
  (map-get? streams stream-id)
)

(define-read-only (get-withdrawable-amount (stream-id uint))
  (calculate-withdrawable-amount stream-id)
)

(define-read-only (get-user-outgoing-streams (user principal))
  (default-to (list) (map-get? user-outgoing-streams user))
)

(define-read-only (get-user-incoming-streams (user principal))
  (default-to (list) (map-get? user-incoming-streams user))
)

(define-read-only (is-token-whitelisted (token-contract principal))
  (default-to false (map-get? whitelisted-tokens token-contract))
)

(define-read-only (is-token-blacklisted (token-contract principal))
  (default-to false (map-get? blacklisted-tokens token-contract))
)

(define-read-only (get-swap-proposal (swap-id uint))
  (map-get? swap-proposals swap-id)
)

(define-read-only (get-stream-count)
  (var-get stream-id-nonce)
)

;; Admin Functions
(define-public (pause-contract)
  (begin
    (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
    (var-set contract-paused true)
    (ok true)
  )
)

(define-public (resume-contract)
  (begin
    (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
    (var-set contract-paused false)
    (ok true)
  )
)