;; Payment Stream Security Contract
;; Handles escrow, withdrawals, refunds, and emergency controls

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_STREAM_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_FUNDS (err u102))
(define-constant ERR_STREAM_ALREADY_EXISTS (err u103))
(define-constant ERR_STREAM_CANCELLED (err u104))
(define-constant ERR_STREAM_COMPLETED (err u105))
(define-constant ERR_NOTHING_TO_WITHDRAW (err u106))
(define-constant ERR_CONTRACT_PAUSED (err u107))
(define-constant ERR_INVALID_RECIPIENT (err u108))
(define-constant ERR_INVALID_AMOUNT (err u109))
(define-constant ERR_INVALID_DURATION (err u110))

;; Data Variables
(define-data-var contract-paused bool false)
(define-data-var stream-counter uint u0)

;; Stream Status Enum
(define-constant STREAM_ACTIVE u1)
(define-constant STREAM_CANCELLED u2)
(define-constant STREAM_COMPLETED u3)

;; Data Maps
(define-map streams
  { stream-id: uint }
  {
    sender: principal,
    recipient: principal,
    total-amount: uint,
    withdrawn-amount: uint,
    start-time: uint,
    end-time: uint,
    status: uint,
    created-at: uint
  }
)

(define-map stream-balances
  { stream-id: uint }
  { escrowed-amount: uint }
)

;; Emergency pause controls
(define-map authorized-pausers
  { pauser: principal }
  { authorized: bool }
)

;; Read-only functions

(define-read-only (get-stream (stream-id uint))
  (map-get? streams { stream-id: stream-id })
)

(define-read-only (get-stream-balance (stream-id uint))
  (map-get? stream-balances { stream-id: stream-id })
)

(define-read-only (is-contract-paused)
  (var-get contract-paused)
)

(define-read-only (get-current-stream-counter)
  (var-get stream-counter)
)

(define-read-only (calculate-withdrawable-amount (stream-id uint))
  (let (
    (stream-data (unwrap! (get-stream stream-id) (err u0)))
    (current-time (unwrap-panic (get-block-info? time block-height)))
    (start-time (get start-time stream-data))
    (end-time (get end-time stream-data))
    (total-amount (get total-amount stream-data))
    (withdrawn-amount (get withdrawn-amount stream-data))
    (stream-status (get status stream-data))
  )
    (if (or (is-eq stream-status STREAM_CANCELLED) (is-eq stream-status STREAM_COMPLETED))
      (ok u0)
      (if (<= current-time start-time)
        (ok u0)
        (if (>= current-time end-time)
          (ok (- total-amount withdrawn-amount))
          (let (
            (elapsed-time (- current-time start-time))
            (total-duration (- end-time start-time))
            (streamed-amount (/ (* total-amount elapsed-time) total-duration))
          )
            (ok (- streamed-amount withdrawn-amount))
          )
        )
      )
    )
  )
)

;; Private functions

(define-private (is-authorized-pauser (pauser principal))
  (default-to false (get authorized (map-get? authorized-pausers { pauser: pauser })))
)

(define-private (transfer-stx-to-contract (amount uint) (sender principal))
  (stx-transfer? amount sender (as-contract tx-sender))
)

(define-private (transfer-stx-from-contract (amount uint) (recipient principal))
  (as-contract (stx-transfer? amount tx-sender recipient))
)

;; Public functions

;; Create a new payment stream with escrow
(define-public (create-stream (recipient principal) (total-amount uint) (duration uint))
  (let (
    (stream-id (+ (var-get stream-counter) u1))
    (current-time (unwrap-panic (get-block-info? time block-height)))
    (end-time (+ current-time duration))
  )
    (asserts! (not (is-contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (not (is-eq recipient tx-sender)) ERR_INVALID_RECIPIENT)
    (asserts! (> total-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> duration u0) ERR_INVALID_DURATION)
    (asserts! (is-none (get-stream stream-id)) ERR_STREAM_ALREADY_EXISTS)
    
    ;; Transfer funds to contract (escrow)
    (try! (transfer-stx-to-contract total-amount tx-sender))
    
    ;; Create stream record
    (map-set streams
      { stream-id: stream-id }
      {
        sender: tx-sender,
        recipient: recipient,
        total-amount: total-amount,
        withdrawn-amount: u0,
        start-time: current-time,
        end-time: end-time,
        status: STREAM_ACTIVE,
        created-at: current-time
      }
    )
    
    ;; Set escrow balance
    (map-set stream-balances
      { stream-id: stream-id }
      { escrowed-amount: total-amount }
    )
    
    ;; Update counter
    (var-set stream-counter stream-id)
    
    (ok stream-id)
  )
)

;; Withdraw available streamed funds
(define-public (withdraw-from-stream (stream-id uint))
  (let (
    (stream-data (unwrap! (get-stream stream-id) ERR_STREAM_NOT_FOUND))
    (withdrawable-amount (unwrap! (calculate-withdrawable-amount stream-id) ERR_STREAM_NOT_FOUND))
    (recipient (get recipient stream-data))
    (current-withdrawn (get withdrawn-amount stream-data))
  )
    (asserts! (not (is-contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (is-eq tx-sender recipient) ERR_UNAUTHORIZED)
    (asserts! (> withdrawable-amount u0) ERR_NOTHING_TO_WITHDRAW)
    (asserts! (is-eq (get status stream-data) STREAM_ACTIVE) ERR_STREAM_CANCELLED)
    
    ;; Transfer funds to recipient
    (try! (transfer-stx-from-contract withdrawable-amount recipient))
    
    ;; Update withdrawn amount
    (map-set streams
      { stream-id: stream-id }
      (merge stream-data { withdrawn-amount: (+ current-withdrawn withdrawable-amount) })
    )
    
    ;; Check if stream is completed
    (let (
      (new-withdrawn (+ current-withdrawn withdrawable-amount))
      (total-amount (get total-amount stream-data))
    )
      (if (is-eq new-withdrawn total-amount)
        (map-set streams
          { stream-id: stream-id }
          (merge stream-data { 
            withdrawn-amount: new-withdrawn,
            status: STREAM_COMPLETED 
          })
        )
        true
      )
    )
    
    (ok withdrawable-amount)
  )
)

;; Cancel stream and refund remaining funds to sender
(define-public (cancel-stream (stream-id uint))
  (let (
    (stream-data (unwrap! (get-stream stream-id) ERR_STREAM_NOT_FOUND))
    (sender (get sender stream-data))
    (total-amount (get total-amount stream-data))
    (withdrawn-amount (get withdrawn-amount stream-data))
    (refund-amount (- total-amount withdrawn-amount))
  )
    (asserts! (not (is-contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (is-eq tx-sender sender) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status stream-data) STREAM_ACTIVE) ERR_STREAM_CANCELLED)
    (asserts! (> refund-amount u0) ERR_NOTHING_TO_WITHDRAW)
    
    ;; Transfer remaining funds back to sender
    (try! (transfer-stx-from-contract refund-amount sender))
    
    ;; Mark stream as cancelled
    (map-set streams
      { stream-id: stream-id }
      (merge stream-data { status: STREAM_CANCELLED })
    )
    
    ;; Update escrow balance
    (map-set stream-balances
      { stream-id: stream-id }
      { escrowed-amount: u0 }
    )
    
    (ok refund-amount)
  )
)

;; Emergency functions (only authorized pausers)

;; Pause the entire contract
(define-public (emergency-pause)
  (begin
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-authorized-pauser tx-sender)) ERR_UNAUTHORIZED)
    (var-set contract-paused true)
    (ok true)
  )
)

;; Unpause the contract
(define-public (emergency-unpause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set contract-paused false)
    (ok true)
  )
)

;; Emergency cancel stream (for dispute resolution)
(define-public (emergency-cancel-stream (stream-id uint))
  (let (
    (stream-data (unwrap! (get-stream stream-id) ERR_STREAM_NOT_FOUND))
    (sender (get sender stream-data))
    (recipient (get recipient stream-data))
    (total-amount (get total-amount stream-data))
    (withdrawn-amount (get withdrawn-amount stream-data))
    (remaining-amount (- total-amount withdrawn-amount))
    (withdrawable-amount (unwrap! (calculate-withdrawable-amount stream-id) ERR_STREAM_NOT_FOUND))
    (refund-amount (- remaining-amount withdrawable-amount))
  )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-authorized-pauser tx-sender)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status stream-data) STREAM_ACTIVE) ERR_STREAM_CANCELLED)
    
    ;; Transfer withdrawable amount to recipient
    (if (> withdrawable-amount u0)
      (try! (transfer-stx-from-contract withdrawable-amount recipient))
      true
    )
    
    ;; Transfer remaining amount back to sender
    (if (> refund-amount u0)
      (try! (transfer-stx-from-contract refund-amount sender))
      true
    )
    
    ;; Mark stream as cancelled
    (map-set streams
      { stream-id: stream-id }
      (merge stream-data { 
        status: STREAM_CANCELLED,
        withdrawn-amount: (+ withdrawn-amount withdrawable-amount)
      })
    )
    
    ;; Update escrow balance
    (map-set stream-balances
      { stream-id: stream-id }
      { escrowed-amount: u0 }
    )
    
    (ok { withdrawn: withdrawable-amount, refunded: refund-amount })
  )
)

;; Admin functions

;; Add authorized pauser
(define-public (add-authorized-pauser (pauser principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-pausers { pauser: pauser } { authorized: true })
    (ok true)
  )
)

;; Remove authorized pauser
(define-public (remove-authorized-pauser (pauser principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-delete authorized-pausers { pauser: pauser })
    (ok true)
  )
)

;; Get contract stats
(define-read-only (get-contract-stats)
  {
    total-streams: (var-get stream-counter),
    contract-paused: (var-get contract-paused),
    contract-owner: CONTRACT_OWNER
  }
)
