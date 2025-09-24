;; Security Module - Handles escrow, withdrawals, and emergency controls

;; Define SIP-010 trait directly instead of importing
(define-trait sip-010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 32) uint))
    (get-decimals () (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-token-uri () (response (optional (string-utf8 256)) uint))
  )
)

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u200))
(define-constant ERR-INSUFFICIENT-FUNDS (err u201))
(define-constant ERR-TRANSFER-FAILED (err u202))
(define-constant ERR-INVALID-STREAM (err u203))

;; Data Variables
(define-data-var emergency-pause bool false)
(define-data-var core-contract principal .core-protocol)

;; Escrow tracking
(define-map escrowed-funds
  { stream-id: uint }
  {
    sender: principal,
    token-contract: principal,
    total-amount: uint,
    remaining-amount: uint,
    escrowed-at: uint
  }
)

;; Emergency controls
(define-map emergency-withdrawals
  { stream-id: uint }
  { allowed: bool, executed: bool }
)

;; Public Functions

;; Fixed escrow-funds to use trait parameter instead of principal
(define-public (escrow-funds (sender principal) (token-contract <sip-010-trait>) (amount uint) (stream-id uint))
  (begin
    ;; Only core contract can call this
    (asserts! (is-eq tx-sender (var-get core-contract)) ERR-UNAUTHORIZED)
    (asserts! (not (var-get emergency-pause)) ERR-UNAUTHORIZED)
    
    ;; Transfer tokens to escrow
    (try! (contract-call? token-contract transfer amount sender (as-contract tx-sender) (some 0x73747265616d2d657363726f77)))
    
    ;; Record escrow
    (map-set escrowed-funds
      { stream-id: stream-id }
      {
        sender: sender,
        token-contract: (contract-of token-contract),
        total-amount: amount,
        remaining-amount: amount,
        escrowed-at: block-height
      }
    )
    
    (ok true)
  )
)

;; Fixed release-funds to use trait parameter instead of principal
(define-public (release-funds (recipient principal) (token-contract <sip-010-trait>) (amount uint) (stream-id uint))
  (let ((escrow-data (unwrap! (map-get? escrowed-funds { stream-id: stream-id }) ERR-INVALID-STREAM)))
    ;; Only core contract can call this
    (asserts! (is-eq tx-sender (var-get core-contract)) ERR-UNAUTHORIZED)
    (asserts! (not (var-get emergency-pause)) ERR-UNAUTHORIZED)
    (asserts! (<= amount (get remaining-amount escrow-data)) ERR-INSUFFICIENT-FUNDS)
    
    ;; Transfer tokens from escrow to recipient
    (try! (as-contract (contract-call? token-contract transfer amount tx-sender recipient (some 0x73747265616d2d77697468647261776c))))
    
    ;; Update remaining amount
    (map-set escrowed-funds
      { stream-id: stream-id }
      (merge escrow-data { remaining-amount: (- (get remaining-amount escrow-data) amount) })
    )
    
    (ok true)
  )
)

;; Fixed cancel-stream function to use trait properly
(define-public (cancel-stream (stream-id uint) (token-contract <sip-010-trait>))
  (let ((escrow-data (unwrap! (map-get? escrowed-funds { stream-id: stream-id }) ERR-INVALID-STREAM)))
    ;; Only core contract can call this
    (asserts! (is-eq tx-sender (var-get core-contract)) ERR-UNAUTHORIZED)
    
    ;; Return remaining funds to sender using the trait
    (try! (as-contract (contract-call? token-contract transfer 
      (get remaining-amount escrow-data) 
      tx-sender 
      (get sender escrow-data) 
      (some 0x73747265616d2d63616e63656c6c6174696f6e))))
    
    ;; Clear escrow record
    (map-delete escrowed-funds { stream-id: stream-id })
    
    (ok true)
  )
)

;; Emergency functions

;; Simplified emergency pause to avoid interdependency
(define-public (set-emergency-pause (paused bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set emergency-pause paused)
    (ok true)
  )
)

;; Fixed emergency withdrawal to use trait properly
(define-public (emergency-withdraw (stream-id uint) (recipient principal) (token-contract <sip-010-trait>))
  (let ((escrow-data (unwrap! (map-get? escrowed-funds { stream-id: stream-id }) ERR-INVALID-STREAM)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    
    ;; Transfer remaining funds to recipient using the trait
    (try! (as-contract (contract-call? token-contract transfer 
      (get remaining-amount escrow-data) 
      tx-sender 
      recipient 
      (some 0x656d657267656e63792d77697468647261776c))))
    
    ;; Mark as emergency withdrawn
    (map-set emergency-withdrawals { stream-id: stream-id } { allowed: true, executed: true })
    
    ;; Clear escrow record
    (map-delete escrowed-funds { stream-id: stream-id })
    
    (ok true)
  )
)

;; Read-only functions

;; Get escrow details
(define-read-only (get-escrow-details (stream-id uint))
  (map-get? escrowed-funds { stream-id: stream-id })
)

;; Check if emergency paused
(define-read-only (is-emergency-paused)
  (var-get emergency-pause)
)

;; Admin functions

;; Update core contract reference (only owner)
(define-public (update-core-contract (new-core principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set core-contract new-core)
    (ok true)
  )
)
