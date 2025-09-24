;; Core Protocol Contract - Main entry point for streaming payments
;; Handles stream creation, management, and coordination with modules

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-STREAM (err u101))
(define-constant ERR-STREAM-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))
(define-constant ERR-MODULE-CALL-FAILED (err u104))

;; Data Variables
(define-data-var next-stream-id uint u1)

;; Added module contract addresses as data variables
(define-data-var token-module-contract principal tx-sender)
(define-data-var security-module-contract principal tx-sender)
(define-data-var curve-module-contract principal tx-sender)

;; Stream Data Structure
(define-map streams
  { stream-id: uint }
  {
    sender: principal,
    recipient: principal,
    token-contract: principal,
    total-amount: uint,
    amount-per-block: uint,
    start-block: uint,
    end-block: uint,
    curve-type: (string-ascii 20),
    status: (string-ascii 10),
    withdrawn-amount: uint,
    created-at: uint
  }
)

;; Stream index by sender
(define-map sender-streams
  { sender: principal, stream-id: uint }
  { active: bool }
)

;; Stream index by recipient
(define-map recipient-streams
  { recipient: principal, stream-id: uint }
  { active: bool }
)

;; Removed circular dependency by calculating rate locally instead of calling curve module
(define-private (calculate-rate-internal (total-amount uint) (duration-blocks uint) (curve-type (string-ascii 20)))
  (if (is-eq curve-type "linear")
    (ok (/ total-amount duration-blocks))
    (if (is-eq curve-type "exponential")
      (ok (/ (* total-amount u2) duration-blocks))
      (err u999)
    )
  )
)

;; Calculate available amount locally to avoid circular dependency
(define-private (calculate-available-internal (stream-data (tuple (sender principal) (recipient principal) (token-contract principal) (total-amount uint) (amount-per-block uint) (start-block uint) (end-block uint) (curve-type (string-ascii 20)) (status (string-ascii 10)) (withdrawn-amount uint) (created-at uint))) (current-block uint))
  (let (
    (start-block (get start-block stream-data))
    (end-block (get end-block stream-data))
    (total-amount (get total-amount stream-data))
    (withdrawn-amount (get withdrawn-amount stream-data))
    (curve-type (get curve-type stream-data))
  )
    (let ((elapsed-blocks (if (> current-block start-block) (- current-block start-block) u0)))
      (let ((total-available 
        (if (>= current-block end-block)
          total-amount
          (if (is-eq curve-type "linear")
            (/ (* total-amount elapsed-blocks) (- end-block start-block))
            (if (is-eq curve-type "exponential")
              (let ((progress (/ (* elapsed-blocks u100) (- end-block start-block))))
                (/ (* total-amount progress progress) u10000)
              )
              u0
            )
          )
        )))
        (if (> total-available withdrawn-amount) (- total-available withdrawn-amount) u0)
      )
    )
  )
)

;; Public Functions

;; Create a new payment stream
(define-public (create-stream 
  (recipient principal)
  (token-contract principal)
  (total-amount uint)
  (duration-blocks uint)
  (curve-type (string-ascii 20)))
  (let (
    (stream-id (var-get next-stream-id))
    (start-block block-height)
    (end-block (+ block-height duration-blocks))
  )
    ;; Use internal rate calculation instead of module call
    (let ((rate-result (try! (calculate-rate-internal total-amount duration-blocks curve-type))))
      
      ;; Create stream record
      (map-set streams
        { stream-id: stream-id }
        {
          sender: tx-sender,
          recipient: recipient,
          token-contract: token-contract,
          total-amount: total-amount,
          amount-per-block: rate-result,
          start-block: start-block,
          end-block: end-block,
          curve-type: curve-type,
          status: "active",
          withdrawn-amount: u0,
          created-at: block-height
        }
      )
      
      ;; Update indexes
      (map-set sender-streams { sender: tx-sender, stream-id: stream-id } { active: true })
      (map-set recipient-streams { recipient: recipient, stream-id: stream-id } { active: true })
      
      ;; Increment stream ID counter
      (var-set next-stream-id (+ stream-id u1))
      
      (ok stream-id)
    )
  )
)

;; Withdraw available funds from a stream
(define-public (withdraw (stream-id uint))
  (let ((stream-data (unwrap! (map-get? streams { stream-id: stream-id }) ERR-STREAM-NOT-FOUND)))
    ;; Verify caller is recipient
    (asserts! (is-eq tx-sender (get recipient stream-data)) ERR-UNAUTHORIZED)
    
    ;; Calculate available amount internally
    (let ((available-amount (calculate-available-internal stream-data block-height)))
      
      ;; Update stream withdrawn amount
      (map-set streams
        { stream-id: stream-id }
        (merge stream-data { withdrawn-amount: (+ (get withdrawn-amount stream-data) available-amount) })
      )
      
      (ok available-amount)
    )
  )
)

;; Cancel a stream
(define-public (cancel-stream (stream-id uint))
  (let ((stream-data (unwrap! (map-get? streams { stream-id: stream-id }) ERR-STREAM-NOT-FOUND)))
    ;; Verify caller is sender
    (asserts! (is-eq tx-sender (get sender stream-data)) ERR-UNAUTHORIZED)
    
    ;; Update stream status
    (map-set streams
      { stream-id: stream-id }
      (merge stream-data { status: "cancelled" })
    )
    
    (ok true)
  )
)

;; Read-only functions

;; Get stream details
(define-read-only (get-stream (stream-id uint))
  (map-get? streams { stream-id: stream-id })
)

;; Get available withdrawal amount
(define-read-only (get-available-amount (stream-id uint))
  (match (map-get? streams { stream-id: stream-id })
    stream-data (ok (calculate-available-internal stream-data block-height))
    (err ERR-STREAM-NOT-FOUND)
  )
)

;; Added admin functions to set module contract addresses
(define-public (set-token-module (contract-address principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set token-module-contract contract-address)
    (ok true)
  )
)

(define-public (set-security-module (contract-address principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set security-module-contract contract-address)
    (ok true)
  )
)

(define-public (set-curve-module (contract-address principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set curve-module-contract contract-address)
    (ok true)
  )
)
