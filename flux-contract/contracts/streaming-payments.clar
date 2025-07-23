;; Real-time Payment Streaming Contract
;; Supports precise calculations with different time intervals and streaming curves

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_STREAM_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_INVALID_PARAMETERS (err u103))
(define-constant ERR_STREAM_ALREADY_EXISTS (err u104))
(define-constant ERR_STREAM_ENDED (err u105))
(define-constant ERR_INVALID_CURVE_TYPE (err u106))

;; Precision multiplier to avoid rounding errors (10^6 for micro-precision)
(define-constant PRECISION_MULTIPLIER u1000000)

;; Time interval constants (in seconds)
(define-constant INTERVAL_SECOND u1)
(define-constant INTERVAL_MINUTE u60)
(define-constant INTERVAL_HOUR u3600)
(define-constant INTERVAL_DAY u86400)

;; Curve types
(define-constant CURVE_LINEAR u1)
(define-constant CURVE_EXPONENTIAL u2)
(define-constant CURVE_LOGARITHMIC u3)
(define-constant CURVE_SIGMOID u4)

;; Data structures
(define-map streams
  { stream-id: uint }
  {
    sender: principal,
    recipient: principal,
    total-amount: uint,
    rate-per-interval: uint,
    time-interval: uint,
    start-time: uint,
    end-time: uint,
    curve-type: uint,
    curve-parameter: uint,
    withdrawn-amount: uint,
    is-active: bool,
    created-at: uint
  }
)

(define-map stream-balances
  { stream-id: uint }
  { deposited-amount: uint }
)

;; Stream counter
(define-data-var next-stream-id uint u1)

;; Helper functions

;; Get current block time
(define-private (get-current-time)
  block-height ;; Using block height as time proxy
)

;; Calculate elapsed time since stream start
(define-private (calculate-elapsed-time (start-time uint))
  (let ((current-time (get-current-time)))
    (if (> current-time start-time)
      (- current-time start-time)
      u0
    )
  )
)

;; Apply precision to amount calculations
(define-private (apply-precision (amount uint))
  (* amount PRECISION_MULTIPLIER)
)

;; Remove precision from final calculations
(define-private (remove-precision (amount uint))
  (/ amount PRECISION_MULTIPLIER)
)

;; Linear streaming calculation
(define-private (calculate-linear-stream (elapsed-intervals uint) (rate-per-interval uint))
  (* elapsed-intervals rate-per-interval)
)

;; Exponential streaming calculation
(define-private (calculate-exponential-stream (elapsed-intervals uint) (rate-per-interval uint) (curve-param uint))
  (let (
    (base-amount (* elapsed-intervals rate-per-interval))
    (exponential-factor (pow curve-param elapsed-intervals))
  )
    (/ (* base-amount exponential-factor) (pow curve-param u10)) ;; Normalize
  )
)

;; Logarithmic streaming calculation
(define-private (calculate-logarithmic-stream (elapsed-intervals uint) (rate-per-interval uint) (curve-param uint))
  (let (
    (base-amount (* elapsed-intervals rate-per-interval))
    (log-factor (+ u1 (/ elapsed-intervals curve-param)))
  )
    (* base-amount log-factor)
  )
)

;; Sigmoid streaming calculation
(define-private (calculate-sigmoid-stream (elapsed-intervals uint) (rate-per-interval uint) (curve-param uint))
  (let (
    (base-amount (* elapsed-intervals rate-per-interval))
    (sigmoid-factor (/ elapsed-intervals (+ elapsed-intervals curve-param)))
  )
    (* base-amount sigmoid-factor)
  )
)

;; Calculate streamed amount based on curve type
(define-private (calculate-streamed-amount 
  (elapsed-time uint) 
  (time-interval uint) 
  (rate-per-interval uint) 
  (curve-type uint) 
  (curve-param uint)
  (total-amount uint)
)
  (let (
    (elapsed-intervals (/ elapsed-time time-interval))
    (precise-rate (apply-precision rate-per-interval))
    (calculated-amount 
      (if (is-eq curve-type CURVE_LINEAR)
        (calculate-linear-stream elapsed-intervals precise-rate)
        (if (is-eq curve-type CURVE_EXPONENTIAL)
          (calculate-exponential-stream elapsed-intervals precise-rate curve-param)
          (if (is-eq curve-type CURVE_LOGARITHMIC)
            (calculate-logarithmic-stream elapsed-intervals precise-rate curve-param)
            (if (is-eq curve-type CURVE_SIGMOID)
              (calculate-sigmoid-stream elapsed-intervals precise-rate curve-param)
              u0
            )
          )
        )
      )
    )
    (final-amount (remove-precision calculated-amount))
  )
    ;; Ensure we don't exceed total amount
    (if (> final-amount total-amount)
      total-amount
      final-amount
    )
  )
)

;; Public functions

;; Create a new payment stream
(define-public (create-stream
  (recipient principal)
  (total-amount uint)
  (rate-per-interval uint)
  (time-interval uint)
  (duration-intervals uint)
  (curve-type uint)
  (curve-parameter uint)
)
  (let (
    (stream-id (var-get next-stream-id))
    (current-time (get-current-time))
    (end-time (+ current-time (* duration-intervals time-interval)))
  )
    ;; Validate parameters
    (asserts! (> total-amount u0) ERR_INVALID_PARAMETERS)
    (asserts! (> rate-per-interval u0) ERR_INVALID_PARAMETERS)
    (asserts! (> duration-intervals u0) ERR_INVALID_PARAMETERS)
    (asserts! (or (is-eq curve-type CURVE_LINEAR)
                  (is-eq curve-type CURVE_EXPONENTIAL)
                  (is-eq curve-type CURVE_LOGARITHMIC)
                  (is-eq curve-type CURVE_SIGMOID)) ERR_INVALID_CURVE_TYPE)
    
    ;; Check if stream already exists
    (asserts! (is-none (map-get? streams { stream-id: stream-id })) ERR_STREAM_ALREADY_EXISTS)
    
    ;; Create stream record
    (map-set streams
      { stream-id: stream-id }
      {
        sender: tx-sender,
        recipient: recipient,
        total-amount: total-amount,
        rate-per-interval: rate-per-interval,
        time-interval: time-interval,
        start-time: current-time,
        end-time: end-time,
        curve-type: curve-type,
        curve-parameter: curve-parameter,
        withdrawn-amount: u0,
        is-active: true,
        created-at: current-time
      }
    )
    
    ;; Set initial balance
    (map-set stream-balances
      { stream-id: stream-id }
      { deposited-amount: total-amount }
    )
    
    ;; Increment stream counter
    (var-set next-stream-id (+ stream-id u1))
    
    (ok stream-id)
  )
)

;; Calculate available amount for withdrawal
(define-read-only (get-available-amount (stream-id uint))
  (match (map-get? streams { stream-id: stream-id })
    stream-data
    (let (
      (current-time (get-current-time))
      (elapsed-time (calculate-elapsed-time (get start-time stream-data)))
      (total-streamed 
        (if (>= current-time (get end-time stream-data))
          (get total-amount stream-data)
          (calculate-streamed-amount
            elapsed-time
            (get time-interval stream-data)
            (get rate-per-interval stream-data)
            (get curve-type stream-data)
            (get curve-parameter stream-data)
            (get total-amount stream-data)
          )
        )
      )
      (available-amount (- total-streamed (get withdrawn-amount stream-data)))
    )
      (ok available-amount)
    )
    ERR_STREAM_NOT_FOUND
  )
)

;; Withdraw available amount from stream
(define-public (withdraw-from-stream (stream-id uint) (amount uint))
  (match (map-get? streams { stream-id: stream-id })
    stream-data
    (let (
      (available-result (get-available-amount stream-id))
    )
      (match available-result
        available-amount
        (begin
          ;; Validate withdrawal
          (asserts! (is-eq tx-sender (get recipient stream-data)) ERR_UNAUTHORIZED)
          (asserts! (get is-active stream-data) ERR_STREAM_ENDED)
          (asserts! (<= amount available-amount) ERR_INSUFFICIENT_BALANCE)
          
          ;; Update withdrawn amount
          (map-set streams
            { stream-id: stream-id }
            (merge stream-data { withdrawn-amount: (+ (get withdrawn-amount stream-data) amount) })
          )
          
          ;; Transfer tokens
          (try! (stx-transfer? amount (get sender stream-data) tx-sender))
          
          (ok amount)
        )
        error-code (err error-code)
      )
    )
    ERR_STREAM_NOT_FOUND
  )
)

;; Get stream details
(define-read-only (get-stream-details (stream-id uint))
  (match (map-get? streams { stream-id: stream-id })
    stream-data
    (let (
      (available-result (get-available-amount stream-id))
      (current-time (get-current-time))
      (is-ended (>= current-time (get end-time stream-data)))
    )
      (match available-result
        available-amount
        (ok {
          stream-data: stream-data,
          available-amount: available-amount,
          is-ended: is-ended,
          current-time: current-time
        })
        error-code (err error-code)
      )
    )
    ERR_STREAM_NOT_FOUND
  )
)

;; Cancel stream (only sender can cancel)
(define-public (cancel-stream (stream-id uint))
  (match (map-get? streams { stream-id: stream-id })
    stream-data
    (begin
      (asserts! (is-eq tx-sender (get sender stream-data)) ERR_UNAUTHORIZED)
      (asserts! (get is-active stream-data) ERR_STREAM_ENDED)
      
      ;; Mark stream as inactive
      (map-set streams
        { stream-id: stream-id }
        (merge stream-data { is-active: false })
      )
      
      (ok true)
    )
    ERR_STREAM_NOT_FOUND
  )
)

;; Get stream rate information
(define-read-only (get-stream-rate-info (stream-id uint))
  (match (map-get? streams { stream-id: stream-id })
    stream-data
    (ok {
      rate-per-second: (if (is-eq (get time-interval stream-data) INTERVAL_SECOND)
                         (get rate-per-interval stream-data)
                         (/ (get rate-per-interval stream-data) (get time-interval stream-data))),
      rate-per-minute: (if (is-eq (get time-interval stream-data) INTERVAL_MINUTE)
                         (get rate-per-interval stream-data)
                         (/ (* (get rate-per-interval stream-data) INTERVAL_MINUTE) (get time-interval stream-data))),
      rate-per-hour: (if (is-eq (get time-interval stream-data) INTERVAL_HOUR)
                       (get rate-per-interval stream-data)
                       (/ (* (get rate-per-interval stream-data) INTERVAL_HOUR) (get time-interval stream-data))),
      rate-per-day: (if (is-eq (get time-interval stream-data) INTERVAL_DAY)
                      (get rate-per-interval stream-data)
                      (/ (* (get rate-per-interval stream-data) INTERVAL_DAY) (get time-interval stream-data)))
    })
    ERR_STREAM_NOT_FOUND
  )
)

;; Get all active streams for a recipient
(define-read-only (get-recipient-streams (recipient principal))
  ;; This would require additional data structures to efficiently query
  ;; For now, return a placeholder
  (ok u0)
)

;; Administrative functions

;; Emergency pause (contract owner only)
(define-public (emergency-pause-stream (stream-id uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (match (map-get? streams { stream-id: stream-id })
      stream-data
      (begin
        (map-set streams
          { stream-id: stream-id }
          (merge stream-data { is-active: false })
        )
        (ok true)
      )
      ERR_STREAM_NOT_FOUND
    )
  )
)
