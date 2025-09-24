;; Curve Module - Provides advanced streaming rate calculations

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u400))
(define-constant ERR-INVALID-CURVE (err u401))
(define-constant ERR-INVALID-PARAMETERS (err u402))
(define-constant ERR-STREAM-NOT-FOUND (err u403))

;; Data Variables for authorized callers
(define-data-var authorized-contracts (list 10 principal) (list))

;; Curve configurations
(define-map curve-configs
  { curve-type: (string-ascii 20) }
  {
    enabled: bool,
    min-duration: uint,
    max-duration: uint,
    parameters: (list 5 uint)
  }
)

;; Stream curve data
(define-map stream-curves
  { stream-id: uint }
  {
    curve-type: (string-ascii 20),
    total-amount: uint,
    duration-blocks: uint,
    rate-per-block: uint,
    curve-parameters: (list 5 uint)
  }
)

;; Initialize default curves
(map-set curve-configs
  { curve-type: "linear" }
  { enabled: true, min-duration: u1, max-duration: u1000000, parameters: (list) }
)

(map-set curve-configs
  { curve-type: "exponential" }
  { enabled: true, min-duration: u100, max-duration: u500000, parameters: (list u2) }
)

;; Public Functions

;; Calculate streaming rate based on curve type
(define-public (calculate-rate (total-amount uint) (duration-blocks uint) (curve-type (string-ascii 20)))
  (let ((curve-config (unwrap! (map-get? curve-configs { curve-type: curve-type }) ERR-INVALID-CURVE)))
    ;; Check if caller is in authorized contracts list
    (asserts! (is-some (index-of (var-get authorized-contracts) tx-sender)) ERR-UNAUTHORIZED)
    (asserts! (get enabled curve-config) ERR-INVALID-CURVE)
    (asserts! (>= duration-blocks (get min-duration curve-config)) ERR-INVALID-PARAMETERS)
    (asserts! (<= duration-blocks (get max-duration curve-config)) ERR-INVALID-PARAMETERS)
    
    ;; Calculate rate based on curve type
    (if (is-eq curve-type "linear")
      (ok (/ total-amount duration-blocks))
      (if (is-eq curve-type "exponential")
        ;; Exponential curve: starts slow, accelerates
        (ok (/ (* total-amount u2) duration-blocks))
        ERR-INVALID-CURVE
      )
    )
  )
)

;; Store curve data for a stream
(define-public (store-curve-data (stream-id uint) (curve-type (string-ascii 20)) (total-amount uint) (duration-blocks uint) (rate-per-block uint))
  (begin
    ;; Check if caller is in authorized contracts list
    (asserts! (is-some (index-of (var-get authorized-contracts) tx-sender)) ERR-UNAUTHORIZED)
    
    (map-set stream-curves
      { stream-id: stream-id }
      {
        curve-type: curve-type,
        total-amount: total-amount,
        duration-blocks: duration-blocks,
        rate-per-block: rate-per-block,
        curve-parameters: (list)
      }
    )
    
    (ok true)
  )
)

;; Admin Functions

;; Add or update curve configuration (only owner)
(define-public (configure-curve (curve-type (string-ascii 20)) (enabled bool) (min-duration uint) (max-duration uint) (parameters (list 5 uint)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    
    (map-set curve-configs
      { curve-type: curve-type }
      {
        enabled: enabled,
        min-duration: min-duration,
        max-duration: max-duration,
        parameters: parameters
      }
    )
    
    (ok true)
  )
)

;; Add authorized contract
(define-public (add-authorized-contract (contract-address principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set authorized-contracts (unwrap! (as-max-len? (append (var-get authorized-contracts) contract-address) u10) (err u999)))
    (ok true)
  )
)

;; Read-only Functions

;; Get curve configuration
(define-read-only (get-curve-config (curve-type (string-ascii 20)))
  (map-get? curve-configs { curve-type: curve-type })
)

;; List supported curve types
(define-read-only (get-supported-curves)
  (list "linear" "exponential")
)
