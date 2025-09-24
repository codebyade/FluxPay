;; Token Module - Manages token validation and multi-asset support

;; Define SIP-010 trait directly instead of invalid import
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
(define-constant ERR-UNAUTHORIZED (err u300))
(define-constant ERR-TOKEN-NOT-WHITELISTED (err u301))
(define-constant ERR-TOKEN-BLACKLISTED (err u302))
(define-constant ERR-INVALID-TOKEN (err u303))

;; Data Variables
(define-data-var core-contract principal .core-protocol)

;; Token whitelist/blacklist
(define-map whitelisted-tokens
  { token-contract: principal }
  { approved: bool, added-at: uint }
)

(define-map blacklisted-tokens
  { token-contract: principal }
  { banned: bool, banned-at: uint }
)

;; Token metadata cache
(define-map token-metadata
  { token-contract: principal }
  {
    name: (string-ascii 32),
    symbol: (string-ascii 32), ;; Updated from (string-ascii 10) to (string-ascii 32) to match SIP-010 trait
    decimals: uint,
    verified: bool
  }
)

;; Public Functions

;; Fixed validate-token function with proper trait usage
(define-public (validate-token (token-contract <sip-010-trait>))
  (begin
    ;; Only core contract can call this
    (asserts! (is-eq tx-sender (var-get core-contract)) ERR-UNAUTHORIZED)
    
    ;; Check if token is blacklisted
    (asserts! (is-none (map-get? blacklisted-tokens { token-contract: (contract-of token-contract) })) ERR-TOKEN-BLACKLISTED)
    
    ;; Check if token is whitelisted (if whitelist is being used)
    (match (map-get? whitelisted-tokens { token-contract: (contract-of token-contract) })
      whitelist-entry (asserts! (get approved whitelist-entry) ERR-TOKEN-NOT-WHITELISTED)
      ;; If no whitelist entry exists, allow by default (open system)
      true
    )
    
    ;; Test SIP-010 compliance and return consistent response type
    (match (contract-call? token-contract get-name)
      success (ok true)
      error ERR-INVALID-TOKEN
    )
  )
)

;; Check token balance
(define-public (check-balance (token-contract <sip-010-trait>) (account principal))
  (contract-call? token-contract get-balance account)
)

;; Admin Functions

;; Fixed whitelist-token function with proper trait usage
(define-public (whitelist-token (token-contract <sip-010-trait>))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    
    ;; Cache token metadata with proper error handling
    (let (
      (token-principal (contract-of token-contract))
      (token-name-result (contract-call? token-contract get-name))
      (token-symbol-result (contract-call? token-contract get-symbol))
      (token-decimals-result (contract-call? token-contract get-decimals))
    )
      ;; Set metadata based on successful calls
      (map-set token-metadata
        { token-contract: token-principal }
        {
          name: (ok-to-value-string token-name-result),
          symbol: (ok-to-value-symbol token-symbol-result),
          decimals: (ok-to-value-uint token-decimals-result),
          verified: (and (is-ok token-name-result) (is-ok token-symbol-result) (is-ok token-decimals-result))
        }
      )
      
      ;; Add to whitelist
      (map-set whitelisted-tokens
        { token-contract: token-principal }
        { approved: true, added-at: block-height }
      )
      
      (ok true)
    )
  )
)

;; Added helper function to safely extract ok values
(define-private (ok-to-value-string (result (response (string-ascii 32) uint)))
  (match result
    success success
    error "Unknown"
  )
)

;; Fixed helper function to handle correct string-ascii length
(define-private (ok-to-value-symbol (result (response (string-ascii 32) uint)))
  (match result
    success success
    error "UNK"
  )
)

(define-private (ok-to-value-uint (result (response uint uint)))
  (match result
    success success
    error u0
  )
)

;; Remove token from whitelist (only owner)
(define-public (remove-from-whitelist (token-contract principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (map-delete whitelisted-tokens { token-contract: token-contract })
    (ok true)
  )
)

;; Add token to blacklist (only owner)
(define-public (blacklist-token (token-contract principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (map-set blacklisted-tokens
      { token-contract: token-contract }
      { banned: true, banned-at: block-height }
    )
    (ok true)
  )
)

;; Remove token from blacklist (only owner)
(define-public (remove-from-blacklist (token-contract principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (map-delete blacklisted-tokens { token-contract: token-contract })
    (ok true)
  )
)

;; Read-only Functions

;; Check if token is whitelisted
(define-read-only (is-whitelisted (token-contract principal))
  (match (map-get? whitelisted-tokens { token-contract: token-contract })
    entry (get approved entry)
    false
  )
)

;; Check if token is blacklisted
(define-read-only (is-blacklisted (token-contract principal))
  (match (map-get? blacklisted-tokens { token-contract: token-contract })
    entry (get banned entry)
    false
  )
)

;; Get token metadata
(define-read-only (get-token-metadata (token-contract principal))
  (map-get? token-metadata { token-contract: token-contract })
)

;; Update core contract reference (only owner)
(define-public (update-core-contract (new-core principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set core-contract new-core)
    (ok true)
  )
)
