;; Payment Streams Smart Contract
;; Enables creation and management of continuous payment streams between parties

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_STREAM_NOT_FOUND (err u101))
(define-constant ERR_STREAM_ALREADY_EXISTS (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))
(define-constant ERR_INVALID_DURATION (err u105))
(define-constant ERR_STREAM_ALREADY_STARTED (err u106))
(define-constant ERR_STREAM_NOT_STARTED (err u107))
(define-constant ERR_STREAM_ENDED (err u108))
(define-constant ERR_INVALID_START_TIME (err u109))
(define-constant ERR_MODIFICATION_NOT_ALLOWED (err u110))
(define-constant ERR_CONSENT_REQUIRED (err u111))

;; Data Variables
(define-data-var stream-counter uint u0)
(define-data-var contract-fee-rate uint u25) ;; 0.25% fee in basis points (25/10000)

;; Stream status constants
(define-constant STATUS_CREATED u0)
(define-constant STATUS_ACTIVE u1)
(define-constant STATUS_PAUSED u2)
(define-constant STATUS_CANCELLED u3)
(define-constant STATUS_COMPLETED u4)

;; Data Maps
(define-map streams
    uint ;; stream-id
    {
        sender: principal,
        recipient: principal,
        total-amount: uint,
        amount-per-block: uint,
        start-block: uint,
        duration-blocks: uint,
        end-block: uint,
        amount-withdrawn: uint,
        status: uint,
        created-at: uint,
        last-withdrawal: uint
    }
)

(define-map stream-balances
    uint ;; stream-id
    {
        deposited: uint,
        available: uint,
        withdrawn: uint
    }
)

(define-map modification-proposals
    uint ;; stream-id
    {
        proposer: principal,
        new-duration: (optional uint),
        additional-amount: (optional uint),
        new-rate: (optional uint),
        sender-consent: bool,
        recipient-consent: bool,
        expires-at: uint
    }
)

(define-map user-stream-count
    principal
    uint
)

;; Read-only functions

(define-read-only (get-stream (stream-id uint))
    (map-get? streams stream-id)
)

(define-read-only (get-stream-balance (stream-id uint))
    (map-get? stream-balances stream-id)
)

(define-read-only (get-modification-proposal (stream-id uint))
    (map-get? modification-proposals stream-id)
)

(define-read-only (get-stream-counter)
    (var-get stream-counter)
)

(define-read-only (get-user-stream-count (user principal))
    (default-to u0 (map-get? user-stream-count user))
)

(define-read-only (calculate-available-amount (stream-id uint))
    (let (
        (stream-data (unwrap! (get-stream stream-id) (err u0)))
        (current-block stacks-block-height)
        (start-block (get start-block stream-data))
        (end-block (get end-block stream-data))
        (amount-per-block (get amount-per-block stream-data))
        (amount-withdrawn (get amount-withdrawn stream-data))
        (status (get status stream-data))
    )
    (if (or (is-eq status STATUS_CREATED) 
            (is-eq status STATUS_CANCELLED)
            (is-eq status STATUS_COMPLETED)
            (< current-block start-block))
        (ok u0)
        (let (
            (effective-current-block (if (> current-block end-block) end-block current-block))
            (blocks-elapsed (- effective-current-block start-block))
            (total-earned (* blocks-elapsed amount-per-block))
            (available (if (> total-earned amount-withdrawn) 
                          (- total-earned amount-withdrawn) 
                          u0))
        )
        (ok available))))
)

(define-read-only (get-contract-fee-rate)
    (var-get contract-fee-rate)
)

;; Private functions

(define-private (increment-stream-counter)
    (let ((current-counter (var-get stream-counter)))
        (var-set stream-counter (+ current-counter u1))
        current-counter
    )
)

(define-private (calculate-fee (amount uint))
    (/ (* amount (var-get contract-fee-rate)) u10000)
)

(define-private (update-user-stream-count (user principal) (increment bool))
    (let ((current-count (get-user-stream-count user)))
        (map-set user-stream-count user 
            (if increment 
                (+ current-count u1) 
                (if (> current-count u0) (- current-count u1) u0)
            )
        )
    )
)

;; Public functions

(define-public (create-stream 
    (recipient principal)
    (total-amount uint)
    (duration-blocks uint)
    (start-block (optional uint))
)
    (let (
        (stream-id (increment-stream-counter))
        (sender tx-sender)
        (actual-start-block (default-to stacks-block-height start-block))
        (end-block (+ actual-start-block duration-blocks))
        (amount-per-block (/ total-amount duration-blocks))
        (fee (calculate-fee total-amount))
        (total-with-fee (+ total-amount fee))
    )
    ;; Validations
    (asserts! (> total-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> duration-blocks u0) ERR_INVALID_DURATION)
    (asserts! (not (is-eq sender recipient)) ERR_UNAUTHORIZED)
    (asserts! (>= actual-start-block stacks-block-height) ERR_INVALID_START_TIME)
    (asserts! (>= (stx-get-balance sender) total-with-fee) ERR_INSUFFICIENT_BALANCE)
    
    ;; Transfer funds to contract
    (try! (stx-transfer? total-with-fee sender (as-contract tx-sender)))
    
    ;; Create stream record
    (map-set streams stream-id {
        sender: sender,
        recipient: recipient,
        total-amount: total-amount,
        amount-per-block: amount-per-block,
        start-block: actual-start-block,
        duration-blocks: duration-blocks,
        end-block: end-block,
        amount-withdrawn: u0,
        status: (if (is-eq actual-start-block stacks-block-height) STATUS_ACTIVE STATUS_CREATED),
        created-at: stacks-block-height,
        last-withdrawal: u0
    })
    
    ;; Create balance record
    (map-set stream-balances stream-id {
        deposited: total-amount,
        available: u0,
        withdrawn: u0
    })
    
    ;; Update user stream counts
    (update-user-stream-count sender true)
    (update-user-stream-count recipient true)
    
    ;; Transfer fee to contract owner if applicable
    (if (> fee u0)
        (try! (as-contract (stx-transfer? fee tx-sender CONTRACT_OWNER)))
        true
    )
    
    (ok stream-id))
)

(define-public (withdraw-from-stream (stream-id uint))
    (let (
        (stream-data (unwrap! (get-stream stream-id) ERR_STREAM_NOT_FOUND))
        (balance-data (unwrap! (get-stream-balance stream-id) ERR_STREAM_NOT_FOUND))
        (recipient (get recipient stream-data))
        (available-amount (unwrap! (calculate-available-amount stream-id) ERR_STREAM_NOT_FOUND))
    )
    ;; Validations
    (asserts! (is-eq tx-sender recipient) ERR_UNAUTHORIZED)
    (asserts! (> available-amount u0) ERR_INSUFFICIENT_BALANCE)
    (asserts! (not (is-eq (get status stream-data) STATUS_CANCELLED)) ERR_STREAM_ENDED)
    
    ;; Transfer available amount to recipient
    (try! (as-contract (stx-transfer? available-amount tx-sender recipient)))
    
    ;; Update stream data
    (map-set streams stream-id (merge stream-data {
        amount-withdrawn: (+ (get amount-withdrawn stream-data) available-amount),
        last-withdrawal: stacks-block-height,
        status: (if (>= stacks-block-height (get end-block stream-data)) STATUS_COMPLETED (get status stream-data))
    }))
    
    ;; Update balance data
    (map-set stream-balances stream-id (merge balance-data {
        available: u0,
        withdrawn: (+ (get withdrawn balance-data) available-amount)
    }))
    
    (ok available-amount))
)

(define-public (cancel-stream (stream-id uint))
    (let (
        (stream-data (unwrap! (get-stream stream-id) ERR_STREAM_NOT_FOUND))
        (sender (get sender stream-data))
        (recipient (get recipient stream-data))
        (available-to-recipient (unwrap! (calculate-available-amount stream-id) ERR_STREAM_NOT_FOUND))
        (amount-withdrawn (get amount-withdrawn stream-data))
        (total-amount (get total-amount stream-data))
        (remaining-to-sender (- total-amount (+ amount-withdrawn available-to-recipient)))
    )
    ;; Only sender can cancel
    (asserts! (is-eq tx-sender sender) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq (get status stream-data) STATUS_CANCELLED)) ERR_STREAM_ENDED)
    (asserts! (not (is-eq (get status stream-data) STATUS_COMPLETED)) ERR_STREAM_ENDED)
    
    ;; Transfer available amount to recipient if any
    (if (> available-to-recipient u0)
        (try! (as-contract (stx-transfer? available-to-recipient tx-sender recipient)))
        true
    )
    
    ;; Return remaining amount to sender if any
    (if (> remaining-to-sender u0)
        (try! (as-contract (stx-transfer? remaining-to-sender tx-sender sender)))
        true
    )
    
    ;; Update stream status
    (map-set streams stream-id (merge stream-data {
        status: STATUS_CANCELLED,
        amount-withdrawn: (+ amount-withdrawn available-to-recipient)
    }))
    
    (ok {
        recipient-amount: available-to-recipient,
        sender-refund: remaining-to-sender
    }))
)

(define-public (propose-stream-modification
    (stream-id uint)
    (new-duration (optional uint))
    (additional-amount (optional uint))
    (new-rate (optional uint))
)
    (let (
        (stream-data (unwrap! (get-stream stream-id) ERR_STREAM_NOT_FOUND))
        (sender (get sender stream-data))
        (recipient (get recipient stream-data))
        (proposer tx-sender)
    )
    ;; Only sender or recipient can propose modifications
    (asserts! (or (is-eq proposer sender) (is-eq proposer recipient)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status stream-data) STATUS_ACTIVE) ERR_MODIFICATION_NOT_ALLOWED)
    (asserts! (< stacks-block-height (get end-block stream-data)) ERR_STREAM_ENDED)
    
    ;; Create modification proposal
    (map-set modification-proposals stream-id {
        proposer: proposer,
        new-duration: new-duration,
        additional-amount: additional-amount,
        new-rate: new-rate,
        sender-consent: (is-eq proposer sender),
        recipient-consent: (is-eq proposer recipient),
        expires-at: (+ stacks-block-height u1440) ;; Expires in ~10 days (1440 blocks)
    })
    
    (ok true))
)

(define-public (consent-to-modification (stream-id uint))
    (let (
        (stream-data (unwrap! (get-stream stream-id) ERR_STREAM_NOT_FOUND))
        (proposal (unwrap! (get-modification-proposal stream-id) ERR_STREAM_NOT_FOUND))
        (sender (get sender stream-data))
        (recipient (get recipient stream-data))
        (consenter tx-sender)
    )
    ;; Only sender or recipient can consent
    (asserts! (or (is-eq consenter sender) (is-eq consenter recipient)) ERR_UNAUTHORIZED)
    (asserts! (< stacks-block-height (get expires-at proposal)) ERR_MODIFICATION_NOT_ALLOWED)
    
    ;; Update consent
    (let (
        (updated-proposal (merge proposal {
            sender-consent: (or (get sender-consent proposal) (is-eq consenter sender)),
            recipient-consent: (or (get recipient-consent proposal) (is-eq consenter recipient))
        }))
    )
    (map-set modification-proposals stream-id updated-proposal)
    
    ;; If both parties consent, execute modification
    (if (and (get sender-consent updated-proposal) (get recipient-consent updated-proposal))
        (try! (execute-modification stream-id))
        true
    )
    
    (ok true)))
)

(define-private (execute-modification (stream-id uint))
    (let (
        (stream-data (unwrap! (get-stream stream-id) ERR_STREAM_NOT_FOUND))
        (proposal (unwrap! (get-modification-proposal stream-id) ERR_STREAM_NOT_FOUND))
        (sender (get sender stream-data))
    )
    ;; Handle additional amount deposit if specified
    (match (get additional-amount proposal)
        additional-amt (begin
            (try! (stx-transfer? additional-amt sender (as-contract tx-sender)))
            (map-set streams stream-id (merge stream-data {
                total-amount: (+ (get total-amount stream-data) additional-amt)
            }))
        )
        true
    )
    
    ;; Handle duration extension if specified
    (match (get new-duration proposal)
        new-dur (map-set streams stream-id (merge stream-data {
            duration-blocks: new-dur,
            end-block: (+ (get start-block stream-data) new-dur)
        }))
        true
    )
    
    ;; Handle rate change if specified
    (match (get new-rate proposal)
        new-rate-val (map-set streams stream-id (merge stream-data {
            amount-per-block: new-rate-val
        }))
        true
    )
    
    ;; Remove the proposal
    (map-delete modification-proposals stream-id)
    
    (ok true))
)

;; Admin functions

(define-public (set-contract-fee-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (<= new-rate u1000) ERR_INVALID_AMOUNT) ;; Max 10% fee
        (var-set contract-fee-rate new-rate)
        (ok true)
    )
)

(define-public (emergency-pause-stream (stream-id uint))
    (let (
        (stream-data (unwrap! (get-stream stream-id) ERR_STREAM_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set streams stream-id (merge stream-data {
        status: STATUS_PAUSED
    }))
    (ok true))
)

(define-public (emergency-resume-stream (stream-id uint))
    (let (
        (stream-data (unwrap! (get-stream stream-id) ERR_STREAM_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status stream-data) STATUS_PAUSED) ERR_MODIFICATION_NOT_ALLOWED)
    (map-set streams stream-id (merge stream-data {
        status: STATUS_ACTIVE
    }))
    (ok true))
)