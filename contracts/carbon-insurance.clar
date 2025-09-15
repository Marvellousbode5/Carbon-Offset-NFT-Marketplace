;; Carbon Insurance Pool Contract
;; Provides risk coverage for carbon credit investments through a decentralized insurance pool

(define-fungible-token insurance-pool-token)
(define-fungible-token claim-token)

(define-data-var total-pool-value uint u0)
(define-data-var pool-reserve-ratio uint u20) ;; 20% minimum reserve
(define-data-var base-premium-rate uint u5) ;; 5% base premium rate
(define-data-var claim-counter uint u0)
(define-data-var policy-counter uint u0)

;; Insurance policy structure
(define-map insurance-policies
    uint
    {
        policy-holder: principal,
        credit-id: uint,
        coverage-amount: uint,
        premium-paid: uint,
        risk-level: uint,
        start-block: uint,
        end-block: uint,
        active: bool,
        risk-types: (list 5 (string-ascii 20))
    }
)

;; Pool contributor information
(define-map pool-contributors
    principal
    {
        tokens-staked: uint,
        rewards-earned: uint,
        join-block: uint,
        contribution-history: uint
    }
)

;; Risk assessment data
(define-map risk-assessments
    uint
    {
        credit-id: uint,
        assessor: principal,
        risk-score: uint,
        assessment-date: uint,
        factors: (list 3 (string-ascii 30)),
        confidence-level: uint
    }
)

;; Claims tracking
(define-map insurance-claims
    uint
    {
        claim-id: uint,
        policy-id: uint,
        claimant: principal,
        claim-amount: uint,
        claim-type: (string-ascii 20),
        evidence-hash: (buff 32),
        status: (string-ascii 15),
        filing-block: uint,
        resolution-block: uint,
        validator-votes: uint,
        total-validators: uint
    }
)

;; Validator network for claims
(define-map claim-validators
    principal
    {
        reputation-score: uint,
        successful-validations: uint,
        total-validations: uint,
        staked-amount: uint,
        active: bool
    }
)

;; Validation votes on claims
(define-map claim-votes
    {validator: principal, claim-id: uint}
    {
        vote: bool,
        vote-block: uint,
        stake-weight: uint
    }
)

;; Premium calculation factors
(define-map premium-factors
    (string-ascii 20)
    {
        base-rate: uint,
        max-rate: uint,
        risk-multiplier: uint
    }
)

;; Pool performance metrics
(define-map pool-metrics
    uint
    {
        week: uint,
        total-premiums: uint,
        total-claims: uint,
        pool-growth: uint,
        utilization-rate: uint
    }
)

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u400))
(define-constant ERR-INVALID-POLICY (err u401))
(define-constant ERR-INSUFFICIENT-COVERAGE (err u402))
(define-constant ERR-POLICY-EXPIRED (err u403))
(define-constant ERR-CLAIM-EXISTS (err u404))
(define-constant ERR-INVALID-CLAIM (err u405))
(define-constant ERR-INSUFFICIENT-POOL (err u406))
(define-constant ERR-ALREADY-VOTED (err u407))
(define-constant ERR-NOT-VALIDATOR (err u408))
(define-constant ERR-INVALID-RISK-LEVEL (err u409))

;; Create insurance policy for carbon credit
(define-public (create-insurance-policy (credit-id uint) (coverage-amount uint) (duration-blocks uint) (risk-types (list 5 (string-ascii 20))))
    (let
        (
            (policy-id (var-get policy-counter))
            (current-block stacks-block-height)
            (end-block (+ current-block duration-blocks))
            (risk-level (calculate-risk-level credit-id risk-types))
            (premium-amount (calculate-premium coverage-amount risk-level duration-blocks))
        )
        (asserts! (> coverage-amount u0) ERR-INVALID-POLICY)
        (asserts! (and (>= risk-level u1) (<= risk-level u10)) ERR-INVALID-RISK-LEVEL)
        (asserts! (>= (var-get total-pool-value) coverage-amount) ERR-INSUFFICIENT-POOL)
        
        ;; Collect premium payment
        (try! (stx-transfer? premium-amount tx-sender (as-contract tx-sender)))
        
        ;; Create policy
        (map-set insurance-policies policy-id {
            policy-holder: tx-sender,
            credit-id: credit-id,
            coverage-amount: coverage-amount,
            premium-paid: premium-amount,
            risk-level: risk-level,
            start-block: current-block,
            end-block: end-block,
            active: true,
            risk-types: risk-types
        })
        
        ;; Update pool value
        (var-set total-pool-value (+ (var-get total-pool-value) premium-amount))
        (var-set policy-counter (+ policy-id u1))
        
        ;; Distribute pool tokens to premium payer
        (try! (ft-mint? insurance-pool-token (/ premium-amount u2) tx-sender))
        
        (ok policy-id)
    )
)

;; Contribute to insurance pool
(define-public (contribute-to-pool (amount uint))
    (let
        (
            (contributor-data (default-to {tokens-staked: u0, rewards-earned: u0, join-block: stacks-block-height, contribution-history: u0}
                              (map-get? pool-contributors tx-sender)))
            (pool-tokens (calculate-pool-tokens amount))
        )
        (asserts! (> amount u0) ERR-INVALID-POLICY)
        
        ;; Transfer STX to pool
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Mint pool tokens
        (try! (ft-mint? insurance-pool-token pool-tokens tx-sender))
        
        ;; Update contributor data
        (map-set pool-contributors tx-sender {
            tokens-staked: (+ (get tokens-staked contributor-data) pool-tokens),
            rewards-earned: (get rewards-earned contributor-data),
            join-block: (get join-block contributor-data),
            contribution-history: (+ (get contribution-history contributor-data) u1)
        })
        
        ;; Update pool value
        (var-set total-pool-value (+ (var-get total-pool-value) amount))
        
        (ok pool-tokens)
    )
)

;; File insurance claim
(define-public (file-insurance-claim (policy-id uint) (claim-amount uint) (claim-type (string-ascii 20)) (evidence-hash (buff 32)))
    (let
        (
            (policy (unwrap! (map-get? insurance-policies policy-id) ERR-INVALID-POLICY))
            (claim-id (var-get claim-counter))
            (current-block stacks-block-height)
        )
        ;; Verify policy ownership
        (asserts! (is-eq tx-sender (get policy-holder policy)) ERR-NOT-AUTHORIZED)
        
        ;; Verify policy is active and not expired
        (asserts! (get active policy) ERR-POLICY-EXPIRED)
        (asserts! (< current-block (get end-block policy)) ERR-POLICY-EXPIRED)
        
        ;; Verify claim amount
        (asserts! (<= claim-amount (get coverage-amount policy)) ERR-INSUFFICIENT-COVERAGE)
        
        ;; Check no existing pending claim for this policy
        (asserts! (is-none (get-pending-claim policy-id)) ERR-CLAIM-EXISTS)
        
        ;; Create claim
        (map-set insurance-claims claim-id {
            claim-id: claim-id,
            policy-id: policy-id,
            claimant: tx-sender,
            claim-amount: claim-amount,
            claim-type: claim-type,
            evidence-hash: evidence-hash,
            status: "pending",
            filing-block: current-block,
            resolution-block: u0,
            validator-votes: u0,
            total-validators: u0
        })
        
        (var-set claim-counter (+ claim-id u1))
        
        ;; Issue claim token for voting process
        (try! (ft-mint? claim-token u1 tx-sender))
        
        (ok claim-id)
    )
)

;; Become a claim validator
(define-public (become-validator (stake-amount uint))
    (let
        (
            (validator-data (default-to {reputation-score: u100, successful-validations: u0, total-validations: u0, staked-amount: u0, active: false}
                            (map-get? claim-validators tx-sender)))
        )
        (asserts! (>= stake-amount u1000) ERR-INVALID-POLICY) ;; Minimum stake requirement
        
        ;; Transfer stake
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
        
        ;; Update validator status
        (map-set claim-validators tx-sender {
            reputation-score: (get reputation-score validator-data),
            successful-validations: (get successful-validations validator-data),
            total-validations: (get total-validations validator-data),
            staked-amount: (+ (get staked-amount validator-data) stake-amount),
            active: true
        })
        
        (ok true)
    )
)

;; Vote on insurance claim
(define-public (vote-on-claim (claim-id uint) (approve bool))
    (let
        (
            (claim (unwrap! (map-get? insurance-claims claim-id) ERR-INVALID-CLAIM))
            (validator (unwrap! (map-get? claim-validators tx-sender) ERR-NOT-VALIDATOR))
            (vote-key {validator: tx-sender, claim-id: claim-id})
            (existing-vote (map-get? claim-votes vote-key))
            (stake-weight (get staked-amount validator))
        )
        ;; Check validator is active
        (asserts! (get active validator) ERR-NOT-VALIDATOR)
        
        ;; Check no existing vote
        (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
        
        ;; Check claim is pending
        (asserts! (is-eq (get status claim) "pending") ERR-INVALID-CLAIM)
        
        ;; Record vote
        (map-set claim-votes vote-key {
            vote: approve,
            vote-block: stacks-block-height,
            stake-weight: stake-weight
        })
        
        ;; Update claim vote count
        (map-set insurance-claims claim-id
            (merge claim {
                validator-votes: (if approve (+ (get validator-votes claim) stake-weight) (get validator-votes claim)),
                total-validators: (+ (get total-validators claim) stake-weight)
            })
        )
        
        ;; Update validator stats
        (map-set claim-validators tx-sender
            (merge validator {total-validations: (+ (get total-validations validator) u1)})
        )
        
        ;; Check if claim should be processed
        (try! (process-claim-if-ready claim-id))
        
        (ok true)
    )
)

;; Process claim if voting threshold reached
(define-public (process-claim-if-ready (claim-id uint))
    (let
        (
            (claim (unwrap! (map-get? insurance-claims claim-id) ERR-INVALID-CLAIM))
            (policy (unwrap! (map-get? insurance-policies (get policy-id claim)) ERR-INVALID-POLICY))
            (approval-threshold (/ (* (get total-validators claim) u66) u100)) ;; 66% threshold
            (approved (>= (get validator-votes claim) approval-threshold))
        )
        ;; Only process if enough votes collected
        (if (>= (get total-validators claim) u3000) ;; Minimum stake threshold
            (begin
                (if approved
                    (begin
                        ;; Approve claim - pay out
                        (try! (as-contract (stx-transfer? (get claim-amount claim) (as-contract tx-sender) (get claimant claim))))
                        
                        ;; Deactivate policy
                        (map-set insurance-policies (get policy-id claim)
                            (merge policy {active: false})
                        )
                        
                        ;; Update claim status
                        (map-set insurance-claims claim-id
                            (merge claim {
                                status: "approved",
                                resolution-block: stacks-block-height
                            })
                        )
                        
                        ;; Update pool value
                        (var-set total-pool-value (- (var-get total-pool-value) (get claim-amount claim)))
                        
                        (ok true)
                    )
                    (begin
                        ;; Reject claim
                        (map-set insurance-claims claim-id
                            (merge claim {
                                status: "rejected",
                                resolution-block: stacks-block-height
                            })
                        )
                        (ok false)
                    )
                )
            )
            (ok false) ;; Not enough votes yet
        )
    )
)

;; Withdraw from pool (with time lock)
(define-public (withdraw-from-pool (token-amount uint))
    (let
        (
            (contributor-data (unwrap! (map-get? pool-contributors tx-sender) ERR-NOT-AUTHORIZED))
            (withdrawal-amount (calculate-withdrawal-value token-amount))
            (min-reserve (/ (* (var-get total-pool-value) (var-get pool-reserve-ratio)) u100))
        )
        ;; Check sufficient tokens
        (asserts! (<= token-amount (get tokens-staked contributor-data)) ERR-INSUFFICIENT-COVERAGE)
        
        ;; Check pool reserve requirements
        (asserts! (>= (- (var-get total-pool-value) withdrawal-amount) min-reserve) ERR-INSUFFICIENT-POOL)
        
        ;; Check time lock (30 days)
        (asserts! (>= (- stacks-block-height (get join-block contributor-data)) u4320) ERR-POLICY-EXPIRED)
        
        ;; Burn pool tokens
        (try! (ft-burn? insurance-pool-token token-amount tx-sender))
        
        ;; Transfer STX
        (try! (as-contract (stx-transfer? withdrawal-amount (as-contract tx-sender) tx-sender)))
        
        ;; Update contributor data
        (map-set pool-contributors tx-sender
            (merge contributor-data {tokens-staked: (- (get tokens-staked contributor-data) token-amount)})
        )
        
        ;; Update pool value
        (var-set total-pool-value (- (var-get total-pool-value) withdrawal-amount))
        
        (ok withdrawal-amount)
    )
)

;; Helper functions
(define-private (calculate-risk-level (credit-id uint) (risk-types (list 5 (string-ascii 20))))
    (let
        (
            (base-risk u5)
            (type-count (len risk-types))
        )
        (+ base-risk (/ type-count u2))
    )
)

(define-private (calculate-premium (coverage-amount uint) (risk-level uint) (duration-blocks uint))
    (let
        (
            (base-rate (var-get base-premium-rate))
            (risk-multiplier (+ u100 (* risk-level u10)))
            (time-factor (/ duration-blocks u1000))
        )
        (/ (* (* (* coverage-amount base-rate) risk-multiplier) time-factor) u1000000)
    )
)

(define-private (calculate-pool-tokens (amount uint))
    (if (is-eq (var-get total-pool-value) u0)
        amount
        (/ (* amount u1000000) (var-get total-pool-value))
    )
)

(define-private (calculate-withdrawal-value (tokens uint))
    (/ (* tokens (var-get total-pool-value)) u1000000)
)

(define-private (get-pending-claim (policy-id uint))
    (let
        (
            (claim-iter u0)
        )
        ;; Simple check - in production would iterate through claims
        none
    )
)

;; Read-only functions
(define-read-only (get-policy-info (policy-id uint))
    (map-get? insurance-policies policy-id)
)

(define-read-only (get-claim-info (claim-id uint))
    (map-get? insurance-claims claim-id)
)

(define-read-only (get-contributor-info (contributor principal))
    (map-get? pool-contributors contributor)
)

(define-read-only (get-validator-info (validator principal))
    (map-get? claim-validators validator)
)

(define-read-only (get-pool-stats)
    {
        total-value: (var-get total-pool-value),
        reserve-ratio: (var-get pool-reserve-ratio),
        total-policies: (var-get policy-counter),
        total-claims: (var-get claim-counter)
    }
)

(define-read-only (get-coverage-quote (coverage-amount uint) (risk-level uint) (duration-blocks uint))
    (calculate-premium coverage-amount risk-level duration-blocks)
)

(define-read-only (get-pool-token-balance (holder principal))
    (ft-get-balance insurance-pool-token holder)
)

(define-read-only (check-policy-eligibility (credit-id uint) (coverage-amount uint))
    (and 
        (>= (var-get total-pool-value) coverage-amount)
        (> coverage-amount u0)
    )
)


