
(define-fungible-token pool-rewards)

(define-data-var total-staked uint u0)
(define-data-var reward-rate uint u100)
(define-data-var pool-active bool true)

(define-map staked-credits
    {staker: principal, credit-id: uint}
    {
        stake-amount: uint,
        stake-time: uint,
        last-claim: uint,
        lock-period: uint
    }
)

(define-map staker-totals
    principal
    {
        total-staked: uint,
        total-rewards: uint,
        active-stakes: uint
    }
)

(define-map pool-stats
    uint
    {
        total-participants: uint,
        total-rewards-distributed: uint,
        pool-tvl: uint
    }
)

(define-constant ERR-POOL-INACTIVE (err u200))
(define-constant ERR-INSUFFICIENT-STAKE (err u201))
(define-constant ERR-STAKE-LOCKED (err u202))
(define-constant ERR-NO-REWARDS (err u203))
(define-constant ERR-INVALID-LOCK-PERIOD (err u204))

(define-public (stake-credit (credit-id uint) (stake-amount uint) (lock-weeks uint))
    (let
        (
            (staker tx-sender)
            (current-block stacks-block-height)
            (lock-period (* lock-weeks u1008))
            (staker-data (default-to {total-staked: u0, total-rewards: u0, active-stakes: u0} 
                         (map-get? staker-totals staker)))
        )
        ;; (asserts! (get pool-active (var-get pool-active)) ERR-POOL-INACTIVE)
        (asserts! (> stake-amount u0) ERR-INSUFFICIENT-STAKE)
        (asserts! (and (>= lock-weeks u1) (<= lock-weeks u52)) ERR-INVALID-LOCK-PERIOD)
        
        (map-set staked-credits {staker: staker, credit-id: credit-id} {
            stake-amount: stake-amount,
            stake-time: current-block,
            last-claim: current-block,
            lock-period: lock-period
        })
        
        (map-set staker-totals staker {
            total-staked: (+ (get total-staked staker-data) stake-amount),
            total-rewards: (get total-rewards staker-data),
            active-stakes: (+ (get active-stakes staker-data) u1)
        })
        
        (var-set total-staked (+ (var-get total-staked) stake-amount))
        
        (ok true)
    )
)

(define-public (unstake-credit (credit-id uint))
    (let
        (
            (staker tx-sender)
            (stake-key {staker: staker, credit-id: credit-id})
            (stake-data (unwrap! (map-get? staked-credits stake-key) ERR-INSUFFICIENT-STAKE))
            (current-block stacks-block-height)
            (stake-end (+ (get stake-time stake-data) (get lock-period stake-data)))
            (staker-data (unwrap! (map-get? staker-totals staker) ERR-INSUFFICIENT-STAKE))
        )
        (asserts! (>= current-block stake-end) ERR-STAKE-LOCKED)
        
        (try! (claim-rewards credit-id))
        
        (map-delete staked-credits stake-key)
        
        (map-set staker-totals staker {
            total-staked: (- (get total-staked staker-data) (get stake-amount stake-data)),
            total-rewards: (get total-rewards staker-data),
            active-stakes: (- (get active-stakes staker-data) u1)
        })
        
        (var-set total-staked (- (var-get total-staked) (get stake-amount stake-data)))
        
        (ok (get stake-amount stake-data))
    )
)

(define-public (claim-rewards (credit-id uint))
    (let
        (
            (staker tx-sender)
            (stake-key {staker: staker, credit-id: credit-id})
            (stake-data (unwrap! (map-get? staked-credits stake-key) ERR-INSUFFICIENT-STAKE))
            (current-block stacks-block-height)
            (blocks-staked (- current-block (get last-claim stake-data)))
            (reward-amount (calculate-rewards (get stake-amount stake-data) blocks-staked))
            (staker-data (unwrap! (map-get? staker-totals staker) ERR-INSUFFICIENT-STAKE))
        )
        (asserts! (> reward-amount u0) ERR-NO-REWARDS)
        
        (try! (ft-mint? pool-rewards reward-amount staker))
        
        (map-set staked-credits stake-key
            (merge stake-data {last-claim: current-block})
        )
        
        (map-set staker-totals staker
            (merge staker-data {total-rewards: (+ (get total-rewards staker-data) reward-amount)})
        )
        
        (ok reward-amount)
    )
)

(define-private (calculate-rewards (stake-amount uint) (blocks-staked uint))
    (/ (* (* stake-amount blocks-staked) (var-get reward-rate)) u1000000)
)

(define-public (update-reward-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) (err u100))
        (var-set reward-rate new-rate)
        (ok true)
    )
)

(define-public (toggle-pool-status)
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) (err u100))
        (var-set pool-active (not (var-get pool-active)))
        (ok (var-get pool-active))
    )
)

(define-read-only (get-stake-info (staker principal) (credit-id uint))
    (map-get? staked-credits {staker: staker, credit-id: credit-id})
)

(define-read-only (get-staker-info (staker principal))
    (map-get? staker-totals staker)
)

(define-read-only (get-pending-rewards (staker principal) (credit-id uint))
    (let
        (
            (stake-data (map-get? staked-credits {staker: staker, credit-id: credit-id}))
        )
        (match stake-data
            data (let
                (
                    (blocks-staked (- stacks-block-height (get last-claim data)))
                )
                (some (calculate-rewards (get stake-amount data) blocks-staked))
            )
            none
        )
    )
)

(define-read-only (get-pool-stats)
    {
        total-staked: (var-get total-staked),
        reward-rate: (var-get reward-rate),
        pool-active: (var-get pool-active)
    }
)

(define-read-only (get-reward-balance (holder principal))
    (ft-get-balance pool-rewards holder)
)

(define-data-var contract-owner principal tx-sender)

(define-public (set-contract-owner (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) (err u100))
        (var-set contract-owner new-owner)
        (ok true)
    )
)

(define-public (emergency-withdraw (credit-id uint))
    (let
        (
            (staker tx-sender)
            (stake-key {staker: staker, credit-id: credit-id})
            (stake-data (unwrap! (map-get? staked-credits stake-key) ERR-INSUFFICIENT-STAKE))
            (penalty-amount (/ (get stake-amount stake-data) u10))
            (return-amount (- (get stake-amount stake-data) penalty-amount))
            (staker-data (unwrap! (map-get? staker-totals staker) ERR-INSUFFICIENT-STAKE))
        )
        (map-delete staked-credits stake-key)
        
        (map-set staker-totals staker {
            total-staked: (- (get total-staked staker-data) (get stake-amount stake-data)),
            total-rewards: (get total-rewards staker-data),
            active-stakes: (- (get active-stakes staker-data) u1)
        })
        
        (var-set total-staked (- (var-get total-staked) (get stake-amount stake-data)))
        
        (ok return-amount)
    )
)