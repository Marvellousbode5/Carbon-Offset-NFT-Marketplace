(define-fungible-token impact-rewards)
(define-non-fungible-token achievement-badge uint)

(define-data-var badge-counter uint u0)
(define-data-var oracle-admin principal tx-sender)
(define-data-var total-carbon-offset uint u0)
(define-data-var reward-multiplier uint u10)

(define-map impact-data
    uint
    {
        verified-tons: uint,
        impact-score: uint,
        verification-timestamp: uint,
        oracle-signature: (buff 65)
    }
)

(define-map user-impact
    principal
    {
        total-tons-offset: uint,
        total-impact-score: uint,
        badges-earned: uint,
        lifetime-rewards: uint,
        last-activity: uint
    }
)

(define-map leaderboard-rank
    uint
    {
        user: principal,
        impact-score: uint,
        rank-timestamp: uint
    }
)

(define-map achievement-definitions
    uint
    {
        name: (string-ascii 50),
        description: (string-ascii 100),
        requirement-type: (string-ascii 20),
        requirement-value: uint,
        reward-amount: uint
    }
)

(define-map user-achievements
    {user: principal, achievement-id: uint}
    {
        earned: bool,
        earned-date: uint,
        badge-id: uint
    }
)

(define-map monthly-challenges
    uint
    {
        month: uint,
        target-tons: uint,
        bonus-multiplier: uint,
        participants: uint,
        completed: uint
    }
)

(define-map challenge-participation
    {user: principal, month: uint}
    {
        tons-contributed: uint,
        completed: bool,
        reward-claimed: bool
    }
)

(define-constant ERR-NOT-ORACLE (err u300))
(define-constant ERR-NOT-ADMIN (err u301))
(define-constant ERR-INVALID-DATA (err u302))
(define-constant ERR-ALREADY-CLAIMED (err u303))
(define-constant ERR-ACHIEVEMENT-NOT-EARNED (err u304))
(define-constant ERR-CHALLENGE-NOT-ACTIVE (err u305))

(define-public (submit-impact-data (tons-offset uint) (credit-id uint) (signature (buff 65)))
    (let
        (
            (current-block stacks-block-height)
            (impact-score (calculate-impact-score tons-offset))
            (user-data (default-to {total-tons-offset: u0, total-impact-score: u0, badges-earned: u0, lifetime-rewards: u0, last-activity: u0}
                       (map-get? user-impact tx-sender)))
        )
        (asserts! (> tons-offset u0) ERR-INVALID-DATA)
        
        (map-set impact-data credit-id {
            verified-tons: tons-offset,
            impact-score: impact-score,
            verification-timestamp: current-block,
            oracle-signature: signature
        })
        
        (map-set user-impact tx-sender {
            total-tons-offset: (+ (get total-tons-offset user-data) tons-offset),
            total-impact-score: (+ (get total-impact-score user-data) impact-score),
            badges-earned: (get badges-earned user-data),
            lifetime-rewards: (get lifetime-rewards user-data),
            last-activity: current-block
        })
        
        (var-set total-carbon-offset (+ (var-get total-carbon-offset) tons-offset))
        
        (try! (distribute-rewards tx-sender impact-score))
        (try! (update-leaderboard tx-sender))
        
        (ok impact-score)
    )
)

(define-public (claim-achievement (achievement-id uint))
    (let
        (
            (achievement-def (unwrap! (map-get? achievement-definitions achievement-id) ERR-INVALID-DATA))
            (user-data (unwrap! (map-get? user-impact tx-sender) ERR-INVALID-DATA))
            (achievement-key {user: tx-sender, achievement-id: achievement-id})
            (already-earned (default-to {earned: false, earned-date: u0, badge-id: u0}
                           (map-get? user-achievements achievement-key)))
            (badge-id (var-get badge-counter))
        )
        (asserts! (not (get earned already-earned)) ERR-ALREADY-CLAIMED)
        (asserts! (meets-requirement user-data achievement-def) ERR-ACHIEVEMENT-NOT-EARNED)
        
        (var-set badge-counter (+ badge-id u1))
        
        (try! (nft-mint? achievement-badge badge-id tx-sender))
        
        (map-set user-achievements achievement-key {
            earned: true,
            earned-date: stacks-block-height,
            badge-id: badge-id
        })
        
        (map-set user-impact tx-sender
            (merge user-data {badges-earned: (+ (get badges-earned user-data) u1)})
        )
        
        (try! (ft-mint? impact-rewards (get reward-amount achievement-def) tx-sender))
        
        (ok badge-id)
    )
)

(define-public (join-monthly-challenge (month uint))
    (let
        (
            (challenge (unwrap! (map-get? monthly-challenges month) ERR-CHALLENGE-NOT-ACTIVE))
            (participation-key {user: tx-sender, month: month})
            (existing-participation (map-get? challenge-participation participation-key))
        )
        (asserts! (is-none existing-participation) ERR-ALREADY-CLAIMED)
        
        (map-set challenge-participation participation-key {
            tons-contributed: u0,
            completed: false,
            reward-claimed: false
        })
        
        (map-set monthly-challenges month
            (merge challenge {participants: (+ (get participants challenge) u1)})
        )
        
        (ok true)
    )
)

(define-public (contribute-to-challenge (month uint) (tons uint))
    (let
        (
            (challenge (unwrap! (map-get? monthly-challenges month) ERR-CHALLENGE-NOT-ACTIVE))
            (participation-key {user: tx-sender, month: month})
            (participation (unwrap! (map-get? challenge-participation participation-key) ERR-INVALID-DATA))
            (new-total (+ (get tons-contributed participation) tons))
            (target-met (>= new-total (get target-tons challenge)))
        )
        (map-set challenge-participation participation-key {
            tons-contributed: new-total,
            completed: target-met,
            reward-claimed: (get reward-claimed participation)
        })

        
        (ok target-met)
    )
)

(define-public (claim-challenge-reward (month uint))
    (let
        (
            (challenge (unwrap! (map-get? monthly-challenges month) ERR-CHALLENGE-NOT-ACTIVE))
            (participation-key {user: tx-sender, month: month})
            (participation (unwrap! (map-get? challenge-participation participation-key) ERR-INVALID-DATA))
            (base-reward u1000)
            (bonus-reward (* base-reward (get bonus-multiplier challenge)))
        )
        (asserts! (get completed participation) ERR-ACHIEVEMENT-NOT-EARNED)
        (asserts! (not (get reward-claimed participation)) ERR-ALREADY-CLAIMED)
        
        (map-set challenge-participation participation-key
            (merge participation {reward-claimed: true})
        )
        
        (try! (ft-mint? impact-rewards bonus-reward tx-sender))
        
        (ok bonus-reward)
    )
)

(define-private (calculate-impact-score (tons uint))
    (* tons (var-get reward-multiplier))
)

(define-private (distribute-rewards (user principal) (impact-score uint))
    (let
        (
            (reward-amount (/ impact-score u100))
            (user-data (unwrap! (map-get? user-impact user) ERR-INVALID-DATA))
        )
        (try! (ft-mint? impact-rewards reward-amount user))
        
        (map-set user-impact user
            (merge user-data {lifetime-rewards: (+ (get lifetime-rewards user-data) reward-amount)})
        )
        
        (ok reward-amount)
    )
)


(define-private (auto-award-achievement (user principal) (achievement-id uint))
    (let
        (
            (achievement-key {user: user, achievement-id: achievement-id})
            (already-earned (default-to {earned: false, earned-date: u0, badge-id: u0}
                           (map-get? user-achievements achievement-key)))
        )
        (if (not (get earned already-earned))
            (begin
                (map-set user-achievements achievement-key {
                    earned: true,
                    earned-date: stacks-block-height,
                    badge-id: u0
                })
                (ok true)
            )
            (ok false)
        )
    )
)

(define-private (update-leaderboard (user principal))
    (let
        (
            (user-data (unwrap! (map-get? user-impact user) ERR-INVALID-DATA))
            (current-score (get total-impact-score user-data))
        )
        (map-set leaderboard-rank (get-next-rank) {
            user: user,
            impact-score: current-score,
            rank-timestamp: stacks-block-height
        })
        
        (ok true)
    )
)

(define-private (get-next-rank)
    u1
)

(define-private (meets-requirement (user-data {total-tons-offset: uint, total-impact-score: uint, badges-earned: uint, lifetime-rewards: uint, last-activity: uint}) 
                                  (achievement {name: (string-ascii 50), description: (string-ascii 100), requirement-type: (string-ascii 20), requirement-value: uint, reward-amount: uint}))
    (if (is-eq (get requirement-type achievement) "tons")
        (>= (get total-tons-offset user-data) (get requirement-value achievement))
        (>= (get total-impact-score user-data) (get requirement-value achievement))
    )
)

(define-public (setup-achievements)
    (begin
        (asserts! (is-eq tx-sender (var-get oracle-admin)) ERR-NOT-ADMIN)
        
        (map-set achievement-definitions u1 {
            name: "First Steps",
            description: "Offset your first 10 tons of carbon",
            requirement-type: "tons",
            requirement-value: u10,
            reward-amount: u500
        })
        
        (map-set achievement-definitions u2 {
            name: "Carbon Champion",
            description: "Offset 100 tons of carbon",
            requirement-type: "tons",
            requirement-value: u100,
            reward-amount: u2000
        })
        
        (map-set achievement-definitions u3 {
            name: "Impact Master",
            description: "Reach 10000 impact score",
            requirement-type: "score",
            requirement-value: u10000,
            reward-amount: u3000
        })
        
        (ok true)
    )
)

(define-public (create-monthly-challenge (month uint) (target-tons uint) (bonus-multiplier uint))
    (begin
        (asserts! (is-eq tx-sender (var-get oracle-admin)) ERR-NOT-ADMIN)
        
        (map-set monthly-challenges month {
            month: month,
            target-tons: target-tons,
            bonus-multiplier: bonus-multiplier,
            participants: u0,
            completed: u0
        })
        
        (ok true)
    )
)

(define-public (set-reward-multiplier (new-multiplier uint))
    (begin
        (asserts! (is-eq tx-sender (var-get oracle-admin)) ERR-NOT-ADMIN)
        (var-set reward-multiplier new-multiplier)
        (ok true)
    )
)

(define-public (transfer-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get oracle-admin)) ERR-NOT-ADMIN)
        (var-set oracle-admin new-admin)
        (ok true)
    )
)

(define-read-only (get-user-impact (user principal))
    (map-get? user-impact user)
)

(define-read-only (get-impact-data (credit-id uint))
    (map-get? impact-data credit-id)
)

(define-read-only (get-achievement-status (user principal) (achievement-id uint))
    (map-get? user-achievements {user: user, achievement-id: achievement-id})
)

(define-read-only (get-leaderboard-entry (rank uint))
    (map-get? leaderboard-rank rank)
)

(define-read-only (get-challenge-info (month uint))
    (map-get? monthly-challenges month)
)

(define-read-only (get-user-challenge (user principal) (month uint))
    (map-get? challenge-participation {user: user, month: month})
)

(define-read-only (get-global-stats)
    {
        total-offset: (var-get total-carbon-offset),
        reward-multiplier: (var-get reward-multiplier),
        total-badges: (var-get badge-counter)
    }
)

(define-read-only (get-reward-balance (user principal))
    (ft-get-balance impact-rewards user)
)
