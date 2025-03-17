;; Carbon Offset NFT Contract
;; Implements basic functionality for minting and retiring carbon credits

(define-non-fungible-token carbon-credit uint)

;; Data variables
(define-data-var credit-counter uint u0)
(define-map credit-metadata
    uint
    {
        amount: uint,
        verification-date: uint,
        retired: bool,
        owner: principal
    }
)

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-RETIRED (err u101))
(define-constant ERR-INVALID-CREDIT (err u102))

;; Mint new carbon credit
(define-public (mint-credit (amount uint) (verification-date uint))
    (let 
        (
            (credit-id (var-get credit-counter))
            (caller tx-sender)
        )
        ;; Increment counter
        (var-set credit-counter (+ credit-id u1))
        
        ;; Mint NFT
        (try! (nft-mint? carbon-credit credit-id caller))
        
        ;; Store metadata
        (map-set credit-metadata credit-id {
            amount: amount,
            verification-date: verification-date,
            retired: false,
            owner: caller
        })
        
        (ok credit-id)
    )
)

;; Transfer credit to new owner
(define-public (transfer-credit (credit-id uint) (recipient principal))
    (let 
        ((credit-data (unwrap! (map-get? credit-metadata credit-id) ERR-INVALID-CREDIT)))
        
        ;; Check if credit is not retired
        (asserts! (not (get retired credit-data)) ERR-ALREADY-RETIRED)
        
        ;; Transfer NFT
        (try! (nft-transfer? carbon-credit credit-id tx-sender recipient))
        
        ;; Update metadata
        (map-set credit-metadata credit-id 
            (merge credit-data { owner: recipient })
        )
        
        (ok true)
    )
)

;; Retire credit (offset carbon)
(define-public (retire-credit (credit-id uint))
    (let 
        ((credit-data (unwrap! (map-get? credit-metadata credit-id) ERR-INVALID-CREDIT)))
        
        ;; Verify ownership
        (asserts! (is-eq tx-sender (get owner credit-data)) ERR-NOT-AUTHORIZED)
        
        ;; Check if already retired
        (asserts! (not (get retired credit-data)) ERR-ALREADY-RETIRED)
        
        ;; Update metadata to mark as retired
        (map-set credit-metadata credit-id 
            (merge credit-data { retired: true })
        )
        
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-credit-info (credit-id uint))
    (map-get? credit-metadata credit-id)
)

(define-read-only (get-credit-owner (credit-id uint))
    (nft-get-owner? carbon-credit credit-id)
)


(define-map credit-ratings uint uint) ;; credit-id -> rating (1-5)

(define-public (rate-credit (credit-id uint) (rating uint))
    (begin
        (asserts! (and (>= rating u1) (<= rating u5)) (err u103))
        (map-set credit-ratings credit-id rating)
        (ok true)
    )
)

(define-read-only (get-credit-rating (credit-id uint))
    (default-to u0 (map-get? credit-ratings credit-id))
)



(define-map market-listings 
    uint 
    {price: uint, listed: bool}
)

(define-public (list-credit (credit-id uint) (price uint))
    (let ((credit-data (unwrap! (map-get? credit-metadata credit-id) ERR-INVALID-CREDIT)))
        (asserts! (is-eq tx-sender (get owner credit-data)) ERR-NOT-AUTHORIZED)
        (map-set market-listings credit-id {price: price, listed: true})
        (ok true)
    )
)


(define-map credit-history 
    uint 
    (list 10 {action: (string-ascii 20), timestamp: uint, actor: principal})
)

(define-public (record-action (credit-id uint) (action (string-ascii 20)))
    (let ((history (default-to (list) (map-get? credit-history credit-id))))
        (map-set credit-history 
            credit-id 
            (unwrap! (as-max-len? (append history {action: action, timestamp: stacks-block-height, actor: tx-sender}) u10) (err u104))
        )
        (ok true)
    )
)

(define-read-only (get-market-listing (credit-id uint))
    (map-get? market-listings credit-id)
)

(define-read-only (get-credit-history (credit-id uint))
    (default-to (list) (map-get? credit-history credit-id))
)


(define-public (unlist-credit (credit-id uint))
    (let ((credit-data (unwrap! (map-get? credit-metadata credit-id) ERR-INVALID-CREDIT)))
        (asserts! (is-eq tx-sender (get owner credit-data)) ERR-NOT-AUTHORIZED)
        (map-set market-listings credit-id {price: u0, listed: false})
        (ok true)
    )
)


(define-constant EXPIRATION-BLOCKS u52560) ;; ~1 year in blocks

(define-public (check-expiration (credit-id uint))
    (let ((credit-data (unwrap! (map-get? credit-metadata credit-id) ERR-INVALID-CREDIT)))
        (if (> stacks-block-height (+ (get verification-date credit-data) EXPIRATION-BLOCKS))
            (begin
                (map-set credit-metadata credit-id (merge credit-data {retired: true}))
                (ok true)
            )
            (ok false)
        )
    )
)



(define-map credit-bundles 
    uint 
    (list 10 uint)
)

(define-data-var bundle-counter uint u0)

(define-public (create-bundle (credit-ids (list 10 uint)))
    (let ((bundle-id (var-get bundle-counter)))
        (var-set bundle-counter (+ bundle-id u1))
        (map-set credit-bundles bundle-id credit-ids)
        (ok bundle-id)
    )
)



(define-map verification-status
    uint 
    {verified: bool, verifier: (optional principal)}
)

(define-constant VERIFIER-ADDRESS 'SP000000000000000000002Q6VF78)

(define-public (verify-credit (credit-id uint))
    (begin
        (asserts! (is-eq tx-sender VERIFIER-ADDRESS) ERR-NOT-AUTHORIZED)
        (map-set verification-status credit-id {
            verified: true,
            verifier: (some tx-sender)
        })
        (ok true)
    )
)



;; Purchase a listed credit
(define-public (purchase-credit (credit-id uint))
    (let 
        (
            (listing (unwrap! (map-get? market-listings credit-id) (err u105)))
            (credit-data (unwrap! (map-get? credit-metadata credit-id) ERR-INVALID-CREDIT))
            (seller (get owner credit-data))
            (price (get price listing))
        )
        ;; Check if credit is listed
        (asserts! (get listed listing) (err u106))
        
        ;; Check if credit is not retired
        (asserts! (not (get retired credit-data)) ERR-ALREADY-RETIRED)
        
        ;; Transfer STX from buyer to seller
        (try! (stx-transfer? price tx-sender seller))
        
        ;; Transfer NFT to buyer
        (try! (nft-transfer? carbon-credit credit-id seller tx-sender))
        
        ;; Update metadata
        (map-set credit-metadata credit-id 
            (merge credit-data { owner: tx-sender })
        )
        
        ;; Remove listing
        (map-set market-listings credit-id {price: u0, listed: false})
        
        ;; Record transaction in history
        (try! (record-action credit-id "purchased"))
        
        (ok true)
    )
)



;; Define certification levels
(define-constant CERTIFICATION-STANDARD "STANDARD")
(define-constant CERTIFICATION-GOLD "GOLD")
(define-constant CERTIFICATION-SILVER "SILVER")
(define-constant CERTIFICATION-BRONZE "BRONZE")

;; Map to store certification data
(define-map credit-certifications
    uint
    {
        level: (string-ascii 10),
        certifier: principal,
        certification-date: uint
    }
)

;; Only authorized certifiers can certify credits
(define-map authorized-certifiers principal bool)

;; Initialize authorized certifiers
(map-set authorized-certifiers 'SP000000000000000000002Q6VF78 true)

;; Certify a credit
(define-public (certify-credit (credit-id uint) (level (string-ascii 10)))
    (let
        ((is-authorized (default-to false (map-get? authorized-certifiers tx-sender))))
        
        ;; Check if caller is authorized
        (asserts! is-authorized ERR-NOT-AUTHORIZED)
        
        ;; Validate certification level
        (asserts! (or 
            (is-eq level CERTIFICATION-STANDARD)
            (is-eq level CERTIFICATION-GOLD)
            (is-eq level CERTIFICATION-SILVER)
            (is-eq level CERTIFICATION-BRONZE)
        ) (err u107))
        
        ;; Set certification
        (map-set credit-certifications credit-id {
            level: level,
            certifier: tx-sender,
            certification-date: stacks-block-height
        })
        
        ;; Record action
        (try! (record-action credit-id "certified"))
        
        (ok true)
    )
)

;; Get certification info
(define-read-only (get-certification (credit-id uint))
    (map-get? credit-certifications credit-id)
)



(define-map credit-projects
    uint
    {
        project-name: (string-ascii 50),
        project-location: (string-ascii 50),
        project-type: (string-ascii 50),
        project-start-date: uint,
        project-end-date: uint
    }
)

(define-public (add-project (credit-id uint) (project-name (string-ascii 50)) (project-location (string-ascii 50)) (project-type (string-ascii 50)) (project-start-date uint) (project-end-date uint))
    (let ((credit-data (unwrap! (map-get? credit-metadata credit-id) ERR-INVALID-CREDIT)))
        (asserts! (is-eq tx-sender (get owner credit-data)) ERR-NOT-AUTHORIZED)
        (map-set credit-projects credit-id {
            project-name: project-name,
            project-location: project-location,
            project-type: project-type,
            project-start-date: project-start-date,
            project-end-date: project-end-date
        })
        (ok true)
    )
)


;; Define fungible token for fractions
(define-fungible-token credit-fraction)

;; Map to track which credits have been fractionalized
(define-map fractionalized-credits
    uint
    {
        total-fractions: uint,
        remaining-fractions: uint,
        price-per-fraction: uint,
        is-fractionalized: bool
    }
)

;; Fractionalize a carbon credit
(define-public (fractionalize-credit (credit-id uint) (num-fractions uint) (price-per-fraction uint))
    (let 
        (
            (credit-data (unwrap! (map-get? credit-metadata credit-id) ERR-INVALID-CREDIT))
        )
        ;; Verify ownership
        (asserts! (is-eq tx-sender (get owner credit-data)) ERR-NOT-AUTHORIZED)
        
        ;; Check if credit is not retired
        (asserts! (not (get retired credit-data)) ERR-ALREADY-RETIRED)
        
        ;; Check if not already fractionalized
        (asserts! (is-none (map-get? fractionalized-credits credit-id)) (err u113))
        
        ;; Transfer NFT to contract
        (try! (nft-transfer? carbon-credit credit-id tx-sender (as-contract tx-sender)))
        
        ;; Mint fractions to owner
        (try! (ft-mint? credit-fraction num-fractions tx-sender))
        
        ;; Record fractionalization
        (map-set fractionalized-credits credit-id {
            total-fractions: num-fractions,
            remaining-fractions: num-fractions,
            price-per-fraction: price-per-fraction,
            is-fractionalized: true
        })
        
        ;; Update metadata owner to contract
        (map-set credit-metadata credit-id 
            (merge credit-data { owner: (as-contract tx-sender) })
        )
        
        ;; Record action
        (try! (record-action credit-id "fractionalized"))
        
        (ok true)
    )
)

;; Buy fractions of a credit
(define-public (buy-fractions (credit-id uint) (num-fractions uint))
    (let 
        (
            (fraction-data (unwrap! (map-get? fractionalized-credits credit-id) (err u114)))
            (price-per-fraction (get price-per-fraction fraction-data))
            (total-cost (* num-fractions price-per-fraction))
            (credit-data (unwrap! (map-get? credit-metadata credit-id) ERR-INVALID-CREDIT))
            (original-owner (get owner credit-data))
        )
        ;; Check if credit is fractionalized
        (asserts! (get is-fractionalized fraction-data) (err u115))
        
        ;; Check if enough fractions remain
        (asserts! (>= (get remaining-fractions fraction-data) num-fractions) (err u116))
        
        ;; Transfer STX payment
        (try! (stx-transfer? total-cost tx-sender original-owner))
        
        ;; Transfer fractions to buyer
        (try! (ft-transfer? credit-fraction num-fractions original-owner tx-sender))
        
        ;; Update remaining fractions
        (map-set fractionalized-credits credit-id
            (merge fraction-data {
                remaining-fractions: (- (get remaining-fractions fraction-data) num-fractions)
            })
        )
        
        (ok true)
    )
)

;; Get fraction balance
(define-read-only (get-fraction-balance (owner principal))
    (ft-get-balance credit-fraction owner)
)

;; Get fractionalization info
(define-read-only (get-fractionalization-info (credit-id uint))
    (map-get? fractionalized-credits credit-id)
)


;; Retirement proof structure
(define-map retirement-proofs
    uint
    {
        retiree: principal,
        retirement-date: uint,
        retirement-reason: (string-utf8 100),
        verification-hash: (buff 32),
        beneficiary: (optional principal)
    }
)

;; Retire credit with proof
(define-public (retire-credit-with-proof (credit-id uint) (retirement-reason (string-utf8 100)) (beneficiary (optional principal)))
    (let 
        (
            (credit-data (unwrap! (map-get? credit-metadata credit-id) ERR-INVALID-CREDIT))
            (current-block stacks-block-height)
            (verification-hash (sha256 (concat (concat (unwrap-panic (to-consensus-buff? tx-sender)) 
                                                      (unwrap-panic (to-consensus-buff? credit-id)))
                                              (unwrap-panic (to-consensus-buff? current-block)))))
        )
        ;; Verify ownership
        (asserts! (is-eq tx-sender (get owner credit-data)) ERR-NOT-AUTHORIZED)
        
        ;; Check if already retired
        (asserts! (not (get retired credit-data)) ERR-ALREADY-RETIRED)
        
        ;; Update metadata to mark as retired
        (map-set credit-metadata credit-id 
            (merge credit-data { retired: true })
        )
        
        ;; Create retirement proof
        (map-set retirement-proofs credit-id {
            retiree: tx-sender,
            retirement-date: current-block,
            retirement-reason: retirement-reason,
            verification-hash: verification-hash,
            beneficiary: beneficiary
        })
        
        ;; Record action
        (try! (record-action credit-id "retired-with-proof"))
        
        (ok verification-hash)
    )
)

;; Verify retirement proof
(define-read-only (verify-retirement-proof (credit-id uint) (claimed-hash (buff 32)))
    (let ((proof (map-get? retirement-proofs credit-id)))
        (match proof
            proof-data (is-eq claimed-hash (get verification-hash proof-data))
            false
        )
    )
)

;; Get retirement proof
(define-read-only (get-retirement-proof (credit-id uint))
    (map-get? retirement-proofs credit-id)
)