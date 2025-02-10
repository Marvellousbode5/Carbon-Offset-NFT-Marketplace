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
