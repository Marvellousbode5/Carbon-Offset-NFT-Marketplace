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