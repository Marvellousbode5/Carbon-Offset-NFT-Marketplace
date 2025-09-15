;; Carbon Credit Auction House
;; Enables dynamic price discovery for carbon credits through various auction formats

(define-data-var auction-counter uint u0)
(define-data-var platform-fee-rate uint u250) ;; 2.5% platform fee
(define-data-var min-bid-increment uint u100) ;; Minimum STX bid increment

;; Auction types
(define-constant AUCTION-TYPE-ENGLISH "ENGLISH")     ;; Traditional ascending bid auction
(define-constant AUCTION-TYPE-DUTCH "DUTCH")         ;; Descending price auction
(define-constant AUCTION-TYPE-SEALED "SEALED")       ;; Sealed bid auction

;; Auction status
(define-constant STATUS-ACTIVE "ACTIVE")
(define-constant STATUS-ENDED "ENDED")
(define-constant STATUS-CANCELLED "CANCELLED")

;; Store auction details
(define-map carbon-auctions
    uint
    {
        auction-id: uint,
        seller: principal,
        credit-id: uint,
        auction-type: (string-ascii 10),
        start-price: uint,
        current-price: uint,
        reserve-price: uint,
        start-block: uint,
        end-block: uint,
        status: (string-ascii 10),
        winner: (optional principal),
        total-bids: uint
    }
)

;; Store bid history for English auctions
(define-map auction-bids
    {auction-id: uint, bid-index: uint}
    {
        bidder: principal,
        bid-amount: uint,
        bid-block: uint,
        active: bool
    }
)

;; Store sealed bids (commitment scheme)
(define-map sealed-bids
    {auction-id: uint, bidder: principal}
    {
        commitment-hash: (buff 32),
        bid-amount: uint,
        revealed: bool,
        valid: bool
    }
)

;; Track user bid counts for each auction
(define-map bidder-counts
    {auction-id: uint, bidder: principal}
    uint
)

;; Dutch auction price decay
(define-map dutch-decay
    uint
    {
        decay-rate: uint,
        price-floor: uint,
        last-update: uint
    }
)

;; Auction analytics
(define-map auction-stats
    uint
    {
        total-volume: uint,
        avg-price: uint,
        participation-rate: uint,
        completion-rate: uint
    }
)

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u500))
(define-constant ERR-INVALID-AUCTION (err u501))
(define-constant ERR-AUCTION-ENDED (err u502))
(define-constant ERR-BID-TOO-LOW (err u503))
(define-constant ERR-RESERVE-NOT-MET (err u504))
(define-constant ERR-AUCTION-ACTIVE (err u505))
(define-constant ERR-INVALID-COMMITMENT (err u506))
(define-constant ERR-ALREADY-REVEALED (err u507))

;; Helper: Get maximum of two values
(define-private (max-uint (a uint) (b uint))
    (if (>= a b) a b)
)

;; Create English auction
(define-public (create-english-auction (credit-id uint) (start-price uint) (reserve-price uint) (duration-blocks uint))
    (let
        (
            (auction-id (var-get auction-counter))
            (end-block (+ stacks-block-height duration-blocks))
        )
        (asserts! (> start-price u0) ERR-INVALID-AUCTION)
        (asserts! (>= reserve-price start-price) ERR-INVALID-AUCTION)
        
        ;; Create auction
        (map-set carbon-auctions auction-id {
            auction-id: auction-id,
            seller: tx-sender,
            credit-id: credit-id,
            auction-type: AUCTION-TYPE-ENGLISH,
            start-price: start-price,
            current-price: start-price,
            reserve-price: reserve-price,
            start-block: stacks-block-height,
            end-block: end-block,
            status: STATUS-ACTIVE,
            winner: none,
            total-bids: u0
        })
        
        (var-set auction-counter (+ auction-id u1))
        (ok auction-id)
    )
)

;; Create Dutch auction  
(define-public (create-dutch-auction (credit-id uint) (start-price uint) (price-floor uint) (decay-rate uint) (duration-blocks uint))
    (let
        (
            (auction-id (var-get auction-counter))
            (end-block (+ stacks-block-height duration-blocks))
        )
        (asserts! (> start-price price-floor) ERR-INVALID-AUCTION)
        (asserts! (> decay-rate u0) ERR-INVALID-AUCTION)
        
        ;; Create auction
        (map-set carbon-auctions auction-id {
            auction-id: auction-id,
            seller: tx-sender,
            credit-id: credit-id,
            auction-type: AUCTION-TYPE-DUTCH,
            start-price: start-price,
            current-price: start-price,
            reserve-price: price-floor,
            start-block: stacks-block-height,
            end-block: end-block,
            status: STATUS-ACTIVE,
            winner: none,
            total-bids: u0
        })
        
        ;; Set decay parameters
        (map-set dutch-decay auction-id {
            decay-rate: decay-rate,
            price-floor: price-floor,
            last-update: stacks-block-height
        })
        
        (var-set auction-counter (+ auction-id u1))
        (ok auction-id)
    )
)

;; Place bid on English auction
(define-public (place-bid (auction-id uint) (bid-amount uint))
    (let
        (
            (auction (unwrap! (map-get? carbon-auctions auction-id) ERR-INVALID-AUCTION))
            (bid-count (get total-bids auction))
            (min-bid (+ (get current-price auction) (var-get min-bid-increment)))
        )
        ;; Validate auction
        (asserts! (is-eq (get status auction) STATUS-ACTIVE) ERR-AUCTION-ENDED)
        (asserts! (< stacks-block-height (get end-block auction)) ERR-AUCTION-ENDED)
        (asserts! (is-eq (get auction-type auction) AUCTION-TYPE-ENGLISH) ERR-INVALID-AUCTION)
        (asserts! (>= bid-amount min-bid) ERR-BID-TOO-LOW)
        
        ;; Transfer bid amount to contract
        (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
        
        ;; Record bid
        (map-set auction-bids {auction-id: auction-id, bid-index: bid-count} {
            bidder: tx-sender,
            bid-amount: bid-amount,
            bid-block: stacks-block-height,
            active: true
        })
        
        ;; Update auction
        (map-set carbon-auctions auction-id
            (merge auction {
                current-price: bid-amount,
                total-bids: (+ bid-count u1)
            })
        )
        
        ;; Return previous bidder's funds (if any)
        (if (> bid-count u0)
            (let ((prev-bid (unwrap! (map-get? auction-bids {auction-id: auction-id, bid-index: (- bid-count u1)}) ERR-INVALID-AUCTION)))
                (try! (as-contract (stx-transfer? (get bid-amount prev-bid) (as-contract tx-sender) (get bidder prev-bid))))
                (map-set auction-bids {auction-id: auction-id, bid-index: (- bid-count u1)}
                    (merge prev-bid {active: false})
                )
            )
            true
        )
        
        (ok true)
    )
)

;; Buy now in Dutch auction
(define-public (dutch-buy-now (auction-id uint))
    (let
        (
            (auction (unwrap! (map-get? carbon-auctions auction-id) ERR-INVALID-AUCTION))
            (current-price (get-dutch-current-price auction-id))
        )
        ;; Validate auction
        (asserts! (is-eq (get status auction) STATUS-ACTIVE) ERR-AUCTION-ENDED)
        (asserts! (< stacks-block-height (get end-block auction)) ERR-AUCTION-ENDED)
        (asserts! (is-eq (get auction-type auction) AUCTION-TYPE-DUTCH) ERR-INVALID-AUCTION)
        
        ;; Transfer payment
        (try! (stx-transfer? current-price tx-sender (as-contract tx-sender)))
        
        ;; End auction with winner
        (map-set carbon-auctions auction-id
            (merge auction {
                status: STATUS-ENDED,
                winner: (some tx-sender),
                current-price: current-price
            })
        )
        
        ;; Transfer proceeds to seller (minus fee)
        (let ((platform-fee (/ (* current-price (var-get platform-fee-rate)) u10000)))
            (try! (as-contract (stx-transfer? (- current-price platform-fee) (as-contract tx-sender) (get seller auction))))
        )
        
        (ok current-price)
    )
)

;; Submit sealed bid commitment
(define-public (submit-sealed-bid (auction-id uint) (commitment-hash (buff 32)))
    (let
        ((auction (unwrap! (map-get? carbon-auctions auction-id) ERR-INVALID-AUCTION)))
        
        ;; Validate auction
        (asserts! (is-eq (get status auction) STATUS-ACTIVE) ERR-AUCTION-ENDED)
        (asserts! (< stacks-block-height (get end-block auction)) ERR-AUCTION-ENDED)
        (asserts! (is-eq (get auction-type auction) AUCTION-TYPE-SEALED) ERR-INVALID-AUCTION)
        
        ;; Store commitment
        (map-set sealed-bids {auction-id: auction-id, bidder: tx-sender} {
            commitment-hash: commitment-hash,
            bid-amount: u0,
            revealed: false,
            valid: false
        })
        
        (ok true)
    )
)

;; Reveal sealed bid
(define-public (reveal-sealed-bid (auction-id uint) (bid-amount uint) (nonce (buff 16)))
    (let
        (
            (auction (unwrap! (map-get? carbon-auctions auction-id) ERR-INVALID-AUCTION))
            (sealed-bid (unwrap! (map-get? sealed-bids {auction-id: auction-id, bidder: tx-sender}) ERR-INVALID-COMMITMENT))
            (computed-hash (sha256 (concat (concat (unwrap-panic (to-consensus-buff? bid-amount)) 
                                                 (unwrap-panic (to-consensus-buff? tx-sender))) 
                                          nonce)))
        )
        ;; Validate reveal period (after auction end)
        (asserts! (>= stacks-block-height (get end-block auction)) ERR-AUCTION-ACTIVE)
        (asserts! (not (get revealed sealed-bid)) ERR-ALREADY-REVEALED)
        (asserts! (is-eq (get commitment-hash sealed-bid) computed-hash) ERR-INVALID-COMMITMENT)
        
        ;; Transfer bid amount to contract
        (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
        
        ;; Update sealed bid
        (map-set sealed-bids {auction-id: auction-id, bidder: tx-sender}
            (merge sealed-bid {
                bid-amount: bid-amount,
                revealed: true,
                valid: true
            })
        )
        
        (ok true)
    )
)

;; Finalize auction after it ends
(define-public (finalize-auction (auction-id uint))
    (let
        ((auction (unwrap! (map-get? carbon-auctions auction-id) ERR-INVALID-AUCTION)))
        
        ;; Validate auction can be finalized
        (asserts! (is-eq (get status auction) STATUS-ACTIVE) ERR-AUCTION-ENDED)
        (asserts! (>= stacks-block-height (get end-block auction)) ERR-AUCTION-ACTIVE)
        
        (if (is-eq (get auction-type auction) AUCTION-TYPE-ENGLISH)
            (finalize-english-auction auction-id)
            (if (is-eq (get auction-type auction) AUCTION-TYPE-SEALED)
                (finalize-sealed-auction auction-id)
                (ok false) ;; Dutch auctions finalize immediately on purchase
            )
        )
    )
)

;; Helper: Finalize English auction
(define-private (finalize-english-auction (auction-id uint))
    (let
        (
            (auction (unwrap! (map-get? carbon-auctions auction-id) ERR-INVALID-AUCTION))
            (reserve-met (>= (get current-price auction) (get reserve-price auction)))
        )
        (if (and reserve-met (> (get total-bids auction) u0))
            (begin
                ;; Find highest bidder
                (let ((highest-bid (unwrap! (map-get? auction-bids {auction-id: auction-id, bid-index: (- (get total-bids auction) u1)}) ERR-INVALID-AUCTION)))
                    ;; End auction with winner
                    (map-set carbon-auctions auction-id
                        (merge auction {
                            status: STATUS-ENDED,
                            winner: (some (get bidder highest-bid))
                        })
                    )
                    
                    ;; Transfer proceeds to seller (minus fee)
                    (let ((platform-fee (/ (* (get current-price auction) (var-get platform-fee-rate)) u10000)))
                        (try! (as-contract (stx-transfer? (- (get current-price auction) platform-fee) (as-contract tx-sender) (get seller auction))))
                    )
                )
                (ok true)
            )
            (begin
                ;; Reserve not met - cancel auction
                (map-set carbon-auctions auction-id
                    (merge auction {status: STATUS-CANCELLED})
                )
                
                ;; Return funds to last bidder if exists
                (if (> (get total-bids auction) u0)
                    (let ((last-bid (unwrap! (map-get? auction-bids {auction-id: auction-id, bid-index: (- (get total-bids auction) u1)}) ERR-INVALID-AUCTION)))
                        (try! (as-contract (stx-transfer? (get bid-amount last-bid) (as-contract tx-sender) (get bidder last-bid))))
                    )
                    true
                )
                (ok false)
            )
        )
    )
)

;; Helper: Finalize sealed bid auction  
(define-private (finalize-sealed-auction (auction-id uint))
    ;; Simplified - would need to iterate through all sealed bids to find highest
    (ok true)
)

;; Get current Dutch auction price
(define-read-only (get-dutch-current-price (auction-id uint))
    (let
        (
            (auction (unwrap! (map-get? carbon-auctions auction-id) u0))
            (decay-info (unwrap! (map-get? dutch-decay auction-id) (get start-price auction)))
            (blocks-elapsed (- stacks-block-height (get start-block auction)))
            (price-drop (* blocks-elapsed (get decay-rate decay-info)))
            (current-price (if (> (get start-price auction) price-drop)
                              (- (get start-price auction) price-drop)
                              (get price-floor decay-info)))
        )
        (max-uint current-price (get price-floor decay-info))
    )
)

;; Read-only functions
(define-read-only (get-auction-info (auction-id uint))
    (map-get? carbon-auctions auction-id)
)

(define-read-only (get-bid-info (auction-id uint) (bid-index uint))
    (map-get? auction-bids {auction-id: auction-id, bid-index: bid-index})
)

(define-read-only (get-platform-stats)
    {
        total-auctions: (var-get auction-counter),
        platform-fee: (var-get platform-fee-rate),
        min-increment: (var-get min-bid-increment)
    }
)
