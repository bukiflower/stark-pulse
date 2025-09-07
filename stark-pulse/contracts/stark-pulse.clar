;; Simplified StarkPulse Gaming Protocol

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-INSUFFICIENT-BALANCE (err u1001))
(define-constant ERR-INVALID-AMOUNT (err u1002))
(define-constant ERR-PROTOCOL-PAUSED (err u1003))
(define-constant ERR-INSUFFICIENT-RESONANCE (err u1004))
(define-constant ERR-USER-NOT-FOUND (err u1005))
(define-constant ERR-INVALID-REALM-ID (err u1006))
(define-constant ERR-REALM-NOT-FOUND (err u1007))
(define-constant ERR-NOT-REALM-OWNER (err u1008))
(define-constant ERR-DIVISION-BY-ZERO (err u1009))

;; Protocol Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-RESONANCE-RATIO u150) ;; 150%
(define-constant MAX-PULSE-SCORE u1000)
(define-constant MIN-REALM-DEPOSIT u1000)
(define-constant INITIAL-TOKEN-SUPPLY u1000000)

;; Data Variables
(define-data-var protocol-paused bool false)
(define-data-var total-pulse-supply uint u0)
(define-data-var total-resonance-supply uint INITIAL-TOKEN-SUPPLY)
(define-data-var total-essence-supply uint INITIAL-TOKEN-SUPPLY)
(define-data-var base-resonance-ratio uint u150)
(define-data-var next-realm-id uint u1)

;; Data Maps
(define-map player-pulse-scores principal uint)
(define-map player-balances-pulse principal uint)
(define-map player-balances-resonance principal uint)
(define-map player-balances-essence principal uint)
(define-map player-exploration-history principal 
  {
    total-explored: uint, 
    last-exploration-block: uint
  })
(define-map player-resonance-positions principal 
  {
    resonance-amount: uint, 
    debt-amount: uint, 
    resonance-ratio: uint
  })
(define-map dimensional-realms uint 
  {
    owner: principal, 
    balance: uint, 
    created-at: uint
  })
(define-map realm-counter principal uint)

;; Authorization Functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER))

;; Input Validation Functions
(define-private (validate-amount (amount uint))
  (> amount u0))

(define-private (check-protocol-status)
  (not (var-get protocol-paused)))

(define-private (realm-exists (realm-id uint))
  (is-some (map-get? dimensional-realms realm-id)))

(define-private (is-realm-owner (realm-id uint) (player principal))
  (match (map-get? dimensional-realms realm-id)
    realm (is-eq (get owner realm) player)
    false))

;; Helper functions
(define-private (min-uint (a uint) (b uint))
  (if (<= a b) a b))

(define-private (max-uint (a uint) (b uint))
  (if (>= a b) a b))

;; Pulse Score Calculation (simplified)
(define-private (calculate-pulse-score (player principal))
  (let (
    (exploration-data (default-to 
      {total-explored: u0, last-exploration-block: u0} 
      (map-get? player-exploration-history player)))
    (base-score u100)
    (exploration-bonus (/ (get total-explored exploration-data) u1000))
    (total-score (+ base-score exploration-bonus))
  )
  (min-uint total-score MAX-PULSE-SCORE)))

;; Initial Token Distribution
(define-public (initialize-player-tokens (player principal) (resonance-amount uint) (essence-amount uint))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (validate-amount resonance-amount) ERR-INVALID-AMOUNT)
    (asserts! (validate-amount essence-amount) ERR-INVALID-AMOUNT)
    
    (map-set player-balances-resonance player 
             (+ (default-to u0 (map-get? player-balances-resonance player)) resonance-amount))
    (map-set player-balances-essence player 
             (+ (default-to u0 (map-get? player-balances-essence player)) essence-amount))
    
    (ok true)))

;; Admin Functions
(define-public (pause-protocol)
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (var-set protocol-paused true)
    (ok true)))

(define-public (unpause-protocol)
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (var-set protocol-paused false)
    (ok true)))

(define-public (update-base-resonance-ratio (new-ratio uint))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (>= new-ratio u100) ERR-INVALID-AMOUNT)
    (var-set base-resonance-ratio new-ratio)
    (ok true)))

;; Core Protocol Functions
(define-public (mint-pulse (resonance-amount uint))
  (let (
    (player tx-sender)
    (required-ratio (var-get base-resonance-ratio))
    (player-resonance-balance (default-to u0 (map-get? player-balances-resonance player)))
  )
  (asserts! (check-protocol-status) ERR-PROTOCOL-PAUSED)
  (asserts! (validate-amount resonance-amount) ERR-INVALID-AMOUNT)
  (asserts! (>= player-resonance-balance resonance-amount) ERR-INSUFFICIENT-BALANCE)
  (asserts! (> required-ratio u0) ERR-DIVISION-BY-ZERO)
  
  (let (
    (mint-amount (/ (* resonance-amount u100) required-ratio))
  )
  (asserts! (>= resonance-amount (* mint-amount required-ratio)) ERR-INSUFFICIENT-RESONANCE)
  
  ;; Deduct resonance from player
  (map-set player-balances-resonance player (- player-resonance-balance resonance-amount))
  
  ;; Add pulse to player
  (map-set player-balances-pulse player 
           (+ (default-to u0 (map-get? player-balances-pulse player)) mint-amount))
  
  ;; Record position
  (map-set player-resonance-positions player 
           {
             resonance-amount: resonance-amount, 
             debt-amount: mint-amount, 
             resonance-ratio: required-ratio
           })
  
  (var-set total-pulse-supply (+ (var-get total-pulse-supply) mint-amount))
  
  (ok mint-amount))))

(define-public (redeem-pulse (pulse-amount uint))
  (let (
    (player tx-sender)
    (player-balance (default-to u0 (map-get? player-balances-pulse player)))
    (position (map-get? player-resonance-positions player))
  )
  (asserts! (check-protocol-status) ERR-PROTOCOL-PAUSED)
  (asserts! (validate-amount pulse-amount) ERR-INVALID-AMOUNT)
  (asserts! (>= player-balance pulse-amount) ERR-INSUFFICIENT-BALANCE)
  (asserts! (is-some position) ERR-USER-NOT-FOUND)
  
  (let (
    (position-data (unwrap! position ERR-USER-NOT-FOUND))
    (debt-amount (get debt-amount position-data))
  )
  (asserts! (> debt-amount u0) ERR-DIVISION-BY-ZERO)
  
  (let (
    (resonance-to-return (/ (* pulse-amount (get resonance-amount position-data)) debt-amount))
  )
  (map-set player-balances-pulse player (- player-balance pulse-amount))
  (map-set player-balances-resonance player 
           (+ (default-to u0 (map-get? player-balances-resonance player)) resonance-to-return))
  (var-set total-pulse-supply (- (var-get total-pulse-supply) pulse-amount))
  
  ;; Update position
  (if (is-eq pulse-amount debt-amount)
    (map-delete player-resonance-positions player)
    (map-set player-resonance-positions player
             {
               resonance-amount: (- (get resonance-amount position-data) resonance-to-return),
               debt-amount: (- debt-amount pulse-amount),
               resonance-ratio: (get resonance-ratio position-data)
             }))
  
  (ok resonance-to-return)))))

(define-public (explore-resonance (amount uint))
  (let (
    (player tx-sender)
    (current-balance (default-to u0 (map-get? player-balances-resonance player)))
    (current-exploration (default-to 
      {total-explored: u0, last-exploration-block: u0} 
      (map-get? player-exploration-history player)))
  )
  (asserts! (check-protocol-status) ERR-PROTOCOL-PAUSED)
  (asserts! (validate-amount amount) ERR-INVALID-AMOUNT)
  (asserts! (>= current-balance amount) ERR-INSUFFICIENT-BALANCE)
  
  (map-set player-balances-resonance player (- current-balance amount))
  (map-set player-exploration-history player 
           {
             total-explored: (+ (get total-explored current-exploration) amount),
             last-exploration-block: block-height
           })
  
  ;; Update Pulse Score after exploration
  (map-set player-pulse-scores player (calculate-pulse-score player))
  
  (ok true)))

(define-public (withdraw-from-exploration (amount uint))
  (let (
    (player tx-sender)
    (current-balance (default-to u0 (map-get? player-balances-resonance player)))
    (exploration-data (map-get? player-exploration-history player))
  )
  (asserts! (check-protocol-status) ERR-PROTOCOL-PAUSED)
  (asserts! (validate-amount amount) ERR-INVALID-AMOUNT)
  (asserts! (is-some exploration-data) ERR-USER-NOT-FOUND)
  
  (let (
    (exploration-info (unwrap! exploration-data ERR-USER-NOT-FOUND))
    (total-explored (get total-explored exploration-info))
  )
  (asserts! (>= total-explored amount) ERR-INSUFFICIENT-BALANCE)
  
  (map-set player-balances-resonance player (+ current-balance amount))
  (map-set player-exploration-history player 
           {
             total-explored: (- total-explored amount),
             last-exploration-block: block-height
           })
  
  ;; Update Pulse Score after withdrawal
  (map-set player-pulse-scores player (calculate-pulse-score player))
  
  (ok true))))

;; Simplified Realm Functions
(define-public (create-dimensional-realm (initial-deposit uint))
  (let (
    (player tx-sender)
    (realm-id (var-get next-realm-id))
    (player-balance (default-to u0 (map-get? player-balances-pulse player)))
  )
  (asserts! (check-protocol-status) ERR-PROTOCOL-PAUSED)
  (asserts! (validate-amount initial-deposit) ERR-INVALID-AMOUNT)
  (asserts! (>= initial-deposit MIN-REALM-DEPOSIT) ERR-INVALID-AMOUNT)
  (asserts! (>= player-balance initial-deposit) ERR-INSUFFICIENT-BALANCE)
  
  ;; Deduct balance and create realm
  (map-set player-balances-pulse player (- player-balance initial-deposit))
  (map-set dimensional-realms realm-id 
           {
             owner: player,
             balance: initial-deposit,
             created-at: block-height
           })
  
  ;; Update realm counter for player
  (map-set realm-counter player 
           (+ (default-to u0 (map-get? realm-counter player)) u1))
  
  ;; Increment global realm ID
  (var-set next-realm-id (+ realm-id u1))
  
  (ok realm-id)))

(define-public (deposit-to-realm (realm-id uint) (amount uint))
  (let (
    (player tx-sender)
    (realm-data (map-get? dimensional-realms realm-id))
    (player-balance (default-to u0 (map-get? player-balances-pulse player)))
  )
  (asserts! (check-protocol-status) ERR-PROTOCOL-PAUSED)
  (asserts! (validate-amount amount) ERR-INVALID-AMOUNT)
  (asserts! (>= player-balance amount) ERR-INSUFFICIENT-BALANCE)
  (asserts! (is-some realm-data) ERR-REALM-NOT-FOUND)
  (asserts! (is-realm-owner realm-id player) ERR-NOT-REALM-OWNER)
  
  (let (
    (realm-info (unwrap! realm-data ERR-REALM-NOT-FOUND))
  )
  ;; Deduct from player balance
  (map-set player-balances-pulse player (- player-balance amount))
  
  ;; Add to realm balance
  (map-set dimensional-realms realm-id
           {
             owner: (get owner realm-info),
             balance: (+ (get balance realm-info) amount),
             created-at: (get created-at realm-info)
           })
  
  (ok true))))

(define-public (withdraw-from-realm (realm-id uint) (amount uint))
  (let (
    (player tx-sender)
    (realm-data (map-get? dimensional-realms realm-id))
  )
  (asserts! (check-protocol-status) ERR-PROTOCOL-PAUSED)
  (asserts! (validate-amount amount) ERR-INVALID-AMOUNT)
  (asserts! (is-some realm-data) ERR-REALM-NOT-FOUND)
  (asserts! (is-realm-owner realm-id player) ERR-NOT-REALM-OWNER)
  
  (let (
    (realm-info (unwrap! realm-data ERR-REALM-NOT-FOUND))
    (realm-balance (get balance realm-info))
  )
  (asserts! (>= realm-balance amount) ERR-INSUFFICIENT-BALANCE)
  
  ;; Update realm balance
  (map-set dimensional-realms realm-id
           {
             owner: (get owner realm-info),
             balance: (- realm-balance amount),
             created-at: (get created-at realm-info)
           })
  
  ;; Add to player balance
  (map-set player-balances-pulse player 
           (+ (default-to u0 (map-get? player-balances-pulse player)) amount))
  
  (ok true))))

(define-public (craft-in-realm (realm-id uint) (essence-amount uint))
  (let (
    (player tx-sender)
    (realm-data (map-get? dimensional-realms realm-id))
    (player-essence-balance (default-to u0 (map-get? player-balances-essence player)))
  )
  (asserts! (check-protocol-status) ERR-PROTOCOL-PAUSED)
  (asserts! (validate-amount essence-amount) ERR-INVALID-AMOUNT)
  (asserts! (>= player-essence-balance essence-amount) ERR-INSUFFICIENT-BALANCE)
  (asserts! (is-some realm-data) ERR-REALM-NOT-FOUND)
  (asserts! (is-realm-owner realm-id player) ERR-NOT-REALM-OWNER)
  
  (let (
    (realm-info (unwrap! realm-data ERR-REALM-NOT-FOUND))
    (craft-rate (max-uint u1 (/ (get balance realm-info) u1000)))
    (crafted-amount (/ (* essence-amount craft-rate) u100))
  )
  ;; Consume essence
  (map-set player-balances-essence player (- player-essence-balance essence-amount))
  
  ;; Award pulse based on craft rate
  (map-set player-balances-pulse player 
           (+ (default-to u0 (map-get? player-balances-pulse player)) crafted-amount))
  
  (var-set total-pulse-supply (+ (var-get total-pulse-supply) crafted-amount))
  
  (ok crafted-amount))))

;; Read-only Functions
(define-read-only (get-player-balances (player principal))
  {
    pulse: (default-to u0 (map-get? player-balances-pulse player)),
    resonance: (default-to u0 (map-get? player-balances-resonance player)),
    essence: (default-to u0 (map-get? player-balances-essence player))
  })

(define-read-only (get-player-pulse-score (player principal))
  (calculate-pulse-score player))

(define-read-only (get-player-exploration-data (player principal))
  (default-to 
    {total-explored: u0, last-exploration-block: u0} 
    (map-get? player-exploration-history player)))

(define-read-only (get-player-resonance-position (player principal))
  (map-get? player-resonance-positions player))

(define-read-only (get-realm-info (realm-id uint))
  (map-get? dimensional-realms realm-id))

(define-read-only (get-player-realm-count (player principal))
  (default-to u0 (map-get? realm-counter player)))

(define-read-only (get-protocol-stats)
  {
    total-pulse-supply: (var-get total-pulse-supply),
    total-resonance-supply: (var-get total-resonance-supply),
    total-essence-supply: (var-get total-essence-supply),
    protocol-paused: (var-get protocol-paused),
    base-resonance-ratio: (var-get base-resonance-ratio),
    next-realm-id: (var-get next-realm-id)
  })

(define-read-only (estimate-mint-amount (resonance-amount uint))
  (let (
    (required-ratio (var-get base-resonance-ratio))
  )
  (if (> required-ratio u0)
    (ok (/ (* resonance-amount u100) required-ratio))
    ERR-DIVISION-BY-ZERO)))

(define-read-only (estimate-redeem-amount (player principal) (pulse-amount uint))
  (let (
    (position (map-get? player-resonance-positions player))
  )
  (match position
    pos (let (
          (debt-amount (get debt-amount pos))
        )
        (if (> debt-amount u0)
          (ok (/ (* pulse-amount (get resonance-amount pos)) debt-amount))
          ERR-DIVISION-BY-ZERO))
    ERR-USER-NOT-FOUND)))

(define-read-only (get-realm-craft-rate (realm-id uint))
  (match (map-get? dimensional-realms realm-id)
    realm (ok (max-uint u1 (/ (get balance realm) u1000)))
    ERR-REALM-NOT-FOUND))

(define-read-only (estimate-craft-output (realm-id uint) (essence-amount uint))
  (match (map-get? dimensional-realms realm-id)
    realm (let (
            (craft-rate (max-uint u1 (/ (get balance realm) u1000)))
          )
          (ok (/ (* essence-amount craft-rate) u100)))
    ERR-REALM-NOT-FOUND))