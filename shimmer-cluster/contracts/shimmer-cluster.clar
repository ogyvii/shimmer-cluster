;; Shimmer Cluster - Blockchain Gaming Ecosystem

;; =============================
;; CONSTANTS
;; =============================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-OWNER (err u100))
(define-constant ERR-NOT-TOKEN-OWNER (err u101))
(define-constant ERR-TOKEN-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))
(define-constant ERR-ALREADY-MINTED (err u104))
(define-constant ERR-EVOLUTION-LOCKED (err u105))
(define-constant ERR-INVALID-BREED (err u106))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u107))
(define-constant ERR-ALREADY-VOTED (err u108))
(define-constant ERR-PROPOSAL-CLOSED (err u109))
(define-constant ERR-NOT-ENOUGH-TOKENS (err u110))
(define-constant ERR-SAME-PARENT (err u111))

;; GLOW token constants
(define-constant GLOW-DECIMALS u6)
(define-constant GLOW-SYMBOL "GLOW")
(define-constant GLOW-NAME "GLOW Token")
(define-constant GLOW-URI (some u"https://shimmercluster.io/glow-token"))

;; Breeding / evolution constants
(define-constant BREED-COST u1000000)           ;; 1 GLOW (in micro-units)
(define-constant EVOLUTION-CHAMBER-BLOCKS u144) ;; ~24 hours at 10 min/block
(define-constant GENESIS-VOTING-MULTIPLIER u3)  ;; Genesis owners get 3x votes
(define-constant MIN-GOVERNANCE-TOKENS u100000) ;; 0.1 GLOW to submit proposals
(define-constant MAX-SUPPLY u10000)             ;; Hard cap on Shimmer NFTs

;; =============================
;; DATA VARIABLES
;; =============================

(define-data-var last-token-id uint u0)
(define-data-var last-proposal-id uint u0)
(define-data-var glow-total-supply uint u0)
(define-data-var contract-paused bool false)

;; =============================
;; DATA MAPS
;; =============================

;; NFT ownership (SIP-009 style)
(define-map token-owner
    { token-id: uint }
    { owner: principal }
)

;; Shimmer DNA: each trait is a uint 0-255
(define-map shimmer-dna
    { token-id: uint }
    {
        element:          uint, ;; 0=fire 1=water 2=earth 3=air 4=crystal
        rarity:           uint, ;; 0-255, higher = rarer
        agility:          uint,
        luminance:        uint,
        resilience:       uint,
        crystalline-core: uint  ;; evolves with achievements
    }
)

;; Genesis flag: true if minted in genesis batch
(define-map is-genesis
    { token-id: uint }
    { genesis: bool }
)

;; Evolution chamber lock: records block-height when evolution started
(define-map evolution-chamber
    { token-id: uint }
    { unlocks-at: uint, stage: uint }
)

;; Breeding lineage
(define-map shimmer-parents
    { token-id: uint }
    { parent-a: uint, parent-b: uint }
)

;; GLOW token balances
(define-map glow-balance
    { owner: principal }
    { amount: uint }
)

;; GLOW allowances
(define-map glow-allowance
    { owner: principal, spender: principal }
    { amount: uint }
)

;; Governance proposals
(define-map proposals
    { proposal-id: uint }
    {
        proposer:      principal,
        title:         (string-ascii 64),
        description:   (string-ascii 256),
        votes-for:     uint,
        votes-against: uint,
        ends-at-block: uint,
        executed:      bool
    }
)

;; Vote records to prevent double-voting
(define-map vote-record
    { proposal-id: uint, voter: principal }
    { voted: bool }
)

;; Planet habitat assignments
(define-map shimmer-planet
    { token-id: uint }
    { planet-id: uint }
)

;; =============================
;; PRIVATE UTILITIES
;; =============================

(define-private (min (a uint) (b uint))
    (if (<= a b) a b)
)

;; Simple uint-to-ascii for token URI construction.
;; Single-digit helper; production should use off-chain metadata indexing.
(define-private (uint-to-ascii (n uint))
    (if (is-eq n u0) "0"
    (if (is-eq n u1) "1"
    (if (is-eq n u2) "2"
    (if (is-eq n u3) "3"
    (if (is-eq n u4) "4"
    (if (is-eq n u5) "5"
    (if (is-eq n u6) "6"
    (if (is-eq n u7) "7"
    (if (is-eq n u8) "8"
    "9")))))))))
)

;; =============================
;; GLOW TOKEN INTERNALS
;; =============================

;; glow-mint does NOT return a response because it cannot fail.
;; Callers invoke it as a plain side-effect statement, which avoids
;; the indeterminate err-type error that occurs when try! wraps a
;; private function whose error branch is never reachable.
(define-private (glow-mint (recipient principal) (amount uint))
    (let ((current (default-to u0 (get amount (map-get? glow-balance { owner: recipient })))))
        (map-set glow-balance { owner: recipient } { amount: (+ current amount) })
        (var-set glow-total-supply (+ (var-get glow-total-supply) amount))
    )
)

;; glow-transfer CAN fail (insufficient balance), so it returns a typed
;; response. Called only via try! from public functions, fixing the err type.
(define-private (glow-transfer (amount uint) (sender principal) (recipient principal))
    (let ((sender-bal (default-to u0 (get amount (map-get? glow-balance { owner: sender })))))
        (asserts! (>= sender-bal amount) ERR-INSUFFICIENT-BALANCE)
        (map-set glow-balance { owner: sender }
            { amount: (- sender-bal amount) })
        (map-set glow-balance { owner: recipient }
            { amount: (+ (default-to u0 (get amount (map-get? glow-balance { owner: recipient }))) amount) })
        (ok true)
    )
)

;; =============================
;; SIP-009 NFT FUNCTIONS
;; =============================

(define-read-only (get-last-token-id)
    (ok (var-get last-token-id))
)

(define-read-only (get-token-uri (token-id uint))
    (ok (some (concat
        "https://shimmercluster.io/metadata/"
        (uint-to-ascii token-id)
    )))
)

(define-read-only (get-owner (token-id uint))
    (match (map-get? token-owner { token-id: token-id })
        entry (ok (some (get owner entry)))
        (ok none)
    )
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (let ((owner-entry (unwrap! (map-get? token-owner { token-id: token-id }) ERR-TOKEN-NOT-FOUND)))
        (asserts! (is-eq tx-sender sender) ERR-NOT-TOKEN-OWNER)
        (asserts! (is-eq (get owner owner-entry) sender) ERR-NOT-TOKEN-OWNER)
        (map-set token-owner { token-id: token-id } { owner: recipient })
        (ok true)
    )
)

;; =============================
;; MINTING
;; =============================

;; Internal helper: mint a new Shimmer with given DNA traits
(define-private (mint-shimmer
    (recipient  principal)
    (element    uint)
    (rarity     uint)
    (agility    uint)
    (luminance  uint)
    (resilience uint)
    (genesis    bool)
)
    (let ((new-id (+ (var-get last-token-id) u1)))
        (asserts! (<= new-id MAX-SUPPLY) ERR-ALREADY-MINTED)
        (var-set last-token-id new-id)
        (map-set token-owner   { token-id: new-id } { owner: recipient })
        (map-set shimmer-dna   { token-id: new-id }
            {
                element:          element,
                rarity:           rarity,
                agility:          agility,
                luminance:        luminance,
                resilience:       resilience,
                crystalline-core: u0
            }
        )
        (map-set is-genesis     { token-id: new-id } { genesis: genesis })
        (map-set shimmer-planet { token-id: new-id } { planet-id: (mod new-id u5) })
        (ok new-id)
    )
)

;; Owner-only: mint genesis Shimmers (limited batch)
(define-public (mint-genesis
    (recipient  principal)
    (element    uint)
    (rarity     uint)
    (agility    uint)
    (luminance  uint)
    (resilience uint)
)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
        (mint-shimmer recipient element rarity agility luminance resilience true)
    )
)

;; Public mint (costs GLOW tokens)
(define-public (mint-shimmer-public
    (element    uint)
    (rarity     uint)
    (agility    uint)
    (luminance  uint)
    (resilience uint)
)
    (begin
        (try! (glow-transfer BREED-COST tx-sender CONTRACT-OWNER))
        (mint-shimmer tx-sender element rarity agility luminance resilience false)
    )
)

;; =============================
;; BREEDING
;; =============================

;; Breed two Shimmers owned by caller to produce an offspring.
;; Offspring traits are blended averages of both parents.
(define-public (breed (parent-a-id uint) (parent-b-id uint))
    (let (
        (owner-a (unwrap! (map-get? token-owner { token-id: parent-a-id }) ERR-TOKEN-NOT-FOUND))
        (owner-b (unwrap! (map-get? token-owner { token-id: parent-b-id }) ERR-TOKEN-NOT-FOUND))
        (dna-a   (unwrap! (map-get? shimmer-dna  { token-id: parent-a-id }) ERR-TOKEN-NOT-FOUND))
        (dna-b   (unwrap! (map-get? shimmer-dna  { token-id: parent-b-id }) ERR-TOKEN-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get owner owner-a)) ERR-NOT-TOKEN-OWNER)
        (asserts! (is-eq tx-sender (get owner owner-b)) ERR-NOT-TOKEN-OWNER)
        (asserts! (not (is-eq parent-a-id parent-b-id)) ERR-SAME-PARENT)

        (try! (glow-transfer BREED-COST tx-sender CONTRACT-OWNER))

        (let (
            (child-element    (get element dna-a))
            (child-rarity     (/ (+ (get rarity     dna-a) (get rarity     dna-b)) u2))
            (child-agility    (/ (+ (get agility    dna-a) (get agility    dna-b)) u2))
            (child-luminance  (/ (+ (get luminance  dna-a) (get luminance  dna-b)) u2))
            (child-resilience (/ (+ (get resilience dna-a) (get resilience dna-b)) u2))
            (new-id           (unwrap!
                                (mint-shimmer
                                    tx-sender
                                    child-element
                                    child-rarity
                                    child-agility
                                    child-luminance
                                    child-resilience
                                    false
                                ) ERR-INVALID-BREED))
        )
            (map-set shimmer-parents { token-id: new-id }
                { parent-a: parent-a-id, parent-b: parent-b-id }
            )
            (ok new-id)
        )
    )
)

;; =============================
;; EVOLUTION CHAMBER
;; =============================

;; Lock a Shimmer into the evolution chamber to begin evolution
(define-public (enter-evolution-chamber (token-id uint))
    (let ((owner-entry (unwrap! (map-get? token-owner { token-id: token-id }) ERR-TOKEN-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get owner owner-entry)) ERR-NOT-TOKEN-OWNER)
        (asserts! (is-none (map-get? evolution-chamber { token-id: token-id })) ERR-EVOLUTION-LOCKED)
        (map-set evolution-chamber { token-id: token-id }
            {
                unlocks-at: (+ block-height EVOLUTION-CHAMBER-BLOCKS),
                stage: u1
            }
        )
        (ok true)
    )
)

;; Complete evolution once the time-lock has passed.
;; glow-mint is called as a plain statement (no try!) because it cannot fail.
(define-public (complete-evolution (token-id uint))
    (let (
        (owner-entry (unwrap! (map-get? token-owner       { token-id: token-id }) ERR-TOKEN-NOT-FOUND))
        (chamber     (unwrap! (map-get? evolution-chamber { token-id: token-id }) ERR-EVOLUTION-LOCKED))
        (dna         (unwrap! (map-get? shimmer-dna       { token-id: token-id }) ERR-TOKEN-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get owner owner-entry)) ERR-NOT-TOKEN-OWNER)
        (asserts! (>= block-height (get unlocks-at chamber)) ERR-EVOLUTION-LOCKED)

        (map-set shimmer-dna { token-id: token-id }
            (merge dna {
                crystalline-core: (+ (get crystalline-core dna) u10),
                rarity:           (min (+ (get rarity dna) u5) u255)
            })
        )
        (map-delete evolution-chamber { token-id: token-id })

        ;; Reward GLOW - direct call, no try! (glow-mint cannot fail)
        (glow-mint tx-sender u500000)
        (ok true)
    )
)

;; =============================
;; GLOW TOKEN PUBLIC INTERFACE
;; =============================

(define-read-only (glow-get-balance (owner principal))
    (ok (default-to u0 (get amount (map-get? glow-balance { owner: owner }))))
)

(define-read-only (glow-get-total-supply)
    (ok (var-get glow-total-supply))
)

(define-read-only (glow-get-name)      (ok GLOW-NAME))
(define-read-only (glow-get-symbol)    (ok GLOW-SYMBOL))
(define-read-only (glow-get-decimals)  (ok GLOW-DECIMALS))
(define-read-only (glow-get-token-uri) (ok GLOW-URI))

;; Owner-only external mint (e.g. for rewards, conservation partners)
(define-public (glow-mint-admin (recipient principal) (amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
        (glow-mint recipient amount)
        (ok true)
    )
)

;; Public GLOW transfer
(define-public (glow-transfer-public (amount uint) (recipient principal))
    (glow-transfer amount tx-sender recipient)
)

;; =============================
;; CLUSTER FORMATION
;; =============================

;; Group three owned Shimmers into a cluster to earn GLOW rewards.
;; glow-mint called as a plain statement (no try!) because it cannot fail.
(define-public (form-cluster (token-a uint) (token-b uint) (token-c uint))
    (let (
        (owner-a (unwrap! (map-get? token-owner { token-id: token-a }) ERR-TOKEN-NOT-FOUND))
        (owner-b (unwrap! (map-get? token-owner { token-id: token-b }) ERR-TOKEN-NOT-FOUND))
        (owner-c (unwrap! (map-get? token-owner { token-id: token-c }) ERR-TOKEN-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get owner owner-a)) ERR-NOT-TOKEN-OWNER)
        (asserts! (is-eq tx-sender (get owner owner-b)) ERR-NOT-TOKEN-OWNER)
        (asserts! (is-eq tx-sender (get owner owner-c)) ERR-NOT-TOKEN-OWNER)

        (glow-mint tx-sender u250000)
        (ok true)
    )
)

;; =============================
;; GOVERNANCE: COUNCIL OF KEEPERS
;; =============================

;; Submit a proposal (requires MIN-GOVERNANCE-TOKENS GLOW)
(define-public (submit-proposal
    (title                (string-ascii 64))
    (description          (string-ascii 256))
    (voting-period-blocks uint)
)
    (let (
        (bal     (default-to u0 (get amount (map-get? glow-balance { owner: tx-sender }))))
        (new-pid (+ (var-get last-proposal-id) u1))
    )
        (asserts! (>= bal MIN-GOVERNANCE-TOKENS) ERR-NOT-ENOUGH-TOKENS)
        (var-set last-proposal-id new-pid)
        (map-set proposals { proposal-id: new-pid }
            {
                proposer:      tx-sender,
                title:         title,
                description:   description,
                votes-for:     u0,
                votes-against: u0,
                ends-at-block: (+ block-height voting-period-blocks),
                executed:      false
            }
        )
        (ok new-pid)
    )
)

;; Vote on a proposal; voting weight equals caller's GLOW balance
(define-public (vote (proposal-id uint) (support bool))
    (let (
        (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
        (bal      (default-to u0 (get amount (map-get? glow-balance { owner: tx-sender }))))
    )
        (asserts! (< block-height (get ends-at-block proposal)) ERR-PROPOSAL-CLOSED)
        (asserts! (is-none (map-get? vote-record { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
        (asserts! (> bal u0) ERR-NOT-ENOUGH-TOKENS)

        (map-set vote-record { proposal-id: proposal-id, voter: tx-sender } { voted: true })

        (if support
            (map-set proposals { proposal-id: proposal-id }
                (merge proposal { votes-for: (+ (get votes-for proposal) bal) })
            )
            (map-set proposals { proposal-id: proposal-id }
                (merge proposal { votes-against: (+ (get votes-against proposal) bal) })
            )
        )
        (ok true)
    )
)

;; =============================
;; READ-ONLY HELPERS
;; =============================

(define-read-only (get-proposal (proposal-id uint))
    (ok (map-get? proposals { proposal-id: proposal-id }))
)

(define-read-only (get-shimmer-dna (token-id uint))
    (ok (map-get? shimmer-dna { token-id: token-id }))
)

(define-read-only (get-shimmer-parents (token-id uint))
    (ok (map-get? shimmer-parents { token-id: token-id }))
)

(define-read-only (get-shimmer-planet (token-id uint))
    (ok (map-get? shimmer-planet { token-id: token-id }))
)

(define-read-only (get-evolution-status (token-id uint))
    (ok (map-get? evolution-chamber { token-id: token-id }))
)

(define-read-only (is-genesis-shimmer (token-id uint))
    (ok (map-get? is-genesis { token-id: token-id }))
)
