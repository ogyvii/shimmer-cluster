;; SIP-010 Fungible Token Standard Trait
;; Clarity Version 2 / Epoch 2.1

(define-trait sip-010-trait
    (
        ;; Transfer tokens from caller to recipient
        (transfer (uint principal principal (optional (buff 34))) (response bool uint))

        ;; Human-readable token name
        (get-name () (response (string-ascii 32) uint))

        ;; Human-readable token symbol
        (get-symbol () (response (string-ascii 32) uint))

        ;; Number of decimal places for display
        (get-decimals () (response uint uint))

        ;; Balance of a given principal
        (get-balance (principal) (response uint uint))

        ;; Total circulating supply
        (get-total-supply () (response uint uint))

        ;; Optional URI pointing to token metadata
        (get-token-uri () (response (optional (string-utf8 256)) uint))
    )
)
