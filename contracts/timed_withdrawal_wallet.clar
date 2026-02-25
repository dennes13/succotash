;; Timed Withdrawal Wallet Contract
;; A wallet that restricts withdrawals until after a specified time

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-WITHDRAWAL-TIME-NOT-SET (err u101))
(define-constant ERR-WITHDRAWAL-TIME-NOT-REACHED (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))
(define-constant ERR-INVALID-AMOUNT (err u104))
(define-constant ERR-INVALID-TIME (err u105))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Data variables
(define-data-var withdrawal-time (optional uint) none)
(define-data-var wallet-balance uint u0)

;; Read-only functions

;; Get the current withdrawal time
(define-read-only (get-withdrawal-time)
  (var-get withdrawal-time)
)

;; Get the current wallet balance
(define-read-only (get-wallet-balance)
  (var-get wallet-balance)
)

;; Check if withdrawal time has been reached
(define-read-only (can-withdraw-now)
  (match (var-get withdrawal-time)
    time-set (>= block-height time-set)
    false
  )
)

;; Get current block height (for reference)
(define-read-only (get-current-time)
  block-height
)

;; Public functions

;; Set the withdrawal time (only contract owner can call this)
(define-public (set-withdrawal-time (timestamp uint))
  (begin
    ;; Check if caller is authorized
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    ;; Validate timestamp is in the future
    (asserts! (> timestamp block-height) ERR-INVALID-TIME)
    ;; Set the withdrawal time
    (var-set withdrawal-time (some timestamp))
    (ok true)
  )
)

;; Deposit STX into the wallet
(define-public (deposit (amount uint))
  (begin
    ;; Validate amount
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    ;; Transfer STX from sender to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    ;; Update wallet balance
    (var-set wallet-balance (+ (var-get wallet-balance) amount))
    (ok amount)
  )
)

;; Withdraw STX from the wallet (only after withdrawal time)
(define-public (withdraw (amount uint))
  (let (
    (current-balance (var-get wallet-balance))
    (withdrawal-timestamp (var-get withdrawal-time))
  )
    (begin
      ;; Check if caller is authorized
      (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
      ;; Validate amount
      (asserts! (> amount u0) ERR-INVALID-AMOUNT)
      ;; Check if withdrawal time is set
      (asserts! (is-some withdrawal-timestamp) ERR-WITHDRAWAL-TIME-NOT-SET)
      ;; Check if withdrawal time has been reached
      (asserts! (>= block-height (unwrap-panic withdrawal-timestamp)) ERR-WITHDRAWAL-TIME-NOT-REACHED)
      ;; Check if sufficient balance
      (asserts! (>= current-balance amount) ERR-INSUFFICIENT-BALANCE)
      ;; Update wallet balance
      (var-set wallet-balance (- current-balance amount))
      ;; Transfer STX from contract to sender
      (try! (as-contract (stx-transfer? amount tx-sender CONTRACT-OWNER)))
      (ok amount)
    )
  )
)

;; Withdraw all available funds
(define-public (withdraw-all)
  (let (
    (current-balance (var-get wallet-balance))
  )
    (withdraw current-balance)
  )
)

;; Emergency function to reset withdrawal time (only owner)
(define-public (reset-withdrawal-time)
  (begin
    ;; Check if caller is authorized
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    ;; Reset withdrawal time
    (var-set withdrawal-time none)
    (ok true)
  )
)