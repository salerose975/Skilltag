
(define-non-fungible-token skilltag uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-listing-not-found (err u102))
(define-constant err-wrong-commission (err u103))
(define-constant err-listing-expired (err u104))
(define-constant err-nft-not-found (err u105))
(define-constant err-sender-equals-recipient (err u106))
(define-constant err-invalid-skill-level (err u107))
(define-constant err-skill-already-exists (err u108))
(define-constant err-unauthorized-issuer (err u109))
(define-constant err-cannot-endorse-self (err u110))
(define-constant err-already-endorsed (err u111))
(define-constant err-endorsement-not-found (err u112))
(define-constant err-endorsement-expired (err u113))
(define-constant err-insufficient-reputation (err u114))

(define-data-var last-token-id uint u0)
(define-data-var commission uint u250)

(define-map token-count principal uint)
(define-map market (tuple (token-id uint) (owner principal)) 
  (tuple (price uint) (commission uint) (expiry uint)))

(define-map skilltag-data uint 
  (tuple 
    (skill-name (string-ascii 50))
    (skill-level (string-ascii 20))
    (issuer principal)
    (issued-at uint)
    (course-provider (string-ascii 100))
    (verification-hash (string-ascii 64))
    (expiry-block uint)
  ))

(define-map authorized-issuers principal bool)

(define-map user-skills principal (list 50 (string-ascii 50)))

(define-map skill-endorsements (tuple (skill-name (string-ascii 50)) (skill-holder principal)) 
  (tuple (endorsement-count uint) (total-reputation uint)))

(define-map user-endorsements (tuple (endorser principal) (skill-name (string-ascii 50)) (skill-holder principal))
  (tuple (reputation-weight uint) (endorsed-at uint) (expiry-block uint)))

(define-map user-reputation principal uint)

(define-read-only (get-last-token-id)
  (ok (var-get last-token-id)))

(define-read-only (get-token-uri (token-id uint))
  (ok none))

(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? skilltag token-id)))

(define-read-only (get-skilltag-data (token-id uint))
  (map-get? skilltag-data token-id))

(define-read-only (get-user-skills (user principal))
  (default-to (list) (map-get? user-skills user)))

(define-read-only (get-commission)
  (ok (var-get commission)))

(define-read-only (get-listing-in-ustx (token-id uint))
  (match (map-get? market (tuple (token-id token-id) (owner (unwrap! (nft-get-owner? skilltag token-id) err-nft-not-found))))
    listing (ok listing)
    err-listing-not-found))

(define-read-only (is-authorized-issuer (issuer principal))
  (default-to false (map-get? authorized-issuers issuer)))

(define-read-only (get-skill-endorsements (skill-name (string-ascii 50)) (skill-holder principal))
  (default-to (tuple (endorsement-count u0) (total-reputation u0)) 
    (map-get? skill-endorsements (tuple (skill-name skill-name) (skill-holder skill-holder)))))

(define-read-only (get-user-endorsement (endorser principal) (skill-name (string-ascii 50)) (skill-holder principal))
  (map-get? user-endorsements (tuple (endorser endorser) (skill-name skill-name) (skill-holder skill-holder))))

(define-read-only (get-user-reputation (user principal))
  (default-to u0 (map-get? user-reputation user)))

(define-read-only (calculate-reputation-weight (endorser principal))
  (let ((base-reputation (get-user-reputation endorser))
        (skill-count (len (get-user-skills endorser))))
    (if (> skill-count u0)
      (+ u1 (/ base-reputation u10) (/ skill-count u5))
      u1)))

(define-read-only (has-valid-endorsement (endorser principal) (skill-name (string-ascii 50)) (skill-holder principal))
  (match (get-user-endorsement endorser skill-name skill-holder)
    endorsement (< stacks-block-height (get expiry-block endorsement))
    false))

(define-public (authorize-issuer (issuer principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-set authorized-issuers issuer true))))

(define-public (revoke-issuer (issuer principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-delete authorized-issuers issuer))))

(define-public (mint-skilltag 
  (recipient principal)
  (skill-name (string-ascii 50))
  (skill-level (string-ascii 20))
  (course-provider (string-ascii 100))
  (verification-hash (string-ascii 64))
  (validity-blocks uint))
  (let 
    (
      (token-id (+ (var-get last-token-id) u1))
      (current-block stacks-block-height)
      (expiry-block (+ current-block validity-blocks))
      (current-skills (get-user-skills recipient))
    )
    (asserts! (or (is-eq tx-sender contract-owner) (is-authorized-issuer tx-sender)) err-unauthorized-issuer)
    (asserts! (or 
      (is-eq skill-level "Beginner")
      (is-eq skill-level "Intermediate") 
      (is-eq skill-level "Advanced")
      (is-eq skill-level "Expert")) err-invalid-skill-level)
    (asserts! (is-none (index-of current-skills skill-name)) err-skill-already-exists)
    (try! (nft-mint? skilltag token-id recipient))
    (map-set skilltag-data token-id 
      (tuple 
        (skill-name skill-name)
        (skill-level skill-level)
        (issuer tx-sender)
        (issued-at current-block)
        (course-provider course-provider)
        (verification-hash verification-hash)
        (expiry-block expiry-block)
      ))
    (map-set user-skills recipient (unwrap! (as-max-len? (append current-skills skill-name) u50) err-skill-already-exists))
    (map-set token-count recipient (+ (get-balance recipient) u1))
    (var-set last-token-id token-id)
    (ok token-id)))

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) err-not-token-owner)
    (asserts! (not (is-eq sender recipient)) err-sender-equals-recipient)
    (let ((current-balance-sender (get-balance sender))
          (current-balance-recipient (get-balance recipient)))
      (map-set token-count sender (- current-balance-sender u1))
      (map-set token-count recipient (+ current-balance-recipient u1))
      (nft-transfer? skilltag token-id sender recipient))))

(define-public (list-in-ustx (token-id uint) (price uint) (expiry uint))
  (let ((listing (tuple (token-id token-id) (owner tx-sender))))
    (asserts! (is-owner token-id tx-sender) err-not-token-owner)
    (map-set market listing (tuple (price price) (commission (var-get commission)) (expiry expiry)))
    (ok (map-get? market listing))))

(define-public (unlist-in-ustx (token-id uint))
  (begin
    (asserts! (is-owner token-id tx-sender) err-not-token-owner)
    (map-delete market (tuple (token-id token-id) (owner tx-sender)))
    (ok true)))

(define-public (buy-in-ustx (token-id uint) (owner principal))
  (let ((listing (unwrap! (map-get? market (tuple (token-id token-id) (owner owner))) err-listing-not-found))
        (price (get price listing))
        (commission-amount (/ (* price (get commission listing)) u10000)))
    (asserts! (< stacks-block-height (get expiry listing)) err-listing-expired)
    (try! (stx-transfer? (- price commission-amount) tx-sender owner))
    (try! (stx-transfer? commission-amount tx-sender contract-owner))
    (try! (transfer token-id owner tx-sender))
    (map-delete market (tuple (token-id token-id) (owner owner)))
    (ok token-id)))

(define-public (set-commission (commission-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (< commission-rate u1000) err-wrong-commission)
    (ok (var-set commission commission-rate))))

(define-public (burn-expired-skilltag (token-id uint))
  (let ((skilltag-info (unwrap! (get-skilltag-data token-id) err-nft-not-found))
        (owner (unwrap! (nft-get-owner? skilltag token-id) err-nft-not-found)))
    (asserts! (>= stacks-block-height (get expiry-block skilltag-info)) err-listing-expired)
    (let ((skill-name (get skill-name skilltag-info))
          (current-skills (get-user-skills owner))
          (updated-skills (filter remove-skill current-skills)))
      (map-set user-skills owner updated-skills)
      (map-set token-count owner (- (get-balance owner) u1))
      (map-delete skilltag-data token-id)
      (nft-burn? skilltag token-id owner))))

(define-private (remove-skill (skill (string-ascii 50)))
  (let ((token-data (unwrap! (get-skilltag-data (var-get last-token-id)) false)))
    (not (is-eq skill (get skill-name token-data)))))

(define-private (is-owner (token-id uint) (user principal))
  (is-eq (some user) (nft-get-owner? skilltag token-id)))

(define-read-only (get-balance (account principal))
  (default-to u0 (map-get? token-count account)))

(define-public (validate-skilltag (token-id uint))
  (let ((skilltag-info (unwrap! (get-skilltag-data token-id) err-nft-not-found)))
    (ok (< stacks-block-height (get expiry-block skilltag-info)))))

(define-public (endorse-skill (skill-name (string-ascii 50)) (skill-holder principal) (validity-blocks uint))
  (let ((endorser tx-sender)
        (reputation-weight (calculate-reputation-weight tx-sender))
        (current-endorsement (get-skill-endorsements skill-name skill-holder))
        (endorsement-key (tuple (endorser endorser) (skill-name skill-name) (skill-holder skill-holder)))
        (skill-key (tuple (skill-name skill-name) (skill-holder skill-holder)))
        (expiry-block (+ stacks-block-height validity-blocks)))
    (asserts! (not (is-eq endorser skill-holder)) err-cannot-endorse-self)
    (asserts! (is-some (index-of (get-user-skills skill-holder) skill-name)) (err u0))
    (asserts! (not (has-valid-endorsement endorser skill-name skill-holder)) err-already-endorsed)
    (asserts! (>= reputation-weight u1) err-insufficient-reputation)
    (map-set user-endorsements endorsement-key 
      (tuple (reputation-weight reputation-weight) (endorsed-at stacks-block-height) (expiry-block expiry-block)))
    (map-set skill-endorsements skill-key
      (tuple 
        (endorsement-count (+ (get endorsement-count current-endorsement) u1))
        (total-reputation (+ (get total-reputation current-endorsement) reputation-weight))))
    (map-set user-reputation skill-holder (+ (get-user-reputation skill-holder) reputation-weight))
    (ok true)))

(define-public (revoke-endorsement (skill-name (string-ascii 50)) (skill-holder principal))
  (let ((endorser tx-sender)
        (endorsement-key (tuple (endorser endorser) (skill-name skill-name) (skill-holder skill-holder)))
        (skill-key (tuple (skill-name skill-name) (skill-holder skill-holder)))
        (existing-endorsement (unwrap! (get-user-endorsement endorser skill-name skill-holder) err-endorsement-not-found))
        (current-skill-endorsement (get-skill-endorsements skill-name skill-holder))
        (reputation-weight (get reputation-weight existing-endorsement)))
    (asserts! (has-valid-endorsement endorser skill-name skill-holder) err-endorsement-expired)
    (map-delete user-endorsements endorsement-key)
    (map-set skill-endorsements skill-key
      (tuple 
        (endorsement-count (- (get endorsement-count current-skill-endorsement) u1))
        (total-reputation (- (get total-reputation current-skill-endorsement) reputation-weight))))
    (map-set user-reputation skill-holder (- (get-user-reputation skill-holder) reputation-weight))
    (ok true)))

(define-public (refresh-endorsement (skill-name (string-ascii 50)) (skill-holder principal) (new-validity-blocks uint))
  (let ((endorser tx-sender)
        (endorsement-key (tuple (endorser endorser) (skill-name skill-name) (skill-holder skill-holder)))
        (existing-endorsement (unwrap! (get-user-endorsement endorser skill-name skill-holder) err-endorsement-not-found))
        (new-expiry-block (+ stacks-block-height new-validity-blocks)))
    (asserts! (has-valid-endorsement endorser skill-name skill-holder) err-endorsement-expired)
    (map-set user-endorsements endorsement-key
      (tuple 
        (reputation-weight (get reputation-weight existing-endorsement))
        (endorsed-at (get endorsed-at existing-endorsement))
        (expiry-block new-expiry-block)))
    (ok true)))

(define-public (cleanup-expired-endorsement (endorser principal) (skill-name (string-ascii 50)) (skill-holder principal))
  (let ((endorsement-key (tuple (endorser endorser) (skill-name skill-name) (skill-holder skill-holder)))
        (skill-key (tuple (skill-name skill-name) (skill-holder skill-holder)))
        (existing-endorsement (unwrap! (get-user-endorsement endorser skill-name skill-holder) err-endorsement-not-found))
        (current-skill-endorsement (get-skill-endorsements skill-name skill-holder))
        (reputation-weight (get reputation-weight existing-endorsement)))
    (asserts! (>= stacks-block-height (get expiry-block existing-endorsement)) err-endorsement-not-found)
    (map-delete user-endorsements endorsement-key)
    (map-set skill-endorsements skill-key
      (tuple 
        (endorsement-count (- (get endorsement-count current-skill-endorsement) u1))
        (total-reputation (- (get total-reputation current-skill-endorsement) reputation-weight))))
    (map-set user-reputation skill-holder (- (get-user-reputation skill-holder) reputation-weight))
    (ok true)))

(define-read-only (get-skill-reputation-score (skill-name (string-ascii 50)) (skill-holder principal))
  (let ((endorsements (get-skill-endorsements skill-name skill-holder)))
    (if (> (get endorsement-count endorsements) u0)
      (/ (get total-reputation endorsements) (get endorsement-count endorsements))
      u0)))

(define-read-only (get-top-endorsed-skills (skill-holder principal))
  (let ((user-skill-list (get-user-skills skill-holder)))
    (map get-skill-score user-skill-list)))

(define-private (get-skill-score (skill-name (string-ascii 50)))
  (tuple (skill-name skill-name) (score (get-skill-reputation-score skill-name tx-sender))))