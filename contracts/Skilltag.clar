
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
(define-constant err-assessment-not-found (err u115))
(define-constant err-assessment-expired (err u116))
(define-constant err-invalid-assessment-type (err u117))
(define-constant err-assessment-already-exists (err u118))
(define-constant err-unauthorized-assessor (err u119))
(define-constant err-assessment-not-passed (err u120))
(define-constant err-invalid-difficulty (err u121))
(define-constant err-challenge-not-found (err u122))
(define-constant err-challenge-expired (err u123))
(define-constant err-invalid-response (err u124))

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

(define-map skill-assessments (tuple (skill-name (string-ascii 50)) (assessor principal))
  (tuple 
    (assessment-type (string-ascii 20))
    (difficulty-level uint)
    (max-score uint)
    (passing-score uint)
    (time-limit uint)
    (created-at uint)
    (expiry-block uint)
    (is-active bool)))

(define-map assessment-attempts (tuple (skill-name (string-ascii 50)) (participant principal) (assessor principal))
  (tuple 
    (score uint)
    (max-possible-score uint)
    (completion-time uint)
    (attempted-at uint)
    (is-passed bool)
    (verification-hash (string-ascii 64))))

(define-map skill-challenges (tuple (challenge-id uint) (skill-name (string-ascii 50)))
  (tuple 
    (challenger principal)
    (challenged-user principal)
    (challenge-type (string-ascii 20))
    (challenge-data (string-ascii 200))
    (expected-response-hash (string-ascii 64))
    (created-at uint)
    (expiry-block uint)
    (is-completed bool)
    (is-passed bool)))

(define-map authorized-assessors (tuple (assessor principal) (skill-name (string-ascii 50))) bool)

(define-map assessment-scores principal (list 20 (tuple (skill-name (string-ascii 50)) (score uint) (max-score uint))))

(define-data-var last-challenge-id uint u0)

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

(define-read-only (get-skill-assessment (skill-name (string-ascii 50)) (assessor principal))
  (map-get? skill-assessments (tuple (skill-name skill-name) (assessor assessor))))

(define-read-only (get-assessment-attempt (skill-name (string-ascii 50)) (participant principal) (assessor principal))
  (map-get? assessment-attempts (tuple (skill-name skill-name) (participant participant) (assessor assessor))))

(define-read-only (get-skill-challenge (challenge-id uint) (skill-name (string-ascii 50)))
  (map-get? skill-challenges (tuple (challenge-id challenge-id) (skill-name skill-name))))

(define-read-only (is-authorized-assessor (assessor principal) (skill-name (string-ascii 50)))
  (default-to false (map-get? authorized-assessors (tuple (assessor assessor) (skill-name skill-name)))))

(define-read-only (get-user-assessment-scores (user principal))
  (default-to (list) (map-get? assessment-scores user)))

(define-read-only (calculate-skill-proficiency (skill-name (string-ascii 50)) (user principal))
  (let ((user-scores (get-user-assessment-scores user))
        (skill-score-data (filter is-skill-match user-scores)))
    (if (> (len skill-score-data) u0)
      (let ((skill-data (unwrap-panic (element-at skill-score-data u0))))
        (/ (* (get score skill-data) u100) (get max-score skill-data)))
      u0)))

(define-private (is-skill-match (score-data (tuple (skill-name (string-ascii 50)) (score uint) (max-score uint))))
  (is-eq (get skill-name score-data) "temp"))

(define-read-only (get-challenge-verification-data (challenge-id uint) (skill-name (string-ascii 50)))
  (let ((challenge-data (get-skill-challenge challenge-id skill-name)))
    (match challenge-data
      challenge (tuple 
        (is-active (< stacks-block-height (get expiry-block challenge)))
        (is-completed (get is-completed challenge))
        (is-passed (get is-passed challenge)))
      (tuple (is-active false) (is-completed false) (is-passed false)))))

(define-public (authorize-skill-assessor (assessor principal) (skill-name (string-ascii 50)))
  (begin
    (asserts! (or (is-eq tx-sender contract-owner) (is-authorized-issuer tx-sender)) err-unauthorized-assessor)
    (ok (map-set authorized-assessors (tuple (assessor assessor) (skill-name skill-name)) true))))

(define-public (revoke-skill-assessor (assessor principal) (skill-name (string-ascii 50)))
  (begin
    (asserts! (or (is-eq tx-sender contract-owner) (is-authorized-issuer tx-sender)) err-unauthorized-assessor)
    (ok (map-delete authorized-assessors (tuple (assessor assessor) (skill-name skill-name))))))

(define-public (create-skill-assessment 
  (skill-name (string-ascii 50))
  (assessment-type (string-ascii 20))
  (difficulty-level uint)
  (max-score uint)
  (passing-score uint)
  (time-limit uint)
  (validity-blocks uint))
  (let ((assessor tx-sender)
        (assessment-key (tuple (skill-name skill-name) (assessor assessor)))
        (expiry-block (+ stacks-block-height validity-blocks)))
    (asserts! (is-authorized-assessor assessor skill-name) err-unauthorized-assessor)
    (asserts! (or 
      (is-eq assessment-type "Multiple-Choice")
      (is-eq assessment-type "Practical")
      (is-eq assessment-type "Code-Review")
      (is-eq assessment-type "Portfolio")) err-invalid-assessment-type)
    (asserts! (and (>= difficulty-level u1) (<= difficulty-level u5)) err-invalid-difficulty)
    (asserts! (> passing-score u0) err-invalid-assessment-type)
    (asserts! (<= passing-score max-score) err-invalid-assessment-type)
    (asserts! (is-none (get-skill-assessment skill-name assessor)) err-assessment-already-exists)
    (map-set skill-assessments assessment-key
      (tuple 
        (assessment-type assessment-type)
        (difficulty-level difficulty-level)
        (max-score max-score)
        (passing-score passing-score)
        (time-limit time-limit)
        (created-at stacks-block-height)
        (expiry-block expiry-block)
        (is-active true)))
    (ok assessment-key)))

(define-public (take-skill-assessment 
  (skill-name (string-ascii 50))
  (assessor principal)
  (score uint)
  (completion-time uint)
  (verification-hash (string-ascii 64)))
  (let ((participant tx-sender)
        (assessment-key (tuple (skill-name skill-name) (assessor assessor)))
        (attempt-key (tuple (skill-name skill-name) (participant participant) (assessor assessor)))
        (assessment-data (unwrap! (get-skill-assessment skill-name assessor) err-assessment-not-found)))
    (asserts! (get is-active assessment-data) err-assessment-expired)
    (asserts! (< stacks-block-height (get expiry-block assessment-data)) err-assessment-expired)
    (asserts! (<= score (get max-score assessment-data)) err-invalid-response)
    (asserts! (<= completion-time (get time-limit assessment-data)) err-invalid-response)
    (let ((is-passed (>= score (get passing-score assessment-data)))
          (user-scores (get-user-assessment-scores participant))
          (new-score-entry (tuple (skill-name skill-name) (score score) (max-score (get max-score assessment-data))))
          (updated-scores (unwrap! (as-max-len? (append user-scores new-score-entry) u20) err-assessment-already-exists)))
      (map-set assessment-attempts attempt-key
        (tuple 
          (score score)
          (max-possible-score (get max-score assessment-data))
          (completion-time completion-time)
          (attempted-at stacks-block-height)
          (is-passed is-passed)
          (verification-hash verification-hash)))
      (if is-passed
        (map-set assessment-scores participant updated-scores)
        true)
      (ok is-passed))))

(define-public (create-skill-challenge 
  (challenged-user principal)
  (skill-name (string-ascii 50))
  (challenge-type (string-ascii 20))
  (challenge-data (string-ascii 200))
  (expected-response-hash (string-ascii 64))
  (validity-blocks uint))
  (let ((challenger tx-sender)
        (challenge-id (+ (var-get last-challenge-id) u1))
        (challenge-key (tuple (challenge-id challenge-id) (skill-name skill-name)))
        (expiry-block (+ stacks-block-height validity-blocks)))
    (asserts! (not (is-eq challenger challenged-user)) err-cannot-endorse-self)
    (asserts! (is-some (index-of (get-user-skills challenged-user) skill-name)) err-nft-not-found)
    (asserts! (or 
      (is-eq challenge-type "Knowledge-Test")
      (is-eq challenge-type "Problem-Solving")
      (is-eq challenge-type "Code-Debug")
      (is-eq challenge-type "Design-Review")) err-invalid-assessment-type)
    (map-set skill-challenges challenge-key
      (tuple 
        (challenger challenger)
        (challenged-user challenged-user)
        (challenge-type challenge-type)
        (challenge-data challenge-data)
        (expected-response-hash expected-response-hash)
        (created-at stacks-block-height)
        (expiry-block expiry-block)
        (is-completed false)
        (is-passed false)))
    (var-set last-challenge-id challenge-id)
    (ok challenge-id)))

(define-public (respond-to-skill-challenge 
  (challenge-id uint)
  (skill-name (string-ascii 50))
  (response-hash (string-ascii 64)))
  (let ((challenge-key (tuple (challenge-id challenge-id) (skill-name skill-name)))
        (challenge-data (unwrap! (get-skill-challenge challenge-id skill-name) err-challenge-not-found)))
    (asserts! (is-eq tx-sender (get challenged-user challenge-data)) err-unauthorized-assessor)
    (asserts! (< stacks-block-height (get expiry-block challenge-data)) err-challenge-expired)
    (asserts! (not (get is-completed challenge-data)) err-challenge-expired)
    (let ((is-correct (is-eq response-hash (get expected-response-hash challenge-data))))
      (map-set skill-challenges challenge-key
        (merge challenge-data (tuple (is-completed true) (is-passed is-correct))))
      (if is-correct
        (map-set user-reputation (get challenged-user challenge-data) 
          (+ (get-user-reputation (get challenged-user challenge-data)) u5))
        true)
      (ok is-correct))))

(define-public (verify-skill-challenge-completion (challenge-id uint) (skill-name (string-ascii 50)))
  (let ((challenge-key (tuple (challenge-id challenge-id) (skill-name skill-name)))
        (challenge-data (unwrap! (get-skill-challenge challenge-id skill-name) err-challenge-not-found)))
    (asserts! (get is-completed challenge-data) err-challenge-not-found)
    (ok (get is-passed challenge-data))))



    