;; Skills Learning Path & Progress Tracker Contract
;; Provides structured learning journeys with tracked progress and milestone rewards

;; Error constants
(define-constant err-not-authorized (err u200))
(define-constant err-path-not-found (err u201))
(define-constant err-already-enrolled (err u202))
(define-constant err-not-enrolled (err u203))
(define-constant err-prerequisites-not-met (err u204))
(define-constant err-path-already-exists (err u205))
(define-constant err-invalid-progress (err u206))
(define-constant err-milestone-not-reached (err u207))
(define-constant err-path-completed (err u208))
(define-constant err-invalid-path-data (err u209))

;; Data variables
(define-data-var path-id-nonce uint u0)
(define-data-var completion-reward-rate uint u10) ;; Base reputation reward for path completion

;; Path difficulty levels
(define-constant difficulty-beginner u1)
(define-constant difficulty-intermediate u2)
(define-constant difficulty-advanced u3)
(define-constant difficulty-expert u4)

;; Learning paths structure
(define-map learning-paths uint
  {
    creator: principal,
    path-name: (string-ascii 64),
    description: (string-ascii 200),
    target-skill: (string-ascii 50),
    difficulty-level: uint,
    estimated-duration: uint, ;; in blocks
    prerequisites: (list 5 (string-ascii 50)),
    milestones: (list 10 (string-ascii 100)),
    completion-reward: uint,
    created-at: uint,
    is-active: bool
  })

;; User progress tracking
(define-map user-path-progress (tuple (user principal) (path-id uint))
  {
    enrolled-at: uint,
    current-milestone: uint,
    milestones-completed: (list 10 bool),
    progress-percentage: uint,
    last-updated: uint,
    is-completed: bool,
    completion-time: (optional uint)
  })

;; Path enrollment tracking
(define-map path-enrollments uint
  {
    total-enrolled: uint,
    total-completed: uint,
    average-completion-time: uint
  })

;; User learning statistics
(define-map user-learning-stats principal
  {
    paths-enrolled: uint,
    paths-completed: uint,
    total-milestones: uint,
    learning-reputation: uint
  })

;; Create a new learning path
(define-public (create-learning-path
  (path-name (string-ascii 64))
  (description (string-ascii 200))
  (target-skill (string-ascii 50))
  (difficulty-level uint)
  (estimated-duration uint)
  (prerequisites (list 5 (string-ascii 50)))
  (milestones (list 10 (string-ascii 100)))
  (completion-reward uint))
  (let ((path-id (+ (var-get path-id-nonce) u1)))
    (asserts! (and (>= difficulty-level difficulty-beginner) (<= difficulty-level difficulty-expert)) err-invalid-path-data)
    (asserts! (> estimated-duration u0) err-invalid-path-data)
    (asserts! (> (len milestones) u0) err-invalid-path-data)
    (asserts! (> completion-reward u0) err-invalid-path-data)
    
    (map-set learning-paths path-id
      {
        creator: tx-sender,
        path-name: path-name,
        description: description,
        target-skill: target-skill,
        difficulty-level: difficulty-level,
        estimated-duration: estimated-duration,
        prerequisites: prerequisites,
        milestones: milestones,
        completion-reward: completion-reward,
        created-at: stacks-block-height,
        is-active: true
      })
    
    (map-set path-enrollments path-id
      {
        total-enrolled: u0,
        total-completed: u0,
        average-completion-time: u0
      })
    
    (var-set path-id-nonce path-id)
    (ok path-id)))

;; Enroll in a learning path
(define-public (enroll-in-learning-path (path-id uint))
  (let ((path-data (unwrap! (map-get? learning-paths path-id) err-path-not-found))
        (user tx-sender)
        (enrollment-key (tuple (user user) (path-id path-id)))
        (user-stats (get-user-learning-stats user))
        (path-stats (unwrap-panic (map-get? path-enrollments path-id))))
    (asserts! (get is-active path-data) err-path-not-found)
    (asserts! (is-none (map-get? user-path-progress enrollment-key)) err-already-enrolled)
    (asserts! (check-prerequisites user (get prerequisites path-data)) err-prerequisites-not-met)
    
    ;; Initialize user progress
    (map-set user-path-progress enrollment-key
      {
        enrolled-at: stacks-block-height,
        current-milestone: u0,
        milestones-completed: (list false false false false false false false false false false),
        progress-percentage: u0,
        last-updated: stacks-block-height,
        is-completed: false,
        completion-time: none
      })
    
    ;; Update user stats
    (map-set user-learning-stats user
      (merge user-stats {paths-enrolled: (+ (get paths-enrolled user-stats) u1)}))
    
    ;; Update path enrollment count
    (map-set path-enrollments path-id
      (merge path-stats {total-enrolled: (+ (get total-enrolled path-stats) u1)}))
    
    (ok true)))

;; Update milestone progress
(define-public (update-milestone-progress (path-id uint) (milestone-index uint))
  (let ((user tx-sender)
        (enrollment-key (tuple (user user) (path-id path-id)))
        (path-data (unwrap! (map-get? learning-paths path-id) err-path-not-found))
        (progress-data (unwrap! (map-get? user-path-progress enrollment-key) err-not-enrolled)))
    (asserts! (< milestone-index (len (get milestones path-data))) err-invalid-progress)
    (asserts! (not (get is-completed progress-data)) err-path-completed)
    (asserts! (is-eq milestone-index (get current-milestone progress-data)) err-invalid-progress)
    
    (let ((milestones-list (get milestones-completed progress-data))
          (updated-milestones (update-milestone-list milestones-list milestone-index))
          (new-current-milestone (+ milestone-index u1))
          (total-milestones (len (get milestones path-data)))
          (new-progress (/ (* new-current-milestone u100) total-milestones))
          (user-stats (get-user-learning-stats user)))
      
      (map-set user-path-progress enrollment-key
        (merge progress-data
          {
            current-milestone: new-current-milestone,
            milestones-completed: updated-milestones,
            progress-percentage: new-progress,
            last-updated: stacks-block-height
          }))
      
      ;; Update user milestone count
      (map-set user-learning-stats user
        (merge user-stats {total-milestones: (+ (get total-milestones user-stats) u1)}))
      
      ;; Award reputation for milestone completion
      (award-milestone-reputation user)
      (ok new-progress))))

;; Complete learning path
(define-public (complete-learning-path (path-id uint))
  (let ((user tx-sender)
        (enrollment-key (tuple (user user) (path-id path-id)))
        (path-data (unwrap! (map-get? learning-paths path-id) err-path-not-found))
        (progress-data (unwrap! (map-get? user-path-progress enrollment-key) err-not-enrolled))
        (user-stats (get-user-learning-stats user))
        (path-stats (unwrap-panic (map-get? path-enrollments path-id))))
    (asserts! (not (get is-completed progress-data)) err-path-completed)
    (asserts! (is-eq (get progress-percentage progress-data) u100) err-milestone-not-reached)
    
    (let ((completion-time (- stacks-block-height (get enrolled-at progress-data)))
          (completion-reward (get completion-reward path-data)))
      
      ;; Mark path as completed
      (map-set user-path-progress enrollment-key
        (merge progress-data
          {
            is-completed: true,
            completion-time: (some completion-time)
          }))
      
      ;; Update user stats and award completion reward
      (map-set user-learning-stats user
        (merge user-stats 
          {
            paths-completed: (+ (get paths-completed user-stats) u1),
            learning-reputation: (+ (get learning-reputation user-stats) completion-reward)
          }))
      
      ;; Update path statistics
      (let ((new-total-completed (+ (get total-completed path-stats) u1))
            (new-avg-time (calculate-average-completion-time 
              (get average-completion-time path-stats)
              (get total-completed path-stats)
              completion-time)))
        (map-set path-enrollments path-id
          (merge path-stats 
            {
              total-completed: new-total-completed,
              average-completion-time: new-avg-time
            })))
      
      (ok completion-reward))))

;; Helper functions
(define-private (check-prerequisites (user principal) (prerequisites (list 5 (string-ascii 50))))
  (let ((user-skills (contract-call? .Skilltag get-user-skills user)))
    (fold check-single-prerequisite prerequisites true)))

(define-private (check-single-prerequisite (required-skill (string-ascii 50)) (acc bool))
  (if acc
    (let ((user-skills (contract-call? .Skilltag get-user-skills tx-sender)))
      (is-some (index-of user-skills required-skill)))
    false))

(define-private (update-milestone-list (milestones (list 10 bool)) (index uint))
  ;; For simplicity, assume milestone at index is completed
  (if (is-eq index u0)
    (list true false false false false false false false false false)
    (if (is-eq index u1)
      (list true true false false false false false false false false)
      (list true true true false false false false false false false))))

(define-private (award-milestone-reputation (user principal))
  (let ((current-reputation (contract-call? .Skilltag get-user-reputation user)))
    ;; Just return true for now, external reputation handling can be done in UI
    true))

(define-private (calculate-average-completion-time (current-avg uint) (completed-count uint) (new-time uint))
  (if (is-eq completed-count u0)
    new-time
    (/ (+ (* current-avg completed-count) new-time) (+ completed-count u1))))

;; Read-only functions
(define-read-only (get-learning-path (path-id uint))
  (map-get? learning-paths path-id))

(define-read-only (get-user-progress (user principal) (path-id uint))
  (map-get? user-path-progress (tuple (user user) (path-id path-id))))

(define-read-only (get-user-learning-stats (user principal))
  (default-to 
    {paths-enrolled: u0, paths-completed: u0, total-milestones: u0, learning-reputation: u0}
    (map-get? user-learning-stats user)))

(define-read-only (get-path-statistics (path-id uint))
  (map-get? path-enrollments path-id))

(define-read-only (get-available-paths-by-difficulty (difficulty uint))
  (ok {
    path-1: (if (and 
      (is-some (get-learning-path u1))
      (is-eq (get difficulty-level (unwrap-panic (get-learning-path u1))) difficulty))
      (get-learning-path u1) none),
    path-2: (if (and 
      (is-some (get-learning-path u2))
      (is-eq (get difficulty-level (unwrap-panic (get-learning-path u2))) difficulty))
      (get-learning-path u2) none),
    path-3: (if (and 
      (is-some (get-learning-path u3))
      (is-eq (get difficulty-level (unwrap-panic (get-learning-path u3))) difficulty))
      (get-learning-path u3) none)
  }))

(define-read-only (calculate-path-completion-rate (path-id uint))
  (let ((path-stats (unwrap! (map-get? path-enrollments path-id) (err u0))))
    (if (> (get total-enrolled path-stats) u0)
      (ok (/ (* (get total-completed path-stats) u100) (get total-enrolled path-stats)))
      (ok u0))))

(define-read-only (get-user-recommended-paths (user principal))
  (let ((user-skills (contract-call? .Skilltag get-user-skills user))
        (user-stats (get-user-learning-stats user)))
    (ok {
      skill-count: (len user-skills),
      completed-paths: (get paths-completed user-stats),
      learning-reputation: (get learning-reputation user-stats),
      recommendation: (if (< (len user-skills) u3) 
        "Start with beginner paths"
        "Try intermediate or advanced paths")
    })))

;; Deactivate learning path (creator only)
(define-public (deactivate-learning-path (path-id uint))
  (let ((path-data (unwrap! (map-get? learning-paths path-id) err-path-not-found)))
    (asserts! (is-eq tx-sender (get creator path-data)) err-not-authorized)
    (map-set learning-paths path-id (merge path-data {is-active: false}))
    (ok true)))