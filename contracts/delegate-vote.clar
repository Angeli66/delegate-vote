;; ------------------------------------------------------------
;; delegate-vote.clar
;; Stacks-compatible vote delegation
;; ------------------------------------------------------------

(define-constant ERR-INVALID-DELEGATE u100)
(define-constant ERR-ALREADY-DELEGATED u101)
(define-constant ERR-NO-DELEGATION u102)
(define-constant ERR-PROPOSAL-NOT-FOUND u103)
(define-constant ERR-ALREADY-VOTED u104)
(define-constant ERR-VOTING-CLOSED u105)

;; ------------------------------------------------------------
;; Delegation state
;; ------------------------------------------------------------

;; delegator -> delegate
(define-map delegation
  { delegator: principal }
  { delegate: principal }
)

;; delegate -> voting power
(define-map delegate-power
  { delegate: principal }
  { power: uint }
)

(define-private (get-power (d principal))
  (default-to u0 (get power (map-get? delegate-power { delegate: d })))
)

;; ------------------------------------------------------------
;; Proposals
;; ------------------------------------------------------------

(define-data-var proposal-count uint u0)

(define-map proposals
  { id: uint }
  {
    creator: principal,
    description: (string-ascii 200),
    start: uint,
    end: uint,
    yes: uint,
    no: uint,
    open: bool
  }
)

;; proposal + voter tracking
(define-map votes
  { id: uint, voter: principal }
  { voted: bool }
)

;; ------------------------------------------------------------
;; Delegate vote
;; ------------------------------------------------------------

(define-public (delegate (to principal))
  (let ((sender tx-sender))
    (if (is-eq sender to)
        (err ERR-INVALID-DELEGATE)
        (match (map-get? delegation { delegator: sender })
          some-existing (err ERR-ALREADY-DELEGATED)
          (begin
            (map-set delegation { delegator: sender } { delegate: to })
            (map-set delegate-power
              { delegate: to }
              { power: (+ (get-power to) u1) }
            )
            (ok true)
          )
        )
    )
  )
)

;; ------------------------------------------------------------
;; Revoke delegation
;; ------------------------------------------------------------

(define-public (revoke-delegation)
  (let ((sender tx-sender))
    (match (map-get? delegation { delegator: sender })
      some-d
        (begin
          (map-delete delegation { delegator: sender })
          (map-set delegate-power
            { delegate: (get delegate some-d) }
            { power: (- (get-power (get delegate some-d)) u1) }
          )
          (ok true)
        )
      (err ERR-NO-DELEGATION)
    )
  )
)

;; ------------------------------------------------------------
;; Create proposal
;; ------------------------------------------------------------

(define-public (create-proposal (description (string-ascii 200)) (duration uint))
  (let ((id (+ (var-get proposal-count) u1)))
    (begin
      (asserts! (> (len description) u0) (err ERR-INVALID-DELEGATE))
      (asserts! (> duration u0) (err ERR-INVALID-DELEGATE))
      (var-set proposal-count id)
      (map-set proposals
        { id: id }
        {
          creator: tx-sender,
          description: description,
          start: u0,
          end: (+ u0 duration),
          yes: u0,
          no: u0,
          open: true
        }
      )
      (ok id)
    )
  )
)

;; ------------------------------------------------------------
;; Vote (delegates vote with accumulated power)
;; ------------------------------------------------------------

(define-public (vote (proposal-id uint) (support bool))
  (let ((sender tx-sender)
        (proposal-check (asserts! (> proposal-id u0) (err ERR-INVALID-DELEGATE))))
    (match (map-get? proposals { id: proposal-id })
      some-p
        (if (or (not (get open some-p)) (> u0 (get end some-p)))
            (err ERR-VOTING-CLOSED)
            (match (map-get? votes { id: proposal-id, voter: sender })
              some-voted (err ERR-ALREADY-VOTED)
              (let ((weight (+ u1 (get-power sender))))
                (begin
                  (asserts! (is-some (map-get? proposals { id: proposal-id })) (err ERR-PROPOSAL-NOT-FOUND))
                  (map-set votes { id: proposal-id, voter: sender } { voted: true })
                    (if support
                        (map-set proposals { id: proposal-id }
                          {
                            creator: (get creator some-p),
                            description: (get description some-p),
                            start: (get start some-p),
                            end: (get end some-p),
                            yes: (+ (get yes some-p) weight),
                            no: (get no some-p),
                            open: (get open some-p)
                          })
                        (map-set proposals { id: proposal-id }
                          {
                            creator: (get creator some-p),
                            description: (get description some-p),
                            start: (get start some-p),
                            end: (get end some-p),
                            yes: (get yes some-p),
                            no: (+ (get no some-p) weight),
                            open: (get open some-p)
                          })
                    )
                    (ok true)
                  )
                )
            )
        )
      (err ERR-PROPOSAL-NOT-FOUND)
    )
  )
)

;; ------------------------------------------------------------
;; Finalize proposal
;; ------------------------------------------------------------

(define-public (finalize (proposal-id uint))
  (if (> proposal-id u0)
    (match (map-get? proposals { id: proposal-id })
      some-p
        (if (get open some-p)
            (begin
              (map-set proposals { id: proposal-id }
                {
                  creator: (get creator some-p),
                  description: (get description some-p),
                  start: (get start some-p),
                  end: (get end some-p),
                  yes: (get yes some-p),
                  no: (get no some-p),
                  open: false
                })
              (if (> (get yes some-p) (get no some-p))
                  (ok { result: "passed" })
                  (ok { result: "rejected" })
              )
            )
            (err ERR-VOTING-CLOSED)
        )
      (err ERR-PROPOSAL-NOT-FOUND)
    )
    (err ERR-INVALID-DELEGATE)
  )
)
