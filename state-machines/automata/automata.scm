(load "../pmatch.scm")
(load "../mk.scm")

(define accepto
  (lambda (str)
    (letrec ([q1o (lambda (str stack)
                    (conde
                      [(== '() str)
                       (== '() stack)]
                      [(q2o str `($ . ,stack))]))]
             [q2o (lambda (str stack)
                    (fresh (rest rest-stack)
                      (conde
                        [(== `(0 . ,rest) str)
                         (q2o rest `(0 . ,stack))]
                        [(== `(1 . ,rest) str)
                         (== `(0 . ,rest-stack) stack)
                         (q3o rest rest-stack)])))]
             [q3o (lambda (str stack)
                    (conde
                      [(fresh (rest  rest-stack)
                         (conde
                           [(== `(1 . ,rest) str)
                            (== `(0 . ,rest-stack) stack)
                            (q3o rest rest-stack)]
                           [(== `($ . ,rest-stack) stack)
                            (q4o str rest-stack)]))]))]
             [q4o (lambda (str stack)
                    (conde
                      [(== '() str)
                       (== '() stack)]))])
      (q1o str '()))))



(define accepto
  (lambda (str)
    (letrec ([S1o (lambda (str)
                    (conde
                      [(== '() str)]
                      [(S3o str)]
                      [(fresh (rest)
                         (conde
                           ; epsilon ???
                           [(== `(b . ,rest) str)
                            (S2o rest)]))]))]
             [S2o (lambda (str)
                    (conde                      
                      [(fresh (rest)
                         (conde
                           [(== `(a . ,rest) str)
                            (conde
                              [(S2o rest)]
                              [(S3o rest)])]
                           [(== `(b . ,rest) str)
                            (S3o rest)]))]))]
             [S3o (lambda (str)
                    (conde
                      [(fresh (rest)
                         (conde
                           [(== `(a . ,rest) str)
                            (S1o rest)]))]))])
      (S1o str))))




(define accepto
  (lambda (str)
    (letrec ([S1o (lambda (str)
                    (conde
                      [(== '() str)]
                      [(S3o str)]
                      [(fresh (rest)
                         (conde
                           ; epsilon ???
                           [(== `(b . ,rest) str)
                            (S2o rest)]))]))]
             [S2o (lambda (str)
                    (conde                      
                      [(fresh (rest)
                         (conde
                           [(== `(a . ,rest) str)
                            (conde
                              [(S2o rest)]
                              [(S3o rest)])]
                           [(== `(b . ,rest) str)
                            (S3o rest)]))]))]
             [S3o (lambda (str)
                    (conde
                      [(fresh (rest)
                         (conde
                           [(== `(a . ,rest) str)
                            (S1o rest)]))]))])
      (S1o str))))

(define accepto
  (lambda (str out)
    (letrec ([S1o (lambda (str)
                    (conde
                      [(== '() str) (== 'accept out)]
                      [(S3o str)]
                      [(fresh (rest)
                         (conde
                           ; epsilon ???
                           [(== `(b . ,rest) str)
                            (S2o rest)]))]))]
             [S2o (lambda (str)
                    (conde
                      [(== '() str) (== 'reject out)]
                      [(fresh (rest)
                         (conde
                           [(== `(a . ,rest) str)
                            (conde
                              [(S2o rest)]
                              [(S3o rest)])]
                           [(== `(b . ,rest) str)
                            (S3o rest)]))]))]
             [S3o (lambda (str)
                    (conde
                      [(== '() str) (== 'reject out)]
                      [(fresh (rest)
                         (conde
                           [(== `(a . ,rest) str)
                            (S1o rest)]))]))])
      (S1o str))))




















(define accepto
  (lambda (str out)
    (letrec ([S0o (lambda (str)
                    (conde
                      [(== '() str) (== 'accept out)]
                      [(fresh (rest)
                         (conde
                           [(== `(0 . ,rest) str)
                            (S0o rest)]
                           [(== `(1 . ,rest) str)
                            (S1o rest)]))]))]
             [S1o (lambda (str)
                    (conde
                      [(== '() str) (== 'reject out)]
                      [(fresh (rest)
                         (conde
                           [(== `(0 . ,rest) str)
                            (S2o rest)]
                           [(== `(1 . ,rest) str)
                            (S0o rest)]))]))]
             [S2o (lambda (str)
                    (conde
                      [(== '() str) (== 'reject out)]
                      [(fresh (rest)
                         (conde
                           [(== `(0 . ,rest) str)
                            (S1o rest)]
                           [(== `(1 . ,rest) str)
                            (S2o rest)]))]))])
      (S0o str))))

(run 1 (q) (accepto '(1 0 0 1) q))





(letrec ([S0 (lambda (str)
               (pmatch str
                 [() 'accept]
                 [(0 . ,rest) (S0 rest)]
                 [(1 . ,rest) (S1 rest)]))]
         [S1 (lambda (str)
               (pmatch str
                 [() 'reject]
                 [(0 . ,rest) (S2 rest)]
                 [(1 . ,rest) (S0 rest)]))]
         [S2 (lambda (str)
               (pmatch str
                 [() 'reject]
                 [(0 . ,rest) (S1 rest)]
                 [(1 . ,rest) (S2 rest)]))])
  (S0 '(1 0 0 1)))




(let ((x (+ 2 3)))
  (* x x))

(letrec ([even?
          (lambda (n)
            (cond
              [(zero? n) #t]
              [else (odd? (sub1 n))]))]
         [odd?
          (lambda (n)
            (cond
              [(zero? n) #f]
              [else (even? (sub1 n))]))])
  (list even? odd?))


(define even?
  (lambda (n)
    (cond
      [(zero? n) #t]
      [else (odd? (sub1 n))])))

(define odd?
  (lambda (n)
    (cond
      [(zero? n) #f]
      [else (even? (sub1 n))])))
