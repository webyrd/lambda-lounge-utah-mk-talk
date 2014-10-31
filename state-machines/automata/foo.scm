(load "../mk.scm")
(load "../pmatch.scm")

(define M1
  (lambda (str)
    (letrec ([S0 (lambda (str)
                   (pmatch str
                     [() 'accept]
                     [(0 . ,d) (S0 d)]
                     [(1 . ,d) (S1 d)]))]
             [S1 (lambda (str)
                   (pmatch str
                     [() 'reject]
                     [(0 . ,d) (S2 d)]
                     [(1 . ,d) (S0 d)]))]
             [S2 (lambda (str)
                   (pmatch str
                     [() 'reject]
                     [(0 . ,d) (S1 d)]
                     [(1 . ,d) (S2 d)]))])
      (S0 str))))

(define M1o
  (lambda (str out)
    (letrec ([S0 (lambda (str)
                   (conde
                     [(== '() str) (== 'accept out)]
                     [(fresh (d)
                        (conde
                          [(== `(0 . ,d) str) (S0 d)]
                          [(== `(1 . ,d) str) (S1 d)]))]))]
             [S1 (lambda (str)
                   (conde
                     [(== '() str) (== 'reject out)]
                     [(fresh (d)
                        (conde
                          [(== `(0 . ,d) str) (S2 d)]
                          [(== `(1 . ,d) str) (S0 d)]))]))]
             [S2 (lambda (str)
                   (conde
                     [(== '() str) (== 'reject out)]
                     [(fresh (d)
                        (conde
                          [(== `(0 . ,d) str) (S1 d)]
                          [(== `(1 . ,d) str) (S2 d)]))]))])
      (S0 str))))


(define M2o
  (lambda (str out)
    (letrec ([S1 (lambda (str)
                   (conde
                     [(== '() str) (== 'accept out)]
                     [(conde
                        [(S3 str)] ; epsilon
                        [(fresh (d)
                           (== `(b . ,d) str)
                           (S2 d))])]))]
             [S2 (lambda (str)
                   (conde
                     [(== '() str) (== 'reject out)]
                     [(fresh (d)
                        (conde
                          [(== `(a . ,d) str)
                           (conde
                             [(S2 d)]
                             [(S3 d)])]
                          [(== `(b . ,d) str) (S3 d)]))]))]
             [S3 (lambda (str)
                   (conde
                     [(== '() str) (== 'reject out)]
                     [(fresh (d)
                        (== `(a . ,d) str)
                        (S1 d))]))])
      (S1 str))))


(define M3o
  (lambda (str)
    (letrec ([S1 (lambda (str)
                   (conde
                     [(== '() str)]
                     [(conde
                        [(S3 str)] ; epsilon
                        [(fresh (d)
                           (== `(b . ,d) str)
                           (S2 d))])]))]
             [S2 (lambda (str)
                   (conde
                     [(fresh (d)
                        (conde
                          [(== `(a . ,d) str)
                           (conde
                             [(S2 d)]
                             [(S3 d)])]
                          [(== `(b . ,d) str) (S3 d)]))]))]
             [S3 (lambda (str)
                   (conde
                     [(fresh (d)
                        (== `(a . ,d) str)
                        (S1 d))]))])
      (S1 str))))
