(load "mk.scm")

; x variables   DUN
; (lambda (x) x)  abstraction
; (e1 e2) application

; x -> 5

; ((x . 5) (y . 7))

(define evalo
  (lambda (expr env val)
    (conde
      [(symbolo expr)
       (lookupo expr env val)]
      [(fresh (x body)
         (== `(lambda (,x) ,body) expr)
         (== `(closure ,x ,body ,env) val))]
      [(fresh (e1 e2 x body env^ arg)
         (== `(,e1 ,e2) expr)
         (evalo e1 env `(closure ,x ,body ,env^))
         (evalo e2 env arg)
         (evalo body `((,x . ,arg) . ,env^) val))])))

(define lookupo
  (lambda (x env val)
    (fresh (y v rest)
      (== `((,y . ,v) . ,rest) env)
      (conde
        [(== x y) (== v val)]
        [(=/= x y) (lookupo x rest val)]))))
