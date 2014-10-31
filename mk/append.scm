(load "mk.scm")
(load "matche.scm")

(define append
  (lambda (l s)
    (cond
      [(null? l) s]
      [else (cons (car l) (append (cdr l) s))])))













(define appendo
  (lambda (l s out)
    (conde
      [(== '() l) (== s out)]
      [(fresh (a d res)
         (== (cons a d) l)
         (== (cons a res) out)
         (appendo d s res))])))

(define appendo
  (lambda (l s out)
    (matche (l s out)
      [(() ,s ,s)]
      [((,a . ,d) ,s (,a . ,res)) (appendo d s res)])))
