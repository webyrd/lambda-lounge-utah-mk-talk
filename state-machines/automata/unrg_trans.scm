Vicare Scheme version 0.1d2+, 64-bit (revision 08bd828acfa9382324150b41f4e86c540c10a886, build 2013-08-27)
Copyright (c) 2006-2010 Abdulaziz Ghuloum and contributors

> (load "foo.scm")
> (run 1 (x) (== x 5))
(5)
> (run 2 (x) (== x 5))
(5)
> (run* (x) (== x 5))
(5)
> (run* (x) (== 5 5))
(_.0)
> (run* (x) (== 5 6))
()
> (run* (x) (fresh (y z) (== y 5) (== z 6)))
(_.0)
> (run* (x) (fresh (y z) (== y 5) (== z 6) (== `(,y ,z) x)))
((5 6))
> (run* (q) (fresh (y z) (== y 5) (== z 6) (== `(,y ,z) q)))
((5 6))
> (run* (q)
    (fresh (y z)
      (conde
        [(== y 5) (== z 6)]
        [(== z 'foo)])            
      (== `(,y ,z) q)))
((5 6)
 (_.0 foo))
> (run* (q)
    (fresh (y z)
      (conde
        [(== y 5) (== z 6) (== `(,y ,z) q)]
        [(== z 'foo) (== `(,y ,z) q)])))
((5 6) (_.0 foo))
> (run* (q)
    (fresh (y z)
      (conde
        [(== y 5) (== z 6) (== `(,y ,z) q)]
        [(=/= z 'foo) (== `(,y ,z) q)])))
((5 6) ((_.0 _.1) (=/= ((_.1 foo)))))
> (run* (q) (=/= q 5))
((_.0 (=/= ((_.0 5)))))
> (run* (q) (=/= q 5) (== q 5))
()
> (run* (q) (== q 5) (=/= q 5))
()
> (run* (q) (== q 6) (=/= q 5))
(6)
> (run* (q)
    (fresh (y z)
      (conde
        [(== y 5) (== z 6)]
        [(== z 'foo)])            
      (== `(,y ,z) q)))
((5 6) (_.0 foo))
> (run* (q)
    (fresh (y z)
      (conde
        [(== z 'foo)]
        [(== y 5) (== z 6)])            
      (== `(,y ,z) q)))
((_.0 foo) (5 6))
> (define append
    (lambda (l s)
      (cond
        [(null? l) s]
        [else (cons (car l) (append (cdr l) s))])))
> (append '(a b c) '(d e))
(a b c d e)
> (define appendo
    (lambda (l s out)
      (conde
        [(== '() l) (== s out)]
        [(fresh (a d res)
           (== `(,a . ,d) l)
           (appendo d s res)
           (== `(,a . ,res) out))])))
> (run 1 (q) (appendo '(a b c) '(d e) q))
((a b c d e))
> (run 1 (q) (appendo '(a b c) '(d e) '(a b c d e)))
(_.0)
> (run 1 (q) (appendo '(a b c) q '(a b c d e)))
((d e))
> (run 1 (q) (fresh (l s) (appendo l s '(a b c d e)) (== `(,l ,s) q)))
((() (a b c d e)))
> (run 3 (q) (fresh (l s) (appendo l s '(a b c d e)) (== `(,l ,s) q)))
((() (a b c d e)) ((a) (b c d e)) ((a b) (c d e)))
> (run 5 (q) (fresh (l s) (appendo l s '(a b c d e)) (== `(,l ,s) q)))
((() (a b c d e)) ((a) (b c d e)) ((a b) (c d e))
  ((a b c) (d e)) ((a b c d) (e)))
> (run 6 (q) (fresh (l s) (appendo l s '(a b c d e)) (== `(,l ,s) q)))
((() (a b c d e)) ((a) (b c d e)) ((a b) (c d e))
  ((a b c) (d e)) ((a b c d) (e)) ((a b c d e) ()))
> (run 7 (q) (fresh (l s) (appendo l s '(a b c d e)) (== `(,l ,s) q)))
  C-c C-cUnhandled exception
 Condition components:
   1. &interrupted
   2. &message: "received an interrupt signal"
> (define appendo
    (lambda (l s out)
      (conde
        [(fresh (a d res)
           (== `(,a . ,d) l)
           (appendo d s res)
           (== `(,a . ,res) out))]
        [(== '() l) (== s out)])))
> (run 7 (q) (fresh (l s) (appendo l s '(a b c d e)) (== `(,l ,s) q)))
  C-c C-cUnhandled exception
 Condition components:
   1. &interrupted
   2. &message: "received an interrupt signal"
> r
Unhandled exception
 Condition components:
   1. &undefined
   2. &who: eval
   3. &message: "unbound variable"
   4. &irritants: (r)
> (define appendo
    (lambda (l s out)
      (conde
        [(== '() l) (== s out)]
        [(fresh (a d res)
           (== `(,a . ,d) l)
           (== `(,a . ,res) out)
           (appendo d s res))])))
> (run 7 (q) (fresh (l s) (appendo l s '(a b c d e)) (== `(,l ,s) q)))
((() (a b c d e)) ((a) (b c d e)) ((a b) (c d e))
  ((a b c) (d e)) ((a b c d) (e)) ((a b c d e) ()))
> (run* (q) (fresh (l s) (appendo l s '(a b c d e)) (== `(,l ,s) q)))
((() (a b c d e)) ((a) (b c d e)) ((a b) (c d e))
  ((a b c) (d e)) ((a b c d) (e)) ((a b c d e) ()))
> (define appendo
    (lambda (l s out)
      (conde
        [(fresh (a d res)
           (== `(,a . ,d) l)
           (== `(,a . ,res) out)
           (appendo d s res))]
        [(== '() l) (== s out)])))
> (run* (q) (fresh (l s) (appendo l s '(a b c d e)) (== `(,l ,s) q)))
((() (a b c d e)) ((a) (b c d e)) ((a b) (c d e))
  ((a b c) (d e)) ((a b c d) (e)) ((a b c d e) ()))
> (load "foo.scm")
> (M1 '())
accept
> (M1 '(1 0 0))
reject
> (M1 '(1 0 1))
reject
> (M1 '(1 1 0))
accept
> (load "foo.scm")
> (run* (q) (M1o '() q))
(accept)
> (run* (q) (M1o '(1 0 0) q))
(reject)
> (run* (q) (M1o '(1 1 0) q))
(accept)
> (run 10 (q) (M1o q 'accept))
(() (0) (0 0) (1 1) (0 0 0) (1 1 0) (0 1 1) (1 0 0 1)
  (0 0 0 0) (1 1 0 0))
> (run 10 (q) (M1o q 'reject))
((1) (0 1) (1 0) (0 0 1) (0 1 0) (1 0 0) (1 1 1) (1 0 1)
  (0 0 0 1) (0 0 1 0))
> (run 10 (q) (M1o `(0 . ,q) 'reject))
((1) (0 1) (1 0) (0 0 1) (0 1 0) (1 0 0) (1 1 1) (1 0 1)
  (0 0 0 1) (0 0 1 0))
> (run 10 (q) (fresh (d) (== `(0 . ,d) q) (M1o q 'reject)))
((0 1) (0 0 1) (0 1 0) (0 0 0 1) (0 0 1 0) (0 1 0 0)
  (0 1 1 1) (0 1 0 1) (0 0 0 0 1) (0 0 0 1 0))
> 

Process scheme finished
Vicare Scheme version 0.1d2+, 64-bit (revision 08bd828acfa9382324150b41f4e86c540c10a886, build 2013-08-27)
Copyright (c) 2006-2010 Abdulaziz Ghuloum and contributors

> (load "foo.scm")
> (run 10 (q) (M2o q 'accept))
(() (a) (a a) (b b a) (b a a) (a a a) (b b a a) (a b b a)
  (a b a a) (b a b a))
> (run 20 (q) (M2o q 'accept))
(() (a) (a a) (b b a) (b a a) (a a a) (b b a a) (a b b a)
  (a b a a) (b a b a) (b a a a) (a a a a) (b b a a a)
  (a b b a a) (a a b b a) (b b a b b a) (b a a a)
  (a a b a a) (b b a b a a) (a b a b a))
> (run 20 (q) (M2o q 'reject))
(() (b) (a) (b b) (a b) (b a) (b a) (a a) (b b a) (a b b)
  (a a b) (b b a b) (a b a) (a b a) (b a b) (b a a) (a a a)
  (b b a a) (a b b a) (b a a))
> 

Process scheme finished
Vicare Scheme version 0.1d2+, 64-bit (revision 08bd828acfa9382324150b41f4e86c540c10a886, build 2013-08-27)
Copyright (c) 2006-2010 Abdulaziz Ghuloum and contributors

> (load "foo.scm")
> (run 20 (q) (M2o q 'reject))
(() (b) (a) (b b) (a b) (b a) (b a) (a a) (b b a) (a b b)
  (a a b) (b b a b) (a b a) (a b a) (b a b) (b a a) (a a a)
  (b b a a) (a b b a) (b a a))
> (run 20 (q) (M3o q))
(() (a) (a a) (b b a) (b a a) (a a a) (b b a a) (a b b a)
  (a b a a) (b a b a) (b a a a) (a a a a) (b b a a a)
  (a b b a a) (a a b b a) (b b a b b a) (b a a a)
  (a a b a a) (b b a b a a) (a b a b a))
> (run 20 (q) (M2o q 'reject))
(() (b) (a) (b b) (a b) (b a) (b a) (a a) (b b a) (a b b)
  (a a b) (b b a b) (a b a) (a b a) (b a b) (b a a) (a a a)
  (b b a a) (a b b a) (b a a))
> (run 20 (q) (M2o q 'accept))
(() (a) (a a) (b b a) (b a a) (a a a) (b b a a) (a b b a)
  (a b a a) (b a b a) (b a a a) (a a a a) (b b a a a)
  (a b b a a) (a a b b a) (b b a b b a) (b a a a)
  (a a b a a) (b b a b a a) (a b a b a))
> (run 20 (q) (M2o '(b b b) 'accept))
()
> (run 20 (q) (M2o '(b b b) 'reject))
()
> 