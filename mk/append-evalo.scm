(load "mk.scm")
(load "interp-helpers.scm")

(define evalo
  (lambda (exp val)
    (eval-expo exp '() val)))

(define eval-expo
  (lambda (exp env val)
    (fresh ()
      (absento 'closure exp)
      (conde
        ((fresh (v)
           (== `(quote ,v) exp)         
           (not-in-envo 'quote env)
           (== v val)))
        ((prim-expo exp env val))
        ((symbolo exp) (lookupo exp env val))
        ((fresh (rator x* rands body env^ a* res)
           (== `(,rator . ,rands) exp)
           (eval-expo rator env `(closure ,x* ,body ,env^))
           (proper-listo rands env a*)
           (ext-env*o x* a* env^ res)
           (eval-expo body res val)))
        ((fresh (x* body)
           (== `(lambda ,x* ,body) exp)
           (not-in-envo 'lambda env)
           (== `(closure ,x* ,body ,env) val)))))))

(define ext-env*o
  (lambda (x* a* env out)
    (conde
      ((== '() x*) (== '() a*) (== env out))
      ((fresh (x a dx* da* env2)
         (== `(,x . ,dx*) x*)
         (== `(,a . ,da*) a*)
         (== `((,x . ,a) . ,env) env2)
         (ext-env*o dx* da* env2 out))))))

(define prim-expo
  (lambda (exp env val)
    (conde
      ((boolean-primo exp env val))    
      ((cons-primo exp env val))
      ((car-primo exp env val))
      ((cdr-primo exp env val))
      ((null?-primo exp env val))
      ((if-primo exp env val)))))

(define boolean-primo
  (lambda (exp env val)
    (conde
      ((== #t exp) (== #t val))
      ((== #f exp) (== #f val)))))

(define cons-primo
  (lambda (exp env val)
    (fresh (a d v-a v-d)
      (== `(cons ,a ,d) exp)
      (== `(,v-a . ,v-d) val)
      (not-in-envo 'cons env)
      (eval-expo a env v-a)
      (eval-expo d env v-d))))

(define car-primo
  (lambda (exp env val)
    (fresh (p d)
      (== `(car ,p) exp)
      (=/= 'closure val)
      (not-in-envo 'car env)
      (eval-expo p env `(,val . ,d)))))

(define cdr-primo
  (lambda (exp env val)
    (fresh (p a)
      (== `(cdr ,p) exp)
      (=/= 'closure a)
      (not-in-envo 'cdr env)
      (eval-expo p env `(,a . ,val)))))

(define null?-primo
  (lambda (exp env val)
    (fresh (e)
      (== `(null? ,e) exp)
      (not-in-envo 'null? env)
      (conde
        [(== #t val)
         (eval-expo e env '())]
        [(== #f val)
         (fresh (v n rest x y)
           (conde
             [(== #f v)]
             [(== #t v)]
             [; any pair, including closures
              (== `(,x . ,y) v)])
           (eval-expo e env v))]))))

(define if-primo
  (lambda (exp env val)
    (fresh (e1 e2 e3 t)
      (== `(if ,e1 ,e2 ,e3) exp)
      (not-in-envo 'if env)
      (eval-expo e1 env t)
      (conde
        ((== #t t) (eval-expo e2 env val))
        ((== #f t) (eval-expo e3 env val))))))



(let ()
  ;; in Scheme
  
  (define Y
    (lambda (fun)
      ((lambda (F)
         (F F))
       (lambda (F)
         (fun (lambda (x) ((F F) x)))))))

  (define append-body
    (lambda (append)
      (lambda (ls1)
        (lambda (ls2)
          (if (null? ls1)
              ls2
              (cons (car ls1) ((append (cdr ls1)) ls2)))))))

  (define append-body-smart-cps
    (lambda (append)
      (lambda (ls1)
        (lambda (ls2 k)
          (if (null? ls1)
              (k ls2)
              ((append (cdr ls1)) ls2
               (lambda (v)
                 (k (cons (car ls1) v)))))))))

  

  ;; started with direct style, uncurried, then CPSed, then Curried & prepared for Y combinator
  ;;
  ;; clearly this isn't really in CPS anymore, since there is a (single) serious call in non-tail position
  (define append-body-fake-cps
    (lambda (append-cps)
      (lambda (ls1)
        (lambda (ls2)
          (lambda (k)
            (if (null? ls1)
                (k ls2)
                (((append-cps
                   (cdr ls1))
                  ls2)
                 (lambda (v)
                   (k (cons (car ls1) v))))))))))

  
  ;; Obviously the calls that return procedures can't diverge  
  (define append-body-naive-cps
    (lambda (append-cps k)
      (k (lambda (ls1 k)
           (k (lambda (ls2 k)
                (if (null? ls1)
                    (k ls2)
                    (append-cps (cdr ls1)
                                (lambda (p)
                                  (p ls2
                                     (lambda (v)
                                       (k (cons (car ls1) v)))))))))))))



  
  (((Y append-body) '(a b c)) '(d e))

  ;; This call is bogus, since there is a (single) serious call in non-tail position
  ((((Y append-body-fake-cps) '(a b c)) '(d e)) (lambda (v) v))

  (((Y append-body-smart-cps) '(a b c)) '(d e) (lambda (v) v))
  
  ;; arrrgh!  would need to make Y handle another arg, I think
  ;;
  ;; time for 'letrec', methinks
  ;;
  ;; (Y append-body-naive-cps
  ;;    (lambda (p1)
  ;;      (p1 '(a b c)
  ;;          (lambda (p2)
  ;;            (p2 '(d e)
  ;;                (lambda (v) v))))))
  
  )

;; in mK
  
(define Y
  '(lambda (fun)
     ((lambda (F)
        (F F))
      (lambda (F)
        (fun (lambda (x) ((F F) x)))))))

(define append-body
  '(lambda (append)
     (lambda (ls1)
       (lambda (ls2)
         (if (null? ls1)
             ls2
             (cons (car ls1) ((append (cdr ls1)) ls2)))))))

;; forward
(run* (q) (evalo `(((,Y ,append-body) '(a b c)) '(d e)) q))


;; check
(run* (q) (evalo `(((,Y ,append-body) '(a b c)) '(d e)) '(a b c d e)))


;; backwards
(run 1 (q) (evalo `(((,Y ,append-body) ,q) '(d e)) '(a b c d e)))
; => ('(a b c))

(run 2 (q) (evalo `(((,Y ,append-body) ,q) '(d e)) '(a b c d e)))
; =>
; ('(a b c) ((lambda () '(a b c))))
; lol!!

(run 3 (q) (evalo `(((,Y ,append-body) ,q) '(d e)) '(a b c d e)))
; =>
('(a b c) ((lambda () '(a b c)))
  ((car '((a b c) . _.0)) (absento (closure _.0))))


; quote trick
(run 1 (q) (evalo `(((,Y ,append-body) (quote ,q)) '(d e)) '(a b c d e)))
; =>
; ((a b c))

; nice!!!
(run* (q) (evalo `(((,Y ,append-body) (quote ,q)) '(d e)) '(a b c d e)))
; =>
; ((a b c))



(run 1 (q) (evalo `(((,Y ,append-body) '(a b c)) ,q) '(a b c d e)))
; => ('(d e))

(run 2 (q) (evalo `(((,Y ,append-body) '(a b c)) ,q) '(a b c d e)))
; =>
; ('(d e) ((lambda () '(d e))))

(run 3 (q) (evalo `(((,Y ,append-body) '(a b c)) ,q) '(a b c d e)))
;=>
; ('(d e) ((lambda () '(d e)))
;   ((car '((d e) . _.0)) (absento (closure _.0))))


; quote trick
(run 1 (q) (evalo `(((,Y ,append-body) '(a b c)) (quote ,q)) '(a b c d e)))
; =>
; ((d e))

(run* (q) (evalo `(((,Y ,append-body) '(a b c)) (quote ,q)) '(a b c d e)))
; =>
; ((d e))


; two variables:
(run 1 (q) (fresh (x y) (evalo `(((,Y ,append-body) ,x) ,y) '(a b c d e)) (== (list x y) q)))
; =>
; (('() '(a b c d e)))

(run 6 (q) (fresh (x y) (evalo `(((,Y ,append-body) ,x) ,y) '(a b c d e)) (== (list x y) q)))
; =>
; (('() '(a b c d e))
;  ('(a) '(b c d e))
;  ('(a b) '(c d e))
;  ('(a b c) '(d e))
;  ('(a b c d) '(e))
;  ('(a b c d e) '()))

(run 7 (q) (fresh (x y) (evalo `(((,Y ,append-body) ,x) ,y) '(a b c d e)) (== (list x y) q)))
; =>
; (('() '(a b c d e))
;  ('(a) '(b c d e))
;  ('(a b) '(c d e))
;  ('(a b c) '(d e))
;  ('(a b c d) '(e))
;  ('(a b c d e) '())
;  ('() ((lambda () '(a b c d e)))))


; quote trick
(run 1 (q) (fresh (x y) (evalo `(((,Y ,append-body) (quote ,x)) (quote ,y)) '(a b c d e)) (== (list x y) q)))
; =>
; ((() (a b c d e)))

(run* (q) (fresh (x y) (evalo `(((,Y ,append-body) (quote ,x)) (quote ,y)) '(a b c d e)) (== (list x y) q)))
; =>
; ((() (a b c d e))
;  ((a) (b c d e))
;  ((a b) (c d e))
;  ((a b c) (d e))
;  ((a b c d) (e))
;  ((a b c d e) ()))


;; impossible

; diverges
(run 1 (q) (evalo `(((,Y ,append-body) ,q) '(d e)) '()))

; quote trick
(run* (q) (evalo `(((,Y ,append-body) (quote ,q)) '(d e)) '()))
; =>
; ()


; diverges
(run 1 (q) (evalo `(((,Y ,append-body) '(a b c)) ,q) '()))

; quote trick
(run* (q) (evalo `(((,Y ,append-body) '(a b c)) (quote ,q)) '()))


; diverges
(run 1 (q) (evalo `(((,Y ,append-body) '(a b c)) ,q) '(d)))

; quote trick
(run 1 (q) (evalo `(((,Y ,append-body) '(a b c)) (quote ,q)) '(d)))


;; partially-instantiated lists

(run* (q) (evalo `(((,Y ,append-body) (quote (a b . ,q))) '(e)) '(a b c d e)))
; =>
; ((c d))

(run* (q) (fresh (x y) (evalo `(((,Y ,append-body) (quote (a b . ,x))) (quote ,y)) '(a b c d e)) (== (list x y) q)))
; =>
; ((() (c d e))
;  ((c) (d e))
;  ((c d) (e))
;  ((c d e) ()))

(run 2 (q) (fresh (x y z) (evalo `(((,Y ,append-body) (quote (a b . ,x))) (quote ,y)) `(a b . ,z)) (== (list x y) q)))
; =>
; (((() _.0) (absento (closure _.0)))
;  (((_.0) _.1) (absento (closure _.0) (closure _.1))))

; impossible
(run* (q) (fresh (x y z) (evalo `(((,Y ,append-body) (quote (a b . ,x))) (quote ,y)) `(a c . ,z)) (== (list x y) q)))
; =>
; ()



;; smart CPSed

(define Y
  '(lambda (fun)
     ((lambda (F)
        (F F))
      (lambda (F)
        (fun (lambda (x) ((F F) x)))))))

(define append-body-smart-cps
  '(lambda (append)
     (lambda (ls1)
       (lambda (ls2 k)
         (if (null? ls1)
             (k ls2)
             ((append (cdr ls1)) ls2
              (lambda (v)
                (k (cons (car ls1) v)))))))))

;; forward:
(run 1 (q) (evalo `(((,Y ,append-body-smart-cps) '()) '(d e) (lambda (v) v)) q))

(run 1 (q) (evalo `(((,Y ,append-body-smart-cps) '(a b c)) '(d e) (lambda (v) v)) q))

(run* (q) (evalo `(((,Y ,append-body-smart-cps) '(a b c)) '(d e) (lambda (v) v)) q))


;; impossible

;; seems to diverge
; quote trick
(run* (q) (evalo `(((,Y ,append-body-smart-cps) (quote ,q)) '(d e) (lambda (v) v)) '()))

; quote trick
(run* (q) (evalo `(((,Y ,append-body-smart-cps) '(a b c)) (quote ,q) (lambda (v) v)) '()))
; => ()

; quote trick
(run* (q) (evalo `(((,Y ,append-body-smart-cps) '(a b c)) (quote ,q) (lambda (v) v)) '(d)))
; => ()




;; fake CPSed

(define Y
  '(lambda (fun)
     ((lambda (F)
        (F F))
      (lambda (F)
        (fun (lambda (x) ((F F) x)))))))

(define append-body-fake-cps
  '(lambda (append-cps)
     (lambda (ls1)
       (lambda (ls2)
         (lambda (k)
           (if (null? ls1)
               (k ls2)
               (((append-cps
                  (cdr ls1))
                 ls2)
                (lambda (v)
                  (k (cons (car ls1) v))))))))))

;; forward:
(run 1 (q) (evalo `((((,Y ,append-body-fake-cps) '(a b c)) '(d e)) (lambda (v) v)) q))

(run* (q) (evalo `((((,Y ,append-body-fake-cps) '(a b c)) '(d e)) (lambda (v) v)) q))



;; impossible

; quote trick
; seems to diverge
(run* (q) (evalo `((((,Y ,append-body-fake-cps) (quote ,q)) '(d e)) (lambda (v) v)) '()))

; quote trick
(run* (q) (evalo `((((,Y ,append-body-fake-cps) '(a b c)) (quote ,q)) (lambda (v) v)) '()))
; => ()

; quote trick
(run* (q) (evalo `((((,Y ,append-body-fake-cps) '(a b c)) (quote ,q)) (lambda (v) v)) '(d)))
; => ()





;; ANF

(define Y
  '(lambda (fun)
     ((lambda (F)
        (F F))
      (lambda (F)
        (fun (lambda (x) ((F F) x)))))))

(define append-anf-body
  '(lambda (append-anf)
     (lambda (l)
       (lambda (s)
         (if (null? l)
             s
             ((lambda (v)
                ((lambda (w)
                   (cons v w))
                 ((append-anf (cdr l)) s)))
              (car l)))))))

;; forward:
(run 1 (q) (evalo `(((,Y ,append-anf-body) '(a b c)) '(d e)) q))

(run* (q) (evalo `(((,Y ,append-anf-body) '(a b c)) '(d e)) q))


;; impossible

; quote trick
; seems to diverge
(run* (q) (evalo `(((,Y ,append-anf-body) (quote ,q)) '(d e)) '()))

; quote trick
(run* (q) (evalo `(((,Y ,append-anf-body) '(a b c)) (quote ,q)) '()))
; => ()

; quote trick
(run* (q) (evalo `(((,Y ,append-anf-body) '(a b c)) (quote ,q)) '(d)))
; => ()



;; Try:

;; abstract interpreter

;; sets

;; append with partially-instantiated list arguments, containing logic variables
;; (works!! See tests above)

;; append with 'letrec' built into the interpreter, rather than using
;; Y combinator.  Does this change performance or divergence behavior?

;; Try changing 'lambda' and application to be Curried.  Does this speed up the interpreter?

;; CPSed append (compare with direct append in evalo, and with CPSed appendo)

;; Direct-style reverse (uses append and reverse) vs reverseo in mK.  How do divergence behaviors compare?
;; Probably want to add 'letrec' first.

;; fib

;; small-step LC reducer, with capture-avoiding substitution: combinator synthesis

;; CL reducer: combinator synthesis

;; CEK/CESK machine

;; muKanren interpreter

;; C311 interpreter

;; CPSer

;; ParentheC

;; macro expander

;; type inferencer


;; I guess this really is closely related to the test-based program synthesis for append (when augmenting the interpreter with the knowledge base).  Can I do example-based program synthesis of append?  Also, this might shed light on why the KB-based program synthesis is slow:  even running append forwards is quite slow


;; observation:
;;
;; relational interpreter supports higher-order functions, which lets us run the CPS version of a program like 'append', without having to make the code RI or defunctionalized.  For this reason, CESK machine should be easy to run.

;; observation:
;;
;; eigen and relatonal interpreter with 'quote': two great things that go together!
;;
;; (however, =/= and eigen  *don't* play well together, so may need to explicitly tag pairs (see append.scm in github/mk-synthesis))
;;
;; forall X . (append '() X) => X
;;
;; Problem: evalo will attempt to evaluate the expression X.  So
;;
;; (run 1 (q) (fresh (prog) (== `(,Y ,q) prog) (eigen (x) (evalo `((,prog '()) ,x) x))))
;;
;; diverges.
;;
;; Quote to the rescue!
;;
;; (time (run 1 (q) (fresh (prog) (== `(,Y ,q) prog) (eigen (x) (evalo `((,prog '()) (quote ,x)) x)))))
;; running stats for (run 1 (q) (fresh (prog) (== `(,Y ,q) prog) (eigen (x) (evalo `((,prog '()) ',x) x)))):
;;     1 collection
;;     46 ms elapsed cpu time, including 0 ms collecting
;;     46 ms elapsed real time, including 0 ms collecting
;;     2824528 bytes allocated
;; (((lambda (_.0) (lambda (_.1) (lambda (_.2) _.2)))
;;    (=/= ((_.0 lambda)) ((_.1 lambda)) ((_.2 closure)))
;;    (sym _.2) (absento (closure _.0) (closure _.1))))

;; Compare with the slow, awkward, and technically unsound approaches I had been using, such as:

;; > (time (run 1 (q) (fresh (prog) (== `(,Y ,q) prog) (evalo `(cons ((,prog '()) '(g)) ((,prog '()) '(h))) '((g) . (h))))))
;; running stats for (run 1 (q) (fresh (prog) (== `(,Y ,q) prog) (evalo `(cons ((,prog '()) '(g)) ((,prog '()) '(h))) '((g) h)))):
;;     1 collection
;;     430 ms elapsed cpu time, including 0 ms collecting
;;     431 ms elapsed real time, including 0 ms collecting
;;     11252416 bytes allocated
;; (((lambda (_.0) (lambda (_.1) (lambda (_.2) _.2)))
;;    (=/= ((_.0 lambda)) ((_.1 lambda)) ((_.2 closure)))
;;    (sym _.2) (absento (closure _.0) (closure _.1))))

;; or the slightly more clever

;; (time (run 1 (q) (fresh (prog) (== `(,Y ,q) prog) (evalo `((lambda (f) (cons (f '(g)) (f '(h)))) (,prog '())) '((g) . (h))))))
;; running stats for (run 1 (q) (fresh (prog) (== `(,Y ,q) prog) (evalo `((lambda (f) (cons (f '(g)) (f '(h)))) (,prog '())) '((g) h)))):
;;     no collections
;;     116 ms elapsed cpu time, including 0 ms collecting
;;     116 ms elapsed real time, including 0 ms collecting
;;     4418736 bytes allocated
;; (((lambda (_.0) (lambda (_.1) (lambda (_.2) _.2)))
;;    (=/= ((_.0 lambda)) ((_.1 lambda)) ((_.2 closure)))
;;    (sym _.2) (absento (closure _.0) (closure _.1))))

;; Or, I had to use a knowledge base, which was awkward, and still
;; required assuming the some eigen X evaluated to another eigen Y.



;; forall X . (append '() X) => X

;; forall A B C . exists D . (append (cons A B) C) => (A . D), where D = (append B C)
;; (not sure D is really existential, or at least, it should be an eigen, not a fresh)
;; only thing that would need to go into the knowledge base is the true inductive hypothesis: D = (append B C)

;; should clean up, and probably significantly speed up, the
;; KB-approach to program synthesis.  Just need to make sure the
;; interpreter supports quote.








#!eof






















exists F . forall X . FX = X(FX)

(fresh (F)
  (eigen (X)
    (evalo `(,F ,X) `(,X (,F ,X)))))

(fresh (F)
  (eigen (X)
    (fresh (v)
      (evalo `(,F ,X) v)
      (evalo `(,X (,F ,X)) v))))









(define append
  (lambda (l s)
    (if (null? l)
        s
        (cons (car l) (append (cdr l) s)))))

(define append-anf
  (lambda (l s)
    (if (null? l)
        s
        (let ((v (car l)))
          (let ((w (append-anf (cdr l) s)))
            (cons v w))))))

(define append-cps
  (lambda (l s k)
    (if (null? l)
        (k s)
        (append-cps (cdr l) s
                    (lambda (v)
                      (k (cons (car l) v)))))))









(define append-anf
  (lambda (l s)
    (cond
      [(null? l) s]
      [else (let ((a (car l)))
              (let ((res (let ((d (cdr l)))
                           (append-anf d s))))
                (cons a res)))])))

(define append-anfo
  (lambda (l s out)
    (conde
      [(== '() l) (== s out)]
      [(fresh (a)
         (caro l a)
         (fresh (res)
           (fresh (d)
             (cdro l d)
             (append-anfo d s res))
           (conso a res out)))])))

(define append-anfo
  (lambda (l s out)
    (conde
      [(== '() l) (== s out)]
      [(fresh (a)
         (caro l a)
         (fresh (res)
           (conso a res out)
           (fresh (d)
             (cdro l d)
             (append-anfo d s res))))])))











(define append-anf
  (lambda (l s)
    (if (null? l)
        s
        ((lambda (v)
           ((lambda (w)
              (cons v w))
            (append-anf (cdr l) s)))
         (car l)))))



;; Y-combinator friendly, ANF version of append

(define Y
  (lambda (fun)
    ((lambda (F)
       (F F))
     (lambda (F)
       (fun (lambda (x) ((F F) x)))))))

(define append-anf-body
  (lambda (append-anf)
    (lambda (l)
      (lambda (s)
        (if (null? l)
            s
            ((lambda (v)
               ((lambda (w)
                  (cons v w))
                ((append-anf (cdr l)) s)))
             (car l)))))))

(((Y append-anf-body) '(a b c)) '(d e))
=>
(a b c d e)
