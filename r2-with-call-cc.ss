#lang racket


;; ----- data structures -----
(struct Closure (fun env))
(struct Continuation (k))

;; ----- main code -----
(define r2
  (lambda (exp)
    (interp exp env0 (lambda (x) x))))

(define interp
  (lambda (exp env k)
    (match exp
      [(? symbol? x)
       (let ([v (lookup x env)])
         (cond
          [(not v)
           (error "undefined variable" x)]
          [else (k v)]))]
      [(? number? x) (k x)]
      [`(lambda (,x) ,e)
       (k (Closure exp env))]
      [`(let ([,x ,e1]) ,e2)
       (interp e1 env
	       (lambda (v1)
		 (interp e2 (ext-env x v1 env) k)))]
      [`(call/cc ,e1) (interp e1 env
			      (lambda (v1)
				(match v1
				       [(Closure `(lambda (,x) ,exp1) env-save)
					(interp exp1
						(ext-env x (Continuation k) env-save)
						k)]
				       [(Continuation k-save)
					(k-save (Continuation k))])))]
      [`(,e1 ,e2)
       (interp e1 env
	       (lambda (v1)
		 (interp e2 env
			 (lambda (v2)
			   (match v1
				  [(Closure `(lambda (,x) ,e) env-save)
				   (interp e (ext-env x v2 env-save) k)]
				  [(Continuation k-save)
				   (k-save v2)])))))]
      [`(,op ,e1 ,e2)
       (interp e1 env
	       (lambda (v1)
		 (interp e2 env
			 (lambda (v2)
			   (match op
				  ['+ (k (+ v1 v2))]
				  ['- (k (- v1 v2))]
				  ['* (k (* v1 v2))]
				  ['/ (k (/ v1 v2))])))))])))


;; ----- environment -----
(define env0 '())

(define ext-env
  (lambda (x v env)
    (cons `(,x . ,v) env)))

(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond
       [(not p) #f]
       [else (cdr p)]))))


;; ----- examples -----
(r2 '(+ 1 2))
;; => 3

(r2 '(* 2 3))
;; => 6

(r2 '(* 2 (+ 3 4)))
;; => 14

(r2 '(* (+ 1 2) (+ 3 4)))
;; => 21

(r2 '((lambda (x) (* 2 x)) 3))
;; => 6

(r2
'(let ([x 2])
   (let ([f (lambda (y) (* x y))])
     (f 3))))
;; => 6

(r2
'(let ([x 2])
   (let ([f (lambda (y) (* x y))])
     (let ([x 4])
       (f 3)))))
;; => 6
(r2
 '(call/cc (lambda (k) (* 5 (k 4)))))
;; =>4
(r2
 '(let ([x (call/cc (lambda (k) k))])
    (x (lambda (ignore) 10))))
;; =>10
