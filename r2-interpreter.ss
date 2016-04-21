#lang racket


;; ----- data structures -----
(struct Closure (fun env))


;; ----- main code -----
(define r2
  (lambda (exp)
    (interp exp env0)))

(define interp
  (lambda (exp env)
    (match exp
      [(? symbol? x)
       (let ([v (lookup x env)])
         (cond
          [(not v)
           (error "undefined variable" x)]
          [else v]))]
      [(? number? x) x]
      [`(lambda (,x) ,body)
       (Closure exp env)]
      [`(let ([,x ,e1]) ,body)
       (let ([v1 (interp e1 env)])
         (interp body (ext-env x v1 env)))]
      [`(,fun ,arg)
       (let ([fun-value (interp fun env)]
             [arg-value (interp arg env)])
         (match fun-value
           [(Closure `(lambda (,x) ,body) env-save)
            (interp body (ext-env x arg-value env-save))]))]
      [`(,op ,e1 ,e2)
       (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))])))


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
