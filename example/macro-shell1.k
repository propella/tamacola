;; pattern macro

(define-pattern ((add a b) (+ a b)))

(define-pattern ((after a b) b))

;(meta-define-pattern '(((add a b) (+ a b))))
;(meta-define-pattern '(((after a b) b)))

;; traditional macro

(define-macro cat (lambda (arg) `(+ ,(car arg) ,(cadr arg))))

;(meta-define-macro 'cat '(lambda (arg) `(+ ,(car arg) ,(cadr arg))))

;(set-macros-at 'cat (lambda (arg) `(+ ,(car arg) ,(cadr arg))))

(define-macro define-form
  (lambda (exp)
    (let ((name (car exp))
          (args (cadr exp))
          (body (caddr exp)))
      `(define-macro ,name
         (lambda (**exp**)
           (let ,(match-assign args '**exp**) ,body))))))

;; (set-macros-at 'define-form
;;   (lambda (exp)
;;     (let ((name (car exp))
;;           (args (cadr exp))
;;           (body (caddr exp)))
;;       `(define-macro ,name
;;          (lambda (**exp**)
;;            (let ,(match-assign args '**exp**) ,body))))))

;; (meta-define-macro 'define-form
;;   '(lambda (exp)
;;     (let ((name (car exp))
;;           (args (cadr exp))
;;           (body (caddr exp)))
;;       `(define-macro ,name
;;          (lambda (**exp**)
;;            (let ,(match-assign args '**exp**) ,body))))))

