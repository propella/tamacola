(define forall-translator (make-translator2 forall/start forall-trans/starts))
(add-translator 'Forall forall-translator)

