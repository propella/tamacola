(define ruby-translator (make-translator2 ruby/program ruby-trans/start))
(add-translator 'Ruby ruby-translator)


