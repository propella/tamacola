(define peg-translator (make-translator peg/definitions))
(add-translator 'PEG peg-translator)
