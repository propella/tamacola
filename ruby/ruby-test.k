(define ruby-test
  (lambda (sym str result)
    (trace str)
    (check (parse-collection sym str) => result)))

(ruby-test rexpr "a123" '(ident a123))
(ruby-test rnumeric "123_456" '(numeric 123456))
(ruby-test rnumeric "123456" '(numeric 123456))

(ruby-test rarg "a = 3" '(assign (ident a) (numeric 3)))
(ruby-test rargdecl "(n)" '(args (ident n)))
(ruby-test rargdecl "(n, m)" '(args (ident n) (ident m)))

(ruby-test rcall-args "n, m" '((ident n) (ident m)))
(ruby-test rcall-args "n" '((ident n)))
(ruby-test rcomp-stmt "n; m; l; o" '(compStmt (ident n) (ident m) (ident l) (ident o)))

(ruby-test rcomp-stmt "return 3" '(compStmt (return (numeric 3))))

(ruby-test rIF "if" 102)
(ruby-test ridentifier "if" 'FAIL)
(ruby-test rexpr " n < 2" '(< (ident n) (numeric 2)))
(ruby-test rTHEN " then" 110)
(ruby-test rcomp-stmt " 3" '(compStmt (numeric 3)))
(ruby-test rEND " end" 100)

(ruby-test roperation "abc " '(ident abc))

;(ruby-test rexprThen "n < 2 then" '())

(ruby-test rstmt "if a < 2 then 1 end" '(cond ((< (ident a) (numeric 2)) (compStmt (numeric 1)))))

(ruby-test rstmt "if a < 2
1
end" '(cond ((< (ident a) (numeric 2)) (compStmt (numeric 1)))))

(ruby-test rPrimExpr "def nfib(n) return 3 end" '(def (ident nfib) (args (ident n)) (compStmt (return (numeric 3)))))

(ruby-test rstmt "def nfib(n) if a < 2 then 1 end end" '(def (ident nfib) (args (ident n)) (compStmt (cond ((< (ident a) (numeric 2)) (compStmt (numeric 1)))))))

(ruby-test rstmt "def nfib(n)
if n < 2 then 1 else nfib(n-2) + nfib(n - 1) + 1 end end" '(def (ident nfib) (args (ident n)) (compStmt (cond ((< (ident n) (numeric 2)) (compStmt (numeric 1))) (#t (compStmt (+ (+ (call (ident nfib) () (- (ident n) (numeric 2))) (call (ident nfib) () (- (ident n) (numeric 1)))) (numeric 1))))))))

(ruby-test rstmt "for i in 1...2
 i*3 end" '(for ((ident i)) (range-i (numeric 1) (numeric 2)) (compStmt (* (ident i) (numeric 3)))))

(ruby-test rDO "
 " 10)

(ruby-test rcomp-stmt "
i*3" '(compStmt (* (ident i) (numeric 3))))

(ruby-test rargs "size + 1, true" '((+ (ident size) (numeric 1)) (ident true)))

(ruby-test rstmt "flags = Array.new(size + 1, true)" '(assign (ident flags) (send (ident new) (ident Array) () (+ (ident size) (numeric 1)) (ident true))))
(ruby-test rstmt "count = 0" '(assign (ident count) (numeric 0)))
(ruby-test rstmt "flags[i] = true" '(assign (index-lhs (ident flags) (ident i)) (ident true)))
(ruby-test rstmt "count = flags[i]" '(assign (ident count) (index (ident flags) (ident i))))
(ruby-test rstmt "while k <= 1
  flags[k] = true
end" '(while (<= (ident k) (numeric 1)) (compStmt (assign (index-lhs (ident flags) (ident k)) (ident true)))))

(ruby-test rstmt "
      if (flags[i])
        count = count + 1
      end
" '(cond ((index (ident flags) (ident i)) (compStmt (assign (ident count) (+ (ident count) (numeric 1)))))))
