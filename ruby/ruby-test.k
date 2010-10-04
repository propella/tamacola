(define ruby-test
  (lambda (sym str result)
    (trace str)
    (check (parse-collection sym str) => result)))

(ruby-test ruby/expr "a123" '(ident a123))
(ruby-test ruby/numeric "123_456" '(numeric 123456))
(ruby-test ruby/numeric "123456" '(numeric 123456))

(ruby-test ruby/arg "a = 3" '(assign (ident a) (numeric 3)))
(ruby-test ruby/argdecl "(n)" '(args (ident n)))
(ruby-test ruby/argdecl "(n, m)" '(args (ident n) (ident m)))

(ruby-test ruby/call-args "n, m" '((ident n) (ident m)))
(ruby-test ruby/call-args "n" '((ident n)))
(ruby-test ruby/comp-stmt "n; m; l; o" '(compStmt (ident n) (ident m) (ident l) (ident o)))

(ruby-test ruby/comp-stmt "return 3" '(compStmt (return (numeric 3))))

(ruby-test ruby/IF "if" 102)
(ruby-test ruby/identifier "if" 'FAIL)
(ruby-test ruby/expr " n < 2" '(< (ident n) (numeric 2)))
(ruby-test ruby/THEN " then" 110)
(ruby-test ruby/comp-stmt " 3" '(compStmt (numeric 3)))
(ruby-test ruby/END " end" 100)

(ruby-test ruby/operation "abc " '(ident abc))

(ruby-test ruby/stmt "if a < 2 then 1 end" '(cond ((< (ident a) (numeric 2)) (compStmt (numeric 1)))))

(ruby-test ruby/stmt "if a < 2
1
end" '(cond ((< (ident a) (numeric 2)) (compStmt (numeric 1)))))

(ruby-test ruby/primExpr "def nfib(n) return 3 end" '(def (ident nfib) (args (ident n)) (compStmt (return (numeric 3)))))

(ruby-test ruby/stmt "def nfib(n) if a < 2 then 1 end end" '(def (ident nfib) (args (ident n)) (compStmt (cond ((< (ident a) (numeric 2)) (compStmt (numeric 1)))))))

(ruby-test ruby/stmt "def nfib(n)
if n < 2 then 1 else nfib(n-2) + nfib(n - 1) + 1 end end" '(def (ident nfib) (args (ident n)) (compStmt (cond ((< (ident n) (numeric 2)) (compStmt (numeric 1))) (#t (compStmt (+ (+ (call (ident nfib) () (- (ident n) (numeric 2))) (call (ident nfib) () (- (ident n) (numeric 1)))) (numeric 1))))))))

(ruby-test ruby/stmt "for i in 1...2
 i*3 end" '(for ((ident i)) (range-i (numeric 1) (numeric 2)) (compStmt (* (ident i) (numeric 3)))))

(ruby-test ruby/DO "
 " 10)

(ruby-test ruby/comp-stmt "
i*3" '(compStmt (* (ident i) (numeric 3))))

(ruby-test ruby/args "size + 1, true" '((+ (ident size) (numeric 1)) (ident true)))

(ruby-test ruby/stmt "flags = Array.new(size + 1, true)" '(assign (ident flags) (send (ident new) (ident Array) () (+ (ident size) (numeric 1)) (ident true))))
(ruby-test ruby/stmt "count = 0" '(assign (ident count) (numeric 0)))
(ruby-test ruby/stmt "flags[i] = true" '(assign (index-lhs (ident flags) (ident i)) (ident true)))
(ruby-test ruby/stmt "count = flags[i]" '(assign (ident count) (index (ident flags) (ident i))))
(ruby-test ruby/stmt "while k <= 1
  flags[k] = true
end" '(while (<= (ident k) (numeric 1)) (compStmt (assign (index-lhs (ident flags) (ident k)) (ident true)))))

(ruby-test ruby/stmt "
      if (flags[i])
        count = count + 1
      end
" '(cond ((index (ident flags) (ident i)) (compStmt (assign (ident count) (+ (ident count) (numeric 1)))))))
