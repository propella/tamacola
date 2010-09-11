;;;; Forall grammar test

(define forall-test
  (lambda (sym str result)
    (trace str)
    (check (parse-collection sym str) => result)))

(forall-test lidentifier "a123" '(ident a123))
(forall-test lscalar "123" '(scalar 123))
(forall-test lscalar "0.5" '(scalar 0.5))
(forall-test lALL "%all" 'all)
(forall-test lSOME "%some" 'some)
(forall-test larrayPart "5..10" '(range (scalar 5) (scalar 10) (scalar 1)))
(forall-test larrayPart "5..10 by 2" '(range (scalar 5) (scalar 10) (scalar 2)))
(forall-test larrayPart "a" '(ident a))
(forall-test lquery "%all{ x[i, j], y[k, l] < V }" '(all (ident V) ((array-decl (ident x) (ident i) (ident j)) (array-decl (ident y) (ident k) (ident l)))))
(forall-test lquery "%some{ x > 0 | x < V }" '(some (ident V) (> (ident x) (scalar 0)) ((ident x))))
(forall-test lprimExpr "[a, 1, 2]" '(array (ident a) (scalar 1) (scalar 2)))
(forall-test lprimExpr "([a, 1, 2])" '(array (ident a) (scalar 1) (scalar 2)))
(forall-test lexpr1 "a.rotate(3)" '(send (ident a) (ident rotate) (scalar 3)))
(forall-test lexpr1 "max(3, 4)" '(send null (ident max)  (scalar 3) (scalar 4)))
(forall-test lexpr1 "a.x" '(prop (ident a) (ident x)))
(forall-test lexpr "false != 0 >= 1 + 2 * 3" '(not (= (ident false) (>= (scalar 0) (+ (scalar 1) (* (scalar 2) (scalar 3)))))))
(forall-test lexpr "1 / 2 - 3" '(- (/ (scalar 1) (scalar 2)) (scalar 3)))
(forall-test lexpr "1 / 2 // 3 + 4 %% 5 * 6" '(+ (int (/ (/ (scalar 1) (scalar 2)) (scalar 3))) (* (int (modulo (scalar 4) (scalar 5))) (scalar 6))))
(forall-test lexpr "1 == 2 && true || false" '(or (and (= (scalar 1) (scalar 2)) (ident true)) (ident false)))
(forall-test lstmt "if 1 == 2 then a = 2 else a = 3" '(if (= (scalar 1) (scalar 2)) (assign (ident a) (scalar 2)) (assign (ident a) (scalar 3))))

(forall-test lstmt "{a = 3;}" '(compoundStmt (assign (ident a) (scalar 3))))
(forall-test lstmts "a = 3; b = 2;" '((assign (ident a) (scalar 3)) (assign (ident b) (scalar 2))))
(forall-test lstmt "if 1 == 2 then {a = 2;} else {a = 3; b = 2;}" '(if (= (scalar 1) (scalar 2)) (compoundStmt (assign (ident a) (scalar 2))) (compoundStmt (assign (ident a) (scalar 3)) (assign (ident b) (scalar 2)))))

(forall-test lstmt "a[1..2] = 10" '(assign (index (ident a) (range (scalar 1) (scalar 2) (scalar 1))) (scalar 10)))

(forall-test lstmt "a = new[2, 3, 4]" '(assign (ident a) (new ((scalar 2) (scalar 3) (scalar 4)))))
(forall-test lstmt "a[i:1..2] = b[i-1]" '(assign (index-v (ident a) ((ident i) (range (scalar 1) (scalar 2) (scalar 1)))) (index-e (ident b) ((- (ident i) (scalar 1))))))
(forall-test lstmt "a[i:1..2, j:2..3] = i*j" '(assign (index-v (ident a) ((ident i) (range (scalar 1) (scalar 2) (scalar 1))) ((ident j) (range (scalar 2) (scalar 3) (scalar 1)))) (* (ident i) (ident j))))
(forall-test lstmt "%all { x[i, j] < V} do {x = a;}" '(query do (all (ident V) ((array-decl (ident x) (ident i) (ident j)))) (compoundStmt (assign (ident x) (ident a)))))
;(forall-test lstmt "%such { x > 0 | x < V} do {x = a;}" '(query do (such (ident V) (> (ident x) (scalar 0)) ((ident x))) (compoundStmt (assign (ident x) (ident a)))))

(forall-test lexpr "a[1] + 0 + a[2] + 1
 + a[3]" '(+ (+ (+ (+ (index-e (ident a) ((scalar 1))) (scalar 0)) (index-e (ident a) ((scalar 2)))) (scalar 1)) (index-e (ident a) ((scalar 3)))))

(forall-test lstmt "%such{a == 1 | a[i, j] < array} do {g.drawRect(i, j, 1, 1);}" '(query do (such (ident array) (= (ident a) (scalar 1))
		(array-decl (ident a) (ident i) (ident j)))
       (compoundStmt (send (ident g) (ident drawRect) (ident i) (ident j) (scalar 1) (scalar 1)))))

;(forall-test lexpr "a == 3 ? 1 : (a == 2 ? 4 : 5)" '(if (= (ident a) (scalar 3)) (scalar 1) (if (= (ident a) (scalar 2)) (scalar 4) (scalar 5))))
(forall-test lexpr "a == 3 ? 1 : a == 2 ? 4 : 5" '(if (= (ident a) (scalar 3)) (scalar 1) (if (= (ident a) (scalar 2)) (scalar 4) (scalar 5))))
(forall-test lstmt "step = function (a, b) {x = 3; y = 2;}" '(assign (ident step) (lambda ((ident a) (ident b)) (compoundStmt (assign (ident x) (scalar 3)) (assign (ident y) (scalar 2))))))

(forall-test lexpr "step()" '(send null (ident step)))
(forall-test lstring "\"123\"" '(string "123"))
(forall-test lstmt "a123" '(ident a123))
(forall-test lexpr "data.get().put()" '(send (send (ident data) (ident get)) (ident put)))
(forall-test lexpr "stage().graphics.beginFill(16711935)" '(send (prop (send null (ident stage)) (ident graphics)) (ident beginFill) (scalar 16711935)))

(forall-test lstmt "((n == 2) || (a == 1 && (n == 2 || n == 3))) ? 1 : 0" '(if (or (= (ident n) (scalar 2)) (and (= (ident a) (scalar 1)) (or (= (ident n) (scalar 2)) (= (ident n) (scalar 3))))) (scalar 1) (scalar 0)))

(forall-test lstmt "a = nextStep((a % 10), (a // 10))" '(assign (ident a) (send null (ident nextStep) (modulo (ident a) (scalar 10)) (int (/ (ident a) (scalar 10))))))
