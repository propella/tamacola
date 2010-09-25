;;;; Forall translator test

(define str "
N = 10;
arr = new[N, N];
arr[i:0...N, j:0...N] = Math.floor(Math.random() + 0.5); FArrayFlip(arr);
arr[i:0...N, j:0...N] = Math.floor(Math.random() + 0.5);
nextStep = function (a, n) {
  return ((n == 2) || (a == 1 && (n == 2 || n == 3))) ? 1 : 0;
};

step = function () {
  arr[i:1..N, j:1..N] = arr[i-1, j-1] + arr[i-1, j]              + arr[i-1, j+1] + 
                        arr[i, j-1]   + (arr[i, j] > 0 ? 10 : 0) + arr[i, j+1] + 
                        arr[i+1, j-1] + arr[i+1, j]              + arr[i+1, j+1];
    FArrayFlip(arr);
  %all{a[i, j] < arr} do {a = nextStep(a // 10, a % 10);};
    FArrayFlip(arr);
  trace(FArrayToString(arr));
};
")

(let ((tree (parse-collection forall/start str)))
  (pretty-print tree)
  (pretty-print (parse-stream forall-trans/starts (->stream (cons tree '()))))
  (map eval (parse-stream forall-trans/starts (->stream (cons tree '()))))
)

(trace (FArrayToString arr))
(do ((i 0 (+ i 1)))
    ((> i 3))
  (step))