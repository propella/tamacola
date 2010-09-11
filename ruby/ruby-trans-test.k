(define str "
def nfib(n)
  if n < 2 then
     1
   else
     nfib(n-2) + nfib(n - 1) + 1
   end
end

print(nfib(36))
")

(define str "
def sieve(n) 
  size = 8190
  flags = Array.new(size + 1, true)
  count = 0
  for iter in 1...n
    count = 0
    for i in 1...size
      if (flags[i])
        prime = i + 1
        k = i + prime
        while (k <= size)
          flags[k] = false
          k = k + prime
        end
        count = count + 1
      end
    end
  end
  return count
end

print(sieve(3000))
")

(let ((tree (parse-collection rprogram str)))
  (pretty-print tree)
  (let ((prog (parse-stream rtstart (->stream (cons tree '())))))
    (pretty-print prog)
    (map eval prog)
))
