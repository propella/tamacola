# Benchmark script ported from Squeak Integer >> benchmark

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

print sieve(3000)
