require "thread"

def compute_prime(n)
    n + 1
end

t1 = Thread.new { compute_prime(1000) }
t2 = Thread.new { compute_prime(2000) }

r1 = t1.join
r2 = t2.join

puts r1
puts r2


