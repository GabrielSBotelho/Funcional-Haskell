collatz n = iterate (seguinte) n

seguinte 1 = 1
seguinte n = if mod n 2 == 0 then (n`div`2) else ((n*3)+1)
