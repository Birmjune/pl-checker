let rec sum = fn x =>
 if (x = 0) then
   0
 else
   (x + sum (x - 1))
in
  !(sum)
end
