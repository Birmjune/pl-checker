let val g = fn x => ((0 - 1) + x)
    rec f = fn x => (if (x = x) then (g x) else (f (g x)))
in
  (f 6)
end
