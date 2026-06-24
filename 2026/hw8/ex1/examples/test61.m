let val g = fn x => x = false
    rec f = fn x => (if (x = x) then (g x) else (f (x - 1)))
in
  (f 6)
end
