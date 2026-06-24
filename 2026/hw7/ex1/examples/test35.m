(fn f => f (f (-6)))
(rec sum x => (ifp x then (sum (x + 1) + x) else
    (ifp (x + 2) then (0) else (sum (x + 2) + x))
  ))
