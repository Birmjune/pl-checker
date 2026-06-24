(rec f x =>
    ifp (x + (-1)) then
      1
    else (
      (rec g y =>
        (ifp y.2 then
          ((y.1) + (y.2))
        else
          ((g ((y.1 + (-3)), y.2)) + 1))
      ) (f (x + 1), f (x + 3))
    )
  ) -4
