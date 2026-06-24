callcc (fn ret => (rec f x => ifp x then x + f (x + (-2)) else (ret 42)) 11)
