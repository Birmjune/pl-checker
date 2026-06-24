callcc (fn ret =>
(rec f x => ifp (x.1) then (x.2.1+(f (x.2.2))) else (ret (x.2)))
(1,(10,(2,(20,(3,(30,(0,42)))))))
)
