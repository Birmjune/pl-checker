callcc (fn ret => (fn x => (x + (ret 42))) -1)
