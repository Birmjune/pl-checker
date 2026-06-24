callcc (fn ret => (fn z => (fn y => (fn x => (ret x) + y + z))) 4 2 42)
