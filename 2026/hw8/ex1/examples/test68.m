let val x = malloc (fn x => (write (x = x); (malloc x, x)))
    val y = x in
  y := (fn x => (write x; (true, x)));
  !y (malloc 10)
end
