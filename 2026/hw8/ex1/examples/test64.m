let val f = fn p =>
  (
    write !(p.1);
    (
      if (p.2 (p.1)) = (p.2 (p.1)) then
        (write (p.2 (p.1)); 1)
      else
        (write ((p.1) = (p.1)); 2),
      (p.2 (p.1))
    )
  )
in
  (
    (f (malloc true, fn i => (write !i; malloc true))).2,
    (f (malloc 3, fn i => (write !i; "test"))).1
  )
end
