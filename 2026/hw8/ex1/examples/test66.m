let val f =
  let val id = fn x => x
      rec foo = fn x => malloc (id x)
  in
    foo
  end
in
  (
    write ((!(f (fn x => (write x; x)))) true),
    ((!(f (fn x => (x = x, x)))) "PL")
  )
end
