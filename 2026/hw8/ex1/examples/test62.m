let val compare = fn p =>
  if p.1 = p.2 then
    (write (p.1 = p.2))
  else
    (write (false))
in
  let val i = 0 - 1
      val s = "hi word"
      val b = true
  in
   compare (i, 1);
   compare (b, false);
   compare (s, "hello world")
  end
end
