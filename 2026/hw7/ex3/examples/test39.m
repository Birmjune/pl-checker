let
  rec f = fn x => 3
  rec f = fn x =>
   (if (x = 0) then
    0
   else
    (x + (f (x - 1))))
  val foo = fn f => f (9)
in
  false;write (foo f)
end
