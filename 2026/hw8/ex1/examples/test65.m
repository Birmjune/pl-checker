let val id = fn x => x
    rec f = fn x =>
      write "Entering loop";
      write !x;
      x := id (!x);
      f x
in
  if (id true) then
    1 + (f (malloc 10))
  else
    !(f (malloc true))
end
