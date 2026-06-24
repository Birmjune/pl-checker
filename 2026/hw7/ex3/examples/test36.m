let
  val x = (malloc (true, read), 6)
in
  let
    val y = (false, 1)
  in
    write ((!(x.1)).2);
    x.1 := (true, read);
    write ((!(x.1)).2);
    (if (read = 0) then
      x.1 := (true, read)
    else
      x.1 := y);
    write ((!(x.1)).2)
  end
end
