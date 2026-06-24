let val x = malloc (3, false) in
  if (x.2 or (x.1 = 4)) then
    write (x.1 + x.2)
  else
    write x.1
end
