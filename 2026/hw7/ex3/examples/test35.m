let val xx = malloc (4, (malloc 3, 2)) in
  write ((!xx).1 +
      !((!xx).2.1 := (fn xx => (malloc (xx.1))) ((fn yyy => (yyy , xx)) 6) ))
end
