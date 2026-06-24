let val p1 = (fn x => x = x)
    val p2 = (fn x => (x and x))
    val f1 = malloc p1 in
  f1 := p2 ;
  (!f1) 2
end
