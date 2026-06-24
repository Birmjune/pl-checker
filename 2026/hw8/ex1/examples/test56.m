let val list = fn x => fn y => fn z => fn w =>((x, y), (z, w))
    val fst = fn x => x.1.1
    val snd = fn x => x.1.2
    val thd = fn x => x.2.1
    val fth = fn x => x.2.2
    rec fold4 = fn f => fn lst => fn init_v =>
      let val v1 = f (fst lst) init_v
          val v2 = f (snd lst) v1
          val v3 = f (thd lst) v2
          val v4 = f (fth lst) v3
      in
        v4
      end
in
  (
    fold4 (fn v => fn acc_v => v + acc_v) (list 1 2 3 4) 10,
    fold4
      (fn l => fn acc_p => (l := "done"; if !l = "abc" then (acc_p.1 + 1, true) else acc_p))
      (list (malloc "aaa") (malloc "abc") (malloc "def") (malloc "true"))
      (0, false)
  )
end
