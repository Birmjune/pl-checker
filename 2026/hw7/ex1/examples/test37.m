ifp ((fn p => p.1) (1,2))
then ((fn p => p.1) ((fn p => p.1) ((fn p => p.1) (((3,4).1,5),6),7) ))
else ((fn p => p.1) (8,9))
