      PARAMETER (NLAYIR=8,NLMXIR=10)
      COMMON/IROADC/ NHITIR(NLAYIR),IHITIR(NLMXIR,NLAYIR)
#if defined(DOC)
C! Road common used in ITC tracking
C
C NLAYIR      : No. of ITC layers
C NLMXIR      : Max. no. of hits on one layer (in road)
C
C NHITIR(i)   : Number of hits on layer i
C IHITIR(j,i) : Coord number (in ITCO) of hit j on layer i
C               (-ve if ambiguity to be used). j = 1,NHITIR(i)
#endif
