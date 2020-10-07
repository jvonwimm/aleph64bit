      PARAMETER (NILIIT=7,NOLIIT=8,NGAPIT=25,NPRGIT=54)
      COMMON/ITGAPC/IGAPIT(NILIIT,NOLIIT),IIGPIT(NGAPIT),IOGPIT(NGAPIT),
     +    IPR1IT(NPRGIT),IPR2IT(NPRGIT),IPR3IT(NPRGIT),NUMISS(NGAPIT)
#if defined(DOC)
C! ITC Gap common used in ITC tracking
C
C NILIIT             : No. of inner layers for gaps
C NOLIIT             : No. of outer layers for gaps
C NGAPIT             : No. of gaps (types)
C NPRGIT             : No. of pairs of gaps (types)
C
C IGAPIT(i,j)        : Gap type for inner layer i and outer layer j
C IIGPIT(k)          : Inner layer no. of gap type k
C IOGPIT(k)          : Outer layer no. of gap type k
C IPR1IT(m)          : Inner  layer no. of gap pair m
C IPR2IT(m)          : Middle layer no. of gap pair m
C IPR3IT(m)          : Outer  layer no. of gap pair m
C NUMISS(igap)       : Number of layers jumped over by a link of gap
C                                                       type IGAP
#endif
