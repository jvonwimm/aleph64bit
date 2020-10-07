      INTEGER JRESIR
      REAL SMAXIR,SIGRIR,SIGZIR
      PARAMETER (JRESIR=8)
      COMMON/IRESOL/SMAXIR(JRESIR),SIGRIR(JRESIR),SIGZIR(JRESIR)
#if defined(DOC)
C ITC nominal resolutions.
C
C JRESIR = Length of arrays: no. of ITC layers.
C SMAXIR = maximum sigma r-phi for layer i in cm
C SIGRIR = nominal sigma r-phi for layer i in cm.
C SIGZIR = nominal sigma z     for layer i in cm.
#endif
