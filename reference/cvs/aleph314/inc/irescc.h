      INTEGER JCOFIR,JLAYRE
      REAL RESCOF
      PARAMETER (JCOFIR=3,JLAYRE=8)
      COMMON/IRESCC/RESCOF(JCOFIR,JLAYRE)
#if defined(DOC)
C ITC R-Phi resolution parametrisation.
C
C JCOFIR = Dimension: Number of coefficients per layer.
C JLAYRE = Dimension: Number of Layers.
C
C RESCOF(i,j) = Coefficients (i=1,2,3) for layer j.
#endif
