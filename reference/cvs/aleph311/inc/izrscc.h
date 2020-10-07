      INTEGER JCOFIZ,JLYRIZ
      REAL ZRTRIZ,ZRESIZ
      PARAMETER (JCOFIZ=2,JLYRIZ=8)
      COMMON/IZRSCC/ZRTRIZ(JLYRIZ),ZRESIZ(JCOFIZ,JLYRIZ)
#if defined(DOC)
C ITC Z coord. resolution parameters.
C
C JCOFIZ = Dimension: Number of coefficients per layer.
C JLYRIZ = Dimension: Number of Layers.
C
C ZRTRIZ(j)   = Z res. for use in tracking (finding & fitting)
C ZRESIZ(i,j) = Z res. Coefficients (i=1,2) for layer j.
#endif
