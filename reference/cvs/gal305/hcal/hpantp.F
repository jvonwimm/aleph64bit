*DK hpantp
      REAL FUNCTION HPANTP(ITYP)
C---------------------------------------------------------
C                                 G.Catanesi 1/9/86
C!   hpantp = +1. hadronic antiparticle
C!     "     = -1. hadronic particle
C!     "     =  0. otherwise
C -------------------------------------------
#ifndef DOC
C
      HPANTP = 0.
C
      IF( ITYP.EQ. 13.OR. ITYP.EQ. 14.OR. (ITYP.GT. 17.AND. ITYP.LT.25)
     &) THEN
         HPANTP = -1.
      ELSE
         IF(ITYP.EQ. 15.OR. (ITYP.GT. 24.AND. ITYP.LT.33))HPANTP = +1.

      ENDIF
C
      IF(ITYP.GE.45.AND.ITYP.LE.47)HPANTP=-1.
C
      RETURN
      END
#endif
