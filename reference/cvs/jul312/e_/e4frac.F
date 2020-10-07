      FUNCTION FRACST(K,I)
C----------------------------------------------------------------------
C!  - R2 ESTIMATOR GIVE STACK NUMBER K IN REEL RELATIVE TO FIRST OR LAST
C!
C!    AUTHOR  : D. PALLIN
C!    MODIFIED:
C!
C?
C!======================================================================
#ifndef DOC
      DIMENSION PK(3)
C  I=1 SORTIE
C  I=2 ENTREE
      PARAMETER (EPSILO=0.001)
C PK(I) : NUMBER OF PLAN /STACK IN ECAL
      DATA (PK(L),L=1,3)/10.,23.,12./
      IF(I.EQ.1)THEN
               FRACST=(PK(K)-1.)/PK(K)
               ELSE
               FRACST=EPSILO
               ENDIF
      RETURN
      END
#endif
