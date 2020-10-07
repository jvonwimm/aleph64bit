      FUNCTION CORAOLD(EN,TH,NRUN)
C ---------------------------------------------------------------------
C!  Old corrections to photon energy
C - Author: M.N Minard          940121
C   Called from FIXGAEN
C - Input : EN      : Energy of cluster from 4 towers
C-          TH      : Cluster theta angle (rad)
C-          NRUN    : Run number
C - Output: CORAOLD : corrected energy
C ---------------------------------------------------------------------
#ifndef DOC
      DIMENSION EGCC (6,4,2)
      DIMENSION EGCOD (6,4), EGCOM(6,4)
      EQUIVALENCE(EGCC(1,1,1),EGCOD(1,1))
      EQUIVALENCE(EGCC(1,1,2),EGCOM(1,1))
      DIMENSION X(3)
      REAL*8 S0,S1,S2,X,D
      DATA EGCOD / 0.,0.72,0.035,0.99,1.5,1.2
     &           ,0.72,0.8 ,0.02,1.  ,1.5,1.2
     &           ,0.8 ,0.9 ,0.01,0.99,1.5,1.2
     &           ,0.9 ,1.  ,0.006,1. ,1.5,1.2/
      DATA EGCOM /  0.,0.72,0.04,0.98,1.5,1.2
     &           ,0.72,0.8 ,0.  ,0.98,1.5,1.2
     &           ,0.8 ,0.9 ,0.04,0.94,1.5,1.2
     &           ,0.9 ,1.  ,0.02,0.96,1.5,1.2/
C
C ---------------------------------------------------------------------
       CORAOLD = 1.
C
C-     Look for correction from EGCO bank
C
       COSI = ABS(COS(TH))
       COSI = MIN (COSI,0.999999)
       IROW = 0
       DO IEGCO = 1,4
        IF (COSI.GE.EGCOD(1,IEGCO).AND.
     &      COSI.LT.EGCOD(2,IEGCO)) IROW=IEGCO
       ENDDO
       IARR = 1
       IF (NRUN.LT.2000) THEN
         IARR =2
       ENDIF
       C1 = EGCC(3,IROW,IARR)
       C2 = EGCC(4,IROW,IARR)
       C3 = EGCC(5,IROW,IARR)
       C4 = EGCC(6,IROW,IARR)
       A = EN
       INTER = 0
       ERAW = 0.
       DENOM = 1.-(C1*A/C4)
       IF (ABS(DENOM).GT.0) THEN
         ERAW =(C2*A-(C3*C1/C4)*A)/(1.-(C1*A/C4))
       ELSE
         INTER = 1
       ENDIF
       IF ( ERAW .GE. C3.OR.INTER.GT.0.OR.ERAW.LT.0)THEN
        S2 = 0.
        S1 = -A*C2
        S0 = -A*C1
        CALL DRTEQ3(S2,S1,S0,X,D)
        IMAX = 99
        FRAC = 999.
        DO IS = 1, 3
          IF( X(IS).GT.0) THEN
            X2 = X(IS)**2
            IF ( X2.GE.C3) THEN
             FR = ABS(1.-(X2/A))
             IF ( FR.LT.FRAC) THEN
             IMAX = IS
             FRAC = FR
             ENDIF
            ENDIF
          ENDIF
        ENDDO
         ERAW = A
         IF ( IMAX.LT.4) ERAW = X(IMAX)**2
        ENDIF
        CORAOLD = ERAW/EN
C
       END
#endif
