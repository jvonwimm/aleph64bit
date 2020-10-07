      SUBROUTINE ALTHRU(ITYP,THR,OBL)
C==================================================================
C!    ALEPHLIB copy of the LUND algorithm LUTHRU.
CKEY PHYSICS LUND THRUST E-FLOW / USER
C
C     Adapted by M.N.Minard & M.Pepe
C
C     Called by EFAXE in E-Flow code
C-    INPUT  : ITYP = type of fit (1=Calorimeters, 2=Masks)
C-
C-    OUTPUT : THR  = Thrust value
C-             OBL  = oblatness value
C==================================================================
#ifndef DOC
      SAVE
C
#include "lundcom.h"
#include "lundequ.h"
      DIMENSION TDI(3),TPR(3)
      DATA NC1,NC2/4, 2/
C...DUMMY CALL TO INITIALIZE BLOCK DATA LUEDAT RESIDING WITH ULALPS
      NP=0
      PS=0.
      DO 280 LD=1,2
         IF(LD.EQ.2) THEN
C...THRUST AXIS ALONG Z DIRECTION FOR MAJOR AXIS SEARCH
C           MST(2)=N+1
            PHI=ATAN2(P(N+1,2),P(N+1,1))
            CALL ALROBO(0.,-PHI,0.,0.,0.,ITYP)
            THE=ATAN2(P(N+1,1),P(N+1,3))
            CALL ALROBO(-THE,0.,0.,0.,0.,ITYP)
         ENDIF
C...FIND AND ORDER PARTICLES WITH HIGHEST P (PT FOR MAJOR)
C...(P(I,5) IS TEMPORARILY USED FOR EXTRA PARTICLE WEIGHT, 1 FOR THRUST)
         DO 100 LF=N+4,N+NC1+4
  100    P(LF,4)=0.
         DO 140 I=1,N
            IF(K(I,1).NE.ITYP) GOTO 140
            IF(LD.EQ.1) THEN
               NP=NP+1
               PA=SQRT(P(I,1)**2+P(I,2)**2+P(I,3)**2)
               P(I,5)=1.
               PS=PS+P(I,5)*PA
            ELSE
               PA=SQRT(P(I,1)**2+P(I,2)**2)
            ENDIF
            DO 110 LF=N+NC1+3,N+4,-1
               IF(PA.LE.P(LF,4)) GOTO 120
               DO 110 J=1,5
            P(LF+1,J)=P(LF,J)
 110        CONTINUE
            LF=N+3
  120       DO 130 J=1,3
  130       P(LF+1,J)=P(I,J)
            P(LF+1,4)=PA
            P(LF+1,5)=P(I,5)
  140    CONTINUE
         IF(NP.LE.1) THEN
C...VERY LOW MULTIPLICITIES (0 OR 1) NOT CONSIDERED
            THR=-1.
            OBL=-1.
            RETURN
         ENDIF
C...FIND AND ORDER INITIAL AXES WITH HIGHEST THRUST
         DO 150 LG=N+NC1+5,N+NC1+15
  150    P(LG,4)=0.
         NC=2**(MIN(NC1,NP)-1)
         DO 210 LC=1,NC
            DO 160 J=1,3
  160       TDI(J)=0.
            DO 170 LF=1,MIN(NC1,NP)
               SGN=P(N+LF+3,5)
               IF(2**LF*((LC+2**(LF-1)-1)/2**LF).GE.LC) SGN=-SGN
               DO 170 J=1,4-LD
  170       TDI(J)=TDI(J)+SGN*P(N+LF+3,J)
            TDS=TDI(1)**2+TDI(2)**2+TDI(3)**2
            DO 180 LG=N+NC1+MIN(LC,10)+4,N+NC1+5,-1
               IF(TDS.LE.P(LG,4)) GOTO 190
               DO 180 J=1,4
  180       P(LG+1,J)=P(LG,J)
            LG=N+NC1+4
  190       DO 200 J=1,3
  200       P(LG+1,J)=TDI(J)
            P(LG+1,4)=TDS
  210    CONTINUE
C...ITERATE DIRECTION OF AXIS UNTIL STABLE MAXIMUM
         P(N+LD,4)=0.
         LG=0
  220    LG=LG+1
         THP=0.
  230    THPS=THP
         DO 240 J=1,3
            IF(THP.LE.1E-10) TDI(J)=P(N+NC1+4+LG,J)
            IF(THP.GT.1E-10) TDI(J)=TPR(J)
  240    TPR(J)=0.
         DO 260 I=1,N
            IF(K(I,1).NE.ITYP) GOTO 260
            SGN=SIGN(P(I,5),TDI(1)*P(I,1)+TDI(2)*P(I,2)+TDI(3)*P(I,3))
            DO 250 J=1,4-LD
  250       TPR(J)=TPR(J)+SGN*P(I,J)
  260    CONTINUE
         THP=SQRT(TPR(1)**2+TPR(2)**2+TPR(3)**2)/PS
         IF(THP.GE.THPS+0.0001) GOTO 230
C...SAVE GOOD AXIS, TRY NEW INITIAL AXIS UNTIL A NUMBER OF TRIES AGREE
         IF(THP.LT.P(N+LD,4)-0.0001.AND.LG.LT.MIN(10,NC)) GOTO 220
         IF(THP.GT.P(N+LD,4)+0.0001) THEN
            LAGR=0
            SGN=(-1.)**INT(RNDM(IDUM)+0.5)
            DO 270 J=1,3
  270       P(N+LD,J)=SGN*TPR(J)/(PS*THP)
            P(N+LD,4)=THP
         ENDIF
         LAGR=LAGR+1
  280 IF(LAGR.LT.NC2.AND.LG.LT.MIN(10,NC)) GOTO 220
C...FIND MINOR AXIS AND VALUE BY ORTHOGONALITY
      SGN=(-1.)**INT(RNDM(IDUM)+0.5)
      P(N+3,1)=-SGN*P(N+2,2)
      P(N+3,2)=SGN*P(N+2,1)
      P(N+3,3)=0.
      THP=0.
      DO 290 I=1,N
         IF(K(I,1).NE.ITYP) GOTO 290
         THP=THP+P(I,5)*ABS(P(N+3,1)*P(I,1)+P(N+3,2)*P(I,2))
         P(I,5)=SQRT(MAX(P(I,4)**2-P(I,1)**2-P(I,2)**2-P(I,3)**2,0.))
  290 CONTINUE
      P(N+3,4)=THP/PS
C...RESET UNUSED COMPONENTS, ROTATE BACK TO ORIGINAL COORDINATE SYSTEM
      DO 300 LD=1,3
         K(N+LD,1)=LD
         K(N+LD,2)=0
  300 P(N+LD,5)=0.
C     MST(2)=N+3
      CALL ALROBO(THE,PHI,0.,0.,0.,ITYP)
C     MST(2)=0
      THR=P(N+1,4)
      OBL=P(N+2,4)-P(N+3,4)
C     MST(3)=3
      RETURN
      END
#endif
