      SUBROUTINE TREMPT(PBEST,LOUT,LIN,NIN)
C-----*************************************--------------------------
C! REMOVE BAD TPC POINTS TO GET A TRACK WITH BETTER CHI**2
C!
C!    AUTHOR        :- MARTIN POPPE 87/9/25
C!
C!
C!    INPUT  : LIN = ARRAY OF 'TPCO' COORDINATE NUMBERS OF A TRACK
C!             NIN = NUMBER OF INPUT PARTICLES
C!    OUTPUT : LOUT= ARRAY OF COORDINATE NUMBERS WITH 1 POINT
C!                   REMOVED SUCH THAT CHI**2 IS MINIMISED
C!             PBEST=FIT PROBABILITY OF BEST CASE
C?
C?    DESCRIPTION
C?    ===========
C?
C?    EACH OF THE POINTS IN "LIN" IS THROWN AWAY ONCE
C?    AT A TIME. THE REMAINING SET OF POINTS IS FITTED.
C?    LOUT IS FILLED WITH THOSE (NIN-1) POINTS FOR WHICH
C?    CHI**2 IS MINIMAL. THE INPUT ARGUMENTS REMAIN UNCHANGED
C------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
C
      LOGICAL LFLIP
      DIMENSION ERR(15),VAL(5),LIN(*),LOUT(*),LEVEN(12),LODD(12)
      IF(NIN.GT.10) GO TO 100
C
C     ONLY FOR SHORT TRACKS, ALL POINTS ARE INVESTIGATED
C
      NM1 = NIN-1
      IBEST = 1
      CALL TFTHEL
     1  ('TPCO',NM1,LIN(2),1,VAL,ERR,CBEST)
      DO 10 I=2,NIN
        IDUMY = LIN(I)
        LIN(I)= LIN(1)
        LIN(1)= IDUMY
        CALL TFTHEL
     1  ('TPCO',NM1,LIN(2),1,VAL,ERR,CHI2)
        IF(CHI2.LT.CBEST)THEN
             IBEST = I
             CBEST = CHI2
        END IF
   10 CONTINUE
C
C-----> RESTORE LIN IN ITS ORIGINAL FORM
C
      IDUMY = LIN(1)
      DO 20 I=2,NIN
   20   LIN(I-1)=LIN(I)
      LIN(NIN)=IDUMY
C
C-----> LIN -> LOUT
C
      IF(IBEST.NE.1) THEN
        DO 30 I=1,IBEST-1
   30   LOUT(I) = LIN(I)
      END IF
      IF(IBEST.NE.NIN) THEN
        DO 40 I=IBEST,NM1
   40   LOUT(I) = LIN(I+1)
      END IF
      PBEST = PROB(CBEST,(2*NM1-5))
      RETURN
  100 CONTINUE
C
C-----> LONG TRACKS ARE SPLIT IN EVEN AND ODD PARTS
C
      LFLIP = .TRUE.
      DO 110 I=1,NIN
      IF(LFLIP) THEN
        K=I/2 + 1
        LODD(K)= LIN(I)
      ELSE
        K = I/2
        LEVEN(K)=LIN(I)
      END IF
      LFLIP = .NOT.LFLIP
  110 CONTINUE
      NEVEN = NIN/2
      NODD  = NIN-NEVEN
      CALL TFTHEL
     1  ('TPCO',NEVEN,LEVEN,1,VAL,ERR,CEVEN)
      CALL TFTHEL
     1  ('TPCO',NODD,LODD,1,VAL,ERR,CODD )
      C0  = 0.0
      CEVEN = CEVEN/FLOAT(NEVEN)
      CODD  = CODD /FLOAT(NODD)
      IF(CEVEN.GT.CODD)THEN
         DO 140 I =1,NEVEN
           LODD(NODD+1) = LEVEN(I)
           CALL TFTHEL
     1     ('TPCO',(NODD+1),LODD,1,VAL,ERR,CC)
           IF(CC.GT.C0) THEN
                 KK = I
                 C0 = CC
           END IF
  140   CONTINUE
        KSHIT = 2*KK
      ELSE
         DO 150 I =1,NODD
           LEVEN(NEVEN+1) = LODD(I)
           CALL TFTHEL
     1     ('TPCO',(NEVEN+1),LEVEN,1,VAL,ERR,CC)
           IF(CC.GT.C0) THEN
                 KK = I
                 C0 = CC
           END IF
        KSHIT = 2*KK+1
  150   CONTINUE
      END IF
      DO 160 I=1,KSHIT
  160   LOUT(I) = LIN(I)
      NM1 = NIN-1
      IF(KSHIT.LT.NIN)THEN
        DO 170 I=KSHIT,NM1
  170     LOUT(I) = LIN(I+1)
      END IF
        CALL TFTHEL
     1  ('TPCO',NM1,LOUT(1),1,VAL,ERR,CBEST)
      PBEST = PROB(CBEST,(2*NM1-5))
      RETURN
      END
#endif
