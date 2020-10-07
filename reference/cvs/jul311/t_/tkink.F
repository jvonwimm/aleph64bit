      SUBROUTINE TKINK(DCHI2,KINK,LIN,NIN)
C-----************************************--------------------------
C!    FIND KINK IN A TPC TRACK
C!
C!    AUTHOR        :- GEORG STIMPFL 87/9/25
C!
C!    DO NOT CALL THIS SUBROUTINE FOR LESS THAN 10 POINTS
C!    ===================================================
C!
C!    INPUT  : LIN = ARRAY OF 'TPCO' COORDINATE NUMBERS OF A TRACK
C!             NIN = NUMBER OF INPUT PARTICLES
C!    OUTPUT : KINK= FIRST COORDINATE AFTER KINK
C!                   REMOVED SUCH THAT CHI**2 IS MINIMISED
C!             DCHI2=IMPROVEMENT OF CHI**2 BY PUTTING THE KINK INTO
C!                   THE RIGHT PLACE
C?
C?    DESCRIPTION
C?    ===========
C?
C?    THE KINK POINT IS CALCULATED AS THAT ONE FOR WHICH
C?    THE SUM OF THE CHI**2 VALUES FOR BOTH ENDS OF THE TRACK
C?    IS MINIMAL. DCHI2 IS DEFINED AS
C?    DCHI2 = CHI**2(BEST KINK POINT) - CHI**2(WORST KINK POINT)
C------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
C
      DIMENSION ERR(15),VAL(5),LIN(*)
      SC1= 0.0
      SC2= 1.0E30
      IFIRST = 4
      ILAST  = NIN-4
      DO 10 I=IFIRST,ILAST
        NP1 = I
        NP2 = NIN-NP1
        CALL TFTHEL
     1  ('TPCO',NP1,LIN(1),1,VAL,ERR,C1)
        CALL TFTHEL
     1  ('TPCO',NP2,LIN(I),1,VAL,ERR,C2)
        SC = C1+C2
        IF(SC.GT.SC1) SC1=SC
        IF(SC.LE.SC2) THEN
          SC2 = SC
          KINK= I + 1
        END IF
   10 CONTINUE
      DCHI2 = SC1 - SC2
      RETURN
      END
#endif
