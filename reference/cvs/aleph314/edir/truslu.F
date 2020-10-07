      SUBROUTINE TRUSLU(ITRQUA,KNCHS,THRUST,AXTR)
C-----------------------------------------------------------------------
CKEY EDIR THRUST
C! Gives the correct format for thrust calculation and call the
C! thrust routine.
C-
C   Input  : ITRQUA  = Charged tracks quality array.
C                      Track quality defined in LEPTO.
C            KNCHS   = Number of good charged tracks.
C                      Good tracks defined in LEPTO.
C   Output : THRUT   = Thrust.
C            AXTR    = Thrust axes.
C-
C   Called by : LEPTO
C   Calls     : ULTHRU,TRPFRF
C   Input banks : PFRF
C-
C Author:  J.C.Brient        Date : 29/6/89
C Updated: M.Cattaneo        Date : 21/7/96
C   Reject tracks with bad TRPFRF return code
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
C --
      DIMENSION PRAX(4,242),IFX(242),PP1(4),AXTR(3)
      DIMENSION ITRQUA(*)
      EXTERNAL NAMIND
      LOGICAL DEBUG
      DATA DEBUG/.TRUE./
C --
#include "bmacro.h"
C --
       IRC = 0
C --
       NPFRF = 0
       JPFRF = IW(NAMIND('PFRF'))
       IF(JPFRF.GT.0) NPFRF = LROWS(JPFRF)
C --
       NPAT = KNCHS +42
       IF(NPAT.GT.242) THEN
          IF(IW(6).GT.0) WRITE(IW(6),*) 'TRUSLU_ too many charged',
     &                   ' tracks to compute the trust'
C -- !!!!          STOP
          THRUST = -2.
          RETURN
       ENDIF
C --
        N42 = 42
        IERR2  = 0
        THRUST = -2.
C --
        JTRU = 0
        NP = NPAT - 42
        DO 10 I = 1 , NPFRF
          IF(ITRQUA(I).NE.1)    GO TO 10
          CALL TRPFRF(I,PP1,IRF)
          IF( IRF .NE. 0 ) GOTO 10
          PP1(4) = PP1(4)**2 + (0.139567)**2
          IF(PP1(4).GT.0.000001) PP1(4) = SQRT(PP1(4))
          JTRU =  JTRU + 1
          IFX (JTRU) = 0
          DO 5 JX = 1 , 4
            PRAX(JX,JTRU) = PP1(JX)
 5        CONTINUE
 10     CONTINUE
C --
        CALL ULTHRU(N42,NP,PRAX,IFX,THRUST,AXTR,OBL)
C --
        RETURN
        END
#endif
