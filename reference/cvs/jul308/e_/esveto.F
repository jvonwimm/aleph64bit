      SUBROUTINE ESVETO ( EWSUM , JERR)
C----------------------------------------------------------------------
C!  - Veto spark event for E-Flow
C
C     Author : M.N.Minard  9-5-90
C     Input : EWSUM wire energy
C     Output: JERR = 0 not a spark event
C                  = 1 spark event detected
C?
C!======================================================================
#ifndef DOC
#include "eclujj.h"
#include "prpwjj.h"
#include "bcs.h"
       DIMENSION ECATOW (36) , EWIRE (36)
       LOGICAL LDECOD
       EXTERNAL NAMIND
       DATA FRCU , FRTU,EGOODU / 0.5,0.9,10./
       DATA EMIN /  80. /
#include "bmacro.h"
C
C-     Calculate energy in Pads & Highest fraction
C
      JERR = 0
      NAETDI = NAMIND('ETDI')
      NAEWHE = NAMIND ('EWHE')
      KEWHE = IW(NAEWHE)
      CALL VZERO (EWIRE , 36)
      IF ( KEWHE.EQ.0) GO TO 990
      DO 5 IMOD = 1,LROWS(KEWHE)
      IMD = ITABL (KEWHE,IMOD,2)
      IF ( IMD.LT.1.OR.IMD.GT.36) GO TO 5
      EWIRE (IMD) = FLOAT(ITABL(KEWHE,IMOD,1))*1.E-6
 5    CONTINUE
      EFRAC = 0.
      NMOD = 0
      JERR = 0
      ETOT = 0.
      CALL VZERO (ECATOW,36)
      KETDI  = IW(NAETDI)
      IF (KETDI.NE.0) THEN
      LETDI  = IW(KETDI + LMHCOL )
      NTOW   = IW(KETDI + LMHROW )
C? Verify on line bank consistency
      IF (IW(KETDI) .NE. LETDI*NTOW+2) GOTO 990
      KETDI  = KETDI + LMHLEN
      DO 20 ITOW = 1,NTOW
      LDECOD = .FALSE.
       DO 10 K=1,3
         IF(IW(KETDI+K+1).NE.0) THEN
             IF (.NOT.LDECOD) THEN
                 ITH = IBITS(IW(KETDI+1),16,8)
                 IPH = IBITS(IW(KETDI+1),2,9)
                 IF ( ITH.LT.1.OR.ITH.GT.228) GO TO 10
                 IF ( IPH.LT.1.OR.IPH.GT.384) GO TO 10
                 CALL EMDTOW(ITH,IPH,ISC,IMD,IRG)
                 IMOD = IMD + 12 * (ISC - 1 )
                 LDECOD = .TRUE.
             END IF
             IF ( IMOD.GT.0.AND.IMOD.LT.37) THEN
             ECATOW (IMOD) = ECATOW(IMOD)+FLOAT(IW(KETDI+1+K))*1.E-6
             ETOT = ETOT+FLOAT(IW(KETDI+1+K))*1.E-6
             ENDIF
         END IF
   10    CONTINUE
      KETDI=KETDI+LETDI
   20 CONTINUE
C
C-    Calculate maximal fraction  & enErgy
C
      IF ( ETOT.LT.200.) GO TO 990
      EGOOD = 0
      FRW = 0.
      FRT = 0.
      IMD = 0
      DO 30 IMOD = 1, 36
      IF ( ECATOW(IMOD).LT. EMIN ) GO TO 30
      FRWC = EWIRE(IMOD)/ECATOW(IMOD)
      IF ( FRWC .GT.0.8 ) THEN
         EGOOD = EGOOD + EWIRE (IMOD)
      ENDIF
      FRTC = ECATOW(IMOD) / ETOT
      IF ( FRTC.GT.FRT) THEN
        FRW = FRWC
        FRT = FRTC
        IMD = IMOD
      ENDIF
 30   CONTINUE
C
C-    Remove spark event
      IF ( FRW.LT.FRCU.AND.FRT.GT.FRTU.AND.EGOOD.LT.EGOODU) THEN
         CALL RERROR ('ESVETO ' ,1,' Spark Ecal candidate')
         JERR = 1
      ENDIF
      ENDIF
 990  RETURN
      END
#endif
