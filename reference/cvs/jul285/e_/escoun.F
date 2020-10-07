      SUBROUTINE ESCOUN(NSTOR,NSMOD)
C----------------------------------------------------------------
C! Count number of nonzero ECAL storeys in ESDA
C
C   Author:   J.Knobloch    18.9.89
C
C   Output:  NSTOR   Number of nonzero storeys
C            NSMOD   Number of non-zero storeys / module
C?
C!======================================================================
#ifndef DOC
#include "ecnamc.h"
#include "etdijj.h"
#include "bcs.h"
      INTEGER NSMOD(36)
      LOGICAL LDECOD
#include "bmacro.h"
      KETDI  = IW(NAETDI)
      NSTOR=0
      CALL VZERO (NSMOD,36)
      IF (KETDI.NE.0) THEN
      LETDI  = IW(KETDI + LMHCOL )
      NTOW   = IW(KETDI + LMHROW )
C? Verify on line bank consistency
      IF (IW(KETDI) .NE. LETDI*NTOW+2) GOTO 990
      KETDI  = KETDI + LMHLEN
      DO 20 ITOW = 1,NTOW
         LDECOD = .FALSE.
         DO 10 K = 1,3
C? --- count
         IF(IW(KETDI+K+1).NE.0)NSTOR=NSTOR+1
         IF(IW(KETDI+K+1).NE.0) THEN
             IF (.NOT.LDECOD) THEN
                 IPH = IBITS(IW(KETDI+1),2,9)
                 ITH = IBITS(IW(KETDI+1),16,8)
                 IF(IPH.LT.1.OR.IPH.GT.384) GOTO 50
                 IF(ITH.LT.1.OR.ITH.GT.228) GOTO 50
                 CALL EMDTOW(ITH,IPH,ISC,IMD,IRG)
                 IMOD = IMD + 12 * (ISC - 1 )
                 IF (IMOD.LT.1.OR.IMOD.GT.36) GO TO 50
                 LDECOD = .TRUE.
             END IF
             NSMOD(IMOD) = NSMOD(IMOD)+1
 50          CONTINUE
         END IF
   10    CONTINUE
      KETDI=KETDI+LETDI
   20 CONTINUE
      ENDIF
 990  RETURN
      END
#endif
