      SUBROUTINE QFHEAD
CKEY FILL HEADER /INTERNAL
C-----------------------------------------------------------------------
C!   Puts selected contents of banks EVEH, DHEA, KEVH into variables
C!   in COMMON blocks
C!
C!  Author   : E. Blucher      5-OCT-1988
C!  Modified : E.Blucher      18-APR-1991
C!     -- All ALPHA variables reset to zero if banks not available.
C!        ALTRIG used to fill trigger words.
C!
C!  Called from QFILL
C!======================================================================
#ifndef DOC
C
#include "qcde.h"
C
#include "evehjj.h"
#include "dheajj.h"
#include "kevhjj.h"
#include "xtcnjj.h"
#include "revhjj.h"
      LOGICAL BTEST
      INTEGER ALTRIG
C!======================================================================
C
C       EVEH is NOT a standard (tabular) bank !
C
      KEVEH = IW(NAEVEH)
      KEXP =   IW(KEVEH+JEVEEN)
C     KRUN, KEVT : filled in QMREAD
      KEVERT = IW(KEVEH+JEVERT)
      KEVEDA = IW(KEVEH+JEVEDA)
      KEVETI = IW(KEVEH+JEVETI)
      KEVEMI(1) = IW(KEVEH+JEVEM1)
      KEVEMI(2) = IW(KEVEH+JEVEM2)
      KEVEMI(3) = IW(KEVEH+JEVEM3)
      KEVEMI(4) = IW(KEVEH+JEVEM4)
      KEVETY = IW(KEVEH+JEVETY)
      KEVEES = IW(KEVEH+JEVEES)
C
C--- DHEA ---------
C
      KDHEA =  IW(NADHEA)
      IF (KDHEA .EQ. 0)THEN
        KDHEFP = 0
        KDHENP = 0
        KDHENM = 0
        QDHEEC = 0.
        QDHEEL = 0.
        QDHEPF = 0.
        QDHETH = 0.
        QDHEPH = 0.
        QDHEEF = 0.
        QDHEET = 0.
        QDHET1 = 0.
        QDHEP1 = 0.
        QDHET2 = 0.
        QDHEP2 = 0.
        QDHEE1 = 0.
        QDHEE2 = 0.
        QDHEE3 = 0.
        KDHENX = 0
        KDHENV = 0
        KDHENJ = 0
        KDHENX = 0
        KDHENV = 0
        KDHENJ = 0
      ELSE
        KDHEFP = IW(KDHEA+JDHEFP+LMHLEN)
        KDHENP = IW(KDHEA+JDHENP+LMHLEN)
        KDHENM = IW(KDHEA+JDHENM+LMHLEN)
        QDHEEC = RW(KDHEA+JDHEEC+LMHLEN)
        QDHEEL = RW(KDHEA+JDHEEL+LMHLEN)
        QDHEPF = RW(KDHEA+JDHEPF+LMHLEN)
        QDHETH = RW(KDHEA+JDHETH+LMHLEN)
        QDHEPH = RW(KDHEA+JDHEPH+LMHLEN)
        QDHEEF = RW(KDHEA+JDHEEF+LMHLEN)
        QDHEET = RW(KDHEA+JDHEET+LMHLEN)
        QDHET1 = RW(KDHEA+JDHET1+LMHLEN)
        QDHEP1 = RW(KDHEA+JDHEP1+LMHLEN)
        QDHET2 = RW(KDHEA+JDHET2+LMHLEN)
        QDHEP2 = RW(KDHEA+JDHEP2+LMHLEN)
        QDHEE1 = RW(KDHEA+JDHEE1+LMHLEN)
        QDHEE2 = RW(KDHEA+JDHEE2+LMHLEN)
        QDHEE3 = RW(KDHEA+JDHEE3+LMHLEN)
        KDHENX = IW(KDHEA+JDHENX+LMHLEN)
        KDHENV = IW(KDHEA+JDHENV+LMHLEN)
        KDHENJ = IW(KDHEA+JDHENJ+LMHLEN)
        KDHENX = IW(KDHEA+JDHENX+LMHLEN)
        KDHENV = IW(KDHEA+JDHENV+LMHLEN)
        KDHENJ = IW(KDHEA+JDHENJ+LMHLEN)
      ENDIF
C
C--- KEVH ---------
C
      KKEVH = IW(NAKEVH)
      IF (KKEVH .EQ. 0)THEN
        KKEVNT = 0
        KKEVNV = 0
        KKEVID = 0
        QKEVWT = 0.
      ELSE
        KKEVNT = IW(KKEVH+JKEVNT+LMHLEN)
        KKEVNV = IW(KKEVH+JKEVNV+LMHLEN)
        KKEVID = IW(KKEVH+JKEVPI+LMHLEN)
        QKEVWT = RW(KKEVH+JKEVWT+LMHLEN)
      ENDIF
C
C--- LOLE ---------
C
      JLOLE = IW(NALOLE) + LMHLEN
      IF(JLOLE.NE.LMHLEN) THEN
        IF(IW(JLOLE + 12).EQ.0)XVLCAL=.TRUE.
      ENDIF
C
C--- REVH ---------
C
      JREVH = IW(NAREVH) + LMHLEN
      IF(JREVH.EQ.LMHLEN)THEN
        KREVDS=0
        XVLCAL=.FALSE.
        XVECAL=.FALSE.
        XVTPC=.FALSE.
        XVTPCD=.FALSE.
        XVITC=.FALSE.
        XVSATR=.FALSE.
        XVHCAL=.FALSE.
      ELSE
        KREVDS=IW(JREVH+JREVDS)
        XVECAL=BTEST(KREVDS,0).AND.BTEST(KREVDS,1).AND.BTEST(KREVDS,2)
        XVHCAL=BTEST(KREVDS,7).AND.BTEST(KREVDS,8).AND.BTEST(KREVDS,9)
        XVTPCD=BTEST(KREVDS,4)
        XVTPC=BTEST(KREVDS,15)
        XVITC=BTEST(KREVDS,5)
        XVSATR=BTEST(KREVDS,6)
      ENDIF
C
C--- XTEB or XTRB
C
      ITRG=ALTRIG(KXTET1,KXTET2,KXTEL2)
      IF(ITRG.EQ.0)THEN
        KXTET1=0
        KXTET2=0
        KXTEL2=0
      ENDIF
C
C--- XTCN
C
      JXTCN = IW(NAXTCN) + LMHLEN
      IF(JXTCN.EQ.LMHLEN)THEN
        KXTCGC= 0
        KXTCLL= 0
        KXTCBN= 0
        KXTCCL= 0
        KXTCHV= 0
        KXTCEN= 0
      ELSE
        KXTCGC=IW(JXTCN+JXTCGC)
        KXTCLL=IW(JXTCN+JXTCLL)
        KXTCBN=IW(JXTCN+JXTCBN)
        KXTCCL=IW(JXTCN+JXTCCL)
        KXTCHV=IW(JXTCN+JXTCTR)
        KXTCEN=IW(JXTCN+JXTCTR + 1)
      ENDIF
C
C--- XLUMOK
C
      XHVTRG=XLUMOK(DUMMY)
C
C---Beam position from BOM
C
      CALL QBOMEV (QVXBOM, QVYBOM, KERBOM)
C
   90 END
#endif
