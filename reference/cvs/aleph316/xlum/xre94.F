      LOGICAL FUNCTION XRE94(KRUN,KEVT)
C**********************************************************
C
C  Author : J. Boucrot, October 29 1997
C  Modified : B Bloch from QRE94(ALPHA)for alephlib,Nov 1997
C
C! Flag laser events hidden in runs 25520 to 25530 taken in 1994
C  in these runs the X1RG bank is unusable to flag these events
C     XGOOD = .True.  if NOT laser TPC event
C     XGOOD = .False. if TPC laser event
C
C***********************************************************
#ifndef DOC
      INTEGER NAEVEH / 0 / 
      SAVE NAEVEH
      LOGICAL XGOOD,BTEST
      PARAMETER ( NBADB = 23)
C
#include "evehjj.h"
#include "bcs.h"
C
      XRE94=.TRUE.
      IF (KRUN.LT.25520.OR.KRUN.GT.25530) GO TO 999

C Get the trigger mask from the EVEH bank, not from X1RG:
      IF( NAEVEH .EQ. 0 ) NAEVEH = NAMIND('EVEH')
      JEVEH=IW(NAEVEH)
      IF (JEVEH.EQ.0) GO TO 999
C
      IWORD=IW(JEVEH+JEVEM1)
C TPC Laser events have only bit # 23 (starting from bit 0 ) set in IWORD:
      NBT=0
      IB23=0
      DO 100 IK=1,32
         IF (BTEST(IWORD,IK-1)) THEN
            NBT=NBT+1
            IF (IK.EQ.NBADB+1) IB23=1
         ENDIF
 100  CONTINUE
      IF (IB23.EQ.1.AND.NBT.EQ.1) XRE94=.FALSE.
C
 999  RETURN
      END
#endif
