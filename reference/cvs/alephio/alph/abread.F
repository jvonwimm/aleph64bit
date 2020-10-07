      SUBROUTINE ABREAD (JW, LUN, ELIST, *, *)
C --------------------------------------------------------------
CKEY ALPHARD READ BOS
C  Author :      H. Albrecht            Nov 89
C
C!    Similar to BREAD (same arguments). It ignores event dir.
C     N.b.: ABREAD cannot and BREAD should not be used to read
C     files with event directories or to select events by means
C     of data cards (SEVT etc.). There exist a special routine
C     ABRREC for this purpose.
C -----------------------------------------------------------------
#ifndef DOC
#include "boscom.h"
      INTEGER JW(*)
      CHARACTER*(*) ELIST
      DATA IBF1 /0/
*     ...
      IF (IBF1 .EQ. 0) IBF1 = NAMIND ('+BUF')
*
      IW(1) = 0
      IBF = IW(IBF1)
   10 IF (IBF .EQ. 0)  GO TO 20
      IF (IW(IBF-2) .EQ. LUN)  GO TO 30
      IBF = IW(IBF-1)
      GO TO 10
   20 CALL BOSRD(JW,LUN,ELIST,*101,*102)
      GO TO 100
   30 CONTINUE
CC*CA UNPACKIO
      IOAC=MOD(IW(IBF+1),8)
      IOMD=MOD(IW(IBF+1)/8,8)
      IOST=MOD(IW(IBF+1)/64,8)
CC*CC UNPACKIO
*        test read
      IF(IOST.EQ. 2) CALL BABEND('WRONG UNIT IN ABREAD')
      IF(IOMD.EQ. 0) THEN
        CALL BOSRD(JW,LUN,ELIST,*101,*102)
      ELSE IF(IOMD.EQ.1) THEN
        CALL BEPRD(JW,LUN,ELIST,*101,*102)
      ELSE IF(IOMD.EQ.2) THEN
        CALL BTERD(JW,LUN,ELIST,*101,*102)
      ELSE
        CALL BABEND('WRONG UNIT IN ABREAD')
      ENDIF
  100 RETURN
  101 RETURN 1
  102 RETURN 2
      END
#endif
