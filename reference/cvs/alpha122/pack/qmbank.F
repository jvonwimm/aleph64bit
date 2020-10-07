      SUBROUTINE QMBANK (CNAM, CTEXT, ARRAY, LENGTH)
CKEY CARD /INTERNAL
C----------------------------------------------------------------------
C! Copy bank CNAM (e.g. given by data cards) into ARRAY(LENGTH)
C  Bank CNAM is assumed to exist only once. QMBANK should be called
C  only once for a given bank as the bank is dropped afterwards.
C  If bank CNAM does not exist, the content of ARRAY is unchanged.
C  Called from anywhere.
C                                      Modified: E. Blucher, 27.04.89
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      CHARACTER *(*) CNAM, CTEXT
      DIMENSION ARRAY(1)
C----------------------------------------------------------------------
C
      J = IW(NAMIND(CNAM))
      IF (J .EQ. 0)  GO TO 20
C
      DO 10 I = 1, MIN0 (LENGTH, IW(J))
   10 ARRAY(I) = RW(J+I)
C
      I = NDROP (CNAM, IW(J-2))
C
   20 IF (CTEXT .EQ. ' ')  GO TO 90
C
      CALL QWMESS (CTEXT)
      WRITE (KUPRNT, 1001)  CNAM, (ARRAY(I), I=1,LENGTH)
      IF (KUPTER .NE. 0)
     &  WRITE (KUPTER, 1001)  CNAM, (ARRAY(I), I=1,LENGTH)
C
   90 RETURN
 1001 FORMAT (1X,A,(1X,10G12.4))
      END
#endif
