      SUBROUTINE SSCPCR(IER)
C----------------------------------------------------------------------
C! Create the SSCP bank
C!
C!    Author:     H. Meinhard       20-Nov-1989
C!
C!    Output:     - IER       /I    error flag (0 = no error)
C!
C!    Description
C!    ===========
C!    Create bank SSCP, fill it with numbers derived from SCOO which
C!    must be present. Common block SGEOMC must also be available
C----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "sanamc.h"
#include "scoojj.h"
#include "sgeomc.h"
#include "sscpjj.h"
      INTEGER NDAT(2,MLASG)
#include "bmacro.h"
#include "smacro.h"
C----------------------------------------------------------------------
C
C---------- locate the SCOO bank
C
      JSCOO = IW(NASCOO)
      IF (JSCOO .EQ. 0)                                     GOTO 901
C
C---------- create the SSCP bank, fill header
C
      CALL AUBOS('SSCP',0,LMHLEN+2*MLAYSG*LSSCPA,JSSCP,IGARB)
      IF (JSSCP .EQ. 0 .OR. IGARB .EQ. 2)                   GOTO 902
      IF (IGARB .EQ. 1) JSCOO = IW(NASCOO)
      IW(JSSCP+LMHCOL) = LSSCPA
      IW(JSSCP+LMHROW) = 2 * MLAYSG
C
C---------- reset the coordinates counter
C
      CALL VZERO(NDAT,2*MLASG)
C
C---------- loop over coordinates, count them
C
      DO 300 ICOOR = 1, LROWS(JSCOO)
        ISIDE = ITABL(JSCOO,ICOOR,JSCOSI)
        ILAYE = ITABL(JSCOO,ICOOR,JSCOLA)
        NDAT(ISIDE,ILAYE) = NDAT(ISIDE,ILAYE) + 1
  300 CONTINUE
C
C---------- fill SSCP bank
C
      DO 310 MROW = 1, LROWS(JSSCP)
        KSSCP = KROW(JSSCP,MROW)
        IF (MROW .EQ. 1) THEN
          IW(KSSCP+JSSCPB) = 1
        ELSE
          IW(KSSCP+JSSCPB) = ITABL(JSSCP,MROW-1,JSSCPE) + 1
        ENDIF
        ISIDE = (MROW-1)/MLAYSG + 1
        ILAYE = KXX( MOD(MROW-1,MLAYSG) + 1 )
        IW(KSSCP+JSSCPE) = ITABL(JSSCP,MROW,JSSCPB) - 1 +
     +    NDAT(ISIDE,ILAYE)
  310 CONTINUE
C
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      GOTO 999
  902 IER = 2
      GOTO 999
  999 CONTINUE
      END
#endif
