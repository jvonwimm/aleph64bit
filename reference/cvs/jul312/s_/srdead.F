      SUBROUTINE SRDEAD
C----------------------------------------------------------------------
C! Simulate broken SATR channels
C!
C!    Author:     H. Meinhard       31-Jul-1989
C!
C!    Description
C!    ===========
C!    Decode data cards SBCH, SBCA, SBCR, SBHV, SBST and fill flags with
C!    dead channel information accordingly
C!    Format of input banks:
C!    SBCH crate_nr[1,3] card_nr[0,23] tdc_nr[0,15]
C!    SBCA crate_nr[1,3] card_nr[0,23]
C!    SBCR crate_nr[1,3]
C!    SBHV hv_chan_nr[0,31]
C!    SBST hv_chan_nr[0,31] hv_string_nr[0,3]
C!    Several input cards with the same name are allowed and treated in
C!    sequence.
C!
C!    Modified:   H. Meinhard       08-Jan-1993
C!    The mechanism implemented herein has never been used. On request
C!    of Francoise Jacot, this routine is modified into a dummy in order
C!    not to refer to the banks indicated above. This allows to drop
C!    the subschema SatrDefects, and to free five bank names.
C----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "scalbc.h"
#include "sgeomc.h"
#include "bosext.h"
C----------------------------------------------------------------------
C C link to banks
C       JSBCH = IW(NAMIND('SBCH'))
C       JSBCA = IW(NAMIND('SBCA'))
C       JSBCR = IW(NAMIND('SBCR'))
C       JSBHV = IW(NAMIND('SBHV'))
C       JSBST = IW(NAMIND('SBST'))
C C
C C SBCH bank: single broken channel
C   300 CONTINUE
C       IF (JSBCH .EQ. 0)                                    GOTO 310
C       ICRAT = IW(JSBCH+1)
C       ICARD = IW(JSBCH+2)
C       ITDCN = IW(JSBCH+3)
C       ICHAN = (ICRAT-1)*MCRDSG*MTDCSG + ICARD*MTDCSG + ITDCN
C       JWD = IBITS(ICHAN,5,27) + 1
C       JBT = IBITS(ICHAN,0,5)
C       JWIRSC(JWD) = IBCLR(JWIRSC(JWD),JBT)
C       JSBCH = IW(JSBCH-1)
C       GOTO 300
C C
C C SBCA bank: broken TDC card
C   310 CONTINUE
C       IF (JSBCA .EQ. 0)                                    GOTO 330
C       ICRAT = IW(JSBCA+1)
C       ICARD = IW(JSBCA+2)
C       DO 320 ITDCN = 0, MTDCSG-1
C         ICHAN = (ICRAT-1)*MCRDSG*MTDCSG + ICARD*MTDCSG + ITDCN
C         JWD = IBITS(ICHAN,5,27) + 1
C         JBT = IBITS(ICHAN,0,5)
C         JWIRSC(JWD) = IBCLR(JWIRSC(JWD),JBT)
C   320 CONTINUE
C       JSBCA = IW(JSBCA-1)
C       GOTO 310
C C
C C SBCR bank: broken TDC crate
C   330 CONTINUE
C       IF (JSBCR .EQ. 0)                                     GOTO 360
C       ICRAT = IW(JSBCR+1)
C       DO 350 ICARD = 0, MCRDSG-1
C         DO 340 ITDCN = 0, MTDCSG-1
C           ICHAN = (ICRAT-1)*MCRDSG*MTDCSG + ICARD*MTDCSG + ITDCN
C           JWD = IBITS(ICHAN,5,27) + 1
C           JBT = IBITS(ICHAN,0,5)
C           JWIRSC(JWD) = IBCLR(JWIRSC(JWD),JBT)
C   340   CONTINUE
C   350 CONTINUE
C       JSBCR = IW(JSBCR-1)
C       GOTO 330
C C
C C SBHV bank: broken HV channel
C   360 CONTINUE
C       IF (JSBHV .EQ. 0)                                     GOTO 361
C       IHVCH = IW(JSBHV+1)
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C C
C C TO BE ADDED
C C
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       JSBHV = IW(JSBHV-1)
C       GOTO 360
C C
C C SBST bank: broken HV channel string
C   361 CONTINUE
C       IF (JSBST .EQ. 0)                                     GOTO 999
C       IHVCH = IW(JSBST+1)
C       IHVST = IW(JSBST+2)
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C C
C C TO BE ADDED
C C
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       JSBST = IW(JSBST-1)
C       GOTO 361
C C
  999 CONTINUE
      RETURN
      END
#endif
