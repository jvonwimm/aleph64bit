      SUBROUTINE LFOURC(XPAD,YPAD,IADDR,ENE4,ENE9)
C-----------------------------------------------------------------------
C! Find central towers
C!
C!   Author   : P. H. Hansen   890101
C!   Modified : P. H. Hansen   890701
C!   Input    : IADDR          Address of central (or hit) tower
C!              XPAD,YPAD      Barycenter in pad coordinates
C!                             for the cluster
C!   Output   : ENE4(3)        Storey energies in the four central tower
C!              ENE9(3)        Storey energies in the nine central tower
C!              XPAD,YPAD      Barycenter in pad coordinates
C!                             for the nonet
C!
C!   Description :
C!   ===========
C?     Find the storey energies of closest four towers
C?     Find the storey energies of closest nonet
C?     Find nonet center in pad coordinates
C?
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
#include "lcnamc.h"
#include "lsdajj.h"
#include "lcrejj.h"
#include "lwbank.h"
      DIMENSION ENE4(*),ENE9(*),LIS4(4)
#include "bmacro.h"
C----------------------------------------------------------------------
C
      CALL VZERO(ENE4,3)
      CALL VZERO(ENE9,3)
      KLCRE = IW(NALCRE)
C
C Find the four tower addresses
      LIS4(1) = IADDR
      IF(XPAD.LE.0..AND.YPAD.GT.0.) THEN
        LIS4(2) = IADDR-1
        LIS4(3) = IADDR+15
        LIS4(4) = IADDR+16
      ENDIF
      IF(XPAD.GT.0..AND.YPAD.GT.0.) THEN
        LIS4(2) = IADDR+16
        LIS4(3) = IADDR+17
        LIS4(4) = IADDR+1
      ENDIF
      IF(XPAD.GT.0..AND.YPAD.LE.0.) THEN
        LIS4(2) = IADDR+1
        LIS4(3) = IADDR-15
        LIS4(4) = IADDR-16
      ENDIF
      IF(XPAD.LE.0..AND.YPAD.LE.0.) THEN
        LIS4(2) = IADDR-16
        LIS4(3) = IADDR-17
        LIS4(4) = IADDR-1
      ENDIF
C
C Find the corresponding storey data
      KLMAP = IW(NALMAP)
      KLSDA = IW(NALSDA)
      DO 10 I=1,4
        IF(LIS4(I).GT.0.AND.LIS4(I).LE.LLMAP) THEN
          IT = IW(KLMAP+LMHLEN+LIS4(I))
          IF(IT.GT.0) THEN
            ENE4(1) = ENE4(1)+RTABL(KLSDA,IT,JLSDEN)
            ENE4(2) = ENE4(2)+RTABL(KLSDA,IT,JLSDEN+1)
            ENE4(3) = ENE4(3)+RTABL(KLSDA,IT,JLSDEN+2)
          ENDIF
        ENDIF
   10 CONTINUE
C
C Find storey data of the nine neighbour towers
      IT = IW(KLMAP+LMHLEN+IADDR)
      IF(IT.GT.0) THEN
        ENE9(1) = RTABL(KLSDA,IT,JLSDEN)
        ENE9(2) = RTABL(KLSDA,IT,JLSDEN+1)
        ENE9(3) = RTABL(KLSDA,IT,JLSDEN+2)
      ENDIF
      ECEN = ENE9(1)+ENE9(2)+ENE9(3)
      ECUT = RTABL(KLCRE,1,JLCRES)
      IF(ECEN.LT.ECUT)                         GOTO 999
      ELEF = 0.
      ERIG = 0.
      EABO = 0.
      EUND = 0.
      EROW = ECEN
      ECOL = ECEN
      DO 20 IADJ=1,8
        JADJ = JLCRNE+IADJ-1
        LMAP = IADDR + ITABL(KLCRE,1,JADJ)
        IF(LMAP.GT.LLMAP.OR.LMAP.LE.0)         GOTO 20
        IT = IW(KLMAP+LMHLEN+LMAP)
        IF(IT.GT.0) THEN
          E = RTABL(KLSDA,IT,JLSDEN)
     &          +RTABL(KLSDA,IT,JLSDEN+1)
     &          +RTABL(KLSDA,IT,JLSDEN+2)
C
C Sum up all 3-tower rows and columns in the 3x3 matrix
          IF(IADJ.EQ.1.OR.IADJ.EQ.2.OR.IADJ.EQ.8) ELEF = ELEF+E
          IF(IADJ.GE.2.AND.IADJ.LE.4) EABO = EABO+E
          IF(IADJ.GE.4.AND.IADJ.LE.6) ERIG = ERIG+E
          IF(IADJ.GE.6.AND.IADJ.LE.8) EUND = EUND+E
          IF(IADJ.EQ.1.OR.IADJ.EQ.5) EROW = EROW+E
          IF(IADJ.EQ.3.OR.IADJ.EQ.7) ECOL = ECOL+E
C
C Sum total energy in each storey
          ENE9(1) = ENE9(1)+RTABL(KLSDA,IT,JLSDEN)
          ENE9(2) = ENE9(2)+RTABL(KLSDA,IT,JLSDEN+1)
          ENE9(3) = ENE9(3)+RTABL(KLSDA,IT,JLSDEN+2)
        ENDIF
   20 CONTINUE
      IF(EROW.LT.ECUT)                         GOTO 999
      IF(ECOL.LT.ECUT)                         GOTO 999
C
C Nonet barycenter
      IF(ELEF.GT.0.AND.ERIG.GT.0.) THEN
        XPAD = (ERIG-ELEF)/(ERIG+ELEF+ECOL)
      ELSEIF(ERIG.GT.0.) THEN
        XPAD = ERIG/(ECOL+ERIG)
      ELSEIF(ELEF.GT.0.) THEN
        XPAD = -ELEF/(ECOL+ELEF)
      ENDIF
      IF(EABO.GT.0.AND.EUND.GT.0.) THEN
        YPAD = (EABO-EUND)/(EABO+EUND+EROW)
      ELSEIF(EABO.GT.0.) THEN
        YPAD = EABO/(EABO+EROW)
      ELSEIF(EUND.GT.0.) THEN
        YPAD = -EUND/(EUND+EROW)
      ENDIF
  999 CONTINUE
      END
#endif
