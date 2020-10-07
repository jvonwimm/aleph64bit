      SUBROUTINE SACOCR(IER)
C----------------------------------------------------------------------
C! Create SACO bank
C!
C!    Author:     H. Meinhard       31-Jan-1990
C!
C!    Output:     - IER       /I    error flag
C!                                  1 = no input bank found
C!                                  2 = no space for new bank
C!
C!    Description
C!    ===========
C!    Create the SACO bank which is mapped 1 : 1 to the coordinates
C!    bank SCOO
C----------------------------------------------------------------------
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "rlunit.h"
#include "sacojj.h"
#include "saligc.h"
#include "sanamc.h"
#include "scoojj.h"
#include "sgeomc.h"
#include "srecpc.h"
      REAL X1UNC(3),X2UNC(3),X3UNC(3),
     +     X1COR(3),X2COR(3),X3COR(3),
     +     X1BCO(3),X2BCO(3),X3BCO(3)
#include "bosext.h"
#include "bmacro.h"
#include "smacro.h"
C----------------------------------------------------------------------
C link to coordinates
      JSCOO = IW(NASCOO)
      IF (JSCOO .EQ. 0)                                     GOTO 901
      NCOOR = LROWS(JSCOO)
C create the SACO bank
      CALL AUBOS('SACO',0,LMHLEN+2*NCOOR*LSACOA,JSACO,IGARB)
      IF (JSACO .EQ. 0 .OR. IGARB .EQ. 2)                   GOTO 902
      IF (IGARB .EQ. 1) JSCOO = IW(NASCOO)
      IW(JSACO+LMHCOL) = LSACOA
      IW(JSACO+LMHROW) = 2*NCOOR
C top of loop over coordinates
      DO 310 ICO = 1, NCOOR
        ISIDE = ITABL(JSCOO,ICO,JSCOSI)
        ILAYE = ITABL(JSCOO,ICO,JSCOLA)
        ISECT = ITABL(JSCOO,ICO,JSCOSC)
        PHSEC = PHIXXX(ILAYE,ISECT)
        Z0 = ZZERSG - ZOFFSG - REAL(MLAYSG-ILAYE)*ZDELSG
        X1UNC(1) = 0.
        X1UNC(2) = 0.
        IF (ISIDE .EQ. 1) THEN
          X1UNC(3) = Z0
        ELSE
          X1UNC(3) = -Z0
        ENDIF
C get module number and reference z for rotation
        IMOD = 1
        IF (ISECT .GE. 5) IMOD = 2
        ZREF = ZZERSG
        IF (ISIDE .EQ. 2) THEN
          IMOD = IMOD + 2
          ZREF = - ZREF
        ENDIF
C alignment correction for X1
        X1COR(1) = X1UNC(1) + XSHISA(IMOD) - XYANSA(IMOD)*X1UNC(2) +
     +               ZXANSA(IMOD)*(X1UNC(3)-ZREF)
        X1COR(2) = X1UNC(2) + YSHISA(IMOD) - YZANSA(IMOD)*
     +               (X1UNC(3)-ZREF) + XYANSA(IMOD)*X1UNC(1)
        X1COR(3) = (X1UNC(3)-ZREF) + ZSHISA(IMOD) -
     +               ZXANSA(IMOD)*X1UNC(1) + YZANSA(IMOD)*X1UNC(2)
C magnetic field correction
        DPHI = DPHBSR * X1COR(3)
        SDP  = SIN(DPHI)
        CDP  = COS(DPHI)
C correct X1 for the influence of the magnetic field
        X1BCO(1) =   X1COR(1) * CDP + X1COR(2) * SDP
        X1BCO(2) = - X1COR(1) * SDP + X1COR(2) * CDP
        X1BCO(3) =   X1COR(3)
C top of loop over theta
        DO 300 ITH = 0, 1
          TANTH = RTABL(JSCOO,ICO,JSCOTT+ITH)
          RLOCL = TANTH * Z0
C get X2 in SATR system
          X2UNC(1) = RLOCL * COS(PHSEC)
          X2UNC(2) = RLOCL * SIN(PHSEC)
          X2UNC(3) = X1UNC(3)
C alignment correction for X2
          X2COR(1) = X2UNC(1) + XSHISA(IMOD) - XYANSA(IMOD)*X2UNC(2) +
     +                 ZXANSA(IMOD)*(X2UNC(3)-ZREF)
          X2COR(2) = X2UNC(2) + YSHISA(IMOD) - YZANSA(IMOD)*
     +                 (X2UNC(3)-ZREF) + XYANSA(IMOD)*X2UNC(1)
          X2COR(3) = (X2UNC(3)-ZREF) + ZSHISA(IMOD) -
     +                 ZXANSA(IMOD)*X2UNC(1) + YZANSA(IMOD)*X2UNC(2)
C correct X2 for the influence of the magnetic field
          X2BCO(1) =   X2COR(1) * CDP + X2COR(2) * SDP
          X2BCO(2) = - X2COR(1) * SDP + X2COR(2) * CDP
          X2BCO(3) =   X2COR(3)
C get X3 in SATR system
          X3UNC(1) = RLOCL * (COS(PHSEC) - SIN(PHSEC))
          X3UNC(2) = RLOCL * (SIN(PHSEC) + COS(PHSEC))
          X3UNC(3) = X1UNC(3)
C alignment correction for X3
          X3COR(1) = X3UNC(1) + XSHISA(IMOD) - XYANSA(IMOD)*X3UNC(2) +
     +                 ZXANSA(IMOD)*(X3UNC(3)-ZREF)
          X3COR(2) = X3UNC(2) + YSHISA(IMOD) - YZANSA(IMOD)*
     +                 (X3UNC(3)-ZREF) + XYANSA(IMOD)*X3UNC(1)
          X3COR(3) = (X3UNC(3)-ZREF) + ZSHISA(IMOD) -
     +                 ZXANSA(IMOD)*X3UNC(1) + YZANSA(IMOD)*X3UNC(2)
C correct X3 for the influence of the magnetic field
          X3BCO(1) =   X3COR(1) * CDP + X3COR(2) * SDP
          X3BCO(2) = - X3COR(1) * SDP + X3COR(2) * CDP
          X3BCO(3) =   X3COR(3)
C fill rows in SACO
          KSACO = KROW(JSACO,ICO*2+ITH-1)
          RW(KSACO+JSACRA)   = RLOCL
          RW(KSACO+JSACXF)   = X1COR(1)
          RW(KSACO+JSACXF+1) = X1COR(2)
          RW(KSACO+JSACXF+2) = X1COR(3)
          RW(KSACO+JSACER)   = (X2COR(1)-X1COR(1)) / RLOCL
          RW(KSACO+JSACER+1) = (X2COR(2)-X1COR(2)) / RLOCL
          RW(KSACO+JSACER+2) = (X2COR(3)-X1COR(3)) / RLOCL
          RW(KSACO+JSACXW)   = X3COR(1) - X2COR(1)
          RW(KSACO+JSACXW+1) = X3COR(2) - X2COR(2)
          RW(KSACO+JSACXW+2) = X3COR(3) - X2COR(3)
          RW(KSACO+JSACBF)   = X1BCO(1)
          RW(KSACO+JSACBF+1) = X1BCO(2)
          RW(KSACO+JSACBF+2) = X1BCO(3)
          RW(KSACO+JSACBR)   = (X2BCO(1)-X1BCO(1)) / RLOCL
          RW(KSACO+JSACBR+1) = (X2BCO(2)-X1BCO(2)) / RLOCL
          RW(KSACO+JSACBR+2) = (X2BCO(3)-X1BCO(3)) / RLOCL
          RW(KSACO+JSACBW)   = X3BCO(1) - X2BCO(1)
          RW(KSACO+JSACBW+1) = X3BCO(2) - X2BCO(2)
          RW(KSACO+JSACBW+2) = X3BCO(3) - X2BCO(3)
C bottom of loop over theta
  300   CONTINUE
C bottom of loop over coordinates
  310 CONTINUE
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      GOTO 999
  902 IER = 2
      CALL RERROR('SACOCR',-IER,'No space for new bank SACO')
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
