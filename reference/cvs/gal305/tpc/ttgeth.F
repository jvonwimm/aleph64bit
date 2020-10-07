*DK ttgeth
      SUBROUTINE TTGETH(ITE)
C ----------------------------------------------------------------------
C. - M.MERMIKIDES - 851206
C. - Compute intersections(hits) of charged track element ITE
C.   with trigger pad rows
C. - Banks created:
C                 TTHT  Intersections with padrows
C                 TTHE  References to track elements
C.
C. -Called from   TTHIT
C. -Calls         ALBOS
C.                BKFMT,BLIST
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "jobcom.h"
#include "iocom.h"
#include "alcons.h"
#include "trkcom.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeot.h"
#include "tpnamc.h"
      DIMENSION XINT(6),PZINT(4)
C
#include "bmacro.h"
C
C ------------------------------------------------------------
C  Create TTHT and TTHE banks on entry
C
      ITTHT = IW(NATTHT)
      IF(ITTHT.EQ.0) THEN
         CALL ALBOS ('TTHT',0,LBHT,ITTHT,IGARB)
         CALL BLIST(IW,'E+','TTHT')
         IW(ITTHT + 1) = NWPHT
         IW(ITTHT + 2) = 0
      ENDIF
C
      IF (ICTPJO(4).GT.0) THEN
         ITTHE = IW(NATTHE)
         IF(ITTHE.EQ.0) THEN
            CALL ALBOS ('TTHE',0,LBHE,ITTHE,IGARB)
C  If garbage collection occured, redefine index for 'TTHT'
            IF (IGARB.EQ.1)  ITTHT = IW(NATTHT)
            CALL BLIST(IW,'E+','TTHE')
            IW(ITTHE + 1) = NWPHE
            IW(ITTHE + 2) = 0
         ENDIF
      ENDIF
C  Get intersections with wire (cylindrical) layers
      R1 = SQRT(TRKELE(1)**2 + TRKELE(2)**2)
      R2 = SQRT(TRKNXT(1)**2 + TRKNXT(2)**2)
      IF(R1.GT.R2) THEN
         RS = R2
         R2 = R1
         R1 = RS
      ENDIF
C
      DO 205 IROW = 1,LTTROW
C
         IF(TPTROW(IROW).LT.R1) GO TO 205
         IF(TPTROW(IROW).GT.R2) GO TO 205
C
C  Determine sector types and endplate according to radius
C  and volume number
C
         IF (IROW.LE.NTPTRW(1)) THEN
C  K-type
            IST = 0
            IF (ITRKEL(10).NE.2) IST = LTSECT/2
            ISL1 = IST + 1
            ISL2 = IST + LTSLOT/2
            IRS = IROW
         ELSE
C  M,W-types
            IST = LTSLOT/2
            IF (ITRKEL(10).NE.2) IST = IST + LTSECT/2
            ISL1 = IST + 1
            ISL2 = IST + LTSLOT
            IRS = IROW - NTPTRW(1)
         ENDIF
C
C   Use GEANT3 routine with cubic model
C
         CALL GICYL(TPTROW(IROW),TRKELE(1),TRKNXT(1),
     *              0., TRKELE(11),3,XINT,SINT,PZINT,IFLAG)
         IF (IFLAG.NE.1) GO TO 205
         PHIGL = PZINT(1)
         IF(PHIGL.LT.0.) PHIGL = PHIGL + TWOPI
C
         DO 19 ISLOT = ISL1,ISL2
            DPHI = TPHDIF(TPPHI0(ISLOT),PHIGL)
            ITYP = ITPTYP(ISLOT)
            PHI1 = TPTPHC(1,IRS,ITYP)-TPTPHW(1,IRS,ITYP)
            NP = NTPTPR(IRS,ITYP)
            PHI2 = TPTPHC(NP,IRS,ITYP)+TPTPHW(NP,IRS,ITYP)
            IF (DPHI.LT.PHI1.OR.DPHI.GT.PHI2) GO TO 19
            DO 119 IPAD = 1,NP
               DP = TPHDIF(DPHI,TPTPHC(IPAD,IRS,ITYP))
               IF(ABS(DP).GT.TPTPHW(IPAD,IRS,ITYP)) GO TO 119
               GO TO 300
119         CONTINUE
19       CONTINUE
         GO TO 205
C
C  Enter hit in BOS bank
C
300      CONTINUE
C  If no room to store next hit, increase size of bank
         IF(LFRWRD(ITTHT) .LT. LCOLS(ITTHT)) THEN
            NDATA = IW(ITTHT) + LBHTX
            CALL ALBOS ('TTHT',0,NDATA,ITTHT,IGARB)
            IF (IGARB.EQ.1) ITTHE = IW(NATTHE)
         ENDIF
C   KHT1 = Address of next free row
         KHT1 = KNEXT(ITTHT)
         IW(KHT1 + 1) = ITRKEL(1)
         IW(KHT1 + 2) = IROW*100000 + ISLOT*1000 + IPAD
         RW(KHT1 + 3) = PHIGL
         RW(KHT1 + 4) = PZINT(2)
         RW(KHT1 + 5) = PZINT(3)
         RW(KHT1 + 6) = PZINT(4)
         IW(ITTHT + 2) = IW(ITTHT + 2) + 1
C
C  Update TTHE bank if track element ref. is given
C
         IF (ITE.EQ.0) GO TO 205
C  If no room to store next hit, increase size of bank
         IF(LFRWRD(ITTHE) .LT. LCOLS(ITTHE)) THEN
            NDATA = IW(ITTHE) + LBHEX
            CALL ALBOS ('TTHE',0,NDATA,ITTHE,IGARB)
            IF (IGARB.EQ.1) ITTHT = IW(NATTHT)
         ENDIF
C   KHE1 = Address of next free row
         KHE1 = KNEXT(ITTHE)
         IW(KHE1 + 1) = ITE
         IW(ITTHE + 2) = IW(ITTHE + 2) + 1
205   CONTINUE
C
      GO TO 999
999   RETURN
      END
#endif
