*DK tpgeth
      SUBROUTINE TPGETH(ITE)
C ----------------------------------------------------------------------
C. - M.MERMIKIDES - 851206
C. - Compute intersections(hits) of charged track element ITE
C.   with padrows
C. - Banks created:
C                 TPHT  Intersections with padrows
C                 TPHE  References to track elements
C.
C. -Called from   TPHIT
C. -Calls         ALBOS
C.                BKFMT,BLIST
C -------------------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "jobcom.h"
#include "iocom.h"
#include "alcons.h"
#include "trkcom.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tpnamc.h"
      DIMENSION V1(6),V2(6),XINT(6),PZINT(4)
C
#include "bmacro.h"
C
C -------------------------------------------------------------
C
C  Create TPHT and TPHE (optionally) banks on entry
C
      ITPHT = IW(NATPHT)
      IF(ITPHT.EQ.0) THEN
         CALL ALBOS ('TPHT',0,LBHT,ITPHT,IGARB)
         CALL BLIST(IW,'E+','TPHT')
         IW(ITPHT + 1) = NWPHT
         IW(ITPHT + 2) = 0
      ENDIF
C
      IF (ICTPJO(4).GT.0) THEN
         ITPHE = IW(NATPHE)
         IF(ITPHE.EQ.0) THEN
            CALL ALBOS ('TPHE',0,LBHE,ITPHE,IGARB)
C  If garbage collection occured, redefine index for 'TPHT'
            IF (IGARB.EQ.1)  ITPHT = IW(NATPHT)
            CALL BLIST(IW,'E+','TPHE')
            IW(ITPHE + 1) = NWPHE
            IW(ITPHE + 2) = 0
         ENDIF
      ENDIF
C
C  Find intersections with padrows between the 2 points
C
      R1 = SQRT(TRKELE(1)**2 + TRKELE(2)**2)
      R2 = SQRT(TRKNXT(1)**2 + TRKNXT(2)**2)
      IF(R1.GT.R2) THEN
         RS = R2
         R2 = R1
         R1 = RS
      ENDIF
C
      DO 205 IROW = 1,LTPDRO
C
         IF(TPPROW(IROW).LT.R1) GO TO 205
         IF(TPPROW(IROW).GT.R2) GO TO 205
C
C  Determine sector types and endplate according to radius
C  and volume number
C
         IF (IROW.LE.NTPDRW(1)) THEN
C  K-type
            IST = 0
            IF (ITRKEL(10).NE.2) IST = LTSECT/2
            ISL1 = IST + 1
            ISL2 = IST + LTSLOT/2
            IRS = IROW
         ELSE
C  M,W types
            IST = LTSLOT/2
            IF (ITRKEL(10).NE.2) IST = IST + LTSECT/2
            ISL1 = IST + 1
            ISL2 = IST + LTSLOT
            IRS = IROW - NTPDRW(1)
         ENDIF
C
C   Use GEANT3 routine with cubic model
C
         CALL GICYL(TPPROW(IROW),TRKELE(1),TRKNXT(1),
     *               0., TRKELE(11), 3, XINT,SINT,PZINT,IFLAG)
         IF (IFLAG.NE.1) GO TO 205
         PHIGL = PZINT(1)
         IF(PHIGL.LT.0.) PHIGL = PHIGL + TWOPI
C
         DO 19 ISLOT = ISL1,ISL2
            DPHI = TPHDIF(PHIGL,TPPHI0(ISLOT))
            IF (IENDTP(ISLOT).EQ.2) DPHI=-DPHI
            ITYP = ITPTYP(ISLOT)
            IF (ABS(DPHI).GT.TPDPHF(IRS,ITYP)) GO TO 19
            IPAD = INT((TPDPHW(IRS,ITYP)-DPHI)/TPDPHS(IRS,ITYP))+2
            GO TO 300
19       CONTINUE
C
         GO TO 205
C
C  Enter hit in BOS bank
C
300      CONTINUE
C  If no room to store next hit, increase size of bank
         IF (LFRWRD(ITPHT) .LT. LCOLS(ITPHT)) THEN
            NDATA = IW(ITPHT) + LBHTX
            CALL ALBOS ('TPHT',0,NDATA,ITPHT,IGARB)
            IF (IGARB.EQ.1) ITPHE = IW(NATPHE)
         ENDIF
C  KHT1 = Address of next free row
         KHT1 = KNEXT(ITPHT)
         IW(KHT1 + 1) = ITRKEL(1)
         IW(KHT1 + 2) = IROW*100000 + ISLOT*1000 + IPAD
         RW(KHT1 + 3) = PHIGL
         RW(KHT1 + 4) = PZINT(2)
         RW(KHT1 + 5) = PZINT(3)
         RW(KHT1 + 6) = PZINT(4)
         IW(ITPHT + 2) = IW(ITPHT + 2) + 1
C
C  Update TPHE bank if track element ref. is given
C
         IF (ITE.EQ.0) GO TO 205
C  If no room to store next hit, increase size of bank
         IF (LFRWRD(ITPHE) .LT. LCOLS(ITPHE)) THEN
            NDATA = IW(ITPHE) + LBHEX
            CALL ALBOS ('TPHE',0,NDATA,ITPHE,IGARB)
            IF (IGARB.EQ.1) ITPHT = IW(NATPHT)
         ENDIF
C  KHE1 = Address of next free row
         KHE1 = KNEXT (ITPHE)
         IW(KHE1 + 1) = ITE
         IW(ITPHE + 2) = IW(ITPHE + 2) + 1
205   CONTINUE
999   RETURN
      END
#endif
