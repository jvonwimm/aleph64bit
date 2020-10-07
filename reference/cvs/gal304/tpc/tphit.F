*DK tphit
      SUBROUTINE TPHIT
C---------------------------------------------------------------------
C  M.Mermikides                                       24/7/85
C
C  Steering for computation of track intersectons with padrows
C  using current track element parameters.
C -Track elements are saved (optionally) in BOS bank 'TPTE' in the form
C   (Track ID, x,y,z,dx/ds,dy/ds/dz/ds,p,s,TOF,m,Q)
C  for detailed simulation in TPDIGI. These are also useful for
C  display purposes.
C -First order pad hits (intersections) are computed in routine TPGETH
C -To avoid excessive numbers of track elements and hits, we stop
C  following low momentum tracks after a given path length)
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "jobcom.h"
#include "jqcom.h"
#include "iocom.h"
#include "trkcom.h"
#include "tpnamc.h"
C
#include "bmacro.h"
C
C --------------------------------------------------------------
C - if 1st entry of the event check BOS banks
C
      DATA PCUT/0.10/, MAXEL/20/
C
      IF (FBEGJO(3)) THEN
         CALL TPIBOS
         FBEGJO(3) = .FALSE.
      ENDIF
C
C  Skip if first entry or step length = 0.
C
      IF (ITRKEL(8).EQ.1.OR.TRKELE(11).EQ.0.0) THEN
         ITE = 0
         IKLST = ITRKEL(1)
         NEL = 0
         GO TO 999
      ENDIF
C
C For low momentum tracks skip storage of track elements and hits
C after MAXEL steps
C
      IF (IKLST.EQ.ITRKEL(1).AND.TRKELE(7).LT.PCUT) THEN
         NEL = NEL + 1
         IF(NEL.GT.MAXEL) GO TO 999
      ENDIF
C
      IF (ICTPJO(4).GT.0)  THEN
C
C  Save track segment parameters in 'TPTE' bank, if requested
C  Create 'TPTE' bank if it doesn't exist.
C
         ITPTE = IW(NATPTE)
         IF (ITPTE.EQ.0) THEN
            CALL ALBOS ('TPTE',0,LBTE,ITPTE,IGARB)
            CALL BLIST(IW,'E+','TPTE')
            IW(ITPTE + 1) = LTPTE
            IW(ITPTE + 2) = 0
         ENDIF
C  If no room to store next hit, increase size of bank
         IF(LFRWRD(ITPTE) .LT. LCOLS(ITPTE)) THEN
            NDATA = IW(ITPTE) + LBTEX
            CALL ALBOS ('TPTE',0,NDATA,ITPTE,IGARB)
         ENDIF
C  KTE1 = address of next row
         KTE1 = KNEXT(ITPTE)
         IW(KTE1 + 1)  = ITRKEL(1)
         CALL UCOPY(TRKELE(1), RW(KTE1 + 2), 7)
         RW(KTE1 +  9) = TRKELE(11)
         RW(KTE1 + 10) = TRKELE(10)
         RW(KTE1 + 11) = TRKELE(13)
         RW(KTE1 + 12) = TRKELE(14)
         IW(ITPTE + 2) = IW(ITPTE + 2) + 1
         ITE = IW(ITPTE + 2)
      ENDIF
C
C  Compute intersections of charged track segment with padrows
C
      IF (NINT(TRKELE(14)).NE.0)  THEN
         IF(ICTPJO(2).GT.0) CALL TPGETH(ITE)
         IF(ICTPJO(3).GT.0) CALL TTGETH(ITE)
      ENDIF
C
999   RETURN
      END
#endif
