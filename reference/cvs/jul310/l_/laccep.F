      SUBROUTINE LACCEP(METH,IADDR,REJFL)
C----------------------------------------------------------------------
C!  - Tests whether an Lcal cluster is inside or outside a region
C!
C!   Author   :- Peter H. Hansen        1-APR-1989
C!   Modified :- P.H.Hansen            18-DEC-1989
C!   Inputs: IADDR            Address of central tower
C!        -  METH             Method number
C!        -  'LSDA'           Storey data
C!        -  'LMAP'           Address-to-Storey data Look-up Table
C!        -  'LACC'           Table of boundary towers
C!
C!   Outputs: REJFL        Logical reject flag
C!        -   IADDR        Nonet centroid tower
C!
C!   Libraries required: CERNLIB, BOS
C!
C!   Description
C!   ===========
C!   The routine tests whether an Lcal cluster is inside
C!   or outside a region bounded by pad boundaries.
C!   It is required that the triplet of first storeys on the
C!   good side of the boundary contains more energy than
C!   the triplet on the bad side.
C!   Depending on the method number, one of two regions
C!   are considered: one leaving a margin of at least
C!   half a pad width to the acceptance edge (METH=5,8), and
C!   one separated by more than a full pad from the edge (6,9).
C!   Methods 8 and 9 checks whether the nonet centroid is
C!   inside the METH 5 (6) acceptance table.
C!   If METH=7, we require instead that the most energetic
C!   tower in the cluster is inside the Method 5 boundary.
C?
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "lcnamc.h"
#include "lsdajj.h"
#include "lwbank.h"
#include "lmapjj.h"
#include "laccjj.h"
        DIMENSION ETRIP(6),LTRIP(3,6)
        LOGICAL REJFL
        DATA LTRIP/-1,0,1,
     &           -16,0,16,
     &           -17,-1,15,
     &           -15,1,17,
     &           -17,-16,-15,
     &           15,16,17/
#include "bmacro.h"
#include "lcmacr.h"
C----------------------------------------------------------------------
C
      REJFL = .TRUE.
      KLSDA = IW(NALSDA)
      KLACC = IW(NALACC)
      KLMAP = IW(NALMAP)
      IF(KLSDA.LE.0.OR.KLACC.LE.0.OR.KLMAP.LE.0)        GOTO 999
C?
C? Choose fiducial region depending on method number -
C? specified by the leftmost and rightmost accepted column
C? and the highest and lowest accepted row
C? for each column of towers (i.e. each row in LACC).
      IF(METH.EQ.5.OR.METH.EQ.7.OR.METH.EQ.8) THEN
        JLR = JLACLR
        JLO = JLACLO
        JHI = JLACHI
      ELSEIF(METH.EQ.6.OR.METH.GE.9) THEN
        JLR = JLACLR+1
        JLO = JLACLO+1
        JHI = JLACHI+1
      ELSE
        GOTO 999
      ENDIF
C?
C? Give up-down symmetric rows the same number.
      MODU = LCMOD(IADDR)
      IROW = LCROW(IADDR)
      ICOL = LCCOL(IADDR)
      IF(IROW.GE.16) THEN
        IROW = IROW-15
      ELSE
        IROW = 16-IROW
      ENDIF
C?
C? Method 7: Require most energetic tower inside acceptance
C?
      IMXFID = LFIDUC(IROW,ICOL,METH)
      IF(METH.EQ.7) THEN
        IF(IMXFID.NE.5)                                 GOTO 999
        REJFL = .FALSE.
        GOTO 999
      ENDIF
C?
C? Find the energies in each triplet of tower rows and
C? tower columns in the central tower nonet.
      CALL VZERO(ETRIP,6)
      DO 11 ITRIP=1,6
        DO 10 I=1,3
          IA = IADDR+LTRIP(I,ITRIP)
          IF(IA.LE.0.OR.IA.GT.LLMAP) GOTO 10
          IT = IW(KLMAP+LMHLEN+IA)
          IF(IT.GT.0) THEN
            ETRIP(ITRIP) = ETRIP(ITRIP) + RTABL(KLSDA,IT,JLSDEN)
     &                                  + RTABL(KLSDA,IT,JLSDEN+1)
          ENDIF
   10   CONTINUE
   11 CONTINUE
C?
C? Interchange above and below triplets for Y<0
      IF(LCROW(IADDR).LE.15) THEN
        E5 = ETRIP(5)
        ETRIP(5) = ETRIP(6)
        ETRIP(6) = E5
      ENDIF
C
C Determine tower containing the triplet centroid (intersection of horizontal
C and vertical triplets with maximal energy).
C Assume it is the maximal tower to start with.
      ITRROW = IROW
      IF (ETRIP(6).GT.ETRIP(1)) ITRROW = ITRROW + 1
      IF (ETRIP(5).GT.ETRIP(1)) ITRROW = ITRROW - 1
      IF (ITRROW.LE.1)  ITRROW = 1
      IF (ITRROW.GE.15) ITRROW = 15
C
      ITRCOL = ICOL
      IF (ETRIP(4).GT.ETRIP(2)) ITRCOL = ITRCOL + 1
      IF (ETRIP(3).GT.ETRIP(2)) ITRCOL = ITRCOL - 1
      IF (ITRCOL.LE.1)  ITRCOL = 1
      IF (ITRCOL.GE.16) ITRCOL = 16
C
C?
C? Method 8 and 9: triplet centroid determined in tower address coordinates
C? and checked against M5 and M6 acceptance regions.
C?
      IF (METH.GE.8) THEN
        ITRACC = LFIDUC(ITRROW,ITRCOL,METH)
        IF (METH.EQ.8.AND.ITRACC.EQ.5) REJFL = .FALSE.
        IF (METH.GE.9.AND.ITRACC.EQ.6) REJFL = .FALSE.
C
C Replace the "maximal tower" by the "nonet centroid".
        IF(LCROW(IADDR).GE.16) THEN
          IR = ITRROW+15
        ELSE
          IR = 16-ITRROW
        ENDIF
        IADDR = (MODU-1)*512+(IR-1)*16+ITRCOL
C
        GOTO 999
      ENDIF
C?
C? Method 5 and 6: Reject clusters on the border pads with the wrong
C? energy sharing across the border.
C?
C? Exclude events with IROW just above low border and E(IR)-E(IR-1)<0
      IF(IROW.EQ.ITABL(KLACC,ICOL,JLO)) THEN
        IF(ETRIP(1).LT.ETRIP(5))                      GOTO 999
C? Exclude events with IROW just below low border and E(IR+1)-E(IR)<0
      ELSEIF(IROW.EQ.ITABL(KLACC,ICOL,JLO)-1) THEN
        IF(ETRIP(6).LT.ETRIP(1))                      GOTO 999
C? Exclude events with IROW more than one row below low border
      ELSEIF(IROW.LT.ITABL(KLACC,ICOL,JLO)-1) THEN
        GOTO 999
      ENDIF
C?
C? Exclude events with IROW just below high border and E(IR)-E(IR+1)<0
      IF(IROW.EQ.ITABL(KLACC,ICOL,JHI)) THEN
        IF(ETRIP(1).LT.ETRIP(6))                      GOTO 999
C? Exclude events with IROW just above high border and E(IR-1)-E(IR)<0
      ELSEIF(IROW.EQ.ITABL(KLACC,ICOL,JHI)+1) THEN
        IF(ETRIP(5).LT.ETRIP(1))                      GOTO 999
C? Exclude events with IROW more than one row above high border
      ELSEIF(IROW.GT.ITABL(KLACC,ICOL,JHI)+1) THEN
        GOTO 999
      ENDIF
C?
C? Exclude events with ICOL just right of left border and E(IC)-E(IC-1)<0
      IF(ITABL(KLACC,ICOL,JLR).EQ.1) THEN
        IF(ETRIP(2).LT.ETRIP(3))                      GOTO 999
C? Exclude events with ICOL just left of left border and E(IC+1)-E(IC)<0
      ELSEIF(ICOL.LE.15.AND.ITABL(KLACC,ICOL+1,JLR).EQ.1) THEN
        IF(ETRIP(4).LT.ETRIP(2))                      GOTO 999
C? Exclude events with ICOL just left of right border and E(IC)-E(IC+1)<
      ELSEIF(ITABL(KLACC,ICOL,JLR).EQ.2) THEN
        IF(ETRIP(2).LT.ETRIP(4))                      GOTO 999
C? Exclude events with ICOL just right of right border and E(IC-1)-E(IC)
      ELSEIF(ICOL.GE.2.AND.ITABL(KLACC,ICOL-1,JLR).EQ.2) THEN
        IF(ETRIP(3).LT.ETRIP(2))                      GOTO 999
      ENDIF
C?
C? Exclude events with IROW below low border in previous column
C? and E(IC)-E(IC-1)<0
      IF(ICOL.GE.2.AND.IROW.LT.ITABL(KLACC,ICOL-1,JLO)) THEN
        IF(ETRIP(2).LT.ETRIP(3))                    GOTO 999
      ENDIF
C? Exclude events with IROW above high border in next column
C? and E(IC)-E(IC-1)<0
      IF(ICOL.LE.15.AND.IROW.GT.ITABL(KLACC,ICOL+1,JHI)) THEN
        IF(ETRIP(2).LT.ETRIP(4))                    GOTO 999
      ENDIF
C
      REJFL = .FALSE.
  999 CONTINUE
      END
      INTEGER FUNCTION LFIDUC(IROW,ICOL,METH)
C--------------------------------------------------------------
C! Return fiducial acceptance method number for a given tower
C!   Author :      J.Wear - 901029
C!   Input  : IROW,ICOL = Nonet tower position
C!            METH      = method number
C!   Output : LFIDUC    = 0 (nonexistant tower)
C!                      = 1 (outside acceptance by >1 tower)
C!                      = 2 (border tower to this method)
C!                      = 5 (method 5,7,8 acceptance)
C!                      = 6 (method 6,9 acceptance)
C--------------------------------------------------------------
      DIMENSION LFID5(15,16),LFID6(15,16)
      DATA LFID5 / 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 1, 1, 0,
     +             0, 0, 0, 0, 2, 2, 5, 5, 5, 5, 5, 2, 1, 1, 0,
     +             0, 0, 0, 2, 2, 5, 5, 5, 5, 5, 5, 2, 1, 1, 1,
     +             2, 2, 2, 2, 5, 5, 5, 5, 5, 5, 2, 2, 1, 1, 1,
     +             5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 2, 1, 1, 1, 1,
     +             5, 5, 5, 5, 5, 5, 5, 5, 5, 2, 2, 1, 1, 1, 1,
     +             5, 5, 5, 5, 5, 5, 5, 5, 2, 2, 1, 1, 1, 1, 1,
     +             5, 5, 5, 5, 5, 5, 5, 2, 2, 1, 1, 1, 1, 1, 0,
     +             5, 5, 5, 5, 5, 5, 2, 2, 1, 1, 1, 1, 1, 1, 0,
     +             5, 5, 5, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 0,
     +             2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0,
     +             1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0,
     +             1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0,
     +             1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
     +             1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
     +             1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
C
      DATA LFID6 / 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 1, 1, 0,
     +             0, 0, 0, 0, 2, 2, 6, 6, 6, 6, 6, 2, 1, 1, 0,
     +             0, 0, 0, 1, 2, 6, 6, 6, 6, 6, 6, 2, 1, 1, 1,
     +             1, 1, 1, 2, 2, 6, 6, 6, 6, 6, 2, 2, 1, 1, 1,
     +             2, 2, 2, 2, 6, 6, 6, 6, 6, 6, 2, 1, 1, 1, 1,
     +             6, 6, 6, 6, 6, 6, 6, 6, 6, 2, 2, 1, 1, 1, 1,
     +             6, 6, 6, 6, 6, 6, 6, 6, 2, 2, 1, 1, 1, 1, 1,
     +             6, 6, 6, 6, 6, 6, 6, 2, 2, 1, 1, 1, 1, 1, 1,
     +             6, 6, 6, 6, 6, 6, 2, 2, 1, 1, 1, 1, 1, 1, 0,
     +             6, 6, 6, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 0,
     +             2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0,
     +             1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0,
     +             1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0,
     +             1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
     +             1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
     +             1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
C -------------------------------------------------------------
      LFIDUC = 0
      IF (IROW .LE. 0 .OR. IROW .GE. 16) GOTO 999
      IF (ICOL .LE. 0 .OR. ICOL .GT. 16) GOTO 999
C
C - Determine tower acceptance
      IF (METH.GE.9) LFIDUC = LFID6(IROW,ICOL)
      IF (METH.EQ.8.OR.METH.EQ.7) LFIDUC = LFID5(IROW,ICOL)
  999 RETURN
      END
#endif
