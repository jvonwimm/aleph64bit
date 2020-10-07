      SUBROUTINE LASYM(IADDR,AX,AY)
C----------------------------------------------------------------------
C!  - This routine finds the energy of the triplet of two first storeys
C!
C!   Author   :- Peter H. Hansen        1-APR-1990
C!   Modified :  H. Meinhard            10-Apr-1990
C!
C!   Inputs: IADDR            Address of central tower
C!        -  'LSDA'           Storey data
C!        -  'LMAP'           Address-to-Storey data Look-up Table
C!        -  'LACC'           Table of boundary towers
C!
C!   Outputs: AX,AY           Energy assymetries used for Method 5
C!                            Ax,AY > 0
C!        -
C!
C!   Libraries required: CERNLIB, BOS
C!
C!   Description
C!   ===========
C!   This routine finds the energy of the triplet of two first storeys
C!   on the good side of the boundary minus the energy of the triplet
C!   on the bad side.
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "lcnamc.h"
#include "lsdajj.h"
#include "laccjj.h"
#include "lmapjj.h"
#include "lwbank.h"
        DIMENSION ETRIP(6),LTRIP(3,6)
        DATA LTRIP/-1,0,1,
     &           -16,0,16,
     &           -17,-1,15,
     &           -15,1,17,
     &           -17,-16,-15,
     &           15,16,17/
#include "bmacro.h"
#include "lcmacr.h"
C-----------------------------------------------------------------------
C
      AX = 1.
      AY = 1.
      KLSDA = IW(NALSDA)
      IF(KLSDA.LE.0)                                    GOTO 999
      NTOW = LROWS(KLSDA)
C
C Create LMAP (tower map)
      KLMAP = IW(NALMAP)
      IF(KLMAP.LE.0) THEN
        LEN = LMHLEN + LLMAP
        CALL AUBOS('LMAP',0,LEN,KLMAP,IGARB)
        IF(IGARB.EQ.2)                                  GOTO 997
        IW(KLMAP+1) = 1
        IW(KLMAP+2) = LLMAP
        CALL VZERO(RW(KLMAP+LMHLEN+1),LLMAP)
        KLSDA = IW(NALSDA)
        DO 100 ITOW = 1,NTOW
          IA = ITABL(KLSDA,ITOW,JLSDAD)
          IW(KLMAP+LMHLEN+IA) = ITOW
  100   CONTINUE
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
C? Select Method 5
      KLACC = IW(NALACC)
      IF(KLACC.LE.0)                                    GOTO 999
      JLR = JLACLR
      JLO = JLACLO
      JHI = JLACHI
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
C?
C? Calculate energy sharing across the border.
C
C Events with IROW just above low border and
      IF(IROW.EQ.ITABL(KLACC,ICOL,JLO)) THEN
        AY = (ETRIP(1)-ETRIP(5))/AMAX1(ETRIP(1)+ETRIP(5),1.)
C Events with IROW just below low border
      ELSEIF(IROW.EQ.ITABL(KLACC,ICOL,JLO)-1) THEN
        AY = (ETRIP(6)-ETRIP(1))/AMAX1(ETRIP(6)+ETRIP(1),1.)
C Events with IROW more than one row below low border
      ELSEIF(IROW.LT.ITABL(KLACC,ICOL,JLO)-1) THEN
        AY = -1.
      ENDIF
C
C events with IROW just below high border and E(IR)-E(IR+1)<0
      IF(IROW.EQ.ITABL(KLACC,ICOL,JHI)) THEN
        AY = (ETRIP(1)-ETRIP(6))/AMAX1(ETRIP(1)+ETRIP(6),1.)
C events with IROW just above high border and E(IR-1)-E(IR)<0
      ELSEIF(IROW.EQ.ITABL(KLACC,ICOL,JHI)+1) THEN
        AY = (ETRIP(5)-ETRIP(1))/AMAX1(ETRIP(5)+ETRIP(1),1.)
C events with IROW more than one row above high border
      ELSEIF(IROW.GT.ITABL(KLACC,ICOL,JHI)+1) THEN
        AY = -1.
      ENDIF
C
C events with ICOL just right of left border
      IF(ITABL(KLACC,ICOL,JLR).EQ.1) THEN
        AX = (ETRIP(2)-ETRIP(3))/AMAX1(ETRIP(2)+ETRIP(3),1.)
C events with ICOL just left of left border
      ELSEIF(ICOL.LE.15.AND.ITABL(KLACC,ICOL+1,JLR).EQ.1) THEN
        AX = (ETRIP(4)-ETRIP(2))/AMAX1(ETRIP(4)+ETRIP(2),1.)
C events with ICOL just left of right border
      ELSEIF(ITABL(KLACC,ICOL,JLR).EQ.2) THEN
        AX = (ETRIP(2)-ETRIP(4))/AMAX1(ETRIP(2)+ETRIP(4),1.)
C events with ICOL just right of right border
      ELSEIF(ICOL.GE.2.AND.ITABL(KLACC,ICOL-1,JLR).EQ.2) THEN
        AX = (ETRIP(3)-ETRIP(2))/AMAX1(ETRIP(3)+ETRIP(2),1.)
      ELSEIF(ITABL(KLACC,ICOL,JLR).EQ.3) THEN
        AX = -1.
      ENDIF
C Col5:
C events with (IC=5,IR<=4)
      IF(ICOL.EQ.5.AND.IROW.LE.4) THEN
        AX = (ETRIP(2)-ETRIP(3))/AMAX1(ETRIP(2)+ETRIP(3),1.)
C events with (IC=4,IR<=4)
      ELSEIF(ICOL.EQ.4.AND.IROW.LE.4) THEN
        AX = (ETRIP(4)-ETRIP(2))/AMAX1(ETRIP(4)+ETRIP(2),1.)
      ENDIF
C
C events with IROW below low border in previous column
      IF(ICOL.GE.2.AND.IROW.LT.ITABL(KLACC,ICOL-1,JLO)) THEN
        AX = (ETRIP(2)-ETRIP(3))/AMAX1(ETRIP(2)+ETRIP(3),1.)
      ENDIF
C events with IROW above high border in next column
      IF(ICOL.LE.15.AND.IROW.GT.ITABL(KLACC,ICOL+1,JHI)) THEN
        AX = (ETRIP(2)-ETRIP(4))/AMAX1(ETRIP(2)+ETRIP(4),1.)
      ENDIF
      GOTO 999
C
  997 WRITE (6,'(A)') ' LASYM --> No room for bank LMAP'
  999 CONTINUE
      END
#endif
