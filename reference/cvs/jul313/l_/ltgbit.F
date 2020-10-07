      SUBROUTINE LTGBIT(IRUN,TLTYP,LTBIT,LTORW,NTBIT,IFAIL)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :-  J.Wear               23-MAR-1990
C!   Mods     :-  J.Wear               20-JUN-1990
C!                       CRAY compatible, Hardwired all lumi-bits for
C!                       protection -- this should go to a database and
C!                       forget reading the trigger banks.
C!   Mods     :-  P.H.Hansen           01-MAY-1996
C!                       New hardwired periods
C!
C!   Inputs: IRUN -- run number
C!          TLTYP -- Character string describing a LCAL trigger type
C!                   The five Luminosity trigger types are
C!                   LO_SING, HI_SING, VH_SING, LO_COIN, HI_COIN .
C!
C!   Outputs: LTBIT(5) :contains bit assignments of TLTYP trigger type
C!        -   LTORW(5) :contains source of trigger
C!        -             (Tower=0, Wire=1, SATR=2)
C!            NTBIT    :contains the number of bits assigned to TLTYP
C!            IFAIL    : = 1, if XTBN is corrupt, use hardwired values
C!                       = 2, if incorrect input
C!
C!   Description  There are five basic types of LCAL triggers whose
C!   ===========  composition and names change from run period to run
C!                period.  This routine maps the types of the triggers
C!   to the corresponding bits in the trigger mask (returns up to five
C!   separate bits for a trigger type to cover redundant triggers, extra
C!   values have bit settings of -1).  It also checks the actual trigger
C!   name to decide whether this is tower-, wire-, or SATR-based.
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "xtbnjj.h"
C
      PARAMETER (JSEP89=1,JFAL89=2,JMAR90=3,JMAR92=4,JMAY93=5,
     &           JNOV95=6,NPER=6)
      PARAMETER (NLTYP=5,NLBIT=5)
      CHARACTER*4 CHAINT
      CHARACTER*8 TLTYP,TLIST(5),TLNAM(NLTYP,NPER,NLBIT),MNM
      DIMENSION LTBIT(NLBIT),LTORW(NLBIT),ICH(2)
      DIMENSION LDFBT(NLBIT,NLTYP,NPER),LDFTW(NLBIT,NLTYP,NPER),
     &          NDFBT(NLTYP,NPER)
      LOGICAL FIRST
      SAVE FIRST
      EXTERNAL NAMIND
C
      DATA FIRST /.TRUE./
      DATA TLIST /'LO_SING','HI_SING','VH_SING','LO_COIN','HI_COIN' /
      DATA LDFBT/   4,5,3*0,  6,7,3*0, 22,23,3*0,   3,4*0, 14,15,3*0,
     &              4,5,3*0,  6,7,3*0,     0,4*0,   3,4*0, 14,15,3*0,
     &                3,4*0,    4,4*0,  5,7,3*0, 1,6,3*0, 14,15,16,0,0,
     &                7,4*0,    7,4*0,   22,4*0,   6,4*0, 14,15,16,0,0,
     &                0,4*0,    0,4*0,    7,4*0,   0,4*0,  6,4*0,
     &                6,4*0,    0,4*0,    7,4*0,   0,4*0,  5,4*0/
      DATA LDFTW/      25*0,
     &                 25*0,
     &                  5*0,      5*0,  0,1,3*0,  2,1,3*0, 0,0,1,0,0,
     &                 1,4*0,    1,4*0,  1,4*0,    1,4*0,  0,0,1,0,0,
     &                 0,4*0,    0,4*0,  1,4*0,    0,4*0,  1,0,0,0,0,
     &                 1,4*0,    0,4*0,  1,4*0,    0,4*0,  1,0,0,0,0/
      DATA NDFBT/         2,        2,        2,       1,         2,
     &                    2,        2,        1,       1,         2,
     &                    1,        1,        2,       2,         3,
     &                    1,        1,        1,       1,         3,
     &                    0,        0,        1,       0,         1,
     &                    1,        0,        1,       0,         1/
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        DO 90 ILTYP = 1,NLTYP
          DO 91 IPER = 1,NPER
            DO 92 ILBIT = 1,NLBIT
 92           TLNAM(ILTYP,IPER,ILBIT) = ' '
 91       CONTINUE
 90     CONTINUE
C
C Sept `89 and Fall `89 triggers
        DO 10 IPER = JSEP89,JFAL89
          TLNAM(1,IPER,1) = 'LC_A_LOW'
          TLNAM(1,IPER,2) = 'LC_B_LOW'
          TLNAM(2,IPER,1) = 'LC_A_HIG'
          TLNAM(2,IPER,2) = 'LC_B_HIG'
          TLNAM(4,IPER,1) = 'LC_LO_LO'
          TLNAM(5,IPER,1) = 'LC_LO_HI'
          TLNAM(5,IPER,2) = 'LC_HI_LO'
 10     CONTINUE
        TLNAM(3,JSEP89,1) = 'LC_A_VHI'
        TLNAM(3,JSEP89,2) = 'LC_B_VHI'
        TLNAM(3,JFAL89,1) = 'LC_VHIGH'
C
C March `90 triggers
        TLNAM(1,JMAR90,1) = 'LT_A+BLO'
        TLNAM(2,JMAR90,1) = 'LT_A+BME'
        TLNAM(3,JMAR90,1) = 'LT_A+BVH'
        TLNAM(3,JMAR90,2) = 'LW_A+BVH'
        TLNAM(4,JMAR90,1) = 'LS_A*B99'
        TLNAM(4,JMAR90,2) = 'LW_ET_LO'
        TLNAM(5,JMAR90,1) = 'LT_LO_HI'
        TLNAM(5,JMAR90,2) = 'LT_HI_LO'
        TLNAM(5,JMAR90,3) = 'LW_ET_HI'
C
C `92 triggers
        TLNAM(1,JMAR92,1) = 'LW_A+BLO'
        TLNAM(2,JMAR92,1) = 'LW_A+BLO'
        TLNAM(3,JMAR92,1) = 'LW_A+BVH'
        TLNAM(4,JMAR92,1) = 'LW_ET_LO'
        TLNAM(5,JMAR92,1) = 'LT_LO_HI'
        TLNAM(5,JMAR92,2) = 'LT_HI_LO'
        TLNAM(5,JMAR92,3) = 'LW_ET_HI'
C
C `93 triggers
        TLNAM(3,JMAY93,1) = 'LW_A+BVH'
        TLNAM(5,JMAY93,1) = 'LW_ET_HI'
C
C `96 triggers
        TLNAM(2,JNOV95,1) = 'LW_A+BLO'
        TLNAM(3,JNOV95,1) = 'LW_A+BVH'
        TLNAM(5,JNOV95,1) = 'LW_LO_HI'
      ENDIF
C
C Decide on the run period, initialize output arrays, find trigger type.
      IFAIL = 0
      NTBIT = 0
      DO 11 ILBIT = 1,NLBIT
 11     LTBIT(ILBIT) = -1
      CALL VZERO(LTORW,NLBIT)
      IF (IRUN.LE.4535) THEN
        KPER = JSEP89
       ELSEIF ((IRUN.GT.4535).AND.(IRUN.LE.5909)) THEN
        KPER = JFAL89
       ELSEIF (IRUN.LT.14000) THEN
        KPER = JMAR90
       ELSEIF (IRUN.LT.20000) THEN
        KPER = JMAR92
       ELSEIF (IRUN.LT.40100) THEN
        KPER = JMAY93
       ELSE
        KPER = JNOV95
      ENDIF
C
      JLTYP = 0
      DO 100 ILTYP = 1,NLTYP
 100    IF (TLTYP.EQ.TLIST(ILTYP)) JLTYP = ILTYP
      IF (JLTYP.EQ.0) THEN
        IFAIL = 2
        GOTO 999
      ENDIF
C
C Go to the run header/database for trigger names
      NAXTBN=NAMIND('XTBN')
      KXTBN=IW(NAXTBN)
      IF(KXTBN.NE.0)THEN
        NBIT=LROWS(KXTBN)
        DO 101 ILBIT = 1,NLBIT
           IF (TLNAM(JLTYP,KPER,ILBIT).NE.' ') THEN
             DO 110 ITBIT = 1,NBIT
               IDEF=ITABL(KXTBN,ITBIT,JXTBTN)
               INUM=ITABL(KXTBN,ITBIT,JXTBTB)
C
C XTBN is corrupted so we perform some major league hardwiring fixes
               IF(INUM.LT.0.OR.INUM.GT.31) IFAIL = 1
C
C Look at trigger type and return bit assignments and tower/wire info.
               IF(IDEF.NE.0.AND.INUM.NE.31)THEN
                 ICH(1) =ITABL(KXTBN,ITBIT,JXTBBM)
                 ICH(2) =ITABL(KXTBN,ITBIT,JXTBBM+1)
                 MNM(1:4) = CHAINT(ICH(1))
                 MNM(5:8) = CHAINT(ICH(2))
                 IF(MNM.EQ.TLNAM(JLTYP,KPER,ILBIT)) THEN
                   LTBIT(ILBIT) = INUM
                   IF (MNM(2:2).EQ.'W') LTORW(ILBIT) = 1
                   IF (MNM(2:2).EQ.'S') LTORW(ILBIT) = 2
                   NTBIT = NTBIT + 1
                 ENDIF
               ENDIF
 110         CONTINUE
           ENDIF
 101    CONTINUE
       ELSE
        IFAIL = 1
      ENDIF
C
C In case of error, use hardwired bit assignments should go to database!
  888 IF (IFAIL.EQ.1) THEN
        DO 889 ILBIT = 1,NLBIT
          LTBIT(ILBIT) = LDFBT(ILBIT,JLTYP,KPER)
          LTORW(ILBIT) = LDFTW(ILBIT,JLTYP,KPER)
  889   CONTINUE
        NTBIT = NDFBT(JLTYP,KPER)
      ENDIF
C
  999 RETURN
      END
#endif
