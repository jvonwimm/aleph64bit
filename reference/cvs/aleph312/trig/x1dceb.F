      SUBROUTINE X1DCEB(IXTEB,IXTDI)
C----------------------------------------------------------------------
C!  - Decode bank XTEB and fill bank XTDI
C.
C.   Author   :- Rainer Geiges          4-AUG-1989
C.               Alois Putzer           4-AUG-1989
C.               Martin Wunsch          4-AUG-1989
C.               Yves A. Maumary       20-DEC-1989 Adapted for ALEPHLIB
C!   Modified :- Yves A. Maumary       15-MAR-1990 for '90 run
C.
C.   Inputs:
C.        - IXTEB : INTEGER : array corresponding to the bank XTEB
C.
C.   Outputs:
C.        - IXTDI : INTEGER : array corresponding to the bank XTDI
C.
C.   Libraries required: ALEPHLIB, CERNLIB
C.
C.   Calls: BTEST, IBSET, MVBITS, UCOPY, VZERO, X1ECWM
C.
C?   Description
C?   ===========
C?   Copies the bit patterns from bank XTEB into XTDI and add a name for
C?   each row, and decodes the bit patterns from mapped signals and
C?   special bits (ITC and TPC)
C?
C.======================================================================
#ifndef DOC
      SAVE
#include "xtebjj.h"
#include "xtdijj.h"
#include "x1para.h"
#include "x1rsc.h"
      DIMENSION IXTEB(*),IXTDI(*)
      DIMENSION NUMOD(3),KXTDI(3)
      DIMENSION IBSTR(NBWRDS)
      DIMENSION NEWBI(NBWRDS)
      DIMENSION ISADR(28)
      DIMENSION ITADR(28)
      LOGICAL BTEST
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
      IF(FIRST)THEN
C - fill strings CHXTD and CHX1S from PARAMETER statement
C   CHXTDI and CHX1SC and then use these strings to please
C   IBM compiler
        CHXTD = CHXTDI
        CHX1S = CHX1SC
C - At first entry set pointers into XTEB
        ISADR(1) = JXTET1
        ISADR(2) = JXTET2
        ISADR(3) = JXTEL2
        ISADR(4) = JXTEHT
        ISADR(5) = JXTEHT +   NBWRDS
        ISADR(6) = JXTEHT + 2*NBWRDS
        ISADR(7) = JXTEHT + 3*NBWRDS
        ISADR(8) = JXTEHW
        ISADR(9) = JXTEHW +   NBWRDS
        ISADR(10)= JXTEHW + 2*NBWRDS
        ISADR(11)= JXTEHW + 3*NBWRDS
        ISADR(12)= JXTELW
        ISADR(13)= JXTELW +        1
        ISADR(14)= JXTELW +        2
        ISADR(15)= JXTELW +        3
        ISADR(16)= JXTEEW
        ISADR(17)= JXTEEW +   NBWRDS
        ISADR(18)= JXTEEW + 2*NBWRDS
        ISADR(19)= JXTEEW + 3*NBWRDS
        ISADR(20)= JXTELT
        ISADR(21)= JXTELT +        1
        ISADR(22)= JXTELT +        2
        ISADR(23)= JXTELT +        3
        ISADR(24)= JXTETE
        ISADR(25)= JXTEIT
        ISADR(26)= JXTEIT +        3
        ISADR(27)= JXTETP
        ISADR(28)= JXTETP +        3
C - and into XTDI
        ITADR(1) = 1
        ITADR(2) = 2
        ITADR(3) = 3
        ITADR(4) = 4
        ITADR(5) = 5
        ITADR(6) = 6
        ITADR(7) = 7
        ITADR(8) = 8
        ITADR(9) = 9
        ITADR(10)= 10
        ITADR(11)= 11
        ITADR(12)= 12
        ITADR(13)= 13
        ITADR(14)= 14
        ITADR(15)= 15
        ITADR(16)= 31
        ITADR(17)= 32
        ITADR(18)= 33
        ITADR(19)= 34
        ITADR(20)= 22
        ITADR(21)= 23
        ITADR(22)= 24
        ITADR(23)= 25
        ITADR(24)= 26
        ITADR(25)= 20
        ITADR(26)= 35
        ITADR(27)= 21
        ITADR(28)= 36
        FIRST =.FALSE.
      ENDIF
      IPOEB = 0
C - Copy the bit-patterns from XTEB
      DO 10 KITEM = 1,28
        IPODI = (ITADR(KITEM)-1) * LXTDIA
        CALL VZERO(IXTDI(IPODI+JXTDBP),NBWRDS)
C -- Mark item as filled
        IXTDI(IPODI+JXTDFI) = 1
C -- Get number of words to be filled
        IF((KITEM.GE.4.AND.KITEM.LE.11).OR.
     &     (KITEM.GE.16.AND.KITEM.LE.19).OR.
     &     (KITEM.EQ.25).OR.
     &     (KITEM.EQ.27)) THEN
          NWCOP = 3
        ELSEIF((KITEM.EQ.15).OR.
     &         (KITEM.EQ.24))THEN
          NWCOP = 2
        ELSE
          NWCOP = 1
        ENDIF
C -- Copy from XTEB to XTDI
        CALL UCOPY(IXTEB(IPOEB+ISADR(KITEM)),IXTDI(IPODI+JXTDBP),
     &             NWCOP)
   10 CONTINUE
C - Now form/fill the additional bit-patterns
      DO 80 KA = 1,2
C -- Get pointers into the bank XTDI
        IF(KA.EQ.1)THEN
          IROW = (INDEX(CHXTD,'HWT1')+3)/4
          IF(IROW.EQ.0) RETURN
          JROW = (INDEX(CHXTD,'HCW1')+3)/4
          IF(JROW.EQ.0) RETURN
        ELSEIF(KA.EQ.2)THEN
          IROW = (INDEX(CHXTD,'EWU1')+3)/4
          IF(IROW.EQ.0) RETURN
          JROW = (INDEX(CHXTD,'EWT1')+3)/4
          IF(JROW.EQ.0) RETURN
        ENDIF
        IPOIN = (IROW-1)*LXTDIA
        JPOIN = (JROW-1)*LXTDIA
C -- Loop over 4 threshold sets
        DO 70 KB = 1,4
C --- Mark item as filled
          IXTDI(JPOIN+JXTDFI) = 1
C --- Get pointers to the bit-patterns
          IND = IPOIN + JXTDBP
          JND = JPOIN + JXTDBP
          CALL VZERO(IXTDI(JND),NBWRDS)
          IF(KA.EQ.1)THEN
C --- Generate HCAL wires module bit-pattern
            DO 20 J = 0,NSEGMS-1
              IF(J.LT.6)THEN
                IP = J
C ---- No redundancy > see dead channels
C              ELSEIF(J.GT.5.AND.J.LT.12)THEN
C                IP = J - 6
              ELSEIF(J.GT.23.AND.J.LT.36)THEN
                IP = J - 18
C ---- No redundancy > see dead channels
C              ELSEIF(J.GT.35.AND.J.LT.48)THEN
C                IP = J - 30
              ELSEIF(J.GT.59.AND.J.LT.66)THEN
                IP = J - 42
C ---- No redundancy > see dead channels
C              ELSEIF(J.GT.65)THEN
C                IP = J - 48
              ELSE
                GOTO 20
              ENDIF
              KND = J/32 + IND
              IBT = MOD(J,32)
              IF(BTEST(IXTDI(KND),IBT)) IXTDI(JND)=IBSET(IXTDI(JND),IP)
   20       CONTINUE
          ELSEIF(KA.EQ.2)THEN
C --- Generate ECAL wires bit-patterns
            CALL VZERO(IBSTR,NBWRDS)
            DO 30 KC = 0,71,2
              KND = KC/32 + IND
              IBT = MOD(KC,32)
              LND = KC/64 + 1
              LBT = MOD(KC/2,32)
C ---- Get the 36 odd/even coinc.as contig.string
              IF(BTEST(IXTDI(KND),IBT).AND.BTEST(IXTDI(KND),IBT+1))
     &          IBSTR(LND) = IBSET(IBSTR(LND),LBT)
   30       CONTINUE
C ---- Get wire signals mapped onto segments
            CALL VZERO(NEWBI,NBWRDS)
            DO 50 J=1,NSEGMS
              LW = (J-1)/32 + 1
C ----- Get the modules mapped onto this segment
              CALL X1ECWM(J,NMODS,NUMOD)
              DO 40 KM = 1,NMODS
                KW = (NUMOD(KM)-1)/32 + 1
                IF(BTEST(IBSTR(KW),MOD(NUMOD(KM)-1,32)))THEN
                  NEWBI(LW) = IBSET(NEWBI(LW),MOD(J-1,32))
                ENDIF
   40         CONTINUE
   50       CONTINUE
C ---- Everything prepared .. now fill
            DO 60 KC = 0,NBWRDS-1
C ----- Fill the ECW segment signals
              IXTDI(JND+KC) = NEWBI(KC+1)
   60       CONTINUE
          ENDIF
          IPOIN = IPOIN + LXTDIA
          JPOIN = JPOIN + LXTDIA
   70   CONTINUE
   80 CONTINUE
C
C UFF... THAT'S DONE, WHO KNOWS IF ALL BITS ARE AT THE RIGHT PLACE ?
C
      RETURN
      END
#endif
