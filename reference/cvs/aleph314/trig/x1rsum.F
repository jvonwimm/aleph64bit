      SUBROUTINE X1RSUM(ILUN,IERR)
C----------------------------------------------------------------------
C!  - Print the trigger run summary
C.
C.   Author   :- Alois Putzer           4-AUG-1989
C.               Martin Wunsch          4-AUG-1989
C.               Yves A. Maumary       20-DEC-1989 Adapted for ALEPHLIB
C    Modified :- Yves A. Maumary       16-MAR-1990 for '90 run
C    Modified :- Yves A. Maumary       15-MAR-1991 New bank XTRB
C    Modified :- F.Ranjard             19-MAR-1991 suppress references
C                                                  to JULIA common blocks
C.
C.   Inputs:
C.        - ILUN : INTEGER : logical unit number for the formatted
C.                           fortran output
C.
C.   Outputs:
C.        - IERR : INTEGER = 0 : printout of run summary successful
C.                         = 1 : at least one of the summary banks not
C.                               found
C.
C.   Libraries required: ALEPHLIB, BOS77, CERNLIB
C.
C.   Calls: AGETDB, ALTELL, CHAINT, INTCHA, NAMIND, UCOPY
C.
C?   Description
C?   ===========
C?   Finds the 3 trigger run summary banks (XSGE,XSHI,XSSC), puts their
C?   content into local arrays and prints the summary onto unit
C?   ILUN
C?
C.======================================================================
#ifndef DOC
      SAVE
      EXTERNAL AGETDB,CHAINT,INTCHA,NAMIND
#include "bcs.h"
#include "xsgejj.h"
#include "xshijj.h"
#include "xsscjj.h"
#include "xtbnjj.h"
      PARAMETER (LROWSH=36, LROWSS=23, NSEGMS=72, NBWRDS=3, NBITPW=32)
      PARAMETER (NSCALS=60)
      INTEGER ITACCU(NBWRDS*NBITPW,LROWSH)
      INTEGER ITACSC(NSCALS,LROWSS)
      INTEGER AGETDB,ICHN(32,2)
      CHARACTER*4 CHAINT,RNAME
#include "bmacro.h"
C-----------------------------------------------------------------------
      IRUN = 0
      IERR = 0
C - Get bank XSGE
      JXSGE = IW(NAMIND('XSGE'))
      IF(JXSGE.NE.0)THEN
C -- Get bank number (= run number)
        IRUN = IW(JXSGE-2)
C -- Get bank contents
        KXSGE = JXSGE + LMHLEN
        GBXSUM = RW(KXSGE+JXSGGS)
        TTREAL = RW(KXSGE+JXSGTR)
        TTOPEN = RW(KXSGE+JXSGTO)
        NBREVT = IW(KXSGE+JXSGNE)
        NBXTEB = IW(KXSGE+JXSGEB)
        NBX1AD = IW(KXSGE+JXSG1A)
        NBX1SC = IW(KXSGE+JXSG1S)
        NBXTCN = IW(KXSGE+JXSGCN)
        NEVREC = IW(KXSGE+JXSGER)
      ELSE
        CALL ALTELL('+++X1RSUM+++ Bank XSGE not found',0,'RETURN')
        IERR = 1
      ENDIF
C - Get bank XSHI
      JXSHI = IW(NAMIND('XSHI'))
      IF(JXSHI.NE.0)THEN
C -- Get bank number (= run number) and check consistency
        IF(IRUN.EQ.0)THEN
          IRUN = IW(JXSHI-2)
        ELSE
          IF(IRUN.NE.IW(JXSHI-2))THEN
            CALL ALTELL('+++X1RSUM+++ Run numbers mixed',0,'RETURN')
            IERR = 1
            RETURN
          ENDIF
        ENDIF
C -- Get bank contents
        ITRG1 = INTCHA('TRG1')
        ITRG2 = INTCHA('TRG2')
        ITRGA = INTCHA('TRGA')
        IND1 = 0
        IND2 = 0
        INDA = 0
        DO 10 I = 1,LROWS(JXSHI)
          KXSHI = KROW (JXSHI,I)
          IF (IW(KXSHI+JXSHNA).EQ. ITRG1) IND1 = I
          IF (IW(KXSHI+JXSHNA).EQ. ITRG2) IND2 = I
          IF (IW(KXSHI+JXSHNA).EQ. ITRGA) INDA = I
          CALL UCOPY(IW(KXSHI+JXSHSH),ITACCU(1,I),NSEGMS)
   10   CONTINUE
      ELSE
        CALL ALTELL('+++X1RSUM+++ Bank XSHI not found',0,'RETURN')
        IERR = 1
      ENDIF
C - Get bank XSSC
      JXSSC = IW(NAMIND('XSSC'))
      IF(JXSSC.NE.0)THEN
C -- Get bank number (= run number) and check consistency
        IF(IRUN.EQ.0)THEN
          IRUN = IW(JXSSC-2)
        ELSE
          IF(IRUN.NE.IW(JXSSC-2))THEN
            CALL ALTELL('+++X1RSUM+++ Run numbers mixed',0,'RETURN')
            IERR = 1
            RETURN
          ENDIF
        ENDIF
C -- Get bank contents
        ITRB = INTCHA('TRB ')
        INDS = 0
        DO 20 I = 1,LROWS(JXSSC)
          KXSSC = KROW(JXSSC,I)
          IF (IW(KXSSC+JXSSNA) .EQ. ITRB) INDS = I
          CALL UCOPY(IW(KXSSC+JXSSSC),ITACSC(1,I),NSCALS)
   20   CONTINUE
      ELSE
        CALL ALTELL('+++X1RSUM+++ Bank XSSC not found',0,'RETURN')
        IERR = 1
      ENDIF
C - Initialize array for trigger bit names
      DO 40 J = 1,2
        DO 30 I = 1,32
          ICHN(I,J) = INTCHA('----')
   30   CONTINUE
   40 CONTINUE
C - Get bank with trigger bit names (XTBN)
      ISTAT = AGETDB('XTBN',IRUN)
      IF(ISTAT.EQ.0)THEN
         CALL ALTELL('+++X1RSUM+++ Bank XTBN not found',0,'RETURN')
         GOTO 55
      ENDIF
      JXTBN = IW(NAMIND('XTBN'))
      IF(JXTBN.NE.0)THEN
        DO 50 K=1,LROWS(JXTBN)
          ICHN(K,1) = ITABL(JXTBN,K,JXTBBM)
          ICHN(K,2) = ITABL(JXTBN,K,JXTBBM+1)
   50   CONTINUE
      ENDIF
   55 CONTINUE
C - Print the results
C -- Title
      WRITE(ILUN,1001) IRUN
 1001 FORMAT(1H1,/
     &          ,38X,'*************************************************'
     &        ,/,38X,'*                                               *'
     &        ,/,38X,'*       R U N     S U M M A R Y     F O R       *'
     &        ,/,38X,'*                                               *'
     &        ,/,38X,'*       P H Y S I C S     R U N  :',I7, '       *'
     &        ,/,38X,'*                                               *'
     &        ,/,38X,'*************************************************'
     &        ,/)
   56 CONTINUE
C
      IF(JXSGE.EQ.0) GOTO 57
C -- Run summary short general info
      IF(TTOPEN.GT.0.)THEN
        TRATE = FLOAT(NBREVT)/TTOPEN
      ELSE
        TRATE = -999.99999
      ENDIF
      WRITE(ILUN,1002) TTREAL,TTOPEN,NBREVT,GBXSUM,TRATE,
     &                 NEVREC,NBXTEB,NBX1AD,NBX1SC,NBXTCN
 1002 FORMAT(40X,'    G E N E R A L    I N F O R M A T I O N',/,
     &       40X,'    ======================================',//,
     &       /,40X,'Integrated Real Time          [s] ',F12.2,
     &       /,40X,'Integrated Open Time          [s] ',F12.2,
     &       /,40X,'Number of Accepted Events         ',I12,
     &       /,40X,'Number of GBX                     ',F12.0,
     &       /,40X,'Total Trigger Rate            [Hz]',F12.5,/,
     &       /,40X,'Number of event records read      ',I12,
     &       /,40X,'Number of events with XTRB filled ',I12,
     &       /,40X,'Number of events with X1AD filled ',I12,
     &       /,40X,'Number of events with X1SC filled ',I12,
     &       /,40X,'Number of events with XTCN filled ',I12,/)
   57 CONTINUE
C
      IF(JXSHI.EQ.0 .OR. JXSSC.EQ.0) GOTO 65
C -- Trigger statistics
      WRITE(ILUN,1003)
 1003 FORMAT(40X,'    T R I G G E R    S T A T I S T I C S',/,
     &       40X,'    ====================================',/)
      IF (IND1.EQ.0.OR.IND2.EQ.0.OR.INDA.EQ.0.OR.INDS.EQ.0) RETURN
      WRITE(ILUN,1004)
 1004 FORMAT(/,5X,'Mnemonic',
     &       ' Bit-Assignment',
     &       '        Level 1',
     &       '        Level 2',
     &       '       Accepted',
     &       '  Trigger Rate[Hz]',
     &       '  Scaler Counts',
     &       '   Scaler Rate[Hz]',/)
      DO 60 J = 1,NBITPW
        IF(GBXSUM.GE.1.)THEN
          RAT1 = FLOAT(ITACCU(J,INDA))*44980./GBXSUM
          RAT2 = FLOAT(ITACSC(J,INDS))*44980./GBXSUM
        ELSE
          RAT1 = 0.
          RAT2 = 0.
        ENDIF
        WRITE(ILUN,1005) ICHN(J,1),ICHN(J,2),
     &                   J-1,ITACCU(J,IND1),ITACCU(J,IND2),
     &                   ITACCU(J,INDA),RAT1,ITACSC(J,INDS),RAT2
 1005   FORMAT(5X,2A4,4I15,3X,F15.5,I15,3X,F15.5)
   60 CONTINUE
   65 CONTINUE
C
      IF(JXSHI.EQ.0) GOTO 75
C - Print register sums
      WRITE(ILUN,1006)
 1006 FORMAT(/,40X,'    S U M M E D    R E G I S T E R    B I T S',/,
     &         40X,'    =========================================',/)
      DO 70 JJJ = 4,LROWS(JXSHI)
        RNAME = CHAINT (ITABL(JXSHI,JJJ,JXSHNA))
C
C -- special bits ( PLU's )
        IF(RNAME(1:3).EQ.'PLU')THEN
          WRITE(ILUN,1111) RNAME,(ITACCU(J,JJJ),J=1,72)
 1111     FORMAT(1X,A4,2(3X,8I6),/,(5X,2(3X,8I6)))
C
C -- Total energy
        ELSEIF(RNAME(1:3).EQ.'ETT')THEN
          WRITE(ILUN,1112) RNAME,(ITACCU(J,JJJ),J=1,72)
 1112     FORMAT(1X,A4,4(3X,4I5),/,(5X,4(3X,4I5)))
C
C -- HCal wires not mapped
        ELSEIF(RNAME(1:3).EQ.'HCW')THEN
          WRITE(ILUN,1113) RNAME,(ITACCU(J,JJJ),J=1,24)
 1113     FORMAT(1X,A4,3X,
     &         ' Endcap A        : ',I12,5I16,/,8X,
     &         ' Barrel          : ',12I8,/,8X,
     &         ' Endcap B        : ',I12,5I16)
C
C -- LCal towers
        ELSEIF(RNAME(1:3).EQ.'LCT')THEN
          WRITE(ILUN,1114) RNAME,(ITACCU(J,JJJ),J=1,24)
 1114     FORMAT(1X,A4,3X,
     &         ' Side A          : ',12I8,/,8X,
     &         ' Side B          : ',12I8)
C
C -- ECal wires odd-even
        ELSEIF(RNAME(1:3).EQ.'EWU')THEN
          WRITE(ILUN,1116) RNAME,((ITACCU(2*J-1+(K-1)*24,JJJ),J=1,12),
     &                     (ITACCU(2*J+(K-1)*24,JJJ),J=1,12),K=1,3)
 1116     FORMAT(1X,A4,3X,
     &         ' Endcap A (odd ) : ',12I8,/,8X,
     &         ' Endcap A (even) : ',12I8,/,8X,
     &         ' Barrel   (odd ) : ',12I8,/,8X,
     &         ' Barrel   (even) : ',12I8,/,8X,
     &         ' Endcap B (odd ) : ',12I8,/,8X,
     &         ' Endcap B (even) : ',12I8)
C
C -- LCal wires
        ELSEIF(RNAME(1:3).EQ.'LCW')THEN
          WRITE(ILUN,1118) RNAME,(ITACCU(J,JJJ),J=1,4),
     &                           (ITACCU(J,JJJ),J=7,10)
 1118     FORMAT(1X,A4,3X,
     &         '                 : ',8I8)
C
C -- standard format (72 segments)
        ELSE
          WRITE(ILUN,1119) RNAME,(ITACCU(J,JJJ),J=1,NSEGMS)
 1119     FORMAT(1X,A4,3X,
     &         ' Endcap A        : ',I12,5I16,/,8X,
     &         ' Endcap A        : ',I12,5I16,/,8X,
     &         ' Overlap         : ',12I8,/,8X,
     &         ' Barrel          : ',12I8,/,8X,
     &         ' Barrel          : ',12I8,/,8X,
     &         ' Overlap         : ',12I8,/,8X,
     &         ' Endcap B        : ',I12,5I16,/,8X,
     &         ' Endcap B        : ',I12,5I16)
        ENDIF
   70 CONTINUE
   75 CONTINUE
C
      IF(JXSSC.EQ.0) GOTO 999
C - Print scaler sums
      WRITE(ILUN,2001)
 2001 FORMAT(/,40X,'    S U M M E D    S C A L E R    C O U N T S',/,
     &         40X,'    =========================================',/)
C
      DO 80 JJJ = 1,LROWS(JXSSC)
        KXSSC = KROW(JXSSC,JJJ)
        RNAME = CHAINT (IW(KXSSC+JXSSNA))
C
C -- ECal wires (coincidence odd-even), trigger bits
        IF((RNAME(1:3).EQ.'ECW').OR.(RNAME.EQ.'TRB '))THEN
          WRITE(ILUN,2111) RNAME,(ITACSC(J,JJJ),J=1,36)
 2111     FORMAT(1X,A4,3X,12I9,/,(8X,12I9))
C
C -- HCal towers: forget it
        ELSEIF(RNAME(1:3).EQ.'HCT')THEN
          CONTINUE
C
C -- standard format (60 scalers)
        ELSE
          WRITE(ILUN,2111) RNAME,(ITACSC(J,JJJ),J=1,NSCALS)
        ENDIF
   80 CONTINUE
  999 RETURN
      END
#endif
