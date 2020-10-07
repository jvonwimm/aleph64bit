      SUBROUTINE X1DISC
C ----------------------------------------------------------------------
C.
C. - Author   : A. Putzer  - 86/08/08  FOR GALEPH 13.0
C. - Modified : A. Putzer  - 88/02/02  FOR GALEPH 19.6
C. - Modified : C. Geweniger - 88/10/11  for GALEPH 20.1
C. - Modified : C. Geweniger - 88/11/06  for GALEPH 20.2
C. - Modified : E. Blucher - 89/15/2 for ALEPHLIB
C. - Modified : C. Geweniger - 89/9/00   for ALEPHLIB 9.9
C. - Modified : B. Bloch-Devaux - 92/12/10 for Sical in 92
C.
C.
C! - Discriminate level1 trigger signals
C.
C? - The analog trigger sources for the level1 trigger are discriminated
C?   using up to 4 sets of thresholds.
C.
C. - Banks    : XTEB is filled
C.
#ifndef DOC
      SAVE
#include "bcs.h"
#include "x1namc.h"
#include "x1tcom.h"
#include "x1thjj.h"
#include "xtebjj.h"
#include "alcons.h"
C
C ----------------------------------------------------------------------
C
      LOGICAL BTEST
      DIMENSION NSHEW(8)
C
C. - Threshold set #2 for ECAL wires total energy (KECTW).
C.   This set is not provided in bank X1TH. It is used for the
C.   total energy triggers ETT_EWEA and ETT_EWEB.
      DIMENSION KECTW(4)
      DATA KECTW/ 2300, 2300, 2800, 3500/
      DATA NSHEW/ 0, 0, 24, 24, 24, 24, 48, 48/
C
#include "bmacro.h"
C ----------------------------------------------------------------------
C
C.
C. - Loop over the 72 trigger segments (24 for LCAL) and apply
C.   the four different sets of tresholds.
C. - At present the thresholds for each item are independent of
C.   phi and theta
C. - After discrimination the two barrel theta bins are combined
C.
      DO 99 I=1,NBTWD
        NHTBIT(I) = 0
        NHWBIT(I) = 0
        NETBIT(I) = 0
        NEWBIT(I) = 0
        NLTBIT(I) = 0
        NITBIT(I) = 0
 99   CONTINUE
      NTEBIT = 0
C
      ID = IW(NAX1TH)
      IADD  = LCOLS(ID)
      IECTR = ID    + 1 + JX1TTV
      IECWI = IECTR + IADD
      IHCTR = IECWI + IADD
      IHCWI = IHCTR + IADD
      ILCTR = IHCWI + IADD
      IECTT = ILCTR + IADD
      IECTW = IECTT + IADD
      IHCTT = IECTW + IADD
C
      KXTEB=IW(NAXTEB)+LMHLEN
C
C
C  - HCAL tower trigger sources
C
      KK=-1
      DO 101 I=1,NTRHL
        IBIT = 1
        DO 111 J=1,NSEGM
          IF (IBIT.GT.NBITVW) THEN
            JBIT = IBIT - NBITVW - 1
            K = 2*I - 1
          ELSE
            JBIT = IBIT - 1
            K = 2*I
          ENDIF
          KBIT=MOD(J-1,32)
          IF (KBIT.EQ.0) KK=KK+1
          KW=KXTEB+KK
C.
C. - HCAL tower trigger sources
C.
          IF (IHTSUM(J).GT.IW(IHCTR+I) .OR. IW(IHCTR+I).EQ.0) THEN
            NHTBIT(K)=IBSET(NHTBIT(K),JBIT)
            IW(KW+JXTEHT)=IBSET(IW(KW+JXTEHT),KBIT)
          ENDIF
C.
C. - HCAL wire trigger sources
C.
          IF (IHWSUM(J).GE.IW(IHCWI+I)) THEN
            NHWBIT(K)=IBSET(NHWBIT(K),JBIT)
            IW(KW+JXTEHW)=IBSET(IW(KW+JXTEHW),KBIT)
          ENDIF
C.
C. - ECAL tower trigger sources
C.
          IF (IETSUM(J).GT.IW(IECTR+I) .OR. IW(IECTR+I).EQ.0) THEN
            NETBIT(K)=IBSET(NETBIT(K),JBIT)
            IW(KW+JXTELW)=IBSET(IW(KW+JXTELW),KBIT)
          ENDIF
C.
C. - ECAL wire trigger sources
C.
          IF (IEWSUM(J).GT.IW(IECWI+I))
     *      IW(KW+JXTEEW)=IBSET(IW(KW+JXTEEW),KBIT)
C
          IBIT = IBIT + 1
          IF (J.EQ.36) IBIT = 25
 111    CONTINUE
 101  CONTINUE
C.
C.
C. - ECAL wire trigger sources:
C.   * Do coincidence between odd and even planes
C.   * Map onto trigger segments
C.
C.
      DO 201 I=1,NTRHL
        NTHVL = IW(IECWI+I)
        DO 202 J=1,NSEGM,2
          IF (IEWSUM(J).LE.NTHVL.OR.IEWSUM(J+1).LE.NTHVL) GO TO 202
          IF (J.LT.24) THEN
C -  ENDCAP A
            JBIT = (J+3)/4 - 1
            NEWBIT(2*I) = IBSET(NEWBIT(2*I),JBIT)
            NEWBIT(2*I) = IBSET(NEWBIT(2*I),JBIT+6)
C
            JBIT = (J+23)/2
            NEWBIT(2*I) = IBSET(NEWBIT(2*I),JBIT)
            IF (JBIT.EQ.23) JBIT = 11
            NEWBIT(2*I) = IBSET(NEWBIT(2*I),JBIT+1)
          ELSE IF (J.GT.48) THEN
C - ENDCAP B
            JBIT = (J+15)/4
            NEWBIT(2*I-1) = IBSET(NEWBIT(2*I-1),JBIT)
            NEWBIT(2*I-1) = IBSET(NEWBIT(2*I-1),JBIT+6)
C
            JBIT = (J-41)/2
            NEWBIT(2*I-1) = IBSET(NEWBIT(2*I-1),JBIT)
            IF (JBIT.EQ.15) JBIT = 3
            NEWBIT(2*I-1) = IBSET(NEWBIT(2*I-1),JBIT+1)
          ELSE
C - BARREL
            JBIT = J/2
            NEWBIT(2*I) = IBSET(NEWBIT(2*I),JBIT)
            NEWBIT(2*I-1) = IBSET(NEWBIT(2*I-1),JBIT-8)
            IF (JBIT.LT.20) THEN
              NEWBIT(2*I) = IBSET(NEWBIT(2*I),JBIT+12)
            ELSE
              NEWBIT(2*I-1) = IBSET(NEWBIT(2*I-1),JBIT-20)
            ENDIF
          ENDIF
  202   CONTINUE
  201 CONTINUE
C
C  -  LCAL tower trigger sources
C
      NHALF = NSEGL/2
      DO 301 I=1,NTRHL
        ICUT = IW(ILCTR+I)
        DO 311 J=1,NSEGL
          IF (ILTSUM(J).GT.ICUT) NLTBIT(I)=IBSET(NLTBIT(I),J-1)
C - Prepare bitpattern for back-to-back coincidences
          IF (J.GT.NHALF.AND.ILTSUM(J).GT.ICUT) THEN
            JJ=MOD(J+5,12)
            II=I+NTRHL
            NLTBIT(II)=IBSET(NLTBIT(II),JJ)
          ENDIF
  311   CONTINUE
        IW(KXTEB+JXTELT+I-1)=NLTBIT(I)
  301 CONTINUE
C
C  - SICAL  trigger sources
C
      CALL SIX2MK
C
C  -  Get the ITC segment trigger bits
C
      IBIT = 1
      DO 401 J=1,NSEGM
        IF (IBIT.GT.NBITVW) THEN
          JBIT = IBIT - NBITVW - 1
          K = 1
        ELSE
          JBIT = IBIT - 1
          K = 2
        ENDIF
        IF (J.LE.NBITVW) THEN
          KK = 1
          KB = J - 1
        ELSE IF (J.GT.2*NBITVW) THEN
          KK = 3
          KB = J - 2*NBITVW - 1
        ELSE
          KK = 2
          KB = J - NBITVW - 1
        ENDIF
        IF (BTEST(IITSUM(KK),KB)) NITBIT(K)=IBSET(NITBIT(K),JBIT)
        IBIT = IBIT + 1
        IF (J.EQ.36) IBIT = 25
 401  CONTINUE
      KK=KXTEB+JXTEIT
      IW(KK)=NITBIT(2)
      CALL MVBITS(NITBIT(1),0,4,IW(KK+1),0)
      CALL MVBITS(NITBIT(1),4,16,IW(KK+1),16)
      IW(KK+2)=IITSUM(3)
      IW(KK+3)=IITSUM(4)
C
C - Total Energy
C
      DO 501 I = 1,NTOEV
        J = I - 1
        IF (IHCTTE(I).GT.IW(IHCTT+I)) NTEBIT=IBSET(NTEBIT,J)
        IF (IECTTE(I).GT.IW(IECTT+I)) NTEBIT=IBSET(NTEBIT,J+NTOEV)
        IF (IECWTE(I).GT.IW(IECTW+I)) NTEBIT=IBSET(NTEBIT,J+2*NTOEV)
        IF (IECWTE(I+NTOEV).GT.IW(IECTW+I))
     *                                NTEBIT=IBSET(NTEBIT,J+3*NTOEV)
        IF (IECWTE(I).GT.KECTW(I))    NTEBIT=IBSET(NTEBIT,J+6*NTOEV)
        IF (IECWTE(I+NTOEV).GT.KECTW(I))
     *                                NTEBIT=IBSET(NTEBIT,J+7*NTOEV)
 501  CONTINUE
      MTEBIT = 0
      CALL MVBITS(NTEBIT, 0,4,MTEBIT, 0)
      CALL MVBITS(NTEBIT, 4,4,MTEBIT,16)
      IW(KXTEB+JXTETE)=MTEBIT
      MTEBIT = 0
      CALL MVBITS(NTEBIT, 8,4,MTEBIT, 0)
      CALL MVBITS(NTEBIT,24,4,MTEBIT, 4)
      CALL MVBITS(NTEBIT,12,4,MTEBIT,16)
      CALL MVBITS(NTEBIT,28,4,MTEBIT,20)
      IW(KXTEB+JXTETE+1)=MTEBIT
      RETURN
      END
#endif
