      LOGICAL FUNCTION TCHKHV(IRUN,IEVT,IFLG)
C
C-----------------------------------------------------------------------
C! Check TPC high voltage status for dE/dx purposes
C!
C!  R. Johnson    22-6-90
C!  F.Ranjard - 26-4-94
C!     get the high voltage word through ALTRHV
C!  Input:   IRUN    /I    Current run number
C!           IEVT    /I    Current event number
C!  Output:  TCHKHV  /L    .TRUE. if high voltage is (believed to be) on
C!           IFLG    /I    0 = test was made on dE/dx HV bit
C!                         1 = test was made on TPC tracking HV bit
C!                         2 = no test was made (banks not found)
C!
C-----------------------------------------------------------------------
C
#ifndef DOC
#include "bcs.h"
#include "tdbsjj.h"
#include "revhjj.h"
C
      SAVE
      INTEGER AGETDB, ALTRHV
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NREVH=NAMIND('REVH')
        NTDBS=NAMIND('TDBS')
        NXTCN=NAMIND('XTCN')
        IRLST=-1
      ENDIF
C
      IF (IRUN.NE.IRLST) THEN
        IRLST=IRUN
        IRET=AGETDB('TDBS',IRUN)
      ENDIF
      TCHKHV=.FALSE.
      IFLG=2
C
      KREVH=IW(NREVH)
      IF (KREVH.EQ.0) THEN
        IRET = ALTRHV(ISTAT)
        IF (IRET.EQ.0) RETURN
      ELSE
        ISTAT=ITABL(KREVH,1,JREVDS)
      ENDIF
      KTDBS=IW(NTDBS)
      IF (KTDBS.EQ.0) RETURN
C
      DO 100 I=1,LROWS(KTDBS)
        IR1=ITABL(KTDBS,I,JTDBVR)
        IF (IR1.GT.IRUN) GO TO 100
        IR2=ITABL(KTDBS,I,JTDBVR+1)
        IF (IR2.LT.IRUN) GO TO 100
C
C++     At least one sector is bad.  Do not check the dE/dx bit
C++     but only the other HV bit.  It would be nice to look at
C++     the slow control info at this point, but that will be
C++     difficult to implement because it is out of sinc with the
C++     event records.
C
        IF (IRUN.LT.4800) THEN
          IBTTP=IBITS(ISTAT,4,1)
        ELSE
          IBTTP=IBITS(ISTAT,15,1)
        ENDIF
        IF (IBTTP.EQ.1) THEN
          TCHKHV=.TRUE.
        ENDIF
        IFLG=1
        RETURN
  100 CONTINUE
C
C++   Did not find any bad sectors, so check the dE/dx bit
C
      IBTTP=IBITS(ISTAT,4,1)
      IF (IBTTP.EQ.1) TCHKHV=.TRUE.
      IFLG=0
C
      RETURN
      END
#endif
