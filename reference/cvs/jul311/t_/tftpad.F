      SUBROUTINE TFTPAD
C----------------------------------------------------------------------
C!  - Remove garbage pulses from TPC raw pad data
C!    Any pulse with T0=0 is removed, along with its digitizations.
C!    If any other sort of error is found in the hit list, then
C!    the sector is skipped and TPRLST will take care of it.
C!
C!   Author   :- Robert P. Johnson     27-MAY-1991
C!
C!======================================================================
#ifndef DOC
C
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "bcs.h"
C
      LOGICAL FIRST,FBAD(LTSECT)
      CHARACTER TEXT*80
      DATA FIRST/.TRUE./
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTPAD=NAMIND('TPAD')
        NTPDI=NAMIND('TPDI')
      ENDIF
C
      DO 50 ISLOT=1,LTSECT
        FBAD(ISLOT)=.FALSE.
   50 CONTINUE
C
      KTPAD=IW(NTPAD)
  100 IF (KTPAD.EQ.0) GO TO 501
        ISLOT=IW(KTPAD-2)
        IF (ISLOT.LT.1 .OR. ISLOT.GT.LTSECT) THEN
          KTPAD=IW(KTPAD-1)
          GO TO 100
        ENDIF
        KTPDI=NLINK('TPDI',ISLOT)
        IF (KTPDI.EQ.0) THEN
          KTPAD=IW(KTPAD-1)
          GO TO 100
        ENDIF
C
C++     Check if the sector has any hits with T0=0
C
        ISTYP=ITPTYP(ISLOT)
C
C++     Loop over padrows
C
        IPNT=0
        NS=0
  120   IF (IPNT.GE.IW(KTPAD)-2) GO TO 151
          IPNT=IPNT+1
          IROW=IBITS(IW(KTPAD+IPNT),0,4)
C
C++       Skip this sector if a row number is bad or if
C++       the number of hits in a row is garbage
C
          IF (IROW.LT.1 .OR. IROW.GT.NTPDRW(ISTYP)) THEN
            FBAD(ISLOT)=.FALSE.
            KTPAD=IW(KTPAD-1)
            GO TO 100
          ENDIF
          IPNT=IPNT+1
          NHIT=IW(KTPAD+IPNT)
          IF (NHIT.LT.1 .OR. NHIT.GT.(IW(KTPAD)-IPNT)) THEN
            FBAD(ISLOT)=.FALSE.
            KTPAD=IW(KTPAD-1)
            GO TO 100
          ENDIF
C
C++       Loop over hits in this padrow
C
          DO 150 IH=1,NHIT
            IPNT=IPNT+1
            IT0=IBITS(IW(KTPAD+IPNT),0,9)
            NS=NS+IBITS(IW(KTPAD+IPNT),16,8)
            IF (IT0.EQ.0) THEN
              FBAD(ISLOT)=.TRUE.
            ENDIF
  150     CONTINUE
          GO TO 120
  151   CONTINUE
C
C++     Skip this sector if the digitization bank is too short
C
        NWRD=NS/4
        IF (MOD(NS,4).GT.0) NWRD=NWRD+1
        IF (NWRD.GT.IW(KTPDI)) FBAD(ISLOT)=.FALSE.
C
        KTPAD=IW(KTPAD-1)
        GO TO 100
  501 CONTINUE
C
C++   Remake TPAD and TPDI banks for the bad sectors
C
      DO 801 ISLOT=1,LTSECT
        IF (.NOT.FBAD(ISLOT)) GO TO 801
C
        WRITE(TEXT,325) ISLOT
  325   FORMAT('Warning, bad hits being removed from TPAD and TPDI ',
     &         'for sector ',I2)
        CALL RERROR('TFTPAD',1,TEXT(1:74))
C
        KTPAD=NLINK('TPAD',ISLOT)
        KTPDI=NLINK('TPDI',ISLOT)
C
C++     Loop over padrows
C
        IRLST=-1
        IPNT=0
        IPNT1=0
        IWRD=KTPDI+1
        IBT=24
        IWRD1=KTPDI+1
        IBT1=24
  620   IF (IPNT.GE.IW(KTPAD)-1) GO TO 621
          IPNT=IPNT+1
          IROW=IBITS(IW(KTPAD+IPNT),0,4)
          IPNT=IPNT+1
          NHIT=IW(KTPAD+IPNT)
C
C++       Loop over hits in this padrow to see if there are any bad hits
C
          NG=0
          DO 650 IH=1,NHIT
            IT0=IBITS(IW(KTPAD+IPNT+IH),0,9)
            IF (IT0.GT.0) NG=NG+1
  650     CONTINUE
C
C++       Copy the row header to the new bank if there are any good hits
C++       and the row number has actually changed
C
          IF (NG.GT.0) THEN
            IF (IROW.NE.IRLST) THEN
              IPNT1=IPNT1+1
              IW(KTPAD+IPNT1)=IW(KTPAD+IPNT-1)
              IPNT1=IPNT1+1
              IW(KTPAD+IPNT1)=NG
              IRLST=IROW
              IPRW=IPNT1
            ELSE
C
C++           If the row number has not changed (due to an spurious
C++           row getting inserted in a good one), then we just
C++           increment the number of hits in the previous row header,
C++           the position of which was marked by IPRW
C
              IW(KTPAD+IPRW)=IW(KTPAD+IPRW)+NG
            ENDIF
          ENDIF
C
C++       Copy the good hits on this row and their digitizations
C
          DO 750 IH=1,NHIT
            IPNT=IPNT+1
            IHTWD=IW(KTPAD+IPNT)
            IT0=IBITS(IHTWD,0,9)
            NBK=IBITS(IHTWD,16,8)
            IF (IT0.GT.0) THEN
              IPNT1=IPNT1+1
              IW(KTPAD+IPNT1)=IHTWD
              DO 735 II=1,NBK
                CALL MVBITS(IW(IWRD),IBT,8,IW(IWRD1),IBT1)
                IBT=IBT-8
                IF (IBT.LT.0) THEN
                  IBT=24
                  IWRD=IWRD+1
                ENDIF
                IBT1=IBT1-8
                IF (IBT1.LT.0) THEN
                  IBT1=24
                  IWRD1=IWRD1+1
                ENDIF
  735         CONTINUE
            ELSE
              DO 736 II=1,NBK
                IBT=IBT-8
                IF (IBT.LT.0) THEN
                  IBT=24
                  IWRD=IWRD+1
                ENDIF
  736         CONTINUE
            ENDIF
  750     CONTINUE
C
          GO TO 620
  621   CONTINUE
C
C++     Set the length of TPAD and TPDI
C
        CALL AUBOS('TPAD',ISLOT,IPNT1,KTPAD,IGARB)
        IF (IBT1.EQ.24) THEN
          LEN=IWRD1-KTPDI-1
        ELSE
          LEN=IWRD1-KTPDI
        ENDIF
        CALL AUBOS('TPDI',ISLOT,LEN,KTPDI,IGARB)
C
  801 CONTINUE
C
  999 RETURN
      END
#endif
