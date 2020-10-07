      SUBROUTINE PHMADJ (LIST,IER)
C----------------------------------------------------------------------
C!   Change PHMA POT bank into HMAD JULIA bank
C
C   Author   :D. SCHLATTER              5-NOV-1988
C     Input :    LIST      BOS event list
C                          if LIST(2:2).eq.'-' drop POT banks
C
C     Output:    IER       = 0  successful
C                          = 1  input bank does not exist or is empty
C                          = 2  not enough space
C                          =-1  OK but garbage collection
C======================================================================
#ifndef DOC
      SAVE
#include "bcs.h"
#include "hmadjj.h"
#include "phmajj.h"
      CHARACTER*(*) LIST, PLIST*4, JLIST*4
      LOGICAL FIRST,BTEST
      DATA FIRST/.TRUE./
#include "bmacro.h"
C
C - set name-indices and bank formats ==============================
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        NAPHMA=NAMIND('PHMA')
        NAHMAD=NAMIND('HMAD')
        CALL BKFMT('HMAD','2I,(5I,2F,5I)')
      ENDIF
C
C - check existence of input banks, return if not there or empty =====
C
      IER=1
      IF(IW(NAPHMA).LE.0) GOTO 999
      NMUON=LROWS(IW(NAPHMA))
      IF(NMUON.LE.0) GOTO 999
C
C - create output bank(s), return if not booked =======================
C
      LNHMA=LMHLEN+NMUON*LHMADA
      CALL AUBOS('HMAD',0,LNHMA,KHMAD,IER)
      IF (IER.EQ.2) GOTO 999
      JLIST = 'HMAD'
      IW(KHMAD+LMHCOL)=LHMADA
      IW(KHMAD+LMHROW)=NMUON
C
C - fill output bank(s) ===========================================
C
      KPHMA=IW(NAPHMA)
      IND2=KHMAD+LMHLEN+JHMAMH-1
      IND1=KPHMA+LMHLEN
      DO 11 J=1,NMUON
        DO 10 I=1,LPHMAA
   10   IW(IND2+I)=IW(IND1+I)
        IND2=IND2+LHMADA
        IND1=IND1+LPHMAA
   11 CONTINUE
C
C          get # of fired/expected planes
C
      DO 25 IMU=1,NMUON
        I1=0
        I2=0
        I3=0
        DO 20 I=1,23
          IF( BTEST(ITABL(KHMAD,IMU,JHMAIE),I) ) I1=I1+1
          IF( BTEST(ITABL(KHMAD,IMU,JHMAIT),I) ) I2=I2+1
          IF(I.GE.12.AND.
     1        BTEST(ITABL(KHMAD,IMU,JHMAIE),I) ) I3=I3+1
   20   CONTINUE
        IW(KROW(KHMAD,IMU)+JHMANF)=I1
        IW(KROW(KHMAD,IMU)+JHMANE)=I2
        IW(KROW(KHMAD,IMU)+JHMANL)=I3
   25 CONTINUE
C
  998 CONTINUE
C - get the drop flag if any, then drop POT banks if required, =======
C   add JUL banks to S-list
C   POT banks are on PLIST, JUL banks on JLIST
C
      PLIST = 'PHMA'
#include "jplist.h"
C
C - if garbage collection then set IER = -1  =======================
C
      IF (IER .EQ. 1) IER = -1
C
C - return  ======================================================
  999 RETURN
      END
#endif
