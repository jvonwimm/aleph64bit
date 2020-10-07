      SUBROUTINE SREALD(IER)
C----------------------------------------------------------------------
C! Apply corrections to real data
C!
C!    Author:     H. Meinhard       12-Oct-1989
C!    Modified:   H. Meinhard       06-Dec-1989
C!    Modified:   H. Meinhard       20-Jun-1991
C!                recalculate TDC times for runs 11494 ... 11592
C!    Modified:   H. Meinhard       02-Jul-1991
C!                recalculate TDC times correctly
C!    Modified:   H. Meinhard       26-Aug-1991
C!                cable swap in 1991 data
C!
C!    Output:     - IER       /I    status flag
C!                                  0 = okay, or no raw data
C!                                  1 = SPED bank not found
C!
C!    Description
C!    ===========
C!    Apply corrections to the TDC times coming from real events
C!    that are necessary to correct for the different crate, card, and
C!    TDC number configuration
C!
C!    For Pilot run data only:
C!    Recalculate TDC times, and swap addresses of TDC's on side B.
C!    (The times specified in SRTD are calculated improperly, and have
C!    all to be recalculated using the SPED bank from the run header
C!    that contains the pedestals and slopes of the TDC's. Furthermore,
C!    the two connectors on the TDC cards of side B were swapped, and
C!    the signals must be swapped again to get proper results.)
C!
C!    For runs 5354 ... 5488 only:
C!    Swap TDC cards in crate 1 from side A to side B and vice versa
C----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "rcurnt.h"
#include "rlunit.h"
#include "sanamc.h"
#include "scalbc.h"
#include "srtdjj.h"
#include "sgeomc.h"
      PARAMETER(JSPESL=1,JSPEPE=2,JSPEES=3,JSPEEP=4,LSPEDA=4)
      INTEGER ITDC(16),ICAR(24)
#include "bosext.h"
      DATA ITDC / 8,9,10,11,12,13,14,15, 0,1,2,3,4,5,6,7 /
      DATA ICAR / 12,13,14,15,16,17,18,19,20,21,22,23,
     +             0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11 /
#include "bmacro.h"
#include "smacro.h"
C----------------------------------------------------------------------
C link to raw data bank
      JSRTD = IW(NASRTD)
      IF (JSRTD .EQ. 0)                                     GOTO 999
C
C link to SPED bank
      IF ((.NOT.FMCRUN .AND. IRUNRC .LE.  2666) .OR.
     +    (IRUNRC .GE. 11494 .AND. IRUNRC .LE. 11592)) THEN
        JSPED = IW(NAMIND('SPED'))
        IF (JSPED .EQ. 0)                                   GOTO 901
      ENDIF
C
C loop over raw data entries and apply corrections
      DO 300 IDAT = 1, LROWS(JSRTD)
        KSRTD = KROW(JSRTD,IDAT)
        IWORD = ITABL(JSRTD,IDAT,JSRTIR)
        ICRAT = JCRAT(IWORD)
        ICARD = JCARD(IWORD)
        ITDCN = JTDCN(IWORD)
        TIME = REAL(ITABL(JSRTD,IDAT,JSRTTR))/1.E12
C
C for pilot run data, recalculate TDC time
        IF ((.NOT.FMCRUN .AND. IRUNRC .LE.  2666) .OR.
     +      (IRUNRC .GE. 11494 .AND. IRUNRC .LE. 11592)) THEN
          ITDCC = JTDCC(IWORD)
C KCHAN is the current channel number [0,1151]
          KCHAN = (ICRAT-1)*MCRDSG*MTDCSG + (ICARD-1)*MTDCSG + ITDCN-1
C dead channel?
          KWD = IBITS(KCHAN,5,27) + 1
          KBT = IBITS(KCHAN,0,5)
          IF (IBITS(JWIRSC(KWD),KBT,1) .EQ. 0) THEN
            IW(KSRTD+JSRTTR) = 0
          ELSE
            KSPED = JSPED + LMHLEN + KCHAN*LSPEDA
            ISLOP = IW(KSPED+JSPESL)
C valid calibration?
            IF (ISLOP .EQ. 0) THEN
              WRITE (LOUTRL,100) ICRAT,ICARD,ITDCN
  100         FORMAT (' SREALD --> CRATE ',I1,', CARD ',I2,
     +          ', TDC ',I2,': BAD CALIBRATION')
              IW(KSRTD+JSRTTR) = 0
            ELSE
C recalculate TDC time
              SLOPE = REAL(ISLOP)/1000.
              PEDST = REAL(IW(KSPED+JSPEPE))/1000.
              IF (IRUNRC .LE. 2666) THEN
                TIME = (REAL(ITDCC) - PEDST) / SLOPE
              ELSE
                TIME = REAL(ITDCC) * SLOPE + PEDST
              ENDIF
              IW(KSRTD+JSRTTR) = INT(TIME*1000.)
              TIME = TIME * 1.0E-9
            ENDIF
          ENDIF
        ENDIF
C
C apply time corrections
        TIME = TIME + TCRASC(ICRAT)
        IF (ITDCN .GE. 3 .AND. ITDCN .LE. 6) TIME = TIME + TCHASC
C
C write time to bank
        IW(KSRTD+JSRTTR) = INT(TIME*1.E12)
C
C for pilot run data, if we are on side B, we have to swap the TDC
C channel numbers
        IF (.NOT.FMCRUN .AND. IRUNRC .LE. 2666) THEN
          ISIDE = (ICARD-1) / (MCRDSG/2) + 1
          IF (ISIDE .EQ. 2) CALL MVBITS(ITDC(ITDCN),0,4,
     +      IW(KSRTD+JSRTIR),24)
        ENDIF
C
C for runs 5354 ... 5488, we have to swap the TDC cards in crate 1
        IF (IRUNRC .GE. 5354 .AND. IRUNRC .LE. 5488) THEN
          IF (ICRAT .EQ. 1) CALL MVBITS(ICAR(ICARD),0,5,
     +      IW(KSRTD+JSRTIR),16)
        ENDIF
C
C for 1991 data, we have to swap the TDC channel numbers for a specific
C TDC card
        IF (IRUNRC .GT. 10000) THEN
          IF (ICRAT .EQ. 2 .AND. ICARD .EQ. 17) CALL MVBITS(ITDC(ITDCN),
     +      0,4,IW(KSRTD+JSRTIR),24)
        ENDIF
C
C bottom of loop over raw data entries
  300 CONTINUE
C
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      CALL RERROR('SREALD',-IER,'Missing SPED bank')
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
