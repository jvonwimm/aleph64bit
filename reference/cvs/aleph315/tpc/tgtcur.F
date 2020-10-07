      SUBROUTINE TGTCUR(TCURA,TCURB,ADOUBT,BDOUBT,IER)
C----------------------------------------------------------------------
C! Read TPC currents in each side of TPC from TCUR database bank.
C! N.B. TCUR bank only exists for very few runs. It can be created
C! using info. from online time-chart information.
C!
C!  Author    :   I. Tomalin  94/07/01
C!
C!   Input:  TCUR database bank.
C!   Output: TCURA /R  : Current in side A of TPC (measured in nA).
C!           TCURB /R  : Current in side B of TPC (measured in nA).
C!   ADOUBT, BDOUBT R/ : Max. amount which current can be wrong by
C!                       on sides A and B respectively (nA).
C!                       (Set to zero if first or last current
C!                        measurement in run).
C!           IER   /I  : Non-zero if error occured (in which case
C!                       TCURA & TCURB set equal to something reasonable
C!   Calls: ALEPHLIB routine TIMDIF.
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA NERR/0/
C
#include "bcs.h"
C
#include "runhjj.h"
#include "evehjj.h"
#include "tcurjj.h"
C
C Specify typical average current (approximate).
      PARAMETER(CURAVE = 450.0)
C Specify minimum believable current.
      PARAMETER(CURMIN = 300.0)
C Specify maximum time in minutes to nearest current reading for it
C to be believed.
      PARAMETER(TIMMAX = 30.0)
C Number of bits for each information in TCUR bank.
      PARAMETER(NTBITS=10,NABITS=11,NBBITS=11)
C
#include "bmacro.h"
C----------------------------------------------------------------------
C Initialize.
      IF (FIRST) THEN
        FIRST = .FALSE.
        LRUN  = 0
        LEVT  = 0
        NRUNH = NAMIND('RUNH')
        NEVEH = NAMIND('EVEH')
      ENDIF
C
      IER = 0
C Initialize currents to something sensible.
      TCURA = CURAVE
      TCURB = CURAVE
      ADOUBT = 0.0
      BDOUBT = 0.0
C
      KRUNH = IW(NRUNH)
      KEVEH = IW(NEVEH)
      IF (KRUNH.EQ.0.OR.KEVEH.EQ.0) THEN
        IER = 1
        WRITE(IW(6),25) KRUNH,KEVEH
   25   FORMAT(' TGTCUR ERROR: RUNH/EVEH banks missing ',2I8)
         CALL EXIT
      END IF
C
C Find current run and event number.
      IRUN = IW(KEVEH + JEVERN)
      IEVT = IW(KEVEH + JEVEEV)
C
C See if run or event number have changed and act accordingly.
C
      IF (IRUN.EQ.LRUN) THEN
        IF (IEVT.EQ.LEVT) THEN
C They have not changed so neither has the TPC current.
          TCURA  = TCURAL
          TCURB  = TCURBL
          ADOUBT = ADOUBL
          BDOUBT = BDOUBL
          IER = IERL
          GOTO 999
        ELSE
C Event number has changed, so relink to TCUR bank.
          LEVT = IEVT
          KTCUR  = NLINK('TCUR',IRUN)
        END IF
      ELSE
C Run number has changed, so load new TCUR bank.
        IF (LRUN.GT.0) KTCUR = NDROP ('TCUR',LRUN)
        LRUN   = IRUN
        LEVT   = IEVT
        IRNEXT = 1
        LDBAS  = JUNIDB(0)
        KTCUR  = MDARD (IW,LDBAS,'TCUR',IRUN)
      END IF
C
      IF (KTCUR.GT.0) THEN
C++ Find time since start of run in minutes.
        TSINCE = TIMDIF(IW(KRUNH + JRUNSD),IW(KRUNH + JRUNST),
     +                  IW(KEVEH + JEVEDA),(IW(KEVEH + JEVETI)+50)/100)
        IF (TSINCE.LT.-1.0.OR.TSINCE.GT.10.0*24.0*60.0) THEN
          IER = 2
          NERR = NERR + 1
          IF (NERR.LE.100) WRITE(IW(6),50) IRUN,TSINCE,
     +    IW(KRUNH + JRUNSD),IW(KRUNH + JRUNST),IW(KEVEH + JEVEDA),
     +    IW(KEVEH + JEVETI)
   50     FORMAT(' TGTCUR WARNING: Timing suspicious ',I7,F15.3,4I10)
C
        ELSE
C Find rows in TCUR bank neighbouring this time.
  100     CONTINUE
C Unpack time from TCUR bank.
          NCURPK = IW(KTCUR + LMHLEN + IRNEXT)
          TNEXT = 0.1*FLOAT(IBITS(NCURPK,0,NTBITS))
          IF (TSINCE.GT.TNEXT) THEN
            IRNEXT = IRNEXT + 1
            IF (IRNEXT.LE.LROWS(KTCUR)) GOTO 100
          END IF
          IRLAST = MAX(IRNEXT - 1,1)
          LCURPK = IW(KTCUR + LMHLEN + IRLAST)
          TLAST = 0.1*FLOAT(IBITS(LCURPK,0,NTBITS))
          IRNEXT = MIN(IRNEXT,LROWS(KTCUR))
C
C Distrust current info. if TIMMAX minutes out of date.
          IF (MIN(ABS(TSINCE - TLAST),ABS(TSINCE - TNEXT)).GT.TIMMAX)
     +    THEN
            IER = 3
            NERR = NERR + 1
            IF (NERR.LE.100)
     +      WRITE(IW(6),125) IRUN,TSINCE,TLAST,TNEXT,
     +      IW(KRUNH + JRUNSD),IW(KRUNH + JRUNST),IW(KEVEH + JEVEDA),
     +      IW(KEVEH + JEVETI)
  125       FORMAT(' TGTCUR WARNING: Time out',I7,3F15.3,4I10)
          ELSE
            IF (IRNEXT.EQ.IRLAST) THEN
              RATIO = 1.0
            ELSE
              RATIO = (TSINCE - TLAST)/(TNEXT - TLAST)
            END IF
C
            TCURAL = FLOAT(IBITS(LCURPK,NTBITS,NABITS))
            TCURAN = FLOAT(IBITS(NCURPK,NTBITS,NABITS))
C
            TCURBL = FLOAT(IBITS(LCURPK,NTBITS+NABITS,NBBITS))
            TCURBN = FLOAT(IBITS(NCURPK,NTBITS+NABITS,NBBITS))
C
            IF (MIN(TCURAL,TCURAN,TCURBL,TCURBN).GT.CURMIN) THEN
              TCURA  = RATIO*TCURAN + (1.0 - RATIO)*TCURAL
              TCURB  = RATIO*TCURBN + (1.0 - RATIO)*TCURBL
              ADOUBT = MAX(ABS(TCURA - TCURAL),ABS(TCURA - TCURAN))
              BDOUBT = MAX(ABS(TCURB - TCURBL),ABS(TCURB - TCURBN))
            ELSE
              IER = 4
              NERR = NERR + 1
              IF (NERR.LE.100)
     +        WRITE(IW(6),135) IRUN,IRLAST,IRNEXT,TCURAL,TCURAN,
     +        TCURBL,TCURBN
  135         FORMAT(' TGTCUR WARNING: TPC current zero. ',3I8,4F10.3)
            END IF
C Print out current once per event for debug purposes.
C           WRITE(6,771) IRUN,IEVT,TSINCE,TLAST,TCURAL,TCURBL,
C    +      TNEXT,TCURAN,TCURBN,TCURA,TCURB
C 771       FORMAT(' RUN=',2I6,' NOW=',F7.2,' L=',3F8.2,
C    +      ' N=',3F8.2,' CUR=',2F8.2)
          END IF
        END IF
C
      ELSE
        IER = 5
        NERR = NERR + 1
        IF (NERR.LE.100) WRITE(IW(6),150) IRUN
  150   FORMAT(' TGTCUR WARNING: TCUR bank missing. ',4I8)
      END IF
C
  999 CONTINUE
C Store results.
      TCURAL = TCURA
      TCURBL = TCURB
      ADOUBL = ADOUBT
      BDOUBL = BDOUBT
      IERL = IER
      END
#endif
