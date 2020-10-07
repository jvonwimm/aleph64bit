      SUBROUTINE MDIPBT
CKEY NANO in ALPHA / INTERNAL
C----------------------------------------------------------------------
C!  - Impact parameter Btag calculation for the MINI (QIPBTAG, D.Brown)
C!
C!   Author   :- Yves A. Maumary (for the NANO )    28-SEP-1992
C!   Modified :- Dave Brown + Gerrit Graefe
C!               02-SEP-1994  fill variables in /NCOMIP/ for MINI/NANO
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
      SAVE FIRST,IYEAR,NASIM,NRUNH,NERR
#include "qdecl.h"
#include "qcde.h"
#include "ncomip.h"
      INTEGER NAMIND
      INTEGER MXTRK,MXJET
      PARAMETER ( MXTRK = 400 , MXJET = 20 )
      INTEGER INJET(MXJET)
      REAL    PROBH(2),PROBJ(MXJET),PROBT(MXTRK)
      CHARACTER*50 MSG
      INTEGER IBIGJ,IJET,IAXIS
      REAL    BIGE,COSA
      INTEGER IYEAR,NASIM,JASIM,NRUNH,JRUNH
      INTEGER NERR
      DATA NERR/0/
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER ITRK,TNUM,IWORD,ISIG
      REAL    BIPTAG,SIG
#include "maxtrk.h"
#include "btagraw.h"
#include "asimjj.h"
#include "runhjj.h"
#include "btaginl.h"
#include "qmacro.h"
C-----------------------------------------------------------------------
C
C!.TEST DATA TAKING PERIOD: VDET AVAILABLE ONLY SINCE 1991
      IF(FIRST)THEN
        FIRST = .FALSE.
        IYEAR = 0
C!..MONTE CARLO
        IF(XMCEV)THEN
          NASIM = NAMIND('ASIM')
          JASIM = IW(NASIM)
          IF(JASIM.GT.0)THEN
            IYEAR = ITABL(JASIM,1,JASIYM)/100
          ELSE
            NRUNH = NAMIND('RUNH')
            JRUNH = IW(NRUNH)
            IF(JRUNH.GT.0) IYEAR = IW(JRUNH+JRUNSD)/10000
          ENDIF
        ELSE
C!..DATA
          IYEAR = 89
          IF(KRUN.GE. 5910) IYEAR = 90
          IF(KRUN.GE. 9115) IYEAR = 91
          IF(KRUN.GE.13494) IYEAR = 92
          IF(KRUN.GE.17999) IYEAR = 93
        ENDIF
        IF(IYEAR.LT.91) CALL QWMESS
     &    ('0###### No VDet for this period: QIPBTAG will NOT be run')
C
        call BTAG_PAR
      ENDIF
C
      IF(IYEAR.LT.91) RETURN
C
      CALL QIPBTAG(IBTRET,NBTTRK,NBTJET,INJET,INTRK,PROBT,PROBJ,PROBH,
     &             BIPTAG)
      IF(IBTRET.GT.0)THEN
        NERR = NERR + 1
        IF(NERR.LE.10 .OR. (KNEVT.GE.KDEBU1.AND.KNEVT.LE.KDEBU2))THEN
          MSG = '+++ MDIPBT +++ Event not useful for QIPBTAG. rc=##'
          WRITE(MSG(49:50),'(I2)') IBTRET
          CALL QWMESE(MSG)
        ENDIF
        CALL HF1(2,61.5,1.)
        RETURN
      ELSEIF(IBTRET.LT.0)THEN
        MSG = '### MDIPBT ### Error in QIPBTAG. rc=##'
        WRITE(MSG(37:38),'(I2)') IBTRET
        CALL QWMESE(MSG)
        CALL HF1(3,25.5,1.)
        RETURN
      ENDIF
C
C!..Pack the track words; save dmin/sig (signed), plus a track flag
C!..including the track,jet,hemisphere, and track type
C
      DO ITRK=1,NBTTRK
        IF(TTYPE(TFLAG(ITRK)).EQ.3)THEN
C
C!..V0 track #s are *10000
C
          TNUM = INTRK(ITRK)/10000
        ELSE
          TNUM = INTRK(ITRK)
        END IF
        IWORD = TNUM                 ! first 8 bits; track #
     &        +ISHFT(TFLAG(ITRK),8)  ! next 14; qipbtag track flag
     &        +ISHFT(JJET(ITRK),22)  ! next 4 bits; jet assignment
     &        +ISHFT(JHEMI(ITRK),26) ! next 2 bits; hemisphere assignment
        INIPAR(ITRK,1) = IWORD
C
C!..Store the significance, integerized in units of 10**6
C
        SIG = DMIN(ITRK)/S_DMIN(ITRK)
        ISIG = NINT(SIG*1.0E6)
        INIPAR(ITRK,2) = ISIG
      END DO
C
C!..Done
C
      END
#endif
