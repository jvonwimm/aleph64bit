      SUBROUTINE GOODMOD(KEEP)
C---------------------------------------------------------------------
C! Compute pad - wire differences in ECAL
C
C  Patrick Janot -- 18 Apr 1990
C---------------------------------------------------------------------
#ifndef DOC
#include "parabank.h"
#include "parcut.h"
#include "qcde.h"
      DIMENSION QT(KCQVEC,1), KT(KCQVEC,1), QV(KCQVRT,1), KV(KCQVEC,1)
#include "qhac.h"
#include "qmacro.h"
C---------------------------------------------------------------------
      keep = 0
      CALL vzero(epad(1)  ,36)
      CALL vzero(ewire(1) ,36)
      CALL vzero(ediff(1) ,36)
      IF ( iewhe .EQ. 0 ) GOTO 999
      IF ( ipewi .EQ. 0 ) GOTO 998
      IF ( ipeco .EQ. 0 ) GOTO 999
      IF ( ipest .EQ. 0 ) GOTO 998
      IF ( ietdi .EQ. 0 ) GOTO 998
      IF ( newhe .NE. 36 ) GOTO 999
C
C  Total wire energy (module by module)
C
      modbad = 0
      DO 100 iwir = 1 , newhe
        modu = itabl(iewhe, iwir, 2)
        IF ( modu .NE. iwir ) modbad = modbad + 1
        ewire(iwir) = itabl(iewhe, iwir, 1) * 1E-6 * rpsf
 100  CONTINUE
      IF ( modbad .NE. 0 ) THEN
        WRITE (IW(6),*) krun,kevt,' : +++ Warning +++ EWHE corrupted !!'
        WRITE (IW(6),*) 'modbad = ',modbad
        IF ( idbg .GE. 1 ) CALL looses('EWHECORR',1)
      ENDIF
C
C  Correction factor on the raw pad energy ( due to thresholds )
C
      facteur = .08
      iecns = 0
      naecns = namind('ECNS')
      IF ( naecns .GT. 0 ) iecns = iw(naecns)
      IF ( iecns .GT. 0 ) facteur = rtabl(iecns, 1, 26)
C
C  Loop on calobjects of PEST ( LCAL excluded !)
C  ( iclu = PECO corresponding , modu = module )
C
      DO 2 isto = 1 , npest
        iclu = itabl(ipest, isto, jpespe)
        IF ( itabl(ipest, isto, jpesed) .NE. 0) GOTO 2
        IF ( itabl(ipeco, iclu, jpeckd) .EQ. 192 ) GOTO 2
C
        eraw = rtabl(ipeco, iclu, jpecer)
        IF ( eraw .LE. 0. ) CALL bldpeco(iclu,eraw)
C
        ntow  = itabl(ipest, isto, jpeset)
        itlab = itabl(ietdi, ntow, jetdtl)
        irow  = ibits(itlab, 16, 8)
        icol  = ibits(itlab, 2, 9)
C
C  Check the reliability of Row/Column numbers
C
        IF ( invalid(irow,icol) .EQ. 0 ) GOTO 2
        CALL emdtow(irow, icol, ksbd, kmodu, krg)
        modu  = kmodu + 12*(ksbd-1)
        IF ( modu .LT. 1 .OR. modu .GT. 36) GOTO 2
C
        corr = 1.
        IF ( eraw .GT. 0.01) corr = 1. + facteur/sqrt(eraw)
C
        efrac = rtabl(ipest, isto, jpeser) * corr
        epad(modu) = epad(modu) + efrac
    2 CONTINUE
C
C  Determine the most energetic wire plane
C
      DO 4 kmodu = 1 , npewi
        totenw = 0.
        frcpla = 0.
        kwirep = 0
        frcpl0 = 0.
        kwire0 = 0
        DO 5 kplan = 2 , 46
          plafrc = ITABL(ipewi, kmodu, kplan)
          IF ( plafrc .GT. -10000. ) THEN
            totenw = totenw + plafrc
            IF ( plafrc .GT. frcpla ) THEN
              frcpla = plafrc
              kwirep = kplan-1
            ENDIF
            IF ( plafrc .GT. frcpl0 .AND. plafrc .LT. frcpla ) THEN
              frcpl0 = plafrc
              kwire0 = kplan-1
            ENDIF
          ENDIF
    5   CONTINUE
        IF ( frcpl0 .EQ. 0. ) frcpl0 = frcpla/1000.
        frcpl0 = frcpla / frcpl0
        frcpla = frcpla / totenw
        IF (( frcpla .GT. .9  .AND. totenw .GT. .5E6 ) .OR.
     .      ( frcpl0 .GT. 20. .AND. totenw .GT. 1.E6 ) .OR.
     .      ( frcpl0 .GT.  5. .AND. totenw .GT. 5.E6 )) THEN
          jmodu = ITABL(ipewi,kmodu,jpewmn)
          ewire(jmodu) = ewire(jmodu) * (1. - frcpla)
          IF ( idbg .GE. 11 ) WRITE (IW(6),*) 'Wire energy in module ',
     &         jmodu,' has been rescaled by a factor ',1.-frcpla
        ENDIF
    4 CONTINUE
C
C  Now the "Good"  modules :
C  Kill the event if a module has a crazy wire energy value
C
      DO 3 modu = 1 , 36
        IF ( ewire(modu) .GT. 200. ) GOTO 999
        ediff(modu) = epad(modu) - ewire(modu)
        IF ( ediff(modu) .LE. 0. ) ediff(modu) = 0.
   3  CONTINUE
C
 998  keep = 1
C
 999  RETURN
      END
#endif
