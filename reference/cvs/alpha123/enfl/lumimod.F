      SUBROUTINE LUMIMOD
C-----------------------------------------------------------------------
C! Compute pad - wire differences in LCAL
C
C  Patrick Janot - 16 May 1990
C-----------------------------------------------------------------------
#ifndef DOC
#include "parabank.h"
#include "parcut.h"
#include "qcde.h"
      DIMENSION QT(KCQVEC,1), KT(KCQVEC,1), QV(KCQVRT,1), KV(KCQVEC,1)
#include "qhac.h"
#include "qmacro.h"
C-----------------------------------------------------------------------
      CALL vzero(wirl(1),4)
      CALL vzero(padl(1),4)
      CALL vzero(difl(1),4)
      IF ( nplsd .LE. 0 .AND. nplpd .LE. 0 ) GOTO 999
C
C -- Correction factor of raw pad energy ( due to thresholds )
C
      facteur = .08
      iecns = 0
      naecns = namind('ECNS')
      IF ( naecns .GT. 0 ) iecns = iw(naecns)
      IF ( iecns .GT. 0 ) facteur = rtabl(iecns, 1, 26)
C
C -- Wire energy  per module
C
      DO 1 kplan = 1 , nplpd
        iaddr = itabl(iplpd, kplan, jplpad)
        modu  = iaddr/64 + 1
        IF ( modu .LE. 0 .OR. modu .GT. 4 ) GOTO 1
        ipla  = iaddr - 64*(modu-1)
        IF ( ipla .LE. 0 .OR. ipla .GE. 39 ) GOTO 1
        enel  = itabl(iplpd, kplan , jplpen) * 1.E-3
        wirl(modu) = wirl(modu) + enel
    1 CONTINUE
C
C -- Pad energy per module
C
      DO 2 itow = 1 , nplsd
        iaddr = itabl(iplsd, itow, jplsad)
        modu  = iaddr/512 + 1
        IF ( modu .LE. 0 .OR. modu .GT. 4 ) GOTO 2
        iclu  = itabl(iplsd, itow, jplspe)
        IF ( iclu .LE. 0 ) GOTO 2
        eraw  = rtabl(ipeco, iclu, jpecer)
        corr = 1.
        IF ( eraw .GT. 0.01) corr = 1. + facteur/sqrt(eraw)
        efrac = itabl(iplsd, itow, jplse1)
     .        + itabl(iplsd, itow, jplse2)
     .        + itabl(iplsd, itow, jplse3)
        efrac = efrac * 5.E-3 * corr
C       IF ( krun .LT. 4000 ) efrac = efrac * 1.45
        padl(modu) = padl(modu) + efrac
    2 CONTINUE
C
C  Difference pad-wire
C
      DO 3 modu = 1 , 4
        difl(modu) = padl(modu)-wirl(modu)
        IF ( idbg .GE. 10 ) WRITE (IW(6),*) 'Module/Pad/Fil/Diff : ',
     .  modu,padl(modu),wirl(modu),difl(modu)
    3 CONTINUE
C
C  That's it
C
  999 RETURN
      END
#endif
