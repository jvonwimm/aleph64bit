      SUBROUTINE DLOCKS
C----------------------------------------------------------------
C! Unlock all the ALPHA-particles which have been locked before
C  during the energy flow algorithm
C
C  Patrick Janot -- 21 june 1990
C----------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
      DIMENSION QT(KCQVEC,1), KT(KCQVEC,1), QV(KCQVRT,1), KV(KCQVEC,1)
#include "qhac.h"
#include "qmacro.h"
C----------------------------------------------------------------
C  First unlock the QLOCKed particles
C
      CALL qlzer(kreco)
C
C  Then unlock the QLITKed particles (charged, ECALobj, HCALobj)
C
      DO ich = kfcht , klcht
        IF ( xlock(ich) ) CALL qlutrk(ich)
      ENDDO
C
      iecobj = kpdir('ECAL',kreco)
    1 IF ( iecobj .EQ. 0 ) GOTO 2
      IF ( xlock(iecobj) ) CALL qlutrk(iecobj)
      iecobj = kfollo(iecobj)
      GOTO 1
C
    2 ihcobj = kpdir('HCAL',kreco)
   11 IF ( ihcobj .EQ. 0 ) GOTO 999
      IF ( xlock(ihcobj) ) CALL qlutrk(ihcobj)
      ihcobj = kfollo(ihcobj)
      GOTO 11
C
  999 RETURN
      END
#endif
