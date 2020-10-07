      SUBROUTINE LUMIAD(IECOBJ)
C---------------------------------------------------------------------
C!  Correct LCAL clusters for clusterization effect and e/pi ratio
C
C  Patrick Janot -- 26 April 1990
C---------------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
#include "qcde.h"
#include "qhac.h"
#include "qmacro.h"
C---------------------------------------------------------------------
      corr = 1.
C
C  Correct for clusterization (IN LCAL only! 31/10/96)
C
      IF ( kpeckd(iecobj) .EQ. 192 ) THEN
C
        IF ( qpecer(iecobj) .GT. .001 )
     .       corr = 1. + .10/sqrt(qpecer(iecobj))
C
C  Take into account a had-oc e/pi ratio for low angle hadrons.
C
        vec(4) = (      qpece1(iecobj) + qpece2(iecobj) ) +
     .           ( 1. - qpece1(iecobj) - qpece2(iecobj) ) * epilca
      ELSE
        IF ( COS( qpecth(iecobj) ) .GT. 0. ) THEN
          vec(4) = sicapi(1)
        ELSE
          vec(4) = sicapi(2)
        ENDIF
      ENDIF
      IF ( idbg .GE. 11 ) WRITE (IW(6),*) 'e/pi factor : ',vec(4)
      IF ( vec(4) .LE. 1. ) vec(4) = 1.
      vec(4) = vec(4) * qpecec(iecobj) * corr
C
C  Correct pad-energy scale in Monte Carlo
C
C     IF ( krun .LT. 4000 .AND. kpeckd(iecobj) .EQ. 192 )
C    .     vec(4) = vec(4) * 1.45
C
C  Correct for LCAL crack
C
      IF ( kpeckd(iecobj) .EQ. 192 ) THEN
        phi = qpecph(iecobj) * 180./qqpi
        the = qpecth(iecobj) * 180./qqpi
        IF ( ABS(phi-90.) .LE. 2. .OR. ABS(phi-270.) .LE. 2. )
     .  vec(4) = SQRT ( vec(4) * (vec(4) + 2.) )
      ENDIF
C
C  And add the particle
C
      vec(3) = vec(4) * cos(qpecth(iecobj))
      vec(2) = vec(4) * sin(qpecth(iecobj)) * sin(qpecph(iecobj))
      vec(1) = vec(4) * sin(qpecth(iecobj)) * cos(qpecph(iecobj))
C
      IF ( kpeckd(iecobj) .EQ. 192 ) THEN
        CALL addvec(vec,5,ktn(iecobj),0,1.)
      ELSE
        CALL addvec(vec,6,ktn(iecobj),0,1.)
      ENDIF
      CALL qltrk(iecobj)
C
      RETURN
      END
#endif
