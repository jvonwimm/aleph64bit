      SUBROUTINE QAPNAM (CPAR,CANT,IFLG, IPAR,IANT)
CKEY PART /INTERNAL
C----------------------------------------------------------------------
C! get particle codes from particle names; check part-antip relation;
C! check whether particles do exist already
C  IFLG = 0 : particles should not yet exist
C  IFLG = 1 : particles should already exist
C  IFLG = -1 : don't care
C called from QCPxxx
C!                                                  H.Albrecht 15.01.89
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      CHARACTER * (*) CPAR,CANT
      CHARACTER * 12 CPAR12,CANT12
C----------------------------------------------------------------------
C
      CPAR12 = CPAR
      CANT12 = CANT
      IPAR = KPARTX (CPAR(1:LENOCC(CPAR)),JPAR)
      IANT = KPARTX (CANT(1:LENOCC(CANT)),JANT)
      IF (IFLG .LT. 0)  GO TO 10
      IF (JPAR .EQ. 0 .AND. IFLG .NE. 0)
     &  CALL QWMESS ('_QAPNAM_ Modify particle ' // CPAR12 //
     &    ' (particle is unknown and now established)')
      IF (JANT .EQ. 0 .AND. IFLG .NE. 0)
     &  CALL QWMESS ('_QAPNAM_ Modify particle ' // CANT12 //
     &    ' (particle is unknown and now established)')
      IF (JPAR .NE. 0 .AND. IFLG .EQ. 0)
     &  CALL QWMESS ('_QAPNAM_ New particle ' // CPAR12 //
     &    '(particle has been established before)')
      IF (JANT .NE. 0 .AND. IFLG .EQ. 0)
     &  CALL QWMESS ('_QAPNAM_ New particle ' // CANT12 //
     &    '(particle has been established before)')
C
   10 LPAR = KOQPAR + IPAR * KCQPAR + JQPAAN
      LANT = KOQPAR + IANT * KCQPAR + JQPAAN
C
      IF (JPAR .NE. 0 .AND. JANT .NE. 0 .AND.
     &    IW(LPAR) .EQ. IANT .AND. IW(LANT) .EQ. IPAR)  GO TO 90
      IF (IW(LPAR) .NE. IPAR .OR. IW(LANT) .NE. IANT)
     &  CALL QWSYNT ('_QAPNAM_ Particle-antip relation conflict for ' //
     &    CPAR12 // CANT12)
      IW(LPAR) = IANT
      IW(LANT) = IPAR
C
   90 END
#endif
