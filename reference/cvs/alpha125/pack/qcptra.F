      SUBROUTINE QCPTRA (CPAR,CANT, IEXT,IAXT)
CKEY PART /INTERNAL
C----------------------------------------------------------------------
C! set transformation table according to PTRA data card
C called from QMPART
C                                                   H.Albrecht 15.01.89
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      CHARACTER * (*) CPAR,CANT
      CHARACTER * 26 CBOTH
C----------------------------------------------------------------------
C
      CBOTH = CPAR
      CBOTH(LENOCC(CBOTH)+2:) = CANT
      IF (IEXT .LE. 0 .OR. IEXT .GT. 5000 .OR.
     &    IAXT .LE. 0 .OR. IAXT .GT. 5000)
     &    CALL QWSYNT ('_QCPTRA_ Wrong external particle code for ' //
     &      CBOTH)
      CALL QAPNAM (CPAR,CANT,-1,IPAR,IANT)
C
      IF ((IPAR .EQ. IANT .AND. IEXT .NE. IAXT) .OR.
     &  (IPAR .NE. IANT .AND. IEXT .EQ. IAXT))
     &  CALL QWSYNT ('_QMPTRA_ Particle -antip. conflict with ' //
     &  CBOTH)
      IF (IEXT .GE. IW(KOQTRA) .OR. IAXT .GE. IW(KOQTRA))
     &  CALL QSBANK ('QTRA', MAX (IEXT,IAXT) + 200)
C
C       negative value : set attributes from MC table
C
      IW(KOQTRA+IEXT) = - IPAR
      IW(KOQTRA+IAXT) = - IANT
      IW(KOQPBT+IPAR) = IOR (IW(KOQPBT+IPAR), KBIT(5))
      IW(KOQPBT+IANT) = IOR (IW(KOQPBT+IANT), KBIT(5))
C
C
      END
#endif
