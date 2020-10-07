      SUBROUTINE EBSATU(ISAT , NATU , ENER , CORR )
C ----------------------------------------------------
C   AUTHOR   : J. Badier     05/10/89
C! Saturation correction
CKEY PHOTONS SATURATION / INTERNAL
C
C   The correction is applied to the isolated gammas.
C
C   Input     : ISAT    Control word.
C               NATU    Particle nature.
C               ENER    Energy
C
C   Output    : CORR    Correction factor.
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C
C ----------------------------------------------------
#ifndef DOC
      SAVE
      PARAMETER ( ESAT = .00078 )
      CORR = 1.
      CALL KIAVER(AVER,IPROG)
C Saturation correction if not Monte-Carlo
      IF (IPROG  .EQ. 0) THEN
         IF( NATU .LE. 3 ) CORR = 1. + ESAT * ENER
      ENDIF
      RETURN
      END
#endif
