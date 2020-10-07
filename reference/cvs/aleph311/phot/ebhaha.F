      SUBROUTINE EBHAHA( IFOT , PHOT , ENHAD )
C -----------------------------------------------------------------
C   AUTHOR   : J.Badier   18/06/90
C!  Hadrons for EBEGID
CKEY PHOTONS
C
C  Input  :     IFOT    Integer array of EBNEUT
C               PHOT    Real array of EBNEUT
C
C  Output :     ENHAD   Hadronic energy
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C     CREATED : NONE
C
C ----------------------------------------------------
#ifndef DOC
      SAVE
C PREMIERE APPROXIMATION.
      DIMENSION IFOT(*),PHOT(*)
      IF( PHOT(1) .GT. .25 ) THEN
        ENHAD = PHOT(4)
      ELSE
        ENHAD = 0.
      ENDIF
      RETURN
      END
#endif
