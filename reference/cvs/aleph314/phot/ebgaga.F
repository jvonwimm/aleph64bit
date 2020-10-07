      SUBROUTINE EBGAGA( KOUIN , GAM1 , GAM2 )
C -----------------------------------------------------------------
C   AUTHOR   : J.Badier   18/06/90
C!  Multi gammas compactness.
CKEY PHOTONS
C
C               KOUIN   = 0 Input
C                       = 1 Output
C               GAM1    First gamma
C               GAM2    Second gamma
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C     CREATED : NONE
C
C ----------------------------------------------------
#ifndef DOC
      SAVE
      DATA GAG1 / 0. / , GAG2 / 0. /
      IF( KOUIN .EQ. 0 ) THEN
        GAG1 = GAM1
        GAG2 = GAM2
      ELSE
        GAM1 = GAG1
        GAM2 = GAG2
      ENDIF
      RETURN
      END
#endif
