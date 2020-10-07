      SUBROUTINE EBCOSI( TETA , PHI , ISC , COSI )
C ----------------------------------------------------------------------
C   AUTHOR   : J.Badier    17/04/89
C!  Incidence angle by respect to the module.
CKEY PHOTONS GAMMA INCIDENCE /  INTERNAL
C   COSI is the cosinus of the incidence angle on the module
C   layer accordingly to the classical optical definition.
C
C   Input : TETA        Polar angle in radians.
C           PHI         Azimuthal angle in radians.
C           ISC         Subcomponent number.
C
C   Output: COSI        Cosinus of the incidence angle.
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C     CREATED : NONE
C
C   Calls EFNDMD , ENTREE
C   Called by EBPRGA , EBLPST , EBSLIM
C-----------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION XP(3) , PLAN(4,8)
      INTEGER  EFNDMD
      EXTERNAL EFNDMD
C ----------------------------------------------------
      IF( ISC .NE. 2 ) THEN
C   Petal.
        COSI = COS( TETA )
        COSI = ABS( COSI )
      ELSE
C   Barrel.
        SNT = SIN( TETA )
        XP(1) = COS( PHI ) * SNT
        XP(2) = SIN( PHI ) * SNT
        XP(3) = COS( TETA )
C   Search module number.
        MDL = EFNDMD( ISC , XP )
C   Orientation of the /ISC,MDL/ module layers.
        CALL ENTREE( 'ALEPH' , ISC , MDL , PLAN )
C   COSI = cos(incidence).
        COSI = PLAN(1,1)*XP(1) + PLAN(2,1)*XP(2) + PLAN(3,1)*XP(3)
      ENDIF
      RETURN
      END
#endif
