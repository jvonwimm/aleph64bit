       FUNCTION YFIXYN(CHARG,X,Y,RO,FI,D0)
C------------------------------------------------------------
C!Finds the psi angle at the point (x,y) def mod twopi
CKEY YV0 /USER
C
C   AUTHOR :M.A. CIOCCI,L.ROLANDI  7-4-88
C   MODIFIED:
C
C
C       INPUT:
C              CHARG/REAL    TRACK CHARGE
C              X,Y/REAL      POINT COORDINATES ON HELIX
C              RO/REAL       RADIUS OF CURVATURE NOT SIGNED
C              FI/REAL       FI0 (SEE FRFT BANK)
C              D0/REAL       D0  (D0=-CHARGE*D0BANK FRFT)
C
C    CALLED BY User
C
C
C--------------------------------------------
      SAVE
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
       YFIXYN=ATAN2(-(CHARG*X-(RO-D0)*SIN(FI)),
     $ CHARG*Y+(RO-D0)*COS(FI))
       YFIXYN=YFIXYN-FI
      IF (YFIXYN.LT.-PI) THEN
        YFIXYN=YFIXYN+TWOPI
      ELSEIF (YFIXYN.GT.PI) THEN
        YFIXYN=YFIXYN-TWOPI
      ENDIF
      YFIXYN=-CHARG*YFIXYN
       RETURN
       END
