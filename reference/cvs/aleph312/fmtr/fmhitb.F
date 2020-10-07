      SUBROUTINE FMHITB(IRG,PAR,CHARG,PAREX,SLENG,ICODE)
C------------------------------------------------------------------
C! Find intersection of track with boundary of region IRG
C------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "fmgeom.h"
C
      DIMENSION PAR(7),PAREX(7),FIELD(3),SPAR(7),BUFIN(7),BUFOUT(7)
      DIMENSION XYZ(3)
C
      DATA EPSIL/0.08/
      DATA STFRAC / .8 /
      RMIN = FMRMIN (IRG)
      RMAX = FMRMAX (IRG)
      ZMAX = FMZMAX (IRG)
      ZMIN = FMZMIN (IRG)
      CALL UFIELD(PAR,IRG,FIELD)
      BTOT = SQRT(FIELD(1)**2 + FIELD(2)**2 + FIELD(3)**2)
C
C      IF (IRG.EQ.1.OR.IRG.EQ.2.OR.IRG.EQ.4.OR.IRG.EQ.5.OR.IRG.EQ.10)
C
      IF (IRG.EQ.1.OR.IRG.EQ.2.OR.IRG.EQ.4.OR.IRG.EQ.5.OR.IRG.EQ.10)
     & GOTO 101
      IF (IRG.EQ.3.OR.(IRG.GE.6.AND.IRG.LE.9).OR.IRG.EQ.11) GOTO 102
C
101      IF (BTOT.GT.0.001) THEN
         CALL AUHCYL (RMAX,ZMAX,FIELD(3),CHARG,PAR,PAREX,ICODE)
         IF (ICODE.EQ.0) GOTO 999
         SLENG = (PAREX(3) - PAR(3))/PAR(6)
      ELSE
         CALL AULCYL (RMAX,ZMAX,PAR,PAREX,ICODE)
         IF (ICODE.EQ.0) GOTO 999
         SLENG = SQRT((PAR(1)-PAREX(1))**2 +
     &                   (PAR(2)-PAREX(2))**2 +
     &                   (PAR(3)-PAREX(3))**2)
      ENDIF
      GOTO 999
C
C  Use a different algorithm to calculate the intersection and path
C  length in a region of nonsolenoidal or ill behaved magnetic field.
C
C  First determine the path length of a straight trajectory to the
C  region wall
C
  102 CALL UCOPY(PAR,BUFIN,7)
      SLENG = 0.
  103 DRDS = 2 * ((BUFIN(1) * BUFIN(4)) + (BUFIN(2) * BUFIN(5)))
C
      IF (DRDS.GE.0) THEN
         CALL AULCYL (RMAX,ZMAX,BUFIN,SPAR,ICODE)
         IF (ICODE.EQ.0) CALL AULCYL(RMIN,ZMAX,BUFIN,SPAR,ICODE)
      ENDIF
C
      IF (DRDS.LT.0) THEN
         CALL AULCYL (RMIN,ZMAX,BUFIN,SPAR,ICODE)
         IF (ICODE.EQ.0) CALL AULCYL(RMAX,ZMAX,BUFIN,SPAR,ICODE)
      ENDIF
C
      IF (ICODE.EQ.0) GOTO 999
      STEP = SQRT((BUFIN(1)-SPAR(1))**2 +
     &                   (BUFIN(2)-SPAR(2))**2 +
     &                   (BUFIN(3)-SPAR(3))**2)
      STEP = STEP * STFRAC
C
C  Now stfrac is a multiplier less than one; it may be tuned for more
C  rapid convergence of the algorithm.  Now take a step of this size.
C
  104 CALL FMHELX(CHARG,STEP,BUFIN,BUFOUT)
C
C  Test to see which region we are in:
C
      CALL UCOPY(BUFOUT(1),XYZ(1),3)
      CALL FMREGN(XYZ,JRG)
C
C  Propagate by small step and test again
C
      XYZ(1) = BUFOUT(1) + EPSIL * BUFOUT(4)
      XYZ(2) = BUFOUT(2) + EPSIL * BUFOUT(5)
      XYZ(3) = BUFOUT(3) + EPSIL * BUFOUT(6)
      CALL FMREGN(XYZ,KRG)
C
C  If the step has carried us into a new region than take a smaller step
C  If the step has taken us to within EPSIL of the boundary then
C  stop and store BUFOUT as PAREX.  If we are still within the region
C  then repeat the process another time.
C
      IF (JRG.NE.IRG) THEN
         STEP = STEP / 2.
         GOTO 104
      ELSEIF (KRG.NE.IRG) THEN
         CALL UCOPY(BUFOUT(1),PAREX(1),7)
         SLENG = SLENG + STEP
      ELSE
         SLENG = SLENG + STEP
         CALL UCOPY(BUFOUT(1),BUFIN(1),7)
         GOTO 103
      ENDIF
C
C  (NB we do not consider the case where the z-intersection is at ZMIN)
C
      IF (ABS(PAREX(3)).LE.ZMIN) ICODE=0
  999 RETURN
      END
#endif
