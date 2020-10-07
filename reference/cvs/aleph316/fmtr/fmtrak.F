      SUBROUTINE FMTRAK (VEC,Q,NRGN,IRGN,TPAR)
C-----------------------------------------------------------------
C!  Track through fiducial regions until the particle leaves the
C!  detector or its energy falls below some minimum value
C   INPUT:
C           VEC(7) = $x,y,z,dx/ds,dy/ds,dz/ds,|p| at start point
C             Q    = charge of track
C
C   OUTPUT:
C           NRGN   = No of regions crossed (Max = 11)
C           IRGN(*) = Region numbers crossed
C           TPAR(14*NRGN) = array of track parameters at each crossing
C               1 - 6. $x,y,z,dx/ds,dy/ds,dz/ds at entry to
C                      present volume
C                   7. Momentum |p| on exit from volume (after
C                      energy loss)
C               8 -13. Parameters after mutliple scattering on
C                      exit from present volume.
C                  14. Step length in present volume
C-----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "fmgeom.h"
C
      PARAMETER (NTRPAR = 14)
C
      DIMENSION VEC(7)
      DIMENSION IRGN(1),TPAR(1)
      DIMENSION PAR(7),PAREX(7)
      DIMENSION POUT(6)
      DIMENSION XYZ(3)
C Momentum limit for tracking
      DATA PMIN /0.2/
      DATA STEP /0.1/
C
      NRGN = 0
C
C Transfer start vector to internal array to avoid overwriting
C
      CALL UCOPY (VEC(1),PAR(1),7)
      CALL UCOPY (VEC(1),XYZ(1),3)
C  See which region we are in from the current position of the track
      CALL FMREGN (XYZ,IRG)
      IF(IRG.EQ.-1) GO TO 999
      IRLST = 0
C
C  Start of new step                   <<----------------------
C
   10 CONTINUE
C
C  Find intersection of track with boundary
C
      CALL FMHITB(IRG,PAR,Q,PAREX,SLENG,ICODE)
      IF (ICODE.EQ.0) GO TO 999
C
C  Now compute the error due to multiple scattering.
C
      PAREX(7) = PAR(7)
      CALL FMSCAT(PAREX,SLENG,.105659,FMRADL(IRG),POUT)
C
C  Get energy loss
C
      CALL FMELOS(IRG,PAR(7),SLENG,DP)
C
      PAR(7) = PAR(7) - DP
      IF (PAR(7).LT.PMIN) GO TO 999
C  Store parameters at boundary crossing
      IND = NRGN*NTRPAR
      CALL UCOPY(PAREX(1),TPAR(IND+1),6)
C  Momentum after energy loss
      TPAR(IND+7) = PAR(7)
C  Position and direction cosines after m/s
      CALL UCOPY(POUT(1),TPAR(IND+8),6)
      TPAR(IND+14) = SLENG
C
C  Find out which region the particle enters next, then set the exit
C  point parameters equal to the input point parameters, and set the
C  current region to the new region.
C
      PAREX(1) = PAREX(1) + STEP*PAREX(4)
      PAREX(2) = PAREX(2) + STEP*PAREX(5)
      PAREX(3) = PAREX(3) + STEP*PAREX(6)
      CALL UCOPY (PAREX(1),XYZ(1),3)
      CALL FMREGN (XYZ,IRNXT)
C
C  Stop if we turn back into the same region
C
      IF(IRNXT.EQ.IRLST) GO TO 999
C  Store region number at start of step
      NRGN = NRGN + 1
      IRGN(NRGN) = IRG
      IRLST = IRG
C  Stop when we leave escape from fiducial region
      IF(IRNXT.EQ.-1) GO TO 999
      IRG = IRNXT
      CALL UCOPY (PAREX,PAR,6)
C
C Next step            --------------------->>
C
      GOTO 10
C
  999 RETURN
      END
#endif
