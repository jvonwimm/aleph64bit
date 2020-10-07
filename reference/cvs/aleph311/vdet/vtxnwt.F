      SUBROUTINE VTXNWT (IDWAF,PAR,XYZ0,VUW,XYZ,VEC,RERRP,IERR)
C ----------------------------------------------------------------------
CKEY VDETDES ALIGN TRACK / USER
C!  Computes the exact intersection point of a track with a wafer
C - Jochen A. Lauber, 6-JUN-1990
C - Modified: G. Redlinger, 15-Oct-1990; simplified the function of
C     this subroutine as part of a general reorganization of the track
C     extrapolation code for the VDET.
C - Modified: G. Redlinger, 30-Jan-1991; changed arguments passed to
C     VTDERV.
C - Modified to use new geometry package, S. Wasserbaech, March 1995
C - Modified: A. Bonissent, March 1995; protect against sqrt(negative)
C
C   This routine computes the intersection point of a track with
C   a VDET wafer.  It starts with an approximation to the
C   intersection point and performs a Newton iteration to find
C   a more precise value.  The final answer includes all knowledge
C   of the alignment.  IT IS UP TO THE USER TO ENSURE THAT THE
C   ORIGINAL CHOICE OF WAFER AND STARTING INTERSECTION POINT
C   VALUES ARE REASONABLE.
C
C - Input:
C   IDWAF     / I  Base-10 encoded wafer identifier (a la VAENWA)
C   PAR(5)    / R  Track parameters, in the order IR,TL,PH,D0,Z0
C   XYZ0(3)   / R  Array containing approximate coordinates (in the
C                    Aleph system) of the intersection point
C
C - Output:
C   VUW(2)    / R  Coordinates of the intersection point in wafer
C                    system: VUW(1) = U coord, VUW(2) = W coord
C   XYZ(3)    / R  Coords of the intersection point in Aleph system
C   VEC(3)    / R  Unit vector giving the direction of the track at the
C                    intersection point in (v,u,w) system
C   RERRP(10) / R  Derivatives of the position of the intersection
C                    point with respect to the track parameters:
C                    RERRP(1) = dU/d(rho)    RERRP(6) = dW/d(rho)
C                    RERRP(2) = dU/d(tl)     RERRP(7) = dW/d(tl)
C                    RERRP(3) = dU/d(phi0)   RERRP(8) = dW/d(phi0)
C                    RERRP(4) = dU/d(d0)     RERRP(9) = dW/d(d0)
C                    RERRP(5) = dU/d(z0)     RERRP(10) = dW/d(z0)
C   IERR      / I  Error flag
C                    =0 if routine ends successfully;
C                    >0 if intersection point not found
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
      SAVE
#include "vglobl.h"
#include "vgpaal.h"
#include "vgvmcr.h"
C
C  Arguments:
C
      INTEGER IDWAF, IERR
      REAL PAR(5), XYZ0(3), VUW(2), XYZ(3), VEC(3), RERRP(10)
C
C  Local variables
C
      REAL ARG
      DOUBLE PRECISION DRV,DUMMY
     $                 ,DDIST,DVNOR(3),DALP(3),DALT(3),DTNRM
     $                 ,SXY,SXYME,DFUNC,DVIP
     $                 ,DSOSP,DCOSP,DMEOX,DMEOY,DDERI,DEPSI,DELTA
      DOUBLE PRECISION OMEGA,OMINV,TANL,PHI0,D0,Z0
      DOUBLE PRECISION RNWC(3),RNUC(3),RCW(3),RLCLU,RLCLW
      DOUBLE PRECISION RPTT(3)
      INTEGER IFAC,ILAY,IWAF,IWFF,IXYZ,IVIEW,IITER
C
C  Data
      DATA DEPSI/0.00000001/
C
C  Statement functions:
#include "vgmacr.h"
C
C ----------------------------------------------------------------------
C
      IERR = 0
C
C -- Decode the wafer identifier:
C
      CALL VADEWA(IDWAF,ILAY,IWFF,IFAC,IVIEW)
C
C -- Track parameters in double precision
C -- Mixed mode arithmetic is used because DBLE and SNGL are not
C -- allowed on the Cray.
C
      OMEGA = PAR(1)
      OMINV = 1.0/OMEGA
      TANL  = PAR(2)
      PHI0  = PAR(3)
      D0    = PAR(4)
      Z0    = PAR(5)
C
C -- Approximate path length to intersection point- starting point for
C    iteration:
C
      ARG = (XYZ0(1)**2+XYZ0(2)**2 - D0**2)/(1.0D0 - OMEGA*D0)
      IF (ARG .LT. 0.) THEN
        IERR = 4
        GO TO 999
      ENDIF
      SXY = SQRT(ARG)
      SXY = 0.5D0*OMEGA*SXY
      SXY = 2.0D0*OMINV*ASIN(SIGN(MIN(1D0,ABS(SXY)),SXY))
C
C  Unpack the vector to the wafer center and its normal from the
C  geometry common
C
      DO 710 IXYZ=1,3
C  --- Vector to centre of wafer
        RCW(IXYZ)   = VFXGWC(IXYZ,IFAC,IWFF,ILAY)
C   orthonormalized vector perpendicular to the wafer
        DVNOR(IXYZ) = VFNLVV(IXYZ,IFAC,IWFF,ILAY)
C   the w-axis basis vector rnwc
        RNWC(IXYZ)  = VFNLWW(IXYZ,IFAC,IWFF,ILAY)
C   the u-axis basis vector rnuc is the vector product of rnwc
C   and DVNOR
        RNUC(IXYZ)  = VFNLUU(IXYZ,IFAC,IWFF,ILAY)
  710 CONTINUE
C
C -- Set up for Newton iteration
C -- Starting point is approximate path length as evaluated earlier
C
      SXYME =  SXY
      DUMMY =  D0 - OMINV
      DMEOX =  DUMMY * SIN(PHI0)
      DMEOY = -DUMMY * COS(PHI0)
      IITER = 20
      DDIST = RFDIST(IFAC,IWFF,ILAY)
C
C -- Iteration loop- This calculates the intesection of the helix
C  with the wafer plane, defined by a vector to the center of the
C  wafer, and by the unit normal to the plane.  The equation being
C  solved is (X - A)*N = 0, Where X is the vector to the intersection
C  point, A is the vector to the wafer center,and N is the normal.
C  DDIST is A*N.
C  The iteration parameter SXY is the TRANSVERSE path length along the h
C
  100 CONTINUE
C
C  OMEGA*SXY is the angle swept out by the track.
C  DUMMY is then the track tangent angle at the 'end'
C
      DUMMY = OMEGA * SXY + PHI0
      DSOSP = SIN(DUMMY)
      DCOSP = COS(DUMMY)
C
C  DFUNC is the distance between the current trial extrapolation point
C  and the wafer plane, solved as DFUNC = (X-A)*N.
C
      DFUNC = -DDIST
     $       + ( OMINV*DSOSP + DMEOX  ) * DVNOR(1)
     $       + (-OMINV*DCOSP + DMEOY  ) * DVNOR(2)
     $       + ( Z0 + SXY*TANL ) * DVNOR(3)
C
C  DDERI is the derrivative of DFUNC wrt the path length SXY
C
      DDERI= DCOSP*DVNOR(1)+DSOSP*DVNOR(2) + TANL*DVNOR(3)
C
C  Check that the current step isn't ridiculously large- if so, error
C
      IF (ABS(DDERI).LT.DEPSI) THEN
        IERR = 1
        GO TO 999
      ENDIF
C
C  Update the path length for this iteration
C
      DELTA = DFUNC / DDERI
      SXY   = SXY - DELTA
C
C  Terminate the iteration when the change in path length gets small
C
      IF (ABS(DELTA) .LE. DEPSI) GOTO 200
      IITER = IITER - 1
C
C  Check that the iteration converges in a reasonable number of steps-
C  if not, set an error.
C
      IF(IITER .LE. 0) THEN
        IERR = 2
        GO TO 999
      ENDIF
C
C  Iterate
C
      GOTO 100
  200 CONTINUE
C
C  Check that the iteration values make sense- IE are physically
C  reasonable (positive path length) and are close to the original
C  guess.  THIS WILL FAIL IF THE TRACK IS EXTRAPOLATED TO A WAFER
C  NOWHERE NEAR IT'S PATH.
C
      IF((ABS(SXY-SXYME).GT. 20.0) .OR. (SXY .LT. 0.0)) THEN
        IERR = 3
        GO TO 999
      ENDIF
C
C   THE ALEPH COORDINATES ARE:
C
      DUMMY   =   OMEGA * SXY + PHI0
      DSOSP   =   SIN(DUMMY)
      DCOSP   =   COS(DUMMY)
      DALP(1)   =   OMINV * DSOSP + DMEOX
      DALP(2)   = - OMINV * DCOSP + DMEOY
      DALP(3)   =   Z0 + SXY * TANL
C
C   The normalized tangent vector DALT of the track at this point
C   in aleph coordinates
C
      DTNRM   = SQRT(DCOSP**2 + DSOSP**2 + TANL**2)
      IF (DTNRM .EQ. 0.0) DTNRM=1.0
      DTNRM   = 1.0/DTNRM
      DALT(1) = DCOSP  * DTNRM
      DALT(2) = DSOSP  * DTNRM
      DALT(3) = TANL * DTNRM
C
C   LOCAL COORDINATE u and w resulting from scalar product of
C   the Vector from the midpoint of the wafer to the track crossing
C   (DVIP) with the basis vectors of the coordinates u and w.
C   The value is already signed propperly.
C
      RLCLU = 0.0
      RLCLW = 0.0
      DO 700 IXYZ=1,3
        DVIP  = DALP(IXYZ) - RCW(IXYZ)
        RLCLU = RLCLU + DVIP*RNUC(IXYZ)
        RLCLW = RLCLW + DVIP*RNWC(IXYZ)
  700 CONTINUE
      DRV = SQRT(DALP(1)**2+DALP(2)**2)
C
C   Convert the track tangent vector into local co-ordinates
C
      DO 300 IXYZ=1,3
        RPTT(IXYZ) = 0.0
  300 CONTINUE
      DO 320 IXYZ=1,3
        RPTT(1) = RPTT(1) + DALT(IXYZ)* DVNOR(IXYZ)
        RPTT(2) = RPTT(2) + DALT(IXYZ)* RNUC(IXYZ)
        RPTT(3) = RPTT(3) + DALT(IXYZ)* RNWC(IXYZ)
  320 CONTINUE
C
C   Calculation of derivatives
C
      CALL VTDERV(PAR,RCW,DVNOR,RERRP)
C
C -- Store the results (they get converted to single precision
C -- automatically via mixed-mode arithmetic)
C
      VUW(1) = RLCLU
      VUW(2) = RLCLW
C
      XYZ(1) = DALP(1)
      XYZ(2) = DALP(2)
      XYZ(3) = DALP(3)
C
      VEC(1) = RPTT(1)
      VEC(2) = RPTT(2)
      VEC(3) = RPTT(3)
C
 999  CONTINUE
      RETURN
      END
#endif
