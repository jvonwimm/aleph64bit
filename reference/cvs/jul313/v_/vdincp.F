      SUBROUTINE VDINCP(WID,PHIN,XYZ,XDIFF)
C ----------------------------------------------------------------------
C!  find the difference vector XDIFF of the points of intersection
C!  of a line drawn from the origin through XYZ with a slab of width WID
C!  placed parallel to the z axis with the normal making an azimuthal angle
C!  PHIN.  Like a wafer for instance.
C!  Basically, given the hit position and the wafer geometry, this routine
C!  gives the difference vector between the intersections.
CKEY V_DECK VDET
C -  Author         Manoj Thulasidas  9-May-1996
C  inputs:       WID   - the wafer width / slab thickness
C                PHIN  - phi of the normal / azimuthal orientation
C                XYZ   - a point on the line array(3), real
C  output:       XDIFF - the difference vector, array(3), real
C ----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C  arguments
      REAL WID, PHIN, XYZ(3), XDIFF(3)
C  functions
      REAL VMOD
      EXTERNAL VMOD
C  locals
      REAL DELX, PHI, PHITOT, CTHETA, STHETA, DELXP
      REAL PI
      PARAMETER (PI = 3.1415926)
C=======================================================================
C-- get the azimuthal and polar angles of the point
      PHI = ATAN2(XYZ(2),XYZ(1))
      CTHETA = XYZ(3)/VMOD(XYZ,3)
      STHETA = SQRT(1.0-CTHETA*CTHETA)
C-- get the (azimuthal) angle between the face and the line from the
C-- origin through XYZ
      PHITOT = PI/2.0-PHIN+PHI
C-- length of the segment inside the wafer, in xy projection
      DELXP = WID/SIN(PHITOT)
C-- the total length in 3d
      DELX = DELXP/STHETA
C-- project it to different directions
      XDIFF(2) = DELXP*SIN(PHI)
      XDIFF(3) = DELX*CTHETA
      RETURN
      END
#endif
