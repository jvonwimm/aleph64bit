      SUBROUTINE VGGTNO(ILAY,IWFF,IFAC,TENO)
C ----------------------------------------------------------------------
CKEY VDETDES ALIGN / INTERNAL
C!  Get the nominal transformation for a wafer
C - Francesco Forti, 18 August 1990
C - Modified to use new geometry package, S. Wasserbaech, January 1995
C
C   Returns the transformation to the nominal position of a wafer, in
C   the sense that if TE=(A,T), where A is the rotation matrix and T is
C   the translation, the transformation is defined by:  X = A*U + T.
C   X is the vector in the Aleph main frame, while U is the vector in
C   the wafer local coordinate system, U=(v,u,w).  For a point in the
C   wafer plane, v = 0.
C
C - Input:
C   ILAY     / I  Layer index
C   IWFF     / I  Local wafer-in-face index
C   IFAC     / I  Local face index
C
C - Output:
C   TENO(33) / R  Nominal transformation for the wafer
C ----------------------------------------------------------------------
C     IMPLICIT NONE
C!    Parameters for VDET geometry package
C ----------------------------------------------------------------------
C
C     Labels for return codes:
C
      INTEGER VDERR, VDOK
      PARAMETER (VDERR = -1)
      PARAMETER (VDOK  = 1)
C
C     Labels for views:
C
      INTEGER VVIEWZ, VVIEWP
      PARAMETER (VVIEWZ = 1)
      PARAMETER (VVIEWP = 2)
C
C     Fixed VDET geometry parameters:
C
      INTEGER NVLAYR, NVMODF, NVVIEW, NPROMM, IROMAX
      PARAMETER (NVLAYR = 2)
      PARAMETER (NVMODF = 2)
      PARAMETER (NVVIEW = 2)
      PARAMETER (NPROMM = 1)
      PARAMETER (IROMAX = 4)
C
C     Array dimensions:
C
      INTEGER NVWMMX, NVWFMX, NVFLMX, NVFMAX, NVMMAX, NVWMAX
      INTEGER NVZRMX, NVPRMX
      PARAMETER (NVWMMX = 3)
      PARAMETER (NVWFMX = NVWMMX*NVMODF)
      PARAMETER (NVFLMX = 15)
      PARAMETER (NVFMAX = 24)
      PARAMETER (NVMMAX = NVFMAX*NVMODF)
      PARAMETER (NVWMAX = NVFMAX*NVWFMX)
      PARAMETER (NVZRMX = NVFMAX*IROMAX)
      PARAMETER (NVPRMX = NVMMAX*NPROMM)
C
C! Parameters used in the alignment geometrical package
      INTEGER LVEMLN, LVTFOR, LVTEXP, JVTFTR, JVTFRO, JVTFEM,
     &        JVTETR, JVTERO, JVTEEM
      PARAMETER( JVTFTR=1, JVTFRO=4, JVTFEM=7,
     &           JVTETR=1, JVTERO=4, JVTEEM=13,
     &           LVEMLN=21, LVTFOR=LVEMLN+JVTFEM-1,
     &           LVTEXP=LVEMLN+JVTEEM-1 )
C
C     (Other dimension parameters are defined in VGLOBL.)
C
      REAL VTEXPD, VTEXPI, RFDIST
      COMMON /VGPAAL/ VTEXPD(LVTEXP,NVFLMX,NVWFMX,NVLAYR),
     &                VTEXPI(LVTEXP,NVFLMX,NVWFMX,NVLAYR),
     &                RFDIST(NVFLMX,NVWFMX,NVLAYR)
C
      INTEGER ILAY, IWFF, IFAC, JWAF, IRET, JFAC
      INTEGER VSETUP, VJWFFW, VXYZWA, VJFACI, VPHIFN
      REAL TENO(*)
      REAL PHIN, CPHIN, SPHIN
C
C Unity transformation
C
      REAL TUNIT(LVTEXP)
C
C  Data statements
C
      DATA TUNIT / 0., 0., 0.,
     >             1., 0., 0., 0., 1., 0., 0., 0., 1.,
     >             LVEMLN*0. /
C
C! Definition of variables used in VGMACR.
      INTEGER III, JJJ, KVINDX
      REAL VDOT
      REAL VFXGWC, VFNLVV, VFNLWW, VFNLUU
      INTEGER IIXYZ, IIWFF, IIFAC, IILAY
C! macros to be used in geometry package.
C
C Matrix to array index for the rotation matrix
      KVINDX(III,JJJ) = JVTERO + (JJJ-1)*3 + III-1
C
C Geometrical quantities for track extrapolation
C VFXGWC : Gives the position in xyz of the wafer center
      VFXGWC(IIXYZ, IIFAC, IIWFF, IILAY) =  VTEXPD(
     &       JVTETR+IIXYZ-1 , IIFAC, IIWFF, IILAY )
C VFNLxx : Gives the three xyz coordinates of basis vector in the local
C          coordinate. xx can be VV, UU, or WW for the three vectors.
      VFNLVV(IIXYZ, IIFAC, IIWFF, IILAY) =  VTEXPD(
     &       KVINDX(IIXYZ,1), IIFAC, IIWFF, IILAY )
      VFNLUU(IIXYZ, IIFAC, IIWFF, IILAY) =  VTEXPD(
     &       KVINDX(IIXYZ,2), IIFAC, IIWFF, IILAY )
      VFNLWW(IIXYZ, IIFAC, IIWFF, IILAY) =  VTEXPD(
     &       KVINDX(IIXYZ,3), IIFAC, IIWFF, IILAY )
C ----------------------------------------------------------------------
C
C     Preset the vector:
C
      CALL UCOPY(TUNIT,TENO,LVTEXP)
C
C     Check that the geometry package is initialized:
C
      IF (VSETUP() .GT. 0) THEN
C
C     Fill the translation vector, i.e., the center of the wafer:
C
        IRET = VJWFFW(ILAY,IFAC,IWFF,JWAF)
        IRET = VXYZWA(JWAF,TENO(JVTETR+0))
C
C     Fill the matrix with the coordinates of three basis vectors
C     in the order v, u, w.  The rotation matrix is given by
C            (         )
C       RO = ( v  u  w ),
C            (         )
C     v,u,w being column vectors that represent in xyz coordinates the
C     three basis vectors of the wafer local reference frame.
C
C     Direction of wafer normal:
C
        IRET = VJFACI(ILAY,IFAC,JFAC)
        IRET = VPHIFN(JFAC,PHIN)
        CPHIN = COS(PHIN)
        SPHIN = SIN(PHIN)
C
C     v: Normal to wafer
C
        TENO(KVINDX(1,1)) = CPHIN
        TENO(KVINDX(2,1)) = SPHIN
        TENO(KVINDX(3,1)) = 0.
C
C     w: equals z
C
        TENO(KVINDX(1,3)) = 0.
        TENO(KVINDX(2,3)) = 0.
        TENO(KVINDX(3,3)) = 1.
C
C     u: phi direction
C
        TENO(KVINDX(1,2)) = -SPHIN
        TENO(KVINDX(2,2)) = CPHIN
        TENO(KVINDX(3,2)) = 0.
C
      ENDIF
C
      RETURN
      END
