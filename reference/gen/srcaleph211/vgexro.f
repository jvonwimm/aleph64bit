      SUBROUTINE VGEXRO(ITFY,TF,TEXP)
C----------------------------------------------------------------------
CKEY VDETDES ALIGN / INTERNAL
C!  Expand transformation structure from DAF into a full matrix
C - Francesco Forti, 18 August 1990.
C
C   The output form is the same as the input but has the rotation
C   matrix explicitily calculated.
C
C   Here is the structure of the two types of transformation forms:
C
C    TFORM =   (bank VAGB or VALC)
C        (TRanslat(3)  = Length [-1.,1.]   : 'Global transl. vector',
C         ROtation(3)  = Angle  [-4.,4.]   : 'Global rotat. angles'
C         EcovarM(21)  = REAL   [*,*]      : 'Triangular covariance
C                                             matrix')
C
C    TEXPD =    ( COMMON /VGPAAL/ )
C        (TRanslat(3)  = Length [-1.,1.]   : 'Total  transl. vector',
C         ROtation(9)  = Angle  [-4.,4.]   : 'Total  rotat. matrix'
C         EcovarM(21)  = REAL   [*,*]      : 'Triangular covariance
C                                             matrix')
C
C - Input:
C   TF(27)   / R  Transformation structure
C   ITFY     / I  Flags the transformation TF to be local or global:
C                   1 -> global transformation
C                   2 -> local tranformation
C                 In this way it is possible to have different
C                 conventions for global and local transformations.
C
C - Output:
C   TEXP(33) / R  Same transformation in expanded form TEXP
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
      REAL TF(*), TEXP(*)
      INTEGER I,ITFY
      DOUBLE PRECISION PSI, THETA, PHI, C1, C2, C3, S1, S2, S3
      DOUBLE PRECISION PHIV, PHIU, PHIW, CV, CU, CW, SV, SU, SW
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
C     Copy translation vector:
C
      DO I=0,2
        TEXP(JVTETR+I) = TF(JVTFTR+I)
      ENDDO
C
C     Copy error matrix:
C
      DO I=0,LVEMLN-1
        TEXP(JVTEEM+I) = TF(JVTFEM+I)
      ENDDO
C
C     Rotation matrix construction:
C
      IF (ITFY .EQ. 1) THEN
C
C     Global transformation:
C
        PSI   = TF(JVTFRO+0)
        THETA = TF(JVTFRO+1)
        PHI   = TF(JVTFRO+2)
        C1 = COS(PSI)
        C2 = COS(THETA)
        C3 = COS(PHI+PSI)
        S1 = SIN(PSI)
        S2 = SIN(THETA)
        S3 = SIN(PHI+PSI)
C
C Lauber-Brownd convention.
C
        TEXP( KVINDX(1,1)) =   C3*C2*C1 + S3*S1
        TEXP( KVINDX(1,2)) =   C3*S1*C2 - C1*S3
        TEXP( KVINDX(1,3)) = - S2*C3
        TEXP( KVINDX(2,1)) =   S3*C2*C1 - S1*C3
        TEXP( KVINDX(2,2)) =   S3*S1*C2 + C3*C1
        TEXP( KVINDX(2,3)) = - S3*S2
        TEXP( KVINDX(3,1)) =   S2*C1
        TEXP( KVINDX(3,2)) =   S1*S2
        TEXP( KVINDX(3,3)) =   C2
C
      ELSEIF (ITFY .EQ. 2) THEN
C
C       Local alignment has a different convention,
C       three rotations around vuw axes in the order:
C                          w, u, v.
C       The angles are stored in the "standard" order vuw.
C
        PHIV = TF(JVTFRO+0)
        PHIU = TF(JVTFRO+1)
        PHIW = TF(JVTFRO+2)
        CV = COS(PHIV)
        CU = COS(PHIU)
        CW = COS(PHIW)
        SV = SIN(PHIV)
        SU = SIN(PHIU)
        SW = SIN(PHIW)
        TEXP( KVINDX(1,1)) =   CU*CW
        TEXP( KVINDX(1,2)) = - CU*SW
        TEXP( KVINDX(1,3)) =   SU
        TEXP( KVINDX(2,1)) =   CV*SW + SV*SU*CW
        TEXP( KVINDX(2,2)) =   CV*CW - SV*SU*SW
        TEXP( KVINDX(2,3)) = - SV*CU
        TEXP( KVINDX(3,1)) =   SV*SW - CV*SU*CW
        TEXP( KVINDX(3,2)) =   SV*CW + CV*SU*SW
        TEXP( KVINDX(3,3)) =   CV*CU
      ENDIF
C
      RETURN
      END
