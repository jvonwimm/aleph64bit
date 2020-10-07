      SUBROUTINE VGTRVE (TE,X,XP,XE,XEP)
C----------------------------------------------------------------------
CKEY VDETDES ALIGN / INTERNAL
C!  Transform a vector with a given transformation
C - Francesco Forti, 17 August 1990
C
C   Transforms a vector according with the transformation given in TF
C   XP = A * X  +  T
C   Where A is the rotation matrix and T is the translation.
C   The error matrices are transformed accordingly (not yet
C   implemented).
C
C - Input:
C   TE(33) / R  Transformation structure in expanded form
C   X(3)   / R  Input vector
C   XE(*)  / R  Input vector correlation matrix
C
C - Output:
C   XP(3)  / R  Transformed vector
C   XEP(*) / R  Transformed vector correlation matrix (not implemented)
C
C   Libraries required: CERNLIBS
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
      REAL TE(*), X(*), XP(*), XE(*), XEP(*)
      INTEGER I
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
C     Initialize output to translation vector:
C
      DO I=0,2
        XP(I+1) = TE(JVTETR+I)
      ENDDO
C
C     Do the transformation. See CERNLIB F003
C     ! Matrix element 1,1
C     ! Matrix element 1,2
C     ! Matrix element 2,1
C     ! Vector element
C     ! out vector
C
      CALL RMMPA(3,3,    TE( KVINDX(1,1) ),
     &                   TE( KVINDX(1,2) ),
     &                   TE( KVINDX(2,1) ),
     &                   X(1), X(2),
     &                   XP(1),XP(2) )
C
      RETURN
      END
