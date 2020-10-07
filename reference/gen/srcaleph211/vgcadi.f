      SUBROUTINE VGCADI
C ----------------------------------------------------------------------
CKEY VDETDES ALIGN / INTERNAL
C!  Calculate distances from wafer midplanes to z axis, with alignment
C - Jochen A. Lauber       8-OCT-1990
C - Modified to use new geometry package, S. Wasserbaech, January 1995
C
C   Initialize the RFDIST array in VGPAAL.  Called by VGRDAL.
C
C - Input:
C   Structures VTEXPD in COMMON VGPAAL
C
C - Output:
C   Array RFDIST in COMMON VGPAAL
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
C     Local variables
C
      INTEGER IXYZ, IFAC, IWFF, ILAY, JFAC, NFACE, NWAFEF, IRET
      INTEGER VNTFAC, VNRWFF, VIFACI
      LOGICAL VSLOTF
      DOUBLE PRECISION DIST
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
C     Initialize RFDIST:
C
      CALL VZERO(RFDIST,NVFLMX*NVWFMX*NVLAYR)
C
C     Loop over all filled slots:
C
      NWAFEF = VNRWFF()
      NFACE = VNTFAC()
      DO JFAC=1,NFACE
        IF (VSLOTF(JFAC)) THEN
          IRET = VIFACI(JFAC,ILAY,IFAC)
C
C     Loop over wafers in the face:
C
          DO IWFF=1,NWAFEF
C
C     Dot product:
C     first three entries in VTEXPD are vector to wafer center,
C     next three entries are normal vector to wafer.
C
            DIST = 0.D0
            DO IXYZ=0,2
              DIST = DIST +
     >                 DBLE(VTEXPD(JVTETR+IXYZ,IFAC,IWFF,ILAY)) *
     >                 DBLE(VTEXPD(KVINDX(1,1)+IXYZ,IFAC,IWFF,ILAY))
C
            ENDDO
            RFDIST(IFAC,IWFF,ILAY) = SNGL(DIST)
          ENDDO
        ENDIF
      ENDDO
C
      RETURN
      END
