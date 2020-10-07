      SUBROUTINE VGWFVU (NWAF,XYZ,VUW)
C ----------------------------------------------------------------------
CKEY VDETDES ALIGN / USER
C!  Transforms absolute coordinates into wafer coordinates
C - Francesco Forti, 12 October 1990
C - Modified to use new geometry package, S. Wasserbaech, January 1995
C
C - Input:
C   NWAF   / I  Encoded wafer address (a la VAENWA)
C   XYZ(3) / R  Coordinates in ALEPH system (cm)
C
C - Output:
C   VUW(3) / R  Coordinates in wafer system (cm)
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
C     Arguments:
C
      INTEGER NWAF
      REAL XYZ(3), VUW(3)
C
C     Local variables
C
      INTEGER ILAY, IWFF, IFAC, IVIEW
C
C ----------------------------------------------------------------------
C
C     Decode the wafer identifier into the local indices:
C
      CALL VADEWA(NWAF,ILAY,IWFF,IFAC,IVIEW)
C
C     Apply the complete transformation matrix:
C
      CALL VGTRVE(VTEXPI(1,IFAC,IWFF,ILAY),XYZ,VUW,0,0)
C
      RETURN
      END
