      SUBROUTINE VDWFXY (NWAF,VUW,XYZ)
C ----------------------------------------------------------------------
CKEY VDETDES ALIGN / USER
C!  Converts wafer point into local and absolute coordinates
C - Dave Brown, 21 February 1991
C - Modified to use new geometry package, S. Wasserbaech, January 1995
C
C   This routine flips the local coordinates u and w according to the
C   "readout sign," then calls VGWFXY.  Ugh!
C
C - Input:
C   NWAF   / I  Encoded wafer address (a la VAENWA)
C   VUW(3) / R  Coordinates in wafer system (cm), but not really--
C               the "readout sign" is multiplied onto u and w.
C
C - Output:
C   VUW(3) / R  Coordinates in wafer system (cm), really!
C   XYZ(3) / R  Coordinates in ALEPH system (cm)
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
      REAL VUW(3), XYZ(3)
C
C     Local variables
C
      INTEGER ILAY, IWFF, IFAC, IVIEW, JWAF, IRET
      REAL ABC(3)
C
C     External references:
C
      INTEGER VJWFFW, VVUWAB, VNRDIR
C
C ----------------------------------------------------------------------
C
C     Decode the wafer identifier into the local indices:
C
      CALL VADEWA(NWAF,ILAY,IWFF,IFAC,IVIEW)
C
C     Get the global wafer index:
C
      IRET = VJWFFW(ILAY,IFAC,IWFF,JWAF)
C
C     Convert (v,u,w) -> (a,b,c) to get the readout sign information:
C
      IRET = VVUWAB(VUW,JWAF,ABC)
      VUW(2) = ABC(2) * FLOAT(VNRDIR(VVIEWP))
      VUW(3) = ABC(1) * FLOAT(VNRDIR(VVIEWZ))
C
C     Apply the complete transformation matrix:
C
      CALL VGWFXY(NWAF,VUW,XYZ)
C
      RETURN
      END
