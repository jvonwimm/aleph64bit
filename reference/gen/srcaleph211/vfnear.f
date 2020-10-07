      INTEGER FUNCTION VFNEAR (ILAY, PHI0, NFAC, IFACS)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Identify faces in a layer near a given phi
C - Steve Wasserbaech, 4 August 1995
C
C   Given a layer number ILAY and a phi coordinate PHI0, this function
C   identifies the nearest slots in phi to the coordinate PHI0, one
C   on the +phi side and one on the -phi side.  The phi coordinates
C   of the slot centers in the nominal VDET geometry are used.  Empty
C   slots are not returned.
C
C - Input:
C   ILAY     / I  Layer number
C   PHI0     / R  Phi coordinate (radians).  This may lie outside of
C                 [0,2pi]; any value is accepted.
C
C - Output:
C   VFNEAR   / I  = VDOK if successful (even if NFAC = 0)
C                 = VDERR if error occurred
C   NFAC     / I  Number of faces found (= 0, 1, or 2)
C   IFACS(2) / I  Local face indices (IFAC) of selected faces
C ----------------------------------------------------------------------
C     IMPLICIT NONE
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
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
C!    Common for VSLT data: VDET slots
C ----------------------------------------------------------------------
      INTEGER NSLOTS, JJLAYF, ISSFLG
      REAL PHIOFF
C
      COMMON / VSLTCO / NSLOTS, JJLAYF(NVFMAX), PHIOFF(NVFMAX),
     >                  ISSFLG(NVFMAX)
C
C!    Common for lookup tables for index manipulation
C ----------------------------------------------------------------------
      INTEGER IVSTUP, NMODUL, NWAFER, NFACEL, NWAFEM, JJFACM, JJMODW
      INTEGER JIFACF, JIMODM, JIWAFW, IJFACE, IJMODU, IJWAFR, JIWFFW
      INTEGER IJWFFR
      CHARACTER*4 TXMODU
C
      COMMON / VGINDX / IVSTUP, NMODUL, NWAFER, NFACEL(NVLAYR),
     >                  NWAFEM, JJFACM(NVMMAX), JJMODW(NVWMAX),
     >                  JIFACF(NVFMAX), JIMODM(NVMMAX),
     >                  JIWAFW(NVWMAX), IJFACE(NVLAYR,NVFLMX),
     >                  IJMODU(NVLAYR,NVFLMX,NVMODF),
     >                  IJWAFR(NVLAYR,NVFLMX,NVMODF,NVWMMX),
     >                  JIWFFW(NVWMAX), IJWFFR(NVLAYR,NVFLMX,NVWFMX),
     >                  TXMODU(NVLAYR,NVFMAX,NVMODF)
C
C!    Common for miscellaneous calculated VDET geometry quantities
C ----------------------------------------------------------------------
      REAL RVDMIN, RVDMAX, ZVDMAX, WAXCEN, WAYCEN, WAZCEN
      REAL WARHOC, WAPHIC, CPHIOF, SPHIOF, TNWTLT, AMXSRZ, AMXSRP
      REAL BMXSRZ, BMXSRP
      INTEGER NZSROM, NPSROM, NZSMOD, NPSMOD, NPRSSC, NZROMM
      INTEGER MSVWAF, MSVNST, ISSLAY, ISSNST
      LOGICAL LZMULT
C
      COMMON / VDETGE / RVDMIN, RVDMAX, ZVDMAX,
     >                  WAXCEN(NVWMAX), WAYCEN(NVWMAX), WAZCEN(NVWMAX),
     >                  WARHOC(NVFMAX), WAPHIC(NVFMAX), CPHIOF(NVFMAX),
     >                  SPHIOF(NVFMAX), TNWTLT(NVLAYR), AMXSRZ, AMXSRP,
     >                  BMXSRZ, BMXSRP, NZSROM, NPSROM, NZSMOD, NPSMOD,
     >                  NPRSSC, NZROMM, LZMULT, MSVWAF, MSVNST, ISSLAY,
     >                  ISSNST
C
C
C     Arguments:
      INTEGER ILAY, NFAC, IFACS(2)
      REAL PHI0
C
C     Local variables
      INTEGER IMI, IPL, IFAC, JFAC
      REAL PHI, DMI, DPL, DMI0, DPL0
      LOGICAL FMI, FPL
C
C ----------------------------------------------------------------------
C
      NFAC = 0
      CALL VZERO(IFACS,2)
C
C     IMI is the local face index of the nearest slot in the -phi
C     direction.  DMI is the phi offset of that face.  FMI = .TRUE.
C     if that slot is filled.  IPL, DPL, and FPL are the corresponding
C     variables for the +phi direction.
      IMI = 0
      DMI = -100.
      IPL = 0
      DPL = 100.
C
      IF ((ILAY .GT. 0) .AND. (ILAY .LE. NVLAYR)) THEN
C
C     Add 2pi to PHI0 until it is greater than zero:
      IF (PHI0 .LT. 0.) THEN
        PHI = PHI0 + TWOPI*AINT(1.-PHI0/TWOPI)
      ELSE
        PHI = PHI0
      ENDIF
C
C     Loop over all slots in the layer and find the nearest one to
C     PHI in each direction:
C
        DO IFAC=1,NFACEL(ILAY)
C
C     Get the offset in phi, evaluated going in both directions:
          JFAC = IJFACE(ILAY,IFAC)
          DPL0 = AMOD(WAPHIC(JFAC) - PHI + 2.*TWOPI, TWOPI)
          DMI0 = DPL0 - TWOPI
C
C     Is this nearer than what we had?
C     -phi direction:
          IF (DMI0 .GT. DMI) THEN
            IMI = IFAC
            DMI = DMI0
            FMI = (ISSFLG(JFAC) .GT. 0)
          ENDIF
C     +phi direction:
          IF (DPL0 .LT. DPL) THEN
            IPL = IFAC
            DPL = DPL0
            FPL = (ISSFLG(JFAC) .GT. 0)
          ENDIF
C
        ENDDO
C
C     Now make sure the slots are filled:
        IF ((IMI .GT. 0) .AND. FMI) THEN
          NFAC = NFAC + 1
          IFACS(NFAC) = IMI
        ENDIF
        IF ((IPL .GT. 0) .AND. (IPL .NE. IMI) .AND. FPL) THEN
          NFAC = NFAC + 1
          IFACS(NFAC) = IPL
        ENDIF
        VFNEAR = VDOK
C
      ELSE
C     Invalid input:
        VFNEAR = VDERR
      ENDIF
C
      RETURN
      END
