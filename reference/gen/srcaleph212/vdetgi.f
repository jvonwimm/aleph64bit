      INTEGER FUNCTION VDETGI ()
C ----------------------------------------------------------------------
CKEY VDETDES / INTERNAL
C!  Fills VDETGE common
C - Steve Wasserbaech, January 1994
C   (Based on VRDDAF, G. Triggiani, 17/02/87.)
C   Modified 2 June 1995, S. Wasserbaech: ISSFLG = face serial number
C   Modified 31 July 1995, SW: add variables for address packing
C
C  This function uses information from the commons VSLTCO, VDLACO,
C  VZPWCO, VWGECO, VRDOCO, and VGINDX to fill the VDETGE common.
C  The five "input" commons are filled in VDAFRD and VINDXI.
C  VDETGE common is initialized in VRDDAF.
C
C  Called by: VRDDAF
C
C - Input:
C   (none)
C
C - Output:
C   VDETGI / I  = VDOK if successful
C               = VDERR if an error occurred
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
C!    Common for VSLT data: VDET slots
C ----------------------------------------------------------------------
      INTEGER NSLOTS, JJLAYF, ISSFLG
      REAL PHIOFF
C
      COMMON / VSLTCO / NSLOTS, JJLAYF(NVFMAX), PHIOFF(NVFMAX),
     >                  ISSFLG(NVFMAX)
C
C!    Common for VDLA data: VDET layers
C ----------------------------------------------------------------------
      INTEGER IORIEN
      REAL RWVDLA, WATILT
C
      COMMON / VDLACO / RWVDLA(NVLAYR), WATILT(NVLAYR), IORIEN(NVLAYR)
C
C!    Common for VZPW data: Wafer z positions
C ----------------------------------------------------------------------
      INTEGER NWAFEF
      REAL WAFERZ
C
      COMMON / VZPWCO / NWAFEF, WAFERZ(NVWFMX)
C
C!    Common for VWGE data: Wafer geometry
C ----------------------------------------------------------------------
      INTEGER NZSTRP, NPSTRP
      REAL WSIZEA, WSIZEB, STPITZ, STPITP, STLENZ, STLENP
      REAL AMNSRZ, AMNSRP, BMNSRZ, BMNSRP, WTHICK
C
      COMMON / VWGECO / WSIZEA, WSIZEB, NZSTRP, NPSTRP, STPITZ, STPITP,
     >                  STLENZ, STLENP, AMNSRZ, AMNSRP, BMNSRZ, BMNSRP,
     >                  WTHICK
C
C!    Common for VRDO data: Readout configuration
C ----------------------------------------------------------------------
      INTEGER NRDSTZ, NRDSTP, NREFRZ, NREFRP, NOFRDZ, NOFRDP
      INTEGER NZRSSC, IECORZ, IECORP, NZEROM, NPEROM, NWFBIT
C
      COMMON / VRDOCO / NRDSTZ, NRDSTP, NREFRZ, NREFRP, NOFRDZ, NOFRDP,
     >                  NZRSSC, IECORZ, IECORP, NZEROM, NPEROM, NWFBIT
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
C!    Packing parameters for VHLS channel/strip addresses
C
C     Two packing schemes are possible: the wafer number
C     requires two bits for VDET91 and three bits for VDET95.
C     The routines VADDPK, VADDUN, VAENSA, and VADESA use the
C     VDET91 scheme only.  The routines VPKADD and VUNADD use
C     the scheme that is appropriate for the VDET setup that is
C     loaded in the VDET Geometry Package commons at the time
C     of the call.  The number of bits allocated for the wafer
C     number is read from the VRDO database bank.
C
C ----------------------------------------------------------------------
C
C     Bit masks (wafer number gets two bits, as in VDET91):
      INTEGER MVSTRP, MVVIEW, MVPHI, MVWAF, MVLAY, MVNSTR
      PARAMETER (MVSTRP = 1023)
      PARAMETER (MVVIEW = 1)
      PARAMETER (MVPHI  = 15)
      PARAMETER (MVWAF  = 3)
      PARAMETER (MVLAY  = 1)
      PARAMETER (MVNSTR = 16383)
C
C     Bit shifts (wafer number gets two bits, as in VDET91):
      INTEGER ISSTRP, ISVIEW, ISPHI, ISWAF, ISLAY, ISNSTR
      PARAMETER (ISSTRP = 0)
      PARAMETER (ISVIEW = 10)
      PARAMETER (ISPHI  = 11)
      PARAMETER (ISWAF  = 15)
      PARAMETER (ISLAY  = 17)
      PARAMETER (ISNSTR = 18)
C
C     Bit masks (wafer number gets three bits, as in VDET95):
C     The only differences with respect to VDET91:
      INTEGER M3VWAF, M3VNST
      PARAMETER (M3VWAF = 7)
      PARAMETER (M3VNST = 8191)
C
C     Bit shifts (wafer number gets three bits, as in VDET95):
C     The only differences with respect to VDET91:
      INTEGER I3SLAY, I3SNST
      PARAMETER (I3SLAY = 18)
      PARAMETER (I3SNST = 19)
C
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
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
C     Local variables
      INTEGER LOUT, ILAY, IFAC, IWFF, JFAC, JWAF
      REAL YWAF, XCEN, YCEN, RMIN, RMAX
C
C ----------------------------------------------------------------------
C
      LOUT = IW(6)
C
C     Fill coordinates of wafer centers in Aleph coordinate system.
C     Fill RVDMIN, RVDMAX, ZVDMAX.
C
C     ZVDMAX is the maximum abs(z) of the wafers in VDET:
      ZVDMAX = AMAX1(ABS(WAFERZ(1)),ABS(WAFERZ(NWAFEF))) + WSIZEA/2.
C
C     Initialize the other VDET dimensions:
      RVDMIN = 1.E9
      RVDMAX = 0.
C
C     Loop over layers:
      DO ILAY=1,NVLAYR
C
C     tan(wafer tilt angle):
        TNWTLT(ILAY) = TAN(WATILT(ILAY))
C
C     YWAF is the projection of the vector from the wafer center
C     to the origin onto the u direction:
C
        YWAF = RWVDLA(ILAY) * TNWTLT(ILAY)
C
C     RMIN is the minimum radius of the wafers in this layer.
C
        RMIN = SQRT(AMAX1(0.,ABS(YWAF) - WSIZEB/2.)**2 +
     >                  (RWVDLA(ILAY) - WTHICK/2.)**2)
C
C     RMAX is the maximum radius of the wafers in this layer.
C
        RMAX = SQRT((ABS(YWAF) + WSIZEB/2.)**2 +
     >                  (RWVDLA(ILAY) + WTHICK/2.)**2)
C
C     Now loop over faces:
C
C     (The (x,y) coordinates are the same for all wafers in a face.)
        DO IFAC=1,NFACEL(ILAY)
C
C     RVDMIN is the minimum radius of the wafers in VDET.
C     RVDMAX is the maximum radius of the wafers in VDET.
C     For this calculation we skip slots that contain no face:
C
          JFAC = IJFACE(ILAY,IFAC)
          IF (ISSFLG(JFAC) .NE. 0) THEN
            RVDMIN = AMIN1(RVDMIN,RMIN)
            RVDMAX = AMAX1(RVDMAX,RMAX)
          ENDIF
C
C     WARHOC is the distance between the origin and the wafer center
C     in the xy projection:
C
          WARHOC(JFAC) = SQRT(RWVDLA(ILAY)**2 + YWAF**2)
C
C     WAPHIC is the phi coordinate of the wafer center
C     in the Aleph system:
C
          WAPHIC(JFAC) = PHIOFF(JFAC) - WATILT(ILAY)
          IF (WAPHIC(JFAC) .LT. 0.)
     >              WAPHIC(JFAC) = WAPHIC(JFAC) + TWOPI
C
C     CPHIOF and SPHIOF are the cos and sin of PHIOFF:
C
          CPHIOF(JFAC) = COS(PHIOFF(JFAC))
          SPHIOF(JFAC) = SIN(PHIOFF(JFAC))
C
C     XCEN and YCEN are the coordinates of the wafer center in the
C     Aleph system:
C
          XCEN = RWVDLA(ILAY)*CPHIOF(JFAC) + YWAF*SPHIOF(JFAC)
          YCEN = RWVDLA(ILAY)*SPHIOF(JFAC) - YWAF*CPHIOF(JFAC)
C
C     Now loop over wafers to fill WAXCEN, WAYCEN, WAZCEN:
C
          DO IWFF=1,NWAFEF
            JWAF = IJWFFR(ILAY,IFAC,IWFF)
            WAXCEN(JWAF) = XCEN
            WAYCEN(JWAF) = YCEN
            WAZCEN(JWAF) = WAFERZ(IWFF)
          ENDDO
C
C     End of loop over faces:
        ENDDO
C
C     End of loop over layers:
      ENDDO
C
C ----------------------------------------------------------------------
C
C     Extent of the sensitive regions:
C     We are assuming that the sensitive regions extend one-half of a
C     physical strip pitch beyond the first and last physical strips.
C
C     z side:
C
      AMXSRZ = AMNSRZ + FLOAT(NZSTRP)*STPITZ
      BMXSRZ = BMNSRZ + STLENZ
C
C     r-phi side:
C
      AMXSRP = AMNSRP + STLENP
      BMXSRP = BMNSRP + FLOAT(NPSTRP)*STPITP
C
C ----------------------------------------------------------------------
C
C     Readout quantities:
C
C     Multiplexing in the z view?
      IF ((NZRSSC .EQ. 1) .AND. (NWAFEF .LE. IROMAX)) THEN
        LZMULT = .FALSE.
        NZROMM = NWAFEM
      ELSEIF (NZRSSC .EQ. 2) THEN
        LZMULT = .TRUE.
        NZROMM = 1
      ELSE
        WRITE (LOUT,'(1X,A)')
     >        '+++VDETGI+++  Invalid VDET readout configuration'
        WRITE (LOUT,'(1X,A,I5,4X,A,I5)')
     >        '+++VDETGI+++  NZRSSC =', NZRSSC, 'NWAFEF =', NWAFEF
        VDETGI = VDERR
        GO TO 1000
      ENDIF
C
C     Number of strip channels per readout module:
      IF (LZMULT) THEN
        NZSROM = NWAFEM * NRDSTZ / 2
      ELSE
        NZSROM = NRDSTZ
      ENDIF
      NPSROM = NRDSTP
C
C     Number of strip channels per module:
      NZSMOD = NZSROM * NZROMM
      NPSMOD = NPSROM * NPROMM
C
C     Number of readout strips per strip channel, r-phi view:
      NPRSSC = NWAFEM
C
C ----------------------------------------------------------------------
C
C     Variables needed for packing and unpacking cluster addresses:
C
      IF (NWFBIT .EQ. 2) THEN
C
C     Use the parameters for VDET91 (the wafer number gets two bits):
        MSVWAF = MVWAF
        MSVNST = MVNSTR
        ISSLAY = ISLAY
        ISSNST = ISNSTR
C
      ELSEIF (NWFBIT .EQ. 3) THEN
C
C     Use the parameters for VDET95 (the wafer number gets three bits):
        MSVWAF = M3VWAF
        MSVNST = M3VNST
        ISSLAY = I3SLAY
        ISSNST = I3SNST
C
      ENDIF
C
C ----------------------------------------------------------------------
C
C     Finished!
C
      VDETGI = VDOK
C
 1000 CONTINUE
      RETURN
      END
