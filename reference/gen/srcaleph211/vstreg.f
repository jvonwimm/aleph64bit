          SUBROUTINE VSTREG(IVIEW,NREG,NDIM,ISRGS)
C ----------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!    Specify regions relating strip channels to readout strips. VDET95
C!
C!
C!    IN:    IVIEW   view, 1=z    2=r-phi
C!    OUT:   NREG    number of regions for this view (3 for z, 1 for r-p
C!                   returns -1 for error
C!           NDIM    number of wafers (2 for z, 3 for r-phi)
C!           ISRGS   ISRGS(8,3)  second dimension is number of regions
C!                   first strip channel, last strip channel,
C!                   first wafer, first readout strip in wafer,
C!                   second wafer, first readout strip in wafer
C!                   thirds wafer, first readout strip in wafer(r phi on
C!
C!----------------------------------------------------------------------
C     IMPLICIT NONE
C
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
C!    Common for VRDO data: Readout configuration
C ----------------------------------------------------------------------
      INTEGER NRDSTZ, NRDSTP, NREFRZ, NREFRP, NOFRDZ, NOFRDP
      INTEGER NZRSSC, IECORZ, IECORP, NZEROM, NPEROM, NWFBIT
C
      COMMON / VRDOCO / NRDSTZ, NRDSTP, NREFRZ, NREFRP, NOFRDZ, NOFRDP,
     >                  NZRSSC, IECORZ, IECORP, NZEROM, NPEROM, NWFBIT
C
C
      INTEGER IVIEW
      INTEGER NREG, NDIM
      INTEGER ISRGS(8,*)
C
C FUNCTIONS
      INTEGER VNSCRM, VDSCRS, VZRSRS
C
C local variables
      INTEGER STATUS
      INTEGER IREG, J, NSRMT
      INTEGER IFIRSC, ILASSC
      INTEGER NRDHLF
C
C----------------------------------------------------------------------
C       VDET95
C   strip chan   wafer,readout strip
C
C    1,320       1,1      2,321
C   321,640      1,321    3,1
C   641,960      2,1      3,321
C
C ----------------------------------------------------------------------
         NRDHLF = NRDSTZ/2
C  z view
      IF(IVIEW .EQ. 1) THEN
         NREG = 3
         NDIM = 2
C
C region 1
         ISRGS(1,1)= 1
         ISRGS(2,1) = NRDHLF
         ISRGS(3,1) = 1
         ISRGS(4,1) = 1
         ISRGS(5,1) = 2
         ISRGS(6,1) = NRDHLF + 1
C region 2
         ISRGS(1,2)=  NRDHLF + 1
         ISRGS(2,2) = NRDSTZ
         ISRGS(3,2) = 1
         ISRGS(4,2) = NRDHLF + 1
         ISRGS(5,2) = 3
         ISRGS(6,2) = 1
C region 3
         ISRGS(1,3) = NRDSTZ + 1
         ISRGS(2,3) = NRDHLF*3
         ISRGS(3,3) = 2
         ISRGS(4,3) = 1
         ISRGS(5,3) = 3
         ISRGS(6,3) = NRDHLF + 1
C
C r phi view
      ELSEIF(IVIEW .EQ. 2) THEN
          NREG = 1
          NDIM = 3
          ISRGS(1,1) = 1
          ISRGS(2,1) = NRDSTP
          ISRGS(3,1) = 1
          ISRGS(4,1) = 1
          ISRGS(5,1) = 2
          ISRGS(6,1) = 1
          ISRGS(7,1) = 3
          ISRGS(8,1) = 1
C  error
      ELSE
          NREG = -1
          NDIM = -1
      ENDIF
C ----------------------------------------------------------------------
      RETURN
      END
