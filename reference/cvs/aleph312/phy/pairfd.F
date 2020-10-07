      SUBROUTINE PAIRFD(I1,I2,FIELD,DXY,DZ0,DZ2,DTH,RMA,ZMA,XMA,NC1,
     &                  DIN1,NC2,DIN2,P,IER)
C-------------------------------------------------------------------------
C Author: D.Cinabro
C Date: 05/09/89
C Modified : D.Rousseau
C Date: 05/04/91
C      Double precision calculation of the invariant mass
C Modified : D.Payne
C Date: 21/04/92
C      Use FRFT #0 bank if 'FRF2' card supplied but no FRFT #2 bank
C! Simple algorithm to find pair conversions
CKEY PHYSICS PAIR GAMMA CONVERSION / USER
C
C Inputs: I1 = The index of one FRFT track
C         I2 = The index of another FRFT track
C         FIELD = the magnetic field in kilogauss
C Output: DXY = the distance(cm) in the xy plane between the two tracks
C               at the closest approach to the materialization point
C         DZ2 = the distance(cm) apart the tracks are in z at the xy point
C         DZ0 = the distance(cm) between the tracks in z at the origin
C         DTH = the theta difference of the two tracks
C         RMA = the rho value at the materializtion point
C         ZMA = the z value at the materialization point
C         XMA = the invariant mass of the tracks at the materializtion
C               point assuming they are both electrons
C         NC1(2) = Number of coordinates with radius inside RMA for
C                  track 1(2).  0 if no coordinate information available or
C                  if there are no such coordinates.
C         DIN1(2) = Distance(cm) in radius that the furthest in coordinate
C                   is inside of RMA for track 1(2).  0. if no coordinate
C                   information is available or there are no such coordinates.
C         P(3) = Summed momentum of the two tracks at the materialization
C                point in order x,y,z.
C         IER = 0 if all OK
C               1 if error in calculation
c
C Banks Used:FRFT,FRTL,FVCL,FICL,FTCL,TPCO,ITCO,VDCO
C Entry point: PAIRCP (XA,YA,ZAV)
C              get the coordinates of the conversion point
C              XA,YA,ZAV are output arguments
C
C The algorithm used is based on the fact that photons have zero mass
C so that the electrons from their "decay" will come out initially
C parallel.  It finds the point on each helix where the tracks are
C parallel in the X-Y plane and pass closest together.  It then
C returns the distance between the two helixes at these points.  Using
C the other parameters of the helix it finds the distance in Z between
C the tracks at these same points and at the origin.  Also the difference
C in theta of the two tracks is returned. The average position of the two
C points is computed in Z and in Radius from the Z-axis.  This is called
C the "materialization point".  The invariant mass of the two tracks
C assuming the electron mass for each is computed at the materialization
C point.  The summed momentum of the two tracks at the materialization
C point is also calculated.  For each track the number of cordinates "inside"
C the materialization point is returned.  A coordinate is "inside" if it
C has Z or Radius from the Z-Axis less than the materialization point.  Also
C the Radius from the Z-Axis the furthest out of the points "inside" the
C materialization point from the materialization point is returned for each
C track.  If there are no coordinates inside the materialization point for
C the track then these last two values are zero.  An error results if
C the track numbers passed in are outside of the FRFT bank or if the
C tracks passed in have the same calculated charge.
C
#ifndef DOC
      SAVE
C------------------------------------------------------------------------
C
#include "frftjj.h"
#include "itcojj.h"
#include "tpcojj.h"
#include "frtljj.h"
#include "alcons.h"
#include "bcs.h"
C
      DIMENSION P(*)
      LOGICAL FIRST
      DOUBLE PRECISION EM1D,EM2D,P1DP2D,XM2D,XMELD
      DOUBLE PRECISION PX1MD,PX2MD,PY1MD,PY2MD,PZ1MD,PZ2MD
      DATA FIRST /.TRUE./
C
#include "bmacro.h"
C
C Get the pointer to FRFT
C
      IF (FIRST) THEN
         NFRF2 = NAMIND('FRF2')
         NFRFT = NAMIND('FRFT')
         FIRST = .FALSE.
         XMEL = .00051099906
         PCON = .00029979
         NFRTL = NAMIND('FRTL')
         NFVCL = NAMIND('FVCL')
         NFICL = NAMIND('FICL')
         NFTCL = NAMIND('FTCL')
         NTPCO = NAMIND('TPCO')
         NITCO = NAMIND('ITCO')
         NVDCO = NAMIND('VDCO')
      ENDIF
C
      IER = 0
      DXY = 100.
      DZ0 = 100.
      DTH = 1000.
      RMA = 1000.
      ZMA = 1000.
      XMA = 1000.
      DZ2 = 1000.
      KFRFT = IW(NFRFT)
C
C  If 'FRF2' card supplied and FRFT #2 bank exists, use it
C  Otherwise use FRFT #0
C
      IF (IW(NFRF2).GT.0) THEN
        KFRFT2 = NLINK('FRFT',2)
        IF (KFRFT2.GT.0)  KFRFT = KFRFT2
      ENDIF
C
C Is FRFT there?
C
      IF (KFRFT.LE.0) THEN
         IER = 1
         GOTO 999
      ENDIF
C
C Are the tracks inside of FRFT
C
      LFRFT = LROWS(KFRFT)
      IF (I1.GT.LFRFT.OR.I2.GT.LFRFT) THEN
         IER = 1
         GOTO 999
      ENDIF
C
C Are the tracks oppositely charged.
C
      RI1 = 1./RTABL(KFRFT,I1,JFRFIR)
      RI2 = 1./RTABL(KFRFT,I2,JFRFIR)
      TEST = RI1*RI2
      IF (TEST.GE.0) THEN
         IER = 1
         GOTO 999
      ENDIF
C
C Find the center of I1
C
      D01 = RTABL(KFRFT,I1,JFRFD0)
      PHI1 = RTABL(KFRFT,I1,JFRFP0)
      X1 = -(RI1-D01)*SIN(PHI1)
      Y1 =  (RI1-D01)*COS(PHI1)
C
C The same for I2
C
      D02 = RTABL(KFRFT,I2,JFRFD0)
      PHI2 = RTABL(KFRFT,I2,JFRFP0)
      X2 = -(RI2-D02)*SIN(PHI2)
      Y2 =  (RI2-D02)*COS(PHI2)
C
C The midpoint of the line connecting the centers
C
      XA = (X1+X2)/2.
      YA = (Y1+Y2)/2.
C
C Find the points on the helixes nearest this point.  Labeled with M.
C
      DIR1X = XA - X1
      DIR1Y = YA - Y1
      END = SQRT(DIR1X**2+DIR1Y**2)
      DX = DIR1X/END
      DY = DIR1Y/END
      X1M = X1 + DX*ABS(RI1)
      Y1M = Y1 + DY*ABS(RI1)
      X2M = X2 - DX*ABS(RI2)
      Y2M = Y2 - DY*ABS(RI2)
C
C The average materialization point
C
      XA = (X1M+X2M)/2.
      YA = (Y1M+Y2M)/2.
      RMA = SQRT(XA**2+YA**2)
C
C Minor diversion to calculate DXY
C
      DXYT = SQRT((X1M-X2M)**2+(Y1M-Y2M)**2)
      DIST1 = SQRT((XA-X1)**2+(YA-Y1)**2)
      DIST2 = SQRT((XA-X2)**2+(YA-Y2)**2)
      IF ((DIST1+DIST2).LT.(ABS(RI1)+ABS(RI2))) THEN
        DXY = -DXYT
      ELSE
        DXY = DXYT
      ENDIF
C
C Get the Z at this point on both tracks.  Track 1.
C
      PT1 = FIELD*PCON*ABS(RI1)
      PX1M = (Y1-Y1M)*PT1/RI1
      PY1M =-(X1-X1M)*PT1/RI1
      THETA=ATAN2(PY1M,PX1M)
      IF (THETA.LT.0.) THETA=THETA+TWOPI
      ALPHA=THETA-PHI1
      IF (ALPHA.LT.-PI) THEN
         ALPHA=ALPHA+TWOPI
      ELSEIF (ALPHA.GT.PI) THEN
         ALPHA=ALPHA-TWOPI
      ENDIF
      SP1 = RI1*ALPHA
C
C SP1 is the signed arc length in xy
C
      TANL1 = RTABL(KFRFT,I1,JFRFTL)
      Z01 = RTABL(KFRFT,I1,JFRFZ0)
      Z1M = Z01 + SP1*TANL1
      PZ1M = PT1*TANL1
C
C Track 2.
C
      PT2 = FIELD*PCON*ABS(RI2)
      PX2M = (Y2-Y2M)*PT2/RI2
      PY2M =-(X2-X2M)*PT2/RI2
      THETA=ATAN2(PY2M,PX2M)
      IF (THETA.LT.0.) THETA=THETA+TWOPI
      ALPHA=THETA-PHI2
      IF (ALPHA.LT.-PI) THEN
         ALPHA=ALPHA+TWOPI
      ELSEIF (ALPHA.GT.PI) THEN
         ALPHA=ALPHA-TWOPI
      ENDIF
      SP2 = RI2*ALPHA
      TANL2 = RTABL(KFRFT,I2,JFRFTL)
      Z02 = RTABL(KFRFT,I2,JFRFZ0)
      Z2M = Z02 + SP2*TANL2
      PZ2M = PT2*TANL2
C
C The Z difference at the materialization point
C
      DZT = ABS(Z01-Z02)
      ZAV = (Z1M+Z2M)/2.
      DZ2 = ABS(Z1M-Z2M)
C
C Fill in the result
C
      DZ0 = DZT
      ZMA = ZAV
C
C The summed momentum
C
      P(1) = PX1M + PX2M
      P(2) = PY1M + PY2M
      P(3) = PZ1M + PZ2M
C
C The dip angle difference
C
      TH1 = ATAN(TANL1)
      TH2 = ATAN(TANL2)
      DTH = ABS(TH1-TH2)
C
C Now the invariant mass, double precision calculation.
C
      PX1MD = DBLE(PX1M)
      PY1MD = DBLE(PY1M)
      PZ1MD = DBLE(PZ1M)
      PX2MD = DBLE(PX2M)
      PY2MD = DBLE(PY2M)
      PZ2MD = DBLE(PZ2M)
      XMELD = DBLE(XMEL)
C
      EM1D = SQRT(PX1MD**2+PY1MD**2+PZ1MD**2+XMELD**2)
      EM2D = SQRT(PX2MD**2+PY2MD**2+PZ2MD**2+XMELD**2)
      P1DP2D = PX1MD*PX2MD+PY1MD*PY2MD+PZ1MD*PZ2MD
      XM2D = 2.*XMELD**2 + 2.*EM1D*EM2D - 2.*P1DP2D
      XMA = REAL(SQRT(ABS(XM2D)))
      IF (XM2D.LT.0.0) XMA = -XMA
C
C OK see if there are any coordinates inside the vertex.
C
      KFRTL = IW(NFRTL)
      KVDCO = IW(NVDCO)
      KFVCL = IW(NFVCL)
      KITCO = IW(NITCO)
      KFICL = IW(NFICL)
      KTPCO = IW(NTPCO)
      KFTCL = IW(NFTCL)
C
C Number of coordinates in each subdetector
C
      ICT = I1
    1 NVDC = ITABL(KFRTL,ICT,JFRTNV)
      NIDC = ITABL(KFRTL,ICT,JFRTNI)
      NTDC = ITABL(KFRTL,ICT,JFRTNT)
      NCOIN = 0
      DIN = 0.
C
C Some in the VDET
C
      IF (NVDC.GT.0.AND.KVDCO.GT.0) THEN
         IVDC = ITABL(KFRTL,ICT,JFRTIV)
         DO 10 K = 1,NVDC
            ICO = ITABL(KFVCL,K+IVDC,1)
            RADC = RTABL(KVDCO,ICO,2)
C
C Coordinate inside the materialization point
C
            IF (RADC.LT.RMA) THEN
               NCOIN = NCOIN + 1
               DINT = RMA - RADC
               IF (DINT.GT.DIN) DIN = DINT
            ENDIF
   10    CONTINUE
      ENDIF
C
C Hits in the ITC
C
      IF (NIDC.GT.0.AND.KITCO.GT.0.AND.KFICL.GT.0) THEN
         IIDC = ITABL(KFRTL,ICT,JFRTII)
         DO 20 K = 1,NIDC
            ICO = IABS(ITABL(KFICL,K+IIDC,1))
            RADC = RTABL(KITCO,ICO,JITCRA)
            IF (RADC.LT.RMA) THEN
               NCOIN = NCOIN + 1
               DINT = RMA - RADC
               IF (DINT.GT.DIN) DIN = DINT
            ENDIF
   20    CONTINUE
      ENDIF
C
C Hits in the TPC
C
      IF (NTDC.GT.0.AND.KTPCO.GT.0) THEN
         ITDC = ITABL(KFRTL,ICT,JFRTIT)
         DO 30 K = 1,NTDC
            ICO = ITABL(KFTCL,K+ITDC,1)
            RADC = RTABL(KTPCO,ICO,JTPCRV)
            IF (RADC.LT.RMA) THEN
               NCOIN = NCOIN + 1
               DINT = RMA - RADC
               IF (DINT.GT.DIN) DIN = DINT
            ENDIF
   30    CONTINUE
      ENDIF
C
C Here try the other track.
C
      IF (ICT.EQ.I2) THEN
         NC2 = NCOIN
         DIN2 = DIN
      ELSEIF (ICT.EQ.I1) THEN
         ICT = I2
         NC1 = NCOIN
         DIN1 = DIN
         GOTO 1
      ENDIF
C
C That is it.
C
  999 RETURN
C
C - entry point to get coordinates of the conversion point
C
      ENTRY PAIRCP (XDUM,YDUM,ZDUM)
      XDUM = XA
      YDUM = YA
      ZDUM = ZAV
      RETURN
C
      END
#endif

