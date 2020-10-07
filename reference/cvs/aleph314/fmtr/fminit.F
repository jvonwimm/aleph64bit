       SUBROUTINE FMINIT
C----------------------------------------------------------------------
C!  Initialise geometry, material constants and cuts for muon tracking.
#ifndef DOC
      SAVE
#include "fmgeom.h"
#include "alcons.h"
#include "bcs.h"
C Statement function
#include "bmacro.h"
C
C  TPC
C
      FMRMIN (1) = 0.
      FMRMAX (1) = 185.0
      FMZMIN (1) = 0.
      FMZMAX (1) = 240.0
C
C  between coil and barrel
C
      FMRMIN (2) = 264.7
      FMRMAX (2) = 300.0
      FMZMIN (2) = 0.
      FMZMAX (2) = 315.0
C
C  end of coil
C
      FMRMIN (3) = 210.0
      FMRMAX (3) = 300.0
      FMZMIN (3) = 315.0
      FMZMAX (3) = 362.0
C
C  inner barrel region
C
      FMRMIN (4) = 300.0
      FMRMAX (4) = 384.2
      FMZMIN (4) = 0.
      FMZMAX (4) = 362.0
C
C  outer barrel region
C
      FMRMIN (5) = 384.2
      FMRMAX (5) = 468.4
      FMZMIN (5) = 0.
      FMZMAX (5) = 362.0
C
C  pole face
C
      FMRMIN (6) = 45.0
      FMRMAX (6) = 210.0
      FMZMIN (6) = 315.0
      FMZMAX (6) = 362.0
C
C  inner endcap
C
      FMRMIN (7) = 45.0
      FMRMAX (7) = 435.0
      FMZMIN (7) = 362.0
      FMZMAX (7) = 420.8
C
C  outer endcap
C
      FMRMIN (8) = 45.0
      FMRMAX (8) = 435.0
      FMZMIN (8) = 420.8
      FMZMAX (8) = 483.4
C
C  quadrupole
C
      FMRMIN (9) = 0.
      FMRMAX (9) = 45.0
      FMZMIN (9) = 315.0
      FMZMAX (9) = 483.4
C
C  electromagnetic calorimeter barrel
C
      FMRMIN (10) = 185.0
      FMRMAX (10) = 264.7
      FMZMIN (10) = 0.
      FMZMAX (10) = 240.0
C
C  electromagnetic calorimeter endcap
C
      FMRMIN (11) = 0
      FMRMAX (11) = 264.7
      FMZMIN (11) = 240.0
      FMZMAX (11) = 315.0
C Muon mass constants
      IND = IW(NAMIND('PART'))
      IF (IND.EQ.0) IND = IW(NAMIND('QPAR'))
      IF (IND.NE.0) THEN
         XMUMAS = RTABL(IND,5,6)
         XMUMS2 = XMUMAS**2
      ENDIF
C
C  radiation lengths of materials
C
C  x0 are expressed in centimeters and assumed  constant
C over the volume they are referring to.
C region 1 : .12 x0 correponds to beam pipe , vdet ,ITC and TPC
C            inner and outer cages in the radial direction
C region 2 : .75 x0 corresponds to half of the cryostat
C region 3 : 6.2 x0 corresponds to the end part of the cryostat
C           there are also a mixture of electronic boxes and cables
C region 4 : 2.55 cm corresponds to the 5cm iron sampling in the barrel
C region 5 : 2.42cm corresponds to the 5cm iron sampling and the last
C              10cm plate
C region 6 : 2.52 cm corresponds to the 5cm iron sampling in the end cap
C region 7 :  "    "    "
C region 8 :2.36 cm corresponds to the 5cm iron sampling and the last
C          10cm  plate in the end cap
C region 9 : 6.5 cm is a guess corresponding to the valves,
C            pumping stations and quadrupole body in the z direction
C region 10: 22x0 correspond to the ecal barrel , .75 x0 to half of
C             the coil cryostat.
C region 11 : 22.4 x0 correspond to the ecal endcap in the z direction
C             .08 x0 correspond to the TPC endplate aand cables
C            a further contribution of about 2x0 is expected from ITC
C            support and cables peaked around 10 degrees....
      FMRADL(1) = (FMRMAX(1)-FMRMIN(1))/.12
      FMRADL(2) = (FMRMAX(2)-FMRMIN(2))/.75
      FMRADL(3) = (FMZMAX(3)-FMZMIN(3))/6.2
      FMRADL(4) = 2.55
      FMRADL(5) = 2.42
      FMRADL(6) = 2.52
      FMRADL(7) = 2.52
      FMRADL(8) = 2.36
      FMRADL(9) = 6.5
      FMRADL(10) = (FMRMAX(10)-FMRMIN(10))/22.75
      FMRADL (11) = (FMZMAX(11)-FMZMIN(11))/22.48
C
C Constants for energy loss using the Bethe-Bloch formula
C dE/dx = c1/beta**2*[ ln(c2*(beta*gam)**2) - beta**2][Mev/cm]
C c1 and c2 are given here for iron, aluminum, and lead
C J. Hilgart 23/03/88
C
C Neglect energy loss in TPC
      CONBB1(1) = 0.
      CONBB2(1) = 0.
      FRATIO(1) = 0.
C Coil contains 11 cm Al
      CONBB1(2) = 0.3926
      CONBB2(2) = 6156.6
      FRATIO(2) = 11./(FMRMAX(2)-FMRMIN(2))
C Region 3. Use equivalent thickness of iron of 4 cm
      CONBB1(3)= 1.110
      CONBB2(3) = 3403.0
      FRATIO(3) = 4./(FMZMAX(3)-FMZMIN(3))
C HCAL Total thickness of Fe in barrel = 50cm + 70 cm
C                            in ecap = 30 + 34 + 40 cm
C Region 9. Use equivalent thickness of iron of 4 cm
      DO 121 ID = 4, 9
         CONBB1(ID) = 1.110
         CONBB2(ID) = 3403.0
 121  CONTINUE
      FRATIO(4) = 50./(FMRMAX(4)-FMRMIN(4))
      FRATIO(5) = 70./(FMRMAX(5)-FMRMIN(5))
      FRATIO(6) = 30./(FMZMAX(6)-FMZMIN(6))
      FRATIO(7) = 34./(FMZMAX(7)-FMZMIN(7))
      FRATIO(8) = 34./(FMZMAX(8)-FMZMIN(8))
      FRATIO(9) = 4./(FMZMAX(9)-FMZMIN(9))
C Ecal contains 11cm Pb
      DO 122 ID = 10, 11
         CONBB1(ID) = 1.317
         CONBB2(ID) = 1241.8
 122  CONTINUE
      FRATIO(10) = 11./(FMRMAX(10)-FMRMIN(10))
      FRATIO(11) = 11./(FMZMAX(11)-FMZMIN(11))
C
C Constants for multiple scattering (FMSCAT) taken from init. of GMUL
C
      DXM=TWOPI/100.
      XM=-0.5*DXM
      SQ=-0.0099999
      DO 80 I=1,101
         SQ=SQ+0.01
         IF(I.LT.101)SQRMUL(I)=SQRT(-2.*ALOG(SQ))
         XM=XM+DXM
         SINMUL(I)=SIN(XM)
         COSMUL(I)=COS(XM)
   80 CONTINUE
      SQRMUL(101)=0.01
C
      RETURN
      END
#endif
