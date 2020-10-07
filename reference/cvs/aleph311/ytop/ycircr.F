      SUBROUTINE YCIRCR(PARIN1,PARIN2,NCRO,NPHYS,CORA,ZCO2)
C----------------------------------------------------------*
C!    Find circle crossings for V0 reconstruction
CKEY YTOP
C!    Author :     W. Manner 25/01/93
C!
C!    Description
C!    ===========
C!     Given two tracks with parameters PARIN1 PARIN2 calculate
C!     NCRO = 0,1,2  circle crossings, for NCRO = 0 the closest
C      point is calculated.
C      NCRO is the total number of crossings found ie 0,1,2
C      NPHYS indicates the physical crossings ie with the v0
C      behind the beamcrossing
C      the meaning is
C      nphys = 0 no physical crossing
C      NPHYS = 1 CORA(*,1) is physical
C      NPHYS = 2 CORA(*,2) is physical
C      NPHYS = 3 both crossings are physical
C      CORA(3,I) are the coordinates of track 1 at crossing I
C      ZCO2(I)  is the z coordinate of track 2 at crossing I

C      Parameter order is 1/R TL PHI D Z
C -------------------------------------------------------
#ifndef DOC
      SAVE
      REAL*8 RAD1,XR1,YR1,RAD2,XR2,YR2,FI1,D01,FI2,D02,XLEN
      REAL*8 XIZ,YIZ,CORAD(3,2),YIZ2,SAL,CAL,PSI,SPSI,CPSI
      REAL*8 SPS2,CPS2
      REAL   CORA(3,2),PARIN1(5),PARIN2(5),ZCO2(2)
      COMMON/YBCRTO/BCROSS(3),VBCROS(6)
#include "bcs.h"
#include "bmacro.h"
C -----------------------------------------------------
      FIELM=ALFIEL(DUM)
      ROFP = 1./ (0.29979 * FIELM / 10.) * 100.
      RAD1=1./PARIN1(1)
      D01=PARIN1(4)
      FI1=PARIN1(3)
      XR1=(-RAD1+D01)*SIN(FI1)
      YR1=(RAD1-D01)*COS(FI1)
      RAD2=1./PARIN2(1)
      D02=PARIN2(4)
      FI2=PARIN2(3)
      XR2=(-RAD2+D02)*SIN(FI2)
      YR2=(RAD2-D02)*COS(FI2)
      XLEN=SQRT((XR2-XR1)**2+(YR2-YR1)**2)
      XIZ=0.5*(XLEN**2+(RAD1+RAD2)*(RAD1-RAD2))/XLEN
      YIZ2=RAD1**2-XIZ**2
      NCRO=2
      IF(YIZ2.EQ.0.) NCRO=1
      IF(YIZ2.LT.0.) THEN
        NCRO=0
        YIZ2=0.
      ENDIF
      YIZ=SQRT(YIZ2)
      SAL=(YR2-YR1)/XLEN
      CAL=(XR2-XR1)/XLEN
      CORAD(1,1)=XIZ*CAL-YIZ*SAL + XR1
      CORAD(2,1)=XIZ*SAL+YIZ*CAL + YR1
      CORAD(1,2)=XIZ*CAL+YIZ*SAL + XR1
      CORAD(2,2)=XIZ*SAL-YIZ*CAL + YR1
Calculate z coordinate of crossing
C Here we assume that z of track 1 is defined
C z of track 2 may not be defined (ITC only)
      IN=0
      NPHYS=0
      DO I=1,MAX(1,NCRO)
        SPSI=(-CORAD(1,I)-(RAD1-D01)*SIN(FI1))/RAD1
        CPSI=(-CORAD(2,I)+(RAD1-D01)*COS(FI1))/RAD1
        PSI=ATAN2(SPSI,CPSI)
        PSI=PSI+FI1
        IF(PSI.LT.-3.141592653589) PSI=PSI+2.*3.141592653589
        IF(PSI.GT. 3.141592653589) PSI=PSI-2.*3.141592653589
        CORAD(3,I)=PARIN1(5)-PSI*RAD1*PARIN1(2)
        SPS2=(-CORAD(1,I)-(RAD2-D02)*SIN(FI2))/RAD2
        CPS2=(-CORAD(2,I)+(RAD2-D02)*COS(FI2))/RAD2
Calculate direction of flight
        PT1=-1./ROFP*ABS(RAD1)
        PX1=-PT1*CPSI
        PY1= PT1*SPSI
        PT2=-1./ROFP*ABS(RAD2)
        PX2=-PT2*CPS2
        PY2= PT2*SPS2
        PTX=PX1+PX2
        PTY=PY1+PY2
C Project beam crossing onto momentum vector
        DIS=(CORAD(1,I)*PTX+CORAD(2,I)*PTY)/SQRT(PTX**2+PTY**2)
C Estimate error (roughly)
        IF(NCRO.EQ.2) THEN
          COS2=(PX1*PX2+PY1*PY2)**2/(PX1**2+PY1**2)/(PX2**2+PY2**2)
          ERR=0.05/SQRT(1.-COS2)
        ELSE
          ERR=SQRT(2.*0.05/(1./ABS(RAD1)+1./ABS(RAD2)))
        ENDIF
        IF(DIS.GT.-ERR) THEN
          NPHYS=NPHYS+I
        ENDIF
      ENDDO
      DO I=1,2
Calculate z of track 2 at crossing
        SPSI=(-CORAD(1,I)-(RAD2-D02)*SIN(FI2))/RAD2
        CPSI=(-CORAD(2,I)+(RAD2-D02)*COS(FI2))/RAD2
        PSI=ATAN2(SPSI,CPSI)
        PSI=PSI+FI2
        IF(PSI.LT.-3.141592653589) PSI=PSI+2.*3.141592653589
        IF(PSI.GT. 3.141592653589) PSI=PSI-2.*3.141592653589
        ZCO2(I)=PARIN2(5)-PSI*RAD2*PARIN2(2)
      ENDDO
      DO I=1,3
        DO J=1,2
          CORA(I,J)=CORAD(I,J)
        ENDDO
      ENDDO
      END
#endif
