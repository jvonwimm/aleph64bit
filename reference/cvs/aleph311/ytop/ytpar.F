      SUBROUTINE YTPAR(IQ,VTX,VVTX,
     &      PSUM,VPSUM,VPSVX,
     &      TRACK,VTRACK,
     &      IFAIL)
C
C----------------------------------------------------------*
C!    Calculation of track parameters from vertex and momentum
CKEY YTOP TRACK
C!    Author :     G. Lutz      /04/91
C!
C!    Description
C!    ===========
C!
C! INPUT
C!    IQ  .......... CHARGE OF TRACK IN UNITS OF ELEMENTARY CHARGE
C!    VTX .......... X,Y,Z OF INPUT VERTEX
C!    VVTX ......... VARIANCE OF INPUT VERTEX
C!                   VX;
C!                   VX.VY;VY;
C!                   VX.VZ;VY.VZ;VZ
C!    PSUM ......... MOMENTUM VECTOR
C!    VPSUM ........ ERROR COVARIANCE MATRIX OF MOMENTUM VECTOR
C!                   PX;
C!                   PX.PY;PY;
C!                   PX.PZ;PY.PZ;PZ
C!    VPSVX ........ ERROR CORRELATION MOMENTUM-VERTEX
C!                   VX.PX; VY.PX; VZ.PX;
C!                   VX.PY; VY.PY; VZ.PY;
C!                   VX.PZ; VY.PZ; VZ.PZ;
C!
C! OUTPUT
C!    TRACK ........ TRACK PARAMETERS AS DEFINED DIFFERENTLY FOR
C!                   CHARGED AND NEUTRAL TRACKS:
C!                   HELIX PARAMETERS ORDERED IN SEQUENCE
C!                   RHO=1/R(SIGNED); T; PHI0; D0(SIGNED); Z0
C!                   RHO>0 FOR COUNTERCLOCKWISE BENDING
C!                   D0.GT.0 IF MOMENTUM AROUND ORIGIN IS POSITIVE
C!
C!                   NEUTRAL TRACK PARAMETERS ORDERED IN SEQUENCE
C!                   P; T; PHI0; D0(SIGNED); Z0
C!                   D0.GT.0 IF MOMENTUM AROUND ORIGIN IS POSITIVE
C!
C!    VTRACK ....... CORRESPONDING VARIANCES ORDERED AS
C!                   RHO;
C!                   RHO.T; T
C!                   RHO.PHI0;   T.PHI0    PHI0
C!                   RHO.D0;    T.D0      PHIZERO     D0
C!                   RHO.Z0     T.Z0     PHI0.Z0    D0.Z0    Z0
C!                   RESPECTIVELY RHO REPLACED BY P FOR NEUTRALS
C!                   1=ELECTRON,2 MUON, 3 PION, 4 KAON, 5 PROTON
C!                   NPIDC WORDS PER COMBIATION
C!
C!    IFAIL ........ =0 FOR REGULAR RETURN
C!
C!
C!    SIGN CONVENTION:
C!    RADIUS: POSITIVE FOR COUNTER CLOCKWISE TURN
C!    DO:     POSITIVE IF PARTICLE HAS POSITIVE ANGULAR MOMENTUM
C!            AROUND ORIGIN
C!
C!    VARIABLE SEQUENCE: 1/R,T,PHI0,D0,Z0
C!
C!---------------------------------------------------------*
#ifndef DOC
      SAVE
#include "bcs.h"
#include "ydimto.h"
#include "yparto.h"
#include "ydumto.h"
C!---------------------------------------------------------*
C
C
      DIMENSION VTX(*),VVTX(*)
      DIMENSION PSUM(*),VPSUM(*),VPSVX(*)
      DIMENSION TRACK(*),VTRACK(*)
#if defined(YDEBUG)
      DIMENSION EHI(5,5)
      DIMENSION TESTM(6,6),TESTMI(6,6)
      LOGICAL LDMP1,LDMP2
#endif
C
      DOUBLE PRECISION PX,PY,PZ,P,PT
      DOUBLE PRECISION VPXX,VPXY,VPYY,VPXZ,VPYZ,VPZZ
      DOUBLE PRECISION VX,VY,VZ
      DOUBLE PRECISION VVXX,VVXY,VVYY,VVXZ,VVYZ,VVZZ
      DOUBLE PRECISION VXPX,VYPX,VZPX,VXPY,VYPY,VZPY,VXPZ,VYPZ,VZPZ
C
C     recalculation of fitted helix parameters from idep. par.
      DOUBLE PRECISION ETA,FR,FI0,SFI0,CFI0,FI,SFI,CFI,
     &                 FS,FF0,SFF0,CFF0,FIFI0
      DOUBLE PRECISION D0,FT,Z0,XC,YC,V1XC,V2YC
      DOUBLE PRECISION ROFP,QQ
      DOUBLE PRECISION PI
      DOUBLE PRECISION HALF,ONE,TWO
C
C
      DOUBLE PRECISION H1P1,H1P2,H1P3,
     &                 H2P1,H2P2,H2P3,
     &                 H3V1,H3V2,     H3P1,H3P2,
     &                 H4V1,H4V2,     H4P1,H4P2,
     &                 H5V1,H5V2,H5V3,H5P1,H5P2,H5P3
C
C     INITIALIZE DOUBLE PRECISON CONSTANTS
      DATA HALF/0.5D+00/,ONE/1.D+00/,TWO/2.D+00/,PI/3.14159265D00/
C
C     helix parameter internal order
      DATA KR,KT,KF,KD,KZ/1,2,3,4,5/
C     HELIX ERROR VARIANCES ORDER
      DATA JRR,JRT,JRF,JRD,JRZ/1,2, 4, 7,11/
     &         JTT,JTF,JTD,JTZ/  3, 5, 8,12/
     &         JFF,JFD,JFZ/     6, 9,13/
     &         JDD,JDZ/       10,14/
     &         JZZ/          15/
C
C
      DATA NENTY/0/
C
      DATA MXCOM/100/
C
C
C-- Define the logical unit for printout
C
      LOUT = IW(6)
C
      NENTY=NENTY+1
C
      ICFHX=ICFHX+1
      NCFHX=NCFHX+1
C
      IFAIL=0
C
#if defined(YDEBUG)
      IF(IDUMPP(37).GE.1) THEN
        LDMP1=.TRUE.
      ELSE
        LDMP1=.FALSE.
      ENDIF
C
      IF(IDUMPP(38).GE.1) THEN
        LDMP2=.TRUE.
      ELSE
        LDMP2=.FALSE.
      ENDIF
      IDUMPP(37)=IDUMPP(37)-1
      IDUMPP(38)=IDUMPP(38)-1
C
      IF(LDMP1.OR.LDMP2)  WRITE(LOUT,10) NENTY,IQ,(VTX(I),I=1,3),
     &   (PSUM(I),I=1,3),(VPSUM(I),I=1,6),(VPSVX(I),I=1,9)
   10 FORMAT(1X,10(1H*),' YTPAR CALLED',I5,' TIMES',
     &  '  IQ=',I3,'  VTX=',3F10.3,'  PSUM=',3F10.3/
     &  '     VPSUM=',6E10.3/( '    VPSVX',3E10.3) )
C
C  CHECK POSITIVENESS OF MOMENTUM SUM ERROR
C  CHECK POSITIVENESS OF VERTEX-PS ERROR MATRIX
      IJ=0
      DO I=1,3
        DO J=1,I
          IJ=IJ+1
          TESTM(J,I)=VVTX(IJ)
          TESTM(I,J)=VVTX(IJ)
          TESTM(3+J,3+I)=VPSUM(IJ)
          TESTM(3+I,3+J)=VPSUM(IJ)
        ENDDO
        DO J=1,3
          IX=3*(I-1)+J
          TESTM(J,I+3)=VPSVX(IX)
          TESTM(I+3,J)=VPSVX(IX)
        ENDDO
      ENDDO
      DO I=1,6
        DO J=1,6
          TESTMI(I,J)=TESTM(J,I)
        ENDDO
      ENDDO
      CALL RSINV(6,TESTMI,6,IIFL)
      IF (IIFL.NE.0.OR.LDMP2)  THEN
        DO I=1,6
          DO J=1,6
            TESTMI(I,J)=TESTM(I,J)
            IF(I.NE.J.AND.ABS(TESTM(I,I)*TESTM(J,J)).GT.0.)
     &                TESTMI(I,J)=TESTM(I,J)/
     &                SQRT(ABS(TESTM(I,I)*TESTM(J,J)))
          ENDDO
        ENDDO
        WRITE(LOUT,9856) IIFL,TESTM,TESTMI
 9856   FORMAT( '  YTPAR: NON-POS. MATRIX TESTM; IIFL=',I5/
     &           6(5X,6E10.3/)/
     &           6(5X,6E10.3/))
      ENDIF
#endif
C
C
C     USE DOUBLE PRECISION CALCULATION: COPY INPUT VECTORS
C     input vertices
      VX=VTX(1)
      VY=VTX(2)
      VZ=VTX(3)
C
      VVXX=VVTX(1)
      VVXY=VVTX(2)
      VVYY=VVTX(3)
      VVXZ=VVTX(4)
      VVYZ=VVTX(5)
      VVZZ=VVTX(6)
C
C
#if defined(YDEBUG)
      IF(LDMP2) THEN
        WRITE(LOUT,901) (VTX(I),I=1,3),(VVTX(I),I=1,6)
  901   FORMAT(/5X,'  VTX',3F10.3,'  VVTX',6E10.3)
      ENDIF
#endif
C
C
C     INPUT MOMENTUM
C
      PX=PSUM(1)
      PY=PSUM(2)
      PZ=PSUM(3)
      PT=SQRT(PX**2+PY**2)
      P =SQRT(PT**2+PZ**2)
C
C     MOMENTUM ERRORS
      VPXX=VPSUM(1)
      VPXY=VPSUM(2)
      VPYY=VPSUM(3)
      VPXZ=VPSUM(4)
      VPYZ=VPSUM(5)
      VPZZ=VPSUM(6)
C
C     MOMENTUM-VERTEX ERROR CORRELATIONS
      VXPX=VPSVX(1)
      VYPX=VPSVX(2)
      VZPX=VPSVX(3)
      VXPY=VPSVX(4)
      VYPY=VPSVX(5)
      VZPY=VPSVX(6)
      VXPZ=VPSVX(7)
      VYPZ=VPSVX(8)
      VZPZ=VPSVX(9)
C
#if defined(YDEBUG)
      IF(LDMP2) THEN
        WRITE(LOUT,907) (PSUM(I),I=1,3),PT,P,(VPSUM(I),I=1,6)
     &    ,(VPSVX(I),I=1,9)
  907   FORMAT(/5X,' YTPAR:  PSUM',3F10.3,' PT,P',2F10.3/,
     &    10X,'  VPSUM',6E10.3/, (10X,'VPSVX',3E10.3))
      ENDIF
#endif
C
C
      IF(IQ.EQ.0) THEN
C NEUTRAL TRACK SECTION
        SFI0=PY/PT
        CFI0=PX/PT
        FI0=ATAN2(SFI0,CFI0)
        D0=VX*SFI0-VY*CFI0
        FS=VX*CFI0+VY*SFI0
        FT=PZ/PT
        Z0=VZ-FS*FT
C
C  NEUTRAL TRACK PARAMETERS
        TRACK(1)=P
        TRACK(2)=FT
        TRACK(3)=FI0
        TRACK(4)=D0
        TRACK(5)=Z0
C
C DERIVATIVES OF TRACK PARAMETERS WITH RESPECT TO VERTEX AND MOMENTUM
        H1P1= PX/P
        H1P2= PY/P
        H1P3= PZ/P
C
        H2P1=-CFI0*PZ/PT**2
        H2P2=-SFI0*PZ/PT**2
        H2P3=ONE/PT
C
        H3P1=-SFI0/PT
        H3P2= CFI0/PT
C
        H4V1= SFI0
        H4V2=-CFI0
        H4P1=FS*H3P1
        H4P2=FS*H3P2
C
        H5V1=-CFI0*FT
        H5V2=-SFI0*FT
        H5V3=ONE
        H5P1=D0*H3P1*FT-FS*H2P1
        H5P2=D0*H3P2*FT-FS*H2P2
        H5P3=-FS/PT
C
C  NEUTRAL TRACK PARAMETER ERRORS
C     MOMENTUM P
        VTRACK(1)=VPXX*H1P1**2+VPYY*H1P2**2+VPZZ*H1P3**2+
     &    TWO*(VPXY*H1P1*H1P2+VPXZ*H1P1*H1P3+
     &    VPYZ*H1P2*H1P3 )
C     SLOPE T
        VTRACK(3)=VPXX*H2P1**2+VPYY*H2P2**2+VPZZ*H2P3**2+
     &    TWO*(VPXY*H2P1*H2P2+VPXZ*H2P1*H2P3+
     &    VPYZ*H2P2*H2P3 )
C     FI0
        VTRACK(6)=VPXX*H3P1**2+VPYY*H3P2**2+
     &    TWO*(VPXY*H3P1*H3P2 )
C     D0
        VTRACK(10)=VVXX*H4V1**2+VVYY*H4V2**2+ TWO*VVXY*H4V1*H4V2+
     &           VPXX*H4P1**2+VPYY*H4P2**2+ TWO*VPXY*H4P1*H4P2+
     &           TWO*(  VXPX*H4V1*H4P1+VYPY*H4V2*H4P2+
     &           VXPY*H4V1*H4P2+VYPX*H4V2*H4P1)
C     Z0
        VTRACK(15)=VVXX*H5V1**2+VVYY*H5V2**2+VVZZ*H5V3**2+
     &    TWO*(VVXY*H5V1*H5V2+VVXZ*H5V1*H5V3+VVYZ*H5V2*H5V3)+
     &    VPXX*H5P1**2+VPYY*H5P2**2+VPZZ*H5P3**2+
     &    TWO*(VPXY*H5P1*H5P2+VPXZ*H5P1*H5P3+VPYZ*H5P2*H5P3)+
     &    TWO*(   VXPX*H5V1*H5P1+VYPY*H5V2*H5P2+VZPZ*H5V3*H5P3+
     &    VXPY*H5V1*H5P2+VYPX*H5V2*H5P1+
     &    VXPZ*H5V1*H5P3+VZPX*H5V3*H5P1+
     &    VYPZ*H5V2*H5P3+VZPY*H5V3*H5P2 )
C     P - TAU
        VTRACK(2)=VPXX*H1P1*H2P1+VPYY*H1P2*H2P2+VPZZ*H1P3*H2P3+
     &    VPXY*(H1P1*H2P2+H2P1*H1P2)+VPXZ*(H1P1*H2P3+H2P1*H1P3)+
     &    VPYZ*(H1P2*H2P3+H2P2*H1P3)
C     P - PHI
        VTRACK(4)=VPXX*H1P1*H3P1+VPYY*H1P2*H3P2+
     &    VPXY*(H1P1*H3P2+H3P1*H1P2)+VPXZ*(          H3P1*H1P3)+
     &    VPYZ*(          H3P2*H1P3)
C     TAU - PHI
        VTRACK(5)=VPXX*H2P1*H3P1+VPYY*H2P2*H3P2+
     &    VPXY*(H2P1*H3P2+H3P1*H2P2)+VPXZ*(          H3P1*H2P3)+
     &    VPYZ*(          H3P2*H2P3)
C     P - D0
        VTRACK( 7)=
     &           VPXX*H1P1*H4P1+VPYY*H1P2*H4P2+
     &           VPXY*(H1P1*H4P2+H4P1*H1P2)+
     &           VPXZ*(          H4P1*H1P3)+
     &           VPYZ*(          H4P2*H1P3)+
     &           (  VXPX*H4V1*H1P1+VYPX*H4V2*H1P1+
     &           VXPY*H4V1*H1P2+VYPY*H4V2*H1P2+
     &           VXPZ*H4V1*H1P3+VYPZ*H4V2*H1P3 )
C     TAU - D0
        VTRACK( 8)=
     &           VPXX*H2P1*H4P1+VPYY*H2P2*H4P2+
     &           VPXY*(H2P1*H4P2+H4P1*H2P2)+
     &           VPXZ*(          H4P1*H2P3)+
     &           VPYZ*(          H4P2*H2P3)+
     &           (  VXPX*H4V1*H2P1+VYPX*H4V2*H2P1+
     &           VXPY*H4V1*H2P2+VYPY*H4V2*H2P2+
     &           VXPZ*H4V1*H2P3+VYPZ*H4V2*H2P3 )
C     FI0 - D0
        VTRACK( 9)=
     &           VPXX*H3P1*H4P1+VPYY*H3P2*H4P2+
     &           VPXY*(H3P1*H4P2+H4P1*H3P2)+
     &           VXPX*H4V1*H3P1+VYPX*H4V2*H3P1+
     &           VXPY*H4V1*H3P2+VYPY*H4V2*H3P2
C     P - Z
        VTRACK(11)=
     &           VPXX*H1P1*H5P1+VPYY*H1P2*H5P2+VPZZ*H1P3*H5P3+
     &           VPXY*(H1P1*H5P2+H5P1*H1P2)+
     &           VPXZ*(H1P1*H5P3+H5P1*H1P3)+
     &           VPYZ*(H1P2*H5P3+H5P2*H1P3)+
     &           (  VXPX*H5V1*H1P1+VXPY*H5V1*H1P2+VXPZ*H5V1*H1P3+
     &           VYPX*H5V2*H1P1+VYPY*H5V2*H1P2+VYPZ*H5V2*H1P3+
     &           VZPX*H5V3*H1P1+VZPY*H5V3*H1P2+VZPZ*H5V3*H1P3 )
C     TAU - Z
        VTRACK(12)=
     &           VPXX*H2P1*H5P1+VPYY*H2P2*H5P2+VPZZ*H2P3*H5P3+
     &           VPXY*(H2P1*H5P2+H5P1*H2P2)+
     &           VPXZ*(H2P1*H5P3+H5P1*H2P3)+
     &           VPYZ*(H2P2*H5P3+H5P2*H2P3)+
     &           (  VXPX*H5V1*H2P1+VXPY*H5V1*H2P2+VXPZ*H5V1*H2P3+
     &           VYPX*H5V2*H2P1+VYPY*H5V2*H2P2+VYPZ*H5V2*H2P3+
     &           VZPX*H5V3*H2P1+VZPY*H5V3*H2P2+VZPZ*H5V3*H2P3 )
C     FI0 - Z
        VTRACK(13)=
     &           VPXX*H3P1*H5P1+VPYY*H3P2*H5P2+
     &           VPXY*(H3P1*H5P2+H5P1*H3P2)+
     &           VPXZ*(H3P1*H5P3          )+
     &           VPYZ*(H3P2*H5P3          )+
     &           (  VXPX*H5V1*H3P1+VXPY*H5V1*H3P2+
     &           VYPX*H5V2*H3P1+VYPY*H5V2*H3P2+
     &           VZPX*H5V3*H3P1+VZPY*H5V3*H3P2  )
C     D0 - Z0
        VTRACK(14)=VVXX*H4V1*H5V1+VVYY*H4V2*H5V2+
     &           VVXY*(H4V1*H5V2+H5V1*H4V2)+
     &           VVXZ*(H4V1*H5V3          )+
     &           VVYZ*(H4V2*H5V3          )+
     &           VPXX*H4P1*H5P1+VPYY*H4P2*H5P2+
     &           VPXY*(H4P1*H5P2+H5P1*H4P2)+
     &           VPXZ*(H4P1*H5P3          )+
     &           VPYZ*(H4P2*H5P3          )+
     &           (  VXPX*(H4V1*H5P1+H5V1*H4P1)+
     &           VYPX*(H4V2*H5P1+H5V2*H4P1)+
     &           VZPX*(          H5V3*H4P1)+
     &           VXPY*(H4V1*H5P2+H5V1*H4P2)+
     &           VYPY*(H4V2*H5P2+H5V2*H4P2)+
     &           VZPY*(          H5V3*H4P2)+
     &           VXPZ*(H4V1*H5P3          )+
     &           VYPZ*(H4V2*H5P3          )  )
C
#if defined(YDEBUG)
C     INVERT 5X5 ERROR MATRIX
        IJ=0
        DO II=1,5
          DO JJ=1,II
            IJ=IJ+1
            EHI(II,JJ)=VTRACK(IJ)
            EHI(JJ,II)=EHI(II,JJ)
          ENDDO
        ENDDO
        CALL RSINV(5,EHI(1,1),5,ITEST)
        IF(ITEST.NE.0.OR.LDMP1.OR.LDMP2) THEN
          WRITE(LOUT,921) ITEST,(TRACK(I),I=1,5),(VTRACK(I),I=1,15)
  921     FORMAT(/5X,' YTPAR: RETURN TEST ON INVERTING ERROR MATRIX =',
     &      I2/,' NEUTRAL TRACK', 5F10.3/
     &      10X,'  VTRACK',1E10.3/18X,2E10.3/18X,3E10.3/
     &      18X,4E10.3/18X,5E10.3)
        ENDIF
#endif
C
C
        RETURN
C
      ENDIF
C
C  END NEUTRAL TRACK SECTION
C
C CHARGED TRACK SECTION
C
C     conversion radius of track <=> momentum
C     radius in meter , B in Tesla, p in GeV/c  q in units of e
C
C      p = 0.29979 * q * B * r
C
C     R[cm] = ROFP * P[Gev/c]:
C
      ROFP = 1./ (0.29979 * BFIELD / 10.) * 100.
C
C
      NGTRTO = 0
      SFI =PY/PT
      CFI =PX/PT
      FI =ATAN2(SFI ,CFI )
      QQ = IQ
      FR = -ROFP * PT / QQ
      FT=PZ/PT
      XC=VX-FR*SFI
      YC=VY+FR*CFI
      ETA=SQRT(XC**2+YC**2)* SIGN(ONE,FR)
      D0=FR-ETA
      SFI0=-XC/ETA
      CFI0= YC/ETA
      FI0=ATAN2(SFI0,CFI0)
      FIFI0=FI-FI0
      IF(FIFI0.GT.PI) FIFI0=FIFI0-TWO*PI
      IF(FIFI0.LT.-PI) FIFI0=FIFI0+TWO*PI
      FS=FR*(FI-FI0)
      Z0=VZ-FS*FT
C
C  CHARGED TRACK PARAMETERS
      TRACK(1)=ONE/FR
      TRACK(2)=FT
      TRACK(3)=FI0
      TRACK(4)=D0
      TRACK(5)=Z0
C
C DERIVATIVES OF TRACK PARAMETERS WITH RESPECT TO VERTEX AND MOMENTUM
      H1P1=-CFI/(FR*PT)
      H1P2=-SFI/(FR*PT)
C
      H2P1=-FT*CFI/PT
      H2P2=-FT*SFI/PT
      H2P3=ONE/PT
C
      H3V1=-CFI0/ETA
      H3V2=-SFI0/ETA
      H3P1=-FR*SFI0/(ETA*PT)
      H3P2=FR*CFI0/(ETA*PT)
C
      H4V1= SFI0
      H4V2=-CFI0
      H4P1=FR/PT*(CFI-CFI0)
      H4P2=FR/PT*(SFI-SFI0)
C
      H5V1=-FR*FT/ETA*CFI0
      H5V2=-FR*FT/ETA*SFI0
      H5V3=ONE
      H5P1=FR*FT/PT*(SFI-FR/ETA*SFI0)
      H5P2=-FR*FT/PT*(CFI-FR/ETA*CFI0)
      H5P3=-FS/PT
C
C  CHARGED TRACK PARAMETER ERRORS
C     INVERSE RADIUS
      VTRACK(1)=VPXX*H1P1**2+VPYY*H1P2**2+
     &   TWO*(VPXY*H1P1*H1P2)
C     SLOPE T
      VTRACK(3)=VPXX*H2P1**2+VPYY*H2P2**2+VPZZ*H2P3**2+
     &   TWO*(VPXY*H2P1*H2P2+VPXZ*H2P1*H2P3+
     &   VPYZ*H2P2*H2P3 )
C     FI0
      VTRACK(6)=VVXX*H3V1**2+VVYY*H3V2**2+
     &          VPXX*H3P1**2+VPYY*H3P2**2+
     &          TWO*(VVXY*H3V1*H3V2 + VXPX*H3V1*H3P1 + VXPY*H3V1*H3P2 +
     &          VYPX*H3V2*H3P1 + VYPY*H3V2*H3P2 + VPXY*H3P1*H3P2 )
C     D0
      VTRACK(10)=VVXX*H4V1**2+VVYY*H4V2**2+ TWO*VVXY*H4V1*H4V2+
     &           VPXX*H4P1**2+VPYY*H4P2**2+ TWO*VPXY*H4P1*H4P2+
     &           TWO*(  VXPX*H4V1*H4P1+VYPY*H4V2*H4P2+
     &           VXPY*H4V1*H4P2+VYPX*H4V2*H4P1)
C     Z0
      VTRACK(15)=VVXX*H5V1**2+VVYY*H5V2**2+VVZZ*H5V3**2+
     &  TWO*(VVXY*H5V1*H5V2+VVXZ*H5V1*H5V3+VVYZ*H5V2*H5V3)+
     &  VPXX*H5P1**2+VPYY*H5P2**2+VPZZ*H5P3**2+
     &  TWO*(VPXY*H5P1*H5P2+VPXZ*H5P1*H5P3+VPYZ*H5P2*H5P3)+
     &  TWO*(   VXPX*H5V1*H5P1+VYPY*H5V2*H5P2+VZPZ*H5V3*H5P3+
     &  VXPY*H5V1*H5P2+VYPX*H5V2*H5P1+
     &  VXPZ*H5V1*H5P3+VZPX*H5V3*H5P1+
     &  VYPZ*H5V2*H5P3+VZPY*H5V3*H5P2 )
C     INV.RADIUS - TAU
      VTRACK(2)=VPXX*H1P1*H2P1+VPYY*H1P2*H2P2+
     &   VPXY*(H1P1*H2P2+H2P1*H1P2)+VPXZ*(H1P1*H2P3          )+
     &   VPYZ*(H1P2*H2P3           )
C     INV.RADIUS - PHI
      VTRACK(4)=VXPX*H1P1*H3V1+VYPY*H1P2*H3V2+
     &      VXPY*  H3V1*H1P2 +VYPX*H3V2*H1P1+
     &      VPXX*H1P1*H3P1+VPYY*H1P2*H3P2+
     &      VPXY*(H1P1*H3P2+H3P1*H1P2)
C     TAU - PHI
      VTRACK(5)=VXPX*H3V1*H2P1+VXPY*H3V1*H2P2+VXPZ*H3V1*H2P3+
     &          VYPX*H3V2*H2P1+VYPY*H3V2*H2P2+VYPZ*H3V2*H2P3+
     &          VPXX*H2P1*H3P1+VPYY*H2P2*H3P2+
     &          VPXY*(H2P1*H3P2+H3P1*H2P2)+VPXZ*(          H3P1*H2P3)+
     &          VPYZ*(          H3P2*H2P3)
C     INV.RADIUS - D0
      VTRACK( 7)=
     &           VPXX*H1P1*H4P1+VPYY*H1P2*H4P2+
     &           VPXY*(H1P1*H4P2+H4P1*H1P2)+
     &           (  VXPX*H4V1*H1P1+VYPX*H4V2*H1P1+
     &           VXPY*H4V1*H1P2+VYPY*H4V2*H1P2)
C     TAU - D0
      VTRACK( 8)=
     &           VPXX*H2P1*H4P1+VPYY*H2P2*H4P2+
     &           VPXY*(H2P1*H4P2+H4P1*H2P2)+
     &           VPXZ*(          H4P1*H2P3)+
     &           VPYZ*(          H4P2*H2P3)+
     &           (  VXPX*H4V1*H2P1+VYPX*H4V2*H2P1+
     &           VXPY*H4V1*H2P2+VYPY*H4V2*H2P2+
     &           VXPZ*H4V1*H2P3+VYPZ*H4V2*H2P3 )
C     FI0 - D0
      VTRACK( 9)=VVXX*H3V1*H4V1+VVXY*(H3V1*H4V2+H3V2*H4V1)+
     &           VVYY*H3V2*H4V2+
     &           VPXX*H3P1*H4P1+VPYY*H3P2*H4P2+
     &           VPXY*(H3P1*H4P2+H4P1*H3P2)+
     &           VXPX*(H3V1*H4P1+H4V1*H3P1)+VYPX*(H3V2*H4P1+H4V2*H3P1)+
     &           VXPY*(H3V1*H4P2+H4V1*H3P2)+VYPY*(H3V2*H4P2+H4V2*H3P2)
C     INV.RADIUS - Z
      VTRACK(11)=
     &           VPXX*H1P1*H5P1+VPYY*H1P2*H5P2+
     &           VPXY*(H1P1*H5P2+H5P1*H1P2)+
     &           VPXZ*(H1P1*H5P3          )+
     &           VPYZ*(H1P2*H5P3          )+
     &           (  VXPX*H5V1*H1P1+VXPY*H5V1*H1P2+
     &           VYPX*H5V2*H1P1+VYPY*H5V2*H1P2+
     &           VZPX*H5V3*H1P1+VZPY*H5V3*H1P2                )
C     TAU - Z
      VTRACK(12)=
     &           VPXX*H2P1*H5P1+VPYY*H2P2*H5P2+VPZZ*H2P3*H5P3+
     &           VPXY*(H2P1*H5P2+H5P1*H2P2)+
     &           VPXZ*(H2P1*H5P3+H5P1*H2P3)+
     &           VPYZ*(H2P2*H5P3+H5P2*H2P3)+
     &           (  VXPX*H5V1*H2P1+VXPY*H5V1*H2P2+VXPZ*H5V1*H2P3+
     &           VYPX*H5V2*H2P1+VYPY*H5V2*H2P2+VYPZ*H5V2*H2P3+
     &           VZPX*H5V3*H2P1+VZPY*H5V3*H2P2+VZPZ*H5V3*H2P3 )
C     FI0 - Z
      VTRACK(13)=VVXX*H3V1*H5V1+VVXY*(H3V1*H5V2+H3V2*H5V1)+
     &           VVXZ*H3V1*H5V3+VVYY*H3V2*H5V2+VVYZ*H3V2*H5V3+
     &           VPXX*H3P1*H5P1+VPYY*H3P2*H5P2+
     &           VPXY*(H3P1*H5P2+H5P1*H3P2)+
     &           VPXZ*(H3P1*H5P3          )+
     &           VPYZ*(H3P2*H5P3          )+
     &           VXPX*(H3V1*H5P1+H5V1*H3P1)+VXPY*(H3V1*H5P2+H5V1*H3P2)+
     &           VXPZ*H3V1*H5P3+
     &           VYPX*(H3V2*H5P1+H5V2*H3P1)+VYPY*(H3V2*H5P2+H5V2*H3P2)+
     &           VYPZ*H3V2*H5P3+
     &           VZPX*H5V3*H3P1+VZPY*H5V3*H3P2
C     D0 - Z0
      VTRACK(14)=VVXX*H4V1*H5V1+VVYY*H4V2*H5V2+
     &           VVXY*(H4V1*H5V2+H5V1*H4V2)+
     &           VVXZ*(H4V1*H5V3          )+
     &           VVYZ*(H4V2*H5V3          )+
     &           VPXX*H4P1*H5P1+VPYY*H4P2*H5P2+
     &           VPXY*(H4P1*H5P2+H5P1*H4P2)+
     &           VPXZ*(H4P1*H5P3          )+
     &           VPYZ*(H4P2*H5P3          )+
     &           (  VXPX*(H4V1*H5P1+H5V1*H4P1)+
     &           VYPX*(H4V2*H5P1+H5V2*H4P1)+
     &           VZPX*(          H5V3*H4P1)+
     &           VXPY*(H4V1*H5P2+H5V1*H4P2)+
     &           VYPY*(H4V2*H5P2+H5V2*H4P2)+
     &           VZPY*(          H5V3*H4P2)+
     &           VXPZ*(H4V1*H5P3          )+
     &           VYPZ*(H4V2*H5P3          )  )
C
#if defined(YDEBUG)
C     INVERT 5X5 ERROR MATRIX
      IJ=0
      DO II=1,5
        DO JJ=1,II
          IJ=IJ+1
          EHI(II,JJ)=VTRACK(IJ)
          EHI(JJ,II)=EHI(II,JJ)
        ENDDO
      ENDDO
      CALL RSINV(5,EHI(1,1),5,ITEST)
      IF(ITEST.NE.0.OR.LDMP1.OR.LDMP2) THEN
        WRITE(LOUT,1921) ITEST,(TRACK(I),I=1,5),(VTRACK(I),I=1,15)
 1921   FORMAT(/5X,' YTPAR: RETURN TEST ON INVERTING ERROR MATRIX =',I2/
     &   , ' CHARGED TRACK', 5F10.3/
     &    10X,'  VTRACK',1E10.3/18X,2E10.3/18X,3E10.3/
     &    18X,4E10.3/18X,5E10.3)
      ENDIF
#endif
C
C
      RETURN
C
C
C  END CHARGED TRACK SECTION
C
C
      END
#endif
