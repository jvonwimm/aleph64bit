      SUBROUTINE YFTVTR(NVX,NHX,NEU,LVAPC,
     &      VXIN,VVXIN,IXHX,NSHX,NSVHX,HXIN,VHXIN,
     &      IXNU,NSNU,NSVNU,TNU,VTNU,
     &      VXOUT,VVXOU,CHISQ,IFAIL)
C
C----------------------------------------------------------*
C!    Fit a vertex to vertices,charged and neutral tracks
CKEY YTOP VERTEX FIT
C!    Author :     G. Lutz     /11/88
C!    Modified  :  G. Lutz   30/03/92
C!
C!    Description
C!    ===========
C!    This routine provides a fit of NVX vertices,
C!    NHX helices and NEU neutral tracks to a new common
C!    vertex
C!
C! INPUT
C!    NVX .......... # OF VERTICES TO BE USED IN FIT
C!    NHX .......... # OF HELICES TO BE USED IN FIT
C!    NEU .......... # OF NEUTRAL TRACKS TO BE USED IN FIT
C!    LVAPC ....... LOGICAL: CALCULATE STARTING VALUES OF VERTEX
C!    VXIN(3,I) .... X,Y,Z OF INPUT VERTEX I
C!    VVXIN(6,I) ... VARIANCE OF INPUT VERTEX I
C!    IXHX(K) ...... INDEX OF HELIX K IN BUFFER HXIN
C!    NSHX ......... SPACING BETWEEN VECTORS IN BUFFER HXIN
C!    NSVHX ........ SPACING BETWEEN ERROR MATRICES IN BUFFER NSVHX
C!    HXIN ......... HELIX PARAMETERS ORDERED IN SEQUENCE
C!                   RHO=1/R(SIGNED); T; PHI0; D0(SIGNED); Z0
C!                   RHO>0 FOR COUNTERCLOCKWISE BENDING
C!                   D0.GT.0 IF MOMENTUM AROUND ORIGIN IS POSITIVE
C!                   FIRST ELEMENT STARTS AT I=(IXHX-1)*NSHX+1
C!    VHXIN(I) ..... CORRESPONDING VARIANCES ORDERED AS
C!                   RHO;
C!                   RHO.T; T
C!                   RHO.PHI0;   T.PHI0    PHI0
C!                   RHO.D0;    T.D0      PHIZERO     D0
C!                   RHO.Z0     T.Z0     PHI0.Z0    D0.Z0    Z0
C!                   FIRST ELEMENT IS AT I=(IXHX-1)*NSVHX+1
C!    IXNU(K) ...... INDEX OF HELIX K IN BUFFER HXIN
C!    NSNU ......... SPACING BETWEEN VECTORS IN BUFFER HXIN
C!    NSVNU ........ SPACING BETWEEN ERROR MATRICES IN BUFFER NSVHX
C!    TNU .......... NEUTRAL TRACK PARAMETERS ORDERED IN SEQUENCE
C!                   P; T; PHI0; D0(SIGNED); Z0
C!                   D0.GT.0 IF MOMENTUM AROUND ORIGIN IS POSITIVE
C!                   FIRST ELEMENT STARTS AT I=(IXHX-1)*NSHX+1
C!    VTNU(I) ...... CORRESPONDING VARIANCES ORDERED AS
C!                   RHO;
C!                   RHO.T; T
C!                   RHO.PHI0;   T.PHI0    PHI0
C!                   RHO.D0;    T.D0      PHIZERO     D0
C!                   RHO.Z0     T.Z0     PHI0.Z0    D0.Z0    Z0
C!                   FIRST ELEMENT IS AT I=(IXHX-1)*NSVHX+1
C!
C! OUTPUT
C!    VXOUT .........VERTEX X,Y,Z
C!    VVXOU ........CORRESPONDING VARIANCES X, XY, Y, XZ, YZ, Z
C!    CHISQ ........ VERTEX CHISQ
C!    IFAIL ........ =1,2,3, PAIR OF TRACKS MISSING BY LARGE AMOUNT IN
C!                    VTX STARTING VALUE SEARCH
C!                   =9,  NO APPROXIMATIVE VERTEX FOUND
C!                   =10, # OF INPUT VERTICES ABOVE ALLOWED MAXIMUM
C!                   =11, # OF CHARGED TRACKS ABOVE ALLOWED MAXIMUM
C!                   =12, # OF NEUTRAL TRACKS ABOVE ALLOWED MAXIMUM
C!                   =21, ERROR IN INPUT VTX ERROR MATRIX
C!                   =22, ERROR IN INPUT HELIX ERROR MATRIX
C!                   =30, ERROR IN GG MATRIX INVERSION
C!
C!
C!---------------------------------------------------------*
#ifndef DOC
      SAVE
#include "bcs.h"
#include "ydimto.h"
#include "yparto.h"
#include "ydumto.h"
C
C
      DIMENSION VXIN(3,*),VVXIN(6,*)
      DIMENSION IXHX(*),HXIN(*),VHXIN(*)
      DIMENSION IXNU(*),TNU(*),VTNU(*),VXOUT(*),VVXOU(*)
C  APPROXIMATIVE VERTEX CALCULATION
      DOUBLE PRECISION D,R1A,R2A,DX,X0,Y0,ALEN,FANGL,SANGL,CANGL,CFPP
      DIMENSION VAPP(3,2),SAPP(2,2),
     &  ZAPP(2,2),JA(2),DZAPP(2)
      DOUBLE PRECISION CFAPP(2,2),SFAPP(2,2),FAPP(2,2)
      EQUIVALENCE (XA,VAPP(1,1)),(XB,VAPP(1,2)),(YA,VAPP(2,1)),
     &  (YB,VAPP(2,2)),(J1,JA(1)),(J2,JA(2))
      DIMENSION HXCR(2),HYCR(2),HRR(2)
      EQUIVALENCE (HR1,HRR(1)),(HR2,HRR(2)),
     &  (HXC1,HXCR(1)),(HXC2,HXCR(2)),(HYC1,HYCR(1)),(HYC2,HYCR(2))
      DIMENSION VXA(3,100)
C
C
      DIMENSION KHX(5),JHX(15)
C
      DOUBLE PRECISION VXI(3,7),VVXI(6,7)
      DOUBLE PRECISION HXI(5,MAXTRK), VHXI(15,MAXTRK),VXO(3),VMTRX(3)
C
      DOUBLE PRECISION HSF0(MAXTRK),HCF0(MAXTRK),HR(MAXTRK),
     &      HX0(MAXTRK),HY0(MAXTRK),HXC(MAXTRK),HYC(MAXTRK)
C
      DOUBLE PRECISION A,B,C
      EQUIVALENCE (A,VXO(1)),(B,VXO(2)),(C,VXO(3))
C
      DOUBLE PRECISION SIGRO
C
      DOUBLE PRECISION HL,HDR,HX,HY,HSFI,HCFI,HFI,HS,HZ,HDZ
      DOUBLE PRECISION HRHOS,COSRS,SINRS,ARHO,AFI,AD0,BRHO,BFI,BD0
C
C     DEVIATION VECTOR FROM VERTEX
      DOUBLE PRECISION DELV(3)
C     DEVIATION VECTOR FROM HELIX
      DOUBLE PRECISION DELTA(2)
C     DERIVATIVES OF DEVIATIONS WITH RESPECT TO INDEP. PAR. D(DELTA)/D(E
      DOUBLE PRECISION T(2,3)
C     SECOND DERIVATIVES WITH RESP. TO INDEP. PAR. (DIM. 2*3*3)
C     MATRIX PRODUCT DELTA(TRANS) * E**-1
C     ERROR MATRIX VERTEX, INVERSE
      DOUBLE PRECISION EV(3,3),EVI(3,3,7)
C     ERROR MATRIX HELIX (TRANSVERSE)
      DOUBLE PRECISION EHX(2,2)
C     INVERSE ERROR MATRIX FOR ALL TRACKS
      DOUBLE PRECISION EINV(2,2,MAXTRK),DET
C     FIRST AND SECOND DERIVATIVE OF CHISQ WITH RESP. TO INDEP. PAR.
      DOUBLE PRECISION G(3),GG(3,3),RGG(9)
      EQUIVALENCE (GG,RGG)
C     INTERMEDIATE MATRIX : T(TRANS) * E**-1      3*2
      DOUBLE PRECISION TEINV(3,2)
C
C     INDEP. PARAMETER CHANGE
      DOUBLE PRECISION DELVX(3)
C
      DOUBLE PRECISION PI
      DOUBLE PRECISION HALF,ONE,TWO
C
      LOGICAL LNEMX,LVAPC
C
#if defined(YDEBUG)
      LOGICAL LDMP1,LDMP2
#endif
C
C set counters to avoid very large nb of error warnings
      DATA ICNER1/0/,ICNER2/0/,ICNER3/0/,ICNMAX/20/
C*******************
C
C     COPY SEQUENCE OF HELIX PARAMETER AND ERRORS
      DATA KHX/1,2,3,4,5/,JHX/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15/
C
C
C     DOUBLE PRECISION CONSTANTS
      DATA PI/3.141592654D+00/
      DATA HALF/0.5D+00/,ONE/1.D+00/,TWO/2.D+00/
C
      DATA MXITR/ 5/,CHISC/1.E-2/,CHISR/.01/
C
C     HELIX PARAMETER INTERNAL ORDER
      DATA KR,KT,KF,KD,KZ/1,2,3,4,5/
C     HELIX ERROR VARIANCES ORDER
      DATA JRR,JRT,JRF,JRD,JRZ/1,2, 4, 7,11/
     &         JTT,JTF,JTD,JTZ/  3, 5, 8,12/
     &         JFF,JFD,JFZ/     6, 9,13/
     &         JDD,JDZ/       10,14/
     &         JZZ/          15/
C
C
      DATA MXIVX/7/
C
C     RECALC. TRANSV.ERR. MATRIX FOR VTX SHIFT ABOVE SQRT(DV2MX)
      DATA DV2MX/0.25/
C     CRITERIA FOR AGREEMENT BETWEEN APPROX. VTX. SOL.
      DATA D2VAP/5./
C
      DATA NENTY/0/
C
      DATA MXCOM/100/
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
#if defined(YDEBUG)
      IF(IDUMPP(33).GE.1) THEN
        LDMP1=.TRUE.
      ELSE
        LDMP1=.FALSE.
      ENDIF
C
      IF(IDUMPP(34).GE.1) THEN
        LDMP2=.TRUE.
      ELSE
        LDMP2=.FALSE.
      ENDIF
C
      IDUMPP(33)=IDUMPP(33)-1
      IDUMPP(34)=IDUMPP(34)-1
      IF(LDMP1.OR.LDMP2)  WRITE(LOUT,10) NENTY,NVX,NHX,NEU,LVAPC
     &  ,(IXHX(I),I=1,NHX),(IXNU(I),I=1,NEU)
   10 FORMAT(1X,3(1H*),' YFTVTR CALLED',I5,' TIMES',
     &  '  NVX=',I3,'  NHX=',I3,'  NEU=',I3,
     &  '  LVAPC=',L5/(10X,'IXHX,IXNU',20I2))
#endif
C
C     RESET OUTPUT VALUES
      DO I=1,3
        VXOUT(I) = 0.
      ENDDO
      DO I=1,6
        VVXOU(I) = 0.
      ENDDO
      CHISQ = 1.E+10
C
C
      IFAIL=0
C
      IF(NVX.GT.MXIVX) THEN
        WRITE(LOUT,280) NVX,MXIVX
  280   FORMAT(' ******** YFTVTR:  NVX=',I5,
     &     ' ABOVE ALLOWED MAXIMUM',I5)
        IFAIL=10
        GOTO 997
      ENDIF
C
      IF(NHX.GT.MAXHLX) THEN
        WRITE(LOUT,281) NHX,MAXHLX
  281   FORMAT(' ******** YFTVTR:  NHX=',I5,
     &     ' ABOVE ALLOWED MAXIMUM',I5)
        IFAIL=11
        GOTO 997
      ENDIF
C
      IF(NEU.GT.MAXNTR) THEN
        WRITE(LOUT,282) NEU,MAXNTR
  282   FORMAT(' ******** YFTVTR:  NEU=',I5,
     &     ' ABOVE ALLOWED MAXIMUM',I5)
        IFAIL=12
        GOTO 997
      ENDIF
C
C
C     USE MORE CONVENIENT SIGN CONVENTION: COPY INPUT VECTORS
C     INPUT VERTICES
      DO 7300 IVX=1,NVX
        DO 7100 I=1,3
          VXI(I,IVX)=VXIN(I,IVX)
 7100   CONTINUE
        DO 7200 I=1,6
          VVXI(I,IVX)=VVXIN(I,IVX)
 7200   CONTINUE
C
C     VERTEX ERROR MATRIX
        EV(1,1)=VVXI(1,IVX)
        EV(2,2)=VVXI(3,IVX)
        EV(3,3)=VVXI(6,IVX)
        EV(1,2)=VVXI(2,IVX)
        EV(2,1)=VVXI(2,IVX)
        EV(2,3)=VVXI(5,IVX)
        EV(3,2)=VVXI(5,IVX)
        EV(1,3)=VVXI(4,IVX)
        EV(3,1)=VVXI(4,IVX)
C
#if defined(YDEBUG)
        IF(LDMP2) THEN
          WRITE(LOUT,901) (VXI(I,IVX),I=1,3),(VVXI(I,IVX),I=1,6)
  901     FORMAT(/5X,'  VXI',3F10.3/5X,'  VVXI',6E10.3)
          WRITE(LOUT,902) ((EV(I,J),J=1,3),I=1,3)
  902     FORMAT(/(5X,'  EV',3E10.3))
        ENDIF
#endif
C
C
        CALL YMS3IN(EV(1,1),EVI(1,1,IVX),IFLLL)
C
        IF(IFLLL.NE.0) THEN
C   PROBLEM IN INPUT VERTEX ERROR MATRIX
          IFAIL=21
          ICNER1=ICNER1+1
#if defined(YDEBUG)
          IF(ICNER1.LE.ICNMAX) THEN
            CALL ABRUEV(IRUN,IEVT)
            WRITE(LOUT,903) IFLLL,IRUN,IEVT,IVX,((EV(I,J),J=1,3),I=1,3)
  903       FORMAT(/5X,' YFTVTR CALLED YMS3IN; IFLLL=',I5,
     &        ' RUN',I7,' EVENT',I8/
     &        5X,' PROBLEM WITH INPUT VERTEX ERROR MATRIX'/
     &        (5X,' IVX=',I5,'  EV',9E10.3))
            IF(ICNER1.EQ.ICNMAX) WRITE(LOUT,9033)
 9033       FORMAT(' !!!!!!! YFTVTR : WARNING !!!!!!! :'/,
     &      ' more than 20 errors in matrix inversion routine YMS3IN ',
     &      ' stop printing messages ')
          ENDIF
#endif
          GOTO 997
        ENDIF
C
 7300 CONTINUE
C
C     INPUT HELICES
C
      DO 500 K=1,NHX
        IX=(IXHX(K)-1)*NSHX
        DO 300 I=1,5
          HXI(I,K)=HXIN(IX+KHX(I))
  300   CONTINUE
        SIGRO=SIGN(ONE,HXI(KR,K))
C
        IX=(IXHX(K)-1)*NSVHX
        DO 400 I=1,15
          VHXI(I,K)=VHXIN(IX+JHX(I))
  400   CONTINUE
C
C     MORE HELIX PARAMETERS
        HSF0(K)=SIN(HXI(KF,K))
        HCF0(K)=COS(HXI(KF,K))
C     RADIUS
        HR(K)=ONE/HXI(KR,K)
C     CLOSEST POINT TO ORIGIN
        HX0(K)= HXI(KD,K)*HSF0(K)
        HY0(K)=-HXI(KD,K)*HCF0(K)
C     CENTRE OF CIRCLE
        HXC(K)=HX0(K)-HR(K)*HSF0(K)
        HYC(K)=HY0(K)+HR(K)*HCF0(K)
C
C
#if defined(YDEBUG)
        IF(LDMP2) THEN
          WRITE(LOUT,907)
     &           (HXI(I,K),I=1,5),(VHXI(I,K),I=1,15),
     &           HR(K),HX0(K),HY0(K),HXC(K),HYC(K)
  907     FORMAT(/
     &      5X,'  HXI',5E10.3/'  VHXI',8E10.3/ 6X, 7E10.3/
     &      5X,'  HR=',F10.3,'  HX0,HY0',2F10.3,'  HXC,HYC',2F10.3)
        ENDIF
#endif
C
  500 CONTINUE
C
      DO 8500 KK=1,NEU
        K=KK+NHX
        IX=(IXNU(KK)-1)*NSNU
        DO 8300 I=1,5
          HXI(I,K)=TNU(IX+KHX(I))
 8300   CONTINUE
        SIGRO=SIGN(ONE,HXI(KR,K))
C
        IX=(IXNU(KK)-1)*NSVNU
        DO 8400 I=1,15
          VHXI(I,K)=VTNU(IX+JHX(I))
 8400   CONTINUE
C
C     MORE TRACK PARAMETERS
        HSF0(K)=SIN(HXI(KF,K))
        HCF0(K)=COS(HXI(KF,K))
C     CLOSEST POINT TO ORIGIN
        HX0(K)= HXI(KD,K)*HSF0(K)
        HY0(K)=-HXI(KD,K)*HCF0(K)
C
C
#if defined(YDEBUG)
        IF(LDMP2) THEN
          WRITE(LOUT,8907)
     &           (HXI(I,K),I=1,5),(VHXI(I,K),I=1,15),
     &           HX0(K),HY0(K)
 8907     FORMAT(/
     &      5X,'  HXI',5E10.3/'  VHXI',8E10.3/ 6X, 7E10.3/
     &      5X,'  HX0,HY0',2F10.3)
        ENDIF
#endif
C
 8500 CONTINUE
C
      LNEMX=.TRUE.
C
C     STARTING VALUE FOR ITERATIONS: INPUT VERTEX
      A=VXIN(1,1)
      B=VXIN(2,1)
      C=VXIN(3,1)
C
      IF(LVAPC) THEN
C
C     FIND STARTING VERTEX FROM CROSSING PAIRS OF TRACKS
        ICOM=0
        NFTRK=NHX+NEU
        DO 495 ITR=1,NFTRK
          JA(1)=ITR
          JA(2)=MOD(ITR,NFTRK)+1
C
          IF(J1.LE.NHX) THEN
            HXC1=HXC(J1)
            HYC1=HYC(J1)
            HR1=HR(J1)
          ELSE
            HR1=1.E+4
            HXC1=-(HR1-HXI(KD,J1))*SIN(HXI(KF,J1))
            HYC1= (HR1-HXI(KD,J1))*COS(HXI(KF,J1))
          ENDIF
C
          IF(J2.LE.NHX) THEN
            HXC2=HXC(J2)
            HYC2=HYC(J2)
            HR2=HR(J2)
          ELSE
            HR2=1.E+4
            HXC2=-(HR2-HXI(KD,J2))*SIN(HXI(KF,J2))
            HYC2= (HR2-HXI(KD,J2))*COS(HXI(KF,J2))
          ENDIF
C
          IF(ICOM.GT.MXCOM) GO TO 600
          D=SQRT((HXC1-HXC2)**2+(HYC1-HYC2)**2)
C ANGLE WITH RESPECT TO Y-AXIS OF VECTOR POINTING FROM 1 TO 2
          CANGL=-(HYC1-HYC2)/D
          SANGL=(HXC1-HXC2)/D
C
          R1A=ABS(HR1)
          R2A=ABS(HR2)
C     CHECK FOR TRACK PROJECTIONS MISSING BY TOO GREAT DISTANCE
          IF((D-R1A-R2A).GT.DHXLIM) GO TO 495
          IF(D.LT.(ABS(R1A-R2A)-DHXLIM) ) GO TO 495
C
#if defined(YDEBUG)
          IF(LDMP2) THEN
            WRITE(LOUT,405) J1,J2,D,R1A,R2A,SANGL,CANGL
  405       FORMAT(5X,'YFTVTR: J1,J2,D',2I5,F8.3,
     &        ' R1A,R2A',2F8.3,' SANGL,CANGL',2F8.3)
          ENDIF
#endif
C
          IF(D.GT.(R1A+R2A))   GO TO 430
          IF(D.LT.ABS(R1A-R2A)) GO TO 420
  410     CONTINUE
C     CIRCLES CROSSING
          KASE=1
          DX=(HR1**2-HR2**2+D**2)/(TWO*D)
          DH2=   (HR1**2-DX**2)
          DH=0.
          IF(DH2.GT.0.) DH=SQRT(DH2)
          X0=HXC1-DX*SANGL
          Y0=HYC1+DX*CANGL
          XA=SNGL(X0)-DH*SNGL(CANGL)
          XB=SNGL(X0)+DH*SNGL(CANGL)
          YA=SNGL(Y0)-DH*SNGL(SANGL)
          YB=SNGL(Y0)+DH*SNGL(SANGL)
C
#if defined(YDEBUG)
          IF(LDMP2)
     &      WRITE(LOUT,412)  DX,DH,X0,Y0,XA,XB,YA,YB
  412     FORMAT(' DX,DH',2F8.3,' X0,Y0',2F8.3,' XA,XB,YA,YB',4F8.3)
#endif
          DO 415 ISOL=1,2
            DO 414 IR=1,2
              CFAPP(IR,ISOL)=-(VAPP(2,ISOL)-HYCR(IR))/
     &          HRR(IR)
              SFAPP(IR,ISOL)= (VAPP(1,ISOL)-HXCR(IR))/
     &          HRR(IR)
              CFPP=CFAPP(IR,ISOL)
              FAPP(IR,ISOL)=ATAN2(SFAPP(IR,ISOL),CFAPP(IR,ISOL))
              IF((FAPP(IR,ISOL)-HXI(KF,JA(IR))).LT.-PI)
     &          FAPP(IR,ISOL)=FAPP(IR,ISOL)+TWO*PI
              SAPP(IR,ISOL)= (FAPP(IR,ISOL)-HXI(KF,JA(IR)))*HRR(IR)
              ZAPP(IR,ISOL)= SNGL(HXI(KZ,JA(IR)))+
     &          SAPP(IR,ISOL)*SNGL(HXI(KT,JA(IR)))
  414       CONTINUE
            DZAPP(ISOL)=ZAPP(2,ISOL)-ZAPP(1,ISOL)
  415     CONTINUE
#if defined(YDEBUG)
          IF(LDMP2) THEN
            WRITE(LOUT,416) VAPP,DZAPP,CFAPP,SFAPP,FAPP,SAPP,ZAPP
  416       FORMAT( 4X,' VAPP',3F8.3,2X,3F8.3,' DZAPP',2F8.3/
     &        4X,' CFAPP',4F7.3,' SFAPP',4F7.3,' FAPP',4F8.3/
     &        4X,' SAPP',4F8.3,' ZAPP', 4F8.3)
          ENDIF
#endif
C
          ISOL=1
          IF(ABS(DZAPP(2)).LT.ABS(DZAPP(1))) ISOL=2
          IF(ABS(DZAPP(ISOL)).GT.DHXLIM) THEN
C     SPECIAL TREATMENT FOR TRACK PROJECTIONS CROSSING UNDER
C     SHALLOW ANGLE: INTERPOLATE SOLUTIONS SO AS TO GET
C     CROSSINGS IN Z
C
C     FIND POINT WHERE Z-DISTANCE = 0
            SIGR1=SIGN(1. ,HR1)
            SIGR2=SIGN(1. ,HR2)
            SIG=SIGR1*SIGR2
            IF(D.GT.MAX(R1A,R2A)) SIG=-SIG
            DDZDS=HXI(KT,J1)-HXI(KT,J2)*DBLE(SIG)
            IF(ABS(DDZDS).LT.1.E-6) THEN
              DSA=0.
            ELSE
              DSA=DZAPP(ISOL)/DDZDS
            ENDIF
C     FIND POINT WHERE PROJECTED DISTANCE = DHXLIM
            IF(SIG.GT.0.)   THEN
              DDRDS=2.*ABS(SIN(SNGL(FAPP(2,ISOL)-FAPP(1,ISOL))/2.))
            ELSE
              DDRDS=2.*ABS(COS(SNGL(FAPP(2,ISOL)-FAPP(1,ISOL))/2.))
            ENDIF
            DSB=DHXLIM/MAX(DDRDS,1.E-20)
C     SELECT CLOSEST OF TWO POINTS
            DS1=DSA
            IF(DSB.LT.ABS(DSA)) THEN
              DS1=SIGN(DSB,DSA)
            ENDIF
            DS2=DS1*SIG
            ZAPP(1,ISOL)=ZAPP(1,ISOL)+DS1*SNGL(HXI(KT,J1))
            ZAPP(2,ISOL)=ZAPP(2,ISOL)+DS2*SNGL(HXI(KT,J2))
            DZAPP(ISOL)=ZAPP(2,ISOL)-ZAPP(1,ISOL)
C
C     CHECK IF HELICES MISS BY LARGE DISTANCE
            IF (ABS(DZAPP(ISOL)).GT.DHXLIM) THEN
              IFAIL=1
              GOTO 997
            ENDIF
C
C     MOVE STARTING POINT FOR VERTEX BY DS1
            VXA(1,ICOM+1)=VAPP(1,ISOL)+
     &      .5*(DS1*COS(SNGL(FAPP(1,ISOL)))+DS2*COS(SNGL(FAPP(2,ISOL))))
            VXA(2,ICOM+1)=VAPP(2,ISOL)+
     &      .5*(DS1*SIN(SNGL(FAPP(1,ISOL)))+DS2*SIN(SNGL(FAPP(2,ISOL))))
C
C
C     check the distances between the helices in space
C     not yet coded
C           IF(DSQ.GT.DHXLIM**2) GO TO 495
C     APPROX VTX FOUND
            ICOM=ICOM+1
            VXA(3,ICOM)=0.5*(ZAPP(1,ISOL)+ZAPP(2,ISOL))
            GO TO 490
          ENDIF
C
          ICOM=ICOM+1
          DO  418 I=1,2
            VXA(I,ICOM)=VAPP(I,ISOL)
  418     CONTINUE
          VXA(3,ICOM)=0.5*(ZAPP(1,ISOL)+ZAPP(2,ISOL))
          GO TO 490
C
  420     CONTINUE
C     ENCLOSED CIRCLES
          KASE=2
          IF(R1A.GT.R2A) GO TO 425
          ALEN=HALF*(-D+R1A+R2A)
          DCIRC=R2A-R1A-D
          FANGL=0.
          GO TO 426
  425     ALEN=HALF*(-D-R1A-R2A)
          DCIRC=R1A-R2A-D
          FANGL=PI
  426     CONTINUE
C     CHECK IF HELICES MISS BY LARGE DISTANCE
          IF(DCIRC.GT.DHXLIM) THEN
            IFAIL=2
            GOTO 997
          ENDIF
C
          VXA(1,ICOM+1)=HXC1+ALEN*SANGL
          VXA(2,ICOM+1)=HYC1-ALEN*CANGL
          FANGL=FANGL+ATAN2(SANGL,CANGL)
C
          DO 427 IR=1,2
            FAPP(IR,1)=FANGL
            IF(HRR(IR).LT.0.) FAPP(IR,1)=FAPP(IR,1)-PI
            IF((FAPP(IR,1)-HXI(KF,JA(IR))).LT.-PI)
     &        FAPP(IR,1)=FAPP(IR,1)+TWO*PI
            IF((FAPP(IR,1)-HXI(KF,JA(IR))).GT. PI)
     &        FAPP(IR,1)=FAPP(IR,1)-TWO*PI
            SAPP(IR,1)=(FAPP(IR,1)-HXI(KF,JA(IR)))*HRR(IR)
            ZAPP(IR,1)=SNGL(HXI(KZ,JA(IR)))+SAPP(IR,1)*
     &        SNGL(HXI(KT,JA(IR)))
  427     CONTINUE
          DZAPP(1)=ZAPP(2,1)-ZAPP(1,1)
C
C     FIND POINT WHERE Z-DISTANCE = 0
          SIGR1=SIGN(1. ,HR1)
          SIGR2=SIGN(1. ,HR2)
          DDZDS=HXI(KT,J1)-HXI(KT,J2)*DBLE(SIGR1*SIGR2)
          IF(ABS(DDZDS).LT.1.E-6) THEN
            DSA=0.
          ELSE
            DSA=DZAPP(1)/DDZDS
          ENDIF
C     FIND POINT WHERE PROJECTED DISTANCE = DHXLIM
          DDRS2=HALF*ABS(ONE/R1A-ONE/R2A)
          DSB=SQRT((DHXLIM-DCIRC)/MAX(DDRS2,1.E-20))
C
C     SELECT CLOSEST OF TWO POINTS
          DS1=DSA
          IF(DSB.LT.ABS(DSA)) THEN
            DS1=SIGN(DSB,DSA)
          ENDIF
          DS2=DS1*SIGR1*SIGR2
          ZAPP(1,1)=ZAPP(1,1)+DS1*SNGL(HXI(KT,J1))
          ZAPP(2,1)=ZAPP(2,1)+DS2*SNGL(HXI(KT,J2))
          DZAPP(1)=ZAPP(2,1)-ZAPP(1,1)
C
C     CHECK IF HELICES MISS BY LARGE DISTANCE
          IF (ABS(DZAPP(1)).GT.DHXLIM) THEN
            IFAIL=2
            GOTO 997
          ENDIF
C
C     MOVE STARTING POINT FOR VERTEX BY DS1
          VXA(1,ICOM+1)=VXA(1,ICOM+1)+DS1*COS(SNGL(FAPP(1,1)))
          VXA(2,ICOM+1)=VXA(2,ICOM+1)+DS1*SIN(SNGL(FAPP(1,1)))
C
#if defined(YDEBUG)
          IF(LDMP2) THEN
            WRITE(LOUT,428) IXHX(J1),HXC1,HYC1,HR1,IXHX(J2),
     &        HXC2,HYC2,HR2,D,VXA(1,ICOM+1),VXA(2,ICOM+1),
     &        FANGL,FAPP(1,1),FAPP(2,1),SAPP(1,1),SAPP(2,1),ZAPP(1,1),
     &        ZAPP(2,1),DZAPP(1)
  428       FORMAT(3X,'YFTVTR:ENCL.CRCL. IXH,HXC,HYC,HR',I3,3F9.3,
     &        I3,3F9.3, ' D',1F9.3,' VXA',2F8.3/
     &        14X, 10X,' FANGL',F8.3,' FAPP',2F8.3,
     &        ' SAPP',2F8.3,' ZAPP',2F8.3,' DZAPP',F8.3)
          ENDIF
#endif
C
          GO TO 485
C
  430     CONTINUE
C     CIRCLES SEPARATED
          KASE=3
          DCIRC=(D-R1A-R2A)
C     CHECK IF HELICES MISS BY LARGE DISTANCE
          IF(DCIRC.GT.DHXLIM) THEN
            IFAIL=3
            GOTO 997
          ENDIF
C
          ALEN=HALF*(-D-R1A+R2A)
          VXA(1,ICOM+1)=HXC1+ALEN*SANGL
          VXA(2,ICOM+1)=HYC1-ALEN*CANGL
          FANGL=ATAN2(SANGL,CANGL)
          FAPP(1,1)=FANGL
C:::::
          IF(HR1.GT.0.) FAPP(1,1)=FAPP(1,1)+PI
C:::::
          FAPP(2,1)=FANGL
C:::::
          IF(HR2.LT.0.) FAPP(2,1)=FAPP(2,1)+PI
C:::::
          DO 480 IR=1,2
            IF((FAPP(IR,1)-HXI(KF,JA(IR))).LT.-PI)
     &        FAPP(IR,1)=FAPP(IR,1)+TWO*PI
            IF((FAPP(IR,1)-HXI(KF,JA(IR))).GT. PI)
     &        FAPP(IR,1)=FAPP(IR,1)-TWO*PI
            SAPP(IR,1)=(FAPP(IR,1)-HXI(KF,JA(IR)))*HRR(IR)
            ZAPP(IR,1)=SNGL(HXI(KZ,JA(IR)))+SAPP(IR,1)*
     &        SNGL(HXI(KT,JA(IR)))
  480     CONTINUE
          DZAPP(1)=ZAPP(2,1)-ZAPP(1,1)
C
C     FIND POINT WHERE Z-DISTANCE = 0
          SIGR1=SIGN(1. ,HR1)
          SIGR2=SIGN(1. ,HR2)
          DDZDS=HXI(KT,J1)+HXI(KT,J2)*DBLE(SIGR1*SIGR2)
          IF(ABS(DDZDS).LT.1.E-6) THEN
            DSA=0.
          ELSE
            DSA=DZAPP(1)/DDZDS
          ENDIF
C     FIND POINT WHERE PROJECTED DISTANCE = DHXLIM
          DDRS2=HALF*(ONE/R1A+ONE/R2A)
          DSB=SQRT((DHXLIM-DCIRC)/MAX(DDRS2,1.E-20))
C     SELECT CLOSEST OF TWO POINTS
          DS1=DSA
          IF(DSB.LT.ABS(DSA)) THEN
            DS1=SIGN(DSB,DSA)
          ENDIF
          DS2=-DS1*SIGR1*SIGR2
          ZAPP(1,1)=ZAPP(1,1)+DS1*SNGL(HXI(KT,J1))
          ZAPP(2,1)=ZAPP(2,1)+DS2*SNGL(HXI(KT,J2))
          DZAPP(1)=ZAPP(2,1)-ZAPP(1,1)
C
C     CHECK IF HELICES MISS BY LARGE DISTANCE
          IF (ABS(DZAPP(1)).GT.DHXLIM) THEN
            IFAIL=3
            GOTO 997
          ENDIF
C
C     MOVE STARTING POINT FOR VERTEX BY DS1
          VXA(1,ICOM+1)=VXA(1,ICOM+1)+DS1*COS(SNGL(FAPP(1,1)))
          VXA(2,ICOM+1)=VXA(2,ICOM+1)+DS1*SIN(SNGL(FAPP(1,1)))
C
#if defined(YDEBUG)
          IF(LDMP2) THEN
            WRITE(LOUT,482) IXHX(J1),HXC1,HYC1,HR1,
     &        IXHX(J2),HXC2,HYC2,HR2,D,VXA(1,ICOM+1),VXA(2,ICOM+1),
     &        FANGL,FAPP(1,1),FAPP(2,1),SAPP(1,1),SAPP(2,1),ZAPP(1,1),
     &        ZAPP(2,1),DZAPP(1)
  482       FORMAT(' YFTVTR: SEP.CIRCLES  IXH,HXC,HYC',
     &        I3,3F9.3,I3,3F9.3,
     &        ' D',1F9.3,' VXA',2F8.3/
     &        10X,' FANGL',F8.3,' FAPP',2F8.3,
     &        ' SAPP',2F8.3,' ZAPP',2F8.3,' DZAPP',F8.3)
          ENDIF
#endif
  485     CONTINUE
C
          ICOM=ICOM+1
          VXA(3,ICOM)=0.5*(ZAPP(1,1)+ZAPP(2,1))
C
  490     CONTINUE
C
C     ACCEPT VERTEX AS STARTING POINT IF IT AGEES WITH A PREVIOUS ONE
C
          DO 491 I=1,3
  491     VXO(I)=VXA(I,ICOM)
C
          IF(NHX.EQ.2) GO TO 498
C
          N=ICOM-1
          IF(N.LE.0) GO TO 495
          DO 493 IV=1,N
            DSQ=0.
            DO 492 I=1,3
              DSQ=DSQ+(SNGL(VXO(I))-VXA(I,IV))**2
  492       CONTINUE
            IF(DSQ.LT.D2VAP) GO TO 498
  493     CONTINUE
  495   CONTINUE
C
  498   CONTINUE
  600   CONTINUE
        IF(ICOM.GE.1) GO TO 610
C
#if defined(YDEBUG)
        IF(LDMP1) THEN
          WRITE(LOUT,605)
  605     FORMAT(5X,' NO APPROXIMATIVE VERTEX FOUND IN YFTVTR')
        ENDIF
#endif
        IFAIL=9
        GOTO 997
  610   CONTINUE
C
      ENDIF
C
C
      ITER=0
      CHISO=1.E+30
      DCHIO=0.
C
 1000 CONTINUE
C
      ITER=ITER+1
C
      IF(ITER.GT.MXITR) GO TO 2000
C
      CHISV=0.
      CHISH=0.
      CHISQ=0.
      CHISN=0.
C
      DO 1100 I=1,3
        G(I)=0.
        DO 1100 J=1,3
          GG(I,J)=0.
 1100 CONTINUE
C
C     LOOP OVER VERTICES
C
      DO 1345 IVX=1,NVX
C
C     DISTANCE VECTOR FROM VERTEX AND HELIX
        DELV(1)=A-VXI(1,IVX)
        DELV(2)=B-VXI(2,IVX)
        DELV(3)=C-VXI(3,IVX)
C
C     CALCULATE CHISQ VERTEX CONTRIB
        CHIV=0.
        DO 1200 I=1,3
          DO 1200 J=1,3
            CHIV=CHIV+SNGL(DELV(I)*EVI(I,J,IVX)*DELV(J))
 1200   CONTINUE
        CHISV=CHISV+CHIV
C
C     DERIVATIVES OF CHISQ WITH RESP. TO FITTED VTX COORD.
C
        DO 1300 I=1,3
          DO 1300 J=1,3
            G(I)=G(I)+SNGL(EVI(I,J,IVX)*DELV(J))
            GG(I,J)=GG(I,J)+EVI(I,J,IVX)
 1300   CONTINUE
C
 1345 CONTINUE
C
C     LOOP OVER HELICES
C
      DO 1500 K=1,NHX
C
C     POINT OF CLOSEST APPROACH TO VERTEX IN X-Y PROJECTION
        HL=SIGN(ONE,HR(K))*SQRT((A-HXC(K))**2+(B-HYC(K))**2)
        HDR=HL-HR(K)
        HX=HXC(K)+(A-HXC(K))*HR(K)/HL
        HY=HYC(K)+(B-HYC(K))*HR(K)/HL
        HSFI=(HX-HXC(K))/HR(K)
        HCFI=-(HY-HYC(K))/HR(K)
        HFI=ATAN2(HSFI,HCFI)
#if defined(YDEBUG)
        IF(LDMP2)
     &    WRITE(LOUT,911) K,HL,HDR,HX,HY,HSFI,HCFI,HFI,HS,HZ,HDZ
#endif
        IF((HFI-HXI(KF,K)).LT.-PI) HFI=HFI+TWO*PI
        HS=(HFI-HXI(KF,K))*HR(K)
        HZ=HXI(KZ,K)+HS*HXI(KT,K)
        HDZ=C-HZ
C
#if defined(YDEBUG)
        IF(LDMP2)
     &    WRITE(LOUT,911) K,HL,HDR,HX,HY,HSFI,HCFI,HFI,HS,HZ,HDZ
  911   FORMAT(/5X,'YFTVTR: K=',I3,'   HL,HDR,HX,HY',4E12.3/
     &    14X,'HSFI,HCFI,HFI,HS,HZ,HDZ',6E12.3)
#endif
C
C
C     DISTANCE VECTOR FROM HELIX
C
        DELTA(1)=HDR
        DELTA(2)=HDZ
C
        IF(LNEMX) THEN
C     CALCULATE NEW TRANSVERSE ERROR MATRIX
C
C     CALCULATE CHISQ WITH OLD ERROR MATRIX
          IF(ITER.LE.1) GO TO 1150
          CHIN=0.
          DO 1130 I=1,2
            DO 1130 J=1,2
              CHIN=CHIN+SNGL(DELTA(I)*EINV(I,J,K)*DELTA(J))
 1130     CONTINUE
C
          CHISN=CHISN+CHIN
 1150     CONTINUE
C     SAVE POSITION OF MATRIX CALCULATION
          DO 1148 I=1,3
            VMTRX(I)=VXO(I)
 1148     CONTINUE
C
#if defined(YDEBUG)
          IF(LDMP2) THEN
            WRITE(LOUT,912) (VXO(I),I=1,3),(DELTA(I),I=1,2)
  912       FORMAT(/5X,'YFTVTR:   VXO',3F10.3,'  DELTA',2E10.3)
          ENDIF
#endif
C
C
C    HELIX TRANSVERSE ERROR MATRIX
          HRHOS=HXI(KR,K)*HS
          COSRS=COS(HRHOS)
          SINRS=SIN(HRHOS)
          ARHO=HR(K)**2*(ONE-COSRS)
          AFI=(-HXI(KD,K)+HR(K))*SINRS
          AD0=-COSRS
          BRHO=HXI(KT,K)*HR(K)**2*(HRHOS-HR(K)/HL*SINRS)
          BFI=HXI(KT,K)*HR(K)*(ONE-(HR(K)-HXI(KD,K))/HL*COSRS)
          BD0=-HXI(KT,K)*HR(K)/HL*SINRS
#if defined(YDEBUG)
          IF(LDMP2)
     &     WRITE(LOUT,913) HRHOS,COSRS,SINRS,ARHO,AFI,AD0,BRHO,BFI,BD0
  913     FORMAT(' YFTVTR: HRHOS,COSRS,SINRS',3E10.3/
     &      ' ARHO,AFI,AD0',3E10.3,' BRHO,BFI,BD0',3E10.3)
#endif
          EHX(1,1)=ARHO**2*VHXI(JRR,K)+AFI**2*VHXI(JFF,K)+
     &         AD0**2*VHXI(JDD,K)+
     &         TWO*(ARHO*(AFI*VHXI(JRF,K)+AD0*VHXI(JRD,K))+
     &         AFI*AD0*VHXI(JFD,K))
          EHX(1,2)=
     &   BRHO*(ARHO*VHXI(JRR,K)+AFI*VHXI(JRF,K)+AD0*VHXI(JRD,K))
     &    -HS*(ARHO*VHXI(JRT,K)+AFI*VHXI(JTF,K)+AD0*VHXI(JTD,K))
     &   +BFI*(ARHO*VHXI(JRF,K)+AFI*VHXI(JFF,K)+AD0*VHXI(JFD,K))
     &   +BD0*(ARHO*VHXI(JRD,K)+AFI*VHXI(JFD,K)+AD0*VHXI(JDD,K))
     &       -(ARHO*VHXI(JRZ,K)+AFI*VHXI(JFZ,K)+AD0*VHXI(JDZ,K))
          EHX(2,1)=EHX(1,2)
          EHX(2,2)=
     &    BRHO**2*VHXI(JRR,K)+HS**2*VHXI(JTT,K)+BFI**2*VHXI(JFF,K)
     &    +BD0**2*VHXI(JDD,K)+VHXI(JZZ,K)+
     &     TWO*(BRHO*(-HS*VHXI(JRT,K)+BFI*VHXI(JRF,K)+BD0*VHXI(JRD,K)
     &          -VHXI(JRZ,K))
     &          -HS*(BFI*VHXI(JTF,K)+BD0*VHXI(JTD,K)-VHXI(JTZ,K))
     &          +BFI*(BD0*VHXI(JFD,K)-VHXI(JFZ,K))
     &          -BD0*VHXI(JDZ,K))
C
#if defined(YDEBUG)
          IF(LDMP2) THEN
            WRITE(LOUT,931) ((EHX(I,J),J=1,2),I=1,2),
     &        SQRT(ABS(EHX(1,1))),SQRT(EHX(2,2))
  931       FORMAT(2(/14X,'  EHX',2E10.3),' DELTAR,DELTAZ',2F10.3)
          ENDIF
#endif
C
          DET=EHX(1,1)*EHX(2,2)-EHX(1,2)**2
          EINV(1,1,K)=EHX(2,2)/DET
          EINV(1,2,K)=-EHX(1,2)/DET
          EINV(2,2,K)=EHX(1,1)/DET
          EINV(2,1,K)=EINV(1,2,K)
C
          IF(DET.LE.0) THEN
            ICNER2=ICNER2+1
#if defined(YDEBUG)
            IF(ICNER2.LE.ICNMAX) THEN
              CALL ABRUEV(IRUN,IEVT)
              WRITE(LOUT,935) K,IXHX(K),IRUN,IEVT,DET,
     &          ((EHX(I,J),J=1,2),I=1,2),
     &          ((EINV(I,J,K),J=1,2),I=1,2)
  935         FORMAT(/('  YFTVTR: TRANSV.HELIX ERROR MATRIX FOR K=',I3,
     &          ' CHARGED HELIX NB ',I3/5X,'RUN',I7,' EVENT',I8,
     &          '  DET=',E10.3/10X,' EHX',4E10.3,'  EINV',4E10.3))
              IF(ICNER2.EQ.ICNMAX) WRITE(LOUT,9355)
 9355         FORMAT(' !!!!!!! YFTVTR : WARNING !!!!!!! :'/,
     &          ' more than 20 errors in inversion transv. track',
     &          ' error matrix  ---  stop printing messages ')
            ENDIF
#endif
            IFAIL=22
            GOTO 997
          ENDIF
C
        ENDIF
C
C     CALCULATE CHISQ
        CHIT=0.
        DO 1400 I=1,2
          DO 1400 J=1,2
            CHIT=CHIT+SNGL(DELTA(I)*EINV(I,J,K)*DELTA(J))
 1400   CONTINUE
C
        CHISH=CHISH+CHIT
C
C     DERIVATIVES OF DELTA WITH RESPECT TO FITTED PARAMETERS A,B,C
        T(1,1)= HSFI
        T(1,2)=-HCFI
        T(1,3)=0.
        T(2,1)=-HXI(KT,K)*HCFI*HR(K)/HL
        T(2,2)=-HXI(KT,K)*HSFI*HR(K)/HL
        T(2,3)=1.
C
#if defined(YDEBUG)
        IF(LDMP2)
     &    WRITE(LOUT,921) ((T(I,J),J=1,3),I=1,2)
  921   FORMAT(/
     &       (14X,'  T',3E10.3))
#endif
C
C
C     MATRIX PRODUCT T(TRANS) * E**-1
        DO 1350 I=1,3
          DO 1340 L=1,2
            TEINV(I,L)=0.
            DO 1330 J=1,2
              TEINV(I,L)=TEINV(I,L)+T(J,I)*EINV(J,L,K)
 1330       CONTINUE
 1340     CONTINUE
 1350   CONTINUE
#if defined(YDEBUG)
        IF(LDMP2)
     &    WRITE(LOUT,941) ((TEINV(I,J),J=1,2),I=1,3)
  941   FORMAT(/(14X,'  TEINV',2E10.3))
#endif
C     FIRST DERIVATIVE OF CHISQ * 0.5 : SUM OF T(TRANS)*E**-1*DELTA
        DO 1450 I=1,3
          DO 1430 J=1,2
            G(I)=G(I)+TEINV(I,J)*DELTA(J)
 1430     CONTINUE
 1450   CONTINUE
#if defined(YDEBUG)
        IF(LDMP2)
     &    WRITE(LOUT,942) (G(I),I=1,3)
  942   FORMAT(/5X,'YFTVTR:  G',3E10.3)
#endif
C     SECOND DERIVATIVE OF CHISQ * 0.5 : SUM OF T(TRANS) * E**-1 * T
        DO 1550 I=1,3
          DO 1540 L=1,3
            DO 1530 J=1,2
              GG(I,L)=GG(I,L)+TEINV(I,J)*T(J,L)
 1530       CONTINUE
 1540     CONTINUE
 1550   CONTINUE
C
C
C
 1500 CONTINUE
C
C     LOOP OVER NEUTRALS
C
      DO 9500 KK=1,NEU
        K=KK+NHX
C
C     POINT OF CLOSEST APPROACH TO VERTEX IN X-Y PROJECTION
        HS=A*HCF0(K)+B*HSF0(K)
        HX=HX0(K)+HS*HCF0(K)
        HY=HY0(K)+HS*HSF0(K)
        HDR=A*HSF0(K)-B*HCF0(K)-HXI(KD,K)
        HZ=HXI(KZ,K)+HS*HXI(KT,K)
        HDZ=C-HZ
        BFI=HXI(KT,K)*(A*HSF0(K)-B*HCF0(K))
C
#if defined(YDEBUG)
        IF(LDMP2)
     &    WRITE(LOUT,8911) K,HDR,HX,HY,HS,HZ,HDZ
 8911   FORMAT(/5X,'YFTVTR: K=',I3,'   HDR,HX,HY',3E12.3/
     &    14X,'HS,HZ,HDZ',3E12.3)
#endif
C
C     DISTANCE VECTOR FROM TRACK
C
        DELTA(1)=HDR
        DELTA(2)=HDZ
C
        IF(LNEMX) THEN
C     CALCULATE NEW TRANSVERSE ERROR MATRIX
C
C     CALCULATE CHISQ WITH OLD ERROR MATRIX
          IF(ITER.LE.1) GO TO 9150
          CHIN=0.
          DO 9130 I=1,2
            DO 9130 J=1,2
              CHIN=CHIN+SNGL(DELTA(I)*EINV(I,J,K)*DELTA(J))
 9130     CONTINUE
C
          CHISN=CHISN+CHIN
 9150     CONTINUE
C     SAVE POSITION OF MATRIX CALCULATION
          DO 9148 I=1,3
            VMTRX(I)=VXO(I)
 9148     CONTINUE
C
#if defined(YDEBUG)
          IF(LDMP2) THEN
            WRITE(LOUT,8912) (VXO(I),I=1,3),(DELTA(I),I=1,2)
 8912       FORMAT(/5X,'YFTVTR:   VXO',3F10.3,'  DELTA',2E10.3)
          ENDIF
#endif
C
C     TRACK TRANSVERSE ERROR MATRIX
          EHX(1,1)=HS**2*VHXI(JFF,K)-TWO*HS*VHXI(JFD,K)+VHXI(JDD,K)
          EHX(1,2)=-HS*(HS*VHXI(JTF,K)-VHXI(JTD,K))
     &            +BFI*(HS*VHXI(JFF,K)-VHXI(JFD,K))
     &                -(HS*VHXI(JFZ,K)-VHXI(JDZ,K))
          EHX(2,1)=EHX(1,2)
          EHX(2,2)=HS**2*VHXI(JTT,K)+BFI**2*VHXI(JFF,K)+VHXI(JZZ,K)
     &       +TWO*(-HS*(BFI*VHXI(JTF,K)-VHXI(JTZ,K))
     &            -BFI*VHXI(JFZ,K))
C
#if defined(YDEBUG)
          IF(LDMP2) THEN
            WRITE(LOUT,8931) ((EHX(I,J),J=1,2),I=1,2),
     &        SQRT(ABS(EHX(1,1))),SQRT(EHX(2,2))
 8931       FORMAT(2(/14X,'  EHX',2E10.3),' DELTAR,DELTAZ',2F10.3)
          ENDIF
#endif
C
          DET=EHX(1,1)*EHX(2,2)-EHX(1,2)**2
          EINV(1,1,K)=EHX(2,2)/DET
          EINV(1,2,K)=-EHX(1,2)/DET
          EINV(2,2,K)=EHX(1,1)/DET
          EINV(2,1,K)=EINV(1,2,K)
C
          IF(DET.LE.0) THEN
            IFAIL=23
            ICNER2=ICNER2+1
#if defined(YDEBUG)
            IF(ICNER2.LE.ICNMAX) THEN
              CALL ABRUEV(IRUN,IEVT)
              WRITE(LOUT,8935) K,IXNU(KK),IRUN,IEVT,DET,
     &          ((EHX(I,J),J=1,2),I=1,2),
     &          ((EINV(I,J,K),J=1,2),I=1,2)
 8935         FORMAT(/('  YFTVTR: TRANSV.HELIX ERROR MATRIX FOR K=',I3,
     &          ' FOR NEUTRAL TRACK NB ',I3/5X,'RUN',I7,' EVENT',I8,
     &          '  DET=',E10.3/10X,' EHX',4E10.3,'  EINV',4E10.3))
              IF(ICNER2.EQ.ICNMAX) WRITE(LOUT,9355)
            ENDIF
#endif
            GOTO 997
          ENDIF
C
        ENDIF
C
C     CALCULATE CHISQ
        CHIT=0.
        DO 9400 I=1,2
          DO 9400 J=1,2
            CHIT=CHIT+SNGL(DELTA(I)*EINV(I,J,K)*DELTA(J))
 9400   CONTINUE
C
        CHISH=CHISH+CHIT
C
C     DERIVATIVES OF DELTA WITH RESPECT TO FITTED PARAMETERS A,B,C
        T(1,1)= HSF0(K)
        T(1,2)=-HCF0(K)
        T(1,3)=0.
        T(2,1)=-HXI(KT,K)*HCF0(K)
        T(2,2)=-HXI(KT,K)*HSF0(K)
        T(2,3)=1.
C
#if defined(YDEBUG)
        IF(LDMP2)
     &    WRITE(LOUT,8921) ((T(I,J),J=1,3),I=1,2)
 8921   FORMAT(/
     &       (14X,'  T',3E10.3))
#endif
C
C     MATRIX PRODUCT T(TRANS) * E**-1
        DO 9350 I=1,3
          DO 9340 L=1,2
            TEINV(I,L)=0.
            DO 9330 J=1,2
              TEINV(I,L)=TEINV(I,L)+T(J,I)*EINV(J,L,K)
 9330       CONTINUE
 9340     CONTINUE
 9350   CONTINUE
C
C
#if defined(YDEBUG)
        IF(LDMP2)
     &    WRITE(LOUT,8941) ((TEINV(I,J),J=1,2),I=1,3)
 8941   FORMAT(/(14X,'  TEINV',2E10.3))
#endif
C
C     FIRST DERIVATIVE OF CHISQ * 0.5 : SUM OF T(TRANS)*E**-1*DELTA
        DO 9450 I=1,3
          DO 9430 J=1,2
            G(I)=G(I)+TEINV(I,J)*DELTA(J)
 9430     CONTINUE
 9450   CONTINUE
C
C
#if defined(YDEBUG)
        IF(LDMP2)
     &    WRITE(LOUT,8942) (G(I),I=1,3)
 8942   FORMAT(/5X,'YFTVTR:  G',3E10.3)
#endif
C
C     SECOND DERIVATIVE OF CHISQ * 0.5 : SUM OF T(TRANS) * E**-1 * T
        DO 9550 I=1,3
          DO 9540 L=1,3
            DO 9530 J=1,2
              GG(I,L)=GG(I,L)+TEINV(I,J)*T(J,L)
 9530       CONTINUE
 9540     CONTINUE
 9550   CONTINUE
C
C
 9500 CONTINUE
C
C
      CHISQ=CHISV+CHISH
      IF(LNEMX)THEN
        CHIQN=CHISV+CHISN
        DCHIQ=CHISO-CHIQN
      ELSE
        DCHIQ=CHISO-CHISQ
      ENDIF
      IF(IFAIL.NE.0) ICNER3=ICNER3+1
#if defined(YDEBUG)
      IF(LDMP2.OR.(IFAIL.NE.0.AND.ICNER3.LE.ICNMAX)) THEN
        WRITE(LOUT,916) NVX,NHX,NEU,(IXHX(I),I=1,2),
     &    ITER,CHISO,CHIQN,CHISQ,DCHIQ,CHISC
  916   FORMAT(1X,'YFTVTR: NVX,NHX,NEU',3I3,' IXHX',2I3,' ITER=',I2/
     &    10X,'  CHISO,CHIQN,CHISQ',3E10.3,
     &    ' DCHIQ,CHISC',2E12.5)
        IF(ICNER3.EQ.ICNMAX) WRITE(LOUT,9166)
 9166   FORMAT(' !!!!!!! YFTVTR : WARNING !!!!!!! :'/,
     &    ' more than 20 errors : no convergence in vertex fit ',
     &    ' stop printing messages ')
      ENDIF
C
      IF(LDMP2)   THEN
        WRITE(LOUT,943) ((GG(I,J),J=1,3),I=1,3)
  943   FORMAT(/(14X,'   GG',3E10.3))
      ENDIF
#endif
C
C
C     INVERT   GG**-1
      CALL YMS3IN(GG,GG,IFLLL)
C
C
      IF(IFLLL.NE.0) THEN
C  PROBLEM IN INVERSION OF GG MATRIX
        IFAIL=30
        ICNER1=ICNER1+1
#if defined(YDEBUG)
        IF(LDMP2.OR.(ICNER1.LE.ICNMAX)) THEN
          WRITE(LOUT,944) NCFHX,IFLLL,KASE,
     &      DCIRC,R1A,R2A,D,ALEN,SANGL,CANGL,
     &      HXC1,HYC1,HXC2,HYC2,VXA(1,ICOM),VXA(2,ICOM),
     &      HR1,HR2,FANGL,FAPP(1,1),FAPP(2,1),
     &      HXI(KF,JA(1)),HXI(KF,JA(2)),HXI(KT,JA(1)),HXI(KT,JA(2)),
     &      SAPP(1,1),SAPP(2,1),ZAPP(1,1),ZAPP(2,1),
     &      RGG,NHX,(IXHX(I),I=1,NHX)
  944     FORMAT(5X,'YFTVTR: NCFHX=',I5,' YMS3IN CALLED  IFLLL=',I5,
     &      '  KASE=',I3/
     &      '  DCIRC,R1A,R2A,D=',4F8.2,' ALEN,SANGL,CANGL',F8.2,2F8.3 /
     &      '  HXC,HYC',2F8.2,2X,2F8.2,'  VXA',2F8.2/
     &      '  HR',2F8.2,'  FANGL',F8.3,'  FAPP',2F8.3,' FI0',2F8.3/
     &      '  T',2F8.3,'  SAPP',2F8.3,' ZAPP',2F8.3/
     &     3(14X,'  GG INVERTED',3E10.3/),14X,'  NHX=',I3,'  IHX:',20I3)
C
          IF(ICNER1.EQ.ICNMAX) WRITE(LOUT,9033)
        ENDIF
#endif
        CHISQ=1.E+30
      ENDIF
C
C
      DCHI2=ABS(DCHIQ)+DCHIO
      IF(DCHI2 .LT.CHISC.OR.DCHI2 .LT.CHISR*CHISQ) GO TO 2000
C
      DCHIO=ABS(DCHIQ)
      CHISO=CHISQ
C
C  CALCULATE VERTEX CHANGE
C
C     INDEP. PAR. CHANGE
      DO 1590 I=1,3
        DELVX(I)=0.
        DO 1570 J=1,3
          DELVX(I)=DELVX(I)+GG(I,J)*G(J)
 1570   CONTINUE
 1590 CONTINUE
C
C
      D2VX=0.
      DO 1600 I=1,3
        VXO(I)=VXO(I)-DELVX(I)
        D2VX=D2VX+(SNGL(VXO(I)-VMTRX(I)))**2
 1600 CONTINUE
      LNEMX=.FALSE.
      IF(D2VX.GT.DV2MX) THEN
        LNEMX=.TRUE.
        CHISN=0.
      ENDIF
C
#if defined(YDEBUG)
      IF(LDMP1)
     &  WRITE(LOUT,951) ITER,DELVX,VXO
  951 FORMAT(/'  ITER=',I5,'  DELVX',3E10.3,'   VXO',3F12.6/)
#endif
C
C
      GO TO 1000
C
C
 2000 CONTINUE
C     OUTPUT VERTEX AND ERRORS
      IDX=0
      DO 2200 I=1,3
        VXOUT(I)=VXO(I)
        DO 2100 J=1,I
          IDX=IDX+1
          VVXOU(IDX)=GG(I,J)
 2100   CONTINUE
 2200 CONTINUE
C
      ITER=ITER-1
#if defined(YDEBUG)
      IF(LDMP1)
     &  WRITE(LOUT,957) ITER,(VXOUT(I),I=1,3),(VVXOU(I),I=1,6)
  957 FORMAT(/'  ITER=',I5,'  VXOUT',3E10.3/10X,'  VVXOU',6E10.3/)
#endif
C
      RETURN
C - error
  997 CHISQ = 1.E30
      END
#endif
