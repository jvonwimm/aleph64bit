      SUBROUTINE YFMVTC(LMASSC,USE,CONMAS,
     &      NVX,NHX,NEU,LVAPC,LPSUM,LMCALC,LVFIX,
     &      VXIN,VVXIN,IXHX,NSHX,NSVHX,HXIN,VHXIN,
     &      IXNU,NSNU,NSVNU,TNUI,VTNUI,
     &      NPIDC,AMPC,
     &      VXOUT,VVXOU,HXOU,VHXOU,TNUO,VTNUO,
     &      PSUM,VPSUM,VPSVX,
     &      AMASS,DMASS,VMVX,VMPS,
     &      CHISQ,IFAIL)
C
C----------------------------------------------------------*
C!    Constrained fit of vertices,charged and neutral tracks
CKEY YTOP FIT
C!    Author :     G. Lutz     /04/89
C!    Modified  :  G. Lutz   30/03/92
C!    Adapted from yfmvtr  :  G. Taylor   15/12/93
C!
C!    Description
C!    ===========
C!    This routine provides a constraint fit of NVX vertices,
C!    NHX helices and NEU neutral tracks to a new common
C!    vertex using a lagrange multiplier mass constraint
C!
C!
C! INPUT
C!    LMASSC........ LOGICAL: APPLY A MASS CONSTRAINT IF TRUE
C!    USE(NHX+NEU).. LOGICAL: USE THIS TRACK IN MASS CONSTRAINT
C!    CONMAS........ MASS TO CONSTRAIN TRACKS TO
C!    NVX .......... # OF VERTICES TO BE USED IN FIT
C!    NHX .......... # OF HELICES TO BE USED IN FIT
C!    NEU .......... # OF NEUTRAL TRACKS TO BE USED IN FIT
C!    LVAPC ....... LOGICAL: CALCULATE STARTING VALUES OF VERTEX
C!    LPSUM ....... LOGICAL: CALCULATE MOMENTUM SUM AND ERRORS
C!    LMCALC....... LOGICAL: CALCULATE INVARIANT MASS AND ERRORS
C!    LVFIX ....... LOGICAL: FIX VERTEX TO INPUT VALUE in first
C!                           few iterations
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
C!    IXNU(K) ...... INDEX OF NEUTRAL K IN BUFFER TNUIN
C!    NSNU ......... SPACING BETWEEN VECTORS IN BUFFER TNUI
C!    NSVNU ........ SPACING BETWEEN ERROR MATRICES IN BUFFER VTNUI
C!    TNUI .......... NEUTRAL TRACK PARAMETERS ORDERED IN SEQUENCE
C!                   P; T; PHI0; D0(SIGNED); Z0
C!                   D0.GT.0 IF MOMENTUM AROUND ORIGIN IS POSITIVE
C!                   FIRST ELEMENT STARTS AT I=(IXHX-1)*NSHX+1
C!    VTNUI(I) ...... CORRESPONDING VARIANCES ORDERED AS
C!                   RHO;
C!                   RHO.T; T
C!                   RHO.PHI0;   T.PHI0    PHI0
C!                   RHO.D0;    T.D0      PHIZERO     D0
C!                   RHO.Z0     T.Z0     PHI0.Z0    D0.Z0    Z0
C!                   FIRST ELEMENT IS AT I=(IXHX-1)*NSVHX+1
C!    NPIDC ........ # OF PARTICLE ASSIGNEMENTS
C!    AMPC(I) ...... MASS ASSGNEMENT TO TRACKS.
C!                   NPIDC WORDS PER COMBIATION
C!
C! OUTPUT
C!    VXOUT .........VERTEX X,Y,Z
C!    VVXOU ........CORRESPONDING VARIANCES X, XY, Y, XZ, YZ, Z
C!    HXOU(I,K) .... HELIX PARAMETERS ORDERED IN SEQUENCE
C!                   RHO=1/R(SIGNED); T; PHI0; D0(SIGNED); Z0
C!                   RHO>0 FOR COUNTERCLOCKWISE BENDING
C!                   D0.GT.0 IF MOMENTUM AROUND ORIGIN IS POSITIVE
C!    VHXOU(I,K) ... CORRESPONDING VARIANCES ORDERED AS
C!                   RHO;
C!                   RHO.T; T
C!                   RHO.PHI0;   T.PHI0    PHI0
C!                   RHO.D0;    T.D0      PHIZERO     D0
C!                   RHO.Z0     T.Z0     PHI0.Z0    D0.Z0    Z0
C!    TNUO(I,K) ..... NEUTRAL TRACK PARAMETERS ORDERED IN SEQUENCE
C!                   P; T; PHI0; D0(SIGNED); Z0
C!                   D0.GT.0 IF MOMENTUM AROUND ORIGIN IS POSITIVE
C!    VTNUO(I,K) .... CORRESPONDING VARIANCES ORDERED AS
C!                   RHO;
C!                   RHO.T; T
C!                   RHO.PHI0;   T.PHI0    PHI0
C!                   RHO.D0;    T.D0      PHIZERO     D0
C!                   RHO.Z0     T.Z0     PHI0.Z0    D0.Z0    Z0
C!    PSUM(I) ......  MOMENTUM SUM VECTOR AND MASS PX,PY,PZ
C!    VPSUM ........  CORRESPONDING VARIANCES ORDERED AS
C!                   PX;
C!                   PX.PY; PY
C!                   PX.PZ; PY.PZ; PZ
C!    VPSVX ........ CORRELATION BETWEEN MOMENTUM SUM AND VERTEX
C!                   VX.PX; VY.PX; VZ.PX;
C!                   VX.PY; VY.PY; VZ.PY;
C!                   VX.PZ; VY.PZ; VZ.PZ;
C!    AMASS(IPA) ... MASS FOR PARTICLE ASSIGNEMENT IPA
C!    DMASS(IPA) ... MASS ERROR FOR PARTICLE ASSIGNEMENT IPA
C!    VMVX(IPA) .... CORRELATION BETWEEN MASS AND VERTEX
C!                   VX.M ; VY.M ; VZ.MZ
C!    VMPS(IPA) .... CORRELATION BETWEEN MASS AND MOMENTUM SUM
C!                   PX.M;  PY.M;  PZ.M
C!    CHISQ ........ VERTEX CHISQ
C!    IFAIL ........ =1,2,3, PAIR OF TRACKS MISSING BY LARGE AMOUNT IN
C!                    VTX STARTING VALUE SEARCH
C!                   =9,  NO APPROXIMATIVE VERTEX FOUND
C!                   =10, # OF INPUT VERTICES ABOVE ALLOWED MAXIMUM
C!                   =11, # OF TRACKS ABOVE ALLOWED MAXIMUM
C!                   =21, ERROR IN INPUT VTX ERROR MATRIX
C!                   =22, ERROR IN INPUT helix ERROR MATRIX
C!                   =23, ERROR IN INPUT neutral tr. ERROR MATRIX
C!                   =30, ERROR IN GG MATRIX INVERSION
C!                   =99, chisq above max. allowed value
C!
C!
C!---------------------------------------------------------*
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C! YTOP array dimensions
      PARAMETER(NMSIZZ=31,MKDIMM=6,MAXTRK=186,MAXHLX=124,MAXNTR=62,
     &  MXVTOP=10,MXMULS=10,MAXVXP=50,MAXEXC=60,MXVTYP=2)
C! YTOP parameters
      COMMON/YPARTO/DHXLIM,CHISEL,PMINSE,PIDACP,BFIELD,
     &       MNTHPV,MXTSPV, PMINRQ,PMINRA,
     &       CHVXCO,CHPTCO,RVACCO,AMCTCO,DZMXCO,NAMXCO,EPLOCO,EPHICO,
     &       CHVXV0,CHPTV0,CHVSV0,CHMLV0,DZMXV0,NAMXV0,
     &       PIPKV0,PRPLV0,PIPLV0,
     &       LBCRFD,LRYOLD,LRFRF2,
     &       LRPVTX,LRSVTX,LVBCR0,LRMVPV,LRLPVX,LCONVS,LVZERS,LRUSER
      LOGICAL LBCRFD,LRYOLD,LRFRF2
      LOGICAL LRPVTX,LRSVTX,LVBCR0,LRMVPV,LRLPVX,LCONVS,LVZERS,LRUSER
C
C     MAX. # OF TRACKS ; OF VERTICES
      PARAMETER(NTMAX=10,NVMAX=7)
C     MAX. # OF FIT PARAMETERS  NPMAX=3+3*NTMAX
      PARAMETER(NPMAX=3+3*NTMAX)
C
      DIMENSION VXIN(3,*),VVXIN(6,*)
      DIMENSION IXHX(*),HXIN(*),VHXIN(*)
      DIMENSION IXNU(*),TNUI(*),VTNUI(*)
      DIMENSION AMPC(*)
      DIMENSION VXOUT(*),VVXOU(*),
     &      HXOU(5,*),VHXOU(15,*),TNUO(5,*),VTNUO(15,*)
     &      ,PSUM(*),VPSUM(*),VPSVX(*)
     &      ,AMASS(*),DMASS(*)
     &      ,VMVX(3,*),VMPS(3,*)
      DOUBLE PRECISION EH(5,5,NTMAX),EHI(5,5,NTMAX)
C     FITTED HELIX AND NEUTRAL TR. PARAMETERS
      DOUBLE PRECISION HHXI(5,NTMAX)
C
      DOUBLE PRECISION G(NPMAX),GG(NPMAX,NPMAX)
C gary 15/12/93
      DOUBLE PRECISION GGC(NPMAX,NPMAX)
      DOUBLE PRECISION PC(NPMAX,6),CMASS,DCDI(NPMAX),DCDP(NPMAX,3)
      DOUBLE PRECISION DEL,DELC,CON
      LOGICAL LMASSC
      DOUBLE PRECISION DPDI(NTMAX,3,NPMAX)
      LOGICAL USE(*)
C gary 15/12/93
C     independent parameters, parameter change
C     order: vertex(3),
C            helix(3) [nhx times], $eta=r-d0,t,fi0
C            neutrals(2)  [neu times]
      DOUBLE PRECISION PARO(NPMAX),DPAR(NPMAX)
C
C     recalculation of fitted helix parameters from idep. par.
      DOUBLE PRECISION ETA,FR,FI0,SFI0,CFI0,FI,SFI,CFI,
     &                 FS,FF0,SFF0,CFF0
      DOUBLE PRECISION D0,FT,Z0,XC,YC,V1XC,V2YC
      DOUBLE PRECISION AA,PCONV,ROFP,PXV2,PXP1,PXP2,PXP3,
     &      PYV1,PYP1,PYP2,PYP3,PZV1,PZV2,PZP1,PZP2,PZP3,
     &      FP,CMASQ,CMAS
C     FOR MOMENTUM CALCULATION
      DOUBLE PRECISION FRSIG,FPT,FPX,FPY,FPZ,FE
      DOUBLE PRECISION FPI(4,NTMAX),DPAQ(3,6,NTMAX),
     &  CEP,FDB(NPMAX),SDMQ,
     &  SDMX,SDMY,SDMZ,SDMPX,SDMPY,SDMPZ
C
C     MOMENTUM SUM
      DOUBLE PRECISION FES,FPS(3),FVPS(3,3),FVVPS(3,3)
C     DERIVATIVES FOR MOMENTUM SUM (DIMENSION 3+3*NTMAX)
      DOUBLE PRECISION DTX(NPMAX),DTY(NPMAX),DTZ(NPMAX)
C
      DOUBLE PRECISION DI,DJ,DDI(5)
C
      DOUBLE PRECISION H1V1,H1V2,H1P1,H1P3,H4V1,H4V2,H4P1,H4P3,
     &   H5V1,H5V2,H5P1,H5P2,H5P3
      DOUBLE PRECISION DHIP(5,6)
C
      DOUBLE PRECISION
     &  H1XX,H1XY,H1XQ1,H1XQ3,H1YY,H1YQ1,H1YQ3,H1Q11,H1Q13,H1Q33,
     &  H4XX,H4XY,H4XQ1,H4XQ3,H4YY,H4YQ1,H4YQ3,H4Q11,H4Q13,H4Q33,
     &  H5XX,H5XY,H5XQ1,H5XQ2,H5XQ3,H5YY,H5YQ1,H5YQ2,H5YQ3,H5Q11,
     &  H5Q12,H5Q13,H5Q23,H5Q33
C
      DOUBLE PRECISION CHIV,CHIH,CHIN
C******************************************
C  approximative vertex calculation
      DOUBLE PRECISION D,R1A,R2A,DX,X0,Y0,ALEN,FANGL,SANGL,CANGL
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
      DOUBLE PRECISION VXI(3,NVMAX),VVXI(6,NVMAX)
      DOUBLE PRECISION HXI(5,NTMAX), VHXI(15,NTMAX),VXO(3)
      DOUBLE PRECISION HSF0(NTMAX),HCF0(NTMAX),HR(NTMAX),
     &      HX0(NTMAX),HY0(NTMAX),
     &      HXC(NTMAX),HYC(NTMAX)
      DOUBLE PRECISION A,B,C
      EQUIVALENCE (A,VXO(1)),(B,VXO(2)),(C,VXO(3))
      DOUBLE PRECISION SIGRO
C     deviation vector from vertex
      DOUBLE PRECISION DELV(3)
C     error matrix vertex, inverse
      DOUBLE PRECISION EV(3,3),EVI(3,3,NVMAX)
      DOUBLE PRECISION PMASS
      DOUBLE PRECISION DDET
      DOUBLE PRECISION PI
      DOUBLE PRECISION ZERO,HALF,ONE,TWO,THREE
C
      LOGICAL LVAPC,LPSUM,LMCALC,LVFIX,LVF
C
C set counters to avoid very large nb of error warnings
      DATA ICNER1/0/,ICNER2/0/,ICNER3/0/,ICNMAX/ 0/
C     copy sequence of helix parameter and errors
      DATA KHX/1,2,3,4,5/,JHX/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15/
C
      DATA MXITR/ 8/,CHISC/1.E-2/,CHISR/.01/,PARCR/0.01/
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
C
C     recalc. transv.err. matrix for vtx shift above sqrt(dv2mx)
      DATA DV2MX/0.25/
C     criteria for agreement between approx. vtx. sol.
      DATA D2VAP/5./
C     internal chisq cut , do not proceed for combinations
C     thatt give very high chisq
      DATA CHIMAX/5000./
C     initialize double precision constants
      DATA PI/3.141592654D+00/
      DATA ZERO/0.D+00/,HALF/0.5D+00/
      DATA ONE/1.D+00/,TWO/2.D+00/,THREE/3.D+00/
      DATA NENTY/0/
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
C
C     RESET OUTPUT VALUES
      DO I=1,3
        VXOUT(I) = 0.
        PSUM(I)  = 0.
      ENDDO
      DO I=1,6
        VVXOU(I) = 0.
        VPSUM(I) = 0.
      ENDDO
      IMAX=MAX(1,NPIDC)
      DO I=1,IMAX
        AMASS(I) = 0.
        DMASS(I) = 0.
      ENDDO
      CHISQ = 1.E+10
C
      IFAIL=0
C
      IF(NVX.GT.NVMAX) THEN
        WRITE(LOUT,280) NVX,NVMAX
  280   FORMAT(' ******** YFMVTC:  NVX=',I5,
     &     ' ABOVE ALLOWED MAXIMUM',I5)
        IFAIL=10
        GOTO 997
      ENDIF
C
      IF((NHX+NEU).GT.NTMAX) THEN
        WRITE(LOUT,281) NHX,NEU,NTMAX
  281   FORMAT('  YFMVTC:  NHX+NEU=',I3,'+',I3,
     &     ' ABOVE ALLOWED MAXIMUM',I5)
        IFAIL=11
        GOTO 997
      ENDIF
C
C
C     COPY INPUT VECTORS
C     input vertices
      DO 7300 IVX=1,NVX
        DO 7100 I=1,3
          VXI(I,IVX)=VXIN(I,IVX)
 7100   CONTINUE
        DO 7200 I=1,6
          VVXI(I,IVX)=VVXIN(I,IVX)
 7200   CONTINUE
C
C     vertex error matrix
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
C
C
        CALL YMS3IN(EV(1,1),EVI(1,1,IVX),IFLLL)
C
        IF(IFLLL.NE.0) THEN
C   PROBLEM IN INPUT VERTEX ERROR MATRIX
          IFAIL=21
          ICNER1=ICNER1+1
          GOTO 997
        ENDIF
C
 7300 CONTINUE
C
C     input helices
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
        IJ=0
        DO 406 I=1,5
          DO 406 J=1,I
            IJ=IJ+1
            EH(I,J,K)=VHXI(IJ,K)
            EH(J,I,K)=EH(I,J,K)
  406   CONTINUE
C
C
C     INVERT 5X5 MATRIX EH
        DO I=1,5
          DO J=1,5
            EHI(I,J,K)=EH(I,J,K)
          ENDDO
        ENDDO
        CALL DSINV(5,EHI(1,1,K),5,IFLLL)
C
        IF(IFLLL.NE.0) THEN
C   problem with charged track error matrix
          IFAIL=22
          ICNER2=ICNER2+1
          GOTO 997
        ENDIF
C
C
C     more helix parameters
        HSF0(K)=SIN(HXI(KF,K))
        HCF0(K)=COS(HXI(KF,K))
C     radius
        HR(K)=ONE/HXI(KR,K)
C     closest point to origin
        HX0(K)= HXI(KD,K)*HSF0(K)
        HY0(K)=-HXI(KD,K)*HCF0(K)
C     centre of circle
        HXC(K)=HX0(K)-HR(K)*HSF0(K)
        HYC(K)=HY0(K)+HR(K)*HCF0(K)
C
C
C
  500 CONTINUE
C
      IF(NEU.LE.0) GO TO 8510
      DO 8500 KK=1,NEU
        K=KK+NHX
        IX=(IXNU(KK)-1)*NSNU
        DO 8300 I=1,5
          HXI(I,K)=TNUI(IX+KHX(I))
 8300   CONTINUE
C
        IX=(IXNU(KK)-1)*NSVNU
        DO 8400 I=1,15
          VHXI(I,K)=VTNUI(IX+JHX(I))
 8400   CONTINUE
C
        IJ=0
        DO 506 I=1,5
          DO 506 J=1,I
            IJ=IJ+1
            EH(I,J,K)=VHXI(IJ,K)
            EH(J,I,K)=EH(I,J,K)
  506   CONTINUE
C
C
C     INVERT 5X5 MATRIX EH
        DO I=1,5
          DO J=1,5
            EHI(I,J,K)=EH(I,J,K)
          ENDDO
        ENDDO
        CALL DSINV(5,EHI(1,1,K),5,IFLLL)
C
        IF(IFLLL.NE.0) THEN
C problem with neutral track error matrix
          IFAIL=23
          ICNER2=ICNER2+1
          GOTO 997
        ENDIF
C
C
C
 8500 CONTINUE
 8510 CONTINUE
C
C===================== SEARCH FOR START VERTEX ==================
C
C     starting value for iterations: first input vertex
      A=VXIN(1,1)
      B=VXIN(2,1)
      C=VXIN(3,1)
C
      IF(LVAPC.AND..NOT.LVFIX) THEN
C
C     find starting vertex from crossing pairs of tracks
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
C
          IF(D.GT.(R1A+R2A))   GO TO 430
          IF(D.LT.ABS(R1A-R2A)) GO TO 420
  410     CONTINUE
C     circles crossing
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
          DO 415 ISOL=1,2
            DO 414 IR=1,2
              CFAPP(IR,ISOL)=-(VAPP(2,ISOL)-HYCR(IR))/
     &          HRR(IR)
              SFAPP(IR,ISOL)= (VAPP(1,ISOL)-HXCR(IR))/
     &          HRR(IR)
              FAPP(IR,ISOL)=ATAN2(SFAPP(IR,ISOL),CFAPP(IR,ISOL))
              IF((FAPP(IR,ISOL)-HXI(KF,JA(IR))).LT.-PI)
     &          FAPP(IR,ISOL)=FAPP(IR,ISOL)+TWO*PI
              SAPP(IR,ISOL)= (FAPP(IR,ISOL)-HXI(KF,JA(IR)))*HRR(IR)
              ZAPP(IR,ISOL)= SNGL(HXI(KZ,JA(IR)))+
     &          SAPP(IR,ISOL)*SNGL(HXI(KT,JA(IR)))
  414       CONTINUE
            DZAPP(ISOL)=ZAPP(2,ISOL)-ZAPP(1,ISOL)
  415     CONTINUE
C
          ISOL=1
          IF(ABS(DZAPP(2)).LT.ABS(DZAPP(1))) ISOL=2
          IF(ABS(DZAPP(ISOL)).GT.DHXLIM) THEN
C     special treatment for track projections crossing under
C     shallow angle: interpolate solutions so as to get
C     crossings in z
C
C     find point where z-distance = 0
            SIGR1=SIGN(1. ,HR1)
            SIGR2=SIGN(1. ,HR2)
            SIG=SIGR1*SIGR2
            IF(D.GT.MAX(R1A,R2A)) SIG=-SIG
            DDZDS=HXI(KT,J1)-HXI(KT,J2)*DBLE(SIG)
            IF(ABS(DDZDS).LT.1.E-10) THEN
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
            DSB=DHXLIM/DDRDS
C     select closest of two points
            DS1=DSA
            IF(DSB.LT.ABS(DSA)) THEN
              DS1=SIGN(DSB,DSA)
            ENDIF
            DS2=DS1*SIG
            ZAPP(1,ISOL)=ZAPP(1,ISOL)+DS1*SNGL(HXI(KT,J1))
            ZAPP(2,ISOL)=ZAPP(2,ISOL)+DS2*SNGL(HXI(KT,J2))
            DZAPP(ISOL)=ZAPP(2,ISOL)-ZAPP(1,ISOL)
C
C     check if helices miss by large distance
            IF (ABS(DZAPP(ISOL)).GT.DHXLIM) THEN
              IFAIL=1
              GOTO 997
            ENDIF
C
C     move starting point for vertex by ds1
            VXA(1,ICOM+1)=VAPP(1,ISOL)+
     &      .5*(DS1*COS(SNGL(FAPP(1,ISOL)))+DS2*COS(SNGL(FAPP(2,ISOL))))
            VXA(2,ICOM+1)=VAPP(2,ISOL)+
     &      .5*(DS1*SIN(SNGL(FAPP(1,ISOL)))+DS2*SIN(SNGL(FAPP(2,ISOL))))
C
C
C     check the distances between the helices in space
C     not yet coded
C           IF(DSQ.GT.DHXLIM**2) GO TO 495
C     approx vtx found
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
C     enclosed circles
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
C     check if helices miss by large distance
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
C     find point where z-distance = 0
          SIGR1=SIGN(1. ,HR1)
          SIGR2=SIGN(1. ,HR2)
          DDZDS=HXI(KT,J1)-HXI(KT,J2)*DBLE(SIGR1*SIGR2)
          IF(ABS(DDZDS).LT.1.E-10) THEN
            DSA=0.
          ELSE
            DSA=DZAPP(1)/DDZDS
          ENDIF
C     find point where projected distance = dhxlim
          DDRS2=HALF*ABS(ONE/R1A-ONE/R2A)
          DSB=SQRT((DHXLIM-DCIRC)/DDRS2)
C     select closest of two points
          DS1=DSA
          IF(DSB.LT.ABS(DSA)) THEN
            DS1=SIGN(DSB,DSA)
          ENDIF
          DS2=DS1*SIGR1*SIGR2
          ZAPP(1,1)=ZAPP(1,1)+DS1*SNGL(HXI(KT,J1))
          ZAPP(2,1)=ZAPP(2,1)+DS2*SNGL(HXI(KT,J2))
          DZAPP(1)=ZAPP(2,1)-ZAPP(1,1)
C
C     check if helices miss by large distance
          IF (ABS(DZAPP(1)).GT.DHXLIM) THEN
            IFAIL=2
            GOTO 997
          ENDIF
C
C     move starting point for vertex by ds1
          VXA(1,ICOM+1)=VXA(1,ICOM+1)+DS1*COS(SNGL(FAPP(1,1)))
          VXA(2,ICOM+1)=VXA(2,ICOM+1)+DS1*SIN(SNGL(FAPP(1,1)))
C
C
          GO TO 485
C
  430     CONTINUE
C     CIRCLES SEPARATED
          KASE=3
          DCIRC=(D-R1A-R2A)
C     check if helices miss by large distance
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
C     find point where z-distance = 0
          SIGR1=SIGN(1. ,HR1)
          SIGR2=SIGN(1. ,HR2)
          DDZDS=HXI(KT,J1)+HXI(KT,J2)*DBLE(SIGR1*SIGR2)
          IF(ABS(DDZDS).LT.1.E-10) THEN
            DSA=0.
          ELSE
            DSA=DZAPP(1)/DDZDS
          ENDIF
C     find point where projected distance = dhxlim
          DDRS2=HALF*(ONE/R1A+ONE/R2A)
          DSB=SQRT((DHXLIM-DCIRC)/DDRS2)
C     select closest of two points
          DS1=DSA
          IF(DSB.LT.ABS(DSA)) THEN
            DS1=SIGN(DSB,DSA)
          ENDIF
          DS2=-DS1*SIGR1*SIGR2
          ZAPP(1,1)=ZAPP(1,1)+DS1*SNGL(HXI(KT,J1))
          ZAPP(2,1)=ZAPP(2,1)+DS2*SNGL(HXI(KT,J2))
          DZAPP(1)=ZAPP(2,1)-ZAPP(1,1)
C
C     check if helices miss by large distance
          IF (ABS(DZAPP(1)).GT.DHXLIM) THEN
            IFAIL=3
            GOTO 997
          ENDIF
C
C     move starting point for vertex by ds1
          VXA(1,ICOM+1)=VXA(1,ICOM+1)+DS1*COS(SNGL(FAPP(1,1)))
          VXA(2,ICOM+1)=VXA(2,ICOM+1)+DS1*SIN(SNGL(FAPP(1,1)))
C
  485     CONTINUE
C
          ICOM=ICOM+1
          VXA(3,ICOM)=0.5*(ZAPP(1,1)+ZAPP(2,1))
C
  490     CONTINUE
C
C     accept vertex as starting point if it agees with a previous one
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
        IFAIL=9
        GOTO 997
C
  610   CONTINUE
C
      ENDIF
C
C================ END OF SEARCH FOR START VERTEX ==================
C
C+++++++++++++++++ START FITTING SECTION ++++++++++++++++++++++++
C
      IF(LPSUM.OR.LMCALC) THEN
C RESET MOMENTUM SUM AND ERRORS
        DO 602 I=1,3
          FPS(I)=ZERO
          DO 601 J=1,3
            FVPS(I,J)=ZERO
  601     CONTINUE
          DO 602 J=1,3
            FVVPS(J,I)=ZERO
  602   CONTINUE
C
        IMX=3*(1+NTMAX)
        DO 603 I=1,IMX
          DTX(I)=ZERO
          DTY(I)=ZERO
          DTZ(I)=ZERO
C
  603   CONTINUE
C
        DO 604 KH=1,NTMAX
          DO 604 I=1,3
            DO 604 J=1,6
              DPAQ(I,J,KH)=ZERO
  604   CONTINUE
C
      ENDIF
C
C number of tracks and number of parameters to fit
      NFTRK=NHX+NEU
      IF(LVFIX) THEN
        NFPAR=  3*NHX+3*NEU
      ELSE
        NFPAR=3+3*NHX+3*NEU
      ENDIF
C
C start values of fitting parameters
C     vertex
      IF(.NOT.LVFIX) THEN
        PARO(1)=VXO(1)
        PARO(2)=VXO(2)
        PARO(3)=VXO(3)
      ENDIF
C
C     helices
      IF(LVFIX) THEN
        IPAR=0
      ELSE
        IPAR=3
      ENDIF
      DO 720 K=1,NHX
        IPAR=IPAR+1
C     ETA=R-D0
        PARO(IPAR)=ONE/HXI(KR,K)-HXI(KD,K)
        IPAR=IPAR+1
C     T
        PARO(IPAR)=HXI(KT,K)
        IPAR=IPAR+1
C     FI0
        PARO(IPAR)=HXI(KF,K)
  720 CONTINUE
C
      IF(NEU.LE.0) GO TO 750
      DO 740 KK=1,NEU
        K=NHX+KK
        IPAR=IPAR+1
C     P
        PARO(IPAR)=HXI(KR,K)
        IPAR=IPAR+1
C     T
        PARO(IPAR)=HXI(KT,K)
        IPAR=IPAR+1
C     FI0
        PARO(IPAR)=HXI(KF,K)
  740 CONTINUE
  750 CONTINUE
C
      LVF=LVFIX
      IF(LVF  ) THEN
C  VARY TRACK PARAMETERS ALONE FOR FEW ITERATIONS
        MXIT=4
      ELSE
        MXIT=MXITR
      ENDIF
C
  900 CONTINUE
      ITER=0
      CHISO=0.
C
 1000 CONTINUE
C
      ITER=ITER+1
C
      IF(ITER.GT.MXIT ) GO TO 2000
C
C
      DO 1100 I=1,NFPAR
        G(I)=0.
        DO 1100 J=1,NFPAR
          GG(I,J)=0.
 1100 CONTINUE
C
C
C     loop over vertices
C
      IF(.NOT.LVF  ) THEN
        DO 1345 IVX=1,NVX
C
C     distance vector from vertex and helix
          DELV(1)=PARO(1)-VXI(1,IVX)
          DELV(2)=PARO(2)-VXI(2,IVX)
          DELV(3)=PARO(3)-VXI(3,IVX)
C
C
C     derivatives of chisq with resp. to fitted vtx coord.
C
          DO 1300 I=1,3
            DO 1300 J=1,3
C     0.5 d(Chiv)/dVi
              G(I)=G(I)+EVI(I,J,IVX)*DELV(J)
C     0.5  d2(Chiv)/dVi dVj
              GG(I,J)=GG(I,J)+EVI(I,J,IVX)
 1300     CONTINUE
C
          IF(ITER.LE.1) THEN
C   VERTEX CHISQ
            CHIV=ZERO
            DO I=1,3
              DO J=1,3
                CHIV=CHIV+DELV(I)*EVI(I,J,IVX)*DELV(J)
              ENDDO
            ENDDO
            CHISO=CHISO+SNGL(CHIV)
          ENDIF
C
 1345   CONTINUE
      ENDIF
C
C     loop over helices
C
      DO 1500 K=1,NHX
C
        IF(LVF  ) THEN
          IX=  3*(K-1)
        ELSE
          IX=3+3*(K-1)
          VXO(1)=PARO(1)
          VXO(2)=PARO(2)
          VXO(3)=PARO(3)
        ENDIF
C
        ETA=PARO(IX+1)
        FT=PARO(IX+2)
        FI0=PARO(IX+3)
        SFI0=SIN(FI0)
        CFI0=COS(FI0)
        XC=-ETA*SFI0
        YC=ETA*CFI0
        V1XC=VXO(1)-XC
        V2YC=VXO(2)-YC
        FR=SQRT(V1XC**2+V2YC**2)*SIGN(ONE,ETA)
        D0=FR-ETA
        SFI=V1XC/FR
        CFI=-V2YC/FR
        FI=ATAN2(SFI,CFI)
        IF((FI-FI0).LT.-PI) FI=FI+TWO*PI
        FS=(FI-FI0)*FR
        Z0=VXO(3)-FS*FT
C
        FF0=FI-FI0
        SFF0=SIN(FF0)
        CFF0=COS(FF0)
        HHXI(KR,K)=ONE/FR
        HHXI(KT,K)=FT
        HHXI(KF,K)=FI0
        HHXI(KD,K)=D0
        HHXI(KZ,K)=Z0
C
C
C add contribution of helix to g and G matrix
C
C  FIRST DERIVATIVES WITH RESPECT TO INDEPENDENT PARAMETERS
C     d(h1)/d(v1)=d(rho)/d(v1) etc.
        H1V1=-HHXI(KR,K)**2*SFI
        H1V2= HHXI(KR,K)**2*CFI
        H1P1=-HHXI(KR,K)**2*CFF0
        H1P3=-HHXI(KR,K)**2*ETA*SFF0
C
C     H2P2=1
C
C     H3P3 =1
C
        H4V1=SFI
        H4V2=-CFI
        H4P1=CFF0-ONE
        H4P3=ETA*SFF0
C
        H5V1=-FT*(SFI*FF0+CFI)
        H5V2=FT*(CFI*FF0-SFI)
C     H5V3=1
        H5P1=-FT*(FF0*CFF0-SFF0)
        H5P2=-FS
        H5P3=-FT*(ETA*(FF0*SFF0+CFF0)-FR)
C
C
C
        DO 1450 I=1,5
          DI=HHXI(I,K)-HXI(I,K)
          DDI(I)=DI
C
C  0.5 * FIRST DERIVATIVE OF CHISQ WITH RESP. TO FIT PAR.
          IF(.NOT.LVF  ) THEN
            G(1)=G(1)+DI*(EHI(I,1,K)*H1V1+EHI(I,4,K)*H4V1+
     &        EHI(I,5,K)*H5V1)
            G(2)=G(2)+DI*(EHI(I,1,K)*H1V2+EHI(I,4,K)*H4V2+
     &        EHI(I,5,K)*H5V2)
            G(3)=G(3)+DI*EHI(I,5,K)
          ENDIF
          G(IX+1)=G(IX+1)+DI*(EHI(I,1,K)*H1P1+EHI(I,4,K)*H4P1+
     &      EHI(I,5,K)*H5P1)
          G(IX+2)=G(IX+2)+DI*(EHI(I,2,K)+EHI(I,5,K)*H5P2)
          G(IX+3)=G(IX+3)+DI*(EHI(I,1,K)*H1P3+EHI(I,3,K)+
     &      EHI(I,4,K)*H4P3+EHI(I,5,K)*H5P3)
 1450   CONTINUE
C
C  0.5 * SECOND DERIVATIVE OF CHISQ WITH RESP. TO FIT PAR.
        IF(.NOT.LVF  ) THEN
          GG(1,1)=GG(1,1)+
     &      EHI(1,1,K)*H1V1**2+EHI(4,4,K)*H4V1**2+EHI(5,5,K)*H5V1**2+
     &      TWO*(EHI(1,4,K)*H1V1*H4V1+EHI(1,5,K)*H1V1*H5V1+
     &      EHI(4,5,K)*H4V1*H5V1)
          GG(1,2)=GG(1,2)+
     &      EHI(1,1,K)*H1V1*H1V2+EHI(4,4,K)*H4V1*H4V2+
     &      EHI(5,5,K)*H5V1*H5V2+
     &      EHI(1,4,K)*(H1V1*H4V2+H4V1*H1V2)+
     &      EHI(1,5,K)*(H1V1*H5V2+H5V1*H1V2)+
     &      EHI(4,5,K)*(H4V1*H5V2+H5V1*H4V2)
          GG(1,3)=GG(1,3)+EHI(5,5,K)*H5V1+
     &      EHI(1,5,K)*H1V1+EHI(4,5,K)*H4V1
          GG(1,IX+1)=GG(1,IX+1)+
     &      EHI(1,1,K)*H1V1*H1P1+EHI(4,4,K)*H4V1*H4P1+
     &      EHI(5,5,K)*H5V1*H5P1+
     &      EHI(1,4,K)*(H1V1*H4P1+H4V1*H1P1)+
     &      EHI(1,5,K)*(H1V1*H5P1+H5V1*H1P1)+
     &      EHI(4,5,K)*(H4V1*H5P1+H5V1*H4P1)
          GG(1,IX+2)=GG(1,IX+2)+
     &      EHI(1,2,K)*H1V1+EHI(4,2,K)*H4V1+EHI(5,2,K)*H5V1+
     &      EHI(1,5,K)*H1V1*H5P2+
     &      EHI(4,5,K)*H4V1*H5P2+
     &      EHI(5,5,K)*H5V1*H5P2
          GG(1,IX+3)=GG(1,IX+3)+
     &      EHI(1,1,K)*H1V1*H1P3+EHI(4,4,K)*H4V1*H4P3+
     &      EHI(5,5,K)*H5V1*H5P3+
     &      EHI(1,3,K)*H1V1+EHI(1,4,K)*(H1V1*H4P3+H4V1*H1P3)+
     &      EHI(1,5,K)*(H1V1*H5P3+H5V1*H1P3)+EHI(3,4,K)*H4V1+
     &      EHI(3,5,K)*H5V1+EHI(4,5,K)*(H4V1*H5P3+H5V1*H4P3)
          GG(2,2)=GG(2,2)+
     &      EHI(1,1,K)*H1V2**2+EHI(4,4,K)*H4V2**2+EHI(5,5,K)*H5V2**2+
     &      TWO*(EHI(1,4,K)*H1V2*H4V2+EHI(1,5,K)*H1V2*H5V2+
     &      EHI(4,5,K)*H4V2*H5V2)
          GG(2,3)=GG(2,3)+EHI(5,5,K)*H5V2+
     &      EHI(1,5,K)*H1V2+EHI(4,5,K)*H4V2
          GG(2,IX+1)=GG(2,IX+1)+
     &      EHI(1,1,K)*H1V2*H1P1+EHI(4,4,K)*H4V2*H4P1+
     &      EHI(5,5,K)*H5V2*H5P1+
     &      EHI(1,4,K)*(H1V2*H4P1+H4V2*H1P1)+
     &      EHI(1,5,K)*(H1V2*H5P1+H5V2*H1P1)+
     &      EHI(4,5,K)*(H4V2*H5P1+H5V2*H4P1)
          GG(2,IX+2)=GG(2,IX+2)+
     &      EHI(1,2,K)*H1V2+EHI(4,2,K)*H4V2+EHI(5,2,K)*H5V2+
     &      EHI(1,5,K)*H1V2*H5P2+
     &      EHI(4,5,K)*H4V2*H5P2+
     &      EHI(5,5,K)*H5V2*H5P2
          GG(2,IX+3)=GG(2,IX+3)+
     &      EHI(1,1,K)*H1V2*H1P3+EHI(4,4,K)*H4V2*H4P3+
     &      EHI(5,5,K)*H5V2*H5P3+
     &      EHI(1,4,K)*(H1V2*H4P3+H4V2*H1P3)+
     &      EHI(1,5,K)*(H1V2*H5P3+H5V2*H1P3)+
     &      EHI(4,5,K)*(H4V2*H5P3+H5V2*H4P3)+
     &      EHI(1,3,K)*H1V2+EHI(4,3,K)*H4V2+EHI(5,3,K)*H5V2
          GG(3,3)=GG(3,3)+EHI(5,5,K)
          GG(3,IX+1)=GG(3,IX+1)+
     &      EHI(5,5,K)*H5P1+EHI(1,5,K)*H1P1+EHI(4,5,K)*H4P1
          GG(3,IX+2)=GG(3,IX+2)+
     &      EHI(5,5,K)*H5P2+EHI(2,5,K)
          GG(3,IX+3)=GG(3,IX+3)+
     &      EHI(5,5,K)*H5P3+EHI(1,5,K)*H1P3+EHI(3,5,K)+EHI(4,5,K)*H4P3
        ENDIF
        GG(IX+1,IX+1)=GG(IX+1,IX+1)+
     &    EHI(1,1,K)*H1P1**2+EHI(4,4,K)*H4P1**2+EHI(5,5,K)*H5P1**2+
     &    TWO*(EHI(1,4,K)*H1P1*H4P1+EHI(1,5,K)*H1P1*H5P1+
     &    EHI(4,5,K)*H4P1*H5P1)
        GG(IX+1,IX+2)=GG(IX+1,IX+2)+
     &    EHI(1,2,K)*H1P1+EHI(4,2,K)*H4P1+EHI(5,2,K)*H5P1+
     &    EHI(1,5,K)*H1P1*H5P2+
     &    EHI(4,5,K)*H4P1*H5P2+
     &    EHI(5,5,K)*H5P1*H5P2
        GG(IX+1,IX+3)=GG(IX+1,IX+3)+
     &    EHI(1,1,K)*H1P1*H1P3+EHI(4,4,K)*H4P1*H4P3+
     &    EHI(5,5,K)*H5P1*H5P3+
     &    EHI(1,3,K)*H1P1+EHI(1,4,K)*(H1P1*H4P3+H4P1*H1P3)+
     &    EHI(1,5,K)*(H1P1*H5P3+H5P1*H1P3)+EHI(3,4,K)*H4P1+
     &    EHI(3,5,K)*H5P1+EHI(4,5,K)*(H4P1*H5P3+H5P1*H4P3)
        GG(IX+2,IX+2)=GG(IX+2,IX+2)+
     &    EHI(2,2,K)+EHI(5,5,K)*H5P2**2+TWO*EHI(2,5,K)*H5P2
        GG(IX+2,IX+3)=GG(IX+2,IX+3)+
     &    EHI(1,2,K)*H1P3+EHI(3,2,K)+
     &    EHI(4,2,K)*H4P3+EHI(5,2,K)*H5P3+
     &    EHI(1,5,K)*H1P3*H5P2+EHI(3,5,K)*H5P2+
     &    EHI(4,5,K)*H4P3*H5P2+
     &    EHI(5,5,K)*H5P3*H5P2
        GG(IX+3,IX+3)=GG(IX+3,IX+3)+
     &    EHI(1,1,K)*H1P3**2+EHI(3,3,K)+
     &    EHI(4,4,K)*H4P3**2+EHI(5,5,K)*H5P3**2+
     &    TWO*(EHI(1,3,K)*H1P3+EHI(1,4,K)*H1P3*H4P3+
     &    EHI(1,5,K)*H1P3*H5P3+EHI(3,4,K)*H4P3+
     &    EHI(3,5,K)*H5P3+EHI(4,5,K)*H4P3*H5P3)
C
C
C
C
C
C
C
C
        IF(ITER.LE.1) THEN
C   HELIX CHISQ
          CHIH=ZERO
          DO I=1,5
            DO J=1,5
              CHIH=CHIH+DDI(I)*EHI(I,J,K)*DDI(J)
            ENDDO
          ENDDO
          CHISO=CHISO+SNGL(CHIH)
        ENDIF
C
 1500 CONTINUE
C
C     LOOP OVER NEUTRALS
C
      IF(NEU.LE.0) GO TO 9510
      DO 9500 KK=1,NEU
        K=KK+NHX
        IF(LVF  ) THEN
          IX=  3*NHX+3*(KK-1)
        ELSE
          IX=3+3*NHX+3*(KK-1)
          VXO(1)=PARO(1)
          VXO(2)=PARO(2)
          VXO(3)=PARO(3)
        ENDIF
C
        FP=PARO(IX+1)
        FT=PARO(IX+2)
        FI0=PARO(IX+3)
        SFI0=SIN(FI0)
        CFI0=COS(FI0)
C     D0
        D0=VXO(1)*SFI0-VXO(2)*CFI0
C     FS
        FS=(VXO(1)*CFI0+VXO(2)*SFI0)
C     Z0
        Z0=VXO(3)-FT*FS
C
        HHXI(KR,K)=FP
        HHXI(KT,K)=FT
        HHXI(KF,K)=FI0
        HHXI(KD,K)=D0
        HHXI(KZ,K)=Z0
C
C
C     add contributions of neutral to g and g matrix
C
C     derivatives with respect to independent parameters
C     H1P1=1
C     H2P2=1
C     H3P3=1
        H4V1=SFI0
        H4V2=-CFI0
        H4P3=FS
        H5V1=-FT*CFI0
        H5V2=-FT*SFI0
C     H5V3=1
        H5P2=-FS
        H5P3= FT*D0
C
        DO 9400 I=1,5
          DI=HHXI(I,K)-HXI(I,K)
          DDI(I)=DI
          IF(.NOT.LVF  ) THEN
            G(1)=G(1)+DI*(EHI(I,4,K)*H4V1+EHI(I,5,K)*H5V1)
            G(2)=G(2)+DI*(EHI(I,4,K)*H4V2+EHI(I,5,K)*H5V2)
            G(3)=G(3)+DI*EHI(I,5,K)
          ENDIF
          G(IX+1)=G(IX+1)+DI*EHI(I,1,K)
          G(IX+2)=G(IX+2)+DI*(EHI(I,2,K)+EHI(I,5,K)*H5P2)
          G(IX+3)=G(IX+3)+DI*(EHI(I,3,K)+EHI(I,4,K)*
     &      H4P3+EHI(I,5,K)*H5P3)
 9400   CONTINUE
C
        IF(.NOT.LVF  ) THEN
          GG(1,1)=GG(1,1)+EHI(4,4,K)*H4V1**2+EHI(5,5,K)*H5V1**2+
     &      TWO*(EHI(4,5,K)*H4V1*H5V1)
          GG(2,2)=GG(2,2)+EHI(4,4,K)*H4V2**2+EHI(5,5,K)*H5V2**2+
     &      TWO*(EHI(4,5,K)*H4V2*H5V2)
          GG(3,3)=GG(3,3)+EHI(5,5,K)
        ENDIF
        GG(IX+1,IX+1)=GG(IX+1,IX+1)+ EHI(1,1,K)
        GG(IX+2,IX+2)=GG(IX+2,IX+2)+ EHI(2,2,K)+
     &    EHI(5,5,K)*H5P2**2+
     &    TWO*(EHI(2,5,K)*H5P2)
        GG(IX+3,IX+3)=GG(IX+3,IX+3)+ EHI(3,3,K)+
     &    EHI(4,4,K)*H4P3**2+EHI(5,5,K)*H5P3**2+
     &    TWO*(EHI(3,4,K)*H4P3+EHI(3,5,K)*H5P3+EHI(4,5,K)*H4P3*H5P3)
        IF(.NOT.LVF  ) THEN
          GG(1,2)=GG(1,2)+EHI(4,4,K)*H4V1*H4V2+
     &      EHI(4,5,K)*(H4V1*H5V2+H5V1*H4V2)+
     &      EHI(5,5,K)*(H5V1*H5V2)
          GG(1,3)=GG(1,3)+EHI(4,5,K)*H4V1+
     &      EHI(5,5,K)*H5V1
          GG(1,IX+1)=GG(1,IX+1)+EHI(1,4,K)*H4V1+EHI(1,5,K)*H5V1
          GG(1,IX+2)=GG(1,IX+2)+EHI(2,4,K)*H4V1+EHI(2,5,K)*H5V1+
     &      EHI(4,5,K)*H4V1*H5P2+EHI(5,5,K)*H5V1*H5P2
          GG(1,IX+3)=GG(1,IX+3)+EHI(3,4,K)*H4V1+EHI(3,5,K)*H5V1+
     &      EHI(4,4,K)*H4V1*H4P3+
     &      EHI(4,5,K)*(H4V1*H5P3+H5V1*H4P3)+EHI(5,5,K)*H5V1*H5P3
          GG(2,3)=GG(2,3)+EHI(4,5,K)*H4V2+
     &      EHI(5,5,K)*H5V2
          GG(2,IX+1)=GG(2,IX+1)+EHI(1,4,K)*H4V2+EHI(1,5,K)*H5V2
          GG(2,IX+2)=GG(2,IX+2)+EHI(2,4,K)*H4V2+EHI(2,5,K)*H5V2+
     &      EHI(4,5,K)*H4V2*H5P2+EHI(5,5,K)*H5V2*H5P2
          GG(2,IX+3)=GG(2,IX+3)+EHI(3,4,K)*H4V2+EHI(3,5,K)*H5V2+
     &      EHI(4,4,K)*H4V2*H4P3+
     &      EHI(4,5,K)*(H4V2*H5P3+H5V2*H4P3)+EHI(5,5,K)*H5V2*H5P3
          GG(3,IX+1)=GG(3,IX+1)+EHI(1,5,K)
          GG(3,IX+2)=GG(3,IX+2)+EHI(2,5,K)+
     &      EHI(5,5,K)*H5P2
          GG(3,IX+3)=GG(3,IX+3)+EHI(3,5,K)+
     &      EHI(4,5,K)*(H4P3)+EHI(5,5,K)*H5P3
        ENDIF
        GG(IX+1,IX+2)=GG(IX+1,IX+2)+EHI(1,2,K)+
     &    EHI(1,5,K)*H5P2
        GG(IX+1,IX+3)=GG(IX+1,IX+3)+EHI(1,3,K)+
     &    EHI(1,4,K)*H4P3+EHI(1,5,K)*H5P3
        GG(IX+2,IX+3)=GG(IX+2,IX+3)+EHI(2,3,K)+
     &    EHI(2,4,K)*H4P3+EHI(2,5,K)*H5P3+
     &    EHI(3,5,K)*H5P2+EHI(4,5,K)*H4P3*H5P2+EHI(5,5,K)*H5P2*H5P3
C
        IF(ITER.LE.1) THEN
C   NEUTRAL TRACK CHISQ
          CHIN=ZERO
          DO I=1,5
            DO J=1,5
              CHIN=CHIN+DDI(I)*EHI(I,J,K)*DDI(J)
            ENDDO
          ENDDO
          CHISO=CHISO+SNGL(CHIN)
        ENDIF
C
C
 9500 CONTINUE
 9510 CONTINUE
C
C symmetrize gg matrix (only upper triangle of submatrices are correct)
      DO 5920 I=1,NFPAR
        DO 5910 J=1,I
          GG(I,J)=GG(J,I)
 5910   CONTINUE
 5920 CONTINUE
C
C
C
C
C     INVERT   GG**-1
      CALL DSINV(NFPAR,GG,NPMAX,IFLLL)
C
      IF(IFLLL.NE.0) THEN
C     problem in inverting gg matrix
        IFAIL=30
        ICNER2=ICNER2+1
        GOTO 997
      ENDIF
C
C  calculate fit parameter change
C
C     indep. par. change
      DO 1590 I=1,NFPAR
        DPAR(I)=0.
        DO 1570 J=1,NFPAR
          DPAR(I)=DPAR(I)+GG(I,J)*G(J)
 1570   CONTINUE
 1590 CONTINUE
C
C gary 15/12/93
C
      IF(LMASSC) THEN
       DO KK=1,NEU+NHX
        DO III=1,3
         DO JJJ=1,NFPAR
          DPDI(KK,III,JJJ)=0.
         ENDDO
        ENDDO
       ENDDO
C
C first do the helices
C
       DO  KK=1,NHX
        K=KK
        IF(LVF  ) THEN
         IX=  3*(K-1)
        ELSE
         IX=3+3*(K-1)
         VXO(1)=PARO(1)
         VXO(2)=PARO(2)
         VXO(3)=PARO(3)
        ENDIF
C
        ETA=PARO(IX+1)
        FT=PARO(IX+2)
        FI0=PARO(IX+3)
        SFI0=SIN(FI0)
        CFI0=COS(FI0)
        XC=-ETA*SFI0
        YC=ETA*CFI0
        V1XC=VXO(1)-XC
        V2YC=VXO(2)-YC
        FR=SQRT(V1XC**2+V2YC**2)*SIGN(ONE,ETA)
        D0=FR-ETA
        SFI=V1XC/FR
        CFI=-V2YC/FR
        FI=ATAN2(SFI,CFI)
        IF((FI-FI0).LT.-PI) FI=FI+TWO*PI
        FS=(FI-FI0)*FR
        Z0=VXO(3)-FS*FT
C
        FF0=FI-FI0
        SFF0=SIN(FF0)
        CFF0=COS(FF0)
        ROFP = 1./ (0.29979 * BFIELD / 10.) * 100.
        FRSIG=SIGN(ONE,FR)
        PCONV= FRSIG/ROFP
        FPT=PCONV*FR
        FPX=FPT*CFI
        FPY=FPT*SFI
        FPZ=FPT*FT
        PC(K,1)=FPX
        PC(K,2)=FPY
        PC(K,3)=FPZ
        PC(K,4)=SQRT(FPX**2+FPY**2+FPZ**2)
        PC(K,5)=AMPC(K)
        PC(K,6)=SQRT(PC(K,4)**2+PC(K,5)**2)
        DPDI(K,1,2)=-PCONV
        DPDI(K,1,IX+1)=PCONV*CFI0
        DPDI(K,1,IX+3)=-PCONV*ETA*SFI0
        DPDI(K,2,1)=PCONV
        DPDI(K,2,IX+1)=PCONV*SFI0
        DPDI(K,2,IX+3)=PCONV*ETA*CFI0
        DPDI(K,3,1)=    PCONV*FT*SFI
        DPDI(K,3,2)=   -PCONV*FT*CFI
        DPDI(K,3,IX+1)= PCONV*FT*CFF0
        DPDI(K,3,IX+2)= FPT
        DPDI(K,3,IX+3)= PCONV*ETA*SFF0
       ENDDO
C
C then do the neutrals
C

       DO KK=1,NEU
        K=KK+NHX
        IX=3+3*NHX+3*(KK-1)
C
        FP=PARO(IX+1)
        FT=PARO(IX+2)
        FI0=PARO(IX+3)
        SFI0=SIN(FI0)
        CFI0=COS(FI0)
        FPT=FP/SQRT(ONE+FT**2)
        FPX=FPT*CFI0
        FPY=FPT*SFI0
        FPZ=FPT*FT
        PC(K,1)=FPX
        PC(K,2)=FPY
        PC(K,3)=FPZ
        PC(K,4)=SQRT(FPX**2+FPY**2+FPZ**2)
        PC(K,5)=AMPC(K)
        PC(K,6)=SQRT(PC(K,4)**2+PC(K,5)**2)
C     DERIVATIVES OF MOM. WITH RESP. TO INDEP. PAR.
        DPDI(K,1,IX+1)=FPX/FP
        DPDI(K,1,IX+2)=-FPX*FT/(ONE+FT**2)
        DPDI(K,1,IX+3)=-FPY
        DPDI(K,2,IX+1)=FPY/FP
        DPDI(K,2,IX+2)=-FPY*FT/(ONE+FT**2)
        DPDI(K,2,IX+3)= FPX
        DPDI(K,3,IX+1)=FPZ/FP
        DPDI(K,3,IX+2)=FPT/(ONE+FT**2)
       ENDDO
C
       DO K=1,NHX+NEU
        DO I=1,3
         DCDP(K,I)=0.
         IF(USE(K)) THEN
          DO J=1,NHX+NEU
           IF(USE(J).AND.J.NE.K) THEN
            DCDP(K,I)=DCDP(K,I)+2*(PC(K,I)/PC(K,6))*PC(J,6)-2*PC(J,I)
           ENDIF
          ENDDO
         ENDIF
        ENDDO
       ENDDO
C
       DO I=1,NFPAR
        DCDI(I)=0.
        DO K=1,NHX+NEU
         DO J=1,3
          DCDI(I)=DCDI(I)+DCDP(K,J)*DPDI(K,J,I)
         ENDDO
        ENDDO
       ENDDO
C
C mass constraint
C
       CMASS=-(CONMAS**2)
       DO K=1,NHX+NEU
        IF(USE(K)) THEN
         CMASS=CMASS+PC(K,5)**2
         DO J=1,NHX+NEU
          IF(USE(J).AND.J.NE.K) THEN
           CMASS=CMASS+PC(J,6)*PC(K,6)
           DO I=1,3
            CMASS=CMASS-PC(J,I)*PC(K,I)
           ENDDO
          ENDIF
         ENDDO
        ENDIF
       ENDDO
C
       DEL=0.
       DELC=0.
       DO I=1,NFPAR
        DO J=1,NFPAR
         DEL=DEL+DCDI(I)*GG(I,J)*G(J)
         DELC=DELC+DCDI(I)*GG(I,J)*DCDI(J)
        ENDDO
       ENDDO
       CON=(CMASS-DEL)/DELC
       DO I=1,NFPAR
        DPAR(I)=0.
        DO J=1,NFPAR
         DPAR(I)=DPAR(I)+GG(I,J)*(G(J)+CON*DCDI(J))
        ENDDO
       ENDDO
       DO I=1,NFPAR
        DO J=1,NFPAR
         GGC(I,J)=GG(I,J)
        ENDDO
       ENDDO

       DO I=1,NFPAR
        DO J=1,NFPAR
         DO K=1,NFPAR
          DO L=1,NFPAR
           GG(I,J)=GG(I,J)-DCDI(K)*GGC(K,I)*GGC(J,L)*DCDI(L)/DELC
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
C
C gary
C
      D2VX=0.
      DO 1600 I=1,NFPAR
        PARO(I)=PARO(I)-DPAR(I)
 1600 CONTINUE
C
      JTER=0
      KTER=0
 1610 CONTINUE
C
      IF(KTER.GT.10) GO TO 2000

      IF(JTER.GT.100) GO TO 2000
C
      CHISV=0.
      CHISH=0.
      CHISN=0.
      CHISQ=0.
C
C
C     loop over vertices
C
      IF(.NOT.LVF  ) THEN
        DO 1620 IVX=1,NVX
C
C     distance vector from vertex and helix
          DELV(1)=PARO(1)-VXI(1,IVX)
          DELV(2)=PARO(2)-VXI(2,IVX)
          DELV(3)=PARO(3)-VXI(3,IVX)
C
C     calculate chisq vertex contrib
          CHIV=ZERO
          DO 1615 I=1,3
            DO 1615 J=1,3
              CHIV=CHIV+DELV(I)*EVI(I,J,IVX)*DELV(J)
 1615     CONTINUE
          CHISV=CHISV+SNGL(CHIV)
 1620   CONTINUE
      ENDIF
C
C
C     loop over helices
C
      DO 1640 K=1,NHX
C
        IF(LVF  ) THEN
          IX=  3*(K-1)
        ELSE
          IX=3+3*(K-1)
          VXO(1)=PARO(1)
          VXO(2)=PARO(2)
          VXO(3)=PARO(3)
        ENDIF
C
        ETA=PARO(IX+1)
        FT=PARO(IX+2)
        FI0=PARO(IX+3)
        SFI0=SIN(FI0)
        CFI0=COS(FI0)
        XC=-ETA*SFI0
        YC=ETA*CFI0
        V1XC=VXO(1)-XC
        V2YC=VXO(2)-YC
        FR=SQRT(V1XC**2+V2YC**2)*SIGN(ONE,ETA)
        D0=FR-ETA
        SFI=V1XC/FR
        CFI=-V2YC/FR
        FI=ATAN2(SFI,CFI)
        IF((FI-FI0).LT.-PI) FI=FI+TWO*PI
        FS=(FI-FI0)*FR
        Z0=VXO(3)-FS*FT
C
        FF0=FI-FI0
        SFF0=SIN(FF0)
        CFF0=COS(FF0)
        HHXI(KR,K)=ONE/FR
        HHXI(KT,K)=FT
        HHXI(KF,K)=FI0
        HHXI(KD,K)=D0
        HHXI(KZ,K)=Z0
C
        CHIH=ZERO
        DO 1630 I=1,5
          DI=HHXI(I,K)-HXI(I,K)
          DO 1630 J=1,5
            DJ=HHXI(J,K)-HXI(J,K)
            CHIH=CHIH+DI*EHI(I,J,K)*DJ
C
 1630   CONTINUE
        CHISH=CHISH+SNGL(CHIH)
 1640 CONTINUE
C
C  LOOP OVER NEUTRALS
C
      DO 1800 KK=1,NEU
        IF(LVF  ) THEN
          IX=  3*NHX+3*(KK-1)
        ELSE
          IX=3+3*NHX+3*(KK-1)
          VXO(1)=PARO(1)
          VXO(2)=PARO(2)
          VXO(3)=PARO(3)
        ENDIF
        K=KK+NHX
C
        FP=PARO(IX+1)
        FT=PARO(IX+2)
        FI0=PARO(IX+3)
        SFI0=SIN(FI0)
        CFI0=COS(FI0)
C     D0
        D0=VXO(1)*SFI0-VXO(2)*CFI0
C     FS
        FS=(VXO(1)*CFI0+VXO(2)*SFI0)
C     Z0
        Z0=VXO(3)-FT*FS
C
        HHXI(KR,K)=FP
        HHXI(KT,K)=FT
        HHXI(KF,K)=FI0
        HHXI(KD,K)=D0
        HHXI(KZ,K)=Z0
C
        CHIN=ZERO
        DO 1720 I=1,5
          DI=HHXI(I,K)-HXI(I,K)
          DO 1720 J=1,5
            DJ=HHXI(J,K)-HXI(J,K)
            CHIN=CHIN+DI*EHI(I,J,K)*DJ
 1720   CONTINUE
        CHISN=CHISN+SNGL(CHIN)
C
 1800 CONTINUE
C
      CHISQ=CHISV+CHISH+CHISN
C  PROVIDE FOR CRAZY CHISQ DEPENDENCE
C  REDUCE PARAMETER CHANGE IF CHISQ INCREASES
C
      IF(ITER.GT.4) THEN
      IF((CHISQ.GT.(CHISO+1.E-4).AND.JTER.EQ.0).OR.
     &  (CHISQ.GT.1.1*CHISO) ) THEN
C
        ICNER3=ICNER3+1
        DO 1850 I=1,NFPAR
          DPAR(I)=DPAR(I)/TWO
          PARO(I)=PARO(I)+DPAR(I)
 1850   CONTINUE
        JTER=JTER+1
        CHIS1=CHISQ
        GO TO 1610
      ELSE
        IF(JTER.GT.0) THEN
C     ESTIMATE BEST PARAMETERS
          CHDIF=CHIS1-CHISO
          CHTRM=CHIS1+CHISO-2.*CHISQ
          IF(CHTRM.GT.0.) THEN
C         CONCAVE CHISQ DEPENDANCE
            FX=(CHIS1-CHISO)/(2.*(CHIS1+CHISO-2.*CHISQ))
            FX=AMAX1(FX,-2.)
            FX=AMIN1(FX,2.)
          ELSE
C         CONVEX CHISQ DEPENDANCE
            FX=-2.
          ENDIF
          DO 1860 I=1,NFPAR
            DPAR(I)=DPAR(I)*FX
 1860     CONTINUE
          JTER=0
          KTER=KTER+1
          GO TO 1610
        ENDIF
      ENDIF
      ENDIF
C
      DCHI2=CHISO-CHISQ
C
      CHISO=CHISQ
C
C
C
C     CHECK FOR CHANGE IN CHISQ
C
      IF(DCHI2 .GT.CHISC.AND.DCHI2 .GT.CHISR*CHISQ) GO TO 1000
C
C     CHECK FOR CHANGE IN PARAMETERS
      DO 1998 IPR=1,NFPAR
        IF(DPAR(IPR)**2.GT.PARCR*GG(IPR,IPR)) GO TO 1000
 1998 CONTINUE
C
 2000 CONTINUE
      IF(LVF  ) THEN
        LVF  =.FALSE.
        DO 2010 I=NFPAR,1,-1
          PARO(I+3)=PARO(I)
 2010   CONTINUE
        PARO(1)=VXO(1)
        PARO(2)=VXO(2)
        PARO(3)=VXO(3)
        NFPAR=NFPAR+3
        GO TO 900
      ENDIF
C
C     output vertex and errors
      IX=0
      DO 2200 I=1,3
        VXOUT(I)=PARO(I)
        DO 2100 J=1,I
          IX=IX+1
          VVXOU(IX)=GG(I,J)
 2100   CONTINUE
 2200 CONTINUE
C
C
C     do not proceed for combinations that give a very high chisq
      IF(CHISQ.GT.CHIMAX) THEN
        IFAIL=99
        RETURN
      ENDIF
C
C     output helices and errors
      DO 2300 K=1,NHX
        IX=3+3*(K-1)
C
        ETA=PARO(IX+1)
        FT=PARO(IX+2)
        FI0=PARO(IX+3)
        SFI0=SIN(FI0)
        CFI0=COS(FI0)
        XC=-ETA*SFI0
        YC=ETA*CFI0
        V1XC=PARO(1)-XC
        V2YC=PARO(2)-YC
        FR=SQRT(V1XC**2+V2YC**2)*SIGN(ONE,ETA)
        D0=FR-ETA
        SFI=V1XC/FR
        CFI=-V2YC/FR
        FI=ATAN2(SFI,CFI)
        IF((FI-FI0).LT.-PI) FI=FI+TWO*PI
        FS=(FI-FI0)*FR
        Z0=PARO(3)-FS*FT
C
        FF0=FI-FI0
        SFF0=SIN(FF0)
        CFF0=COS(FF0)
        HHXI(KR,K)=ONE/FR
        HHXI(KT,K)=FT
        HHXI(KF,K)=FI0
        HHXI(KD,K)=D0
        HHXI(KZ,K)=Z0
C
        CHISH=CHISH+SNGL(CHIH)
C
C     derivatives with respect to independent parameters
C     d(h1)/d(v1)=d(rho)/d(v1) etc.
        H1V1=-HHXI(KR,K)**2*SFI
        H1V2= HHXI(KR,K)**2*CFI
        H1P1=-HHXI(KR,K)**2*CFF0
        H1P3=-HHXI(KR,K)**2*ETA*SFF0
C
C     H2P2=1
C
C     H3P3 =1
C
        H4V1=SFI
        H4V2=-CFI
        H4P1=CFF0-ONE
        H4P3=ETA*SFF0
C
        H5V1=-FT*(SFI*FF0+CFI)
        H5V2=FT*(CFI*FF0-SFI)
C     H5V3=1
        H5P1=-FT*(FF0*CFF0-SFF0)
        H5P2=-FS
        H5P3=-FT*(ETA*(FF0*SFF0+CFF0)-FR)
C
C
        IJS=1
C
        DO 2220 I=1,5
          DO 2220 J=1,6
            DHIP(I,J)=ZERO
 2220   CONTINUE
        DHIP(1,1)=H1V1
        DHIP(1,2)=H1V2
        DHIP(1,4)=H1P1
        DHIP(1,6)=H1P3
        DHIP(2,5)=ONE
        DHIP(3,6)=ONE
        DHIP(4,1)=H4V1
        DHIP(4,2)=H4V2
        DHIP(4,4)=H4P1
        DHIP(4,6)=H4P3
        DHIP(5,1)=H5V1
        DHIP(5,2)=H5V2
        DHIP(5,3)=ONE
        DHIP(5,4)=H5P1
        DHIP(5,5)=H5P2
        DHIP(5,6)=H5P3
C
        IJ=0
        DO 2230 II=1,5
          DO 2230 JJ=1,II
            IJ=IJ+1
            AA=ZERO
            DO 2225 I=IJS,6
              IDX=I
              IF(I.GT.3) IDX=IX+I-3
              DO 2225 J=IJS,6
                JDX=J
                IF(J.GT.3) JDX=IX+J-3
                AA=AA+DHIP(II,I)*GG(IDX,JDX)*DHIP(JJ,J)
 2225       CONTINUE
            VHXOU(IJ,K)=AA
 2230   CONTINUE
C
        DO 2240 I=1,5
          HXOU(I,K)=HHXI(I,K)
 2240   CONTINUE
C
C
        IF(LPSUM.OR.LMCALC) THEN
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
          FRSIG=SIGN(ONE,FR)
          PCONV= FRSIG/ROFP
          FPT=PCONV*FR
          FPX=FPT*CFI
          FPY=FPT*SFI
          FPZ=FPT*FT
C
C     DERIVATIVES OF MOM. WITH RESP. TO INDEP. PAR.
          PXV2=-PCONV
          PXP1=PCONV*CFI0
          PXP3=-PCONV*ETA*SFI0
          PYV1=PCONV
          PYP1=PCONV*SFI0
          PYP3=PCONV*ETA*CFI0
          PZV1=PCONV*FT*SFI
          PZV2=-PCONV*FT*CFI
          PZP1=PCONV*FT*CFF0
          PZP2=FPT
          PZP3=PCONV*ETA*SFF0
C
          FPS(1)=FPS(1)+FPX
          FPS(2)=FPS(2)+FPY
          FPS(3)=FPS(3)+FPZ
C
          DTX(2)=DTX(2)+PXV2
          DTX(IX+1)=PXP1
          DTX(IX+3)=PXP3
C
          DTY(1)=DTY(1)+PYV1
          DTY(IX+1)=PYP1
          DTY(IX+3)=PYP3
C
          DTZ(1)=DTZ(1)+PZV1
          DTZ(2)=DTZ(2)+PZV2
          DTZ(IX+1)=PZP1
          DTZ(IX+2)=PZP2
          DTZ(IX+3)=PZP3
C
          IF(LMCALC) THEN
C
            FPI(1,K)=FPX
            FPI(2,K)=FPY
            FPI(3,K)=FPZ
C
            DPAQ(1,2,K)=PXV2
            DPAQ(1,4,K)=PXP1
            DPAQ(1,6,K)=PXP3
            DPAQ(2,1,K)=PYV1
            DPAQ(2,4,K)=PYP1
            DPAQ(2,6,K)=PYP3
            DPAQ(3,1,K)=PZV1
            DPAQ(3,2,K)=PZV2
            DPAQ(3,4,K)=PZP1
            DPAQ(3,5,K)=PZP2
            DPAQ(3,6,K)=PZP3
C
          ENDIF
C
        ENDIF
C
 2300 CONTINUE
C
C     output neutrals and errors
      IF(NEU.LE.0) GO TO 2410
      DO 2400 KK=1,NEU
        K=KK+NHX
        IX=3+3*NHX+3*(KK-1)
C
        FP=PARO(IX+1)
        FT=PARO(IX+2)
        FI0=PARO(IX+3)
        SFI0=SIN(FI0)
        CFI0=COS(FI0)
C     D0
        D0=PARO(1)*SFI0-PARO(2)*CFI0
C     FS
        FS=(PARO(1)*CFI0+PARO(2)*SFI0)
C     Z0
        Z0=PARO(3)-FT*FS
C
        HHXI(KR,K)=FP
        HHXI(KT,K)=FT
        HHXI(KF,K)=FI0
        HHXI(KD,K)=D0
        HHXI(KZ,K)=Z0
C
C
C     derivatives with respect to independent parameters
C     H1P1=1
C     H2P2=1
C     H3P3=1
        H4V1=SFI0
        H4V2=-CFI0
        H4P3=FS
        H5V1=-FT*CFI0
        H5V2=-FT*SFI0
C     H5V3=1
        H5P2=-FS
        H5P3= FT*D0
C
        IJ=0
        DO 2340 I=1,5
          TNUO(I,KK)=HHXI(I,K)
 2340   CONTINUE
C
        DO 2360 I=1,5
          DO 2360 J=1,6
            DHIP(I,J)=ZERO
 2360   CONTINUE
        DHIP(1,4)=ONE
        DHIP(2,5)=ONE
        DHIP(3,6)=ONE
        DHIP(4,1)=H4V1
        DHIP(4,2)=H4V2
        DHIP(4,6)=H4P3
        DHIP(5,1)=H5V1
        DHIP(5,2)=H5V2
        DHIP(5,3)=ONE
        DHIP(5,5)=H5P2
        DHIP(5,6)=H5P3
C
        IJS=1
C
        IJ=0
        DO 2370 II=1,5
          DO 2370 JJ=1,II
            IJ=IJ+1
            AA=ZERO
            DO 2365 I=IJS,6
              IDX=I
              IF(I.GT.3) IDX=IX+I-3
              DO 2365 J=IJS,6
                JDX=J
                IF(J.GT.3) JDX=IX+J-3
                AA=AA+DHIP(II,I)*GG(IDX,JDX)*DHIP(JJ,J)
 2365       CONTINUE
            VTNUO(IJ,KK)=AA
 2370   CONTINUE
C
C
C
C
        IF(LPSUM.OR.LMCALC) THEN
C
          FPT=FP/SQRT(ONE+FT**2)
          FPX=FPT*CFI0
          FPY=FPT*SFI0
          FPZ=FPT*FT
C     DERIVATIVES OF MOM. WITH RESP. TO INDEP. PAR.
          PXP1=FPX/FP
          PXP2=-FPX*FT/(ONE+FT**2)
          PXP3=-FPY
          PYP1=FPY/FP
          PYP2=-FPY*FT/(ONE+FT**2)
          PYP3= FPX
          PZP1=FPZ/FP
          PZP2=FPT/(ONE+FT**2)
C
          FPS(1)=FPS(1)+FPX
          FPS(2)=FPS(2)+FPY
          FPS(3)=FPS(3)+FPZ
C
          DTX(IX+1)=PXP1
          DTX(IX+2)=PXP2
          DTX(IX+3)=PXP3
C
          DTY(IX+1)=PYP1
          DTY(IX+2)=PYP2
          DTY(IX+3)=PYP3
C
          DTZ(IX+1)=PZP1
          DTZ(IX+2)=PZP2
C
          IF(LMCALC) THEN
C
            FPI(1,K)=FPX
            FPI(2,K)=FPY
            FPI(3,K)=FPZ
C
            DPAQ(1,4,K)=PXP1
            DPAQ(1,5,K)=PXP2
            DPAQ(1,6,K)=PXP3
            DPAQ(2,4,K)=PYP1
            DPAQ(2,5,K)=PYP2
            DPAQ(2,6,K)=PYP3
            DPAQ(3,4,K)=PZP1
            DPAQ(3,5,K)=PZP2
            DPAQ(3,6,K)=ZERO
C
C
          ENDIF
C
        ENDIF
C
 2400 CONTINUE
 2410 CONTINUE
C
C
      IF(LPSUM.OR.LMCALC) THEN
C     ERROR CORRELATION BETWEEN MOM. SUM VECTOR COMP.
        DO 6100 IX=1,NFPAR
          DO 6100 JX=1,NFPAR
            FVPS(1,1)=FVPS(1,1)+DTX(IX)*GG(IX,JX)*DTX(JX)
            FVPS(1,2)=FVPS(1,2)+DTX(IX)*GG(IX,JX)*DTY(JX)
            FVPS(2,2)=FVPS(2,2)+DTY(IX)*GG(IX,JX)*DTY(JX)
            FVPS(1,3)=FVPS(1,3)+DTX(IX)*GG(IX,JX)*DTZ(JX)
            FVPS(2,3)=FVPS(2,3)+DTY(IX)*GG(IX,JX)*DTZ(JX)
            FVPS(3,3)=FVPS(3,3)+DTZ(IX)*GG(IX,JX)*DTZ(JX)
 6100   CONTINUE
C
        DO 6200 IX=1,3
          DO 6200 JX=1,NFPAR
            FVVPS(IX,1)=FVVPS(IX,1)+GG(IX,JX)*DTX(JX)
            FVVPS(IX,2)=FVVPS(IX,2)+GG(IX,JX)*DTY(JX)
            FVVPS(IX,3)=FVVPS(IX,3)+GG(IX,JX)*DTZ(JX)
 6200   CONTINUE
C
C
C
        IF(LMCALC) THEN
          NHP=NHX+NEU
C LOOP OVER PARTICLE ASSIGNEMENT
          DO 6600  IPA=1,NPIDC
            FES=ZERO
            DO 6220 K=1,NHP
              KPI=K+(IPA-1)*NHP
              FE=FPI(1,K)**2+FPI(2,K)**2+FPI(3,K)**2
              PMASS=AMPC(KPI)
              FE=FE+PMASS**2
              FE=SQRT(ABS(FE))
              FPI(4,K)=FE
              FES=FES+FE
 6220       CONTINUE
            CMASQ=FES**2-FPS(1)**2-FPS(2)**2-FPS(3)**2
            CMAS=SQRT(ABS(CMASQ))
C
C     MASS ERROR AND CORRELATIONS
            DO IML=1,IMX
              FDB(IML)=ZERO
            ENDDO
            DO 6300 K=1,NHP
              IX=3*K
              DO 6300 IA=1,3
                CEP=FES*FPI(IA,K)/FPI(4,K)-FPS(IA)
                FDB(1)=FDB(1)+CEP*DPAQ(IA,1,K)
                FDB(2)=FDB(2)+CEP*DPAQ(IA,2,K)
                FDB(3)=FDB(3)+CEP*DPAQ(IA,3,K)
C
                FDB(IX+1)=FDB(IX+1)+CEP*DPAQ(IA,4,K)
                FDB(IX+2)=FDB(IX+2)+CEP*DPAQ(IA,5,K)
                FDB(IX+3)=FDB(IX+3)+CEP*DPAQ(IA,6,K)
 6300       CONTINUE
C
            SDMQ=ZERO
            SDMX=ZERO
            SDMY=ZERO
            SDMZ=ZERO
            SDMPX=ZERO
            SDMPY=ZERO
            SDMPZ=ZERO
            DO 6400 I=1,NFPAR
              SDMX=SDMX+GG(1,I)*FDB(I)
              SDMY=SDMY+GG(2,I)*FDB(I)
              SDMZ=SDMZ+GG(3,I)*FDB(I)
              DO 6400 J=1,NFPAR
                SDMQ=SDMQ+FDB(I)*GG(I,J)*FDB(J)
                SDMPX=SDMPX+DTX(I)*GG(I,J)*FDB(J)
                SDMPY=SDMPY+DTY(I)*GG(I,J)*FDB(J)
                SDMPZ=SDMPZ+DTZ(I)*GG(I,J)*FDB(J)
 6400       CONTINUE
            SDMQ=SDMQ/CMASQ
            SDMX=SDMX/CMAS
            SDMY=SDMY/CMAS
            SDMZ=SDMZ/CMAS
            SDMPX=SDMPX/CMAS
            SDMPY=SDMPY/CMAS
            SDMPZ=SDMPZ/CMAS
C
C   STORE OUTPUT VARIABLES
C     MOMENTUM SUM
            PSUM(1)=FPS(1)
            PSUM(2)=FPS(2)
            PSUM(3)=FPS(3)
C
C     MASS
            AMASS(IPA)=CMAS
C
C     MOMENTUM SUM ERROR
            VPSUM(1)=FVPS(1,1)
            VPSUM(2)=FVPS(1,2)
            VPSUM(3)=FVPS(2,2)
            VPSUM(4)=FVPS(1,3)
            VPSUM(5)=FVPS(2,3)
            VPSUM(6)=FVPS(3,3)
C
C     CORRELATION MASS/MOMENTUM SUM
            VMPS(1,IPA)=SDMPX
            VMPS(2,IPA)=SDMPY
            VMPS(3,IPA)=SDMPZ
C
C     MASS ERROR
            DMASS(IPA)=SQRT(ABS(SDMQ))
C
C     CORRELATION MOMENTUM SUM/VERTEX
            VPSVX(1)=FVVPS(1,1)
            VPSVX(2)=FVVPS(2,1)
            VPSVX(3)=FVVPS(3,1)
            VPSVX(4)=FVVPS(1,2)
            VPSVX(5)=FVVPS(2,2)
            VPSVX(6)=FVVPS(3,2)
            VPSVX(7)=FVVPS(1,3)
            VPSVX(8)=FVVPS(2,3)
            VPSVX(9)=FVVPS(3,3)
C
C     CORRELATION MASS/VERTEX


            VMVX(1,IPA)=SDMX
            VMVX(2,IPA)=SDMY
            VMVX(3,IPA)=SDMZ
C
 6600     CONTINUE
C
C
        ENDIF
C
      ENDIF
C
      RETURN
C - error
  997 CHISQ = 1.E30
      END
