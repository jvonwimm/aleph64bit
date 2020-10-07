*DK DTE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTE
CH
      SUBROUTINE DTE
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C INVOKED BY TANSW.EQ.'TE'  (DTE)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      PARAMETER (NPR=2)
      CHARACTER *1 DT1
      CHARACTER *2 TANSW,TPR(NPR),DT2
      CHARACTER *3 DT3,DTR
      CHARACTER *49 T
      DATA TPR/  'TR'      ,     'CO'/
      DIMENSION PR(4,NPR)
      DATA PR/-99.,0.,999.,0.,  0.,8.,32.,-1./
      CHARACTER *24 TMAC
      DATA TMAC/'(PI:ZP:DO:GB:T) :'/
      DIMENSION XST(3,9),YST(3,9),NST(9)
      LOGICAL FDAT,FST,FNRT,FLIST,CHG
      DATA FDAT/.TRUE./
      DLINDD=PDCODD(2,LITRDD)
      IF(NTRKDP.NE.-99999.AND.NTRKDP.NE.0) PR(2,1)=NTRKDP
C        123456789 123456789 123456789 123456789 123456789
  930 T='TE:W1 Track extrapolation from LIst: TR=123 CO:12'
      IF(FDAT) T(32:35)='FRft'
      T(41:43)=DT3(PR(2,1))
      T(48:49)=DT2(PR(2,2))
      CALL DTNOSL(PR,2,47,T)
      CALL DWR_HL_AR(T)
      IF(NUM.GT.0) THEN
C          123456789 123456789 123456789 123456789 123456789
        T='List(9):   .   .   .   .   .   .   .   .   .  '
        T(6:6)=DT1(FLOAT(NUM))
        L=11
        DO N=1,NUM
          T(L:L+1)=DT3(FLOAT(NST(N)))
          L=L+4
        END DO
        CALL DWRT(T)
      END IF
  936 CHG=.FALSE.
      CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(1,0,
     &  1,0,' ',0,
     &  1,NPR,TPR,PR,
     &  NEXEC,CHG,TANSW)
      IF(TANSW.EQ.'PI') THEN
        NTRKDP=0
        CALL DPOS(TANSW,FDUM,TDUM,NEXEC,CHG)
        IF(NTRKDP.NE.-99999.AND.NTRKDP.NE.0) PR(2,1)=NTRKDP
      END IF
      CALL DGZOOM(6,-1,0,0)
      NTR=PR(2,1)
      IF(NUM.GT.0) THEN
        DO NLIN=1,NUM
          IF(NTR.EQ.NST(NLIN)) GO TO 900
        END DO
      END IF
      NLIN=9999
  900 GO TO (910,920,930,940),NEXEC
  910 RETURN
C     ...................................................................... PT
  920 IF(TANSW.EQ.'PT') THEN
        CALL DWRT('Move cursor to track AND TYP <CR>')
        CALL DQHLP('C0 ')
        CALL DGINMA(TMAC)
C       THLP='C0'
        GO TO 936
      END IF
C     ...................................................................... LI
      IF(TANSW.EQ.'LI') THEN
        FDAT=.FALSE.
        IF(NUM.LE.0) GO TO 929
        GO TO 930
      END IF
C     ...................................................................... FR
      IF(TANSW.EQ.'FR') THEN
        FDAT=.TRUE.
        GO TO 930
      END IF
C     ...................................................................... FL
      IF(TANSW.EQ.'FL') THEN
        IF(.NOT.FST) GO TO 928
        CALL DTEMF(PR(1,2))
        GO TO 936
      END IF
      DTR=DT3(PR(2,1))
C     ...................................................................... TY
      IF(TANSW.EQ.'TY') THEN
        IF(NUM.LE.0) GO TO 929
        DO N=1,NUM
          WRITE(T,1003) NST(N),(XST(J,N),J=1,3),(YST(I,N),I=1,3)
 1003     FORMAT(1X,I3,' X=',3F6.0,' Y=',3F6.0)
          CALL DWRT(T)
        END DO
        GO TO 936
      END IF
C     ...................................................................... RT
      IF(TANSW.EQ.'RT') THEN
        CALL DTER(NUM,XST,YST,NST)
        IF(NUM.LE.0) GO TO 929
        CALL DWRT(DT1(FLOAT(NUM))//' tracks read from '//
     &    TFILDC//'EXTR')
        GO TO 930
      END IF
C     ...................................................................... WT
      IF(TANSW.EQ.'WT') THEN
        IF(NUM.LE.0) GO TO 929
        CALL DTEW(NUM,XST,YST,NST)
        CALL DWRT(DT1(FLOAT(NUM))//' tracks appended to '//
     &    TFILDC//'EXTR')
        GO TO 936
      END IF
C     ..................................................................... CA
      IF(TANSW.EQ.'CA') THEN
        IF(NUM.LE.0) GO TO 929
        NUM=0
        PR(2,1)=1.
C ?????????????????????? CHANGE OF 15.2.96 MAY BE WRONG
        GO TO 930
      END IF
C     ..................................................................... DA
      IF(TANSW.EQ.'DA') THEN
        IF(NUM.LE.0) GO TO 929
        CALL DSCTR
        DO N=1,NUM
          CALL DTED1(XST(1,N),YST(1,N),NST(N),PR(1,2))
        END DO
        GO TO 930
      END IF
C     ..................................................................... ST
      IF(TANSW.EQ.'ST') THEN
        IF(.NOT.FST) GO TO 928
        IF(NLIN.NE.9999) THEN
          CALL DTEM1(XST(1,NLIN),YST(1,NLIN))
          CALL DWRT('Track '//DTR//' replaced.')
          GO TO 930
        ELSE
          IF(NUM.EQ.9) THEN
            CALL DWRT('List full: track '//DT3(FLOAT(NST(1)))//
     &        ' deleted.')
            DO K=1,8
              CALL UCOPY(XST(1,K+1),XST(1,K),3)
              CALL UCOPY(YST(1,K+1),YST(1,K),3)
              NST(K)=NST(K+1)
            END DO
          ELSE
            NUM=NUM+1
            IF(NUM.EQ.9) THEN
              CALL DWR_BELL(1,.FALSE.)
              CALL DWRT('List full.')
            END IF
          END IF
          CALL DTEM1(XST(1,NUM),YST(1,NUM))
          NST(NUM)=NTR
          CALL DWRT('Track '//DTR//' stored.')
        END IF
        GO TO 930
      END IF
C     ..................................................................... CT
      IF(TANSW.EQ.'CT') THEN
        IF(NUM.LE.0) GO TO 929
        IF(NLIN.NE.9999) THEN
          IF(NLIN.NE.NUM) THEN
            DO K=NLIN+1,NUM
              CALL UCOPY(XST(1,K),XST(1,K-1),3)
              CALL UCOPY(YST(1,K),YST(1,K-1),3)
              NST(K-1)=NST(K)
            END DO
          END IF
          NUM=NUM-1
          CALL DWRT('Track '//DTR//' cleared.')
        ELSE
          CALL DWRT('Track '//DTR//' not found.')
        END IF
        GO TO 930
      END IF
C     ..................................................................... DT
      IF(TANSW.EQ.'DT') THEN
        IF(NUM.LE.0) GO TO 929
        IF(NLIN.NE.9999) THEN
          CALL DTED1(XST(1,N),YST(1,NLIN),NST(NLIN),PR(1,2))
        ELSE
          CALL DWRT('Track '//DTR//' is not in the list.')
        END IF
        GO TO 930
      END IF
C     .......................................................... wrong command
      CALL DWR_IC(TANSW)
      GO TO 936
  928 CALL DWRT('Visualize (and modify) track first ("DO")')
      GO TO 936
  929 CALL DWRT('List is empty.')
      GO TO 936
C     ...................................................... display and modify
C
C     ....................... I = IAREDO ?????????????????????    
  940 IF(TPICDP(ISTODS(5,IAREDO,IWUSDO)).NE.'YX'.OR.
     &  ISTODS(4,IAREDO,IWUSDO).LE.0) THEN
        NWI=0
        DO I=0,12
          IF(TPICDP(ISTODS(5,I,IWUSDO)).EQ.'YX'.AND.
     &      ISTODS(4,I,IWUSDO).GT.0) THEN
            NWI=NWI+1
            NXY=I
          END IF
        END DO
        IF(NWI.EQ.0) THEN
          CALL DWRT('Extrapolated tracks are drawn only on Y/X!')
          GO TO 936
        ELSE IF(NWI.GT.1) THEN
          CALL DWRT('Select window with Y/X picture.')
          GO TO 936
        END IF
        IAREDO=NXY
      END IF
      IF((.NOT.FDAT).AND.NLIN.EQ.9999) FDAT=.TRUE.
      CALL DTEM(NTR,FDAT,XST(1,NLIN),YST(1,NLIN))
      CALL DWRT('Fix Line or STore.')
      FST=.TRUE.
      GO TO 936
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DTEX
CH
      ENTRY DTEX(FLIST)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
      IF(FDAT) THEN
        FLIST=.FALSE.
      ELSE
        FLIST=.TRUE.
        IF(NUM.LE.0) THEN
          IF(FNRT) THEN
            CALL DTER(NUM,XST,YST,NST)
            FNRT=.FALSE.
          END IF
          IF(NUM.LE.0) THEN
            CALL DWRT('No tracks in extrapolation list.')
            RETURN
          END IF
        END IF
        PR(4,2)=-1.
        CALL DSCTR
        DO N=1,NUM
          CALL DTED1(XST(1,N),YST(1,N),NST(N),PR(1,2))
        END DO
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DTE0
CH
      ENTRY DTE0
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
      NUM=0
      FST=.FALSE.
      CALL DVETRN
      NTRKDP=0
      FNRT=.TRUE.
      END
*DK DTEM
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTEM
CH
      SUBROUTINE DTEM(ITR,FNEW,XU3,YU3)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION XYZ(3)
      DIMENSION HD(35),VD(35),IH(35),IV(35),NDSEG(2,2)
      DATA NDSEG/1,28,  31,5/
      DIMENSION HU3(3),VU3(3),XU3(3),YU3(3)
      DATA N2/2/,N5/5/
      LOGICAL FIN,FNEW
      EXTERNAL DTEMM
      DIMENSION COL(4)
      NTR=ITR
      CALL DSCTR
      IF(FNEW) THEN
        IF(NTR.NE.0) THEN
          CALL DVETR0(NTR,NUMP,XYZ)
          IF(NUMP.LT.6) NUMP=0
        ELSE
          NUMP=0
        END IF
        IF(NUMP.NE.0) THEN
          CALL DQSET(IAREDO,0.,0.)
          DO K=2,NUMP
            CALL DVETR(K,XYZ)
            K1=K-1
            CALL DQPOC(XYZ(1),XYZ(2),HD(K1),VD(K1),FIN)
          END DO
          NUMP=NUMP-1
          CALL DVETR(N2,XYZ)
          HU3(1)=XYZ(1)
          VU3(1)=XYZ(2)
          CALL DVETR(N5,XYZ)
          HU3(2)=XYZ(1)
          VU3(2)=XYZ(2)
          CALL DVETR(NUMP,XYZ)
          HU3(3)=XYZ(1)
          VU3(3)=XYZ(2)
        ELSE
C          CALL DGCURG(HDCUR,VDCUR)
C          CALL DQINV(IAREDO,HDCUR,VDCUR,HUCUR,VUCUR)
C          FI=ATAN2(VUCUR,HUCUR)
C          SF=SIN(FI)
C          CF=COS(FI)
C          DO K=1,3
C            HU3(K)=RU3(K)*CF
C            VU3(K)=RU3(K)*SF
C            CALL DQPOC(HU3(K),VU3(K),HD(K),VD(K),FIN)
C          END DO
C          NUMP=3
          NUMP=0
        END IF
      ELSE
        DO K=1,3
          HU3(K)=XU3(K)
          VU3(K)=YU3(K)
          CALL DQPOC(HU3(K),VU3(K),HD(K),VD(K),FIN)
        END DO
        NUMP=3
      END IF
      CALL DTEM0(HU3,VU3,HDC,VDC)
      NDSEG(2,1)=NUMP
      NUMPL=1
      IHC=HDC
      IVC=VDC
      CALL DGLINM(IHC,IVC,HD,VD,IH,IV,NAR,DTEMM,.FALSE.,NDSEG,NUMPL)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DTEMF
CH
      ENTRY DTEMF(COL)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
      IF(COL(4).EQ.1.) THEN
        CALL DGLEVL(IFIX(COL(2)))
      ELSE
        IF(NTR.GT.0) THEN
          IF(FVTRDC) CALL DGLEVL(NCTRDC(NTR))
        ELSE
          CALL DQLEVL(LCNCDD)
        END IF
      END IF
      CALL DGDRAW(NDSEG(2,1),HD,VD)
      END
*DK DTEMM
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTEMM
CH
      SUBROUTINE DTEM0(HUI3,VUI3,HDC0,VDC0)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION HD(35),VD(35),NDSEG(2,3)
      DIMENSION HUI3(3),VUI3(3),HU3(3),VU3(3),HD3(0:3),VD3(0:3),NP(2,3)
      DIMENSION HDSQ(31:35),VDSQ(31:35)
      DATA HD3(0)/2048./
      DIMENSION RU3(3)
      DATA RU3/236.,298.,529./
      DATA NP/2,3, 1,3, 1,2/,QC/.3/,DSLOW/4./,DDSQ/12./
      DATA NPO/6/
      CHARACTER *(*) TKBD
      LOGICAL FIN,FBUT(4),FSTRT,FROT
      HDSQ(31)=-DDSQ
      HDSQ(34)=-DDSQ
      HDSQ(35)=-DDSQ
      HDSQ(32)= DDSQ
      HDSQ(33)= DDSQ
      VDSQ(31)=-DDSQ
      VDSQ(32)=-DDSQ
      VDSQ(35)=-DDSQ
      VDSQ(33)= DDSQ
      VDSQ(34)= DDSQ
      CALL UCOPY(HUI3,HU3,3)
      CALL UCOPY(VUI3,VU3,3)
      DO K=1,3
        CALL DQPOC(HU3(K),VU3(K),HD3(K),VD3(K),FIN)
      END DO
      QPO=1./(NPO-2)
      NIN=NPO-1
      HDC0=0.5*(HMINDG(0)+HHGHDG(0))
      VDC0=0.5*(VMINDG(0)+VHGHDG(0))
      HD0 =HDC0
      VD0 =VDC0
      IHD0=HDC0
      IVD0=VDC0
      NBUT=0
      FSTRT=.TRUE.
      FROT=.TRUE.
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DTEMM
CH
      ENTRY DTEM1(HUI3,VUI3)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
      CALL UCOPY(HU3,HUI3,3)
      CALL UCOPY(VU3,VUI3,3)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DTEMM
CH
      ENTRY DTEMM(IHC,IVC,HD,VD,NDSEG,NUMPL,FBUT,TKBD)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
      IF(.NOT.FBUT(4)) RETURN ! DO NOT REACT ON KEYBOARD INPUT
      IF(FSTRT) THEN
        IF(NDSEG(2,1).GT.2) THEN
          FSTRT=.FALSE.
          GO TO 1
        END IF
        IF(FROT) THEN
          HDCUR=IHC
          VDCUR=IVC
          CALL DQINV(IAREDO,HDCUR,VDCUR,HUCUR,VUCUR)
          IF(HUCUR.EQ.0..AND.VUCUR.EQ.0.) RETURN
          FI=ATAN2(VUCUR,HUCUR)
          SF=SIN(FI)
          CF=COS(FI)
          DO K=1,3
            HU3(K)=RU3(K)*CF
            VU3(K)=RU3(K)*SF
            CALL DQPOC(HU3(K),VU3(K),HD3(K),VD3(K),FIN)
          END DO
          HD(1)=HD3(1)
          HD(2)=HD3(3)
          VD(1)=VD3(1)
          VD(2)=VD3(3)
          NUMPL=1
          NDSEG(2,1)=2
          IF(FBUT(1).AND.FBUT(2).AND.FBUT(3)) RETURN
          FROT=.FALSE.
        END IF
      END IF
      IF(.NOT.FBUT(1)) THEN
        IF(FBUT(2)) THEN
          NN=1
        ELSE
          NN=3
        END IF
      ELSE IF(.NOT.FBUT(2)) THEN
        NN=2
      ELSE IF(.NOT.FBUT(3)) THEN
        NN=3
      ELSE
        NBUT=0
        NUMPL=1
        RETURN
      END IF
      IF(NN.NE.NBUT) THEN
        NBUT=NN
        IHC=IHD0
        IVC=IVD0
        RETURN
      END IF
      DHD=(IHC-HD0)
      DVD=(IVC-VD0)
      IF((ABS(DHD)+ABS(DVD)).LE.DSLOW) THEN
        DHD=DHD*QC
        DVD=DVD*QC
      END IF
      IHC=IHD0
      IVC=IVD0
      HDN=HD3(NN)+DHD
      VDN=VD3(NN)+DVD
      CALL DQINV(IAREDO,HDN,VDN,HUN,VUN)
      N1=NP(1,NN)
      IF(HUN.EQ.HU3(N1).AND.VUN.EQ.VU3(N1)) RETURN
      N2=NP(2,NN)
      IF(HUN.EQ.HU3(N2).AND.VUN.EQ.VU3(N2)) RETURN
      HD3(NN)=HDN
      VD3(NN)=VDN
      HU3(NN)=HUN
      VU3(NN)=VUN
    1 HU21=HU3(2)-HU3(1)
      VU21=VU3(2)-VU3(1)
      HU32=HU3(3)-HU3(2)
      VU32=VU3(3)-VU3(2)
      FU21=ATAN2(VU21,HU21)
      FU32=ATAN2(VU32,HU32)
      RU32=SQRT(HU32*HU32+VU32*VU32)
      DURU=RU32*QPO
      QUFR=(FU32-FU21)/RU32
      CALL DQPOC(HU3(1),VU3(1),HD(  1),VD(  1),FIN)
      CALL DQPOC(HU3(2),VU3(2),HD(  2),VD(  2),FIN)
      CALL DQPOC(HU3(3),VU3(3),HD(NPO),VD(NPO),FIN)
      RU=0.
      DO N=3,NIN
        RU=RU+DURU
        FU=FU21+QUFR*RU
        HU=HU3(2)+RU*COS(FU)
        VU=VU3(2)+RU*SIN(FU)
        CALL DQPOC(HU,VU,HD(N),VD(N),FIN)
      END DO
      NDSEG(2,1)=NPO
      DO K=30,35
        HD(K)=HD3(NN)+HDSQ(K)
        VD(K)=VD3(NN)+VDSQ(K)
      END DO
      NUMPL=2
      END
*DK DTED1
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTED1
CH
      SUBROUTINE DTED1(HU3,VU3,NTR,COL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DATA NPO/6/
      DIMENSION HU3(3),VU3(3),VU(2),HU(2),COL(4)
      IF(COL(4).EQ.1.) THEN
        CALL DGLEVL(IFIX(COL(2)))
      ELSE
        IF(NTR.GT.0.AND.NTR.LE.IFIX(BNUMDB(2,FRFTDB)) ) THEN
          IF(FVTRDC) CALL DGLEVL(NCTRDC(NTR))
        ELSE
          CALL DQLEVL(LCNCDD)
        END IF
      END IF
      HU21=HU3(2)-HU3(1)
      VU21=VU3(2)-VU3(1)
      HU32=HU3(3)-HU3(2)
      VU32=VU3(3)-VU3(2)
      FU21=ATAN2(VU21,HU21)
      FU32=ATAN2(VU32,HU32)
      RU32=SQRT(HU32*HU32+VU32*VU32)
      QPO=1./(NPO-2)
      DURU=RU32*QPO
      QUFR=(FU32-FU21)/RU32
      CALL DQLIE(HU3(1),VU3(1))
      HU(1)=HU3(2)
      VU(1)=VU3(2)
      RU=0.
      DO N=3,NPO
        RU=RU+DURU
        FU=FU21+QUFR*RU
        HU(2)=HU3(2)+RU*COS(FU)
        VU(2)=VU3(2)+RU*SIN(FU)
        CALL DQLIE(HU,VU)
        HU(1)=HU(2)
        VU(1)=VU(2)
      END DO
      END
*DK DTER
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTER
CH
      SUBROUTINE DTER(NUM,XST,YST,NST)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *12 TFIL
      DIMENSION XS(3),YS(3),XST(3,9),YST(3,9),NST(9)
      NUM=0
      TFIL=TFILDC//'EXTR'
      CALL DGOPEN(NUNIDU,TFIL,2,*99,IER)
    1 READ(NUNIDU,1000,END=9,ERR=9) IRUN,IEVT,NS,(XS(J),YS(J),J=1,3)
 1000 FORMAT(2I9,I4,6F9.3)
      IF(IRUN.EQ.IRUNDE(1).AND.IEVT.EQ.IEVTDE(1)) THEN
        NUM=NUM+1
        IF(NUM.GT.9) NUM=1
        CALL UCOPY(XS,XST(1,NUM),3)
        CALL UCOPY(YS,YST(1,NUM),3)
        NST(NUM)=NS
      END IF
      GO TO 1
    9 CLOSE(UNIT=NUNIDU)
      RETURN
   99 CALL DWRT(TFIL//' does not exist.')
      END
*DK DTEW
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTEW
CH
      SUBROUTINE DTEW(NUM,XST,YST,NST)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION XST(3,9),YST(3,9),NST(9)
      CALL DGOPEN(NUNIDU,TFILDC//'EXTR',*99,IER)
    1 DO N=1,NUM
        WRITE(NUNIDU,1000) IRUNDE(1),IEVTDE(1),NST(N),
     &    (XST(J,N),YST(J,N),J=1,3)
 1000   FORMAT(2I9,I4,6F9.3)
      END DO
      CLOSE(UNIT=NUNIDU)
      RETURN
 99   CALL DWRT('File '//TFILDC//'EXTR could not be opened.')
      END
*DK DTSTOB
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTSTOB
CH
      SUBROUTINE DTSTOB(TIN,NIN,TOUT,NOUT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     Convert small to big letters
      CHARACTER *(*) TIN,TOUT
      IF(NIN.EQ.0) THEN
         L=LENOCC(TIN)
         NOUT=L
      ELSE
         L=NIN
      END IF
      TOUT=TIN
      DO 700 K=1,L
         I=ICHAR(TIN(K:K))
         IF(I.GE.97.AND.I.LE.122) TOUT(K:K)=CHAR(I-32)
  700 CONTINUE
      END
*DK DTX
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTX
CH
      SUBROUTINE DTX
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  ADD TEXT INTERACTIVELY
C ---------------------------------------------------------------------
C INVOKED BY TANSW.EQ.'TX'  (DTX)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *5 TAR
      CHARACTER *1 TINDX,TIND,TW,TYES
      CHARACTER *2 TL(6),TYPC
      DATA TL/'S1','S2','S3','S4','S5','S6'/
C             12pt,14pt,19pt,24pt,28pt,38pt
      PARAMETER (NST=100,GST=100.)
      CHARACTER *150 TCUR,TX(NST)
      DATA TCUR/'1-3-5-7-9 1-3-5-7-9 1-3-5-7-9 1-3-5-7-9 1-3-5-7-9'/
      CHARACTER *300 TWORK
      DIMENSION HT(NST),VT(NST),NC(NST),LS(NST),ND(NST),AT(NST)
C     CHANGE MPAT EVERYWHERE!!
      PARAMETER(MPR=15,MPAT=7)
      CHARACTER *2 TANSW,TP(MPR),DT2,TA(MPAT)
      DATA TA/'A1','A2','A3','A4','A5','A6','A7'/
      DATA TP/'LI','CO','DG','EC','HC','WI','PR','AT','SZ','DA',
     &        'HH','HD','VV','VD','CF'/
      DIMENSION PR(4,MPR),PA(4,MPAT)
      DATA PR/1.,  1.,   0., 0.,
     &       -1.,  8.,  32., 0.,
     &        0.,  0., 359., 0.,
     &        0.,  1., 999., 0.,
     &        0.,  1., 999., 0.,
     &        0.,  1., 999., 0.,
     &        0.,100., 999., 0.,
     &        0.,  2.,9999.,-1.,
     &        1.,  1.,   5., 0.,
     &        1.,  4.,   9.,-1.,
     &        1.,100.,1024., 0.,
     &     -999.,330.,1024.,-1.,
     &        1.,100.,1024., 0.,
     &     -999.,-17.,1024.,-1.,
     &       -1.,  8.,  31., 1./
      DATA PA/
     &  0.,-1.,9.,0.,
     &  0.,-1.,9.,0.,
     &  0.,-1.,9.,0.,
     &  0.,-1.,9.,0.,
     &  0.,-1.,9.,0.,
     &  0.,-1.,9.,0.,
     &  0.,-1.,9.,0./
      DATA L/5/,NLIN/0/
      DIMENSION HSC(20),VSC(20),IHSC(20),IVSC(20),NDSEG(2,19)
      DIMENSION HSCST(20,5),VSCST(20,5),NDSST(2,19,5),NMPST(5)
      DIMENSION ATR(MPAT),IP(2)
      DATA IP/4,4/,AAR/30./,C2/2./,C3/3./
      CHARACTER *40 TSCAL
      DATA TSCAL/'Scale on wrong window; select right one.'/
      DATA NAR/-1/,LTXMX/78/
      DIMENSION LCTAB(9)
      CHARACTER *10 TTAB(10)
      DATA DVTAB/12./,DH1/0./,DV1/2./,H,V/2*100./
      EXTERNAL DTXSCH,DTXSCV,DTXPL,DTXRS,DTXGR
      LOGICAL CHG,FTXT,FSP,FYES,FMC
      DATA FMC/.FALSE./,FSP/.FALSE./
      CHARACTER *10 TRR
      DATA TRR/'DO:MC'/
      CHARACTER *49 T1,T2,T3,T4
C              123456789 123456789 123456789 123456789 123456789
      DATA T1/'TX:W1 12 lines. DO:store+fix text        1234.567'/
      DATA T2/'LI_12  SS  CO_123  DG_123  SZ_1 DA$1  AT$1234.567'/
      DATA T3/'Window scale of EC_123 HC_123 WI_123 PR_123'/
      DATA T4/'HH_1234  HD$1234   VV_1234  VD$1234  CF$12'/
      DATA JDEB/0/
      DIMENSION HPC(0:121),VPC(0:121)
      IAOLD=IAREDO
      FRNL=FRNLDU
      FRNLDU=0.
      SCALE=0.
      PR(2,1)=NLIN
  930 WRITE(T1(7:8),'(I2)') NLIN
      CALL DWR_HL_AR(T1)
C     IF(NLIN.EQ.0)
C    &  TCUR='1-3-5-7-9 1-3-5-7-9 1-3-5-7-9 1-3-5-7-9 1-3-5-7-9'
      LTX=MIN(LTXMX,LENOCC(TCUR))
      CALL DWRT(TCUR(1:LTX))
      T2( 8: 9)=TL(L)
      CALL DTNOSL(PR,8,41,T2)
      WRITE(T2(42:49),1932) PR(2,8)
1932  FORMAT(F8.3)
      CALL DTYPT('TYPE',' ',1,MPR,PR,TP,T2)
      IF(FSP) CALL DTYPT('TYPE',' ',1,MPR,PR,TP,T3)
      PR(2,11)=H
      PR(2,13)=V
      CALL DTYPT('TYPE',' ',1,MPR,PR,TP,T4)
      IF(FMONDT) CALL DWRT('White CO=0 ; black CO=8.')
  936 CHG=.FALSE.
      PR(4,1)=-1.
      PR(4,12)=-1.
      PR(4,14)=-1.
      CALL DOPER(1,0,
     &  1,MPAT,TA,PA,
     &  1,MPR,TP,PR,
     &  NEXEC,CHG,TANSW)
      IF(FMC.AND..NOT.CHG.AND.NEXEC.EQ.3) THEN
        CALL DGINMA(TRR)
        GO TO 936
      END IF
      FMC=.FALSE.
      IF(PR(4,1).EQ.1.) THEN
        I=PR(2,1)
        IF(LS(I).NE.0) THEN
          L=LS(I)
          PR(2,2)=NC(I)
          PR(2,3)=ND(I)
          PR(2,8)=ABS(AT(I))
          PR(4,8)=SIGN(1.,AT(I))
          TCUR=TX(I)
          H=HT(I)
          V=VT(I)
        END IF
      ELSE
        IF(PR(4,12).EQ.1.) PR(2,11)=PR(2,11)+PR(2,12)
        IF(PR(4,14).EQ.1.) PR(2,13)=PR(2,13)+PR(2,14)
        H=PR(2,11)
        V=PR(2,13)
      END IF
      CALL DTXDEA(PR(2,8),ATR)
      PR(2,8)=0.
      F10=1000.
      DO K=1,MPAT
        IF(PA(2,K).GE.0.) THEN
          ATR(K)=PA(2,K)
          PA(2,K)=-1.
        END IF
        PR(2,8)=PR(2,8)+ATR(K)*F10
        F10=0.1*F10
      END DO
      IF(TANSW.EQ.'PI') CALL DPOS(TANSW,FDUM,TDUM,NEXEC,CHG)
      GO TO (910,920,930,940),NEXEC
  910 FRNLDU=FRNL
      IF(IAREDO.GT.12) IAREDO=IAOLD
      RETURN
  920 FMC=.FALSE.
      IF(TANSW.EQ.'WH') THEN
        IAREDO=13
        GO TO 930
      END IF
      IF(TANSW.EQ.'WC') THEN
        IAREDO=14
        GO TO 930
      END IF
      IF(TANSW.EQ.'MC') GO TO 922
      DO LL=1,6
        IF(TANSW.EQ.TL(LL)) THEN
          L=LL
          GO TO 930
        END IF
      END DO
      IF(TANSW.EQ.'IN') THEN
  921   CALL DWRT_SETUP('END CHARACTER= ')
        CALL DWRT('text  = ')
        CALL DGETLM(TWORK,LENTC,150)
        CALL DWRT_SETUP('DEFAULT')
        IF(LENTC.LE.0) GO TO 930
        IF(TWORK.EQ.' ') GO TO 930
        TCUR=TWORK
  922   CALL DTXDEA(PR(2,8)*PR(4,8),ATR)
        CALL DTXFTB(ATR(6),TCUR,TWORK)
        CALL DWRT('Type <CR> to Stop and to reenable terminal + help.')
        CALL DWRT_END(0)
        CALL DGTXTM(L,TWORK,PR(2,3),H,V,LENOCC(TWORK))
        FMC=.TRUE.
        GO TO 930
      END IF
      IF(TANSW.EQ.'CA') THEN
        CALL DTYANS('Clear all? Yes,No=<CR>','NY',Nansw)
        IF(NANSW.NE.2) GO TO 930
        NAST=0
        NLIN=0
        PR(2,1)=0.
        PR(3,1)=0.
        CALL DWRT(' All text cleared.')
        GO TO 936
      END IF
      IF(TANSW.EQ.'CL') THEN
        LC=PR(2,1)
        IF(LC.GT.0.AND.LC.LE.NLIN) THEN
          DO I=LC,NST-1
            LS(I)=LS(I+1)
            ND(I)=ND(I+1)
            NC(I)=NC(I+1)
            HT(I)=HT(I+1)
            VT(I)=VT(I+1)
            AT(I)=AT(I+1)
            TX(I)=TX(I+1)
          END DO
          NLIN=NLIN-1
          CALL DWRT(' Line' //DT2(PR(2,1))//' cleared.')
          LC=MIN(LC,NLIN)
          PR(2,1)=LC
          PR(3,1)=NLIN
          TCUR=TX(LC)
        ELSE
          CALL DWRT('Non existent line')
        END IF
        GO TO 930
      END IF
      IF(TANSW.EQ.'LA') THEN
        IF(NLIN.LE.0) THEN
          CALL DWRT(' Nothing stored')
        ELSE
          CALL DWRT(' # W S Deg Co Attribute Text')
          DO I=1,NLIN
            IF(I.NE.1.AND.MOD(I,5).EQ.1) THEN
              CALL DTYANS('Type <CR> to continue or anything to stop',
     &          'S',LANSW)
              IF(LANSW.NE.0) GO TO 930
              CALL DWRT(' # W S Deg Co Attribute Text')
            END IF
            CALL DTXWIN(HT(I),VT(I),TW)
            LTX=MIN(LTXMX-23,LENOCC(TX(I)))
            IF(AT(I).GT.0.) THEN
              WRITE(TXTADW,1020) I,TW,LS(I),ND(I),NC(I),AT(I),
     &          TX(I)(1:LTX)
 1020         FORMAT(I2,1X,A,I2,I4,I3,F9.3,1X,A)
              CALL DWRC
            ELSE
              WRITE(TXTADW,1021) I,TW,LS(I),ND(I),NC(I),
     &          TX(I)(1:LTX)
 1021         FORMAT(I2,1X,A,I2,I4,I3,10X,A)
              CALL DWRC
            END IF
          END DO
        END IF
        GO TO 930
      END IF
      IF(TANSW.EQ.'RC') THEN
        NCOL=PR(2,2)
        CALL UFILL(NC,1,NST,NCOL)
        CALL DWRT('All colors set to '//DT2(PR(2,2)))
        GO TO 924
      END IF
      IF(TANSW.EQ.'RA') THEN
  924   DO I=1,NLIN
          IF(NC(I).GE.0.)
     &      CALL DTXFIX('SHA',NC(I),LS(I),AT(I),TX(I),FLOAT(ND(I))
     &      ,HT(I),VT(I))
        END DO
        DO I=1,NLIN
          IF(NC(I).GE.0.)
     &      CALL DTXFIX('TXT',NC(I),LS(I),AT(I),TX(I),FLOAT(ND(I))
     &      ,HT(I),VT(I))
        END DO
        GO TO 936
      END IF
      IF(TANSW.EQ.'WF') THEN
        CALL DWRT('Write file DALI_D#.DTX. Type 1 letter index')
        CALL DGETLN(TIND,NINDX,1)
        IF(NINDX.LE.0) GO TO 930
        CALL DGOPEN(NUNIDU,TFILDC//'DTX',1,*991,IER)
        DO I=1,NLIN
          IF(NC(I).GE.0.) THEN
            IHT=HT(I)
            IVT=VT(I)
            LTX=LENOCC(TX(I))
            WRITE(NUNIDU,1024) TIND,NC(I),LS(I),ND(I),AT(I),IHT,IVT,
     &        TX(I)(1:LTX)
          END IF
 1024     FORMAT(1X,A,2(I2,','),I3,',',F9.3,',',2(I4,','),1X,A)
        END DO
        IF(NDSEG(2,1).NE.0) THEN
          CALL DTYANS('Store polyLine or Area or Not?','LA',Nansw)
          IF(NANSW.GT.0) THEN
            INCO=PR(2,2)
            DO K=1,NDSEG(2,1)
              WRITE(NUNIDU,1024) INCO,-1,0,IHSC(K),IVSC(K),'area'
            END DO
          END IF
        END IF
        CLOSE(UNIT=NUNIDU)
        CALL DWRT('Text,lines/area saved on DALI_D#.DTX')
        GO TO 936
      END IF
      IF(TANSW.EQ.'RF') THEN
C       .. BLANK,T_INDEX(1 letter),BLANK,
C       .. I_COLOR,I_SIZE,I_DEGREE,ATTRIBUTE,I_HORIZONTAL,I_VERTICAL,BLANK,TEXT
C       .......... IF I_SIZE >0 TEXT, ELSE AREA
C       .......... IF I_DEGREE >0 ABSOLUTE POSITIONS, ELSE RELATIVE POSITIONS
C       ... & AT THE END OF TEXT: TEXT CONTINUES ON NEXT LINE THEN:
C       .......... 1.LETTER = BLANK
        CALL DWRT('Read file DALI_D#.DTX. Type 1 letter index')
        CALL DGETLN(TIND,NINDX,1)
        IF(NINDX.LE.0) GO TO 930
        NLIR=0
        NDSG=0
        TAR=' '
        CALL DGOPEN(NUNIDU,TFILDC//'DTX',2,*991,IER)
        DO I=1,NST
  926     READ(NUNIDU,1025,ERR=926,END=925)
     &      TINDX,INCO,INSZ,INDG,AA,IHT,IVT,TWORK
          IF(TINDX.NE.TIND) GO TO 926
 1025     FORMAT(1X,A,I3,2(1X,I2),I3,1X,F2.0,2(1X,I4),1X,A)
          IF(INSZ.GT.0) THEN
            NC(I)=INCO
            LS(I)=INSZ
            AT(I)=AA
            IF(I.GT.1.AND.INDG.LT.0) THEN
              ND(I)=ND(I-1)
              HT(I)=HT(I-1)+IHT
              VT(I)=VT(I-1)+IVT
            ELSE
              ND(I)=INDG
              HT(I)=IHT
              VT(I)=IVT
            END IF
            NLIR=NLIR+1
            TX(I)=TWORK
            CALL DWRT(TWORK)
            LWORK=LENOCC(TWORK)
            IF(TWORK(LWORK:LWORK).EQ.'&') THEN
              READ(NUNIDU,1026,END=925) TWORK
 1026         FORMAT(1X,A)
              TX(I)=TX(I)(1:LWORK-1)//TWORK(1:150+1-LWORK)
            END IF
          ELSE
            NDSG=NDSG+1
            IHSC(NDSG)=IHT
            IVSC(NDSG)=IVT
            HSC (NDSG)=IHT
            VSC (NDSG)=IVT
            TAR=TWORK(1:5)
          END IF
  928   END DO
  925   CALL DWRT(DT2(FLOAT(NLIR))//
     &    ' lines of text from DALI_D#,DTX  '//TAR)
        IF(NLIR.GT.0) THEN
          NLIN=NLIR
          PR(2,1)=NLIN
          PR(3,1)=NLIN
        END IF
        IF(NDSG.GT.0) THEN
          NDSEG(1,1)=NDSG
          NDSEG(2,1)=NDSG
          NUMPA=1
        END IF
        CLOSE(UNIT=NUNIDU)
        GO TO 936
      END IF
      IF(TANSW.EQ.'RP') THEN
        CALL DGOPEN(NUNIDU,TFILDC//'PIC',2,*991,IER)
        NCORN=0
  210   READ(NUNIDU,*,END=211,ERR=210) TYPC,IN1,IN2
c 1210   FORMAT(1X,A2,2I)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC LINUX INCOMPATIBLE
        IF(TYPC.EQ.'ST') THEN
          NCORN=0
        ELSE IF(TYPC(1:1).EQ.'P'.OR.TYPC(1:1).EQ.'D') THEN
          NCORN=NCORN+1
          IF(TYPC(1:1).EQ.'P') THEN
            HPC(NCORN)=IN1
          ELSE IF(TYPC(1:1).EQ.'D') THEN
            HPC(NCORN)=HPC(NCORN-1)+IN1
          END IF
          IF(TYPC(2:2).EQ.'P') THEN
            VPC(NCORN)=IN2
          ELSE IF(TYPC(2:2).EQ.'D') THEN
            VPC(NCORN)=VPC(NCORN-1)+IN2
          END IF
        ELSE IF(TYPC.EQ.'CL') THEN
          NCORN=NCORN+1
          HPC(NCORN)=HPC(1)
          VPC(NCORN)=VPC(1)
        ELSE IF(TYPC.EQ.'RP') THEN
          DHPC=IN1-HPC(1)
          DVPC=IN2-VPC(1)
          DO K=1,NCORN
            HPC(K)=HPC(K)+DHPC
            VPC(K)=VPC(K)+DVPC
          END DO
        ELSE IF(TYPC.EQ.'RD') THEN
          DHPC=IN1
          DVPC=IN2
          DO K=1,NCORN
            HPC(K)=HPC(K)+DHPC
            VPC(K)=VPC(K)+DVPC
          END DO
        ELSE IF(TYPC.EQ.'CI') THEN
          RCIRH=IN1
          RCIRV=IN2
          HCIR=HPC(NCORN)
          VCIR=VPC(NCORN)
          A=0.
          DO N=1,121
            HPC(N)=HCIR+RCIRH*COSD(A)
            VPC(N)=VCIR+RCIRV*SIND(A)
            A=A+3.
          END DO
          NCORN=121
        ELSE IF(TYPC.EQ.'AR') THEN
          ICPIC=IN1
          CALL DGLEVL(ICPIC)
          CALL DGAREA(NCORN,HPC(1),VPC(1))
        ELSE IF(TYPC.EQ.'LI') THEN
          DLIN=DLINDD
          DLINDD=IN2
          ICPIC=IN1
          CALL DGLEVL(ICPIC)
          CALL DGDRAW(NCORN,HPC(1),VPC(1))
          DLINDD=DLIN
        END IF
        GO TO 210
  211   CALL DGCHKX
        CLOSE(UNIT=NUNIDU)
        GO TO 930
      END IF
      IF(TANSW.EQ.'RL') THEN
        I=PR(2,1)
        IF(I.LE.0.OR.I.GT.NLIN) GO TO 990
        NC(I)=PR(2,2)
        PR(2,3)=ND(I)
C       PR(2,8)=ABS(AT(I))
C       PR(4,8)=SIGN(1.,AT(I))
        L=LS(I)
        TCUR=TX(I)
        H=HT(I)
        V=VT(I)
        GO TO 945
      END IF
      IF(TANSW.EQ.'EL') THEN
        I=PR(2,1)
        IF(I.LE.0.OR.I.GT.NLIN) GO TO 990
        NC(I)=PR(2,2)
        PR(2,3)=ND(I)
C       PR(2,8)=ABS(AT(I))
C       PR(4,8)=SIGN(1.,AT(I))
        L=LS(I)
        TCUR=TX(I)
        H=HT(I)
        V=VT(I)
        NCOL=PR(2,2)
        ATC=PR(2,8)*PR(4,8)
        CALL DTXFIX('CLR',NCOL,L,ATC,TCUR,PR(2,3),H,V)
        GO TO 930
      END IF
      IF(TANSW.EQ.'SC') THEN
        FSP=.TRUE.
        GO TO 930
      END IF
      IF(TANSW.EQ.'SE') THEN
        SCALE=SCALDS(IAREDO,MSECDS)*PR(2,4)
        GO TO 110
      END IF
      IF(TANSW.EQ.'SH') THEN
        SCALE=SCALDS(IAREDO,MSHCDS)*PR(2,5)
        GO TO 110
      END IF
      IF(TANSW.EQ.'SW') THEN
        SCALE=SCALDS(IAREDO,MSWIDS)*PR(2,6)
        GO TO 110
      END IF
      IF(TANSW.EQ.'SP') THEN
        CALL DQSET(IAREDO,0.,0.)
        IF(PR(2,3).EQ.0..OR.PR(2,3).EQ.180.) THEN
          SCALE=PR(2,7)*AHSCDQ
        ELSE IF(PR(2,3).EQ.0..OR.PR(2,3).EQ.270.) THEN
          SCALE=PR(2,7)*BVSCDQ
        END IF
  110   FSP=.TRUE.
        IF(SCALE.EQ.0.) GO TO 929
        NUMPA=0
        IHC=HHGHDG(IAREDO)-2.*SCALE
        IVC=VHGHDG(IAREDO)-2.*SCALE
        IF(PR(2,3).EQ.0..OR.PR(2,3).EQ.180.) THEN
          CALL DTXSC0(SCALE)
          CALL DGLINM(IHC,IVC,HSC,VSC,IHSC,IVSC,IAR,DTXSCH,
     &      .FALSE.,NDSEG,NUMPL)
        ELSE IF(PR(2,3).EQ.90..OR.PR(2,3).EQ.270.) THEN
          CALL DTXSC0(SCALE)
          CALL DGLINM(IHC,IVC,HSC,VSC,IHSC,IVSC,IAR,DTXSCV,
     &      .FALSE.,NDSEG,NUMPL)
        ELSE
          CALL DWRT('Set DG to 0, 90, 180 or 270.')
          GO TO 936
        END IF
        CALL DPOAR(FLOAT(IHC),FLOAT(IVC),NAR)
        IF(NAR.EQ.IAREDO) THEN
          CALL DWRT(' FS : Fix scale at selected position!')
        ELSE
          CALL DWRT(TSCAL)
          NAR=-1
        END IF
        GO TO 936
      END IF
      IF(TANSW.EQ.'FS'.AND.SCALE.NE.0.) THEN
        IF(NAR.GE.0) THEN
          CALL DGLINF(HSC,VSC,IHSC,IVSC,NDSEG,NUMPL,IFIX(PR(2,2)))
          GO TO 930
        ELSE
          CALL DWRT(TSCAL)
          GO TO 936
        END IF
      END IF
C     .......................................................... COLOUR TABLES
      IF(TANSW.EQ.'TL') THEN
        CALL DSCRSC(PLSDDB,NROW,NLET,TTAB,LCTAB)
        GO TO 300
      END IF
      IF(TANSW.EQ.'TE') THEN
        CALL DSCRSC(ESDADB,NROW,NLET,TTAB,LCTAB)
        GO TO 300
      END IF
      IF(TANSW.EQ.'TH') THEN
        CALL DSCRSC(HSDADB,NROW,NLET,TTAB,LCTAB)
  300   IF(NROW.LE.0) GO TO 936
        CALL DTXRS0(NROW,NLET,DVTAB)
        CALL DGLINM(IHC,IVC,HSC,VSC,IHSC,IVSC,IAR,DTXRS,
     &    .FALSE.,NDSEG,NUMPA)
        CALL DWRT('Fix Table ("FT") or fill area ("FA").')
        GO TO 936
      END IF
      IF(TANSW.EQ.'FT') THEN
        HTAB=HSC(1)+DH1
        VTAB=VSC(1)+DV1
        DO K=1,NROW
          CALL DGLEVL(LCTAB(K))
          CALL DGTEXT(HTAB,VTAB,TTAB(K),NLET)
          VTAB=VTAB+DVTAB
        END DO
        GO TO 936
      END IF
      IF(TANSW.EQ.'PL') THEN
        NAR=-1
        CALL DTXPL0(1)
        CALL DGLINM(IHC,IVC,HSC,VSC,IHSC,IVSC,IAR,DTXPL,
     &    .FALSE.,NDSEG,NUMPA)
        CALL DWRT('Fix lines ("FL") or fill area ("FA").')
        GO TO 936
      END IF
      IF(TANSW.EQ.'ST'.AND.NAST.LT.5) THEN
        NAST=NAST+1
        NMPST(NAST)=NUMPA
        CALL UCOPY(HSC  ,HSCST(1,  NAST),20)
        CALL UCOPY(VSC  ,VSCST(1,  NAST),20)
        CALL UCOPY(NDSEG,NDSST(1,1,NAST),38)
        CALL DWRT(TL(NAST))
        GO TO 936
      END IF
      IF(TANSW.EQ.'PA') THEN
        NAR=-1
        CALL DTXPL0(-1)
        CALL DGLINM(IHC,IVC,HSC,VSC,IHSC,IVSC,IAR,DTXPL,
     &    .FALSE.,NDSEG,NUMPA)
        DO N=1,NUMPA
          NDSEG(2,N)=-NDSEG(2,N)
        END DO
        CALL DWRT('Fix lines ("FL") or fill area ("FA").')
        GO TO 936
      END IF
      IF(TANSW.EQ.'CI') THEN
        NAR=-1
        CALL DTXGR0
        CALL DGLINM(IHC,IVC,HSC,VSC,IHSC,IVSC,IAR,DTXGR,
     &    .FALSE.,NDSEG,NUMPA)
        GO TO 936
      END IF
      IF(TANSW.EQ.'FL'.AND.NUMPA.NE.0) THEN
        IF(JDEB.EQ.0) THEN
          DLINDD=IFIX(PR(2,9))
          IF(PR(4,10).GT.0.) THEN
            IP(1)=PR(2,10)
            CALL DGDASH(2,IP)
          END IF
          CALL DGLEVL(IFIX(PR(2,2)))
          CALL DGDRAW(NDSEG(2,1),HSC,VSC)
          CALL DGDASH(0,0)
          DLINDD=PDCODD(2,LITRDD)
        ELSE
          CALL DGLINF(HSC,VSC,IHSC,IVSC,NDSEG,NUMPA,IFIX(PR(2,2)))
        END IF
        GO TO 936
      END IF
      IF(TANSW.EQ.'AR'.AND.NUMPA.NE.0) THEN
        DLINDD=IFIX(PR(2,9))
        IF(DLINDD.EQ.0.) DLINDD=1.
        CALL DGLEVL(IFIX(PR(2,2)))
        ALIN=DATN2D(VSC(2)-VSC(1),HSC(2)-HSC(1))
        CAR=C3+C2*L
        DO K=1,2
          AAR=-AAR
          AARW=ALIN+AAR
          VSC(2)=VSC(1)+CAR*SIND(AARW)
          HSC(2)=HSC(1)+CAR*COSD(AARW)
          CALL DGDRAW(2,HSC,VSC)
        END DO
        DLINDD=PDCODD(2,LITRDD)
        GO TO 936
      END IF
      IF(TANSW.EQ.'FA'.AND.NUMPA.NE.0) THEN
        CALL DGLEVL(IFIX(PR(2,2)))
        CALL DGAREA(NDSEG(2,1),HSC,VSC)
        GO TO 936
      END IF
      IF(TANSW.EQ.'CW') THEN
        IARE=IAREDO
        GO TO 927
      END IF
      IF(TANSW.EQ.'CH') THEN
        IARE=13
  927   CALL DQCL(IARE)
        CALL DGLEVL(IFIX(PR(2,2)))
        CALL DGRAR (HMINDG(IARE),VMINDG(IARE),HHGHDG(IARE),VHGHDG(IARE))
        IF(PR(4,15).GT.0.) THEN
          IF(PR(2,15).GE.0.) THEN
            CALL DGLEVL(IFIX(PR(2,15)))
          ELSE
            CALL DQLEVL(ICFRDD)
          END IF
          CALL DQDAR(HMINDG(IARE),VMINDG(IARE),
     &               HHGHDG(IARE),VHGHDG(IARE))
        END IF
        CALL DGEXEC
        GO TO 936
      END IF
      IF(TANSW.EQ.'FR') THEN
        CALL DQLEVL(ICFRDD)
        CALL DQDAR(HMINDG(IARE),VMINDG(IARE),HHGHDG(IARE),VHGHDG(IARE))
      END IF
      IF(TANSW.EQ.'DH') THEN
        IFULDB=1
        CALL DQTIT(IFULDB)
        GO TO 936
      END IF
      IF(TANSW.EQ.'DC') THEN
        CALL DQFWAF(IFIX(PR(2,2)))
        GO TO 936
      END IF
      CALL DAREA('D',TANSW,0,12,IAREDO,FYES)
      IF(FYES) THEN
        NAR=IAREDO
        CALL DPCEAR(NAR)
        IAREDO=NAR
        GO TO 936
      END IF
  929 CALL DWR_IC(TANSW)
      GO TO 936
  940 IF(NLIN.GE.NST) THEN
        CALL DWRT('Too many lines to store. Clear.')
      ELSE
        NLIN=NLIN+1
        PR(2,1)=NLIN
        PR(3,1)=NLIN
        ND(NLIN)=PR(2,3)
        NC(NLIN)=PR(2,2)
        LS(NLIN)=L
        AT(NLIN)=PR(2,8)*PR(4,8)
        HT(NLIN)=H
        VT(NLIN)=V
        TX(NLIN)=TCUR
        IF(NLIN.EQ.NST) CALL DWRT('Buffer is full.')
      END IF
  945 NCOL=PR(2,2)
      ATC=PR(2,8)*PR(4,8)
      CALL DTXFIX('ALL',NCOL,L,ATC,TCUR,PR(2,3),H,V)
      GO TO 930
  990 CALL DWRT('Line '//DT2(PR(2,1))//' is empty.')
      GO TO 930
  991 CALL DWRT('File cannot be opened')
      GO TO 936
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DTXCA
CH
      ENTRY DTXCA
CH
CH --------------------------------------------------------------------
CH
      NLIN=0
      PR(2,1)=0.
      PR(3,1)=0.
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DTXRA
CH
      ENTRY DTXRA(TYES,FTXT)
CH
CH --------------------------------------------------------------------
CH
      FTXT=.FALSE.
      IF(NLIN.EQ.0) THEN
        IF(TYES.EQ.' ') CALL DWRT('No text stored.')
        RETURN
      ELSE
        IF(TYES.EQ.' ') THEN
          GLIN=NLIN
          CALL DTYANS(
     &      'Text ( '//DT2(GLIN)//' lines ) to be stored? Y/N',
     &      'YN',IANSW)
          IF(IANSW.NE.1) RETURN
        END IF
        CALL DGCOME('Drawing text')
        DO I=1,NLIN
          IF(NC(I).GE.0.AND.AT(I).GT.0.)
     &      CALL DTXFIX('SHA',NC(I),LS(I),AT(I),TX(I),FLOAT(ND(I)),
     &      HT(I),VT(I))
        END DO
        DO I=1,NLIN
          IF(NC(I).GE.0.)
     &      CALL DTXFIX('TXT',NC(I),LS(I),AT(I),TX(I),FLOAT(ND(I)),
     &      HT(I),VT(I))
        END DO
        FTXT=.TRUE.
      END IF
      END
*DK DTXFIX
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTXFIX
CH
      SUBROUTINE DTXFIX(TMOD,NC,LS,AT,TX,DG,HT,VT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     ..................... FILL BLANKS INTO TEXT AND DRAW WITH BLACK SHADOW
      CHARACTER *(*) TX,TMOD
      CHARACTER *300 TW
      DIMENSION ATR(7)
      DATA NB/1/,NW/8/
      IF(AT.LE.0.) THEN
        CALL DGTXTF(NC,LS,TX,DG,HT,VT,LENOCC(TX))
      ELSE
        CALL DTXDEA(AT,ATR)
        IF(TMOD.EQ.'CLR') THEN
          N1=NC
        ELSE
          IF     (ATR(7).EQ.0.) THEN
C           ....................................  SHADOW BLACK, LETTER COLOR
            N1=NB
            N2=NC
          ELSE IF(ATR(7).EQ.1.) THEN
C           ....................................  SHADOW COLOR, LETTER BLACK
            N1=NC
            N2=NB
          ELSE IF(ATR(7).EQ.2.) THEN
C           ....................................  SHADOW WHITE, LETTER COLOR
            N1=NW
            N2=NC
          ELSE
C           ....................................  SHADOW COLOR, LETTER WHITE
            N1=NC
            N2=NW
          END IF
        END IF
        CALL DTXFTB(ATR(6),TX,TW)
        IF(TMOD.EQ.'SHA'.OR.TMOD.EQ.'ALL'.OR.TMOD.EQ.'CLR') THEN
          IF     (ATR(1).EQ.9.) THEN
            DO HV=-ATR(3),ATR(4)
              CALL DGTXTF(N1,LS,TW,DG,HT+HV,VT+HV,LENOCC(TW))
            END DO
          ELSE IF(ATR(2).EQ.9.) THEN
            DO HV=-ATR(3),ATR(4)
              CALL DGTXTF(N1,LS,TW,DG,HT+HV,VT-HV,LENOCC(TW))
            END DO
          ELSE
            DO V=VT-ATR(1),VT+ATR(2)
              DO H=HT-ATR(3),HT+ATR(4)+ATR(5)
                CALL DGTXTF(N1,LS,TW,DG,H,V,LENOCC(TW))
              END DO
            END DO
          END IF
        END IF
        IF(TMOD.EQ.'TXT'.OR.TMOD.EQ.'ALL') THEN
          DO H=HT,HT+ATR(5)
            CALL DGTXTF(N2,LS,TW,DG,H,VT,LENOCC(TW))
          END DO
        END IF
      END IF
      END
*DK DTXFTB
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTXFTB
CH
      SUBROUTINE DTXFTB(AT,TX,TW)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C  FILL TEXT WITH BLANKS
C
      CHARACTER *(*) TX,TW
      IF(AT.LE.0.) THEN
        TW=TX
      ELSE
        IDA=AT+1
        ID=IDA
        LT=LENOCC(TX)
        TW=' '
        I=1
        DO L=1,LT
          IF(TX(L:L).EQ.'\') ID=1
          IF(TX(L:L).EQ.' ') ID=IDA
          TW(I:I)=TX(L:L)
          I=I+ID
          IF(I.GT.300) THEN
            CALL DWRT('Text filled with blanks is too long >300.')
            RETURN
          END IF
        END DO
      END IF
      END
*DK DTXDEA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTXDEA
CH
      SUBROUTINE DTXDEA(PR,A)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C  DECODE ATTRIBUTE
C
      DATA F100/1000./,DFM/0.1/
C     CHANGE F100 IF MPAT CHANGES! MPAT=6: F100=100, MPAT=7: F100=1000 ...
      PARAMETER (MPAT=7)
      DIMENSION A(MPAT)
      FM=F100*PR
      MM=FM+DFM
      DO K=MPAT,1,-1
        A(K)=FLOAT(MOD(MM,10))
        MM=MM/10
      END DO
      END
*DK DTXSC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTXSC
CH
      SUBROUTINE DTXSC0(SCALE)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(20),V(20),NDSEG(2,19)
      CHARACTER *(*) TKBD
      LOGICAL FBUT(4)
      DATA D/4./
      CALL DWRT('Type <CR> to Stop and to reenable terminal + help.')
      CALL DWRT_END(0)
      DS=0.5*SCALE
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DT010
CH
      ENTRY DTXSCH(IHC,IVC,H,V,NDSEG,NUMPL,FBUT,TKBD)
CH
CH --------------------------------------------------------------------
CH
      IF(.NOT.FBUT(4)) RETURN ! DO NOT REACT ON KEYBOARD INPUT
      HM=IHC
      VM=IVC
      H(1)=HM-DS
      H(2)=HM+DS
      H(3)=H(1)
      H(4)=H(1)
      H(5)=H(2)
      H(6)=H(2)
      V(1)=VM
      V(2)=VM
      V(3)=VM-D
      V(4)=VM+D
      V(5)=V(3)
      V(6)=V(4)
      GO TO 9
CH..............---
CH
CH
CH
CH
CH
CH
      ENTRY DTXSCV(IHC,IVC,H,V,NDSEG,NUMPL,FBUT,TKBD)
CH
CH --------------------------------------------------------------------
CH
      IF(.NOT.FBUT(4)) RETURN ! DO NOT REACT ON KEYBOARD INPUT
      HM=IHC
      VM=IVC
      V(1)=VM-DS
      V(2)=VM+DS
      V(3)=V(1)
      V(4)=V(1)
      V(5)=V(2)
      V(6)=V(2)
      H(1)=HM
      H(2)=HM
      H(3)=HM-D
      H(4)=HM+D
      H(5)=H(3)
      H(6)=H(4)
    9 NDSEG(1,1)=1
      NDSEG(1,2)=3
      NDSEG(1,3)=5
      NDSEG(2,1)=2
      NDSEG(2,2)=2
      NDSEG(2,3)=2
      NUMPL=3
      END
*DK DTXRS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTXRS
CH
      SUBROUTINE DTXRS(IHC,IVC,H,V,NDSEG,NUMPL,FBUT,TKBD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TKBD
      LOGICAL FBUT(4)
      DIMENSION H(5),V(5),NDSEG(2,1)
      DATA DHTAB/8./,DH2/4./,DV2/4./
      IF(.NOT.FBUT(4)) RETURN ! DO NOT REACT ON KEYBOARD INPUT
      HC0=IHC
      VC0=IVC
      H(1)=HC0
      H(3)=HC0+DH
      V(1)=VC0
      V(3)=VC0+DV
      H(2)=H(3)
      H(4)=H(1)
      H(5)=H(1)
      V(2)=V(1)
      V(4)=V(3)
      V(5)=V(1)
      NUMPL=1
      NDSEG(1,1)=1
      NDSEG(2,1)=5
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DTRSL0
CH
      ENTRY DTXRS0(NROW,NLET,DVTAB)
CH
CH --------------------------------------------------------------------
CH
      DH=NLET*DHTAB+DH2
      DV=NROW*DVTAB+DV2
      END
*DK DTXGR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTXGR
CH
      SUBROUTINE DTXGR(IHC,IVC,H,V,NDSEG,NUMPL,FBUT,TKBD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DIMENSION H1(12),V1(12)
      DIMENSION H(20),V(20),NDSEG(2,19)
      CHARACTER *(*) TKBD
      LOGICAL FBUT(4),F1
      DATA DR0/1./,M12/-12/,RO0/10./,AR/1./,BR/0./,CR/0.0001/
      IF(.NOT.FBUT(4)) RETURN ! DO NOT REACT ON KEYBOARD INPUT
      IF(.NOT.FBUT(2)) THEN
        HC=IHC
        VC=IVC
      ELSE
        IHC=HC
        IVC=VC
      END IF
      DO K=1,12
        H(K)=H1(K)*RO+HC
        V(K)=V1(K)*RO+VC
      END DO
      IF(.NOT.FBUT(1)) THEN
        RO=RO+DR
        DR=AR+BR*RO+CR*RO*RO
      END IF
      NUMPL=1
      NDSEG(1,1)=1
      NDSEG(2,1)=M12
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DTXGR0
CH
      ENTRY DTXGR0
CH
CH --------------------------------------------------------------------
CH
      CALL DGGCUR(HH,VV)
      CALL DPOAR(HH,VV,MAR)
      IF(MAR.NE.-1) THEN
        CALL DQSET(MAR,0.,0.)
        CALL DQPOC(0.,0.,HC,VC,F1)
      ELSE
        HC=331.
        VC=331.
      END IF
      F1=.TRUE.
      A=0.
      DO K=1,12
        H1(K)=COSD(A)
        V1(K)=SIND(A)
        A=A+30.
      END DO
      RO=RO0
      DR=DR0
      END
*DK DTXPL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTXPL
CH
      SUBROUTINE DTXPL(IHC,IVC,H,V,NDSEG,NUMPL,FBUT,TKBD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(20),V(20),NDSEG(2,19)
      DATA D/10./
      CHARACTER *(*) TKBD
      LOGICAL FBUT(4),F1
      IF(.NOT.FBUT(4)) RETURN ! DO NOT REACT ON KEYBOARD INPUT
      IF(FBUT(1).AND.N1.EQ.0) THEN
        HC0=IHC
        VC0=IVC
        H(1)=HC0-D
        H(2)=HC0+D
        H(3)=HC0
        H(4)=HC0
        V(1)=VC0
        V(2)=VC0
        V(3)=VC0-D
        V(4)=VC0+D
        NUMPL=2
        NDSEG(1,1)=1
        NDSEG(1,2)=3
        NDSEG(2,1)=2
        NDSEG(2,2)=2
        RETURN
      END IF
      IF(FBUT(1).AND.N1.GT.0) THEN
        IF(F1) THEN
          N1=N1+1
          NDSEG(2,1)=ISIGN*N1
          F1=.FALSE.
        END IF
        IF(.NOT.FBUT(2)) IHC=H(N1)
        IF(.NOT.FBUT(3)) IVC=V(N1)
        H(N1)=IHC
        V(N1)=IVC
      ELSE
        IF(N1.EQ.0) THEN
          H(1)=HC0
          V(1)=VC0
          NUMPL=1
          N1=1
        END IF
        F1=.TRUE.
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DTXPL0
CH
      ENTRY DTXPL0(NSIGN)
CH
CH --------------------------------------------------------------------
CH
      N1=0
      CALL DWRT('Toggle left button to draw lines.')
      CALL DWRT('Use other buttons to draw vertically or horizont.')
      CALL DWRT('Type <CR> to Stop and to reenable terminal + help.')
      CALL DWRT_END(0)
      ISIGN=NSIGN
      END
*DK DTXWIN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTXWIN
CH
      SUBROUTINE DTXWIN(H,V,T)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *1 T,TNUM(6)
      DATA TNUM/'1','2','3','4','5','6'/
      DO K=1,6
        IF(
     &    H.GE.HMINDG(K).AND.H.LT.HHGHDG(K).AND.
     &    V.GE.VMINDG(K).AND.V.LT.VHGHDG(K)) THEN
          T=TNUM(K)
          RETURN
        END IF
      END DO
      T=' '
      END
*DK DTNOSL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTNOSL
CH
      SUBROUTINE DTNOSL(P,I,N,T)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      CHARACTER *(*) T
      DIMENSION P(4,1)
      IF(P(4,I)) 1,2,3
    1 T(N:N)='/'
      RETURN
    2 T(N:N)='='
      RETURN
    3 T(N:N)=':'
      END
*DK DTYANS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTYANS
CH
      SUBROUTINE DTYANS(T,TR,N)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C     INPUT: T=TEXT TO TYPE, TR=1 WORD OF ACCEPTED LETTERS TO ANSWER
C     OUTPUT: N=-1 ANSWER NOT IN TR, but stored in TXTADW
C             N= 0 ANSWER = RETURN
C             ELSE N = # OF LETTER IN TR
C
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) T,TR,TX
      CHARACTER *1 TA
      CALL DWRT(T)
      L=LENOCC(TR)
      IF(TR.EQ.'<') THEN
        CALL DGETLN(TA,NA,0)
      ELSE
        CALL DGETLN(TA,NA,1)
      END IF
      IF(NA.EQ.1) THEN
        TXTADW=TA
        DO N=1,L
          IF(TA.EQ.TR(N:N)) RETURN
        END DO
        N=-1
      ELSE
        N=0
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DTYATX
CH
      ENTRY DTYATX(TX)
CH
CH --------------------------------------------------------------------
CH
      TX=TA
      END
*DK DTSTOI
CH..............+++
CH
CH
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTSTOI
CH
      SUBROUTINE DTSTOI(TMOD,TIN,TS,NUM,NDIG)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C        Store integer NUM OF NDIG<13  digits into output line TIN.
C        TMOD='B' just before text TS (of any length)
C        TMOD='A' just  after text TS
      CHARACTER *(*) TIN,TS,TMOD
      CHARACTER *12 T12
      LIN=LENOCC(TIN)
      LIS=LENOCC(TS)
      IF(LIS.GT.LIN) GO TO 9
      DO L=1,LIN-LIS+1
        IF(TIN(L:L+LIS-1).EQ.TS) GO TO 1
      END DO
      GO TO 9
    1 WRITE(T12,1000) NUM
 1000 FORMAT(I12)
      IF(T12(12-NDIG:12-NDIG).EQ.' ') THEN
        IF(TMOD.EQ.'B') THEN
          LL=L-1
          L1=L-NDIG
          IF(L1.LT.1) GO TO 4
          TIN(L1:LL)=T12(13-NDIG:12)
        ELSE
          DO J=1,12
            IF(T12(J:J).NE.' ') GO TO 3
          END DO
    3     L1=L+LIS
          I=13-J
          LL=L1+NDIG-1
          IF(LL.GT.LIN) GO TO 4
          TIN(L1:LL)=' '
          TIN(L1:L1+I-1)=T12(J:12)
        END IF
      ELSE
    4   IF(TMOD.EQ.'B') THEN
          LL=L-1
          IF(LL.LE.0) GO TO 9
          L1=MAX(1,L-NDIG)
          TIN(L1:LL)=' '
          TIN(LL:LL)='*'
        ELSE
          L1=L+LIS
          IF(L1.GT.LIN) GO TO 9
          LL=MIN(LIN,L1+NDIG-1)
          TIN(L1:LL)='*'
        END IF
      END IF
      RETURN
    9 CALL DWRT('DTSTOI:ERROR')
      END
*DK DTYPT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTYPT
CH
      SUBROUTINE DTYPT(TMOD,TPIC,NPR1,NPR2,PR,TPR,TIN)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C        Store parameters pr into output line tin.
C  INPUT: all. TIN may be also output (TMODE='BACK').
C
C  NPR1=# START OF PARAMETRER, NPR2=# END OF PARAMETERS
C  PR=PARAMETERS (4,*), TPR=P.-NAMES
C
C  EXAMPLE TPR(  1:5)='T1' , 'T2' , 'T3' , 'T4' , 'T5'
C           PR(2,1:5)=3412 , 1.51 ,-1.235,   3  ,   2
C           PR(4,1:5)= -1. , 1.   ,  1.  ,  -1. ,   1.
C        TIN='P?:W?:Z?: T2_123 T1$1234 T4_-12 T5_-1 T3$12345
C     RESULT='YX:WS:NZ: T2=1.5 T1/3412        T5=2  T3:-1.23
C
C  TMODE='TYPE': type T
C  TMODE='SAVE': T is saved for the next time DTYPT is called. DTYPT
C                           MUST be called again.
C  TMODE='BACK': T is not typed but returned in TIN.
C  TPIC#' '      : IF TIN='P?:.........' processor name is filled in.
C  TPIC#' '      : IF TIN='..:W?:......' window is filled in.
C  TPIC#' '      : IF TIN='......Z?:...' zoom mode is filled in.
C  TIN = '..... TS_123 : IF('TS'.EQ.TPR(N)) PR(2,N) FILLS 123.
C                        _ IS REPLACED BY =.
C  TIN = '..... TS$123 : IF('TS'.EQ.TPR(N)) PR(2,N) FILLS 123.
C                        $ IS REPLACED BY /,=,:, depending on PR(4,N).
C  TIN = '..... TS_-23 : IF('TS'.EQ.TPR(N)) IF(PR(4,N).LT.0) TS_-23=' '
C                                           ELSE:PR(2,N) FILLS 123.
C                                           _ IS REPLACED BY =.
C  TIN = '..... TS$-23 : IF('TS'.EQ.TPR(N)) IF(PR(4,N).LT.0) TS_-23=' '
C                                           ELSE:PR(2,N) FILLS 123.
C                                           _ IS REPLACED BY /,=:.
C  TIN = '..... <<&    : <<& IS REPLACED BY <T3 ETC
C  TIN = '..... >>&    : >>& IS REPLACED BY >T0 ETC
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION PR(4,*)
      CHARACTER *(*) TIN,TPIC
      CHARACTER *50 T
      DATA T/' '/
      CHARACTER *4 TMOD,TLAST
      CHARACTER *2 TPR(*)
    1 IF(TLAST.NE.'SAVE') THEN
        LN=LENOCC(TIN)
        LM=MIN(49,LN)
        T=TIN(1:LM)
      END IF
      TLAST=TMOD
      DO L=3,LM
        IF(T(L:L).EQ.'$'.OR.T(L:L).EQ.'_') THEN
          DO N=NPR1,NPR2
            IF(T(L-2:L-1).EQ.TPR(N)) THEN
              I2=MIN(LM,L+8)
              DO I=L+1,I2
                IF(T(I:I).EQ.' ') GO TO 2
              END DO
              I=I2+1
    2         I=I-1
              IF(PR(4,N).LT.0..AND.T(L+1:L+1).EQ.'-') THEN
                T(L-2:I)=' '
              ELSE
                CALL DTN(PR(2,N),I-L,T(L+1:I))
                IF(T(L:L).EQ.'$') THEN
                  CALL DTNOSL(PR,N,L,T)
                ELSE
                  T(L:L)='='
                END IF
              END IF
              GO TO 3
            END IF
          END DO
        ELSE IF(T(L:L+1).EQ.'[]') THEN
          CALL DDRTYP(T(L-1:L+1))
        END IF
    3 END DO
      IF     (TPIC.EQ.' ') THEN
        IF(TMOD.EQ.'TYPE'.AND.T.NE.' ') THEN
          CALL DWRT(T)
        END IF
      ELSE 
        CALL DWR_LINE(4,'.............')
        IF(T(1:3).EQ.'P?:') T(1:2)=TPIC
        IF(T(4:6).EQ.'W?:') T(4:5)=TAREDO(IAREDO)
        IF(T(7:9).EQ.'Z?:') T(7:8)=TZOODO(IZOMDO)
        CALL DWR_HIGH_LIGHT(T,1,2)
      END IF
      IF(TMOD.EQ.'BACK') TIN=T(1:LM)
      END
