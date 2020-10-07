*DK D_DEF_PAR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------  D_DEF_PAR
CH
      SUBROUTINE D_DEF_PAR(NPR,TITLE)
CH
CH --------------------------------------------------------------------
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
      COMMON /DOPR1T/ TPLNDO(-2:11)
      COMMON /DOPR2T/ TAREDO( 0:14)
      COMMON /DOPR3T/ TZOODO( 0: 1)
      COMMON /DOPR4T/ TPICDO,THLPDO,TNAMDO
      CHARACTER *2 TPLNDO,TAREDO,TZOODO,TPICDO,THLPDO,TNAMDO
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DH
      COMMON /DHELPT/ TAN1DH,TPRGDH,TLT3DH
      CHARACTER *1 TPRGDH,TLT3DH
      CHARACTER *3 TAN1DH
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *(*) TITLE
      CHARACTER *2 TANSW,TP(4),TPR,TG,TGL,TGR(8)
      CHARACTER *3 THELP
      DATA TP/'P1','P2','P3','P4'/
      DATA TGR/8*'? '/
      DIMENSION PR(4,4)
      DATA PR/-9999.,999998.,99999.,-1.,
     &        -9999.,999998.,99999.,-1.,
     &        -9999.,999998.,99999.,-1.,
     &        -9999.,999998.,99999.,-1./
      CHARACTER *42 THEAD,TCOM
      DATA TCOM/' '/
      LOGICAL FOUND,FCH,FUS
      CHARACTER *49 T1
C                       1         2         3         4         5
C              12345678901234567890123456789012345678901234567890
      DATA T1/'P?:W?: Define ------------------------------ p=??'/
C                        1   symbols
C                        2   color functions
C                        3   constant data colors
C                        4   detector colors
C                        5   styles and modes
C                        6   header
C                        7   scales
C                        8   other parameters
      TG=TGR(NPR)
  931 TPR=TPICDO
      NP=0
      NPAR=0
C     ................................... normal users
      FUS=.TRUE.
  930 T1(40:41)=TG
      IF(NP.EQ.0.or.NPAR.EQ.0) THEN
        T1(46:49)=' '
      ELSE
        T1(46:49)='p='//TPOPDA(NPAR)
      END IF
      N_2_DA=0
C     ............................... Subgroup set to blanc = not used yet
      CALL DPAR_GET_HEADER(TPR,TG,THEAD,THELP,FOUND)
C     .............................. Only one group is displyed directly.
  932 IF(.NOT.FUS) N2=0
      IF(FOUND) THEN
        IF(TG.EQ.'? ') THEN
          T1(15:44)=TITLE
          CALL DTYPT('TYPE',TPICDO, 0    ,  0   ,  0   ,  ' ' ,T1)
C         ....................................... possible selections
          CALL DWRT('select '//THEAD)
          TCOM=THEAD
C         .............. changed 28.2.97, otherwise "SD" gives wrong menu
C         TAN1DH=' '
          TAN1DH=TPICDO//'0'
        ELSE
C         ................ display group and store into TPOPDA
          T1(15:44)=THEAD
          CALL DTYPT('TYPE',TPICDO, 0    ,  0   ,  0   ,  ' ' ,T1)
          CALL DPAR_DSP_STR_GROUP
C         ................... remember group for next use of the processor
          IF(FUS) TGR(NPR)=TG
          TAN1DH=THELP
        END IF
      ELSE
        CALL DWRT('Wrong command '//TG)
        TG=TGL
      END IF
  936 FCH=.FALSE.
      CALL DOPER(1,0,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  1,NP,TP,PR,
     &  NEXEC,FCH,TANSW)
C     ............................. change of limits, on/off for specialists
      IF(FCH.AND.NP.GT.0.AND.N_2_DA.GT.0.AND.NPAR.GT.0) THEN
        FCH=.FALSE.
        DO N=1,4
          IF(PR(2,N).NE.999998.) THEN
            PARADA(N,NPAR)=PR(2,N)
            PR(2,N)=999998.
            FCH=.TRUE.
          END IF
        END DO
        IF(FCH) THEN
          WRITE(TXTADW,1001) NPAR,TPOPDA(NPAR),(PARADA(J,NPAR),J=1,4)
          CALL DWRC
          GO TO 936
        END IF
      END IF
      GO TO (910,920,930,940),NEXEC
  910 CALL DSC0
      TAN1DH=' '
      RETURN
  920 IF(N_2_DA.GT.0) THEN
        CALL DPAR_CHG_BY_NAME(TANSW,FOUND)
        IF(FOUND) GO TO 936
      END IF
      CALL DO_STR('C.: Clear group ( = detector)')
      IF(TANSW.EQ.'SD') THEN
        TG='? '
        GO TO 931
      END IF
      IF(TG.NE.'1G') THEN
        CALL DO_STR('L.: List all')
        IF(TANSW.EQ.'L.') THEN
          LCOM=LENOCC(TCOM)
          DO L=1,LCOM-1,3
            IF(TCOM(L:L+1).EQ.'..') GO TO 930
            CALL DPAR_GET_HEADER(TPR,TCOM(L:L+1),THEAD,THELP,FOUND)
            IF(TG.NE.TCOM(L:L+1)) CALL DPAR_DSP_STR_GROUP
          END DO
          GO TO 930
        END IF
      END IF
      CALL DO_STR('M?: Redefine main group.')
      IF(TANSW.EQ.'M?') THEN
C       ............................ for very specialists
        CALL DWRT('Redefine main group or <cr>')
        CALL DGETLN(TANSW,LANSW,2)
        IF(LANSW.EQ.2) THEN
          FUS=.FALSE.
          TPR=TANSW
          T1(15:34)=TPR
          TG='? '
          NP=0
          NPAR=0
        END IF
        GO TO 930
      END IF
      CALL DO_STR('P?: Change all 4 parameter values.')
      IF(TANSW.EQ.'P?'.AND.N_2_DA.GT.0) THEN
        CALL DOPERG(NSP,NPAR)
        IF(NSP.EQ.1.AND.NPAR.GE.N_1_DA.AND.NPAR.LE.N_2_DA) THEN
          NP=4
          WRITE(TXTADW,1001) NPAR,TPOPDA(NPAR),(PARADA(J,NPAR),J=1,4)
 1001     FORMAT(I3,1X,A,1X,F8.2,F10.3,F9.0,F4.0)
          CALL DWRC
          GO TO 936
        END IF
      END IF
      CALL DO_STR('D?: For debugging of parameters only.')
      IF(TANSW.EQ.'D?') THEN
        CALL DPAR_DEBUG
        GO TO 936
      END IF
      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('D_DEF_PAR')
        IF(TCOM.NE.' ') CALL DWRT(TCOM)
        GO TO 936
      END IF
      CALL DAREA('D',TANSW,0,13,IAREDO,FOUND)
      IF(FOUND) GO TO 940
      IF(TG.EQ.'IG') THEN
        CALL DWR_IC(TANSW)
        GO TO 936
      END IF
      CALL DPAR_GET_HEADER(TPR,TANSW,THEAD,THELP,FOUND)
      IF(FOUND) THEN
        TGL=TG
        TG=TANSW
        GO TO 932
      END IF
      CALL DWRT('Wrong command '//TANSW)
      GO TO 930
  940 CALL DSC0
C     CALL DSCTR1
      NAR=IAREDO
      CALL DPCEAR(NAR)
      IAREDO=NAR
      GO TO 936
      END
*DK DDLHIT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDLHIT
CH
      SUBROUTINE DDLHIT
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C
C!:  LIST TPCO HITS
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C------------------------------------------------------------------- DC
C      PARAMETER (MCTPDC=2752)
      PARAMETER (MCTPDC=50000)
      COMMON /DCOTP/ FDTPDC,FVTPDC,NCTPDC(MCTPDC)
      LOGICAL FDTPDC,FVTPDC
      COMMON /DCOTP1/FNOCDC
      LOGICAL FNOCDC
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION DV(-1:1)
      DATA DV/12.,0.,0./,MAX/108/,DH/6./,N8/8/,DD/12./
      CHARACTER *41 T
      LOGICAL FOUT,FUSFI,FUSTE
      DIMENSION H(-1:1)
      IF(IAREDO.LT.9.OR.IAREDO.GT.11) RETURN
      CALL DDLCL(ICTPDD)
      H(-1)=HMINDG(IAREDO)+DH
      H( 1)=0.5*(HHGHDG(IAREDO)+HMINDG(IAREDO))+DH
      V=VHGHDG(IAREDO)-DD
C        123456789 123456789 123456789 123456789 1
C     T=' 12  1234 1234 1234   12  1234 1234 1234'
      T='  #    X    Y    Z     #    X    Y    Z '
      CALL DGLEVL(8)
      CALL DGTEXT(H(-1),V,T,41)
      CALL DV0(TPCODB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL DSCTP
      CALL DCUTTF(FUSFI,FC1,FC2,FUSTE,TC1,TC2)
      N=0
      L=-1
      DO 300 K=NUM1,NUM2
        IF(FCUHDT) THEN
          CALL=DVTPNT(K,NTRK)
          IF(FNOHDT(NTRK)) GO TO 300
        END IF
        IF(FUSTE) THEN
          TE=DVTP(IVTEDV,K)
          IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 300
        END IF
        IF(FUSFI) THEN
          FI=DFINXT(FIMID,DVTP(IVFIDV,K))
          IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 300
        END IF
        N=N+1
        IF(N.GT.MAX) GO TO 9
        IF(FVTPDC) CALL DGLEVL(NCTPDC(K))
        IX=DVTP(IVXXDV,K)
        IY=DVTP(IVYYDV,K)
        IZ=DVTP(IVZZDV,K)
        WRITE(T,1000) NTRK,IX,IY,IZ
 1000   FORMAT(I3,3I5)
        V=V-DV(L)
        CALL DGTEXT(H(L),V,T,18)
        L=-L
  300 CONTINUE
    9 CALL DGLEVL(N8)
      CALL DQDAR(HMINDG(IAREDO),VMINDG(IAREDO),
     &            HHGHDG(IAREDO),VHGHDG(IAREDO))
      CALL DGEXEC
      END
*DK DDRCA0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDRCA0
CH
      SUBROUTINE DDRCA0
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!: SETUP DISTANCE CALCULATION
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *2 T,TIN
      CHARACTER *3 T3
      LOGICAL NYES
      CHARACTER *2 TA
C     RZ(-1:19=DETECTOR LIMITS ; 3='RO','ZZ','YZ','RR' ; 0:1=FR,TO)
      DIMENSION RZ(-1:19,4,0:1)
      CHARACTER *2 TPLN(0:19),TDET(6)
      DATA TPLN/
     &  'VX',
     &  'V0','V1',
     &  'I0','I1',
     &  'T0','T1','T2','T3',
     &  'E0','E1','E2','E3',
     &  'H0','H1','H2',
     &  'M0','M1','M2',
     &  'oo'/
C                                       ! If TPLN is changed change also
c                                       ! in DTU
      DATA RZ/
C                                       FROM RO
     &  -1000.,
     &    0.,
     &    7., 13.,
     &   13., 28.,
     &   30., 95.,148.,180.,
     &  185.,200.,220.,240.,
     &  280.,382.,480.,
     &  470.,538.,595.,
     &  1000.,
C                                       FROM ZZ
     &  -1000.,
     &    0.,
     &  0.01,0.02,
     &  0.03,0.04,
     &  0.05, 75.,150.,225.,
     &  235.,265.,285.,315.,
     &  320.,425.,500.,
     &  501.,555.,610.,
     &   1000.,
C                                       FROM YZ
     &  -1000.,
     &    0.,
     &  0.01,0.02,
     &  0.03,0.04,
     &  0.05, 75.,150.,225.,
     &  235.,265.,285.,315.,
     &  320.,425.,500.,
     &  501.,555.,610.,
     &   1000.,
C                                       FROM RR
     &  21*0.,
C                                       TO RO
     &  -1000.,
     &    0.,
     &    7., 13.,
     &   14., 28.,
     &   30., 95.,148.,180.,
     &  185.,200.,220.,240.,
     &  280.,382.,480.,
     &  470.,538.,595.,
     &  1000.,
C                                       TO ZZ
     &  -1000.,
     &    0.,
     &   11.,11.1,
     &  11.2,103.,
     &  105.,145.,185.,225.,
     &  235.,265.,285.,315.,
     &  320.,425.,500.,
     &  501.,555.,610.,
     &  1000.,
C                                       TO YZ
     &  -1000.,
     &    0.,
     &  0.01,0.02,
     &  0.03,0.04,
     &  0.05, 75.,150.,225.,
     &  235.,265.,285.,315.,
     &  320.,425.,500.,
     &  501.,555.,610.,
     &  1000.,
C                                       TO RR
     &  21*0./

      DATA TDET/ 'VD','IT','TP','EC','HC','MU'/

      DO 710 K=0,1
        DO 711 N=-1,19
          RZ(N,4,K)=SQRT(RZ(N,1,K)*RZ(N,1,K)+RZ(N,2,K)*RZ(N,2,K))
  711   CONTINUE
  710 CONTINUE
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DDRCAL
CH
      ENTRY DDRCAL(TIN,T)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:CALCULATE DISTANCE
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C                                       ! PARADA(2,IFRTO)= FILL BY DOPER #
C                                       ! PARADA(1,IL1L2)=
C                                       !  0 NO CALCULATION
C                                       !  1 LAST INPUT BY LOGO
C                                       !  2 LAST INPUT BY NUMBER
C                                       ! PARADA(2,IL1L2)= LOGO
C                                       ! PARADA(4,IL1L2)= LAST LOGO
C ---------------------------------------------------------------------
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PTE,J_PL1,J_PL2,J_PFR'
      CALL DPARAM(11
     &  ,J_PTE,J_PL1,J_PL2,J_PFR)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(     T.EQ.'RO') THEN
         NRZ=1
      ELSE IF(T.EQ.'ZZ') THEN
         NRZ=2
      ELSE IF(T.EQ.'YZ') THEN
         NRZ=3
      ELSE IF(T.EQ.'RR') THEN
         NRZ=4
      ELSE IF(T.EQ.'RZ') THEN
         TEMID=PARADA(2,J_PTE)
         IF(TEMID.GT.90.) TEMID=180.-TEMID
         IF(TEMID.LT.40..OR.IZOMDO.EQ.0) THEN
            NRZ=2
         ELSE
            NRZ=1
         END IF
      ELSE
        RETURN
      END IF
   10 IF(TIN.EQ.'NZ') THEN
        PARADA(2,J_PL1)=PARADA(4,J_PL1)
        PARADA(2,J_PL2)=PARADA(4,J_PL2)
        IF(PARADA(2,J_PL1).GE.PARADA(2,J_PL2)) PARADA(2,J_PL1)=0.
      END IF
      DO M=0,1
         IFRTO=J_PFR+M
         IL1L2=J_PL1+M
         IF(TIN.EQ.'RB'.OR.PARADA(1,IL1L2).EQ.2.) THEN
            D=PARADA(2,IFRTO)
            IF(D.GT.0.) THEN
              DO K=0,18
                 IF(D.EQ.RZ(K,NRZ,M)) THEN
                   PARADA(2,IL1L2)=K
                   GO TO 21
                 END IF
                 IF(D.LT.RZ(K,NRZ,M)) GO TO 20
              END DO
              K=19
   20         K=K-1
              PARADA(2,IL1L2)=K+(D-RZ(K,NRZ,M))/
     &                (RZ(K+1,NRZ,M)-RZ(K,NRZ,M))
            ELSE
              PARADA(2,IL1L2)=D
            END IF
   21       PARADA(1,IL1L2)=0.
         ELSE IF(TIN.EQ.'IN'.OR.TIN.EQ.'NZ'.
     &    OR.PARADA(1,IL1L2).EQ.1.) THEN
            IF(PARADA(2,IL1L2).GT.0.) THEN
              N=PARADA(2,IL1L2)
              PARADA(2,IFRTO)=RZ(N,NRZ,M)
              IF(TIN.EQ.'IN') THEN
                D=PARADA(2,IL1L2)-N
                PARADA(2,IFRTO)=PARADA(2,IFRTO)+
     &            D*(RZ(N+1,NRZ,M)-RZ(N,NRZ,M))
              END IF
            ELSE
              PARADA(2,IFRTO)=PARADA(2,IL1L2)
            END IF
            PARADA(1,IL1L2)=0.
         END IF
      END DO
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DDRLOG
CH
      ENTRY DDRLOG(NP,TA,NYES)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!: STORE DISTANCE LOGO
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PL1,J_PFR,J_PTO'
      CALL DPARAM(11
     &  ,J_PL1,J_PFR,J_PTO)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(NP.EQ.J_PFR.OR.NP.EQ.J_PTO) THEN
        MP=J_PL1-J_PFR
        DO M=0,18
          IF(TA.EQ.TPLN(M)) THEN
            NYES=.TRUE.
            PARADA(2,NP+MP)=M
            PARADA(4,NP+MP)=M
            PARADA(1,NP+MP)=1.
            RETURN
          END IF
        END DO
        DO M=1,6
          IF(TA.EQ.TDET(M)) THEN
            NYES=.TRUE.
            IF(NP.EQ.J_PFR) THEN
              D=DET1DR(M)
            ELSE
              D=DET2DR(M)
            END IF
            PARADA(1,NP+MP)=1.
            PARADA(2,NP+MP)=D
            PARADA(4,NP+MP)=D
            RETURN
          END IF
        END DO
      END IF
      NYES=.FALSE.
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DDRNUM
CH
      ENTRY DDRNUM(NP)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:STORE DISTANCE NUMBER
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PL1,J_PL2,J_PFR,J_PTO'
      CALL DPARAM(11
     &  ,J_PL1,J_PL2,J_PFR,J_PTO)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(     NP.EQ.J_PFR) THEN
        PARADA(1,J_PL1)=2.
      ELSE IF(NP.EQ.J_PTO) THEN
        PARADA(1,J_PL2)=2.
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DDRTYP
CH
      ENTRY DDRTYP(T3)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!: TYPE DISTANCE
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      IF(T3(1:1).EQ.'>') THEN
        CALL DPARGV(11,'PL1',2,P)
      ELSE
        CALL DPARGV(11,'PL2',2,P)
      END IF
      IF(P.LT.0.OR.P.GT.18.) THEN
        T3=' '
      ELSE
        N=P
        IF(MOD(P,1.).EQ.0.) THEN
          T3(1:1)='='
        ELSE IF(T3(1:1).EQ.'<') THEN
          N=N+1
        END IF
        T3(2:3)=TPLN(N)
      END IF
      END
*DK DDATIN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDATIN
CH
      SUBROUTINE DDATIN
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      PARAMETER (MXYZDP=19)
      COMMON /DPICK2/ NXYZDP,XYZPDP(3,19)
      COMMON /DPICK3/ NTRKDP,LTRKDP(8)
      COMMON /DPICK4/ NUMPDP,LORPDP
      PARAMETER (MTRSDP=26)
      COMMON /DPICK5/ TRSTDP(MTRSDP)
C------------------------------------------------------------------- DT
      COMMON /DTESCT/ TREVDT,TULIDT,TNORDT
      CHARACTER *4 TREVDT,TNORDT
      CHARACTER *8 TULIDT
C------------------------------------------------------------------- DK
      COMMON /DKNSTC/ PIFCDK,RMAXDK,ZMAXDK,TCO1DK,TCO2DK,
     1   ROTODK(-1:10),ZZTODK(-1:10),ZMTODK(-1:10),DMAXDK,RMINDK,
     1   RLOWDK
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
C
C     TRST IS THE STEP POSITION FOR TRACKS IN [CM]
      DIMENSION TRST(MTRSDP)
      DATA TRST/0.,.1,.2,.5,1.,2.,5.,
     &        10., 20., 30., 40., 50., 60., 70., 80., 90.,
     &  100.,110.,120.,130.,140.,150.,160.,170.,180.,190./
      DO 1 K=1,MTRSDP
        TRSTDP(K)=TRST(K)/TRST(MTRSDP)
    1 CONTINUE
C      DO L=16,127
C        LTABDD(L)=L+1
C      END DO
      TREVDT(1:1)=CHAR(27)
      TULIDT(1:1)=TREVDT(1:1)
      TULIDT(5:5)=TREVDT(1:1)
      TNORDT(1:1)=TREVDT(1:1)
      TCO1DK=DATN2D(RMAXDK,ZMAXDK)
      TCO2DK=180.-TCO1DK
      END
*DK DDISPA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDISPA
CH
      SUBROUTINE DDISPA
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
C     INCLUDE 'DALI_CF.INC'
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CALL DGCLWK
      CALL DQCL(0)
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DDISPP
CH
      ENTRY DDISPP
CH
CH --------------------------------------------------------------------
CH
      CALL DCOPTL
      CALL DQFR(0)
      CALL DPCEAR(0)
      CALL DQTIN
      CALL DCOPFL
      END
C   =====================================================================
*DK DDLD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDLD
CH
      SUBROUTINE DDLD
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C
C   **  DISPLAY LIST  Processor  **
C
C
C    Created by H.Drevermann  28-JUL-1988
C    Updated by C.Grab        13-Sep-1989
C               C.Grab        15-Sep-1989  Added Trigger bits
C               C.Grab        19-Sep-1989  Added detector bits
C               C.Grab        21-Sep-1989  Added Cuts list
C               C.Grab        29-Sep-1989  Added Muon-HCAL association
C               C.Grab        24-Jun-1990  Added Muon-track association
C
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
C
      CALL DQWIL(0.)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_LLI'
      CALL DPARAM(33
     &  ,J_LLI)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      LIST=PARADA(2,J_LLI)
c
cC Command =  LT TB DB CU DM TM       IO  F1  F2  IS
c      GO TO (10,20,30,40,50,60,70,52,90,100,110,120),LIST
cC  --  Display track list without cuts
c
      IF(     LIST.EQ.1) THEN
C       ............................................. List of tracks
        CALL DDLTR1(.FALSE.,.FALSE.)
C
      ELSE IF(LIST.EQ.2) THEN
C       .............................................  Display trigger bits
        CALL DDLTB(1)
C
      ELSE IF(LIST.EQ.3) THEN
C       .............................................  Display detector bits
        CALL DDLDB
C
      ELSE IF(LIST.EQ.5) THEN
C       ............................ Display muon HCAL association parameters
        CALL DDLDM
C
      ELSE IF(LIST.EQ.6) THEN
C       ........................................ Display track list with cuts
        CALL DDLTR1(FCUTDT,.FALSE.)
C
      ELSE IF(LIST.EQ.7) THEN
C       .................................................... Short track list
        CALL DDLTR2
C
      ELSE IF(LIST.EQ.8) THEN
C       ........................... Display muon track association parameters
        CALL DDLTM
C
      ELSE IF(LIST.EQ.9) THEN
C       ...................................................... Ionisation list
        CALL DDLTR1(FCUTDT,.TRUE.)
C
      ELSE IF(LIST.EQ.10) THEN
C       ................................................. Full list of 1 track
        CALL DDLFTI(1)
C
      ELSE IF(LIST.EQ.11) THEN
C       ................................................ Full list of 2 tracks
        CALL DDLFTI(2)
C
      ELSE IF(LIST.EQ.12) THEN
C       .............................................. ionisation pro sector
        CALL DDLISL
      ELSE
C       ..................................... ALPHA LISTINGS 1:9 FRFT=0
C        CALL DDL_ALPHA(LIST-13)
C
      END IF
   99 IF(FPIKDP) RETURN
      CALL DQFR(IAREDO)
      CALL DPCSAR
      END
C
C**********************************************************************
      SUBROUTINE DDLFTI(NUMTR)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Ilias T. Efthymiopou  18-MAR-1991
C!
C!   Inputs:    NUMTR   /I      Number of tracks
C!        -
C!
C!   Outputs:
C!        -
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!   List the full dE/dx information for selected tracks, that is the dE/dx for
C!   each sector the track crosses
C?
C!======================================================================
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DP
      PARAMETER (MXYZDP=19)
      COMMON /DPICK2/ NXYZDP,XYZPDP(3,19)
      COMMON /DPICK3/ NTRKDP,LTRKDP(8)
      COMMON /DPICK4/ NUMPDP,LORPDP
      PARAMETER (MTRSDP=26)
      COMMON /DPICK5/ TRSTDP(MTRSDP)
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DC
      PARAMETER (MCTRDC=1000)
      COMMON /DCOTR/ FDTRDC,FVTRDC,NCTRDC(0:MCTRDC)
      LOGICAL FDTRDC,FVTRDC
C------------------------------------------------------------------- DS
      PARAMETER (NSEC=36)
      COMMON /DSLDEX/ NSIODS,NSECDS(NSEC),DEDXDS(NSEC),ERORDS(NSEC),
     &  NSAMDS(NSEC),TRLGDS(NSEC)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
C++++++ Particle hypotheses
      PARAMETER( NHYP = 5 )
      DIMENSION RIEXP(NHYP),SIGMA(NHYP),CHI(NHYP)
      DIMENSION RMASS(NHYP),Q(NHYP)
C                  electr  pion    muon     kaon     proton
      DATA RMASS/0.000511,0.13957,0.10565,0.493667,0.9382796/
      DATA Q/5*1./
      DATA FIELD/15./
      CHARACTER *42 T
      CHARACTER *2 DT2,TR
      LOGICAL FOUT
      DIMENSION NTPIK(3)
      DATA DVMAX/15./,N8/8/,DH/10./,H/0./
C-----------------------------------------------------------------------
      CALL DV0(FRFTDB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      HM=HMINDG(IAREDO)
      VH=VHGHDG(IAREDO)
      DV=0.5*(VHGHDG(IAREDO)-VMINDG(IAREDO))
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_LLI'
      CALL DPARAM(33
     &  ,J_LLI)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(FPIKDP) THEN
        NPAR=PARADA(4,J_LLI)
        NTPIK(1)=MOD(NPAR,1000)
        NTPIK(2)=NPAR/1000
        IF(FPIMDP) THEN
          DO 703 N=1,NUMTR
            IF(NTPIK(N).EQ.NPIKDP) GO TO 10
  703     CONTINUE
          RETURN
        ELSE
          VM=VH-DV
          IF(VPIKDP.LT.VH-DV.AND.NUMTR.EQ.2) THEN
            N=2
          ELSE
            N=1
          END IF
          NPIKDP=NTPIK(N)
          FNTRDP=FKTRDP
          MDLPDP=MDLRDP
        END IF
   10   DPIKDP=0.
        HHPKDP=H+DH
        VVPKDP=VHGHDG(IAREDO)-(N-1)*DV-2.*DVMAX
        RETURN
      END IF
      NUM=NUMTR
      NTPIK(1)=NTRKDP
      IF(NTPIK(1).LE.0.OR.NTPIK(1).GT.NUM2) THEN
        CALL DWRT('Pick a track first')
        RETURN
      END IF
      IF(NUM.EQ.2) THEN
        NTPIK(2)=LTRKDP(1)
        IF(NTPIK(2).LE.0.OR.NTPIK(2).GT.NUM2) THEN
          CALL DWRT('Only 1 track was picked.')
          NUM=1
        END IF
      END IF
      PARADA(4,J_LLI)=NTPIK(1)+1000*NTPIK(2)
      CALL DDLCL(ICTPDD)
      CALL DSCTR0(FVTRDC,NTCOL)
      DO 700 N=1,NUM
        NTR=NTPIK(N)
        IF(FVTRDC) THEN
          CALL DGLEVL(NCTRDC(NTR))
        ELSE
          CALL DGLEVL(NTCOL)
        END IF
        VH=VHGHDG(IAREDO)-(N-1)*DV-DVMAX
        TR=DT2(FLOAT(NTR))
        T='***************  Track '//TR//'  **************'
        CALL DGTEXT(HM,VH,T,42)
        CALL DGLEVL(N8)
        CALL DVTRT1(0,T)
        VH=VH-DVMAX
        CALL DGTEXT(HM,VH,T(5:),38)
        CALL DVTRT1(NTR,T)
        VH=VH-DVMAX
        CALL DGTEXT(HM,VH,T(5:),38)
        WRITE(T(1:2),1002) NTR
 1002   FORMAT(I2)
        CALL DVTHYP(NTR,FIELD,NHYP,RMASS,Q,RI,NS,TL,RIEXP,SIGMA,IER)
        IF(IER.EQ.0) THEN
C
C        123456789 123456789 123456789 123456789
C     T='   ION  DI    NS  TL'
C     T=' 12345 12345 123 123
C
          T='   ION  DI    NS  TL'
          VH=VH-DVMAX
          CALL DGTEXT(HM,VH,T,40)
          CALL DVTION(RI,NS,TL,NHYP,RIEXP,SIGMA,DION,CHI)
          ITL=TL
          RI=MIN(RI,9.999)
          DION=MIN(DION,9.999)
          WRITE(T,1004) RI,DION,NS,ITL
 1004     FORMAT(2F6.3,2I4)
          VH=VH-DVMAX
          CALL DGTEXT(HM,VH,T,40)
        END IF
C        123456789 123456789 123456789 123456789
        T=' SE  ION     DI   NS  TL'
C       T='123 123456 12345 123 123
        VH=VH-DVMAX
        CALL DGTEXT(HM,VH,T,23)
        IF(IER.EQ.0) THEN
          DO 1014  I=  1,  NSIODS
            ITL = TRLGDS(I)
            DEDXS = DEDXDS(I)
            DEDXS=MIN(DEDXS,99.999)
            DISEC = ERORDS(I)
            DISEC=MIN(DISEC,9.999)
            WRITE(T,1000) NSECDS(I),DEDXS,DISEC,NSAMDS(I),
     &        ITL
 1000       FORMAT(I3,F7.3,F6.3,2I4)
            VH=VH-DVMAX
            CALL DGTEXT(HM,VH,T,24)
 1014     CONTINUE
        END IF
  700 CONTINUE
      END

C**********************************************************************
      SUBROUTINE DDLISL
C----------------------------------------------------------------------
C!
C!   Author   :- Ilias T. Efthymiopou  22-MAR-1991
C!
C!      List the tracks and the Ionization for a given sector
C?
C!======================================================================
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DC
      PARAMETER (MCTRDC=1000)
      COMMON /DCOTR/ FDTRDC,FVTRDC,NCTRDC(0:MCTRDC)
      LOGICAL FDTRDC,FVTRDC
C------------------------------------------------------------------- DS
      PARAMETER (NSEC=36)
      COMMON /DSLDEX/ NSIODS,NSECDS(NSEC),DEDXDS(NSEC),ERORDS(NSEC),
     &  NSAMDS(NSEC),TRLGDS(NSEC)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
C++++++ Particle hypotheses
      PARAMETER( NHYP = 5 )
      DIMENSION RIEXP(NHYP),SIGMA(NHYP)
      DIMENSION RMASS(NHYP),Q(NHYP)
C                  electr  pion    muon     kaon     proton
      DATA RMASS/0.000511,0.13957,0.10565,0.493667,0.9382796/
      DATA Q/5*1./
      DATA FIELD/15./
      CHARACTER *42 T,TT
      CHARACTER *2 DT2,ISL
      DATA DVMAX/15./,N8/8/
C-----------------------------------------------------------------------
      IF(FPIKDP) THEN
        CALL DWRT('Pick not yet supported for IS list')
        RETURN
      END IF
      NTRIS=BNUMDB(2,FRFTDB)
      IF(NTRIS.EQ.0) RETURN
      HM=HMINDG(IAREDO)
      VH=VHGHDG(IAREDO)-DVMAX
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_LNS'
      CALL DPARAM(33
     &  ,J_LNS)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      ISL=DT2(PARADA(2,J_LNS))
      T='*************** Sector '//ISL//' **************'
      CALL DDLCL(ICTPDD)
      CALL DSCTR0(FVTRDC,NTCOL)
      CALL DGLEVL(N8)
      CALL DGTEXT(HM,VH,T,42)
      CALL DVTRT1(0,TT)
C        123456789 123456789 123456789 123456789 123456789
C     T=' #   ION  NS    P   THETA  D0   Z0 '
C        12 12345 123 12345 123  1234 12345
C
C        123456789 123456789 123456789 123456789 123456789
C    TT='   #    P   dP  phi theta  D0   Z0 chiq
C    TT=' *** -23.5 1.2 1.34 1.34 12.4 -99.0 1.3'
C
      T=' '
      T(1:38)=' #   ION  NS'//TT(5:10)//TT(20:35)
      VH=VH-DVMAX
      CALL DGTEXT(HM,VH,T,42)
      DO 1014  N=1, NTRIS
        IF(FVTRDC) THEN
          CALL DGLEVL(NCTRDC(N))
        ELSE
          CALL DGLEVL(NTCOL)
        END IF
        CALL DVTHYP(N,FIELD,NHYP,RMASS,Q,RI,NS,TL,RIEXP,SIGMA,IER)
        IF ( IER.NE.0 ) GOTO 1014
        DO 1005 I =  1,  NSIODS
          ISECT = INT(PARADA(2,J_LNS))
          IF ( NSECDS(I).NE.ISECT ) GO TO 1005
          DEDXS = MIN(DEDXDS(I),9.999)
          T = ' '
          WRITE(T(1:12),1004) N,DEDXS,NSAMDS(I)
 1004     FORMAT(I2,F6.3,2I4)
          CALL DVTRT1(N,TT)
          T(13:35)=TT(5:10)//TT(20:35)
          VH=VH-DVMAX
          CALL DGTEXT(HM,VH,T,42)
   20     CONTINUE
 1005   CONTINUE
 1014 CONTINUE
  999 RETURN
      END
C**********************************************************************
      SUBROUTINE DDLION(NTR,T)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Ilias T. Efthymiopou  18-MAR-1991
C!
C!   Inputs:    NTR  /I track number ( FRFT raw)
C!        -
C!
C!   Outputs:   T    /C what to write in the list for each track
C!        -
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!      List the dE/dx for all the charged tracks
C?
C!======================================================================
C     INCLUDE 'DALI_CF.INC'
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
C++++++ Particle hypotheses
      PARAMETER( NHYP = 5 )
      DIMENSION RIEXP(NHYP),SIGMA(NHYP),CHI(NHYP)
      DIMENSION RMASS(NHYP),Q(NHYP)
C                  electr  pion    muon     kaon     proton
      DATA RMASS/0.000511,0.13957,0.10565,0.493667,0.9382796/
      DATA Q/5*1./
      DATA FIELD/15./
      CHARACTER *(*) T
      CHARACTER *1 DT1
C-----------------------------------------------------------------------
      IF(NTR.EQ.0) THEN
        CALL DVEXS0(ITR)
        IF(ITR.EQ.0) THEN
          PRINT *,'--- dE/dx banks are missing'
        ELSE
          T=' #  ION  DI   NS  EL   PI   MU    K   PR'
        END IF
        RETURN
      END IF
      IF(ITR.EQ.0) RETURN
      CALL DVTHYP(NTR,FIELD,NHYP,RMASS,Q,RI,NS,TL,RIEXP,SIGMA,IER)
C
C         123456789 123456789 123456789 123456789
C      T=' #  ION  DI   NS  EL   PI   MU    K   PR'
C         12 1234 1234 123 1234 1234 1234 1234 1234
      T=' '
      WRITE(T(1:2),1002) NTR
 1002 FORMAT(I2)
      IF(IER.EQ.0) THEN
        CALL DVTION(RI,NS,TL,NHYP,RIEXP,SIGMA,DION,CHI)
        ITL=TL
        RI=MIN(RI,9.99)
        DION=MIN(DION,9.99)
        WRITE(T(3:41),1000) RI,DION,NS,CHI
 1000   FORMAT(2F5.2,I4,5F5.2)
      ELSE
        T(4:)='error '//DT1(FLOAT(IER))
      END IF
      END
CH
CH
C ========================================================================
C
      SUBROUTINE DDLTB(ITLIS)
C
C     Author :  C.Grab               15-Sep-1989
C  --  Display trigger bits
C
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *41 T
      DATA DV/10.2/,DVMIN/0./
      IF(FPIKDP) RETURN
      CALL DDLCL(ICBADD)
      VH=VHGHDG(IAREDO)
      VM=VMINDG(IAREDO)+0.5*DVMIN
      HM=HMINDG(IAREDO)
C
      NUM = ITLIS
C Initialise var. and extract info
      CALL DDLTB0(NUM,T)
      CALL DGLEVL(8)
      IF (NUM.LE.0) THEN
         CALL DWRT('DL: ** XTEB-bank missing - no trigger  bits  **  ')
         RETURN
      ENDIF
C
C  Display them now :
      DO 700 N=1,NUM
        CALL DDLTBN(N,T)
        VH=VH-DV
        CALL DGTEXT(HM,VH,T,41)
  700 CONTINUE
      END
*DK DDLCL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDLCL
CH
      SUBROUTINE DDLCL(ICDD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!: Clear window and redraw backgdround
C    Inputs    :pdcodd(2,ICDD) = COLOR OF BACKGROUND
C    Outputs   :NONE
C
C    Called by :DDL routines
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      IF(FPIKDP) RETURN
      CALL DQCL(IAREDO)
      IF(PDCODD(4,ICDD).GE.0..AND.PDCODD(2,ISTYDD).GE.2.) THEN
        CALL DQLEVL(ICDD)
        CALL DGRAR(HMINDG(IAREDO),VMINDG(IAREDO),
     &             HHGHDG(IAREDO),VHGHDG(IAREDO))
      END IF
      END
CH
C
C -------------------------------------------------------------------------
C
      SUBROUTINE DDLTB0(NUM,T)
C
C  Author : Christoph  Grab
C           12-Sep-89
C
C  List trigger bits in a DALI window, with explanations.
C  Get info from the trigger bank XTEB.
C      Input  :  NUM= index i for Ti- command (= level i decision)
C                     accepted are 1 or 2
C      Output :  NUM= number of bits to list
C                   = number of lines to print out
C                  T= String to display for the NUM-th bit
C
C -------------------------------------------------------------------------
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      LOGICAL BTEST
      INTEGER NUM
      CHARACTER *41 T,T1(32)
C
C  Explanation of trigger bits :
C  T = 'B#  X  Explanation                        '
C          *12    *=Mask enabled; 1,2= set after Level 1,2 decision:
      DATA T1/
     1 ' 0     LC_VHIGH Single Arm A.or.B V-high ',
     1 ' 1     RNDM_TRG RaNDoM Trig (Downsc. GBX)',
     1 ' 2     SNG_N_EM Single neutral em.energy ',
     1 ' 3     LC_LO_LO Bhabha A-low - B-low (10)',
     1 ' 4     LC_A_LOW Single arm Bhabha A low  ',
     1 ' 5     LC_B_LOW Single arm Bhabha B low  ',
     1 ' 6     LC_A_HIG Single arm Bhabha A high ',
     1 ' 7     LC_B_HIG Single arm Bhabha B high ',
     1 ' 8     Single muon                       ',
     1 ' 9     SNG_C_EM SiNGle Charged EM.energy ',
     1 '10     not used                          ',
     1 '11     SNG_C_HA SiNGle Charged HAdronic E',
     1 '12     SNG_N_HA SiNGle Neutral HAdronic E',
     1 '13     not used                          ',
     1 '14     LC_LO_HI Bhabha A-LOw - B-HIgh    ',
     1 '15     LC_HI_LO Bhabha A-HIgh - B-LOw    ',
     1 '16     not used                          ',
     1 '17     ETT_EWBA EToT Ecal Wires BArrel   ',
     1 '18     not used                          ',
     1 '19     not used                          ',
     1 '20     ETT_EWE* EToT Ecal Wire Endcap AND',
     1 '21     COS_SKEW COSmic ECal wires SKEWed ',
     1 '22     not used                          ',
     1 '23     not used                          ',
     1 '24     ETT_EWEA EToT Ecal Wires Endcap A ',
     1 '25     ETT_EWEB EToT Ecal Wires Endcap B ',
     1 '26     TRK_CNT1 TRacK CouNT 1 (ITC/TPC)  ',
     1 '27     TRK_CNT2 TRacK CouNT 2 (ITC/TPC)  ',
     1 '28     not used                          ',
     1 '29     COS_ENDC Cosmic HCAL ENDCaps      ',
     1 '30     COS_BARL Cosmic HCAL BARreL       ',
     1 '31     LVL2_YES Force LVL2 YES on L1 yes '/
C
C
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'
C   ---------------------------------------------------------------
C
      IF(FPIKDP) RETURN
      CALL DDLCL(ICBADD)
      JIN = NUM
      NUM=32
C Get the trigger decision words:
      KXTEB=IW(NAMIND('XTEB'))
      JXTEB= KXTEB + LMHLEN
      IF(KXTEB.EQ.0) NUM=0
      T = 'B# Set Trigger bits after Level i decis.  '
C
C get wanted word (ITWRD) of trig-bank, depending on arg.
      ITRBT1 = IW(JXTEB+1)
      ITRBT2 = IW(JXTEB+2)
C
C Now get the trigger-enable-mask:
      KXTCN=IW(NAMIND('XTCN'))
      JXTCN=KXTCN + LMHLEN
      ITENBL=IW(JXTCN+8)
      IF (KXTCN.LE.0) ITENBL=0
      RETURN
C
C  Get trigger bits : Display line for the K-th trigger bit.
C           BTEST counts 0...31
      ENTRY DDLTBN(K,T)
      IF (KXTEB.EQ.0) RETURN
      T = '                                          '
      IF (K.LT.1 .OR.K.GT.32) THEN
        T = '  ====>    K out of range                 '
      ELSE
        T = T1(K)
        IF (BTEST(ITENBL,K-1)) T(4:4) = '*'
        IF (BTEST(ITRBT1,K-1)) T(5:5) = '1'
        IF (BTEST(ITRBT2,K-1)) T(6:6) = '2'
      ENDIF
C
  990 CONTINUE
      RETURN
      END
C
C =======================================================================
C
      SUBROUTINE DDLDB
C
C     Author : C.Grab               15-Sep-1989
C  --  Display detector hardware status bits
C
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *41 T
      DATA DV/12./,DVMIN/0./
      IF(FPIKDP) RETURN
      CALL DDLCL(ICBADD)
      VH=VHGHDG(IAREDO)
      VM=VMINDG(IAREDO)+0.5*DVMIN
      HM=HMINDG(IAREDO)
C  Type title:
      CALL DDLDB0(NUM,T)
      CALL DGLEVL(8)
      IF (NUM.LE.0) THEN
         CALL DWRT('DL: ** REVH-bank missing - no detector bits  **  ')
         RETURN
      ENDIF
C
C  Type title :
      VH=VH-DV
      CALL DGTEXT(HM,VH,T,41)
C Add an empty line for title separation:
      T= '                                          '
      VH=VH-DV
      CALL DGTEXT(HM,VH,T,41)
C
C  Display bits now :
      DO 700 N=1,NUM
        CALL DDLDBN(N,T)
        VH=VH-DV
        CALL DGTEXT(HM,VH,T,41)
  700 CONTINUE
      END
CH
C -------------------------------------------------------------------------
C
      SUBROUTINE DDLDB0(NUM,T)
C
C  Author : Christoph  Grab
C           18-Sep-89
C
C  List detector bits in a DALI window, with explanations.
C  Get info from various banks (REVH,XCTN...).
C      Output :  NUM= number of bits to list
C                   = number of lines to print out
C                  T= String to display for the NUM-th bit
C
C -------------------------------------------------------------------------
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      LOGICAL BTEST
      INTEGER NUM
      CHARACTER *41 T,T0(32)
      CHARACTER *4 BNAME
C
      DATA BNAME/'REVH'/
C  Explanation of detector bits :
      DATA T0/
     1 ' 0     HV in ECAL end-cap A all ON and OK',
     1 ' 1     HV in ECAL end-cap B all ON and OK',
     1 ' 2     HV in ECAL barrel    all ON and OK',
     1 ' 3     HV in LCAL           all ON and OK',
     1 ' 4     HV in TPC dE/dx      all ON and OK',
     1 ' 5     HV in ITC            all ON and OK',
     1 ' 6     HV in SATR           all ON and OK',
     1 ' 7     HV in HCAL end-cap A all ON and OK',
     1 ' 8     HV in HCAL end-cap B all ON and OK',
     1 ' 9     HV in HCAL barrel    all ON and OK',
     1 '10     HV in MUON           not available',
     1 '11     HV in BCAL           not available',
     1 '12     HV in VDET           not available',
     1 '13     TRIGGER analog crate     ON and OK',
     1 '14     TRIGGER CAMAC  crate     ON and OK',
     1 '15     HV in TPC            all ON and OK',
     1  14*
     1 '                                         ',
     1 ' Convention :   1=ON   0=OFF or faulty   ',
     1 '                                         '/
C
C      T = 'B# Set Detector bits : 1=ON  0=Off/faulty '
C
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'
C   ---------------------------------------------------------------
C
      IF(FPIKDP) RETURN
      CALL DDLCL(ICBADD)
      NUM=16
C  NUM determines how many lines will be displayed:
      KREVH=IW(NAMIND(BNAME))
      JREVH= KREVH + LMHLEN
      IF(KREVH.EQ.0) NUM=0
      T = 'B# Set Detector bits : 1=ON  0=Off/faulty '
C
C get wanted word : depending on arg.
      IDTB1 = IW(JREVH+1)
      RETURN
C
C  Get detector bits : Display one line for the K-th bit.
C           BTEST counts 0...31;
      ENTRY DDLDBN(K,T)
      IF (KREVH.EQ.0 .OR. K.LT.1 .OR.K.GT.32) RETURN
      T = '                                          '
        T = T0(K)
        T(5:5) = '0'
        IF (BTEST(IDTB1,K-1)) T(5:5) = '1'
C
  990 CONTINUE
      RETURN
      END
C
C =======================================================================
C
      SUBROUTINE DDLDM
C
C     Author : C.Grab               29-Sep-1989
C  Display Muon ....
C  List HCAL muon-detector association in a DALI window, with explanations.
C  Get info from HMAD bank.
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *41 T,t0,t1
      DATA T0/
     1 '  HCAL - MUON  track association :       '/
      DATA T1/
     1 'Trk#   Nfird  Nlas10  IdFlg  TrkAss      '/
      DATA DV/10./,DVMIN/0./
C
      IF(FPIKDP) RETURN
      CALL DDLCL(ICBADD)
      VH=VHGHDG(IAREDO)
      VM=VMINDG(IAREDO)+0.5*DVMIN
      HM=HMINDG(IAREDO)
      CALL DDLDM0(NUM,T)
      CALL DGLEVL(8)
      IF (NUM.LE.0) THEN
         CALL DWR_BELL(2,.FALSE.)
         CALL DWRT('DL: ** HMAD-bank missing - no MUON * ')
         RETURN
      ENDIF
C
C  Type title :
      VH=VH-DV
      CALL DGTEXT(HM,VH,T0,41)
      VH=VH-DV
      CALL DGTEXT(HM,VH,T1,41)
C Add an empty line for title separation:
      T= '                                          '
      VH=VH-DV
      CALL DGTEXT(HM,VH,T,41)
C
C  Display bits now :
      INUM = MIN(NUM,29)
      DO 700 N=1,INUM
        CALL DDLDMN(N,T)
        VH=VH-DV
        CALL DGTEXT(HM,VH,T,41)
  700 CONTINUE
      END
CH
C -------------------------------------------------------------------------
C
      SUBROUTINE DDLDM0(NUM,T)
C
C  Author : Christoph  Grab       29-Sep-89
C
C  List HCAL muon-detector association in a DALI window, with explanations.
C  Get info from HMAD bank
C      Output :  NUM= number of lines to print out
C                  T= String to display for the NUM-th bit
C
C -------------------------------------------------------------------------
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      INTEGER NUM
      CHARACTER *41 T,T1
      CHARACTER *4 BNAME
      CHARACTER *3 DT3
      CHARACTER *2 DT2
C
      DATA BNAME/'HMAD'/
C  Display track information :
      DATA T1/
     1 '  Too many tracks to list ......         '/
C       123456789012345678901234567890123456789012
C
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'
C   ---------------------------------------------------------------
C
      IF(FPIKDP) RETURN
      CALL DDLCL(ICBADD)
C  NUM determines how many lines will be displayed:
      KHMAD=IW(NAMIND(BNAME))
      IF(KHMAD.EQ.0) THEN
         NUM=0
      ELSE
         NUM = LROWS(KHMAD)
      ENDIF
      RETURN
C
C  Now extract the info for each track:
      ENTRY DDLDMN(K,T)
      IF (KHMAD.EQ.0 .OR. K.LT.1) RETURN
      IF (K.GE.29) THEN
        T = T1
        RETURN
      ENDIF
C
C  Number of fired planes :
      NDMNF = ITABL(KHMAD,K,1)
C
C  Number of fired planes in last 10:
      NDMNL = ITABL(KHMAD,K,3)
C
C  Identification flag:
      NDMIF = ITABL(KHMAD,K,11)
C
C  Associated track number:
      NDMTN = ITABL(KHMAD,K,12)
C
      T = '                                          '
      T(2:3)   = DT2(FLOAT(K))
      T(10:11) = DT2(FLOAT(NDMNF))
      T(17:18) = DT2(FLOAT(NDMNL))
      T(24:25) = DT2(FLOAT(NDMIF))
      T(32:34) = DT3(FLOAT(NDMTN))
C
  990 CONTINUE
      RETURN
      END
C =======================================================================
C
      SUBROUTINE DDLTM
C
C  Author : C.Grab               24-June-1990
C  Display Muon information
C  List track muon-detector association in a DALI window, with explanations.
C  Get info from MCAD bank.
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *41 T,t0,t1
      CHARACTER *3 DT3
      DATA T0/
     1 ' MUON - Track association for     Tracks '/
      DATA T1/
     1 'NassH1 NassH2 DistH1  DistH2 Ass.Track   '/
      DATA DV/10./,DVMIN/0./
C
      IF(FPIKDP) RETURN
      CALL DDLCL(ICBADD)
      VH=VHGHDG(IAREDO)
      VM=VMINDG(IAREDO)+0.5*DVMIN
      HM=HMINDG(IAREDO)
      CALL DDLTM0(NUM,T)
      CALL DGLEVL(8)
      IF (NUM.LE.0) THEN
         CALL DWR_BELL(2,.FALSE.)
         CALL DWRT('DL: ** MCAD-bank missing - no MU association **')
         RETURN
      ENDIF
C
C  Type title :
      VH=VH-DV
      T0(31:33) = DT3(FLOAT(NUM))
      CALL DGTEXT(HM,VH,T0,41)
      VH=VH-DV
      CALL DGTEXT(HM,VH,T1,41)
C Add an empty line for title separation:
      T= '                                          '
      VH=VH-DV
      CALL DGTEXT(HM,VH,T,41)
C
C  Display info now :
      INUM = MIN(NUM,29)
      DO 700 N=1,INUM
        CALL DDLTMN(N,T)
        VH=VH-DV
        CALL DGTEXT(HM,VH,T,41)
  700 CONTINUE
      END
CH
C -------------------------------------------------------------------------
C
      SUBROUTINE DDLTM0(NUM,T)
C
C  Author : C.Grab               24-June-1990
C
C  List tracks  muon-detector association in a DALI window, with explanations.
C  Get info from MCAD bank
C      Output :  NUM= number of lines to print out
C                  T= String to display for the NUM-th bit
C
C -------------------------------------------------------------------------
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      INTEGER NUM
      CHARACTER *41 T,T1
      CHARACTER *4 BNAME
      CHARACTER *3 DT3
      CHARACTER *4 DT4
C
      DATA BNAME/'MCAD'/
C  Display track information :
      DATA T1/
     1 '  Too many tracks to list ......         '/
C       123456789012345678901234567890123456789012
C
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'
C   ---------------------------------------------------------------
C
      IF(FPIKDP) RETURN
      CALL DDLCL(ICBADD)
C  NUM determines how many lines will be displayed:
      KMCAD=IW(NAMIND(BNAME))
      IF(KMCAD.EQ.0) THEN
         NUM=0
      ELSE
         NUM = LROWS(KMCAD)
      ENDIF
      RETURN
C
C  ------------------------------------
C  Now extract the info for each track:
      ENTRY DDLTMN(K,T)
      IF (KMCAD.EQ.0 .OR. K.LT.1) RETURN
      IF (K.GE.29) THEN
        T = T1
        RETURN
      ENDIF
C
C  Number of associated hits :
      NTMH1 = ITABL(KMCAD,K,1)
      NTMH2 = ITABL(KMCAD,K,2)
C
C  Distance hit tracks:
      NTMD1 = ITABL(KMCAD,K,3)
      NTMD2 = ITABL(KMCAD,K,4)
C
C  Associated track number:
      NTMTN = ITABL(KMCAD,K,9)
CCC     1 'NassH1 NassH2 DistH1  DistH2 Ass.Track   '/
C
      T = '                                          '
      T(2:4) = DT3(FLOAT(NTMH1))
      T(9:11) = DT3(FLOAT(NTMH2))
      T(15:18) = DT4(FLOAT(NTMD1))
      T(23:26) = DT4(FLOAT(NTMD2))
      T(32:34) = DT3(FLOAT(NTMTN))
C
  990 CONTINUE
      RETURN
      END
C ====================================================================
*DK DDLTR1
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDLTR1
CH
      SUBROUTINE DDLTR1(FCUT,FION)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DC
      PARAMETER (MCTRDC=1000)
      COMMON /DCOTR/ FDTRDC,FVTRDC,NCTRDC(0:MCTRDC)
      LOGICAL FDTRDC,FVTRDC
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DW
      COMMON /DWORKC/ WORKDW(3000)
      DIMENSION NWRKDW(3000)
      EQUIVALENCE (WORKDW,NWRKDW)
C------------------------------------------------------------------- DT
      COMMON /DTRAKC/ FORDDT,NORDDT(999),NCOLDT(999)
      COMMON /DTRAKT/ ULABDT(999),COLRDT(999)
      COMMON /DTRAKF/ FHTRDT,FCOLDT
      LOGICAL FORDDT,FHTRDT,FCOLDT
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DATA DVMIN/11./,DVMAX/15./
      CHARACTER *82 T
      CHARACTER *3 DT3
      CHARACTER *4 DT4
      LOGICAL FOUT,FCUT,FION
      CALL DDLCL(ICTPDD)
      VH=VHGHDG(IAREDO)
      VM=VMINDG(IAREDO)+0.5*DVMIN
      HM=HMINDG(IAREDO)
C     ORDER TRACKS IN PHI
      CALL DORDTR
      CALL DHTINP
      IF(BNUMDB(2,FRFTDB).EQ.0) RETURN
      FTPCDR=.TRUE.
      CALL DSCTR0(FVTRDC,NTCOL)
      CALL DV0(FRFTDB,NUM1,NUM2,FOUT)
      NUM2=BNUMDB(2,FRFTDB)
      CALL DGLEVL(8)
      VH=VH-DVMAX
C     TYPE HEADER
      IF(.NOT.FPIKDP) THEN
        IF(FION) THEN
          CALL DDLION(0,T)
        ELSE
          CALL DVTRT1(0,T)
        END IF
        CALL DGTEXT(HM,VH,T,42)
      END IF
      IF(FOUT) GO TO 99
      MDLRDP=FRFTDB
      NT=0
C     COUNT # OF TRACKS AFTER TRACK CUT
      DO 700 N=1,NUM2
        IF(FCUT) THEN
          IF(FNOTDT(N)) GO TO 700
        END IF
        NT=NT+1
  700 CONTINUE
      IF(NT.LE.0) THEN
        CALL DWRT('No tracks left after cut.')
        RETURN
      END IF
C     CALCULATE LINE SPACING
      DV=(VH-VM)/NT
C     TOO MANY TRACKS
      IF(DV.LT.DVMIN) THEN
        DV=DVMIN
C       order tracks IN momentum
        CALL VZERO(NWRKDW,NT)
        DO   710  K1=1,NT-1
           CALL DVTRTP(K1,P1)
           P1=ABS(P1)
           DO   720  K2=K1+1,NT
              CALL DVTRTP(K2,P2)
              P2=ABS(P2)
              IF(P1.LT.P2) THEN
                 NWRKDW(K1)=NWRKDW(K1)+1
              ELSE
                 NWRKDW(K2)=NWRKDW(K2)+1
              END IF
  720      CONTINUE
  710   CONTINUE
C       FIND MINIMUM MOMENTUM OF DISPLAYED TRACKS
C                         IF(.NOT.FPIKDP) THEN
        PMIN=9999.
        VM=VM+DV
        DLOST=NT
        NT=(VH-VM)/DV
        DO 725 N=1,NUM2
          IF(NWRKDW(N).GE.NT) GO TO 725
          IF(FCUT) THEN
            IF(FNOTDT(N)) GO TO 725
          END IF
          DLOST=DLOST-1.
          CALL DVTRTP(N,P1)
          P1=ABS(P1)
          IF(P1.LT.PMIN) PMIN=P1
  725   CONTINUE
        IF(.NOT.FPIKDP) THEN
C         TYPE FOOTER
C            123456789 123456789 123456789 123456789
          T='123 Tracks below 12.3 GEV not displayed.'
          T(1:3)=DT3(DLOST)
          T(18:21)=DT4(PMIN)
          CALL DGTEXT(HM,VM-DV,T,40)
        END IF
C                                              END IF
C       DISPLAY TRACKS
        DO 730 I=1,NUM2
          N=NORDDT(I)
          IF(NWRKDW(N).GE.NT) GO TO 730
          IF(FCUT) THEN
            IF(FNOTDT(N)) GO TO 730
          END IF
          IF(FVTRDC) THEN
            CALL DGLEVL(NCTRDC(N))
          ELSE
            CALL DGLEVL(NTCOL)
          END IF
C         CALL DCOLN(N)
          VH=VH-DV
          IF(VH.LT.VM) GO TO 99
          IF(FION) THEN
            CALL DDLION(N,T)
          ELSE
            CALL DVTRT1(N,T)
          END IF
          IF(FPIKDP) THEN
            CALL DDLPIK(N,HM,VH)
          ELSE
            CALL DGTEXT(HM,VH,T,42)
          END IF
  730   CONTINUE
      ELSE
C       TYPE TRACKS IF NOT TOO MANY
        DV=MIN(DVMAX,DV)
        DO 740 I=1,NUM2
          N=NORDDT(I)
          IF(FCUT) THEN
            IF(FNOTDT(N)) GO TO 740
          END IF
C         CALL DCOLN(N)
          IF(FVTRDC) THEN
            CALL DGLEVL(NCTRDC(N))
          ELSE
            CALL DGLEVL(NTCOL)
          END IF
          VH=VH-DV
C         IF(VH.LT.VM) GO TO 99
          IF(FION) THEN
            CALL DDLION(N,T)
          ELSE
            CALL DVTRT1(N,T)
          END IF
          IF(FPIKDP) THEN
            CALL DDLPIK(N,HM,VH)
          ELSE
C           NOW TYPE THE TEXT FOR THE TRACKS
            CALL DGTEXT(HM,VH,T,42)
          END IF
  740   CONTINUE
      END IF
   99 END
CH
C
C ------------------------------------------------------------------------
*DK DDLTR2
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDLTR2
CH
      SUBROUTINE DDLTR2
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DC
      PARAMETER (MCTRDC=1000)
      COMMON /DCOTR/ FDTRDC,FVTRDC,NCTRDC(0:MCTRDC)
      LOGICAL FDTRDC,FVTRDC
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DATA DV/15./,DH/110./
      CHARACTER *12 T
      CHARACTER *3 DT3
      LOGICAL FOUT
      DIMENSION NROW(0:12),NCOL(0:12)
C                W  1  2  3  4  5  6  U  D  L  M  R  S
      DATA NROW/40,20,20,20,20,20,20,20,20,40,40,40,40/
      DATA NCOL/ 9, 3, 3, 3, 3, 3, 3, 6, 6, 3, 3, 3, 6/
      DATA DHP/30./,DVP/5./
      CALL DSCTR0(FVTRDC,NTCOL)
      CALL DV0(FRFTDB,NUM1,NUM2,FOUT)
      NUM2=BNUMDB(2,FRFTDB)
      IF(NUM2.LE.0) THEN
        IF(.NOT.FPIKDP) CALL DWRT(' FRFT bank is empty.')
        RETURN
      END IF
      H=HMINDG(IAREDO)
      CALL DDLCL(ICTPDD)
      IF(FPIMDP) THEN
        N1=NPIKDP-1
        NC=N1/NROW(IAREDO)
        NR=MOD(N1,NROW(IAREDO))+2
        HHPKDP=H+NC*DH+DHP
        VVPKDP=VHGHDG(IAREDO)-NR*DV+DVP
        DPIKDP=0.
        RETURN
      END IF
      IF(FPIKDP) THEN
        NC=(HPIKDP-H)/DH
        NR=(VHGHDG(IAREDO)-VPIKDP)/DV
        NPIKDP=NROW(IAREDO)*NC+NR
        HHPKDP=H+NC*DH+DHP
        VVPKDP=VHGHDG(IAREDO)-(NR+1)*DV+DVP
        IF(NPIKDP.GE.1.AND.NPIKDP.LE.NUM2) THEN
          DPIKDP=0.
          MDLPDP=FRFTDB
        END IF
      RETURN
      END IF
C     CALL DORDTR
      CALL DHTINP
      FTPCDR=.TRUE.
C     CALL DCOL0('TR',MTPCDL,IVNTDV,ICOL)
      N=0
      DO 730 IC=1,NCOL(IAREDO)
        V=VHGHDG(IAREDO)-DV
        CALL DVTRT2(0,T)
        CALL DQLEVL(ICTXDD)
        CALL DGTEXT(H,V,T,12)
        DO 740 IR=1,NROW(IAREDO)
          N=N+1
          IF(N.GT.NUM2) RETURN
          V=V-DV
          CALL DVTRT2(N,T)
          IF(FNOTDT(N)) T(10:10)='X'
C         CALL DCOLN(N)
          IF(FVTRDC) THEN
            CALL DGLEVL(NCTRDC(N))
          ELSE
            CALL DGLEVL(NTCOL)
          END IF
          CALL DGTEXT(H,V,T,12)
  740   CONTINUE
        H=H+DH
  730 CONTINUE
      CALL DWRT(DT3(FLOAT(NUM2-N+1))//' tracks not displayed')
      END
*DK DDLPIK
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDLPIK
CH
      SUBROUTINE DDLPIK(N,H,V)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DATA DH/11./,DV/5.5/
      IF(FPIMDP.AND.N.NE.NPIKDP) RETURN
      D=ABS(V-VPIKDP)
      IF(D.LT.DPIKDP) THEN
         DPIKDP=D
         NPIKDP=N
         FNTRDP=FKTRDP
         MDLPDP=MDLRDP
         HHPKDP=H+DH
         VVPKDP=V+DV
      END IF
      END
*DK DDRAWA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDRAWA
CH
      SUBROUTINE DDRAWA(DIS,F1,H1,V1,F2,H2,V2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C_CG     8-May-1989 C.Grab  Adapted to CERNVM
C
C!:Draw polyline from H1,V1 with helix parameter F1
C    Inputs    :
C    Outputs   :
C    Note      :DIS = external function to calculate H,V at F
C                     for given projection
C    Called by :
C ---------------------------------------------------------------------
      LOGICAL FC
      EXTERNAL DIS
      DM=999.
      FC=.FALSE.
      CALL DIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,DM,FC)
      IF(FC) THEN
         CALL DDRAWB(DIS,F1,H1,V1,FM,HM,VM,DM)
         CALL DDRAWB(DIS,FM,HM,VM,F2,H2,V2,DM)
      END IF
      END
*DK DDRAWB
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDRAWB
CH
      SUBROUTINE DDRAWB(DIS,F1,H1,V1,F2,H2,V2,D12)
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
      LOGICAL FC
      EXTERNAL DIS
      DM=D12
      FC=.FALSE.
      CALL DIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,DM,FC)
      IF(FC) THEN
         CALL DDRAWC(DIS,F1,H1,V1,FM,HM,VM,DM)
         CALL DDRAWC(DIS,FM,HM,VM,F2,H2,V2,DM)
      END IF
      END
*DK DDRAWC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDRAWC
CH
      SUBROUTINE DDRAWC(DIS,F1,H1,V1,F2,H2,V2,D12)
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
      LOGICAL FC
      EXTERNAL DIS
      DM=D12
      FC=.FALSE.
      CALL DIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,DM,FC)
      IF(FC) THEN
         CALL DDRAWD(DIS,F1,H1,V1,FM,HM,VM,DM)
         CALL DDRAWD(DIS,FM,HM,VM,F2,H2,V2,DM)
      END IF
      END
*DK DDRAWD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDRAWD
CH
      SUBROUTINE DDRAWD(DIS,F1,H1,V1,F2,H2,V2,D12)
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
      LOGICAL FC
      EXTERNAL DIS
      DM=D12
      FC=.FALSE.
      CALL DIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,DM,FC)
      IF(FC) THEN
         CALL DDRAWE(DIS,F1,H1,V1,FM,HM,VM,DM)
         CALL DDRAWE(DIS,FM,HM,VM,F2,H2,V2,DM)
      END IF
      END
*DK DDRAWE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDRAWE
CH
      SUBROUTINE DDRAWE(DIS,F1,H1,V1,F2,H2,V2,D12)
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
      LOGICAL FC
      EXTERNAL DIS
      DM=D12
      FC=.FALSE.
      CALL DIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,DM,FC)
      IF(FC) THEN
         CALL DDRAWF(DIS,F1,H1,V1,FM,HM,VM,DM)
         CALL DDRAWF(DIS,FM,HM,VM,F2,H2,V2,DM)
      END IF
      END
*DK DDRAWF
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDRAWF
CH
      SUBROUTINE DDRAWF(DIS,F1,H1,V1,F2,H2,V2,D12)
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
      LOGICAL FC
      EXTERNAL DIS
      DM=D12
      FC=.FALSE.
      CALL DIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,DM,FC)
      IF(FC) THEN
         CALL DDRAWG(DIS,F1,H1,V1,FM,HM,VM,DM)
         CALL DDRAWG(DIS,FM,HM,VM,F2,H2,V2,DM)
      END IF
      END
*DK DDRAWG
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDRAWG
CH
      SUBROUTINE DDRAWG(DIS,F1,H1,V1,F2,H2,V2,D12)
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
      LOGICAL FC
      EXTERNAL DIS
      DM=D12
      FC=.FALSE.
      CALL DIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,DM,FC)
      IF(FC) THEN
         CALL DDRAWH(DIS,F1,H1,V1,FM,HM,VM,DM)
         CALL DDRAWH(DIS,FM,HM,VM,F2,H2,V2,DM)
      END IF
      END
*DK DDRAWH
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDRAWH
CH
      SUBROUTINE DDRAWH(DIS,F1,H1,V1,F2,H2,V2,D12)
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
      LOGICAL FC
      EXTERNAL DIS
      DM=D12
      FC=.FALSE.
      CALL DIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,DM,FC)
      IF(FC) THEN
         CALL DDRAWI(DIS,F1,H1,V1,FM,HM,VM,DM)
         CALL DDRAWI(DIS,FM,HM,VM,F2,H2,V2,DM)
      END IF
      END
*DK DDRAWI
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDRAWI
CH
      SUBROUTINE DDRAWI(DIS,F1,H1,V1,F2,H2,V2,D12)
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
      LOGICAL FC
      EXTERNAL DIS
      DM=D12
      FC=.FALSE.
      CALL DIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,DM,FC)
      IF(FC) THEN
         CALL DDRAWJ(DIS,F1,H1,V1,FM,HM,VM,DM)
         CALL DDRAWJ(DIS,FM,HM,VM,F2,H2,V2,DM)
      END IF
      END
*DK DDRAWJ
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDRAWJ
CH
      SUBROUTINE DDRAWJ(DIS,F1,H1,V1,F2,H2,V2,D12)
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
      LOGICAL FC
      EXTERNAL DIS
      DM=D12
      FC=.FALSE.
      CALL DIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,DM,FC)
      IF(FC) THEN
         CALL DDRAWK(DIS,F1,H1,V1,FM,HM,VM,DM)
         CALL DDRAWK(DIS,FM,HM,VM,F2,H2,V2,DM)
      END IF
      END
*DK DDRAWK
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDRAWK
CH
      SUBROUTINE DDRAWK(DIS,F1,H1,V1,F2,H2,V2,D12)
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
      LOGICAL FC
      EXTERNAL DIS
      DM=D12
      FC=.FALSE.
      CALL DIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,DM,FC)
      IF(FC) THEN
         CALL DDRAWL(DIS,F1,H1,V1,FM,HM,VM,DM)
         CALL DDRAWL(DIS,FM,HM,VM,F2,H2,V2,DM)
      END IF
      END
*DK DDRAWL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDRAWL
CH
      SUBROUTINE DDRAWL(DIS,F1,H1,V1,F2,H2,V2,D12)
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
      LOGICAL FC
      EXTERNAL DIS
      DM=D12
      FC=.FALSE.
      CALL DIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,DM,FC)
      IF(FC) THEN
         CALL DDRAWM(DIS,F1,H1,V1,FM,HM,VM,DM)
         CALL DDRAWM(DIS,FM,HM,VM,F2,H2,V2,DM)
      END IF
      END
*DK DDRAWM
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDRAWM
CH
      SUBROUTINE DDRAWM(DIS,F1,H1,V1,F2,H2,V2,D12)
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
      LOGICAL FC
      EXTERNAL DIS
      DM=D12
      FC=.FALSE.
      CALL DIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,DM,FC)
      IF(FC) THEN
         CALL DDRAWN(DIS,F1,H1,V1,FM,HM,VM,DM)
         CALL DDRAWN(DIS,FM,HM,VM,F2,H2,V2,DM)
      END IF
      END
*DK DDRAWN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDRAWN
CH
      SUBROUTINE DDRAWN(DIS,F1,H1,V1,F2,H2,V2,D12)
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
      LOGICAL FC
      EXTERNAL DIS
      DM=D12
      FC=.FALSE.
      CALL DIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,DM,FC)
      IF(FC) THEN
         CALL DDRAWO(DIS,F1,H1,V1,FM,HM,VM,DM)
         CALL DDRAWO(DIS,FM,HM,VM,F2,H2,V2,DM)
      END IF
      END
*DK DDRAWO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DDRAWO
CH
      SUBROUTINE DDRAWO(DIS,F1,H1,V1,F2,H2,V2,D12)
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
      LOGICAL FC
      EXTERNAL DIS
      DM=D12
C     FC=.FALSE.
C     CALL DIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,DM,FC)
C     IF(FC) THEN
        FC=.TRUE.
        CALL DIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,DM,FC)
C     END IF
      END
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DDRFLG
CH
      SUBROUTINE DDRFLG
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:SET UP DETECTOR FLAGS
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DU
      PARAMETER (MPNPDU=60)
      REAL DACUDU
      COMMON /DUSDAC/ NUNIDU,
     1 WAITDU,ECCHDU,BOXSDU,ECSQDU,HCSQDU,
     1 HIMXDU,RESCDU,SLOWDU,WISUDU,SETTDU,
     1 USLVDU,SYLRDU,SDATDU,RZDHDU,ROTADU,
     1 SCMMDU,CLMMDU,CUMMDU,WIMIDU,CCMIDU,
     1 ECMIDU,HCMIDU,SHTFDU(2)    ,FTDZDU,
     1 FRNLDU,DACUDU,SHECDU,SHHCDU,DRLIDU,
     1 SQLCDU,CHLCDU,HEADDU,VRDZDU,CHHCDU,
     1 AREADU,DFWIDU(0:1)  ,DFLGDU,PBUGDU,
     1 CHTFDU,RNCLDU,PR43DU,PR44DU,PR45DU,
     1 DSOFDU,TPOFDU,ROVDDU,ROECDU,ROHCDU,
     1 HSTRDU,REALDU,PR53DU,PR54DU,PR55DU,
     1 PR56DU(4),                  PR60DU
      DIMENSION PARADU(MPNPDU)
      EQUIVALENCE (PARADU,WAITDU)
      COMMON /DUSDA2/PLIMDU(2,MPNPDU)
      COMMON /DUSDAT/ TPR1DU(MPNPDU)
      CHARACTER *2 TPR1DU
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      LOGICAL FDET(6)
      EQUIVALENCE (FDET,FVDEDR)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PL1,J_PL2,J_PFR,J_PTO'
      CALL DPARAM(11
     &  ,J_PL1,J_PL2,J_PFR,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(DFLGDU.LE.0.) GO TO 100
      IF(PARADA(2,J_PFR).GE.PARADA(2,J_PTO).OR
     &  .PARADA(2,J_PL1).GE.PARADA(2,J_PL2)) THEN
        CALL DWRT('FR must be smaller than TO. Give correct value.')
        FNOPDR=.TRUE.
        RETURN
      ELSE
        FNOPDR=.FALSE.
      END IF
      DO 700 K=1,6
        IF(PARADA(2,J_PL1).GE.DET2DR(K).OR.
     &     PARADA(2,J_PL2).LE.DET1DR(K)) THEN
          FDET(K)=.FALSE.
        ELSE
          FDET(K)=.TRUE.
        END IF
  700 CONTINUE
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DDRFLO
CH
      ENTRY DDRFLO
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:CALCULATE DISTANCE
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
  100 FNOPDR=.FALSE.
      DO 701 K=1,6
        FDET(K)=.TRUE.
  701 CONTINUE
      END
*DK DDSTPL
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DDSTPL
CH
      SUBROUTINE DDSTPL(H1,V1,H2,V2,HP,VP,HL,VL,D)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:SET UP DETECTOR FLAGS
C    Inputs    :H1,V1,H2,V2 DEFINE A LINE; HP,VP = POINT
C    Outputs   :HL,VL=NEXT POINT ON THE LINE TO (HP,VP) D=DISTANCE
C
C ---------------------------------------------------------------------
      H21=H2-H1
      V21=V2-V1
      HL=( H1*V21*V21+HP*H21*H21+(VP-V1)*V21*H21 )
     &         /( V21*V21+H21*H21)
      VL=( HL-H1)*V21/H21+V1
      D=SQRT( (HP-HL)*(HP-HL)+(VP-VL)*(VP-VL) )
      END
*DK IDSTO
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ IDSTO
CH
      INTEGER FUNCTION IDSTO(I,NW)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     This function is necessary to eliminate ISTODS from all packages, which
C     are common to ATLANTIS and DALI, as ISTODS is dimensioned differently
C     in ATLANTIS and DALI.

C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DS
      PARAMETER (MPARDS=12,MVIRDS=-4,MPNPDS=200,MWUSDS=4)
      COMMON /DSTRC1/ IAUTDS,ISTODS(8,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC2/PSTODS(2,MPNPDS,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC3/TVRUDS(6,MVIRDS:MPARDS,0:MWUSDS)
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77

      IDSTO=ISTODS(I,NW,IWUSDO)
      END
