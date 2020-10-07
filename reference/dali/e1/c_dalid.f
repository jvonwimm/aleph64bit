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
      INCLUDE 'DALI_CF.INC'
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
      INCLUDE 'DALI_CF.INC'
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
      INCLUDE 'DALI_CF.INC'
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
C                                       ! PARADA(4,IFRTO)=
C                                       ! 0 NO CALCULATION
C                                       ! 1 LAST INPUT BY LOGO
C                                       ! 2 LAST INPUT BY NUMBER
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
         IF(TIN.EQ.'RB'.OR.PARADA(4,IFRTO).EQ.2) THEN
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
   21       PARADA(4,IFRTO)=0.
         ELSE IF(TIN.EQ.'IN'.OR.TIN.EQ.'NZ'.
     &    OR.PARADA(4,IFRTO).EQ.1) THEN
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
            PARADA(4,IFRTO)=0.
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
               PARADA(4,NP)=1.
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
               PARADA(4,NP)=1.
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
     &  'J_PFR,J_PTO'
      CALL DPARAM(11
     &  ,J_PFR,J_PTO)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(NP.EQ.J_PFR.OR.NP.EQ.J_PTO) PARADA(4,NP)=2.
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
