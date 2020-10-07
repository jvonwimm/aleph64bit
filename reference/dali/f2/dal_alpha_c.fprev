*DK DAC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++  DAC
CH
      SUBROUTINE DAC
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
      DATA IAR/9/
C     CHARACTER *20 TWT,TWW
C     DATA TWT/'(IC:CC:CF:DW:DO)\J'/
C     DATA TWW/'(IC:CC:CF:DW)\J'/
      DIMENSION KHIT(7),KLST(7),KPRO(7)
      PARAMETER (MP1=3,MP9=11,MOPT=6)
      DIMENSION PR(4,MP9)
      DATA PR/0.,0.140,99.,0.,
     &       -99.,0.,199.,0.,
     &       -99.,0.,199.,0.,        ! CHANGE ALS HERE IF MLINDA IS MOFIFIED
     &       -99.,0.,199.,0.,
     &       -99.,0.,199.,0.,
     &       -99.,0.,199.,0.,
     &       -99.,0.,199.,0.,
     &       -99.,0.,199.,0.,
     &       -99.,0.,199.,0.,
     &       -99.,0.,199.,0.,
     &       -99.,0.,199.,0./
      DATA LJET/8/
      CHARACTER *4 TNAM
      CHARACTER *11 TGT
      DATA TGT/'(GG:AP:T) :'/
      CHARACTER *1 TCOR,TNUM(9)
      DATA TNUM/'1','2','3','4','5','6','7','8','*'/
C                -3   -2   -1    0    1    2    3    4
      CHARACTER *2 TPR(MP9),TANSW,TLIST,TMOD(0:4),TWTA(-1:1)
      DATA TPR/'RM','TO',9*'**'/
      DATA TMOD/'  ','OB','SE','AP','DE'/
      DATA TWTA/'UT','UD','US'/
      CHARACTER *2 TOPT(MOPT),TOPS,TLOCK
      DATA TOPS/'EF'/,TLOCK/'  '/
      DATA TOPT/'CH','RE','EJ','PC','EF','**'/
      DIMENSION LIST(99),IOBJ(99),LCOL(6)
C     LOGICAL FLST(99)    ! change in DV_QVEC_IN also.
      DATA ISLST/5/,IMO/0/,ITT/0/,IRC/0/,NPI3/3/,NWTA/1/
      LOGICAL FSTRT,FYES,FTT(2)
      DATA FSTRT/.FALSE./,FTT/.TRUE.,.FALSE./
      CHARACTER *49 T1,T2
C                       1         2         3         4         5
C              12345678901234567890123456789012345678901234567890
      DATA T1/'P?:W?:US ALPHA list = CH(col=frft)      LK     OB'/
      DATA T2/'RM_12345 opt.=EF  HC_123 YJ_1234 YD_1234 EV_12345'/
C                    ident c  P   phi theta  M  name
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_AFM,J_ATE'
      CALL DPARAM(39
     &  ,J_AFM,J_ATE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DPARP0
      CALL DPAROP(100,'AHC_AYJ_AYD_AEV')
      CALL DV_QVEC_IN(NFSTDA,NLSTDA)
      CALL DPARGI(39,'ALI',ISLST)
      CALL DPARGI(98,'APP',IPOPI)
C     CALL DGSPPT(NPI3,IPOPI)
      FDTRDC=.FALSE.
      CALL DSCTR
      IF(.NOT.FGETDO) NWTA=0
C     ............................................................. DRAW HEADER
  930 IRC=0
      TLIST=TLSTDA(ISLST)
      T1(7:8)=TWTA(NWTA)
      T1(23:24)=TLIST
      CALL DPARGV(98,'ACF',2,CHCF)
      IF((TLIST.EQ.'CH'.OR.TLIST.EQ.'EF').AND.CHCF.EQ.1.) THEN
        T1(25:34)='(col=frft)'
      ELSE
        T1(25:34)=' '
      END IF
      T1(41:42)=TLOCK
      T1(48:49)=TMOD(IMO)
      CALL DTYPT('TYPE',TPICDO,0,0,0,' ',T1)
      T2(15:16)=TOPS
      CALL DTYPT('SAVE',' ',1,MP1-1,PR,TPR,T2)
      CALL DTYPT('TYPE',' ',N_1_DA,N_2_DA,PARADA,TPOPDA,T2)
      TXTADW='Default color set to ='
      L=24
      DO K=1,9
        IF(IOCODA(K).NE.0) THEN
          TXTADW(L:L+2)=TLSTDA(K)//':'
          CALL DACOL(IOCODA(K),TXTADW(L+3:L+4))
          L=L+6
        END IF
      END DO
      IF(L.NE.24) CALL DWRT(TXTADW(1:47))
      IF(L.GT.47) CALL DWRT('      '//TXTADW(48:80))
C     .............................................................. OPERATOR
  936 DO K=2,MP9
        PR(2,K)=-999.
      END DO
      IF(IMO.NE.0) CALL DOPERS(2,MP1,1.)
      CALL DAREAW(0)
      IF(NWTA.EQ.0) CALL DGZOOM(6,IAREDO,0,0)
      IWUS=IWUSDO
      IWUSDO=0
      CALL DOPER(1,1,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  1,MP9,TPR,PR,
     &  NEXEC,CHG,TANSW)
      IWUSDO=IWUS
      IF(NWTA.EQ.0) CALL DGZOOM(6,-1,0,0)
      CALL DAREAW(1)
C     .................................................. STORE SELECTED TRACKS
      IF(IMO.NE.0.AND.(PR(2,MP1).NE.-999..OR.PR(2,2).NE.-999.)) THEN
        IF(IMO.LE.2) CALL VZERO(FSELDA,MTOTDA)
        IF(IMO.EQ.1) THEN
          DO K=MP1,MP9
            IF(PR(2,K).GT.0.) THEN
              JOB1=PR(2,K)
              JAL=JOB1+NFSTDA(ISLST)-1
              DO L=1,NLINDA
                IF(IALPDA(L).EQ.JAL) THEN
                  FSELDA(L)=.TRUE.
                  GO TO 937
                END IF
              END DO
              NLINDA=NLINDA+1
              IALPDA(NLINDA)=JAL               ! # from ALPHA  example: 134
              ILSTDA(NLINDA)=ISLST             ! 0,1, ... 9
              FSELDA(NLINDA)=.TRUE.
            END IF
  937     END DO
C         ............................................................. TO
          IF(PR(2,2).GT.0.) THEN
            JOB2=PR(2,2)
            IF(JOB2.GE.JOB1) THEN
              JSTP=1
            ELSE
              JSTP=-1
            END IF
            DO J=JOB1,JOB2,JSTP
              JAL=J+NFSTDA(ISLST)-1
              DO K=MP1,MP9
                DO L=1,NLINDA
                  IF(IALPDA(L).EQ.JAL) THEN
                    FSELDA(L)=.TRUE.
                    GO TO 2937
                  END IF
                END DO
                NLINDA=NLINDA+1
                IALPDA(NLINDA)=JAL               ! # from ALPHA  example: 134
                ILSTDA(NLINDA)=ISLST             ! 0,1, ... 9
                FSELDA(NLINDA)=.TRUE.
 2937         END DO
            END DO
          END IF
        ELSE
          IF(PR(2,MP1).NE.-999.) SS=SIGN(1.,PR(2,MP1))
          DO K=MP1,MP9
            IF(PR(2,K).NE.-999.) THEN
              JLIN1=SS*ABS(PR(2,K))
              IF(IMO.EQ.4) THEN
                FSELDA(JLIN1)=.FALSE.
              ELSE
                FSELDA(JLIN1)=.TRUE.
              END IF
            END IF
          END DO
C         ............................................................. TO
          IF(PR(2,2).NE.-999.) THEN
            JLIN2=SS*ABS(PR(2,2))
            IF(JLIN2.GE.JLIN1) THEN
              JSTP=1
            ELSE
              JSTP=-1
            END IF
            DO J=JLIN1,JLIN2,JSTP
              IF(IMO.EQ.4) THEN
                FSELDA(J)=.FALSE.
              ELSE
                FSELDA(J)=.TRUE.
              END IF
            END DO
          END IF
        END IF
        ITT=0
        IRC=0
        IF(NEXEC.EQ.3) GO TO 942
      END IF
      IF(TANSW.NE.'TT') ITT=0
      CALL DOPERG(J2,J9)
      IF(J2.NE.2.OR.J9.LT.1) IMO=0
C     ................................................................... PICK
C??   .......................................      IRC=0
C      CALL DPOS(TANSW,FDUM,TDUM,NEXEC,CHG)
C      IF(NEXEC.EQ.3) GO TO 930
      GO TO (910,920,930,940), NEXEC
  910 CALL DGSPPT(1,1)
      CALL DQSWIN(0,LDUM)
C     IF(TANSW.NE.'GW') IAREDO=IARLS
      RETURN
  920 CALL DO_STR('IV"DF"to"BL: color')
C     .................................. color selection must be first (IRC!)
      CALL DACOL_NUM(TANSW,M)
      IF(M.GT.-2) THEN
        IF(IRC.EQ.1) THEN
C         .................................. get line # on cursor position
          CALL DAC_POINTER_1_LINE(L)
          CALL DV_QVEC_SET_COL(IALPDA(L),M)
        ELSE
          DO L=NCALDA,NLINDA
            IF(FSELDA(L)) CALL DV_QVEC_SET_COL(IALPDA(L),M)
          END DO
        END IF
        GO TO 940
      END IF
      IRC=0
      CALL DO_STR('DI: default colors into interactive colors')
      IF(TANSW.EQ.'DI') THEN
        CALL DPARGI(98,'ACF',ICHCF)
        DO L=1,NLINDA
          IF(FSELDA(L)) THEN
            CALL DV_QVEC_DEF_COL(NLIST,ICHCF,NCTRDC(1))
            CALL DV_QVEC_SET_COL(IALPDA(L),-9)
          END IF
        END DO
        GO TO 940
      END IF
      IF(TANSW.EQ.'tc'.AND.(ISLST.EQ.1.OR.ISLST.EQ.5)) THEN
        CALL DPARGV(98,'ACF',2,CHCF)
        CHCF=-CHCF
        CALL DPARSV(98,'ACF',2,CHCF)
        CALL DV_QVEC_DEF_COL_IN
        IF(CHCF.GT.0.) THEN
          CALL DWRT('Color of CH and EF = color of FRFT.')
        ELSE
          CALL DWRT('Color of CH and EF = log(momentum)')
        END IF
        GO TO 940
      END IF
      CALL DO_STR('W0"WW: set up windows')
      CALL DAREA('W',TANSW,0,12,IAREDO,FYES)
      IF(FYES) THEN
        CALL DQHL_W
        GO TO 930
      END IF
      CALL DO_STR(
     &  'UT"UD"US: Use terminal, DALI window, 2. window')
      DO K=-1,1
        IF(TANSW.EQ.TWTA(K)) THEN
          NWTA=K
          GO TO 930
        END IF
      END DO
      IF(TANSW.EQ.'AC') THEN
        CALL DQDWIN(1)
        CALL DACD_0
        GO TO 936
      END IF
      CALL DO_STR('AA: append to selection via cursor')
      IF(TANSW.EQ.'AA') THEN
        CALL DAC_POINTER_1_LINE(L)
        FSELDA(L)=.TRUE.
        ITT=1
        GO TO 940
      END IF
      CALL DO_STR('EE: erase from selection via cursor')
      IF(TANSW.EQ.'EE') THEN
        CALL DAC_POINTER_1_LINE(L)
        FSELDA(L)=.FALSE.
        ITT=2
        GO TO 940
      END IF
      CALL DO_STR('TT: append or erase up to here')
      IF(TANSW.EQ.'TT') THEN
        CALL DAC_POINTER_LINES(FTT(ITT))
        GO TO 940
      END IF
      CALL DO_STR('RC: read cursor for colors')
      IF(TANSW.EQ.'RC') THEN
        IRC=1
        GO TO 936
      END IF
      CALL DO_STR_LIST(4,TMOD,'selection mode')
      DO K=1,4
        IF(TANSW.EQ.TMOD(K)) THEN
          IMO=K
          GO TO 936
        END IF
      END DO
      CALL DO_STR_LIST(MLSTDA,TLSTDA(1),'ALPHA list')
      DO K=1,MLSTDA
        IF(TANSW.EQ.TLSTDA(K)) THEN
          ISLST=K
          CALL DPARSV(39,'ALI',2,FLOAT(ISLST))
          IF(TANSW.NE.'CH'.AND.TANSW.NE.'EF') GO TO 930
        END IF
      END DO
      CALL DO_STR_LIST(MOPT,TOPT,'option')
      DO K=1,MOPT
        IF(TANSW.EQ.TOPT(K)) THEN
          TOPS=TOPT(K)
          GO TO 930
        END IF
      END DO
      KP=0
      CALL DO_STR('L1"to"L7: add picked objects to list')
      IF(TANSW(1:1).EQ.'L') THEN
        READ(TANSW(2:2),1029,ERR=921) KP
 1029   FORMAT(I1)
        IF(KP.LE.0.OR.KP.GT.7) GO TO 923
        TANSW='LP'
      END IF
  921 CALL DO_STR('LP: append all picked objects to list')
      IF(TANSW.EQ.'LP') THEN
        CALL DAC_PICK_GET(K2,KHIT,KPRO,KLST)
        IF(K2.LE.0) THEN
          CALL DWRT('Pick ALPHA objects first')
          GO TO 930
        ELSE IF(KP.NE.0) THEN
          K1=MAX(1,K2-KP+1)
        ELSE
          K1=1
        END IF
        CALL VZERO(FSELDA,MTOTDA)
        DO K=K1,K2
          DO L=1,NLINDA
            IF(KHIT(K).EQ.IALPDA(L)) THEN
              FSELDA(L)=.TRUE.
              GO TO 925
            END IF
          END DO
          NLINDA=NLINDA+1
          IALPDA(NLINDA)=KHIT(K)
          ILSTDA(NLINDA)=KLST(K)
          FSELDA(NLINDA)=.TRUE.
  925   END DO
        GO TO 940
      END IF
  923 CALL DO_STR('LI: store all')
      IF(TANSW.EQ.'LI') THEN
        NLINDA=0
        DO K=NFSTDA(ISLST),NLSTDA(ISLST)
          NLINDA=NLINDA+1
          IALPDA(NLINDA)=K
          ILSTDA(NLINDA)=ISLST
          FSELDA(NLINDA)=.FALSE.
        END DO
        GO TO 940
      END IF
      CALL DO_STR('EL: erase list')
      IF(TANSW.EQ.'EL') THEN
        NLINDA=0
        GO TO 940
      END IF
      CALL DO_STR('LA: List all')
      IF(TANSW.EQ.'LA') THEN
        DO K=NFSTDA(ISLST),NLSTDA(ISLST)
          DO L=1,NLINDA
            IF(IALPDA(L).EQ.K) GO TO 924
          END DO
          NLINDA=NLINDA+1
          IALPDA(NLINDA)=K
          ILSTDA(NLINDA)=ISLST
          FSELDA(NLINDA)=.FALSE.
  924   END DO
        GO TO 940
      END IF
      CALL DO_STR('EC: erase calculated objects')
      IF(TANSW.EQ.'EC') THEN
        NCALDA=0
        GO TO 940
      END IF
      CALL DO_STR('EO: erase selected objects')
      IF(TANSW.EQ.'EO') THEN
        K=0
        DO N=1,NLINDA
          IF(.NOT.FSELDA(N)) THEN
            K=K+1
            IALPDA(K)=IALPDA(N)
            ILSTDA(K)=ILSTDA(N)
C         ELSE
CC          .................... Objects not on list get default color
C           CALL DV_QVEC_SET_COL(IALPDA(N),0)
          END IF
        END DO
        NLINDA=K
        K=0
        DO N=-1,NCALDA,-1
          IF(.NOT.FSELDA(N)) THEN
            K=K-1
            IALPDA(K)=IALPDA(N)
            ILSTDA(K)=ILSTDA(N)
            TNAMDA(K)=TNAMDA(N)
            NCORDA(K)=NCORDA(N)
            DO I=1,NCORDA(K)
              ICORDA(I,K)=ICORDA(I,N)
              TCORDA(I,K)=TCORDA(I,N)
            END DO
C         ELSE
C           CALL DV_QVEC_SET_COL(IALPDA(N),0)
          END IF
        END DO
        NCALDA=K
        CALL VZERO(FSELDA,MTOTDA)
        GO TO 940
      END IF
      CALL DO_STR('ER: erase last new object')
      IF(TANSW.EQ.'ER') THEN
        NCALDA=NCAL
        GO TO 940
      END IF
      CALL DO_STR('DA: deselect all')
      IF(TANSW.EQ.'DA') THEN
        CALL VZERO(FSELDA,MTOTDA)
        GO TO 940
      END IF
      CALL DO_STR('SA: select all')
      IF(TANSW.EQ.'SA') THEN
        DO L=1,NLINDA
          FSELDA(L)=.TRUE.
        END DO
        GO TO 940
      END IF
      CALL DO_STR('SC: select all calculations')
      IF(TANSW.EQ.'SC') THEN
        DO L=NCALDA,-1
          FSELDA(L)=.TRUE.
        END DO
        GO TO 940
      END IF
      CALL DO_STR('SW:  switch selection')
      IF(TANSW.EQ.'SW') THEN
        DO L=1,NLINDA
          FSELDA(L)=.NOT.FSELDA(L)
        END DO
        GO TO 940
      END IF
      CALL DO_STR('S+"SJ"SL:  select +,J,L')
      IF(TANSW.EQ.'S+'.OR.
     &   TANSW.EQ.'SJ'.OR.
     &   TANSW.EQ.'SL') THEN
        TCOR=TANSW(2:2)
        DO L=NCALDA,-1
          IF(FSELDA(L)) THEN
            CALL VZERO(FSELDA,MTOTDA)
            DO N=1,NCORDA(L)
              IF(TCORDA(N,L).EQ.TCOR) THEN
                ICOR=ICORDA(N,L)
                DO I=NCALDA,NLINDA
                  IF(IALPDA(I).EQ.ICOR) FSELDA(I)=.TRUE.
                END DO
              END IF
            END DO
          END IF
        END DO
        GO TO 940
      END IF
      CALL DO_STR('IC: individual color')
      IF(TANSW.EQ.'IC') THEN
        FYES=.FALSE.
        CALL DPAR_GET_ARRAY(98,'AJ1',6,LCOL)
        LM=0
        DO L=NCALDA,-1
          IF(FSELDA(L)) THEN
            FYES=.TRUE.
            LM=MOD(LM,6)+1
            CALL DV_QVEC_SET_COL(IALPDA(L),LCOL(LM))
          END IF
        END DO
        IF(FYES) GO TO 940
        CALL DWRT('Select calculated objects first.#')
        FMACDM=.FALSE.
        GO TO 930
      END IF
      CALL DO_STR('CC: color corresponding tracks')
      IF(TANSW.EQ.'CC') THEN
        FYES=.FALSE.
        DO L=NCALDA,-1
          IF(FSELDA(L)) THEN
            FYES=.TRUE.
            CALL DV_QVEC_GET_COL(IALPDA(L),NCOL)
            DO N=1,NCORDA(L)
              IF(TCORDA(N,L).EQ.'J'.OR.TCORDA(N,L).EQ.'+')
     &          CALL DV_QVEC_SET_COL(ICORDA(N,L),NCOL)
            END DO
          END IF
        END DO
        IF(FYES) GO TO 940
        CALL DWRT('Select calculated objects first.#')
        FMACDM=.FALSE.
        GO TO 930
      END IF
      CALL DO_STR('TC: EF track color to all CH tracks')
      IF(TANSW.EQ.'TC') THEN
        CALL DV_QVEC_COL_EF_TO_CH(IOCODA)
        GO TO 942
      END IF
      CALL DO_STR('TE: CH track color to all EF tracks')
      IF(TANSW.EQ.'TE') THEN
        CALL DV_QVEC_COL_CH_TO_EF(IOCODA)
        GO TO 942
      END IF
      CALL DO_STR('FA: FRFT to ALPHA track color')
      IF(TANSW.EQ.'FA') THEN
        CALL DV_QVEC_COL_FROM_FRFT(ISLST,NCTRDC(1))
        GO TO 942
      END IF
      CALL DO_STR('AF"CF: ALPHA to FRFT track color')
      IF(TANSW.EQ.'AF'.OR.TANSW.EQ.'CF') THEN
        CALL DPARGV(98,'ANA',2,CNAS)
        TANSW='af'
      END IF
      IF(TANSW.EQ.'JF') THEN
        CNAS=-1.
        TANSW='af'
      END IF

      IF(TANSW.EQ.'af') then
        NUM=BNUMDB(2,FRFTDB)
        CALL DV_QVEC_COL_FRFT(ISLST,IOCODA,CNAS,NUM,COLRDT)
        CALL DSC0
        CALL DSCTR
        GO TO 942
      END IF

      CALL DO_STR('A0: Reset colors of current ALPHA tracks')
      IF(TANSW.EQ.'A0') THEN
        CALL DV_QVEC_DEF_COL_IN
        IF(ISLST.NE.8) THEN
C         ........TAKE OUT WHEN JETS ARE NOT MORE STORED INTO NFSTDA(8)..
          CALL DV_QVEC_DEF_COL(ISLST,-1,0)
          CALL DV_QVEC_SET_COL_AR(NFSTDA(ISLST),NLSTDA(ISLST),0)
        END IF
        IOCODA(ISLST)=0
        CALL DPARSV(98,'ACF',2,-1.)
        CALL DWRT('Colors of current ALPHA tracks set to default.')
        GO TO 942
      END IF
      CALL DO_STR('A9: Reset colors of all ALPHA tracks')
      IF(TANSW.EQ.'A9') THEN
        CALL DV_QVEC_DEF_COL_IN
        DO LLS=1,9
          IF(LLS.NE.8) THEN
C           ........TAKE OUT WHEN JETS ARE NOT MORE STORED INTO NFSTDA(8)..
            CALL DV_QVEC_DEF_COL(LLS,-1,0)
            CALL DV_QVEC_SET_COL_AR(NFSTDA(LLS),NLSTDA(LLS),0)
            IOCODA(LLS)=0
          END IF
        END DO
        CALL DPARSV(98,'ACF',2,-1.)
        CALL DWRT('Colors of all ALPHA tracks set to default.')
        GO TO 942
      END IF
      CALL DO_STR('F0: FRFT track color = default')
      IF(TANSW.EQ.'F0') THEN
        NUM=BNUMDB(2,FRFTDB)
        CALL UFILL(COLRDT,1,NUM,0.)
        CALL DSC0
        CALL DSCTR
        GO TO 942
      END IF
      CALL DO_STR('OO: define color for objects with default color')
      IF(TANSW.EQ.'OO') THEN
C               123456789 123456789 123456789
        TXTADW='Color of '//TLIST//' = **     Define color:'
        CALL DACOL(IOCODA(ISLST),TXTADW(24:25))
        CALL DWRC
        CALL DOPER(1,1,
     &    0,0,' ',0,
     &    0,0,' ',0,
     &    NEXEC,CHG,TANSW)
        GO TO (910,927,930,940), NEXEC
  927   CALL DACOL_NUM(TANSW,M)
        IF(M.GT.-2) THEN
          IOCODA(ISLST)=M
          GO TO 940
        END IF
        GO TO 930
      END IF
      CALL DO_STR('NM: set new mass')
      IF(TANSW.EQ.'NM') THEN
        DO L=1,NLINDA
          IF(FSELDA(L)) CALL DV_QVEC_SET_MASS(IALPDA(L),PR(2,1))
        END DO
        GO TO 940
      END IF
      CALL DO_STR('OM: set original mass')
      IF(TANSW.EQ.'OM') THEN
        DO L=1,NLINDA
          IF(FSELDA(L)) CALL DV_QVEC_SET_MASS(IALPDA(L),-1.)
        END DO
        GO TO 940
      END IF
      CALL DO_STR('LK: lock / unlock')
      IF(TANSW.EQ.'LK') THEN
        IF(TLOCK.EQ.'  ') THEN
          TLOCK=TANSW
        ELSE
          TLOCK=' '
        END IF
        GO TO 930
      END IF
      CALL DO_STR('AV: add 4 vectors 9.2.2')
      IF(TANSW.EQ.'AV') THEN
        NCAL=NCALDA
        NCALDA=NCALDA-1
        NCORDA(NCALDA)=0
        CALL DV_QVEC_ADD_4V(0,IALPDA(NCALDA))
        ILOLD=0
        TNAMDA(NCALDA)=' '
        DO L=NCALDA+1,NLINDA
          IF(FSELDA(L)) THEN
            IF(ILOLD.EQ.0) THEN
              ILOLD=ILSTDA(L)
            ELSE
              IF(ILOLD.NE.ILSTDA(L)) TNAMDA(NCALDA)='??_AV'
            END IF
            CALL DV_QVEC_ADD_4V(IALPDA(L),IALPDA(NCALDA))
            CALL DAC_CORR_STORE(IALPDA(L),'+')
          END IF
        END DO
        IF(ILOLD.EQ.0) GO TO 930
        IF(TNAMDA(NCALDA).EQ.' ')
     &     TNAMDA(NCALDA)=TLSTDA(ILOLD)//'_AD'
        GO TO 940
      END IF
      CALL DO_STR('SV"SM: vertex fitting and add 4 vectors 9.4')
      IF(TANSW.EQ.'SV'.OR.TANSW.EQ.'SM') THEN
        NCAL=NCALDA
        NCALDA=NCALDA-1
        NCORDA(NCALDA)=0
        LSV=0
        DO L=NCALDA+1,NLINDA
          IF(FSELDA(L)) THEN
            LSV=LSV+1
            LIST(LSV)=IALPDA(L)
          END IF
        END DO
        IF(LSV.LE.1) THEN
          CALL DWRT('Select more than 1 "track".#')
          GO TO 930
        ELSE
          CALL DV_QVEC_SEC_VERTEX(TANSW,LSV,LIST,IALPDA(NCALDA),PR(2,1))
          IF(IALPDA(NCALDA).LT.0) THEN
            CALL DWRT('Secondary vertex failed.#')
            NCALDA=NCALDA+1
            GO TO 936
          END IF
          DO K=1,LSV
            CALL DAC_CORR_STORE(LIST(K),'V')
          END DO
          ISV=MIN(99,ISV+1)
          WRITE(TNAMDA(NCALDA),1003) ISV
 1003     FORMAT('SV_',I2)
          IF(TNAMDA(NCALDA)(4:4).EQ.' ') TNAMDA(NCALDA)(4:4)='_'
        END IF
        GO TO 940
      END IF
C      CALL DO_STR('EI: Eigenvector 10.4')
C      IF(TANSW.EQ.'EI') THEN
C        NCAL=NCALDA
C        GO TO 940
C      END IF
      CALL DO_STR('SP: Sphericity 10.6')
      IF(TANSW.EQ.'SP') THEN
        NCAL=NCALDA
        NCALDA=NCALDA-1
        CALL VZERO(FSELDA,-MCALDA)
        FSELDA(NCALDA)=.TRUE.
        IF(TLOCK.EQ.' ') THEN
          NCORDA(NCALDA)=0
          CALL DV_QVEC_SPHERICITY(IALPDA(NCALDA))
        ELSE
          CALL DAC_CORR_STORE_SEL('L')
          CALL DV_QVEC_LOCK
          CALL DV_QVEC_SPHERICITY(IALPDA(NCALDA))
          CALL DV_QVEC_UNLOCK
        END IF
        TNAMDA(NCALDA)=TOPS//'_SP'
        GO TO 940
      END IF
      CALL DO_STR('TH: Thrust 10.7')
      IF(TANSW.EQ.'TH') THEN
        NCAL=NCALDA
        NCALDA=NCALDA-1
        CALL VZERO(FSELDA,-MCALDA)
        FSELDA(NCALDA)=.TRUE.
        IF(TLOCK.EQ.' ') THEN
          NCORDA(NCALDA)=0
          CALL DV_QVEC_THRUST(IALPDA(NCALDA))
        ELSE
          CALL DAC_CORR_STORE_SEL('L')
          CALL DV_QVEC_LOCK
          CALL DV_QVEC_THRUST(IALPDA(NCALDA))
          CALL DV_QVEC_UNLOCK
        END IF
        TNAMDA(NCALDA)=TOPS//'_TH'
        GO TO 940
      END IF
C      CALL DO_STR('HE: 2 Hemispheres 10.9')
C      IF(TANSW.EQ.'HE') THEN
C        NCAL=NCALDA
C        GO TO 940
C      END IF
      CALL DO_STR('MI: Missing mass ... 10.10')
      IF(TANSW.EQ.'MI') THEN
        NCAL=NCALDA
        NCALDA=NCALDA-1
        CALL VZERO(FSELDA,-MCALDA)
        FSELDA(NCALDA)=.TRUE.
        CALL DV_QVEC_SET_OPT(TOPS)
        IF(TLOCK.EQ.' ') THEN
          NCORDA(NCALDA)=0
          CALL DV_QVEC_VMISS(IALPDA(NCALDA))
        ELSE
          CALL DAC_CORR_STORE_SEL('L')
          CALL DV_QVEC_LOCK
          CALL DV_QVEC_VMISS(IALPDA(NCALDA))
          CALL DV_QVEC_UNLOCK
        END IF
        TNAMDA(NCALDA)=TOPS//'_MI'
        FMISDA=.TRUE.
        GO TO 940
      END IF
      CALL DO_STR('JJ: Jade jet 10.11.1')
      IF(TANSW.EQ.'JJ') THEN
        NCAL=NCALDA
        CALL DV_QVEC_SET_OPT(TOPS)
        IF(TLOCK.EQ.' ') THEN
          CALL DV_QVEC_JADE(19,NJETS,LIST)
        ELSE
          CALL DV_QVEC_LOCK
          CALL DV_QVEC_JADE(19,NJETS,LIST)
          CALL DV_QVEC_UNLOCK
        END IF
        TNAM=TOPS//'JJ'
        TANSW='jt'
      END IF
      CALL DO_STR('JD: Durham jet 10.11.1')
      IF(TANSW.EQ.'JD') THEN
        NCAL=NCALDA
        CALL DV_QVEC_SET_OPT(TOPS)
        IF(TLOCK.EQ.' ') THEN
          CALL DV_QVEC_DURHAM(19,NJETS,LIST)
        ELSE
          CALL DV_QVEC_LOCK
          CALL DV_QVEC_DURHAM(19,NJETS,LIST)
          CALL DV_QVEC_UNLOCK
        END IF
        TNAM=TOPS//'JD'
        TANSW='jt'
      END IF
      IF(TANSW.EQ.'jt') THEN
        IF(NJETS.GT.0) THEN
C         .................................... store jets
          CALL VZERO(FSELDA,-MCALDA)
          DO N=1,NJETS
            IF(NCALDA.GT.MCALDA) THEN
              NCALDA=NCALDA-1
              FSELDA(NCALDA)=.TRUE.
              IALPDA(NCALDA)=LIST(N)
              TNAMDA(NCALDA)=TNAM//TNUM(MIN(9,N))
              IF(TLOCK.EQ.' ') THEN
                NCORDA(NCALDA)=0
              ELSE
                CALL DAC_CORR_STORE_SEL('L')
              END IF
              CALL DV_QVEC_JET_CORR(LIST(N),NOBJ,IOBJ)
              DO K=1,NOBJ
                CALL DAC_CORR_STORE(IOBJ(K),'J')
              END DO
            END IF
          END DO
          IF(LJET.GT.0) THEN
            NFSTDA(LJET)=LIST(1)
            NLSTDA(LJET)=LIST(NJETS)
          END IF
        END IF
        GO TO 940
      END IF
      CALL DO_STR('BT: jet Btags')
      IF(TANSW.EQ.'BT') THEN
        CALL DV_QVEC_BTAG(NJETS,LIST)
        GO TO 936
      END IF
      CALL DO_STR('P0: Pi0 s')
      IF(TANSW.EQ.'P0') THEN
        NCAL=NCALDA
        CALL VZERO(FSELDA,-MCALDA)
        CALL DAC_PI0(NP1,NP2)
        IF(NP2.GT.0) THEN
          CALL DPARGI(98,'AMI',JCOL)
          DO N=NP1,NP2
            IF(NCALDA.GT.MCALDA) THEN
              DO L=NCALDA,-1
                IF(IALPDA(L).EQ.N) THEN
                  FSELDA(L)=.TRUE.
                  GO TO 928
                END IF
              END DO
              NCALDA=NCALDA-1
              FSELDA(NCALDA)=.TRUE.
              IALPDA(NCALDA)=N
              CALL DV_QVEC_SET_COL(N,JCOL)
              WRITE(TNAMDA(NCALDA),1002) N-NP1+1
 1002         FORMAT('P0_',I2)
              IF(TNAMDA(NCALDA)(4:4).EQ.' ') TNAMDA(NCALDA)(4:4)='_'
              NCORDA(NCALDA)=0
            END IF
  928     END DO
          CALL DAC_PI0_GA0(NGUM,NP1)
          DO K=1,NGUM
            CALL DAC_PI0_GA(K,N)
            IF(NCALDA.GT.MCALDA) THEN
              DO L=NCALDA,NLINDA
                IF(IALPDA(L).EQ.N) THEN
                  FSELDA(L)=.TRUE.
                  GO TO 929
                END IF
              END DO
              NCALDA=NCALDA-1
              FSELDA(NCALDA)=.TRUE.
              IALPDA(NCALDA)=N
              CALL DV_QVEC_SET_COL(N,JCOL)
              WRITE(TNAMDA(NCALDA),1004) N-NP1+1
 1004         FORMAT('PH_',I2)
              IF(TNAMDA(NCALDA)(4:4).EQ.' ') TNAMDA(NCALDA)(4:4)='_'
              NCORDA(NCALDA)=0
            END IF
  929     END DO
          GO TO 940
        ELSE
          CALL DWRT('There is no Pi0.#')
          GO TO 936
        END IF
      END IF
      CALL DO_STR('V1"to"VW: selected vector = axis')
      CALL DAREA('V',TANSW,0,12,NAR,FYES)
      IF(FYES) THEN
        DO L=NCALDA,NLINDA
          IF(FSELDA(L)) THEN
            CALL DV_QVEC_VECTOR_POL(IALPDA(L),P,FI,TE,IDUM,IDUM,IDUM)
            IFI=FI
            ITE=TE
            WRITE(TXTADW,1000) L,IFI,ITE
 1000       FORMAT('Line',I3,' : FI = ',I4,' TE = ',I4,' stored.')
            CALL DWRC
            PARADA(2,J_AFM)=FI
            PARADA(2,J_ATE)=TE
            DO M=0,MPNWDW
              IF(NWINDW(M,NAR).EQ.-2.OR.NWINDW(M,NAR).EQ.1)
     &          CALL DPACNT(J_AFM,J_ATE,PSTODS(1,1,M,0))
            END DO
            CALL DPCEAR(NAR)
            GO TO 930
          END IF
        END DO
      END IF
      CALL DO_STR('PN: Particle name')
      IF(TANSW.EQ.'PN') THEN
        CALL DWRT('Give integer identifier of particle')
        CALL DGETLN(TXTADW,LPN,3)
        IF(LPN.LT.1) GO TO 930
        READ(TXTADW(1:LPN),3000) NPN
 3000   FORMAT(I3)
        CALL DV_QVEC_PART_NAME(NPN,TXTADW)
        CALL DWRC
        GO TO 936
      END IF
      CALL DO_STR('AC: ALPHA window is closed')
      CALL DO_STR('D1"to"DW: Redisplay')
      CALL DAREA('D',TANSW,0,12,NAR,FYES)
      IF(FYES) THEN
        CALL DQSWIN(0,LWUS)
        CALL DPCEAR(NAR)
        CALL DQSWIN(LWUS,LDUM)
        GO TO 930
      END IF
      CALL DO_STR('CT: copy to terminal')
      IF(TANSW.EQ.'CT') THEN
        NWTA=-1
        GO TO 941
      END IF
      CALL DO_STR('TP"PP: = toggle processor')
      IF(TANSW.EQ.'TP'.OR.TANSW.EQ.'PP') THEN
        CALL DGINMA(TGT)
        GO TO 936
      END IF
      CALL DO_STR('LL: List lists')
      CALL DO_STR('AN: debug output with ALPHA track numbers')
      IF(TANSW.EQ.'LL'.OR.TANSW.EQ.'AN') THEN
        CALL DAD_ALPHA_NUMBERS(TANSW)
        GO TO 936
      END IF
      CALL DO_STR('WF: write user list to file')
      IF(TANSW.EQ.'WF') THEN
        CALL DAC_WRITE_FILE
        GO TO 936
      END IF
C      CALL DO_STR('NA: navigate from EF to FRFT, PECO, PHCO')
C      IF(TANSW.EQ.'NA') THEN
C        CALL DAC_EF_TEH(NWTA)
C        GO TO 936
C      END IF
      CALL DO_STR('M1"M2"to"MW: draw missing vector on YX,RZ,TF,FR')
      CALL DAC_DRAW_VECTOR(TANSW,FYES)
      IF(FYES) GO TO 936
      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('D_ALPHA')
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  942 IF(NWTA.EQ.-1) GO TO 930
C     ............................................................ LIST objects
  940 CALL DAREA('C',TANSW,0,12,IAREDO,FYES)
      IF(FYES) NWTA=0
  941 CALL DACD(NWTA,.TRUE.)
      IF(IRC.NE.0) GO TO 936
      GO TO 930
CH
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------- DAC0
CH
      ENTRY DAC_NUM_OF_PI0(IPI0,IAP0)
CH
CH --------------------------------------------------------------------
      IF(IPI0.LT.0     .AND.
     &   IPI0.GT.MCALDA.AND.
     &   TNAMDA(IPI0)(1:2).EQ.'P0') THEN
        IAP0=IALPDA(IPI0)
      ELSE    
        IAP0=0
      END IF
      RETURN
CH
CH
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------- DAC0
CH
      ENTRY DAC0
CH
CH --------------------------------------------------------------------
CH
      NLINDA=0
      NCALDA=0
      ISV=0
      CALL DV_QVEC_NEW_EV(ENFLDA)
      CALL VZERO(IOCODA,MLSTDA)
      CALL VZERO(FSELDA,MTOTDA)
      FMISDA=.FALSE.
      CALL DACD_0
      CALL DAC_PI0_0
      END
*DK DACD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++  DACD
CH
      SUBROUTINE DACD(NWTA,FAC)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C      INPUT: NWTA=-1 , if output is directed to the terminal.
C             FAC = .TRUE. , if DACD is called from DAC, so that overflow
C                                      is directed to the terminal.
C             From DBR2 FAC must be set to .false. and NWTA to 0.
      INCLUDE 'DALI_CF.INC'
      CHARACTER *1 TINT,T1
      CHARACTER *2 TFLA
      CHARACTER *3 TM
      DATA TM/'___'/
      CHARACTER *12 TALPH
      DATA TALPH/'ALPHA window'/
      CHARACTER *42 TBACK
      DATA TBACK/'The ALPHA window is closed by typing "AC".'/
      CHARACTER *5 TNAM
      DATA L33/33/,L47/47/,L8/8/,MDACD/7000/
      DATA HA/2./,VA/8./,AT/0.11/,HB/300./,VB/14./,IW12/12/,LEV1/1/
      DATA H1/661./,VMAX/661./,DVL/16./,Q1511/0.3/
      DATA IDEB/0/,LLICA/0/,NEWW/1/,LIN9/9/,L20/20/
      LOGICAL FAC,FTERM,F1
      IF(FAC) THEN
        ISTODS(5,IW12,NEWW)=0
      ELSE
        NWTA=IWUSDO
      END IF
      MDLRDP=MDACD
C     ..................................................... SETUP + HEADER
      CALL DPARGI(98,'ACF',ILCH)
      TXTADW='    #'
      CALL DAC_LINE0(TXTADW(7:49))
      IF(     NWTA.EQ.-1) THEN
        CALL DWRC
        FTERM=.TRUE.
        LEND=L47
        F1=.FALSE.
      ELSE
        IF(NWTA.EQ.1) THEN
          IF(FAC) THEN
            NLICA=MAX(-NCALDA+NLINDA+1,LIN9)
            L20=FLOAT(NLICA)*Q1511
            IF(ABS(NLICA-LLICA).GT.L20) THEN
              CALL DQDWIN(NEWW)
              LLICA=NLICA
              CALL DPARGV(100,'AHW',2,H1)
              V1=NLICA*DVL
              IF(V1.GT.POSIDW(9)) THEN
                HFAC=2.
                H1=HFAC*H1
                V1=MIN(POSIDW(9),0.5*V1+DVL)
              END IF
            END IF
            CALL DPARSV(100,'AH1',2,H1)
            CALL DPARSV(100,'AV1',2,V1)
          ELSE
            CALL DPARGV(100,'AH1',2,H1)
            CALL DPARGV(100,'AV1',2,V1)
          END IF
          CALL DQCWIN(NEWW,H1,V1,'DALI window A: ALPHA listing','ALPHA')
          CALL DQ_CHANGE_WINDOW(IW12,'STORE',1.,1.,H1,V1)
          CALL DQCL_US(IAREDO)
        ELSE
          CALL DQCL(IAREDO)
          CALL DQSWIN(NWTA,LDUM)
        END IF
        IF(.NOT.FPIKDP) CALL DQFFWI(LEV1)
        FTERM=.FALSE.
        F1=FAC
C       .................... initialize drawing, calculate line spacing
        CALL DAC_DRAW_IN(NLINDA-NCALDA+1,LEND,.TRUE.)
      END IF
      CALL DAC_CORR_CALC(L33,LEND)
      CALL DV_QVEC_V0_PV(IVRODV)
      DO L=1,NLINDA
        LST=ILSTDA(L)
        CALL DV_QVEC_DEF_COL(LST,ILCH,NCTRDC(1))
        KPIKDP=IALPDA(L)
        CALL DV_QVEC_OBJECT(KPIKDP,P,FI,TE,IC,RM,T1,NCDF,NCOL)
        IF(F1.AND.FTERM.AND.NCOL.GE.0) THEN
          F1=.FALSE.
          CALL DWRT('---------    O v e r f l o w    ---------#')
        END IF
        TINT='_'
        IF(NCOL.EQ.0) THEN
          NCOL=IOCODA(LST)
          IF(NCOL.EQ.0) THEN
            NCOL=NCDF
            TINT=' '
          END IF
        END IF
        I=IALPDA(L)-NFSTDA(LST)+1
        TNAM=TLSTDA(LST)//TM
        IF(LST.EQ.5) CALL DV_QVEC_EF_TYPE(IALPDA(L),TNAM)
        CALL DAC_LINE(TNAM,I,IC,P,FI,TE,RM,TXTADW(7:80))
        TXTADW(26:26)=T1
        IF(FSELDA(L)) THEN
          TFLA='=>'
        ELSE
          TFLA='  '
        END IF
        WRITE(TXTADW(1:6),3001) TFLA,L,TINT
 3001   FORMAT(A,I3,A)
        CALL DAC_CORR_LINE(L)
C       IF(FTERM.OR.NCOL.LT.0) THEN
        IF(FTERM) THEN
          IF(FAC) THEN
            IF(TLSTDA(LST).EQ.'V0')
     &        CALL DV_QVEC_V0(KPIKDP,NCTRDC(1),46,TXTADW)
            CALL DACOL(NCOL,TXTADW(48:80))
            CALL DWRC
          END IF
        ELSE
          IF(TLSTDA(LST).EQ.'V0')
     &      CALL DV_QVEC_V0(KPIKDP,NCTRDC(1),LEND,TXTADW)
          CALL DAC_DRAW(L,NCOL,FTERM)
        END IF
      END DO
      TINT='_'
      DO L=-1,NCALDA,-1
        KPIKDP=IALPDA(L)
        CALL DV_QVEC_OBJECT(KPIKDP,P,FI,TE,IC,RM,T1,NCDF,NCOL)
        IF(F1.AND.FTERM.AND.NCOL.GE.0) THEN
          F1=.FALSE.
          CALL DWRT('---------    O v e r f l o w    ---------#')
        END IF
        CALL DAC_LINE(TNAMDA(L)      ,0,IC,P,FI,TE,RM,TXTADW(7:80))
        TXTADW(26:26)=T1
        IF(FSELDA(L)) THEN
          TFLA='=>'
        ELSE
          TFLA='  '
        END IF
        WRITE(TXTADW(1:6),3001) TFLA,L,TINT
        CALL DAC_CORR_LINE(L)
        IF(FTERM.OR.NCOL.LT.0) THEN
          IF(FAC) THEN
            CALL DACOL(NCOL,TXTADW(48:80))
            CALL DWRC
          END IF
        ELSE
          CALL DAC_DRAW(L,NCOL,FTERM)
        END IF
      END DO
      IF(NWTA.GE.0) THEN
        IF(NWTA.EQ.0) THEN
          CALL DQFR(IAREDO)
          IWARDO=-1
          CALL DPCSAR
        ELSE
          IWARDO=IW12
          CALL DPCSAV(IWUSDO,IW12)
          CALL DGEXEC
          CALL DQ_CHANGE_WINDOW_BACK
        END IF
      END IF
      RETURN
CH
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------- DACD_0
CH
      ENTRY DACD_0
CH
CH --------------------------------------------------------------------
CH
      LLICA=-99
      CALL DQDWIN(NEWW)
      END
*DK DAC_WRITE_FILE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++ DAC_WRITE_FILE
CH
      SUBROUTINE DAC_WRITE_FILE
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Same as DACD but no overflow on terminal
C     User list and interaction list may be seperately wtched on or off.
      INCLUDE 'DALI_CF.INC'
      CHARACTER *1 T1
      CHARACTER *3 TM
      DATA TM/'___'/
      CHARACTER *5 TNAM
      DATA L2/2/,L31/31/
      CALL DGOPEN(NUNIDU,'DAL.DAT',1,*99,IER)
      WRITE(NUNIDU,1001) IRUNDE(1),IEVTDE(1),TFINDE(1)(1:45)
 1001 FORMAT(1X,I7,'=run ',I7,'=event  from ',A)
C     ..................................................... SETUP + HEADER
      TXTADW=' '
      CALL DAC_LINE0(TXTADW(2:80))
      WRITE(NUNIDU,1000) TXTADW(1:79)
      NLIN=1
C       .................... initialize drawing, calculate line spacing
      CALL DAC_CORR_CALC(L31,79)
      CALL DPARGI(98,'ACF',ILCH)
      IF(TLSTDA(LST).EQ.'V0') CALL DV_QVEC_V0_PV(IVRODV)
      DO L=1,NLINDA
        LST=ILSTDA(L)
        CALL DV_QVEC_DEF_COL(LST,ILCH,NCTRDC(1))
        CALL DV_QVEC_OBJECT(IALPDA(L),P,FI,TE,IC,RM,T1,NCDF,NCOL)
        IF(NCOL.EQ.0) THEN
          NCOL=IOCODA(LST)
          IF(NCOL.EQ.0) NCOL=NCDF
          I=IALPDA(L)-NFSTDA(LST)+1
          TXTADW=' '
          TNAM=TLSTDA(LST)//TM
          IF(LST.EQ.5) CALL DV_QVEC_EF_TYPE(IALPDA(L),TNAM)
          CALL DAC_LINE(TNAM,I,IC,P,FI,TE,RM,TXTADW(L2:80))
          CALL DACOL(NCOL,TXTADW(29:30))
          CALL DAC_CORR_LINE(L)
          IF(TLSTDA(LST).EQ.'V0')
     &      CALL DV_QVEC_V0(IALPDA(L),NCTRDC(1),79,TXTADW)
          WRITE(NUNIDU,1000) TXTADW(1:79)
 1000     FORMAT(1X,A)
        END IF
      END DO
      DO L=-1,NCALDA,-1
        CALL DV_QVEC_OBJECT(IALPDA(L),P,FI,TE,IC,RM,T1,NCDF,NCOL)
        IF(NCOL.GE.0) THEN
          TXTADW=' '
          CALL DAC_LINE(TNAMDA(L)      ,0,IC,P,FI,TE,RM,TXTADW(2:80))
          CALL DAC_CORR_LINE(L)
          CALL DACOL(NCOL,TXTADW(29:30))
          WRITE(NUNIDU,1000) TXTADW(1:79)
        END IF
      END DO
      CLOSE(UNIT=NUNIDU)
      CALL DWRT('Userlist stored on DAL.DAT.')
   99 END
*DK DAC_EF_TEH
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++  DAC_EF_TEH
CH
      SUBROUTINE DAC_EF_TEH(NWTA)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C      INPUT: NWTA=-1, if output is directed to the terminal.
C             FAC = .TRUE. , if DACD is called from DAC, so that overflow
C                                      is directed to the terminal.
C             From DBR2 FAC must be set to .false. and NWTA to 0.
      INCLUDE 'DALI_CF.INC'
      DIMENSION ITEHC(4)
      DATA DV/12./,DHT/1./,DBNK/46./DBAR/72./,DCOL/82./,DEND/82./
      CHARACTER *10 TXTL
      CHARACTER *3 DT3
      CHARACTER *2 TOPS
      CHARACTER *1 TM,T1
      CHARACTER *1 TSEL,TARW,TBAR,TLIN,TL(0:15)
      DATA TARW/'>'/,TLIN/'_'/,TBAR/'|'/,TL/
     &  ' ','T','E','a','H','b','c','d','C','e','f','g','h','i','j','k'/
      DATA L8/8/
      PARAMETER (MTEH=200,MTEH3=600)
      DIMENSION NTEH(3,MTEH)
      LOGICAL FTERM,FSTRT
      MDLRDP=7001
      CALL DSCTR
      CALL DPARGI(98,'ACF',ILCH)
      IF(NWTA.EQ.-1) THEN
        FTERM=.TRUE.
        NCOL=0
      ELSE
        CALL DSC_EO_0
        CALL DV_QVEC_DEF_COL_IN
        CALL DV_QVEC_DEF_COL(5,ILCH,NCTRDC(1))
        FTERM=.FALSE.
        FSTRT=.TRUE.
        CALL DQWIL(0.)
        CALL DDLCL(ICTPDD)
        V1=VMINDG(IAREDO)
        V2=VHGHDG(IAREDO)
        H1=HMINDG(IAREDO)
        H2=HHGHDG(IAREDO)
        HC=H1+DHT
        V=V2
      END IF
      LL=-9
      TXTADW=' '
      IEF=0
      DO NAL=NFSTDA(5),NLSTDA(5)
        DO L=1,NLINDA
          IF(IALPDA(L).EQ.NAL.AND.FSELDA(L)) THEN
            TSEL=TARW
            GO TO 10
          END IF
        END DO
        TSEL=TLIN
   10   CALL DV_QVEC_EF_TEH(NAL,ITEHC)
        NL=0
        IROW=0
        DO N=4,1,-1
          IF(ITEHC(N).NE.0) THEN
            IROW=ITEHC(N)
            KBNK=N
            NL=NL+2**(N-1)
          END IF
        END DO
        IEF=IEF+1
C       ....................................................... 123456789
C       ....................................................... EF_12 T12|
        WRITE(TXTL,1000) TSEL,IEF,TL(NL),IROW,TBAR
 1000   FORMAT('EF',A,I2,1X,A,I2,A)
        CALL DV_QVEC_EF_TYPE(NAL,TXTL(1:2))
        IF(TXTL(4:4).EQ.' ') TXTL(4:4)=TLIN
        IF(.NOT.FTERM) THEN
          CALL DV_QVEC_VECTOR_COL(NAL,NCOL,NCDF)
          IF(NCOL.EQ.0) THEN
            NCOL=IOCODA(5)
            IF(NCOL.EQ.0) NCOL=NCDF
          END IF
          IF(NCOL.LT.0) GO TO 20
          V=V-DV
          IF(V.LE.V1) THEN
            HC=HC+DCOL
            IF(HC+DEND.GT.H2) THEN
              FTERM=.TRUE.
              GO TO 20
            END IF
            V=V2-DV
          END IF
          LT=1
          CALL DGLEVL(NCOL)
          CALL DGTEXT(HC,V,TXTL( 1: 5),5)
          IF(NL.GT.0) THEN
            IF(     KBNK.EQ.1) THEN
              CALL DGLEVL(NCTRDC(IROW))
            ELSE IF(KBNK.EQ.2) THEN
              CALL DSC_EO(IROW,KCOL)
              CALL DGLEVL(KCOL)
            ELSE
              CALL DGLEVL(L8)
            END IF
            CALL DGTEXT(HC+DBNK,V,TXTL(7:9),3)
          END IF
          CALL DGLEVL(L8)
          CALL DGTEXT(HC+DBAR,V,TBAR,1)
        END IF
   20   IF(FTERM.OR.NCOL.LT.0) THEN
          LL=LL+10
          TXTADW(LL:LL+9)=TXTL
          IF(LL.GE.41) THEN
            CALL DWRC
            LL=-9
            TXTADW=' '
          END IF
        END IF
      END DO
      IF(LL.GT.0) CALL DWRC
      IF(NWTA.GE.0) THEN
        CALL DQFR(IAREDO)
      END IF
      END
*DK DAD_US
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++  DAD_US
CH
      SUBROUTINE DAD_US
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Same as DACD but no overflow on terminal
C     User list and interaction list may be seperately wtched on or off.
      INCLUDE 'DALI_CF.INC'
      CHARACTER *1 T1
      CHARACTER *3 TM
      DATA TM/'___'/
      CHARACTER *5 TNAM
      DATA L2/2/,L28/28/
      LOGICAL FTERM
C     ..................................................... SETUP + HEADER
      TXTADW=' '
      CALL DAC_LINE0(TXTADW(2:80))
      CALL DPARGV(39,'ALI',4,ALST)
      CALL DPARGV(39,'AAC',4,CALC)
      NLIN=1
      FTERM=.FALSE.
      IF(CALC.GT.0.) NLIN=NLIN-NCALDA
      IF(ALST.GT.0.) NLIN=NLIN+NLINDA
C       .................... initialize drawing, calculate line spacing
      CALL DAC_DRAW_IN(NLIN,LEND,.FALSE.)
      CALL DAC_CORR_CALC(L28,LEND)
      IF(ALST.GT.0.) THEN
        CALL DPARGI(98,'ACF',ILCH)
        IF(TLSTDA(LST).EQ.'V0') CALL DV_QVEC_V0_PV(IVRODV)
        DO L=1,NLINDA
          LST=ILSTDA(L)
          CALL DV_QVEC_DEF_COL(LST,ILCH,NCTRDC(1))
          KPIKDP=IALPDA(L)
          CALL DV_QVEC_OBJECT(KPIKDP,P,FI,TE,IC,RM,T1,NCDF,NCOL)
          IF(NCOL.GE.0) THEN
            IF(NCOL.EQ.0) THEN
              NCOL=IOCODA(LST)
              IF(NCOL.EQ.0) NCOL=NCDF
              I=IALPDA(L)-NFSTDA(LST)+1
              TXTADW=' '
              TNAM=TLSTDA(LST)//TM
              IF(LST.EQ.5) CALL DV_QVEC_EF_TYPE(IALPDA(L),TNAM)
              CALL DAC_LINE(TNAM,I,IC,P,FI,TE,RM,TXTADW(L2:80))
              CALL DAC_CORR_LINE(L)
              IF(TLSTDA(LST).EQ.'V0')
     &          CALL DV_QVEC_V0(KPIKDP,NCTRDC(1),LEND,TXTADW)
              CALL DAC_DRAW(L,NCOL,FTERM)
              IF(FTERM) GO TO 99
            END IF
          END IF
        END DO
      END IF
      IF(CALC.GT.0.) THEN
        DO L=-1,NCALDA,-1
          KPIKDP=IALPDA(L)
          CALL DV_QVEC_OBJECT(KPIKDP,P,FI,TE,IC,RM,T1,NCDF,NCOL)
          IF(NCOL.GE.0) THEN
            TXTADW=' '
            CALL DAC_LINE(TNAMDA(L)      ,0,IC,P,FI,TE,RM,TXTADW(2:80))
            CALL DAC_CORR_LINE(L)
            CALL DAC_DRAW(L,NCOL,FTERM)
            IF(FTERM) GO TO 99
          END IF
        END DO
      END IF
      RETURN
   99 IF(FPIKDP) CALL DWRT('List not completw.#')
      END
*DK DAD_ALPHA_NUMBERS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++ DAD_ALPHA_NUMBERS
CH
      SUBROUTINE DAD_ALPHA_NUMBERS(TMOD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Same as DAD_US but only interaction list with Alpha numbers on terminal
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TMOD
      CHARACTER *1 T1
C     ..................................................... SETUP + HEADER
      WRITE(TXTADW,1000) (TLSTDA(K),K=1,MLSTDA)
 1000 FORMAT(12(2X,A))
      CALL DWRC
      DO L=1,MLSTDA
        NWRKDW(L)=NLSTDA(L)-NFSTDA(L)+1
      END DO
      WRITE(TXTADW,1001) (NWRKDW(L),L=1,MLSTDA)
 1001 FORMAT(12I4)
      CALL DWRC
      IF(TMOD.EQ.'LL') RETURN
      WRITE(TXTADW,1001) NFSTDA
      CALL DWRC
      WRITE(TXTADW,1001) NLSTDA
      CALL DWRC
      TXTADW=' '
      CALL DAC_LINE0(TXTADW(2:80))
      TXTADW(34:41)='ALPHA T.'
      DO L=-1,NCALDA,-1
        K=IALPDA(L)
        CALL DV_QVEC_OBJECT(K,P,FI,TE,IC,RM,T1,NCDF,NCOL)
        TXTADW=' '
        CALL DAC_LINE(TNAMDA(L)      ,0,IC,P,FI,TE,RM,TXTADW(2:80))
        WRITE(TXTADW(35:40),'(I6)') K
        CALL DACOL(NCOL,TXTADW(48:49))
        CALL DWRC
      END DO
      END
*DK DAC_CORR_STORE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++ DAC_CORR_STORE
CH
      SUBROUTINE DAC_CORR_STORE(NA,T)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     ... INPUT: NAL=ALPHA #, T= correlation letter
C     ...     If T=' ' correlation for line NCALDA is reset
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) T
      CHARACTER *1 TC(48,MCALDA:MLINDA),TR
      DATA TR/'R'/
      CHARACTER *47 TVL
C     DATA TVL/'. . . . . . . . . . . . . . . . . . . . . . . .'/
      DATA TVL/'. . : . . : . . : . . : . . : . . : . . : . . :'/
      DIMENSION IPOS(48)
      LOGICAL FNEXT
      IF(T.EQ.' ') THEN
        NCORDA(NCALDA)=0
      ELSE
        N=NCORDA(NCALDA)+1
        NCORDA(NCALDA)=N
        ICORDA(N,NCALDA)=NA
        TCORDA(N,NCALDA)=T
      END IF
      RETURN
CH
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------- D_ALPH_0
CH
      ENTRY DAC_CORR_STORE_SEL(T)
CH
CH --------------------------------------------------------------------
CH
C     Reset correlation line and store with '-' all selected objects
      N=0
      DO L=1,NLINDA
        IF(FSELDA(L)) THEN
          N=N+1
          ICORDA(N,NCALDA)=IALPDA(L)
          TCORDA(N,NCALDA)=T
        END IF
      END DO
      NCORDA(NCALDA)=N
      RETURN
CH
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------- D_ALPH_0
CH
      ENTRY DAC_CORR_CALC(IP1,IP2)
CH
CH --------------------------------------------------------------------
CH
C     ... Output: correlation text can be stored from positio IP1 to IP2
      KC=0
      ICMAX=IP2-IP1+1
      DO K=1,-NCALDA
        DO L=NCALDA,NLINDA
          TC(K,L)=' '
        END DO
      END DO
      JP1=IP1
      JP2=MIN(80,IP2)
      IC=0
      DO K=NCALDA,-1
        IF(NCORDA(K).GT.0) THEN
          FNEXT=.TRUE.
          DO N=1,NCORDA(K)
            IALP=ICORDA(N,K)
            DO L=NLINDA,NCALDA,-1
              IF(IALP.EQ.IALPDA(L)) THEN
                IF(FNEXT) THEN
                  IC=IC+1
                  IF(IC.GT.ICMAX) GO TO 10
                  TC(IC,K)=TR
                  FNEXT=.FALSE.
                END IF
                TC(IC,L)=TCORDA(N,K)
              END IF
            END DO
          END DO
        END IF
      END DO
   10 IPOS(1)=JP1
      IPD=JP2-JP1
      ICR=IC-1
      ID=1
      DO I=2,IC
        IR=(IPD-I)/2
        IF(ICR.LE.IR) ID=2
        IPOS(I)=IPOS(I-1)+ID
        ICR=ICR-1
      END DO
      RETURN
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------- DAC_CORR_LINE
CH
      ENTRY DAC_CORR_LINE(LINT)
CH
CH --------------------------------------------------------------------
CH
      IF(IC.EQ.0) RETURN
      TXTADW(JP1:JP2)=TVL(1:JP2-JP1+1)
      DO I=1,IC
        L=IPOS(I)
        IF(TC(I,LINT).NE.' ') TXTADW(L:L)=TC(I,LINT)
      END DO
      END
*DK DAC_LINE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DAC_LINE
CH
      SUBROUTINE DAC_LINE(TNAM,NOB,ICH,P,FI,TE,RM,T)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INPUT  TNAM = name of object
C            NOB=#OF OBJECT if NOB=0 not used
C            ICH = CHARGE '=,-, ,+,#' IF 999 =A XIS P=VALUE
C            P=MOMENTUM if P=0 axis
C            FI,TE = phi,theta
C            RM = restmass
C
C     OUTPUT T example:
C              123456789 123456789 123456789
C            T=EF__1 +14.7 295 114 .1396
C              ident c  P  phi theta M..
C
      CHARACTER *(*) T
      CHARACTER *5 TNAM,DT5
CBSN Here there are problems on IRIX. That compiler is buggy ...
      CHARACTER *4 T4
      CHARACTER *5 T5
CBSN
      CHARACTER *4 DT4
      CHARACTER *2 TC(-2:2)
      DATA TC/' =',' -','  ',' +',' #'/
      IFI=FI
      ITE=TE
      IF(ICH.EQ.999) THEN
        WRITE(T,1001) TNAM,'  ',   DT4(P),IFI,ITE
      ELSE
        ICH=MIN(MAX(-2,ICH),2)
C       ................ The next line crashed on 14-3-96 on SGI. DT4 and DT5
C       ................ needed to be taken out.
CBSN    WRITE(T,1001) TNAM,TC(ICH),DT4(P),IFI,ITE,DT5(RM)
        T4=DT4(P)
        T5=DT5(RM)
        WRITE(T,1001) TNAM,TC(ICH),T4,IFI,ITE,T5
 1001   FORMAT(3A,2I4,1X,A)
      END IF
      IF(     NOB.GE.100) THEN
        WRITE(T(3:5),'(I3)') NOB
      ELSE IF(NOB.GE. 10) THEN
        WRITE(T(4:5),'(I2)') NOB
      ELSE IF(NOB.GE.  1) THEN
        WRITE(T(5:5),'(I1)') NOB
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------- D_AL_LINE0
CH
      ENTRY DAC_LINE0(T)
CH
CH --------------------------------------------------------------------
CH
C     T='EF__1 +14.7 295 114 .1396'
      T='ident c  P  phi th. M'
      END
*DK DAC_DRAW
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++ D_AL_LINE_D
CH
      SUBROUTINE DAC_DRAW(KLIN,NCOL,FTERM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Draw TXTADW on selected window
C     output: FTERM = true if last line on window is drawn
      INCLUDE 'DALI_CF.INC'
      CHARACTER *1 TX
      DATA TX/'X'/
      CHARACTER *2 TTR
      CHARACTER *42 T1
      DATA LA/4/,LB/9/,L79/79/,LRD/12/
      DATA HSEL1/20./,HSEL2/42./,HSEL3/16./,HTXT1/42./,HTXT2/40./
      DATA DHFR/2./,DHMI2/4./,LS8/8/
      DATA DVMIN/11./,DVMAX/15./,DHMIN/270./,LAR/8/,L8/8/,L1/1/,L2/2/
      DATA DHPIK/126./,DVPIK/4./
      DIMENSION ITEHC(4)
      DIMENSION ICUS(200)
      DATA QVH/7./,DVA1/2./,DVA2/3./
      LOGICAL FHEAD,FTERM,FSET,FLIN,F2,F2P,FALI,FAL
      IF(FPIKDP) THEN
        IF(FHEAD) THEN
          VH=VH-DV
          FHEAD=.FALSE.
        END IF
        IF(FPIMDP.AND.KPIKDP.NE.NPIKDP) GO TO 10
        IF((HMIN.LT.HPIKDP.AND.HPIKDP.LT.HMAX).OR.FPIMDP) THEN
          D=ABS(VPIKDP-VH)
          IF(D.LE.DPIKDP) THEN
            DPIKDP=D
            NPIKDP=KPIKDP
            MDLPDP=MDLRDP
            HHPKDP=HMIN+DHPIK
            VVPKDP=VH  +DVPIK
          END IF
        END IF
      ELSE
C       .................................................. header
        IF(FHEAD) THEN
          CALL DGLEVL(L8)
          CALL DGTEXT(HMIN,VH,T1,42)
          VH=VH-DV
          FHEAD=.FALSE.
        END IF
        IF(FAL) THEN
C         ................................................... FRFT track
          IF(     TXTADW(7:7).EQ.'E') THEN
            CALL DV_QVEC_EF_ITYPE(IALPDA(KLIN),ITYP)
            IF(ITYP.LE.3) THEN
              CALL DV_QVEC_EF_TEH(IALPDA(KLIN),ITEHC)
              ITRK=ITEHC(1)
              IF(ITRK.GT.0) THEN
                IF(NCTRDC(ITRK).LT.0) THEN
                  CALL DGLEVL(LIV)
                ELSE
                  CALL DGLEVL(NCTRDC(ITRK))
                END IF
                IF(ITRK.LE.99) THEN
                  WRITE(TTR,1000) ITRK
 1000             FORMAT(I2)
                ELSE
                  TTR='*'
                END IF
                CALL DGTEXT(HMIN,VH,TTR,2)
              END IF
            END IF
          ELSE IF(TXTADW(7:7).EQ.'C') THEN
            ITRK=IALPDA(KLIN)-NFSTDA(1)+1
            IF(ITRK.GT.0) THEN
              IF(NCTRDC(ITRK).LT.0) THEN
                CALL DGLEVL(LIV)
              ELSE
                CALL DGLEVL(NCTRDC(ITRK))
              END IF
              IF(ITRK.LE.99) THEN
                WRITE(TTR,1000) ITRK
              ELSE
                TTR='*'
              END IF
              CALL DGTEXT(HMIN,VH,TTR,2)
            END IF
          END IF
C          ................................................. selection
          IF(TXTADW(1:2).EQ.'=>') THEN
            CALL DGLEVL(L8)
            CALL DQFAR(HMIN+HSEL1,VH-DVA1,HMIN+HSEL2,VH+DV-DVA2)
            KSCOL=L1
          ELSE
            KSCOL=L8
          END IF
          IF(NCOL.GE.0) THEN
            CALL DGLEVL(KSCOL)
          ELSE
            IF(FMONDT) THEN
              TXTADW(6:6)=TX
              CALL DGLEVL(KSCOL)
            ELSE
              CALL DGLEVL(LRD)
            END IF
          END IF
          CALL DGTEXT(HMIN+HSEL3,VH,TXTADW(3:5),3)
C         ................................................... object
          CALL DGLEVL(NCOL)
          IF(NCOL.LT.LAR)THEN
            IF(    NCOL.GT.L2) THEN
              CALL DQFAR(HMIN+HTXT1,VH-DVA1,HMAX,VH+DV-DVA2)
              CALL DGLEVL(L8)
            ELSE IF(NCOL.GT.0) THEN
              CALL DGLEVL(L8)
              CALL DQFAR(HMIN+HTXT1,VH-DVA1,HMAX,VH+DV-DVA2)
              CALL DGLEVL(L1)
            ELSE IF(NCOL.LT.0) THEN
              CALL DGLEVL(LIV)
            END IF
          END IF
          CALL DGTEXT(HMIN+HTXT2,VH,TXTADW(6:LTX),LTX-5)
        ELSE
          CALL DGLEVL(NCOL)
          IF(NCOL.LT.LAR) THEN
            IF(    NCOL.GT.L2) THEN
              CALL DQFAR(HMIN,VH-DVA1,HMAX,VH+DV-DVA2)
C             CALL DQFAR(HMIN+HTXT1,VH-DVA1,HMAX,VH+DV-DVA2)
              CALL DGLEVL(L8)
            ELSE IF(NCOL.GT.0) THEN
              CALL DGLEVL(L8)
              CALL DQFAR(HMIN,VH-DVA1,HMAX,VH+DV-DVA2)
C             CALL DQFAR(HMIN+HTXT1,VH-DVA1,HMAX,VH+DV-DVA2)
              CALL DGLEVL(L1)
            END IF
          END IF
          CALL DGTEXT(HMIN,VH,TXTADW,LTX)
        END IF
        ICUP=ICUP+1
        ICUS(ICUP)=KLIN
      END IF
   10 VH=VH-DV
      IF(VH.LT.VM) THEN
        IF(F2) THEN
          F2=.FALSE.
          VH=V2-DVMAX
          HMIN=MIN(0.5*(H1+H2),HMIN+LTX*LS8+DHMI2)
          HMINP=HMIN
          HMAX=H2
          FHEAD=.TRUE.
        ELSE
          FTERM=.TRUE.
        END IF
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------- D_AL_LINE_D0
CH
      ENTRY DAC_DRAW_IN(NLIN,LEND,FALI)
CH
CH --------------------------------------------------------------------
CH
      FAL=FALI
      CALL DSCTR
      CALL DQWIL(0.)
C     CALL DDLCL(ICTPDD)
      V1=VMINDG(IAREDO)
      V2=VHGHDG(IAREDO)
      H1=HMINDG(IAREDO)+DHFR
      H2=HHGHDG(IAREDO)
      VH=V2
      VM=V1+0.5*DVMIN
      HMIN=H1
      DH=H2-H1
      IF(DH.LT.DHMIN) CALL DWRT('Select wider window.#')
      VH=VH-DVMAX
C     ..................................................... setup line spacing
      IDV=(VH-VM)/NLIN
      DV=IDV
      F2=.FALSE.
      LEND=LA+IFIX(H2-H1)/LB
      LEND=MIN(L79,LEND)
      HMAX=H2
      IF(DV.LT.DVMIN) THEN
        IF(DH.GE.2.*DHMIN) THEN
          DV=2.*DV
          F2=.TRUE.
          HMAX=0.5*(H2-H1)
          LEND=LA+IFIX(0.5*(H2-H1))/LB
        END IF
      END IF
      LTX=LEND
      F2P=F2
      DV=MAX(DVMIN,DV)
      DV=MIN(DVMAX,DV)
      IF(FAL) THEN
        T1='tr'//TXTADW(3:42)
      ELSE
        T1=      TXTADW(1:42)
      END IF
      CALL DGLEVL(L8)
      CALL DGTEXT(HMIN,VH,T1,42)
      VH=VH-DV
      FHEAD=.FALSE.
      ICUP=0
      CALL DPARGI(98,'AIV',LIV)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------------- D_AL_LINE_SELECT
CH
      ENTRY DAC_POINTER_LINES(FSET)
CH
CH --------------------------------------------------------------------
CH
      FLIN=.FALSE.
      GO TO 20
CH..............---
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------- D_AL_LINE_NUM
CH
      ENTRY DAC_POINTER_1_LINE(I1LIN)
CH
CH --------------------------------------------------------------------
CH
      FLIN=.TRUE.
   20 CALL DGCURG(HC,VC)
      ILIN=ICUS(1)
      VH=V2-DVMAX-DV-DV/QVH
      HMIN=H1
      HMAX=HMIN+DHMIN
      DO I=1,ICUP
        IF(VC.GT.VH   .AND.
     &     VC.LT.VH+DV.AND.
     &     HC.GT.HMIN .AND.
     &     HC.LT.HMAX) THEN
          ILIN=ICUS(I)
          GO TO 21
        END IF
        VH=VH-DV
        IF(VH.LT.VM) THEN
          IF(F2P) THEN
            HMIN=HMINP
            HMAX=H2
            VH=V2-DVMAX-DV
          ELSE
            GO TO 21
          END IF
        END IF
      END DO
      ILIN=ICUS(ICUP)
   21 IF(FLIN) THEN
        I1LIN=ILIN
        IPOS=ILIN
      ELSE
        DO I=ILIN,IPOS,ISIGN(1,IPOS-ILIN)
          FSELDA(I)=FSET
        END DO
      END IF
      END
*DK DAC_PICK
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++  DAC_PIK
CH
      SUBROUTINE DAC_PICK(NHIT,MODUL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INCLUDE 'DALI_CF.INC'
      DIMENSION IHIT(*),IPRO(*),ILST(*)
      CHARACTER *1 T1
      CHARACTER *3 TM
      DATA TM/'___'/
      CHARACTER *5 TNAM(7),TNL
      DIMENSION KHIT(7),KPRO(7),KLST(7)
      DO L=NCALDA,NLINDA
        IF(NHIT.EQ.IALPDA(L)) GO TO 2
      END DO
      L=0
    2 IF(MODUL.EQ.7000.AND.KPICK.EQ.0) THEN
        TXTADW='    #'
        CALL DAC_LINE0(TXTADW(7:49))
        CALL DWRC
      END IF
      TXTADW=' '
      IF(L.NE.0) WRITE(TXTADW(1:5),1000) L
 1000 FORMAT(I5)
      IF(L.GE.0) THEN
        DO NL=1,MLSTDA
          IF(NFSTDA(NL).LE.NHIT.AND.NHIT.LE.NLSTDA(NL)) THEN
            CALL DV_QVEC_OBJECT(NHIT,P,FI,TE,IC,RM,T1,NCDF,NCOL)
            I=NHIT-NFSTDA(NL)+1
            TNL=TLSTDA(NL)//TM
            IF(NL.EQ.5) CALL DV_QVEC_EF_TYPE(NHIT,TNL)
            CALL DAC_LINE(TNL,I,IC,P,FI,TE,RM,TXTADW(7:80))
            IF(NCOL.EQ.0) THEN
              NCOL=IOCODA(NL)
              IF(NCOL.EQ.0) NCOL=NCDF
            END IF
            GO TO 10
          END IF
        END DO
      ELSE
        CALL DV_QVEC_OBJECT(NHIT,P,FI,TE,IC,RM,T1,NCDF,NCOL)
        CALL DAC_LINE(TNAMDA(L),0,IC,P,FI,TE,RM,TXTADW(7:80))
        GO TO 10
      END IF
      CALL DWRT('Object not found.#')
      RETURN
   10 CALL DACOL(NCOL,TXTADW(48:49))
      IF(L.GE.0) THEN
        DO K=1,KPICK
          IF(KHIT(K).EQ.NHIT) THEN
            J=K-1
            DO N=K+1,KPICK
              J=J+1
              KHIT(J)=KHIT(N)
              KPRO(J)=KPRO(N)
              KLST(J)=KLST(N)
              TNAM(J)=TNAM(N)
            END DO
            KPICK=J
            GO TO 20
          END IF
        END DO
   20   IF(KPICK.GE.7) THEN
          DO K=2,KPICK
            KHIT(K-1)=KHIT(K)
            KPRO(K-1)=KPRO(K)
            KLST(K-1)=KLST(K)
            TNAM(K-1)=TNAM(K)
          END DO
          KPICK=6
        END IF
        KPICK=KPICK+1
        KHIT(KPICK)=NHIT
        KPRO(KPICK)=MODUL
        KLST(KPICK)=NL
        TNAM(KPICK)=TXTADW(7:11)
        IF(L.GT.0) CALL CUTOL(TNAM(K)(1:2))
C       IF(MODUL.EQ.7001) THEN
C        IF(MODUL.EQ.7000) THEN
CC         ............................. Change text list to short list
C          WRITE(TXTADW,1001) (TNAM(K),K=1,KPICK)
C 1001     FORMAT('Picked:',7(1X,A))
C        END IF
      END IF
      CALL DWRC
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------- DAC_PICK_0
CH
      ENTRY DAC_PICK_0
CH
CH --------------------------------------------------------------------
CH
      KPICK=0
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------- DAC_PICK_GET
CH
      ENTRY DAC_PICK_GET(IPICK,IHIT,IPRO,ILST)
CH
CH --------------------------------------------------------------------
CH
      IPICK=KPICK
      DO K=1,KPICK
        IHIT(K)=KHIT(K)
        IPRO(K)=KPRO(K)
        ILST(K)=KLST(K)
      END DO
      END
*DK DACOL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++  DACOL
CH
      SUBROUTINE DACOL(NCOL,TCOL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      CHARACTER *(*) TCOL
      CHARACTER *2 TC(-1:15),T0
      DATA TC/'IV',
     &  'DF','BK','B2','B3','B4','B5','B6','GY',
     &  'WH','GN','YE','BR','RD','MA','CY','BL'/
      TCOL=TC(NCOL)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------- DACOL_NUM
CH
      ENTRY DACOL_NUM(TCOL,NCOL)
CH
CH --------------------------------------------------------------------
CH
      DO NCOL=15,-1,-1
        IF(TCOL.EQ.TC(NCOL)) RETURN
      END DO
      NCOL=-2
      END
*DK DAC_DRAW_VECTOR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++ DAC_DRAW_VECTOR
CH
      SUBROUTINE DAC_DRAW_VECTOR(TANSW,FYES)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TANSW
      CHARACTER *2 TPIC
      LOGICAL FYES,FIN
      DATA G999/999./
      CALL DAREA('M',TANSW,0,12,IARE,FYES)
      IF(FYES.AND.ISTODS(4,IARE,IWUSDO).GT.0) THEN
        TPIC=TPICDP(ISTODS(5,IARE,IWUSDO))
        DO L=NCALDA,-1
          IF(TNAMDA(L).EQ.'EF_MI') THEN
            NAL=IALPDA(L)
            CALL DQSET(IARE,0.,0.)
            CALL DPARGV(87,'STR',2,DLINDD)
            CALL DV_QVEC_VECTOR(NAL,PX,PY,PZ,P,IC,NCOL1,NCOL2)
            CALL DV_QVEC_VECTOR_POL(NAL,P,FI,TE,IC,NCOL1,NCOL2)
            CALL DGLEVL(NCOL2)
            IF(     TPIC.EQ.'YX') THEN
              CALL DQL2E(0.,0.,PX*G999,PY*G999)
              GO TO 10
            ELSE IF(TPIC.EQ.'RZ') THEN
              TPARDA=
     &          'J_PFI,J_PTE'
              CALL DPARAM(11
     &          ,J_PFI,J_PTE)
              FIMID=PSTODS(1,J_PFI,IARE,IWUSDO)
              IF(PSTODS(2,J_PTE,IARE,IWUSDO).NE.0.) THEN
                FMIN=FIMID-90.
                FMAX=FIMID+90.
              ELSE
                FMIN=-999.
                FMAX= 999.
              END IF
              PR=SQRT(PX*PX+PY*PY)
              FI=DFINXT(FIMID,FI)
              IF(FI.LT.FMIN.AND.FI.GT.FMAX) PR=-PR
              CALL DQL2E(0.,0.,PZ*G999,PR*G999)
              GO TO 10
            ELSE IF(TPIC.EQ.'FT') THEN
              CALL DPARGV(98,'FRM',2,RC)
              CALL DPARGV(98,'UWF',2,DLINDD)
              CALL DPARGV(98,'UNP',2,POCIA)
              CALL DQPOC(-TE,FI     ,H,V1,FIN)
              CALL DQPOC(-TE,FI+360.,H,V2,FIN)
              CALL DQSET1(IARE,0.,0.)
              CALL DQCIRC(H,V1,RC,-1,NCOL2,POCIA)
              CALL DQCIRC(H,V2,RC,-1,NCOL2,POCIA)
              GO TO 10
            ELSE IF(TPIC.EQ.'FR') THEN
              CALL DQL2E(-G999,FI     ,G999,FI     )
              CALL DQL2E(-G999,FI+360.,G999,FI+360.)
            END IF
          END IF
        END DO
      END IF
      FYES=.FALSE.
      RETURN
   10 FYES=.TRUE.
      END
*DK DAC_DRAW_MISSING_VECTOR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++ DAC_DRAW_VECTOR
CH
      SUBROUTINE DAC_DRAW_MISSING_VECTOR(TPR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TPR
      EXTERNAL DAC_DRAW_RZ_MV
      DIMENSION HD(2),VD(2),HU(2),VU(2)
      LOGICAL FYES,FIN
      MDLRDP=7000
      DO L=NCALDA,-1
        IF(TNAMDA(L).EQ.'EF_MI') THEN
          NAL=IALPDA(L)
          KPIKDP=NAL
          CALL DPARGV(87,'STR',2,DLINDD)
          CALL DV_QVEC_VECTOR(NAL,PX,PY,PZ,P,IC,NCOL1,NCOL2)
          CALL DV_QVEC_VECTOR_POL(NAL,P,FI,TE,IC,NCOL1,NCOL2)
          IF(NCOL2.GE.0) THEN
            CALL DPARGV(18,'PTO',2,RTO)
            CALL DGLEVL(NCOL2)
            IF(     TPR.EQ.'YX') THEN
              CALL DQL2EP(0.,0.,PX*RTO,PY*RTO)
            ELSE IF(TPR.EQ.'RZ') THEN
              TPARDA=
     &          'J_PFI,J_PTE'
              CALL DPARAM(11
     &         ,J_PFI,J_PTE)
              FIMID=PARADA(2,J_PFI)
              IF(PARADA(4,J_PTE).NE.0.) THEN
                FMIN=FIMID-90.
                FMAX=FIMID+90.
              ELSE
                FMIN=-999.
                FMAX= 999.
              END IF
              PR=SQRT(PX*PX+PY*PY)
              FI=DFINXT(FIMID,FI)
              IF(FI.LT.FMIN.OR.FI.GT.FMAX) PR=-PR
              HU1=0.
              VU1=0.
              RTO=RTO/MAX(ABS(PZ),ABS(PR))
              HU2=RTO*PZ
              VU2=RTO*PR
              CALL DQPOC(HU1,VU1,HD1,VD1,FIN)
              CALL DQPOC(HU2,VU2,HD2,VD2,FIN)
              CALL DAC_DRAW_RZ_MV_0(HU1,VU1,HU2,VU2)
              CALL DDRAWA(DAC_DRAW_RZ_MV,0.,HD1,VD1,1.,HD2,VD2)
              IF(IDEB.NE.0) THEN
                CALL DGLEVL(IDEB)
                CALL DQL2EP(0.,0.,PZ*RTO,PR*RTO)
              END IF
            ELSE IF(TPR.EQ.'FT') THEN
              CALL DPARGV(98,'FRM',4,RC)
              IF(RC.EQ.1.) THEN
                CALL DPARGV(98,'FRM',2,RC)
                CALL DPARGV(98,'UWF',2,DLINDD)
                CALL DPARGV(98,'UNP',2,POCIA)
                CALL DQPOC(-TE,FI     ,H,V1,FIN)
                CALL DQPOC(-TE,FI+360.,H,V2,FIN)
                CALL DQSET1(IARE,0.,0.)
                CALL DQCIRC(H,V1,RC,-1,NCOL2,POCIA)
                CALL DQCIRC(H,V2,RC,-1,NCOL2,POCIA)
              END IF
            ELSE IF(TPR.EQ.'FR') THEN
              CALL DQL2EP(-RTO,FI     ,RTO,FI     )
              CALL DQL2EP(-RTO,FI+360.,RTO,FI+360.)
            END IF
            RETURN
          END IF
        END IF
      END DO
      FMISDA=.FALSE.
      END
*DK DAC_DRAW_RZ_MV
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++ DAC_DRAW_RZ_MV
CH
      SUBROUTINE DAC_DRAW_RZ_MV(T1,HD1,VD1,T2,HD2,VD2,TM,HDM,VDM,DD,FC)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INPUT T1,HD1,VD1,T2,HD2,VD2
C     OUTPUT TM,HDM,VDM, FC if(true) continue to divide track further
C
      INCLUDE 'DALI_CF.INC'
      LOGICAL FC,FIN
      DIMENSION HD(3),VD(3)
      DATA DMIN/2./
      TM=0.5*(T1+T2)
      HUM=HU1+DUH*TM
      VUM=VU1+DUV*TM
      CALL DQPOC(HUM,VUM,HDM,VDM,FIN)
      HDS=0.5*(HD1+HD2)
      VDS=0.5*(VD1+VD2)
      DD=(HDM-HDS)**2+(VDM-VDS)**2
      IF(DD.LE.DMIN) THEN
        HD(1)=HD1
        VD(1)=VD1
        HD(2)=HDM
        VD(2)=VDM
        HD(3)=HD2
        VD(3)=VD2
        CALL DQLIE1(HD(1),VD(1))
        CALL DQLIE1(HD(2),VD(2))
        FC=.FALSE.
      ELSE
        FC=.TRUE.
      END IF
      RETURN
CH
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------- DSC_EO
CH
      ENTRY DAC_DRAW_RZ_MV_0(HU0,VU0,HU2,VU2)
CH
CH --------------------------------------------------------------------
CH
C     .................................... HU=HU1+(HU2-HU1)*T   T=0,1
C     .................................... VU=VU1+(VU2-VU1)*T
      HU1=HU0
      VU1=VU0
      DUH=HU2-HU1
      DUV=VU2-VU1
      END
*DK DAC_COL_EO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++ DAC_COL_EO
CH
      SUBROUTINE DAC_COL_EO(NCEO)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DIMENSION NCEO(*)
      END
*DK DSC_EO_0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSC_EO_0
CH
      SUBROUTINE DSC_EO_0
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
      DIMENSION ICOL(8)
      PARAMETER (MCEO=199)
      DIMENSION NCEO(MCEO)
      CALL DSC_CONST(FV,NCOL)
      IF(NCOL.GE.0) THEN
        MOCO=0
      ELSE
        CALL DPARGI(70,'FEO',MOCO)
        IF(MOCO.EQ.0) THEN
C         ................................................ CONSTANT COLOR
          CALL DPAR_SET_CO(53,'CEO')
          NCOL=ABS(ICOLDP)
        ELSE IF(MOCO.EQ.1) THEN
C         ................................................ COLOR(ENERGY)
          CALL DPAR_GET_ARRAY(47,'CC1',8,ICOL)
          CALL DPARGV(70,'FFO',2,EFAC)
        ELSE IF(MOCO.EQ.2) THEN
C         ................................................ COLOR(CUT)
          CALL DPAR_GET_ARRAY(53,'CE1',2,ICOL)
          CALL DPARGV(70,'FCO',2,ENCUT)
        ELSE IF(MOCO.EQ.3) THEN
C         ................................................ COLOR(NT)
          CALL DPARGI(46,'CNU',NUCOL)
          NUCOL=MIN(8,NUCOL)
          CALL DPAR_GET_ARRAY(46,'CC1',NUCOL,ICOL)
        ELSE
C         ................................................ Color of ALPHA EF
          CALL DAC_COL_EO(NCEO)
        END IF
      END IF
      RETURN
CH
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------- DSC_EO
CH
      ENTRY DSC_EO(K,KCOL)
CH
CH --------------------------------------------------------------------
CH
      IF(     MOCO.EQ.0) THEN
C       ................................................ CONSTANT COLOR
        KCOL=NCOL
      ELSE IF(MOCO.EQ.1) THEN
C       ................................................ COLOR(ENERGY)
        E=DVEO(IVENDV,K)
        CALL DSCLOG(E,EFAC,ICOL,KCOL)
      ELSE IF(MOCO.EQ.2) THEN
C       ................................................ COLOR(CUT)
        E=DVEO(IVENDV,K)
        IF(E.LT.ENCUT) THEN
          KCOL=ICOL(1)
        ELSE
          KCOL=ICOL(2)
        END IF
      ELSE IF(MOCO.EQ.3) THEN
C       ................................................ COLOR(NT)
        KCOL=ICOL(1+MOD(K,NUCOL))
      ELSE
C       ................................................ Color of ALPHA EF
        KCOL=NCEO(K)
      END IF
      END
*DK DAC_COL_EF_0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++ DAC_COL_EF_0
CH
      SUBROUTINE DAC_COL_EF_0
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CALL DPARGI(98,'ACF',ILCH)
      CALL DV_QVEC_DEF_COL_IN
      CALL DV_QVEC_DEF_COL(5,ILCH,NCTRDC(1))
      RETURN
CH
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------- DSC_EO
CH
      ENTRY DAC_COL_EF(NAL,NCOL)
CH
CH --------------------------------------------------------------------
CH
      CALL DV_QVEC_VECTOR_COL(NAL,NCDF,NCOL)
      IF(NCOL.EQ.0) THEN
        NCOL=IOCODA(5)
        IF(NCOL.EQ.0) NCOL=NCDF
      END IF
      END
*DK DAC_GC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DAC_GC
CH
      SUBROUTINE DAC_GC
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C    Draw cursor line
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(500),V(500),IH(500),IV(500),NDSEG(2,50)
      CHARACTER *2 TPIC
      LOGICAL FU
      EXTERNAL DAC_GC_M
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_AFM,J_APR'
      CALL DPARAM(39
     &  ,J_AFM,J_APR)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DGCURG(HC,VC)
      DO IW=0,12
        IF(ISTODS(4,IW,IWUSDO).GT.0) THEN
          IF(HMINDG(IW).LE.HC.AND.HC.LT.HHGHDG(IW).AND.
     &       VMINDG(IW).LE.VC.AND.VC.LT.VHGHDG(IW)) GO TO 1
        END IF
      END DO
      IW=1
    1 DO NW=IW,IW+12
        KW=MOD(NW,13)
        IF(ISTODS(4,KW,IWUSDO).GT.0) THEN
          TPIC=TPICDP(ISTODS(5,KW,IWUSDO))
          IF(TPIC.EQ.'AP') THEN
            NPR=PSTODS(1,J_APR,KW,IWUSDO)
            IF(     NPR.EQ. 2.OR.NPR.EQ. 3) THEN
              FU=.TRUE.
              GO TO 2
            ELSE IF(NPR.EQ.-2.OR.NPR.EQ.-3) THEN
              FU=.FALSE.
              GO TO 2
            END IF
          END IF
        END IF
      END DO
      GO TO 9
2     IARE=IAREDO
      IAREDO=KW
      FIMID=PSTODS(1,J_AFM,IAREDO,IWUSDO)
      CALL DAC_GC_VEC0(FU,FIMID)
      CALL DQSET(IAREDO,0.,0.)
      HC=0.5*(HMINDG(IAREDO)+HHGHDG(IAREDO))
      VC=0.5*(VMINDG(IAREDO)+VHGHDG(IAREDO))
      IHC=HC
      IVC=VC
      CALL DAC_GC_M0(IHC,IVC,H,V)
      NUMPL=2
      NDSEG(1,1)=1
      NDSEG(2,1)=2
C
      NDSEG(1,2)=3
      NDSEG(2,2)=2
C
      NDSEG(1,3)=5
      NDSEG(2,3)=5
C
      CALL DGLINM(IHC,IVC,H,V,IH,IV,NAR,DAC_GC_M,.FALSE.,NDSEG,NUMPL)
      IAREDO=IARE
      RETURN
    9 CALL DWRT('Selected window is empty or has wrong projection.#')
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------- DAC_GC_FX
CH
      ENTRY DAC_GC_FX
CH
C     called by DAC
CH --------------------------------------------------------------------
      CALL DPAR_SET_CO(98,'MMC')
      CALL DPAR_SET_WIDTH(98,'MMW',10,' ')
      IF(NUMPL.GT.4) THEN
        DO K=4,NUMPL-1
          CALL DGDRAW(NDSEG(2,K),H(NDSEG(1,K)),V(NDSEG(1,K)))
        END DO
      END IF
      END
*DK DAC_GC_M
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DAC_GC_M
CH
      SUBROUTINE DAC_GC_M(IHC,IVC,H,V,NDSEG,NUMPL,FBUT,TKBD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    MOVE CROSS OR MARKERS
C    Called by : DGTLNM <- DM3D
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(*),V(*),NDSEG(2,*)
      CHARACTER *(*) TKBD
      LOGICAL FBUT(*),FB1,FB2,FB3,FIN
      DATA FB1/.TRUE./,FB2/.TRUE./,FB3/.TRUE./,J/1/
      DATA DA/2./,DG/100./,DMIN/20./,DS2/8./,DS3/14./,GCL/0.2/,N/0/
      DIMENSION HC(2),VC(2),FI(2),TE(2)
      DIMENSION XYZ(3,2),XYZP(3),XYZGC(3)
      DIMENSION DSCAL(5)
      DATA DSCAL/30.,15.,10.,0.,45./
      CHARACTER *49 T1
C     ......   123456789 123456789 123456789 123456789 123456789
      DATA T1/'Step      FI 1   TE 1      FI 2   TE 2     Delta'/
C     ........ 30.5    -180.1 -180.1    -360.1 -180.1    -360.1
C
C     ......................... Button 1 pressed = move corner
      IF(.NOT.FBUT(1)) THEN
        IF(FB1) THEN
          FB1=.FALSE.
          IF(NB1.EQ.1) THEN
            NB1=2
            H(3)=HC(2)
            V(3)=VC(2)
            H(1)=GCL*H(2)+(1.-GCL)*HC(1)
            V(1)=GCL*V(2)+(1.-GCL)*VC(1)
            IMO=4
            IFX=2
            IHF=1
          ELSE
            NB1=1
            H(1)=HC(1)
            V(1)=VC(1)
            H(3)=GCL*H(4)+(1.-GCL)*HC(2)
            V(3)=GCL*V(4)+(1.-GCL)*VC(2)
            IMO=2
            IFX=4
            IHF=3
          END IF
          IHC=H(IMO)
          IVC=V(IMO)
        END IF
        RETURN
      END IF
C     ......................... Button 2 pressed = move other point
      IF(.NOT.FBUT(2)) THEN
        IF(FB2) THEN
          FB2=.FALSE.
          IF(NB2.EQ.1) THEN
            NB2=2
            NDSEG(2,1)=0
            NDSEG(2,2)=0
          ELSE
            NB2=1
            NDSEG(2,1)=2
            NDSEG(2,2)=2
          END IF
        END IF
        RETURN
      END IF
C     ......................... Button 3 pressed = output
      IF(.NOT.FBUT(3)) THEN
        IF(FB3) THEN
          FB3=.FALSE.
          NB3=MOD(NB3,5)+1
          DSTEP=DSCAL(NB3)
        END IF
        RETURN
      END IF
C     ....................................... no button pressed
      FB1=.TRUE.
      FB2=.TRUE.
      FB3=.TRUE.
      H(IMO)=IHC
      V(IMO)=IVC
      CALL DAC_GC_HV_TO_VEC(H(IMO),V(IMO),TE(NB1),FI(NB1),XYZ(1,NB1))
      IHC=H(IMO)
      IVC=V(IMO)
      CD=0.
      DO K=1,3
        CD=CD+XYZ(K,1)*XYZ(K,2)
      END DO
      DELTA=ACOSD(CD)
      WRITE(TXTADW,1000) DSTEP,(FI(K),TE(K),K=1,2),DELTA
      CALL DWR_OVER_PRINT(49)
      IF(NB2.EQ.1) THEN
        NUMPL=2
      ELSE
        N=5
        HD=IHC
        VD=IVC
        CALL DAC_GC_SQ(HD,VD,DS3,H(N),V(N))
        N=10
        NUMPL=3
        IF(DELTA.GT.DMIN) THEN
          SD=SQRT(1.-CD*CD)
          IF(SD.NE.0.) THEN
            QF=1./SD
            DO K=1,3
              XYZP(K)=(XYZ(K,2)-CD*XYZ(K,1))*QF
            END DO
            NP=N
            NUMPL=4
            NDSEG(1,NUMPL)=N
            CALL DAC_GC_VEC_TO_HV(XYZ,H(N),V(N))
            DO A=DA,360.,DA
              CA=COSD(A)
              SA=SIND(A)
              DO K=1,3
                XYZGC(K)=CA*XYZ(K,1)+SA*XYZP(K)
              END DO
              N=N+1
              CALL DAC_GC_VEC_TO_HV(XYZGC,H(N),V(N))
              IF(ABS(H(N)-H(N-1)).GT.DG.OR.
     &           ABS(V(N)-V(N-1)).GT.DG) THEN
                NDSEG(2,NUMPL)=N-NP
                NUMPL=NUMPL+1
                NDSEG(1,NUMPL)=N
                NP=N
              END IF
            END DO
            NDSEG(2,NUMPL)=N-NP
            IF(DSTEP.GT.0) THEN
              N=N+1
              DO A=DSTEP,360.,DSTEP
                CA=COSD(A)
                SA=SIND(A)
                DO K=1,3
                  XYZGC(K)=CA*XYZ(K,1)+SA*XYZP(K)
                END DO
                CALL DAC_GC_VEC_TO_HV(XYZGC,HH,VV)
                NUMPL=NUMPL+1
                CALL DAC_GC_SQ(HH,VV,DS1,H(N),V(N))
                NDSEG(1,NUMPL)=N
                NDSEG(2,NUMPL)=5
                N=N+5
              END DO
            END IF
          END IF
        END IF
        NUMPL=NUMPL+1
        CALL DAC_GC_SQ(H(IFX),V(IFX),DS2,H(N),V(N))
        NDSEG(1,NUMPL)=N
        NDSEG(2,NUMPL)=5
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------- DAC_GC_M0
CH
      ENTRY DAC_GC_M0(IHC,IVC,H,V)
CH
C     called by DAC
CH --------------------------------------------------------------------
      NB1=1
      NB2=1
      NB3=1
      DSTEP=DSCAL(NB3)
      CALL DPARGV(98,'MMS',2,DS1)
C
      H(2)=IHC
      V(2)=IVC
C
C     .................................... setup XYZ(1,1),XYZ(2,1),XYZ(3,1)
      CALL DAC_GC_HV_TO_VEC(H(2),V(2),TE(1),FI(1),XYZ)
      FI(2)=FI(1)+90.
      TE(2)=90.
C
C     ................  setup XYZ(1,2),XYZ(2,2),XYZ(3,2) perpendicular to 1
      CALL DQINV(IAREDO,H(2),V(2),TE1,FI1)
      FI2=FI1+90.
      XYZ(1,2)=COSD(FI2)
      XYZ(2,2)=SIND(FI2)
      XYZ(3,2)=0.
      CALL DAC_GC_VEC_TO_HV(XYZ(1,2),H(4),V(4))
      CALL DWRT(T1)
      WRITE(TXTADW,1000) DSTEP,(FI(K),TE(K),K=1,2),90.
 1000 FORMAT(F4.1,2(F10.1,F7.1),F10.1,'>')
      CALL DWRC
C     ................................ setup corners
      HC(1)=1.
      VC(1)=VHGHDG(13)-1.
      HC(2)=1.
      VC(2)=1.
C
      H(1)=HC(1)
      V(1)=VC(1)
      H(3)=GCL*H(4)+(1.-GCL)*HC(2)
      V(3)=GCL*V(4)+(1.-GCL)*VC(2)
      IMO=2
      IFX=4
      IHF=3
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++ DAC_GC_M_ROT
CH
      ENTRY DAC_GC_M_ROT(FR,TR,AR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      IF(N.EQ.0) RETURN
      TR=TE(1)
      FR=FI(1)
      SF=SIND(FR)
      CF=COSD(FR)
      ST=SIND(90.-TR)
      CT=COSD(90.-TR)
      X1=SIND(TE(2)*COSD(FI(2)))
      Y1=SIND(TE(2)*SIND(FI(2)))
      Z1=COSD(TE(2))
      X2= CF*X1+SF*Y1
      Y2=-SF*X1+CF*Y1
C     X3= CT*X2+ST*Z1
      Z3=-ST*X2+CT*Z1
C     Z4=-SG*Y2+CG*Z3  calculat ALPHA so that Z4=0:  SG=SIND(ALPHA) ...
C     ................ SG/CG=Z3/Y2
      IF(Z3.EQ.0..AND.Y2.EQ.0.) RETURN
      AR=DATN2D(Z3,Y2)
      END
*DK DAC_GC_HV_TO_VEC
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++ DAC_GC_HV_TO_VEC
CH
      SUBROUTINE DAC_GC_HV_TO_VEC(H,V,TE,FI,XYZ)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DIMENSION XYZ(3)
      DATA SS/1./
      LOGICAL FIN,FU,FUIN
      CALL DQINV(IAREDO,H,V,TE,FT)
      TE=-TE
      IF(     TE.GT.180.) THEN
        TE=180.
      ELSE IF(TE.LT.  0.) THEN
        TE=  0.
      END IF
      ST=SIND(TE)
      FT=FT-180.
      IF(FU) THEN
        FMAX=180.*ST
        IF(     FT.GT. FMAX) THEN
          FI= 180.
        ELSE IF(FT.LT.-FMAX) THEN
          FI=-180.
        ELSE IF(ST.NE.0.) THEN
          FI=FT/ST
C         .................................. If ST=0, FI is not modified.
        END IF
      ELSE
        FI=FT
        IF(     FI.GT. 180.) THEN
          FI= 180.
        ELSE IF(FI.LT.-180.) THEN
          FI=-180.
        END IF
      END IF
      FI=FI+SS*FIMID
      FI=MOD(3600.+FI,360.)
      XYZ(1)=ST*COSD(FI)
      XYZ(2)=ST*SIND(FI)
      XYZ(3)=COSD(TE)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++ DAC_GC_VEC_TO_HV
CH
      ENTRY DAC_GC_VEC_TO_HV(XYZ,H,V)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      R2=XYZ(1)*XYZ(1)+XYZ(2)*XYZ(2)
      RO=SQRT(R2)
      T=DATN2D(RO,XYZ(3))
      IF(RO.EQ.0.) THEN
        F=0.
      ELSE
        F=DATN2D(XYZ(2),XYZ(1))
      END IF
      F=F-FIMID
      IF(     F.LT.-180.) THEN
        F=F+360.
      ELSE IF(F.GE. 180.) THEN
        F=F-360
      END IF
      IF(FU) THEN
        ST=SIND(T)
        FT=180.+F*ST
      ELSE
        FT=180.+F
      END IF
      CALL DQPOC(-T,FT,H,V,FIN)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++ DAC_GC_VEC
CH
      ENTRY DAC_GC_VEC0(FUIN,FIIN)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      FU=FUIN
      FIMID=90.+FIIN
      END
*DK DAC_GC_SQ
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++ DAC_GC_SQ
CH
      SUBROUTINE DAC_GC_SQ(HH,VV,DS,H,V)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      DIMENSION H(*),V(*)
      H(1)=HH
      H(3)=HH
      H(5)=HH
      H(2)=HH+DS
      H(4)=HH-DS
      V(2)=VV
      V(4)=VV
      V(1)=VV-DS
      V(5)=V(1)
      V(3)=VV+DS
      END
*DK DAC_PI0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++  DAC_PI0
CH
      SUBROUTINE DAC_PI0(NFPI0,NLPI0)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      LOGICAL FDONE
      DATA FDONE/.FALSE./

C  From: buskulic@lapphp2.in2p3.fr "Damir Buskulic" 28-NOV-1996 12:27:20.69
C  Here is the routine to init Pi0's in DALI. It took me a little longer 
C  than I thought to make it.
C  I didn't substitute in the routine QCDE.INC and QMACRO.INC.
C  If you need the files, I can send them to you or I can even make the 
C  substitution. Just tell me.
C  In order to initialise the Pi0's, I use the routine  QPI0DO from ALPHA.
C  Normaly, this routine is called at the beginning of a program. Perhaps is 
C  it already done in yours. If so, you can delete or comment the line 
C  containing
C  
C        CALL QPI0DO
C  
C  Anyway, all the places where you have to be careful are indicated by an 
C  arrow such as
C
C	<---------------------
C
C  If you have any problem, please call me. My phone is :
C  10 (to go to france from a CERN phone) 04 50 09 16 94
C
C  Damir
c======================================================================
c sous programme de selection des pi0 les plus valides
c j.p. adapte par d.b.
c======================================================================
c modifications:
c..  30Aout 1996 : cree une particule alpha pour chaque pi0 selectionne
c..                cette particule va de KFPI0 a KLPI0
c..                ajoute la matrice de covariance
c..  28 Nov. 1996 : adapted for DALI
c======================================================================
c
c  must include QCDE     <----------------------------------
      INCLUDE 'P_QCDE.INC'
c.. JPUTIL.INC...
      PARAMETER(MXTRK=400)
      DIMENSION PROBTRK(MXTRK),IGOLD(MXTRK),PRGOLD(MXTRK)
     +,IPI0SEL(MXTRK),IGAMSEL(MXTRK),
     + PV0BT(4,MXTRK),ITV0BT(MXTRK),ITRKBT(2,MXTRK)
      COMMON/JPUTIL/PROBTRK,NGOLD,IGOLD,ESUM5,ENU
     +,IOK5,PRGOLD,NPI0SEL,IPI0SEL
     +,KFPI0,KLPI0
     +,KFGCONV,KLGCONV,IT1CONV(MXTRK),IT2CONV(MXTRK),RMACONV(MXTRK)
     +,IBSTK(MXTRK),NGAMSEL,IGAMSEL
     +,NV0BT,PV0BT,ITV0BT,ITRKBT
c.. common des pi0
      PARAMETER(MXPI0=200)
      COMMON/GAMPI0/IQPI0, NTPI0, PI0MOM(4,MXPI0),ITYPI0(MXPI0),
     +              IPI0GAM(2,MXPI0),CHIPI0(MXPI0)
      PARAMETER(MXGAM=1000)
      DIMENSION IGUSED(MXGAM)
      DIMENSION NPI0NUM(MXTRK),IPI0NUM(10,MXTRK),IBESTNUM(MXTRK),
     +CHI2BEST(MXTRK),IPI0VAL(MXTRK)
      DIMENSION VPHI(3)
      DIMENSION ISELT(4),PI0ERR(4,4)
      DATA ISELT/1,3,2,4/
c
c  must include QMACRO     <----------------------------------
      INCLUDE 'P_QMACRO.INC'
c
c
c  this is normally included in the initialisation subroutine
c  check it and remove or comment the line that follows if it is so
c
      IF(FDONE) THEN
        NFPI0=KFPI0
        NLPI0=KLPI0
        RETURN
      END IF
      FDONE=.TRUE.
c       CHECK              <----------------------------------
c
      CALL QPI0DO
c
      KFPI0=0
      KLPI0=0
      CALL VZERO(IGUSED,MXGAM)
      CALL VZERO(NPI0NUM,MXTRK)
      NPI0SEL=0
      NGAMSEL=0
      DO I=1,MXTRK
        CHI2BEST(I)=9999.
      ENDDO
C.. selectionne en priorite (iloop=1) les pi0 ayant 2 photons dans
C.. le meme ams et seulement deux
      if(iqpi0.eq.0)then
        ILOOP=1
 10     CONTINUE
        IF(ILOOP.GT.4)GOTO 900
        DO IPI0=1,NTPI0
          IF(PI0MOM(4,IPI0).GT.1.)THEN
            IF(ITYPI0(IPI0).NE.ISELT(ILOOP))GOTO 920
            J1=IPI0GAM(1,IPI0)
            J2=IPI0GAM(2,IPI0)
            I1=IPI0GAM(1,IPI0)-kfgat+1
            I2=IPI0GAM(2,IPI0)-kfgat+1
            IF(ILOOP.LE.2.AND.I1.LT.MXGAM)THEN
              IGUSED(I1)=1
            ENDIF
            IF(ILOOP.LE.2.AND.I2.LT.MXGAM)THEN
              IGUSED(I2)=1
            ENDIF
            IF(ILOOP.GT.2.AND.IGUSED(I1).EQ.1)GOTO 920
            IF(ILOOP.GT.2.AND.IGUSED(I2).EQ.1)GOTO 920
            NPI0SEL=NPI0SEL+1
            IPI0SEL(NPI0SEL)=IPI0
            IF(NPI0NUM(I1).LT.mxtrk)THEN
              NPI0NUM(I1)=NPI0NUM(I1)+1
              K=NPI0NUM(I1)
              IPI0NUM(K,I1)=IPI0
              IF(CHIPI0(IPI0).LT.CHI2BEST(I1))THEN
                CHI2BEST(I1)=CHIPI0(IPI0)
                IBESTNUM(I1)=IPI0
              ENDIF
            ENDIF
            IF(NPI0NUM(I2).LT.mxtrk)THEN
              NPI0NUM(I2)=NPI0NUM(I2)+1
              K=NPI0NUM(I2)
              IPI0NUM(K,I2)=IPI0
              IF(CHIPI0(IPI0).LT.CHI2BEST(I2))THEN
                CHI2BEST(I2)=CHIPI0(IPI0)
                IBESTNUM(I2)=IPI0
              ENDIF
            ENDIF
            XM=QM2(J1,J2)
 920        CONTINUE
          ENDIF
        ENDDO
        ILOOP=ILOOP+1
        GOTO 10
 900    CONTINUE
c.. elimine les doubles combinaisons sur la base du meilleur chi2
        NPI0VAL=0
        NLOOP=0
 910    CONTINUE
        NLOOP=NLOOP+1
        ICOUNT=0
        DO I=1,NPI0SEL
          IPI0=IPI0SEL(I)
          IF(IPI0.NE.0)THEN
            icount=icount+1
            I1=IPI0GAM(1,IPI0)-kfgat+1
            I2=IPI0GAM(2,IPI0)-kfgat+1
            IF(IBESTNUM(I1).EQ.IBESTNUM(I2))THEN
              IF(IBESTNUM(I1).EQ.IPI0)THEN
                NPI0VAL=NPI0VAL+1
                IPI0VAL(NPI0VAL)=IPI0
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        if(icount.eq.0)goto 930
c.. elimine les pi0 impliquant ces 2 photons
        DO I=1,npi0val
          ipi0=ipi0val(i)
          I1=IPI0GAM(1,IPI0)-kfgat+1
          I2=IPI0GAM(2,IPI0)-kfgat+1
          DO J=1,NPI0SEL
            JPI0=IPI0SEL(J)
            IF(JPI0.NE.0)THEN
              J1=IPI0GAM(1,JPI0)-KFGAT+1
              J2=IPI0GAM(2,JPI0)-KFGAT+1
              IF(J1.EQ.I1.OR.J1.EQ.I2.OR.
     +           J2.EQ.I1.OR.J2.EQ.I2)THEN
                IPI0SEL(J)=0
              ENDIF
            ENDIF
          ENDDO
        ENDDO
c.. recalcule les meilleurs chi2 pour les pi0 survivants
        CALL VZERO(NPI0NUM,100)
        DO j=1,MXTRK
          CHI2BEST(j)=9999.
        ENDDO
        DO J=1,NPI0SEL
          JPI0=IPI0SEL(J)
          IF(JPI0.NE.0)THEN
            J1=IPI0GAM(1,JPI0)-KFGAT+1
            J2=IPI0GAM(2,JPI0)-KFGAT+1
            IF(NPI0NUM(J1).LT.mxtrk)THEN
              NPI0NUM(J1)=NPI0NUM(J1)+1
              K=NPI0NUM(J1)
              IPI0NUM(K,J1)=JPI0
              IF(CHIPI0(JPI0).LT.CHI2BEST(J1))THEN
                CHI2BEST(J1)=CHIPI0(IPI0)
                IBESTNUM(J1)=JPI0
              ENDIF
            ENDIF
            IF(NPI0NUM(J2).LT.mxtrk)THEN
              NPI0NUM(J2)=NPI0NUM(J2)+1
              K=NPI0NUM(J2)
              IPI0NUM(K,J2)=JPI0
              IF(CHIPI0(JPI0).LT.CHI2BEST(J2))THEN
                CHI2BEST(J2)=CHIPI0(IPI0)
                IBESTNUM(J2)=JPI0
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        IF(NLOOP.LT.NPI0SEL)GOTO 910
 930    continue
c.. compte les pi0 restant
       JLOC=0
       DO I=1,NPI0SEL
         IPI0=IPI0SEL(I)
         IF(IPI0.NE.0)THEN
           JLOC=JLOC+1
           IPI0SEL(JLOC)=I
         ENDIF
       ENDDO
       NPI0SEL=JLOC
c.. ne garde que les pi0 non ambigus
       NPI0SEL=NPI0VAL
       CALL UCOPY(IPI0VAL,IPI0SEL,NPI0SEL)
c.. cree une particule alpha pour chaque pi0 selectionne
       KFPI0=0
       KLPI0=0
       JTPI0 = KVNEW(DUMMY) !vecteur local
       DO I=1,NPI0SEL
         JPI0 = KVSAVC(JTPI0,'PI0D',KRECO) !working vector for pi0
         IF(I.EQ.1)KFPI0=JPI0
         IF(I.EQ.NPI0SEL)KLPI0=JPI0
         IPI0 = IPI0SEL(I)
         CALL QVSET4(JPI0,PI0MOM(1,IPI0))
c.. remplit la matrice d erreur approchee du pi0
         PI0UX = QX(JPI0)/QP(JPI0)
         PI0UY = QY(JPI0)/QP(JPI0)
         PI0UZ = QZ(JPI0)/QP(JPI0)
         PI0SE2= (.07 * QE(JPI0))**2
         pi0err(1,1) = pi0ux**2 * pi0se2
         pi0err(1,2) = pi0ux*pi0uy * pi0se2
         pi0err(1,3) = pi0ux*pi0uz * pi0se2
         pi0err(1,4) = pi0ux * pi0se2
         pi0err(2,1) = pi0err(1,2)
         pi0err(2,2) = pi0uy**2 * pi0se2
         pi0err(2,3) = pi0uy*pi0uz * pi0se2
         pi0err(2,4) = pi0uy * pi0se2
         pi0err(3,1) = pi0err(1,3)
         pi0err(3,2) = pi0err(2,3)
         pi0err(3,3) = pi0uz**2 * pi0se2
         pi0err(3,4) = pi0uz * pi0se2
         pi0err(4,1) = pi0err(1,4)
         pi0err(4,2) = pi0err(2,4)
         pi0err(4,3) = pi0err(3,4)
         pi0err(4,4) = pi0se2
         call qvsets(JPI0,PI0ERR)
       ENDDO
c
        CALL VZERO(IGUSED,MXGAM)
        DO I=1,NPI0SEL
          IPI0=IPI0SEL(I)
          I1=IPI0GAM(1,IPI0)-kfgat+1
          I2=IPI0GAM(2,IPI0)-kfgat+1
          IF(I1.GT.0.AND.I1.LT.MXGAM)THEN
            IGUSED(I1)=1
          ENDIF
          IF(I2.GT.0.AND.I2.LT.MXGAM)THEN
            IGUSED(I2)=1
          ENDIF
        ENDDO
C.. Photons de 500MeV ou plus non associes:
        DO 20 I = KFGAT,KLGAT
          igam=i-kfgat+1
          IF(QE(I).GT.0.5.AND.IGUSED(IGAM).EQ.0)THEN
            NGAMSEL=NGAMSEL+1
            IGAMSEL(NGAMSEL)=I
          ENDIF
  20    CONTINUE
      ENDIF
 999  NFPI0=KFPI0
      NLPI0=KLPI0
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------------ DAC_PI0_0
CH
      ENTRY DAC_PI0_GA0(NGUM,NGUM0)
CH
CH --------------------------------------------------------------------
      NGUM=NGAMSEL
      NGUM0=KFGAT
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------------ DAC_PI0_0
CH
      ENTRY DAC_PI0_GA(KGASL,NGASL)
CH
CH --------------------------------------------------------------------
      NGASL=IGAMSEL(KGASL)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------------ DAC_PI0_0
CH
      ENTRY DAC_PI0_0
CH
CH --------------------------------------------------------------------
      FDONE=.FALSE.
      END
