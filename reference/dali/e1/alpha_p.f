*DK DAD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++  DAD
CH
      SUBROUTINE DAD
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
      CHARACTER *2 TPRO(-3:4)
C                -3   -2   -1    0    1    2    3    4
      DATA TPRO/'TF','FT','**','RO','AC','UT','TU','DL'/
      CHARACTER *2 TANSW,TLIST
      CHARACTER *11 TGT
      DATA TGT/'(GG:AL:T) :'/
      CHARACTER *2 TMIS(-1:1)
      DATA TMIS/'ml','  ','mc'/
      DIMENSION HRB(5),VRB(5)
      CHARACTER *2 TRB(10)
      DATA TRB/10*'**'/
      DATA BMOD/1./
      LOGICAL FYES,FCHG
      CHARACTER *49 T1,T2,T3,T4
C                       1         2         3         4         5
C              12345678901234567890123456789012345678901234567890
      DATA T1/'P?:W?:Z?:DL  "tracks": CH(CF)  + Interaction List'/
      DATA T2/'FM_123 TE_123 AL_123 FR_123 TO$123 AS$123 DS$123 '/
      DATA T3/'FM_123 SK$123 SL$1234 WI_123  SS_1234 SY$123   mc'/
      DATA T4/'FI_12345 DF_12345 TE_12345 DT_12345              '/
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_AFM,J_ATE,J_AAL,J_APR'
      CALL DPARAM(39
     &  ,J_AFM,J_ATE,J_AAL,J_APR)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DPARGI(39,'ALI',ISLST)
      CALL DPARGV(39,'ALI',4,ALST)
      CALL DPARGV(39,'AAC',4,CALC)
      CALL DPARGV(98,'ACF',2,CHCF)
      CALL DV_QVEC_IN(NFSTDA,NLSTDA)
      FDTRDC=.FALSE.
      CALL DSCTR
C     ............................................................. DRAW HEADER
  930 TLIST=TLSTDA(ISLST)
      NPRO=PARADA(2,J_APR)
      T1(10:11)=TPRO(NPRO)
      T1(24:25)=TLIST
      IF(CALC.EQ.1.) THEN
        T1(32:49)='+ Interaction List'
      ELSE
        T1(32:49)=' '
      END IF
      IF(ALST.EQ.1.) THEN
        IF(TLIST.EQ.'CH'.AND.CHCF.EQ.1.) THEN
          T1(26:30)='(CF)'
        ELSE
          T1(26:30)=' '
        END IF
      ELSE
        T1(26:32)=' = OF'
      END IF
      CALL DTYPT('TYPE',TPICDO,0,0,0,' ',T1)
      IPRO=ABS(NPRO)
      CALL DPARP0
      IF(     IPRO.LE.1) THEN
        CALL DPAROP(39,'AFM_ATE_AAL_AFR_ATO_AAS_ADS')
        CALL DTYPT('TYPE',' ',N_1_DA,N_2_DA,PARADA,TPOPDA,T2)
      ELSE IF(IPRO.NE.4) THEN
        CALL DPAROP(39,'AFM_ATO_ASK_ASL_AWI_ASS_ASY')
        IF(FMISDA) THEN
          CALL DPARGI(39,'ATM',ITM)
          T3(48:49)=TMIS(ITM)
        ELSE
          T3(48:49)='  '
        END IF
        CALL DTYPT('TYPE',' ',N_1_DA,N_2_DA,PARADA,TPOPDA,T3)
        IF(IZOMDO.EQ.1) THEN
          CALL DPAROP(11,'PFI_PDF_PTE_PDT_PDD')
          CALL DTYPT('TYPE',' ',N_1_DA,N_2_DA,PARADA,TPOPDA,T4)
        END IF
      END IF
C     .............................................................. OPERATOR
  936 CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(1,1,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  0,0,' ',0,
     &  NEXEC,FCHG,TANSW)
      CALL DGZOOM(6,-1,0,0)
C     ................................................................... PICK
      IF(TANSW.EQ.'PI'.AND.NEXEC.EQ.2) THEN
        IRC=0
        CALL DPOS(TANSW,FDUM,TDUM,NEXEC,FCHG)
        IF(NEXEC.EQ.3) GO TO 930
      END IF
      GO TO (910,920,930,940), NEXEC
  910 RETURN
  920 CALL DQZOM(TRB,TANSW,1.,HRB,VRB,BMOD,NYES)
      IF(NYES.LT.0) GO TO 929
      IF(NYES.EQ.1.OR.NYES.EQ.2) GO TO 936
      IF(NYES.GT.2) THEN
         CALL DALRBC(FYES,HRB,VRB)
         IF(FYES) IZOMDO=1
         GO TO 930
      END IF
      CALL DO_STR('DB')
      IF(TANSW.EQ.'DB') THEN
        NPRO=PARADA(2,J_APR)
        IPRO=PSTODS(1,J_APR,IAREDO,IWUSDO)
        IF(NPRO.LT.-1.AND.IPRO.LT.-1) THEN
          IZOMDO=1
C         :::::::::::::::::::::::::::::::::::::::::::::::::::::::
          TPARDA=
     &      'J_PFI'
          CALL DPARAM(10
     &      ,J_PFI)
C         ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
          FI=PARADA(2,J_PFI)
          IF(ISTODS(6,IAREDO,IWUSDO).EQ.0) THEN
            FI=FI+90.+PSTODS(1,J_AFM,IAREDO,IWUSDO) 
            FI=DFINXT(180.,FI)
          ELSE
            FM=PARADA(2,J_PFI)
            FI=DFINXT(FM,FI)
          END IF
          CALL DADRCB(FI,HRB,VRB)
          CALL DQZBN(HRB,VRB)
          IF(FCHG) GO TO 930
          GO TO 936
        END IF
      END IF
      CALL DO_STR('NZ')
      IF(TANSW.EQ.'NZ') THEN
         IZOMDO=0
         GO TO 936
      END IF
      CALL DO_STR('ZO')
      IF(TANSW.EQ.'ZO') THEN
         IZOMDO=1
         GO TO 930
      END IF
      CALL DO_STR_LIST(MLSTDA,TLSTDA,'ALPHA list')
      DO K=1,MLSTDA
        IF(TANSW.EQ.TLSTDA(K)) THEN
          ISLST=K
          CALL DPARSV(39,'ALI',2,FLOAT(ISLST))
          ALST=1.
          CALL DPARSV(39,'ALI',4,ALST)
          CALL DPARSV(39,'AAC',2,0.)
          GO TO 930
        END IF
      END DO
      CALL DO_STR('US: user list')
      IF(TANSW.EQ.'US') THEN
        ISLST=0
        CALL DPARSV(39,'AAC',2,1.)
        GO TO 930
      END IF
      CALL DO_STR('IL: interaction list')
      IF(TANSW.EQ.'IL') THEN
        CALC=-CALC
        CALL DPARSV(39,'AAC',4,CALC)
        GO TO 930
      END IF
      CALL DO_STR('IO: interaction list only')
      IF(TANSW.EQ.'IO') THEN
        ALST=-ALST
        CALL DPARSV(39,'ALI',4,ALST)
        GO TO 930
      END IF
      CALL DO_STR_LIST(8,TPRO,'projection')
      DO K=-3,4
        IF(TANSW.EQ.TPRO(K)) THEN
          CALL DPARSV(39,'APR',2,FLOAT(K))
          GO TO 930
        END IF
      END DO
      CALL DO_STR('TP"TT: = toggle projection')
      IF(TANSW.EQ.'TP'.OR.TANSW.EQ.'PP') THEN
        CALL DGINMA(TGT)
        GO TO 936
      END IF
      CALL DO_STR('CF: toggle color source for CH')
      IF(TANSW.EQ.'CF'.AND.ISLST.EQ.1) THEN
        CHCF=-CHCF
        CALL DPARSV(98,'ACF',2,CHCF)
        CALL DV_QVEC_DEF_COL_IN
        GO TO 940
      END IF
      CALL DO_STR('MG"MM: great circle')
      IF(TANSW.EQ.'MG'.OR.TANSW.EQ.'MM') THEN
        CALL DAC_GC
        GO TO 930
      END IF
      CALL DO_STR('FG: fix great circle')
      IF(TANSW.EQ.'FG') THEN
        CALL DAC_GC_FX
        GO TO 936
      END IF
      CALL DO_STR('AG: Get angles great circle')
      IF(TANSW.EQ.'AG') THEN
        CALL DAC_GC_M_ROT(PARADA(2,J_AFM),
     &                    PARADA(2,J_ATE),
     &                    PARADA(2,J_AAL))
        GO TO 930
      END IF
      CALL DO_STR('MA: Set rotation angle via MM')
      IF(TANSW.EQ.'MA') THEN
        CALL DAC_GC
        CALL DAC_GC_M_ROT(PARADA(2,J_AFM),
     &                    PARADA(2,J_ATE),
     &                    PARADA(2,J_AAL))
        GO TO 930
      END IF
      CALL DO_STR('TM: toggle missing mass circle/chimney')
      IF(TANSW.EQ.'TM') THEN
        CALL DPAR_TOGGLE(39,'ATM',2)
        GO TO 930
      END IF
      CALL DO_STR('TS: toggle scale of UT')
      IF(TANSW.EQ.'TS') THEN
        CALL DPAR_TOGGLE(98,'CCI',4)
        GO TO 930
      END IF
      CALL DO_STR('TN: toggle naming of UT')
      IF(TANSW.EQ.'TN') THEN
        CALL DPAR_TOGGLE(98,'CSC',4)
        GO TO 930
      END IF
      CALL DO_STR('D1"to"DW: Redisplay')
      IAR=IAREDO
      CALL DAREA('D',TANSW,0,12,NAR,FYES)
      IF(FYES) THEN
        CALL DPCEAR(NAR)
        IAREDO=IAR
        GO TO 930
      END IF
      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('DAD')
        GO TO 936
      END IF
  929 CALL DWR_IC(TANSW)
      GO TO 936
  940 CALL DADD(FMACDM)
      GO TO 930
      END
*DK DADRCB
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DADRCB
CH
      SUBROUTINE DADRCB(FI,HRB,VRB)
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
      DIMENSION HRB(4),VRB(4)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PDF,J_PTE,J_PDT'
      CALL DPARAM(10
     &  ,J_PDF,J_PTE,J_PDT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      DT=PARADA(2,J_PDT)*0.5
      TE=PARADA(2,J_PTE)
      DF=PARADA(2,J_PDF)*0.5
      HRB(1)=-TE-DT
      HRB(3)=-TE+DT
      VRB(1)= FI-DF
      VRB(3)= FI+DF
      HRB(2)=HRB(3)
      HRB(4)=HRB(1)
      VRB(2)=VRB(1)
      VRB(4)=VRB(3)
      END
*DK DAD_UT_ROT
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DAD_UT_ROT
CH
      SUBROUTINE DAD_UT_ROT(HRB,VRB)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      DIMENSION HRB(*),VRB(*)
      HRB(5)=HRB(1)
      VRB(5)=VRB(1)
      DO K=1,4
        HRB(K)=HRB(K+1)
        VRB(K)=VRB(K+1)
      END DO
      END
*DK DAD_UT_ROT_INV
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DAD_UT_ROT_INV
CH
      SUBROUTINE DAD_UT_ROT_INV(HRB,VRB)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      DIMENSION HRB(*),VRB(*)
      DO K=5,2,-1
        HRB(K)=HRB(K-1)
        VRB(K)=VRB(K-1)
      END DO
      HRB(1)=HRB(5)
      VRB(1)=VRB(5)
      END
*DK DALRBC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DALRBC
CH
      SUBROUTINE DALRBC(FYES,HRB,VRB)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION HRB(*),VRB(*)
      LOGICAL FYES
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PTE,J_PDT'
      CALL DPARAM(11
     &  ,J_PFI,J_PDF,J_PTE,J_PDT)
      TPARDA=
     &  'J_AFM,J_APR'
      CALL DPARAM(39
     &  ,J_AFM,J_APR)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      FYES=.FALSE.
      PRO=PSTODS(1,J_APR,NWINDZ,IWUSDO)
      FIMID=PSTODS(1,J_AFM,NWINDZ,IWUSDO)
      IF(FIMID.GE.360.) FIMID=FIMID-360.
      APR=ABS(PRO)
      IF(APR.NE.2.AND.APR.NE.3) THEN
C       ................................................. RO,AC,DL
        CALL DWRT('Wrong AP-projection on the window')
        RETURN
      END IF
C     ............................................... TU,TF -> UT,FT
      IF(APR.EQ.3) CALL DAD_UT_ROT_INV(HRB,VRB)
      IF(PRO.GT.0.) THEN
C       ............................................. UT,(TU)
        SV=0
        DO K=1,4
          FT=VRB(K)-180.
          ST=SIND(-HRB(K))
          FT=FT/ST
          VRB(K)=FT+FIMID
          SV=SV+VRB(K)
        END DO
        FI=0.25*SV
        DF=ABS(0.5*(VRB(4)-VRB(1)+VRB(3)-VRB(2)))
      ELSE
        FI= 0.5*(VRB(1)+VRB(3))
        IF(ISTODS(6,NWINDZ,IWUSDO).EQ.0) FI=FI-180.+FIMID
        DF=ABS(VRB(3)-VRB(1))
      END IF
      FI=DFINXT(180.,FI)
      IF(DF.LT.PARADA(1,J_PDF).OR.DF.GT.PARADA(3,J_PDF)) RETURN
      TE=-0.5*(HRB(1)+HRB(3))
      IF(TE.LT.PARADA(1,J_PTE).OR.TE.GT.PARADA(3,J_PTE)) RETURN
      DT=ABS(HRB(2)-HRB(1))
      IF(DT.LT.PARADA(1,J_PDT).OR.DT.GT.PARADA(3,J_PDT)) RETURN
      PARADA(2,J_PFI)=FI
      PARADA(2,J_PTE)=TE
      PARADA(2,J_PDF)=DF
      PARADA(2,J_PDT)=DT
      FYES=.TRUE.
      END
*DK DADD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++  DADD
CH
      SUBROUTINE DADD(FNINT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      LOGICAL FNINT
      IF(FPIMDP.AND.MDLPDP.NE.7000.AND.MDLPDP.NE.7001) RETURN
C     ...................... NPR: 2=UT,  3=TU, 0=RO, 1=AC, 4=DL
C     ......................     -2=FT, -3=TF
      CALL DPARGI(39,'APR',NPR)
      IPR=ABS(NPR)
      MDLRDP=7000
      CALL DV_QVEC_IN(NFSTDA,NLSTDA)
      IF(     IPR.EQ.0) THEN
        CALL DQWIL(0.)
        CALL DAD_RO
      ELSE IF(IPR.EQ.1) THEN
        CALL DQWIL(0.)
C       Next statement is needed for D_ROT_AC_IN; see there.
        ISTODS(5,IAREDO,IWUSDO)=IPICDO
        CALL DAD_AC(FNINT)
      ELSE IF(IPR.LE.3) THEN
        CALL DAD_UT_D
      ELSE
        CALL DQWIL(0.)
        MDLRDP=7001
        CALL DPARGI(39,'AAC',LIST)
        IF(LIST.EQ.1) THEN
          CALL DAD_US
        ELSE
          CALL DAD_LIST
        END IF
      END IF
      CALL DPCSAR
      CALL DQFR(IAREDO)
      END
*DK DAD_LIST
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DAD_LIST
CH
      SUBROUTINE DAD_LIST
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
      EQUIVALENCE (K,KPIKDP)
      DATA J1/2/,L28/28/,LEV1/1/
      CHARACTER *1 T1
      CHARACTER *3 TM
      DATA TM/'___'/
      CHARACTER *5 TNAM
      LOGICAL FTERM
      FTERM=.FALSE.
      TXTADW=' '
      CALL DAC_LINE0(TXTADW(2:80))
      CALL DPARGV(39,'ALI',4,ALST)
      CALL DPARGV(39,'AAC',4,CALC)
      NLIN=1
      IF(CALC.EQ.1.) NLIN=NLIN-NCALDA
      IF(ALST.GT.0) THEN
        CALL DPARGI(39,'ALI',LIST)
        IF(NLSTDA(LIST).GT.0) THEN
          NLIN=NLIN+NLSTDA(LIST)-NFSTDA(LIST)+1
        ELSE
          ALST=-1.
        END IF
      END IF
      CALL DQCL(IAREDO)
      IF(.NOT.FPIKDP) CALL DQFFWI(LEV1)
      CALL DAC_DRAW_IN(NLIN,LEND,.FALSE.)
      IF(ALST.GT.0.) THEN
        CALL DPARGI(98,'ACF',ILCH)
        CALL DV_QVEC_DEF_COL(LIST,ILCH,NCTRDC(1))
        I=0
        IF(TLSTDA(LIST).EQ.'V0') CALL DV_QVEC_V0_PV(IVRODV)
        DO K=NFSTDA(LIST),NLSTDA(LIST)
          CALL DV_QVEC_OBJECT(K,P,FI,TE,IC,RM,T1,NCDF,NCOL)
          IF(NCOL.EQ.0) THEN
            NCOL=IOCODA(LIST)
            IF(NCOL.EQ.0) NCOL=NCDF
          END IF
          I=I+1
          IF(NCOL.GE.0) THEN
            TXTADW=' '
            TNAM=TLSTDA(LIST)//TM
            IF(LIST.EQ.5) CALL DV_QVEC_EF_TYPE(K,TNAM)
            CALL DAC_LINE(TNAM,I,IC,P,FI,TE,RM,TXTADW(J1:80))
            IF(TLSTDA(LIST).EQ.'V0')
     &        CALL DV_QVEC_V0(K,NCTRDC(1),LEND,TXTADW)
            CALL DAC_DRAW(I,NCOL,FTERM)
            IF(FTERM) GO TO 99
          END IF
   10   END DO
      END IF
      IF(CALC.EQ.1.) THEN
        DO L=-1,NCALDA,-1
          KPIKDP=IALPDA(L)
          IF(FPIMDP.AND.KPIKDP.NE.NPIKDP) GO TO 20
          CALL DV_QVEC_OBJECT(IALPDA(L),P,FI,TE,IC,RM,T1,NCDF,NCOL)
          IF(NCOL.GE.0) THEN
            CALL DAC_LINE(TNAMDA(L)      ,0,IC,P,FI,TE,RM,TXTADW(J1:80))
            CALL DAC_DRAW(L,NCOL,FTERM)
            IF(FTERM) GO TO 99
          END IF
   20   END DO
      END IF
      RETURN
   99 IF(.NOT.FPIKDP) CALL DWRT('List not complete.#')
      END
*DK DAD_UT_D
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DAD_UT_D
CH
      SUBROUTINE DAD_UT_D
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
      EQUIVALENCE (K,KPIKDP)
      DIMENSION LCAR(2)
      CHARACTER *3 DT3
      CHARACTER *1 TC(-4:4)
      DATA TC/'=','=','=','-','o','x','*','*','*'/
      DATA POP,POM,PO0/4.,4.5,12./,QPI2/0.63662/
C     QPI2 = 2/PI
      DATA DHTX/-7./,DVTX/0./,LCAR/0,-1/
      DATA DCI/20./,DHSC/20./,DVSC/22./
      LOGICAL FU,FMIS,FZOM
      CALL DQCL(IAREDO)
C     ............................................................... IZOMDO=0!
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_ALI,J_AFM,J_ATE,J_AAL,J_ASK,J_ASL,J_APR,J_AWI,J_ASS,J_ASY'
      CALL DPARAM(39
     &  ,J_ALI,J_AFM,J_ATE,J_AAL,J_ASK,J_ASL,J_APR,J_AWI,J_ASS,J_ASY)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(4,J_ASK).EQ.1.) THEN
        SKEW=PARADA(2,J_ASK)
      ELSE
        SKEW=0.
      END IF
      APR=PARADA(2,J_APR)
      CALL DAC_EGG_D(APR,SKEW,FU,FZOM,FIZ)
      ST=1.
      LIST=PARADA(2,J_ALI)
      PMAX=0.
      DO N=NFSTDA(LIST),NLSTDA(LIST)
        CALL DV_QVEC_PTOT(N,P)
        PMAX=MAX(PMAX,P)
      END DO
      FMIS=.FALSE.
      IF(PMAX.NE.0.) THEN
        IF(PARADA(4,J_ASL).EQ.1.) THEN
          PMAX=PARADA(2,J_ASL)
        ELSE
          PARADA(2,J_ASL)=PMAX
        END IF
        CALL DPARGV(98,'ULS',2,ULS)
        QP=ULS/PMAX
        CALL DQCH0(QP,PARADA(2,J_AWI)+1.)
        IF(FMISDA) THEN
          CALL DPARGI(39,'ATM',IML)
          IF(IML.EQ.-1.) FMIS=.TRUE.
        END IF
      END IF
      FIMID=PARADA(2,J_AFM)
      IF(FIMID.GE.360.) FIMID=FIMID-360.
      CALL DPARGV(39,'AAC',4,CALC)
      IF(CALC.GT.0.AND.NCALDA.LT.0) THEN
C       ........................................... DRAW WORK
        CALL DPARGV(98,'URC',2,RC)
        CALL DPARGV(98,'UWF',2,DLINDD)
        CALL DPARGI(98,'UCA',LCAR(1))
        CALL DPARGI(98,'UCF',LCFR)
        CALL DPARGV(98,'UNP',2,POCIA)
        DO I=1,2
          DO N=-1,NCALDA,-1
            K=IALPDA(N)
            CALL DV_QVEC_VECTOR_POL(K,P,FI,TE,IC,NCDF,NCOL)
            IF(NCOL.GE.0) THEN
              IF(FZOM) THEN
                FT=DFINXT(FIZ,FI)
              ELSE
                FI=FI-FIMID
                IF(FI.LT.-180.) FI=FI+360.
                IF(FI.GE. 180.) FI=FI-360.
                IF(FU) ST=SIND(TE)
                FT=180.+FI*ST
              END IF
              IF(FMIS.AND.TNAMDA(N).EQ.'EF_MI') THEN
                CALL DGLEVL(NCOL)
                IF(FPIKDP) THEN
                  CALL DQCH_PI(-TE,FT,QP*P)
                ELSE
                  CALL DQCH  (-TE,FT,QP*P)
                END IF
              ELSE
                IF(IC.EQ.999) THEN
                  IF(I.EQ.2) NCOL=-1
                  CALL DQCIRC(-TE,FT,RC,NCOL,LCFR,POCIA)
                ELSE
                  RP=SQRT(P)*PARADA(2,J_ASS)
                  IF(PARADA(4,J_ASY).LT.0.) THEN
                    IF(     IC.LT.0) THEN
                      POCIP=POM
                    ELSE IF(IC.GT.0) THEN
                      POCIP=POP
                    ELSE
                      POCIP=PO0
                      RP=RP*QPI2
                    END IF
                  ELSE
                    POCIP=PARADA(2,J_ASY)
                  END IF
                  IF(NCOL.LT.7) THEN
                    IF(I.EQ.2) NCOL=-1
                    CALL DQCIRC(-TE,FT,RP,NCOL,LCFR,POCIP)
                  ELSE
                    CALL DQCIRC(-TE,FT,RP,LCAR(I),NCOL,POCIP)
                  END IF
                END IF
              END IF
            END IF
          END DO
        END DO
      END IF
C     .................................................... DRAW LIST
      CALL DPARGV(39,'ALI',4,ALST)
      IF(ALST.LT.0.) GO TO 99
      IF(NLSTDA(LIST).LE.0.OR.NLSTDA(LIST).LT.NFSTDA(LIST)) GO TO 99
      IF(PMAX.EQ.0.) GO TO 99
      CALL DQTXT0(DHTX,DVTX)
      IF(LIST.NE.0) THEN
        CALL DPARGI(98,'ACF',ILCH)
        CALL DV_QVEC_DEF_COL(LIST,ILCH,NCTRDC(1))
      END IF
      DO K=NFSTDA(LIST),NLSTDA(LIST)
        CALL DV_QVEC_VECTOR_POL(K,P,FI,TE,IC,NCDF,NCOL)
        IF(NCOL.EQ.0) THEN
          NCOL=IOCODA(LIST)
          IF(NCOL.EQ.0) NCOL=NCDF
        END IF
        IF(NCOL.GE.0) THEN
          CALL DGLEVL(NCOL)
          IF(FZOM) THEN
            FT=DFINXT(FIZ,FI)
          ELSE
            FI=FI-FIMID
            IF(FI.LT.-180.) FI=FI+360.
            IF(FI.GE. 180.) FI=FI-360.
            IF(FU) ST=SIND(TE)
            FT=180.+FI*ST
          END IF
          IF(FPIKDP) THEN
            CALL DQCH_PI(-TE,FT,QP*P)
          ELSE
            CALL DQCH  (-TE,FT,QP*P)
            CALL DQTXTS(-TE,FT,TC(IC),1)
          END IF
        END IF
      END DO
      IF(.NOT.FPIKDP.AND.FU.AND.PMAX.GT.0.) THEN
        CALL DPAR_SET_CO(98,'CCI')
        IF(ICOLDP.GE.0.) THEN
          RP=SQRT(PMAX)*PARADA(2,J_ASS)
          CALL DQINV(IAREDO,HMINDG(IAREDO)+DCI,
     &                      VMINDG(IAREDO)+2.*DCI,HCI,VCI)
          IF(CALC.GT.0.AND.NCALDA.LT.0)  THEN
            VCI=VCI+RP
            IF(ABS(APR).EQ.2.) THEN
              HCI=HCI+RP
            ELSE
              HCI=HCI-RP
            END IF
            CALL DQCIRC(HCI,VCI,RP,-1,ICOLDP,12.)
          END IF
          CALL DQCH(HCI,VCI,QP*PMAX)
          TXTADW=DT3(PMAX)//' GeV'
          CALL DGTEXT(HMINDG(IAREDO)+DHSC,VMINDG(IAREDO)+DVSC,TXTADW,7)
        END IF
      END IF
   99 CALL DQFR(IAREDO)
      END
*DK DAC_EGG_D
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DAC_EGG_D
CH
      SUBROUTINE DAC_EGG_D(PROJ,SKEW,FU,FZOM,FZ)
CH 
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    : SKEW, PROJ: 2=UT, 3=TU, -2=FT, -3=TF
C    Outputs   : no
C                       setup area and draw egg or TF
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DATA DFR/5./,DTR/5./
      DIMENSION HRB(5),VRB(5)
      DIMENSION HVA(62,2),HVAR(2)
      DIMENSION HVL(2,2),TVL(5)
      DATA QSK/0.005/,DA1/6./
      DIMENSION HSC(3),VSC(3)
      DATA DHT1/3./,DVT1/16./,DHT2/60./,DVT2/6./
      DIMENSION NGR(2)
      DATA NGR/2,13/
      CHARACTER *14 TU
      CHARACTER *3 DT3,TJ
      DATA TJ/'-&j'/
      LOGICAL FU,FIN,FZOM
C     ...................... 2=UT, 3=TU, -2=FT, -3=TF
      CALL DPARGV(39,'AFM',2,AFM)
      FZOM=.FALSE.
      IF(PROJ.GT.0..AND.IZOMDO.EQ.0) THEN
C       ........................................................... U/T
        CALL DQWIL(0.)
        CALL DQRER(0,-180.-DTR,-DFR,DTR,360.+DFR,HRB,VRB)
        FU=.TRUE.
      ELSE
C       ........................................................... F/T
        CALL DQWIL(MOD(DFWIDU(IZOMDO),10.))
        FU=.FALSE.
        ST=1.
        A0=0.
        A360=360.
        IF(IZOMDO.EQ.0) THEN
          CALL DQRER(0,-180.-DTR,-DFR,DTR,360.+DFR,HRB,VRB)
        ELSE
          CALL DPARGV(10,'PFI',2,FZ)
          CALL DADRCB(FZ,HRB,VRB)
          AF1=VRB(1)
          AF2=VRB(3)
          AT1=HRB(1)
          AT2=HRB(3)
          FZOM=.TRUE.
          A0=AFM
          A360=A0+360.
  321     IF(VRB(1).LT.A0  ) THEN
            A0  =A0  -180.
            GO TO 321
          END IF
  322     IF(VRB(2).GT.A360) THEN
            A360=A360+180.
            GO TO 322
          END IF
        END IF
      END IF
C     ......................................................... T/U
      IF(ABS(PROJ).NE.2.) CALL DAD_UT_ROT(HRB,VRB)
      DH=QSK*SKEW*(VRB(1)-VRB(3))
C     ........................................................... SKEW
      HRB(1)=HRB(1)-DH
      HRB(2)=HRB(2)-DH
      HRB(3)=HRB(3)+DH
      HRB(4)=HRB(4)+DH
      CALL DQRU(HRB,VRB)
      IF(FPIKDP) RETURN
      CALL DPAR_SET_CO(98,'CEA')
      CALL DPARGI(98,'UNT',NVL)
      IF(NVL.GT.0) CALL DPAR_GET_REAL(98,'UT1',5,TVL)
      CALL DPARGV(68,'SDW',2,DLINDD)
      CALL DPARGV(98,'UDF',2,DFL)
      IF(FU) THEN
C       .................................................................. EGG
        N=0
        DO TE=0.,180.,DA1
          ST=SIND(TE)
          HVAR(2)=180.-180.*ST
          HVAR(1)=-TE
          N=N+1
          CALL DQPOC(HVAR(1),HVAR(2),HVA(N,1),HVA(N,2),FIN)
        END DO
        DO TE=180.,0.,-DA1
          ST=SIND(TE)
          HVAR(2)=180.+180.*ST
          HVAR(1)=-TE
          N=N+1
          CALL DQPOC(HVAR(1),HVAR(2),HVA(N,1),HVA(N,2),FIN)
        END DO
        CALL DGAREA(N,HVA(1,1),HVA(1,2))
        CALL DPAR_SET_CO(98,'CEL')
        DO FI=-180.,180.,DFL
          HVL(1,2)=180.
          HVL(1,1)=0.
          DO TE=DA1,180.,DA1
            ST=SIND(TE)
            HVL(2,2)=180.+FI*ST
            HVL(2,1)=-TE
            CALL DQLIE(HVL(1,1),HVL(1,2))
            HVL(1,2)=HVL(2,2)
            HVL(1,1)=HVL(2,1)
          END DO
        END DO
        DO K=1,NVL
          ST=SIND(TVL(K))
          HVL(1,2)=180.-180.*ST
          HVL(2,2)=180.+180.*ST
          HVL(1,1)=-TVL(K)
          HVL(2,1)=-TVL(K)
          CALL DQLIE(HVL(1,1),HVL(1,2))
        END DO
        CALL DPAR_SET_CO(98,'CSC')
        IF(ICOLDP.GE.0.) THEN
C          DO K=1,3
C            HSC2(K)=HMINDG(IAREDO)+HSC1(K)
C            VSC2(K)=VMINDG(IAREDO)+VSC1(K)
C          END DO
C          DLINDD=DSC
C          CALL DGDRAW(3,HSC2,VSC2)
          HSC(1)=HMINDG(IAREDO)+DHT1
          VSC(1)=VHGHDG(IAREDO)-DVT1
          HSC(2)=HMINDG(IAREDO)+DHT1
          VSC(2)=VMINDG(IAREDO)+DVT2
          HSC(3)=HHGHDG(IAREDO)-DHT2
          VSC(3)=VMINDG(IAREDO)+DVT2
          TU='(f-'//DT3(AFM)//')*SIN(j)'
          IF(PROJ.EQ.2.) THEN
C           ................................................. UT
            CALL DGTXTG(HSC(1)     ,VSC(1),TU,14,NGR,2)
            CALL DGTXTG(HSC(2)     ,VSC(2),'j=180',5,1,1)
            CALL DGTXTG(HSC(3)     ,VSC(3),'j=0',3,1,1)
          ELSE
            CALL DGTXTG(HSC(1)     ,VSC(1),'j=180',5,1,1)
            CALL DGTXTG(HSC(2)     ,VSC(2),'j=0',3,1,1)
            CALL DGTXTG(HSC(3)-DHT2,VSC(3),TU,14,NGR,2)
          END IF
        END IF
      ELSE
        ICAR=ICOLDP
        CALL DQRER(0,-180.,A0,0.,A360,HRB,VRB)
        CALL DPAR_SET_CO(98,'CEL')
        CALL DQPO0('AR+L',ICAR,ICOLDP,' ')
        CALL DQPOL(4,HRB,VRB)
        DO FI=A0,A360,DFL
          CALL DQL2E(-180.,FI,0.,FI)
        END DO
        DO K=1,NVL
          CALL DQL2E(-TVL(K),A0,-TVL(K),A360)
        END DO
        CALL DQL2E(-180.,A0,-180.,A360)
        CALL DQL2E(   0.,A0,   0.,A360)
        IF(IZOMDO.EQ.0) THEN
          AF1=AFM-180.-DFR
          AF2=AFM+180.+DFR
          AT1=-180.-DTR
          AT2=DTR
        END IF
        IF(ABS(PROJ).EQ.2.) THEN
          CALL DQSCA('V',AF1,AF2,' deg',4, '&f',2)
          CALL DQSCA('H',AT1-DH,AT2+DH,' deg',4,TJ,3)
        ELSE
          CALL DQSCA('H',AF1-DH,AF2+DH,' deg',4, '&f',2)
          CALL DQSCA('V',-AT2,-AT1,' deg',4,'&j',2)
C         CALL DQSCA('V',AT1,AT2,' deg',4,TJ,3)
        END IF
      END IF
      END
*DK DAD_AC
CH..............+++         AI
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++ DAD_AC
CH
      SUBROUTINE DAD_AC(FNINT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   21-MAR-1995
C
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
C
      DIMENSION HRB(4),VRB(4)
      DATA Q1/0.0/,Q2/0.000000005/,DS/20./,WLONG/0.04/,SFAC/.97/
      DIMENSION IP(2)
      DATA IP/2,9/,NDASH/2/
C
      DIMENSION XYZSQ(3,8)
C     ............................................ cube in system C1
      DATA XYZSQ/
     &   1., 1.,-1.,    ! 1
     &   1.,-1.,-1.,    ! 2
     &  -1.,-1.,-1.,    ! 3
     &  -1., 1.,-1.,    ! 4
     &   1., 1., 1.,    ! 5
     &   1.,-1., 1.,    ! 6
     &  -1.,-1., 1.,    ! 7
     &  -1., 1., 1./    ! 8
C
      DIMENSION HUWSQ(3,8),HSQ(8),VSQ(8),DSQ(8),NSQL(12,2)
      DIMENSION LSQ1(3,8),LSQ2(9,8),NSQF(4,6),NFAC(3,8)
      DIMENSION LICO(12),LSCO(6)
C     .................................................. 12 lines of the cube
C                1  2  3  4  5  6  7  8  9 10 11 12
      DATA NSQL/ 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4,
     &           2, 3, 4, 1, 6, 7, 8, 5, 5, 6, 7, 8/
C     .................................thin lines containing corner #
      DATA LSQ1/1,4, 9,  ! 1
     &          1,2,10,  ! 2
     &          2,3,11,  ! 3
     &          3,4,12,  ! 4
     &          5,8, 9,  ! 5
     &          5,6,10,  ! 6
     &          6,7,11,  ! 7
     &          7,8,12/  ! 8
C     ................................thick lines not containing corner #
      DATA LSQ2/2,3,5,6,7,8,10,11,12,  ! 1
     &          3,4,5,6,7,8, 9,11,12,  ! 2
     &          1,4,5,6,7,8, 9,10,12,  ! 3
     &          1,2,5,6,7,8, 9,10,11,  ! 4
     &          1,2,3,4,6,7,10,11,12,  ! 5
     &          1,2,3,4,7,8, 9,11,12,  ! 6
     &          1,2,3,4,5,8, 9,10,12,  ! 7
     &          1,2,3,4,5,6, 9,10,11/  ! 8
C     .................................... corners of the 6 Faces of the cube
      DATA NSQF/1,2,3,4,   ! 1 BLACK=1
     &          5,6,7,8,   ! 2 BLACK=1
     &          1,2,6,5,   ! 3 RED=5
     &          2,3,7,6,   ! 4 BLUE=6
     &          3,4,8,7,   ! 5 RED=5
     &          4,1,5,8/   ! 6 BLUE=6
C     .......................................... face containing corner # 
      DATA NFAC/1,3,6,   ! 1
     &          1,3,4,   ! 2
     &          1,4,5,   ! 3
     &          1,5,6,   ! 4
     &          2,3,6,   ! 5
     &          2,3,4,   ! 6
     &          2,4,5,   ! 7
     &          2,5,6/   ! 8
C     ............................................ colors of lines and faces
      DATA LSCO/1,1,5,6,5,6/
      DATA LICO/9,9,9,9,10,10,10,10,13,14,12,8/
C     ............................................. line width
      DATA DL1/1./,DL2/3./,DLL/3./,DLC/3./,JDEB/0/
C
      PARAMETER (NLIN=198,NLIN2=99,MVX=9)
      DIMENSION X1(NLIN),Y1(NLIN),Z1(NLIN),NCOL(NLIN),NALP(NLIN2)
      DIMENSION XYZL(3,2),HVDL(6,2,NLIN),R3D(NLIN)
C     ............................................ face of nside #
      DIMENSION NSID(2,NLIN),NFASI(3)
      DATA NFASI/3,4,1/
      DIMENSION NSCOL(3)
      DATA NSCOL/9,10,8/,QCR/0.04/
      DATA IDEB/0/,SRG/1.05/
C
      DIMENSION H(4),V(4),HCR(2),VCR(2)
C
      LOGICAL FNO,FBUT(4),DGPNTR,FIN(NLIN),FNINT
C
      TPARDA=
     &  'J_AAL,J_ATO'
      CALL DPARAM(39
     &  ,J_AAL,J_ATO)
C
      TPARDA=
     &  'J_RBA'
      CALL DPARAM(86
     &  ,J_RBA)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
C     ............................................. setup tracks
      CALL DAD_STR(NUM,X1,Y1,Z1,NCOL,PMAX,DSTR,NALP)
      NFR1=NUM+1
      CALL DQCL(IAREDO)
      IF(PARADA(4,J_ATO).EQ.1.) THEN
        PMAX=PARADA(2,J_ATO)/(1.+DSTR*PARADA(2,J_ATO))
      ELSE
        PARADA(2,J_ATO)=PMAX/(1.-DSTR*PMAX)
      END IF
      IF(PMAX.LE.0.) RETURN
      RTO=PMAX*SRG
      CALL DQRER(0,-RTO,-RTO,RTO,RTO,HRB,VRB)
      CALL DQRU(HRB,VRB)
      IF(.NOT.FPIKDP)
     &  CALL DWRT('Rotation of data and artificila cube. Move mouse.')
      CALL DPARSV(86,'RBA',4,1.)
      CALL DPARGI(86,'RCB',L7)
      CALL DPARGI(86,'RCS',LSCO(1))
      CALL DPARGI(86,'RCT',LSCO(3))
      CALL DPARGI(86,'RCF',LSCO(4))
      LSCO(2)=LSCO(1)
      LSCO(5)=LSCO(3)
      LSCO(6)=LSCO(4)
      IF(LSCO(1).LT.16) CALL DCDST0
      CALL DPARGV(86,'RFS',4,CHG)
      IF(CHG.EQ.1.) THEN
        LS=LSCO(1)
        CALL DPARGV(86,'RFS',2,GRCODD(LS))
        RDCODD(LS)=0.
        BLCODD(LS)=0.
      END IF
      CALL DPARGV(86,'RFT',4,CHG)
      IF(CHG.EQ.1.) THEN
        LS=LSCO(3)
        CALL DPARGV(86,'RFT',2,RDCODD(LS))
        GRCODD(LS)=0.
        BLCODD(LS)=0.
      END IF
      CALL DPARGV(86,'RFF',4,CHG)
      IF(CHG.EQ.1.) THEN
        LS=LSCO(4)
        CALL DPARGV(86,'RFF',2,BLCODD(LS))
        RDCODD(LS)=0.
        GRCODD(LS)=0.
      END IF
      CALL DW_SET_CO
      CALL DPARGI(86,'RLL',LICO( 1))
      CALL DPARGI(86,'RRL',LICO( 5))
      CALL DPARGI(86,'RL1',LICO( 9))
      CALL DPARGI(86,'RL2',LICO(10))
      CALL DPARGI(86,'RL3',LICO(11))
      CALL DPARGI(86,'RL4',LICO(12))
      CALL UFILL(LICO,2,4,LICO(1))
      CALL UFILL(LICO,6,8,LICO(5))
      CALL DPARGV(86,'RSC',2,QCR)
      CALL DPARGV(86,'RQC',4,RQ4)
      IF(RQ4.EQ.1.) THEN
        CALL DPARGV(86,'RQC',2,QDI)
      ELSE
        QDI=0.
      END IF
      CALL DPARGV(86,'RD1',2,DL1)
      CALL DPARGV(86,'RD2',2,DL2)
      CALL DPARGV(86,'RDC',2,DLC)
      CALL DPARGV(86,'RDT',2,DLL)
      CALL DPARGV(86,'RET',2,DLE)
      IP(1)=DLE
      RDASH=0.
      CALL DPARGV(86,'RLE',0,RDASH)
      CALL DPARGI(86,'RXS',NSCOL(1))
      CALL DPARGI(86,'RXT',NSCOL(2))
      CALL DPARGI(86,'RXF',NSCOL(3))
      H1=HLOWDG(IAREDO)
      V1=VLOWDG(IAREDO)
      H2=HHGHDG(IAREDO)
      V2=VHGHDG(IAREDO)
      HM=0.5*(H2+H1)
      VM=0.5*(V2+V1)
      DH=0.5*(H2-H1)
      DV=0.5*(V2-V1)
      VHS=VM+DS
      VLS=VM-DS
      DLIN=DLINDD
      AL0=PARADA(2,J_AAL)
      IF(.NOT.FPIKDP) CALL DGSCUR(HM,VM)
      AL=0.
C     ........................................... SETUP ANGLES      
   33 CALL DAD_AC_IN(0,0.,FNO,BETA)
      IF(FNO) GO TO 99
C     K=1,8 cube corners / J=1,3 xyz
C     L=1,NUM # of lines / I=1,2 points of line
C     ....................... get rotated cube (rotation 2, system C1 -> CR)
      DO K=1,8
        CALL D_ROT_VECTOR(2,XYZSQ(1,K),HUWSQ(1,K))
      END DO
C     ........................................... calculate cube size
      RMAX=0.01
      DO K=1,8
        RMAX=MAX(RMAX,HUWSQ(2,K)**2+HUWSQ(3,K)**2)
      END DO
      HMAX=ABS(SQRT(2.)*COSD(45-BETA))
      RMAX=SQRT(RMAX)
      SC=SFAC*MIN(DH/HMAX,DV/RMAX)
      CALL DQ_CLIP_3D_IN(SC)
      DO K=1,8
        DO J=1,3
          HUWSQ(J,K)=HUWSQ(J,K)*SC
        END DO
      END DO
      XYZL(1,1)=0.
      XYZL(2,1)=0.
      XYZL(3,1)=0.
      DO L=1,NUM
        XYZL(1,2)=X1(L)
        XYZL(2,2)=Y1(L)
        XYZL(3,2)=Z1(L)
        CALL DAD_LINE_AC(XYZL,HVDL(1,1,L),NSID(1,L),FIN(L))
        R3D(L)=SQRT(
     &    HVDL(1,2,L)*HVDL(1,2,L)+
     &    HVDL(2,2,L)*HVDL(2,2,L)+
     &    HVDL(3,2,L)*HVDL(3,2,L))
      END DO
C     
      SB=SIND(BETA)
      CB=COSD(BETA)
C     ............................................. start of loop
    1 CALL DGLEVL(L7)
      IF(.NOT.FPIKDP) CALL DQFAR(H1,V1,H2,V2)
      SA=SIND(AL)
      CA=COSD(AL)
C     ............................................. rotate cube      
      DO K=1,8
        HSQ(K)=HUWSQ(1,K)
        VSQ(K)= CA*HUWSQ(2,K)+SA*HUWSQ(3,K)
        DSQ(K)=-SA*HUWSQ(2,K)+CA*HUWSQ(3,K)
      END DO
      DO K=1,8
        HSQK  = CB*HSQ(K)+SB*DSQ(K)
        DSQ(K)=-SB*HSQ(K)+CB*DSQ(K)
        HSQ(K)=HSQK
      END DO
C     .............................................. deep corner of cube
      DMIN=9999.
      DO K=1,8
        IF(DSQ(K).LT.DMIN) THEN
          DMIN=DSQ(K)
          KMIN=K
        END IF
      END DO
C     ............................................... draw faces
      DO N=1,3
        MN=NFAC(N,KMIN)
        DO I=1,4
          MI=NSQF(I,MN)
          H(I)=HSQ(MI)+HM
          V(I)=VSQ(MI)+VM
        END DO
        CALL DGLEVL(LSCO(MN))
        IF(.NOT.FPIKDP) CALL DGAREA(4,H,V)
      END DO
C     ................................................ draw thin lines
      DLINDD=DL1
      DO N=1,3
        MN=LSQ1(N,KMIN)
        DO I=1,2
          II=NSQL(MN,I)
          H(I)=HSQ(II)+HM
          V(I)=VSQ(II)+VM
        END DO
        CALL DGLEVL(LICO(MN))
        IF(.NOT.FPIKDP) CALL DGDRAW(2,H,V)
      END DO
C     ................................................ draw thick lines
      DLINDD=DL2
      DO N=1,9
        MN=LSQ2(N,KMIN)
        DO I=1,2
          II=NSQL(MN,I)
          H(I)=HSQ(II)+HM
          V(I)=VSQ(II)+VM
        END DO
        CALL DGLEVL(LICO(MN))
        IF(.NOT.FPIKDP) CALL DGDRAW(2,H,V)
      END DO
C     ................................................ draw tracks
      NUM2=NUM/2
C      NUMH=1+NUM2
      DO L=1,NUM
        IF(FIN(L).AND.NCOL(L).GE.0) THEN
          DO I=1,2
            H2D= HVDL(1,I,L)
            V2D= CA*HVDL(2,I,L)+SA*HVDL(3,I,L)
            DI  =-SA*HVDL(2,I,L)+CA*HVDL(3,I,L)
            H2D= CB*H2D+SB*DI
            H(I)=H2D+HM
            V(I)=V2D+VM
            IF(L.LE.NUM2.AND.NSID(I,L).NE.0) THEN
              IF(FPIKDP) THEN
                IF(FPIMDP.AND.NALP(L).NE.NPIKDP) GO TO 10
                D=ABS(H(I)-HPIKDP)+ABS(V(I)-VPIKDP)
                IF(D.LE.DPIKDP) THEN
                  DPIKDP=D
                  NPIKDP=NALP(L)
                  MDLPDP=MDLRDP
                  HHPKDP=H(I)
                  VVPKDP=V(I)
                END IF
                GO TO 10
              END IF
              IF(JDEB.EQ.0) THEN
                CALL DGLEVL(NCOL(L+NUM2))
              ELSE
                CALL DGLEVL(NSCOL(NSID(I,L)))
              END IF
              DLINDD=DLC
              NF=NFASI(NSID(I,L))
              N0=NSQF(2,NF)
              N1=NSQF(1,NF)
              DO IC=1,2
C               ............................................ draw crosses
                DCR=QCR*(1.+QDI*DI/SC)
                DCH=DCR*(HSQ(N1)-HSQ(N0))
                DCV=DCR*(VSQ(N1)-VSQ(N0))
                HCR(1)=H(I)-DCH
                HCR(2)=H(I)+DCH
                VCR(1)=V(I)-DCV
                VCR(2)=V(I)+DCV
                CALL DGDRAW(2,HCR,VCR)
                N1=NSQF(3,NF)
              END DO
            END IF
          END DO
          IF(FPIKDP) GO TO 10
          IF(L.LE.NUM2) THEN
            DLINDD=DLE
          ELSE
            DLINDD=DLL
          END IF
          CALL DGLEVL(NCOL(L))
          H(3)=H(1)
          V(3)=V(1)
          IF(RDASH.GT.1.AND.L.LE.NUM2) THEN
            R2D=SQRT(H2D*H2D+V2D*V2D)
            IF(R2D.LT.RDASH) GO TO 10  
            IP(2)=RDASH*R2D/R3D(L)-IP(1)
            IP(2)=MAX(IP(2),1)
            CALL DGDASH(NDASH,IP)
            CALL DGDRAW(2,H(2),V(2))
            CALL DGDASH(0,0)
          ELSE
            CALL DGDRAW(2,H(2),V(2))
          END IF
        END IF
   10 END DO
      IF(FPIKDP) GO TO 99
      CALL DGCHKX
      IF(FNINT) RETURN
   20 IF(IDEB.EQ.1) CALL DWAIT1(WLONG)
      IF(DGPNTR(JHC,JVC,FBUT)) THEN
        IF(FBUT(1)) THEN
          GO TO 20
        ELSE IF(FBUT(3)) THEN
          CALL DTYANS(
     &      'blank=Continue, anything else = stop',
     &      ' C',NANSW)
C            1234567
          IF(NANSW.LE.0) GO TO 99
        END IF
        HC=JHC
        VC=JVC
        IF(HC.NE.HCOLD.OR.VC.NE.VCOLD) THEN
          HCOLD=HC
          VCOLD=VC
        END IF
        IF(VC.GT.VHS) THEN
          D=(VC-VHS)*(H2-HC)
          IF(FBUT(2)) THEN
            DA=-Q1*D-Q2*D*D
          ELSE
            DA= Q1*D+Q2*D*D
          END IF
        ELSE IF(VC.LT.VLS) THEN
          D=(VLS-VC)*(H2-HC)
          IF(FBUT(2)) THEN
            DA= Q1*D+Q2*D*D
          ELSE
            DA=-Q1*D-Q2*D*D
          END IF
        ELSE
          GO TO 20
        END IF
C       ........................................ rotate cube + tracks
        AL=AL+DA
        IF(AL.GE.360.) THEN
          AL=AL-360.
        ELSE IF(AL.LT.0.) THEN
          AL=AL+360.
        END IF
        AG=AL0+AL
        IF(AG.GE.360.) THEN
          AG=AG-360.
        ELSE IF(AG.LT.0.) THEN
          AG=AG+360.
        END IF
        WRITE(TXTADW,1001) AG
 1001   FORMAT('General rotation: AL =',F7.2,'m')
        CALL DWR_OVER_PRINT(30)
        GO TO 1
      END IF
   99 DLINDD=DLIN
      IF(LSCO(1).LT.16) CALL DCDSTC
      CALL DQFR(IAREDO)
      PARADA(2,J_AAL)=AG
      END
*DK DAD_STR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DAD_STR
CH
      SUBROUTINE DAD_STR(N2,X,Y,Z,NCOL,PMAX,DS,NALP)
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
      DIMENSION X(*),Y(*),Z(*),NCOL(*),NALP(*)
      DATA QFAR/100000./
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_ALI,J_ADS'
      CALL DPARAM(39
     &  ,J_ALI,J_ADS)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      LIST=PARADA(2,J_ALI)
      IF(PARADA(4,J_ADS).EQ.1.) THEN
        DS=PARADA(2,J_ADS)-1.
        IF(DS.GE.0.) THEN
          IDS=2
        ELSE
          IDS=3
        END IF
      ELSE
        DS=0.
        IDS=1
      END IF
      COLE=0.
      CALL DPARGV(86,'RCE',0,COLE)
      LCOLE=COLE
      N2=0
      IF(NLSTDA(LIST).LE.0.OR.NLSTDA(LIST).LT.NFSTDA(LIST)) RETURN
      IF(LIST.NE.0) THEN
        CALL DPARGI(98,'ACF',ILCH)
        CALL DV_QVEC_DEF_COL(LIST,ILCH,NCTRDC(1))
      END IF
      QN=1.
      QF=QFAR
      PMAX=0
      N1=0
      N2=NLSTDA(LIST)-NFSTDA(LIST)+1
C     ..................... For FPIMDP total loop must be done to get PMAX 
      DO K=NFSTDA(LIST),NLSTDA(LIST)
        CALL DV_QVEC_VECTOR(K,PX,PY,PZ,PP,IC,NCDF,NCOL2)
        IF(NCOL2.EQ.0) THEN
          NCOL2=IOCODA(LIST)
          IF(NCOL2.EQ.0) NCOL2=NCDF
        END IF
        N1=N1+1
        N2=N2+1
        NALP(N1)=K
        NCOL(N2)=NCOL2
        IF(NCOL(N2).EQ.0) THEN
          NCOL(N2)=IOCODA(LIST)
          IF(NCOL(N2).EQ.0) NCOL(N2)=NCDF
        END IF
        IF(LCOLE.EQ.0.OR.NCOL(N2).LT.0) THEN
          NCOL(N1)=NCOL(N2)
        ELSE
          NCOL(N1)=LCOLE
        END IF
        IF(NCOL(N2).LT.0) THEN
          NCOL(N1)=NCOL(N2)
        ELSE
          IF(     IDS.EQ.2) THEN
            QN=1./(1.+DS*PP)
            PP=PP*QN
            QF=QFAR*QN
          END IF
          PMAX=MAX(PMAX,PP)
          X(N2)=QN*PX
          Y(N2)=QN*PY
          Z(N2)=QN*PZ
          X(N1)=QF*PX
          Y(N1)=QF*PY
          Z(N1)=QF*PZ
        END IF
      END DO
      END
*DK DAD_LINE_AC
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++ DAD_LINE_AC
CH
      SUBROUTINE DAD_LINE_AC(XYZ,HVD,NSID,FIN)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   21-MAR-1995
C
C    line is rotated into rot system (axis = track or jet etc) and clipped
C    inside cube 
c
C    Inputs    : XYZ(3,2) = coordinates of input line
C    Outputs   : HVD(3,2) = coordinates of output line inside artificail cube 
C                FIN = TRUE if line inside
C
C ---------------------------------------------------------------------
C     1=DROD system 1=cube system
C
      INCLUDE 'DALI_CF.INC'
      DIMENSION XYZ(3,2),HVD(6,2),PU(3),PD(3),U(3,2),W(3,2),S(3,2)
      DIMENSION NSID(2)
      LOGICAL FIN
C     ..............................................................
C     DIMENSION VXYZ(3),RXYZ(3)
      LOGICAL FNO
      DATA A90/90./,A0/0./,M/2/
      DO I=1,2
C       .............................. rotate FI,TE axis to H-axis        
        CALL D_ROT_VECTOR(1,XYZ(1,I),PU)
C       ................. transform line into 3D display system around 0,0,0
        PD(1)=AHSCDQ*(PU(1))
        PD(2)=BVSCDQ*(PU(2))
        PD(3)=BVSCDQ*(PU(3))
C       .............................. rotate into cube system CR -> C1        
        CALL D_ROT_VECTOR_INVERS(2,PD,U(1,I))
      END DO
C     ................................ CLIP line into cube
      CALL DQ_CLIP_3D(U,W,NSID,FIN)

      IF(.NOT.FIN) RETURN
C     ................................ rotate back
      DO I=1,2
C       ..................... rotate into rotated cube system C1 -> CR        
        CALL D_ROT_VECTOR(2,W(1,I),HVD(1,I))
        IF(NSHAD.EQ.1) THEN
          S(1,I)=W(1,I)
          S(2,I)=W(2,I)
          S(3,I)=W(3,I)
          S(M,I)=-SC
          CALL D_ROT_VECTOR(2,S(1,I),HVD(4,I))
        END IF
      END DO
      RETURN
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------  D_ROT_AC_IN
CH
      ENTRY DAD_AC_IN(ISHAD,SCI,FNO,BETA)
CH
CH --------------------------------------------------------------------
CH
C  initialize artificial cube
C ---------------------------------------------------------------------
C    INPUT:      SC = half side of cube in display coordinates
C
C    Outputs   : FNO = true  rotation cannot be done.
C                Beta = angle between rotation axis and screen
C ---------------------------------------------------------------------
      FNO=.TRUE.
      IF(ISTODS(5,IAREDO,IWUSDO).NE.IPICDO) THEN
        CALL DWRT('No RO picture an the selected window ?"')
        RETURN
      END IF
      SC=SCI
      NSHAD=ISHAD
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_AFM,J_ATE,J_AAL'
      CALL DPARAM(39
     &  ,J_AFM,J_ATE,J_AAL)
      TPARDA=
     &  'J_RFA,J_RTA,J_RGA,J_RBA'
      CALL DPARAM(86
     &  ,J_RFA,J_RTA,J_RGA,J_RBA)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      FNO=.FALSE.
      CALL D_ROT_VECTOR_IN(1,
     &  PARADA(2,J_AFM),
     &  PARADA(2,J_ATE),
     &  PARADA(2,J_AAL))
      IF(PARADA(4,J_RBA).LE.0.) THEN
        FA=PARADA(2,J_RFA)
        TA=PARADA(2,J_RTA)
      ELSE
        FA=A0
        TA=A90
        BETA=PARADA(2,J_RBA)
      END IF
      AA=PARADA(2,J_RGA)
      CALL D_ROT_VECTOR_IN(2,FA,TA,AA)
      CALL DQSET(IAREDO,0.,0.)
      END
*DK DAD_RO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++  DAD_RO
CH
      SUBROUTINE DAD_RO
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      DIMENSION HRB(4),VRB(4)
      DIMENSION XYZ(99,3),P(99),NCOL(99),NALP(99)
      DIMENSION SCAL(14),SLOG(14),ICLG(14)
      DATA SCAL/.2,.4,.6,.8,1.,2.,4.,6.,8.,10.,20.,40.,60.,80./
      DATA ICLG/ 0, 0, 0, 0,8, 0, 0, 0, 0,  8 , 0,  0,  0,  0 /
      DATA L1/1/,LP/1/,L10/8/,SZ/2./,DSTP/1./,DP/0.1/,SRG/1.05/
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_ALI,J_ALG,J_AFM,J_ATE,J_AAL,J_AFR,J_ATO,J_AAS,J_ADS'
      CALL DPARAM(39
     &  ,J_ALI,J_ALG,J_AFM,J_ATE,J_AAL,J_AFR,J_ATO,J_AAS,J_ADS)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      LIST=PARADA(2,J_ALI)
      IF(.NOT.FPIKDP) THEN
        CALL DQCL(IAREDO)
        CALL DQFFWI(L1)
      END IF
      IF(PARADA(4,J_ADS).EQ.1.) THEN
        DS=PARADA(2,J_ADS)-1.
        IF(DS.GE.0.) THEN
          IDS=2
          DO K=1,14
            SLOG(K)=SCAL(K)/(1.+DS*SCAL(K))
          END DO
        ELSE
          IDS=3
          DO K=1,14
            SLOG(K)=1.+LOG10(SCAL(K)+DP)
          END DO
        END IF
      ELSE
        IDS=1
      END IF
      IF(NLSTDA(LIST).LE.0.OR.NLSTDA(LIST).LT.NFSTDA(LIST)) GO TO 99
      IF(LIST.NE.0) THEN
        CALL DPARGI(98,'ACF',ILCH)
        CALL DV_QVEC_DEF_COL(LIST,ILCH,NCTRDC(1))
      END IF
      PMAX=0
      NAL=NFSTDA(LIST)
      NUM=NLSTDA(LIST)-NAL+1
      DO L=1,NUM
        CALL DV_QVEC_VECTOR(NAL,PX,PY,PZ,P(L),IC,NCDF,NCOL(L))
        IF(FPIMDP.AND.NPIKDP.EQ.NAL) LPICK=L
        NALP(L)=NAL
        NAL=NAL+1
        IF(NCOL(L).EQ.0) THEN
          NCOL(L)=IOCODA(LIST)
          IF(NCOL(L).EQ.0) NCOL(L)=NCDF
        END IF
        IF(NCOL(L).GE.0) THEN
          IF(P(L).GT.0.) THEN
            Q=1./P(L)
            XYZ(L,1)=Q*PX
            XYZ(L,2)=Q*PY
            XYZ(L,3)=Q*PZ
            PMAX=MAX(PMAX,P(L))
          ELSE
            NCOL(L)=-1
          END IF
        END IF
      END DO
      IF(PMAX.LE.0.) GO TO 99
      IF(PARADA(4,J_ATO).EQ.1.) THEN
        PMAX=PARADA(2,J_ATO)
      ELSE
        PARADA(2,J_ATO)=PMAX
      END IF
      IF(     IDS.EQ.1) THEN
        HR=PMAX
      ELSE IF(IDS.EQ.2) THEN
        HR=PMAX/(1.+DS*PMAX)
      ELSE
        HR=MAX(0.,1.+LOG10(PMAX+DP))
      END IF
      HR=HR*SRG
      IF(PARADA(4,J_AAS).EQ.1.) THEN
        VR=HR/PARADA(2,J_AAS)
      ELSE
        VR=HR
      END IF
      IF(IZOMDO.EQ.0) THEN
        CALL DQRER(0,-HR,-VR,HR,VR,HRB,VRB)
      ELSE
        VR=0.5*VR
        CALL DQRER(0,0.,-VR,HR,VR,HRB,VRB)
      END IF
      CALL DQRU(HRB,VRB)
      CALL DPARGV(87,'STR',2,DLINDD)
      SF=SIND(PARADA(2,J_AFM))
      CF=COSD(PARADA(2,J_AFM))
      ST=SIND(90.-PARADA(2,J_ATE))
      CT=COSD(90.-PARADA(2,J_ATE))
      SG=SIND(PARADA(2,J_AAL))
      CG=COSD(PARADA(2,J_AAL))
      CALL DQPD0(8,SZ,0.)
      IF(FPIMDP) THEN
        K1=LPICK
        K2=LPICK
      ELSE
        K1=1
        K2=NUM
      END IF
      DO K=K1,K2
        IF(NCOL(K).GE.0) THEN
          CALL DGLEVL(NCOL(K))
          IF(     IDS.EQ.1) THEN
            PP=P(K)
          ELSE IF(IDS.EQ.2) THEN
            PP=P(K)/(1.+DS*P(K))
          ELSE
            PP=MAX(0.,1.+LOG10(P(K)+DP))
          END IF
          X1=XYZ(K,1)
          Y1=XYZ(K,2)
          Z1=XYZ(K,3)
          X2= CF*X1+SF*Y1
          Y2=-SF*X1+CF*Y1
          H1= CT*X2+ST*Z1
          Z3=-ST*X2+CT*Z1
          V1= CG*Y2+SG*Z3
          KPIKDP=NALP(K)
          CALL DQL2EP(0.,0.,H1*PP,V1*PP)
          IF(FPIKDP) GO TO 10
          IF(IDS.GT.1.) THEN
            DO L=1,14
              IF(SLOG(L).GT.PP) GO TO 10
              CALL DGLEVL(ICLG(L))
              CALL DQPD(H1*SLOG(L),V1*SLOG(L))
            END DO
          ELSE
            S=0.
            DO L=1,90
              S=S+DSTP
              IF(S.GT.PP) GO TO 10
              IF(MOD(L,5).EQ.0) THEN
                CALL DGLEVL(L10)
              ELSE
                CALL DGLEVL(LP)
              END IF
              CALL DQPD(H1*S,V1*S)
            END DO
          END IF
        END IF
   10 END DO
   99 END
