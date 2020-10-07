*DK DSCRSC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSCRSC
CH
      SUBROUTINE DSCRSC(NBNK, NROW,NLET,T,LCOL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!: Set Colour Reference Scale for Calorimeters:
C       INPUT: NBNK=ESDADB,PLSDDB,HSDADB NUMBER OF BOS BANK
C       INPUT: LCK(3) = ADRESS IN PDCODD OF 3 COLORS OF K-LAYERS
C       OUTPUT: NROW=0 IF CONSTANT COLOR
C       OUTPUT: NROW=# OF ROWS, NLET=MAX # OF LETTERS
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION LCOL(9)
      DIMENSION ICOL(0:7)
C               WH RE GR BL OR CY MG YE
      DATA ICOL/ 8,12, 9,15,11,14,13,10/
      CHARACTER *(*) T(*)
      CHARACTER *4 DT4
      CHARACTER *1 T1(0:9)
      DATA T1/'0','1','2','3','4','5','6','7','8','9'/
      CHARACTER *6 TDET
      IF(FMONDT) GO TO 99
      IF(PDCODD(4,ICCNDD).EQ.1.) GO TO 99
      CALL DV0(NBNK,NUM1,NUM2,FOUT)
      IF(NBNK.EQ.ESDADB) THEN
        IF(NUM2.GT.MCECDC) GO TO 99
        MO=MOECDC
        LK=LCEKDD(1)-1
        EN=PDCODD(2,JSECDD)
        TDET=' ECAL '
      ELSE IF(NBNK.EQ.PLSDDB) THEN
        IF(NUM2.GT.MCLCDC) GO TO 99
        MO=MOLCDC
        LK=LCLKDD(1)-1
        EN=PDCODD(2,JSLCDD)
        TDET=' LCAL '
      ELSE
        IF(NUM2.GT.MCHCDC) GO TO 99
        MO=MOHCDC
        EN=PDCODD(2,JSHCDD)
        TDET=' HCAL '
      END IF
      IF(MO.EQ.0) THEN
C       ................................ constant color may varying with layer
        IF(NBNK.EQ.MOHCDC) GO TO 99
        DO N=1,3
          T(N)='Layer '//T1(N)
          LCOL(N)=PDCODD(2,LK+N)
        END DO
        IF(LCOL(1).EQ.LCOL(2).AND.LCOL(1).EQ.LCOL(3)) GO TO 99
        NROW=3
        NLET=7
      ELSE IF(MO.EQ.1) THEN
C       .................................................... COLOR = C(ENERGY)
        DO N=1,7
          T(N)='E<'//DT4(EN)
          LCOL(N)=PDCODD(2,NCOLDD(N))
          EN=EN*2.
        END DO
        LCOL(8)=PDCODD(2,NCOLDD(8))
        T(N)='E<'//DT4(EN)
        T(N)='E<'//'9999'
    2   NROW=8
        NLET=6
      ELSE
C       ................................................. color = C(cluster #)
        T(1)='Undef.'
        LCOL(1)=ICOL(0)
        DO N=1,8
          T(N+1)='CL.# '//T1(N)
          LCOL(N+1)=ICOL(1+MOD(N,7))
        END DO
        NROW=9
        NLET=6
      END IF
      NROW=NROW+1
      T(NROW)=TDET
      LCOL(NROW)=PDCODD(2,ICTXDD)
      RETURN
   99 CALL DWRT('Constant color.')
      NROW=0
      END
*DK DSETUP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSETUP
CH
      SUBROUTINE DSETUP
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
      DATA NSTRT/0/,HC,VC/2*99./
      DATA IRSTR/998899/
      IF(NSTRT.EQ.0) THEN
        SEC1=SECNDS(0.)
C       ..................... DGETIN must be used first to open DALI.LOG
        CALL DGETIN
        CALL DATE(TISTD0( 1: 9))
        TISTD0(10:10)=' '
        CALL TIME(TISTD0(11:18))
        CALL DWRT(TISTD0(1:18))
        CALL DWRT_SETUP('END CHARACTER= ')
        CALL DWR_INPUT_SETUP('TERMINAL=OFF')
        CALL DW_ERROR_HANDLING_SETUP      
        CALL DW_GET_PLATFORM_TEXT('PF',TPLFD0,6)
        CALL DW_GET_PLATFORM_TEXT('WR',TWRTD0,1)
        CALL DWR_PLATFORM_TEXT('HD')
        CALL DWRT('Initialize DALI')
        NSTRT=1
        CALL DDATIN
C       ................................ DDATIN must be called before DCDRD !
        CALL DOPRIN
        CALL DPARST(TFILDC//'PAR_D0')
        CALL DPARGI(84,'ONC',NUMCDD)
C       ................................ DOPRIN must be called before DCDRD !
        CALL DWRT('Read '//TFILDC//'*')
        CALL DGETM0('***',1)
        TDEFDC='DEF'
        CALL DSPARD(-1)
C       .................................... DQIN MUST BE CALLED AFTER DCDRD
        CALL DQIN
C       CALL DW_SET_CO
C       CALL DWSETC
        CALL DWC_READ('PRINT_DEF')
        CALL DWC_READ('DISPLAY_DEF')
        IF(FMONDT) THEN
          PDCODD(2,ISTYDD)=0.
          PDCODD(2,ISTHDD)=0.
        END IF
        CALL DJFIR
C       ............................ DJFIR must be called before DWARNR
C       .The file DALI_??.IFI should not exist, when a new version is installed.
C       ....... and must never exist in DALINEW .... It serves as flag that the
C       ....... warning was read by the user. 
        CALL DWARNR
        CALL DDRCA0
C
C  Next call to define cursors for DALI window. DGDFCU pops up the DALI window.
C
        CALL DGDFCU
        CALL DGPOP('PUSH')
        CALL DWRT('Read help file.')
        CALL DLUTIN
        CALL DUSL0
        CALL DQHL0
        CALL DQHLP('<-')
        CALL DGOPEN( 0,'BOSLO1.LOG',5,*1,ISTAT)
        CALL DGOPEN(90,'BOSLO2.LOG',5,*1,ISTAT)
        GO TO 2
    1   CALL DWRT('File BOSLOn.LOG could not be opened.')
    2   CALL DWRT('Open BOS.')
        CALL DPARGV(81,'IW6',2,W6)
        IW6=W6
        CALL DJBOP(IW6)
        CALL MINFMT
        CALL DWRT('Initialise DALI geometry from database.')
        CALL DGDINT(IRSTR)
        CALL DEVNEW
        CALL DVVXJ
        CALL DBR0
        CALL DBRLOG('READ')
        CALL DWRT('End of DALI initialisation.')
        CALL DWRT_SETUP('END CHARACTER=|')
        CALL DWR_INPUT_SETUP('TERMINAL=ON')
        CALL DGSCUR(HC,VC)
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------  DSETUP_TIM
CH
      ENTRY DSETUP_TIM(ISEC)
CH
CH --------------------------------------------------------------------
CH
      ISEC=SECNDS(SEC1)
      IF(ISEC.LT.0) ISEC=999999
      END
*DK DSIM
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSIM
CH
      SUBROUTINE DSIM
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C INVOKED BY TANSW.EQ.'SI'  (DSIM)
      INCLUDE 'DALI_CF.INC'
      DATA PIDEG/57.29577951/
      CHARACTER *3 DT3
      CHARACTER *5 DT5
      CHARACTER *2 TANSW,TP
      CHARACTER *9 TSIM
      DATA TSIM/'   normal'/
      DATA TP/'MO'/
      DATA C/222.22222/
      DIMENSION PR(4)
      LOGICAL FOUT
      DATA PR/1.,2.,999.,0./
      DATA ZMAX/217./,RMAX/170.622/,NPT/21/,RO1/39.74/
      IF(TSIM.EQ.'   normal') NUMH=BNUMDB(2,TPCODB)
  930 CALL DWR_HIGH_LIGHT(
     $  'SI:'//TSIM//' tracks  MO='//DT5(PR(2))//'=NP',1,2)
  936 CALL DOPER(1,0,
     &  1,0,' ',0,
     &  1,1,TP,PR,
     &  NEXEC,FCHNG,TANSW)
      GO TO (910,920,930,940) NEXEC
  910 RETURN
  920 IF(TANSW.EQ.'AD') THEN
        OLD=NUMAD
        EV=EV+1.
        CALL DSIMAD(NUMAD)
        CALL DSIMAC
        GO TO 921
      END IF
      IF(TANSW.EQ.'CA') THEN
        CALL DSIMAC
        GO TO 936
      END IF
      IF(TANSW.EQ.'CL') THEN
        CALL DWRT(' SE SC DVTP, D RMAX=164.')
        CALL DWRT(' SE SC DFTD, D TETT1=-145., D TETT2=-35.')
        OLD=NUMAD
        NUMAD=0
        EV=0.
  921   AD=NUMAD
        CALL DWRT(
     &    ' #='//DT3(OLD)//'=>'//DT3(AD)//' EV='//DT3(EV))
        GO TO 930
      END IF
      IF(TANSW.EQ.'NT') THEN
        CALL=DVTPCL(0)
        TSIM='   normal'
        BNUMDB(2,TPCODB)=NUMH
        GO TO 930
      END IF
      IF(TANSW.EQ.'SN') THEN
        NUMH=BNUMDB(2,TPCODB)
        CALL DWRT('# of non simulated hits stored.')
        GO TO 936
      END IF
      IF(TANSW.EQ.'RH') THEN
        CALL DSIMRH(PR)
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  940 TSIM='simulated'
      MOTRDC=0
      CALL DWRT('Track colour set constant, do not change!')
      CALL DV0(FRFTDB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      MODEDT=1
      CP2=-2.*C*PR(2)
      DR=(RMAX-RO1)/FLOAT(NPT-1)
      MTPC=MTPCDV-1
      NH=0
      DO N=NUM1,NUM2
        IF(FNOTDT(N)) GO TO 9
        CALL DVTRV(N)
        FI0=TPHEDT(3)
        TE0=ATAN2(1.,TPHEDT(2))
        IF(TPHEDT(1).GE.0.) THEN
          Q2CPT=1./(CP2*SIN(TE0))
          CPZ2=CP2*COS(TE0)
        ELSE
          Q2CPT=-1./(CP2*SIN(TE0))
          CPZ2=-CP2*COS(TE0)
        END IF
        RO=RO1
        DO K=1,NPT
          DF=Q2CPT*RO
          ZZ=CPZ2*DF
          IF(ZZ.LT.-ZMAX.OR.ZZ.GT.ZMAX) GO TO 9
          FI=FI0+DF
          XX=RO*COS(FI)
          YY=RO*SIN(FI)
          FI=DFINXT(180.,FI*PIDEG)
          TE=DATN2D(RO,ZZ)
          ZA=ABS(ZZ)
          D0=SQRT(ZZ**2+RO**2)
          IF(RO*ZMAX.GT.ZA*RMAX) THEN
            RB=D0*(RMAX/RO-1.)
          ELSE
            RB=D0*(ZMAX/ZA-1.)
          END IF
          NH=NH+1
          VTPCDV(IVXXDV,NH)=XX
          VTPCDV(IVYYDV,NH)=YY
          VTPCDV(IVZZDV,NH)=ZZ
          VTPCDV(IVRODV,NH)=RO
          VTPCDV(IVFIDV,NH)=FI
          VTPCDV(IVTEDV,NH)=TE
          VTPCDV(IVRBDV,NH)=RB
          NTPCDV(9,NH)=N
          NCTPDC(NH)=COLRDT(N)
          IF(NH.GE.MTPC) GO TO 99
          RO=RO+DR
        END DO
    9 END DO
   99 BNUMDB(2,TPCODB)=NH
      CALL=DVTPS1(DUMMY)
      FDTPDC=.TRUE.
      FVTPDC=.TRUE.
      CALL DWRT(DT5(BNUMDB(2,TPCODB))//' points stored.')
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH
      SUBROUTINE DSIMAD(NP)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    ADD MANY EVENTS TOGETHER
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DATA MAX/999/,R1,R2/147.,167./
      DATA N8/8/
      CALL=DVTQ0(NUM)
      IF(NUM.LE.0) GO TO 9
      IF(NP.GT.MAX) GO TO 9
      DO K=1,NUM
        IF(DVTP(IVRODV,K).GT.R1.AND.DVTP(IVRODV,K).LT.R2) THEN
          IF(NP.GE.MAX) GO TO 9
          NP=NP+1
          DO N=1,8
            VTPCDV(N,NP)=DVTP(N,K)
          END DO
          NTPCDV(9,NP)=DVTP(18,K)
          NCTPDC(NP)=MOD(IFIX(DVTP(IVTEDV,K)),N8)+N8
        END IF
      END DO
    9 BNUMDB(2,TPCODB)=NP
      CALL=DVTPS1(DUMMY)
      FDTPDC=.TRUE.
      FVTPDC=.TRUE.
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH
      SUBROUTINE DSIMAC
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    COLOUR HITS WITH THETA
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION ICOL(0:7)
      DATA ICOL/ 8 ,15,14,13,12,11, 10, 9/,AMP/1./
      NUM=BNUMDB(2,TPCODB)
      DO I=1,NUM
        NN=VTPCDV(IVTEDV,I)*AMP
        LCOL=MOD(NN+8888,8)
        NCTPDC(I)=ICOL(LCOL)
      END DO
      CALL=DVTPS1(DUMMY)
      FDTPDC=.TRUE.
      FVTPDC=.TRUE.
      END
*DK DSIMRH
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSIMRH
CH
      SUBROUTINE DSIMRH(PR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DIMENSION PR(4)
      DATA IRAN/534277/,ICOL/8/
      DATA ZMAX/220./,RMAX/180./,RMIN/30./
      NUM=PR(2)
      DO K=1,NUM
        Q=RAN(IRAN)
        FI=Q*360.
        Q=RAN(IRAN)
        ZZ=Q*2.*ZMAX-ZMAX
        Q=RAN(IRAN)
        RO=RMIN+(RMAX-RMIN)*Q
        VTPCDV(IVXXDV,K)=RO*COSD(FI)
        VTPCDV(IVYYDV,K)=RO*SIND(FI)
        VTPCDV(IVZZDV,K)=ZZ
        VTPCDV(IVRODV,K)=RO
        VTPCDV(IVFIDV,K)=FI
        VTPCDV(IVTEDV,K)=70.
        VTPCDV(IVRBDV,K)=0
        ICOL=ICOL+1
        IF(ICOL.EQ.16) ICOL=8
        NCTPDC(K)=ICOL
      END DO
      BNUMDB(2,TPCODB)=NUM
      FDTPDC=.TRUE.
      FVTPDC=.TRUE.
      CALL=DVTPS1(DUMMY)
      END
*DK DSU
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSU
CH
      SUBROUTINE DSU
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!: SETUP PICTURE
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C INVOKED BY TANSW.EQ.'SU'  (DSU)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW
      CHARACTER *3 DT3
      LOGICAL FYES
      DATA JDEB/0/
      DATA HFH,HRH,HAH,HTH/0.,-1027.,-7.,-1543./
      CALL DCOPTL
  930 CALL DWR_HL_AR('SU:W?: setup picture')
  936 CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(1,0,
     &  1,0,' ',0,
     &  1,0,' ',0,
     &  NEXEC,CHG,TANSW)
      CALL DGZOOM(6,-1,0,0)
      GO TO (910,920,930,940),NEXEC
  910 IF(TANSW.NE.'GW') CALL DCOPFL
C      FRNLDU=FRNL
      RETURN
  920 IF(TANSW.EQ.'TA') THEN
        CALL DCDTWF
        GO TO 930
      END IF
      IF(TANSW.EQ.'TW') THEN
        IF(JDEB.EQ.0) CALL DGCLWK
        CALL DQWSU
        CALL DWSETC
        CALL DGCLWK
        GO TO 930
      END IF
      IF(TANSW.EQ.'TF') THEN
        FRNLDU=-FRNLDU
        GO TO 930
      END IF
C      IF(TANSW.EQ.'ED'.AND.WISUDW.EQ.1.) THEN
C        CALL DQCL(8)
C        GO TO 936
C      END IF
      IF(TANSW.EQ.'CW') THEN
        CALL DGCLWK
        CALL DQTIT(1)
      END IF
      IF(TANSW.EQ.'TB'.AND.(.NOT.FMONDT)) THEN
        CALL DCDBW
        GO TO 936
      END IF
      IF(TANSW.EQ.'SC') THEN
        DFWIDU(0)=1111.
        CALL DWRT('Normal scales switched on.')
        TANSW='sc'
      END IF
      IF(TANSW.EQ.'NS') THEN
        DFWIDU(0)=0.
        CALL DWRT('All scales switched of.')
        TANSW='sc'
      END IF
      IF(TANSW.EQ.'SB') THEN
        DFWIDU(0)=12.
        CALL DWRT('Scale bars switched on.')
        TANSW='sc'
      END IF
      IF(TANSW.EQ.'sc') THEN
  937   IF(.NOT.FMACDM) THEN
          CALL DTYANS('Same scaling for zoom?  N,Y=<CR>','N',NANSW)
          IF(NANSW.EQ.1) GO TO 930
        END IF
        DFWIDU(1)=DFWIDU(0)
        CALL DWRT('Zoom setup = No-zoom setup.')
        GO TO 930
      END IF
      IF(TANSW.EQ.'FH') THEN
        HEADDU=HFH
        IFULDB=1
        CALL DQTIT(IFULDB)
        GO TO 930
      END IF
      IF(TANSW.EQ.'RH') THEN
        HEADDU=HRH
        IFULDB=1
        CALL DQTIT(IFULDB)
        GO TO 930
      END IF
      IF(TANSW.EQ.'AH') THEN
        HEADDU=HAH
        IFULDB=1
        CALL DQTIT(IFULDB)
        GO TO 930
      END IF
      IF(TANSW.EQ.'RH') THEN
        HEADDU=HRH
        IFULDB=1
        CALL DQTIT(IFULDB)
        GO TO 930
      END IF
      IF(TANSW.EQ.'TH') THEN
        HEADDU=HTH
        IFULDB=1
        CALL DQTIT(IFULDB)
        GO TO 930
      END IF
      IF(TANSW.EQ.'M0') THEN
        PDCODD(2,MODEDD)=0.
        CALL DWRT('No sectors for TPC and HCAL.')
        GO TO 930
      END IF
      IF(TANSW.EQ.'M1') THEN
        PDCODD(2,MODEDD)=1.
        CALL DWRT('Sectors and gaps fo TPC and HCAL.')
        GO TO 930
      END IF
      IF(TANSW.EQ.'M2') THEN
        PDCODD(2,MODEDD)=2.
        CALL DWRT('Sectors without gaps for TPC,no HCAL sectors.')
        GO TO 930
      END IF
      IF(TANSW.EQ.'M3') THEN
        PDCODD(2,MODEDD)=3.
        CALL DWRT('Sectors without gaps for TPC and HCAL.')
        GO TO 930
      END IF
      IF(TANSW.EQ.'RE') THEN
        PDCODD(4,MXTPDD)=1.
        CALL DWRT('Enable R-cut ('//DT3(PDCODD(2,MXTPDD))//
     &    ') for TPC sectors.')
        GO TO 930
      END IF
      IF(TANSW.EQ.'RD') THEN
        PDCODD(4,MXTPDD)=-1.
        CALL DWRT('Disable R-cut for TPC sectors.')
        GO TO 930
      END IF
      IF(TANSW.EQ.'T1') THEN
        DLINDD=1.
        PDCODD(2,LITRDD)=DLINDD
        CALL DPARSV(87,'STR',2,DLINDD)
        CALL DWRT(' Thin lines for tracks.')
        GO TO 930
      END IF
      IF(TANSW.EQ.'T2') THEN
        DLINDD=2.
        PDCODD(2,LITRDD)=DLINDD
        CALL DPARSV(87,'STR',2,DLINDD)
        CALL DWRT(' Medium lines for tracks.')
        GO TO 930
      END IF
      IF(TANSW.EQ.'T3') THEN
        DLINDD=3.
        PDCODD(2,LITRDD)=DLINDD
        CALL DPARSV(87,'STR',2,DLINDD)
        CALL DWRT(' Thick lines for tracks.')
        GO TO 930
      END IF
      IF(TANSW.EQ.'L1') THEN
        PDCODD(2,LIDTDD)=1.
        CALL DWRT(' Thin lines for the detector.')
        GO TO 930
      END IF
      IF(TANSW.EQ.'L2') THEN
        PDCODD(2,LIDTDD)=2.
        CALL DWRT(' Medium lines for the detector.')
        GO TO 930
      END IF
      IF(TANSW.EQ.'L3') THEN
        PDCODD(2,LIDTDD)=3.
        CALL DWRT(' Thick lines for the detector.')
        GO TO 930
      END IF
      CALL DAREA('D',TANSW,0,12,IAREDO,FYES)
      IF(FYES) GO TO 940
      CALL DWR_IC(TANSW)
      GO TO 936
  940 CALL DSC0
      CALL DSCTR1
      NAR=IAREDO
      CALL DPCEAR(NAR)
      IAREDO=NAR
      GO TO 936
      END
*DK DSPARD
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------------------  DSTPARD
CH
      SUBROUTINE DSPARD(MODE)
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
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      DIMENSION LUT(MPNPDL),MODC(MONUDC)
      EQUIVALENCE (LUT,MNUMDL),(MODC,MOTRDC)
      CHARACTER *1 T1,TMOD(-1:3),TM
C               -1   0   1   2   3
      DATA TMOD/' ',' ',' ',' ','*'/
      CHARACTER *2 T2,TN
      CHARACTER *38 TFIL,TCOM
      CHARACTER *(*) TNAM
      PARAMETER (NC=10)
      CHARACTER *2 TC(NC)
      DIMENSION P(4)
      DIMENSION LTAB(0:15)
      DATA LDEB/0/
      DATA LTAB/ 1,13, 2, 6, 3, 5, 7, 8,14, 4,11,10,16,12,15, 9/
C                0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
      LOGICAL FRET
      IF(MODE.LE.0) THEN
        TFIL=TFILDC//'PAR_DEF'
      ELSE
        CALL DSPANA(' Read ',TFIL,FRET)
        IF(FRET) RETURN
      END IF
      GO TO 1
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DSPARN
CH
      ENTRY DSPARN(TNAM,MODE)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
      TFIL=TNAM

C   1 USLL=USLVDU
    1 TC( 1)='PU'
      TC( 2)='PO'
      TC( 3)='TF'
      TC( 4)='SM'
      TC( 5)='SV'
      TC( 6)='MO'
      TC( 7)='PD'
      TC( 8)='CO'
      TC( 9)='CD'
      TC(10)='CP'
      TM=TMOD(MODE)
      CALL DGOPEN(NUNIDU,TFIL,2,*99,ISTAT)
    8 READ(NUNIDU,1000,ERR=8,END=98) T2,T1,NP,P,TN,TCOM
 1000 FORMAT(A2,A1,I3,3F16.5,F7.0,1X,2A)
      IF(T1.NE.TM) GO TO 8
      IF(NP.EQ.-1) THEN
        IF(MODE.GT.1) CALL DSPAOF('READ ',TCOM,T2,TC,NC)
        GO TO 8
      END IF
      IF(NP.LT.0) GO TO 8
      IF     (T2.EQ.TC(1)) THEN
C       .................................................................... PU
        TPR1DU(NP)=TN
        PLIMDU(1,NP)=P(1)
        PARADU(  NP)=P(2)
        PLIMDU(2,NP)=P(3)
      ELSE IF(T2.EQ.TC(2)) THEN
C       .................................................................... PO
        CALL UCOPY(P,PARADA(1,NP),4)
      ELSE IF(T2.EQ.TC(3)) THEN
C       .................................................................... TF
        CALL UCOPY(P,COLIDF(1,NP),4)
      ELSE IF(T2.EQ.TC(4)) THEN
C       .................................................................... SM
        LUT(NP)=P(2)
      ELSE IF(T2.EQ.TC(5)) THEN
C       .................................................................... SV
        JD=MOD(NP,10)
        JP=NP/10
C//     CALL UCOPY(P,WDSNDL(1,JP,JD),4)
      ELSE IF(T2.EQ.TC(6)) THEN
C       .................................................................... MO
        MODC(NP)=P(2)
      ELSE IF(T2.EQ.TC(7)) THEN
C       .................................................................... PD
        TDCODD(NP)=TN
        CALL UCOPY(P,PDCODD(1,NP),4)
      ELSE IF(T2.EQ.TC(8)) THEN
C       .................................................................... CO
        RDCODD(NP)=P(1)
        GRCODD(NP)=P(2)
        BLCODD(NP)=P(3)
        IF(MODE.LE.0) THEN
          RDCPDD(NP)=P(1)
          GRCPDD(NP)=P(2)
          BLCPDD(NP)=P(3)
          RDCDDD(NP)=P(1)
          GRCDDD(NP)=P(2)
          BLCDDD(NP)=P(3)
        END IF
      ELSE IF(T2.EQ.TC(9)) THEN
C       .................................................................... CD
        RDCDDD(NP)=P(1)
        GRCDDD(NP)=P(2)
        BLCDDD(NP)=P(3)
      ELSE IF(T2.EQ.TC(10)) THEN
C       .................................................................... CP
        RDCPDD(NP)=P(1)
        GRCPDD(NP)=P(2)
        BLCPDD(NP)=P(3)
      ELSE
        GO TO 8
      END IF
      IF(LDEB.EQ.1.OR.MODE.EQ.3) THEN
        WRITE(TXTADW,2000) T2,T1,NP,P,TN,TCOM(1:7)
 2000   FORMAT(A2,A1,I3,3F16.5,F7.0,1X,2A)
        CALL DWRC
      END IF
      GO TO 8
   98 CLOSE(NUNIDU)
      IF(MODE.EQ.-1.OR.(.NOT.FMONDT)) THEN
        IF(PDCODD(2,MOLTDD).EQ.0) THEN
          DO K=0,15
            LTABDD(K)=LTAB(K)
          END DO
          DO K=16,127
            LTABDD(K)=K+1
          END DO
        ELSE
          DO K=0,127
            LTABDD(K)=K+1
          END DO
        END IF
      END IF
      CALL DSC0
      CALL DSCTR1
      IF(MODE.GE.0) CALL DW_SET_CO
C     IF(USLL.GT.0.) USLVDU=USLL
      RETURN
C
   99 CALL DWRT('File '//TFIL(1:LENOCC(TFIL))//' not found.')
C     USLVDU=USLL
      END
*DK DSPANA
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DSPANA
CH
      SUBROUTINE DSPANA(TRW,TFILE,FRET)
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
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      CHARACTER *6 TRW
      CHARACTER *38 TFILE,TLAST,TN
C                 123456789 123456789
      DATA TLAST/'DALI_B#.PAR_NEW'/
      CHARACTER *15 TDEF
      DATA TDEF /'DALI_B#.PAR_DEF'/
      LOGICAL FRET,FSTRT
      DATA FSTRT/.TRUE./
      IF(FSTRT) THEN
        FSTRT=.FALSE.
        TLAST(1:8)=TFILDC
        TDEF(1:8)=TFILDC
      END IF
    1 TFILE=TLAST
    2 CALL DWRT(TRW//TFILE//' ? <N>')
      CALL DGETLN(TN,LN,LEN(TN))
      IF(LN.LE.1) THEN
        IF(LN.LE.0.OR.TN(1:1).NE.'Y') THEN
          FRET=.TRUE.
        ELSE
          FRET=.FALSE.
          TLAST=TFILE
        END IF
        RETURN
      END IF
      IF(TN(1:1).EQ.'.') THEN
        DO L=1,38
          IF(TLAST(L:L).EQ.'.') GO TO 3
        END DO
        GO TO 9
    3   TFILE=TLAST(1:L-1)//TN
      ELSE IF(TN(1:1).EQ.'_') THEN
        DO L=38,1,-1
          IF(TLAST(L:L).EQ.'_') GO TO 4
        END DO
        GO TO 9
    4   TFILE=TLAST(1:L-1)//TN
      ELSE
        DO L=2,38
          IF(TN(L:L).EQ.'.') GO TO 5
        END DO
        GO TO 9
    5   TFILE=TN
      END IF
      IF(TRW.EQ.' Read '.OR.TFILE.NE.TDEF) GO TO 2
    9 CALL DWRT('Wrong name '//TFILE)
      GO TO 1
      END
*DK DSPAOF
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DSPAOF
CH
      SUBROUTINE DSPAOF(TWR,TCOM,TIN,TC,NC)
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
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TWR
      CHARACTER *(*) TC(*),TCOM
      CHARACTER *2 TN,TIN
      DO N=1,NC
        IF(TIN.EQ.TC(N)) THEN
          TXTADW=TWR//TCOM//' ? <N>'
          CALL DWRC
          CALL DGETLN(TN,LN,LEN(TN))
          IF(LN.LE.0.OR.TN(1:1).NE.'Y') THEN
            TC(N)='OF'
            TIN='OF'
          END IF
          GO TO 9
        END IF
      END DO
    9 END
