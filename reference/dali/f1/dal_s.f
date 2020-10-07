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
        CALL DATE4(TISTD0( 1:11))
        TISTD0(12:12)=' '
        CALL TIME(TISTD0(13:20))
        CALL DWRT(TISTD0(1:20))
        CALL DWRT_SETUP('END CHARACTER= ')
C        CALL DWR_INPUT_SETUP('TERMINAL=OFF')
        CALL DW_ERROR_HANDLING_SETUP
        CALL DW_READ_PLATFORM_TEXT
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
        CALL DQHLP('<+')
        CALL DBR0
        CALL DBRLOG('READ')
        CALL DO_BAR(2)
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
C     DATA HFH,HRH,HAH,HTH,HEH/0.,-1027.,-7.,-1543./
      DATA HFH,HRH,HAH,HTH,HEH/0.,-1027.,-7.,-1543.,-1547./
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
        CALL DPARSV(95,'CTM',4,1.)
        CALL DWRT('Normal scales switched on.')
        TANSW='sc'
      END IF
      IF(TANSW.EQ.'NS') THEN
        DFWIDU(0)=0.
        CALL DPARSV(95,'CTM',4,-1.)
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
      IF(TANSW.EQ.'TH') THEN
        HEADDU=HTH
        IFULDB=1
        CALL DQTIT(IFULDB)
        GO TO 930
      END IF
      IF(TANSW.EQ.'EH') THEN
        HEADDU=HEH
        IFULDB=1
        CALL DQTIT(IFULDB)
        GO TO 930
      END IF
      IF(TANSW.EQ.'HS') THEN
        CALL DQCL(13)
        HHGHDG(13)=HHGHDG(12)
        IFULDB=1
        CALL DQTIT(IFULDB)
        GO TO 930
      END IF
      IF(TANSW.EQ.'HL') THEN
        HHGHDG(13)=HHGHDG( 0)
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
*DK DSC0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSC0
CH
      SUBROUTINE DSC0
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      IF(FNOCDC) THEN
        FDTRDC=.TRUE.
        FDITDC=.TRUE.
        FDTPDC=.TRUE.
        FDLCDC=.TRUE.
        FDECDC=.TRUE.
        FDEODC=.TRUE.
        FDHCDC=.TRUE.
      ELSE
        FDTRDC=.FALSE.
        FDITDC=.FALSE.
        FDTPDC=.FALSE.
        FDLCDC=.FALSE.
        FDECDC=.FALSE.
        FDEODC=.FALSE.
        FDHCDC=.FALSE.
      END IF
      CALL DV_QVEC_DEF_COL_IN
      END
*DK DSC_CONST
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DSC_CONST
CH
      SUBROUTINE DSC_CONST(FV,NCOL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DATA L15/15/
      LOGICAL FV
      IF(FMONDT) THEN
C       ................................................................. MONO
        FV=.FALSE.
        NCOL=15
        CALL DGLEVL(L15)
        RETURN
      END IF
C     ................................... data colors set constant
      CALL DPAR_SET_CO(48,'CCN')
      IF(ICOLDP.GE.0) THEN
        FV=.FALSE.
        NCOL=ICOLDP
        RETURN
      END IF
      NCOL=-1
      END
*DK DSET_ECAL_COL
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++ DSET_ECAL_COL
CH
      SUBROUTINE DSET_ECAL_COL(MO,LC,NCO,IC)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Setup ecal colors
C     output: MO=-1 color = layer, =0: color=const, =1 color=energy
C             LC loop color from 1 to LC
C             NC = constant color
C             IC = color array
      INCLUDE 'DALI_CF.INC'
      DIMENSION IC(8)
      DIMENSION IA(8)
      EXTERNAL DVEC
      IF(.NOT.FDECDC) THEN
        FDECDC=.TRUE.
        CALL DSET_CAL_COL(ESDADB,DVEC,FVECDC,NCECDC,M,LA,NC,IA)
      END IF
      MO=M
      LC=LA
      NCO=NC
      CALL UCOPY(IA,IC,8)
      END
C
C
C
      SUBROUTINE DSET_CAL_COL(NBNK,DFU, FV,NC,MODE,MCOL,NCOL,IC)
C
C
C
C
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!: SET UP COLORS OF ECAL AND LCAL
C       INPUT: NBNK=ESDADB,PLSDDB NUMBER OF BOS BANK
C       INPUT: LCK(3) = ADRESS IN PARADA OF 3 COLORS OF K-LAYERS
C       BOTH: FV=FVECDC,FVLCDC   =TRUE IF VARIABLE COLOR
C       OUTPUT: MODE=-1 VARIABLE COLOR 1 LOOP            MCOL=1 CO=CO(K)
C       OUTPUT: MODE= 0 CONSTANT COLOR 1 LOOP            MCOL=1 CO=CNST
C       OUTPUT: MODE= 1 VARIABLE COLOR 8 LOOPS PER COLOR MCOL=8 CO=CO(E)
C             : MCOL : NUMBER OF LOOPS (1 OR 8)
C             : NCOL=CONSTANT COLOR
C             : IC = color array
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      EXTERNAL DFU
      DIMENSION NC(*),IC(*)
      DIMENSION ICOL(8)
      DATA ICOL/1,2,3,4,5,6,7,8/
      LOGICAL FV
      TSUBDJ='DSET_CAL_COL'
      IF(FPIKDP) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_FLC,J_FFL,J_FCL,J_FEC,J_FFE,J_FCE'
      CALL DPARAM(70
     &  ,J_FLC,J_FFL,J_FCL,J_FEC,J_FFE,J_FCE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DSC_CONST(FV,NCOL)
      IF(NCOL.GE.0) THEN
        MCOL=1
        MODE=0
        RETURN
      END IF
      IF(NBNK.EQ.ESDADB) THEN
        MX=MCECDC                  ! = PARAMETER IN DALI_CF.INC
        MO=PARADA(2,J_FEC)
        EFAC=PARADA(2,J_FFE)
        ENCUT=PARADA(2,J_FCE)
        NGRUP=53
      ELSE
        MX=MCLCDC                  ! = PARAMETER IN DALI_CF.INC
        MO=PARADA(2,J_FLC)
        EFAC=PARADA(2,J_FFL)
        ENCUT=PARADA(2,J_FCL)
        NGRUP=56
      END IF
      NUM=BNUMDB(2,NBNK)
      IF(NUM.LE.0) RETURN
      IF(NUM.GT.MX.OR.MO.EQ.0) THEN
        FV=.FALSE.
        CALL DPAR_SET_CO(NGRUP,'CCN')
        NCOL=ICOLDP
        MODE=0
        MCOL=1
        RETURN
      END IF
C     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ VARIABLE COLOR
      FV=.TRUE.
      IF(MO.EQ.1) THEN
C       .................................................... COLOR = C(ENERGY)
        DO K=1,NUM
          E=DFU(IVENDV,K)
          CALL DSCLOG(E,EFAC,ICOL,NC(K))
        END DO
        MODE=1
        CALL DPARGI(47,'CNU',MCOL)
        CALL DPAR_GET_ARRAY(47,'CC1',MCOL,IC)
      ELSE IF(MO.EQ.2) THEN
C       ............................................. COLOR = Cut(ENERGY)
        DO K=1,NUM
          IF(DFU(IVENDV,K).LT.ENCUT) THEN
            NC(K)=1
          ELSE
            NC(K)=2
          END IF
        END DO
        MODE=1
        MCOL=2
        CALL DPAR_GET_ARRAY(53,'CE1',MCOL,IC)
      ELSE
C       ......................... constant color varying with layer
        DO N=1,NUM
          NC(N)=DFU(IVKKDV,N)
        END DO
        MODE=-1
        MCOL=1
        CALL DPAR_GET_ARRAY(NGRUP,'CL1',3,IC)
      END IF
      END
*DK DSCELM
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSCELM
CH
      SUBROUTINE DSCELM(K1,K2,MSY,MCO,FVC,IM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!: SET UP DRAW MODE FOR ECAL AND LCAL IN DFTPE AND DFTPL
C   INPUT:
C       draw from layer K1 to layer K2
C       MSY = SYMBOL     may also be output
C   OUTPUT:
C       MCO=MOECDC or MO=MOLCDC = color function
C       FVC=FVECDC or FV=FVLCDC
C       IM=
C       0   SET MSY TO 1 and repeat
C       1=1 LOOP  CONSTANT COLOR
C       2=1 LOOP  COLOUR=NC_CDC   = NCECDC or NCLCDC
C       3=8 LOOPS COLOUR=NC_CDC MAX
C       4=8 LOOPS COLOUR=NC_CDC BACKWARDS MAX(FT)
C       5=  ADD   and set constant colors

C       SYMB: MSY=1:CONST / =2:LEGO / =3:NOT USED / =4:NUMBER / =5:SQUARE
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION IMOD(0:1,2,4,3),JMOD(48),LARR(12)
      EQUIVALENCE (IMOD,JMOD)
C     &   2, 2 , 1, 1 ,  2, 2 , 5, 5 ,  2, 2 , 1, 5 ,  2, 2 , 5, 5,  ! CN=K
C      DATA IMOD/
C            CN             LE             NU             SQ       =SYMBOL
C       K1=K2  K1#K2   K1=K2  K1#K2   K1=K2  K1#K2   K1=K2  K1#K2
C       NZ ZO  NZ ZO   NZ ZO  NZ ZO   NZ ZO  NZ ZO   NZ ZO  NZ ZO  COLOUR=
C     &   1, 1 , 1, 1 ,  2, 2 , 5, 5 ,  0, 2 , 0, 5 ,  1, 1 , 5, 5,  ! CN
C     &   3, 2 , 3, 3 ,  3, 3 , 5, 5 ,  0, 2 , 0, 5 ,  4, 2 , 4, 5,  ! EN,CU
C     &   3, 2 , 1, 1 ,  2, 1 , 5, 5 ,  0, 2 , 0, 5 ,  2, 2 , 5, 5/  ! KK
C
      DIMENSION ISYM(0:5),ICFU(0:3)
      DATA ISYM/1,1,2,1,3,4/,ICFU/1,2,2,3/,M1/1/
      LOGICAL FVC,FSTRT,F0
      DATA FSTRT/.TRUE./
      CALL DPARGv(88,'EM1',4,DEB)
      F0=.TRUE.
      IF(DEB.EQ.1..OR.FSTRT) THEN
        CALL DPAR_GET_ARRAY(88,'EM1',12,LARR)
        J=1
        DO L=1,12
          DO K=J+3,J,-1
            JMOD(K)=MOD(LARR(L),10)
            LARR(L)=LARR(L)/10
          END DO
          J=J+4
        END DO
        FSTRT=.FALSE.
      END IF
      IF(K1.EQ.K2) THEN
        IK=1
      ELSE
        IK=2
      END IF
      IF(FVC) THEN
        MC=ICFU(MCO)
      ELSE
        MC=M1
      END IF
    2 IM=IMOD(IZOMDO,IK,ISYM(MSY),MC)
      IF(IM.EQ.0) THEN
        IF(F0) THEN
          F0=.FALSE.
          MSY=1
          GO TO 2
        END IF
        IM=1
      END IF
      IF(DEB.EQ.1.) THEN
        WRITE(TXTADW,1000) 'K1',K1,'K2',K2,'CF',MCO,'SY',MSY,'IM',IM
 1000   FORMAT(5(A,'=',I2,4X))
        CALL DWRC
      END IF
      END
*DK DSCHC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSCHC
CH
      SUBROUTINE DSCHC
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION LCOL(8)
C      DIMENSION ICOL(0:7)
C               WH RE GR BL OR CY MG YE
C      DATA ICOL/ 8,12, 9,15,11,14,13,10/
      LOGICAL FOUT
      IF(FPIKDP) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_FHC,J_FFH,J_FCH'
      CALL DPARAM(70
     &  ,J_FHC,J_FFH,J_FCH)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DSC_CONST(FVHCDC,NCOL)
      IF(NCOL.GE.0) RETURN
      MO=PARADA(2,J_FHC)
      IF(MO.EQ.0) THEN
        FVHCDC=.FALSE.
        CALL DPAR_SET_CO(54,'CHI')
        RETURN
      END IF
C     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ VARIABLE COLOR
      CALL DV0(HSDADB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      IF(NUM2.GT.MCHCDC) THEN
        FVHCDC=.FALSE.
        CALL DPAR_SET_CO(54,'CHI')
        RETURN
      END IF
      IF(FDHCDC) RETURN
      FDHCDC=.TRUE.
      FVHCDC=.TRUE.
      IF(MO.EQ.1) THEN
C       .................................................... COLOR = C(ENERGY)
        CALL DPAR_GET_ARRAY(47,'CC1',8,LCOL)
        EFAC=PARADA(2,J_FFH)
        DO 710 K=NUM1,NUM2
          E=DVHC(IVENDV,K)
          CALL DSCLOG(E,EFAC,LCOL,NCHCDC(K))
  710   CONTINUE
      ELSE
        CALL DPARGI(54,'CE1',ICOE1)
        CALL DPARGI(54,'CE2',ICOE2)
        ENCUT=PARADA(2,J_FCH)
        DO K=NUM1,NUM2
          IF(DVHC(IVENDV,K).LT.ENCUT) THEN
            NCHCDC(K)=ICOE1
          ELSE
            NCHCDC(K)=ICOE2
          END IF
        END DO
      END IF
      END
*DK DSCIT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSCIT
CH
      SUBROUTINE DSCIT(IVNT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      IF(FPIKDP.OR.FDITDC) RETURN
      FDITDC=.TRUE.
      CALL DSC_CONST(FVITDC,NCOL)
      IF(NCOL.GE.0) RETURN
      NUM=BNUMDB(2,ITCODB)
      IF(NUM.EQ.0) RETURN
      IF(NUM.GT.MCITDC) THEN
        CALL DPAR_SET_CO(51,'CCN')
        FVITDC=.FALSE.
        RETURN
      END IF
      CALL DPARGI(70,'FIT',MO)
      IF(MO.EQ.0) THEN
C       ................................................... constant hit color
        FVITDC=.FALSE.
        CALL DPAR_SET_CO(51,'CCN')
      ELSE
        CALL DSCTR0(FVITDC,NCOL)
        IF(.NOT.FVITDC) THEN
          CALL DGLEVL(NCOL)
        ELSE
C         ............................................... variable track color
          CALL DPARGI(51,'CUC',LUC)
          CALL=DVITST(IVNTDV,IVNT,IDUM)
          DO K=1,NUM
            N=MAX(NITCDV(IVNT,K),NITCDV(IVNT+1,K))
            IF(N.EQ.0) THEN
              NCITDC(K)=LUC
            ELSE
              NCITDC(K)=NCTRDC(N)
            END IF
          END DO
        END IF
      END IF
      END
*DK DSCLOG
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSCLOG
CH
      SUBROUTINE DSCLOG(G,FAC,LCOL,NCOL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C
C    Called by :
C ---------------------------------------------------------------------
      DIMENSION LCOL(8)
      F=FAC
      DO 700 L=1,7
        IF(G.LT.F) THEN
          NCOL=LCOL(L)
          GO TO 9
        END IF
        F=F*2.
  700 CONTINUE
      NCOL=LCOL(8)
    9 END
*DK DSCTP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSCTP
CH
      SUBROUTINE DSCTP
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION ICOL(0:20)
      DATA ICOL/12, 9,15,11,14,13,10,
     &          12,15,14,10, 9,11,13,
     &          12,14, 9,13,15,10,11/
      IF(FPIKDP.OR.FDTPDC) RETURN
      FDTPDC=.TRUE.
      CALL DSC_CONST(FVTPDC,NCOL)
      IF(NCOL.GE.0) RETURN
      NUM=BNUMDB(2,TPCODB)
      IF(NUM.EQ.0) RETURN
      IF(NUM.GT.MCTPDC) THEN
        CALL DPAR_SET_CO(52,'CCN',NCOL)
        FVTPDC=.FALSE.
        RETURN
      END IF
      CALL DPARGI(70,'FTP',MO)
      IF(MO.EQ.0) THEN
C       ................................................... constant hit color
        FVTPDC=.FALSE.
        CALL DPAR_SET_CO(52,'CCN')
      ELSE IF(MO.EQ.1) THEN
        CALL DSCTR0(FVTPDC,NCOL)
        IF(.NOT.FVTPDC) THEN
          CALL DGLEVL(NCOL)
        ELSE
C         ............................................... variable track color
          CALL DPARGI(52,'CUC',NCTRDC(0))
          CALL DPARGV(52,'CNF',4,P4UF)
          IF(P4UF.GT.0.)  CALL DPARGI(52,'CNF',LNF)
          CALL=DVTPST(IVNTDV,IVNT)
          DO K=1,NUM
            CALL=DVTPNT(K,N)
            IF(N.LT.0) THEN
C             .............................. hits rejected for fitting N<0
              IF(P4UF.LT.0.) THEN
                NCTPDC(K)=NCTRDC(-N)
              ELSE
                NCTPDC(K)=LNF
              END IF
            ELSE
C             .................................. associated fitted hits N>0
              NCTPDC(K)=NCTRDC(N)
            END IF
          END DO
        END IF
      ELSE
C       ........................................ ncol = mod (theta,21')
        FVTPDC=.TRUE.
        CALL DPARGV(52,'CDT',2,CDT)
        QDT=1./CDT
        DO K=1,NUM
          NTE=DVTP(IVTEDV,K)*QDT
          NCTPDC(K)=ICOL(MOD(NTE,21))
        END DO
      END IF
      END
*DK DSCTR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSCTR
CH
      SUBROUTINE DSCTR
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!: SET UP COLOR OF TRACKS
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      IF(FPIKDP) RETURN
      CALL DSC_CONST(FVTRDC,NCOL)
      IF(NCOL.GE.0) RETURN
      CALL DPARGI(70,'FTR',MO)
      IF(MO.EQ.0) THEN
        CALL DPAR_SET_CO(58,'CCN')
        FVTRDC=.FALSE.
      ELSE
        CALL DSCTR0(FVTRDC,NCOL)
        IF(.NOT.FVTRDC) CALL DGLEVL(NCOL)
      END IF
      END
*DK DSCTR0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSCTR0
CH
      SUBROUTINE DSCTR0(FVCOL,NCOL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!: SET UP COLOR OF TRACKS
C    Outputs   :FVCOL=true : color is variable ;else =NCOL
C
C    Tracks are divided into two objects: visible tracks TR and charged
C    as stored in FRFT.
C    Vdet hits, ITC hits, TPC hits and visible tracks may be set sperately
C    constant or get the color of the charged particle. This is defined in
C    GT:CF.
C    DSCTR0 defines the color of the charged particle.




C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION FI(1000)
C     EQUIVALENCE (FI,NCTRDC)
      LOGICAL FVCOL,FVOLD
      DIMENSION ICOL(0:7),LCOL(8)
C     DIMENSION LCOL(8),ICOL0(0:7)
C               RE BL OR CY MG YE
C     DATA ICOL0/12,9,15,11,14,8,13,10/
C       ++++++++++++++++++++++++++++++++++ CONSTANT TRACK COLOR
      IF(FNOCDC) RETURN
      CALL DSC_CONST(FVCOL,NCOL)
      IF(NCOL.GE.0) RETURN
      N=BNUMDB(2,FRFTDB)
      IF(N.GT.MCTRDC.OR.N.LE.0) THEN
C       ................. constant if nctrdc dimension too small
        FVCOL=.FALSE.
        CALL DPAR_SET_CO(59,'CCN')
        GO TO 88
      END IF
      IF(FDTRDC) THEN
C       ......................................................... already done
        FVCOL=FVOLD
        NCOL=NCOLD
        RETURN
      END IF
C     +++++++++++++++++++++++++++++++++++++++++++++++++++ VARIABLE TRACK COLOR
      FVCOL=.TRUE.
      FDTRDC=.TRUE.
      CALL DPARGI(70,'FCP',MO)
      IF(MO.EQ.0) THEN
        CALL DPARGI(59,'CCN',NCOL)
        NCOLD=NCOL
        NCTRDC(0)=NCOL
        CALL UFILL(NCTRDC(1),1,N,NCOL)
C       .... colors can still be varied due to manual list, therefore:
      ELSE IF(MO.EQ.1) THEN
C       ............................................... color mod(tr#,NUCOL)
        CALL DVTRHT(0,NDUM)
        CALL DPARGI(59,'CIO',NCITO)
        CALL DPARGI(46,'CNU',NUCOL)
        CALL DPAR_GET_ARRAY(46,'CC1',NUCOL,ICOL)
        CALL=DVCHT0(NUM)
        CALL=DVCHI0(NUM)
        IF(N.EQ.1) THEN
          NORDDT(1)=1
          CALL DVTRHT(1,KTPC)
          IF(KTPC.NE.0) THEN
            NCTRDC(1)=ICOL(0)
          ELSE
            NCTRDC(1)=NCITO
          END IF
          GO TO 80
        END IF
C       .................................... Order tracks with increasing phi
        N=MIN(N,999)
        DO K=1,N
          CALL DVTRFI(K,FI(K))
        END DO
        CALL VZERO(NWRKDW,N)
        DO K1=1,N-1
          DO K2=K1+1,N
            IF(FI(K1).GT.FI(K2)) THEN
               NWRKDW(K1)=NWRKDW(K1)+1
            ELSE
               NWRKDW(K2)=NWRKDW(K2)+1
            END IF
          END DO
        END DO
        DO I=1,N
          NORDDT(NWRKDW(I)+1)=I
        END DO
        DO I=1,N
          CALL DVTRHT(I,KTPC)
          IF(KTPC.NE.0) THEN
            NCTRDC(I)=ICOL(MOD(NWRKDW(I),NUCOL))
          ELSE
            NCTRDC(I)=NCITO
          END IF
        END DO
      ELSE IF(MO.EQ.2) THEN
C       ....................................... color = c(monentum)
        CALL DPAR_GET_ARRAY(47,'CC1',8,LCOL)
        CALL DPARGV(70,'FFC',2,EFAC)
        DO K=1,N
          CALL DVTRTP(K,P)
          P=ABS(P)
          CALL DSCLOG(P,EFAC,LCOL,NCTRDC(K))
        END DO
      ELSE
C       ....................................... color = momentum cut
        CALL DPARGI(59,'CC1',ICOE1)
        CALL DPARGI(59,'CC2',ICOE2)
        CALL DPARGV(70,'FCC',2,ENCUT)
        DO K=1,N
          CALL DVTRTP(K,P)
          P=ABS(P)
          IF(P.LT.ENCUT) THEN
            NCTRDC(K)=ICOE1
          ELSE
            NCTRDC(K)=ICOE2
          END IF
        END DO
      END IF
C     ================================== SUPER-IMPOSE COLOR FROM MANUAL LIST
   80 DO I=1,N
        IF(COLRDT(I).GT.0.) THEN
          NCTRDC(I)=COLRDT(I)
        END IF
      END DO
      CALL DPARGI(50,'CAT',NCTRDC(MCTRDC))
   88 FVOLD=FVCOL
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DSCTR1
CH
      ENTRY DSCTR1
CH
      END
C
*DK DSETMI
CH..............+++
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSETMI
CH
      SUBROUTINE DSETMI
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C_CG 10-May-1989   C.Grab  Adapted to CERNVM
C
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      IF(NSTART.EQ.0) THEN
    1    NSTART=1
         CALL DQIN
         CALL DDATIN
         CALL DOPRIN
         CALL DLUTIN
         CALL DJBOP
      END IF
      RETURN
      END
*DK DSCMC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSCMC
CH
      SUBROUTINE DSCMC(ROFF,NS)
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
      DIMENSION PTRA(3),CLTM(3),XVER(3),XYZ(3)
      DATA ZSIC/250./
      DATA Z249/249./,Z264/264./
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_CMC'
      CALL DPARAM(40
     &  ,J_CMC)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(4,J_CMC).LE.0.) RETURN
      CALL DVMCNV(NVTX,NTRA)
      IF(NVTX.EQ.0) RETURN
      DLINDD=PDCODD(2,LITRDD)
      DO NV=1,NVTX
        CALL DVMCVX(NV,KTRA1,KTRA2,KTRIN,XVER)
        IF(ABS(XVER(3)).GT.Z249.AND.ABS(XVER(3)).LT.Z264) THEN
          CALL DVMCTR(KTRIN,PTRA,CLTM,ITYPE,NVR)
          ITYPE=ABS(ITYPE)
          IF(ITYPE.EQ.2.OR.ITYPE.EQ.3)
     &      CALL DSCSY(ROFF,NS,XVER,PTRA,ITYPE)
        ELSE
          DO K=KTRA1,KTRA2
            CALL DVMCTR(K,PTRA,CLTM,ITYPE,NVR)
            IF(ITYPE.EQ.1) THEN
              IF(PTRA(3).GT.0.) THEN
                QZ=(ZSIC-XVER(3))/PTRA(3)
              ELSE IF(PTRA(3).LT.0.) THEN
                QZ=(-ZSIC-XVER(3))/PTRA(3)
              END IF
              DO I=1,3
                XYZ(I)=XVER(I)+QZ*PTRA(I)
              END DO
              CALL DSCSY(ROFF,NS,XYZ,PTRA,ITYPE)
            END IF
          END DO
        END IF
      END DO
      END
*DK DSCHSY
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSCHSY
CH
      SUBROUTINE DSCSY(ROFF,NS,XYZ,PTRA,ITYP)
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
      CHARACTER *49 T
      CHARACTER *10 TTYP(3)
      CHARACTER *4 DT4
      CHARACTER *3 DT3
      DATA TTYP/'o:Gamma','+:Positron','x:Electron'/
      DIMENSION D360(3),XYZ(3),PTRA(3)
      DATA D360/0.,-360.,360./,LMC/8/
      LOGICAL FIN
C             123456789 123456789 123456789 123456789 123456789
      DATA T/'+:positron P=45.1 phi=345 rho=7.4 z=-252 (B)'/
      P=SQRT(PTRA(1)*PTRA(1)+PTRA(2)*PTRA(2)+PTRA(3)*PTRA(3))
      RO=SQRT(XYZ(1)*XYZ(1)+XYZ(2)*XYZ(2))
      FI=DATN2D(XYZ(2),XYZ(1))
      T( 1:10)=TTYP(ITYP)
      T(14:17)=DT4(P)
      T(23:25)=DT3(FI)
      T(31:33)=DT3(RO)
      T(37:40)=DT4(XYZ(3))
      IF(XYZ(3).GT.0.) THEN
        T(43:43)='A'
      ELSE
        T(43:43)='B'
      END IF
      T(47:49)='   '
      IF(NS.EQ.1.AND.XYZ(3).LT.0.) T(47:49)='out'
      IF(NS.EQ.2.AND.XYZ(3).GT.0.) T(47:49)='out'
      IF(RO.LT.ROFF) T(47:49)='out'
      CALL DWRT(T)
      IF(T(47:47).NE.' ') RETURN
      MF=1
      IF(ROFF.EQ.0.) THEN
C       ................................... ROFF=0. : flag for XY projection.
        HU=XYZ(1)
        VU=XYZ(2)
      ELSE
        HU=RO-ROFF
        IF(NS.EQ.0)THEN
          IF(XYZ(3).LT.0.) THEN
            FI=MOD(FI+180.,360.)
            HU=-HU
          END IF
          MF=3
        END IF
        VU=FI
      END IF
      CALL DGLEVL(LMC)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_CMC'
      CALL DPARAM(40
     &  ,J_CMC)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      DO M=1,MF
        CALL DQPOC(HU,VU+D360(M),HD,VD,FIN)
        IF(FIN) THEN
          CALL DQSET1(IAREDO,0.,0.)
          D=PARADA(2,J_CMC)
          IF     (ITYP.EQ.1) THEN
C           ............................................... 0:PHOTON = 1
            CALL DQL2E(HD-D,VD-D,HD+D,VD-D)
            CALL DQL2E(HD+D,VD-D,HD+D,VD+D)
            CALL DQL2E(HD+D,VD+D,HD-D,VD+D)
            CALL DQL2E(HD-D,VD+D,HD-D,VD-D)
          ELSE IF(ITYP.EQ.2) THEN
C           .............................................. +:POSITRON = 2
            CALL DQL2E(HD  ,VD-D,HD  ,VD+D)
            CALL DQL2E(HD-D,VD  ,HD+D,VD  )
          ELSE
C           .............................................. X:ELECTRON = 3
            CALL DQL2E(HD-D,VD-D,HD+D,VD+D)
            CALL DQL2E(HD-D,VD+D,HD+D,VD-D)
          END IF
          CALL DQSET(IAREDO,0.,0.)
        END IF
      END DO
      END
*DK DSCHST
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSCHST
CH
      SUBROUTINE DSCHST(JCOL)
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
      DIMENSION ENH(20,2),NMH(20,2),NPH(20,2),NCH(20,2),NW(20,2)
      DIMENSION NCL(2)
      DIMENSION JCOL(0:21),LCOL(0:6)
      DATA LCOL/10,14,13, 9,12,11,15/
      CHARACTER *2 TS(0:3)
      CHARACTER *49 THEA,TSCR
      DATA TS/'  ',' A',' B','AB'/
      CHARACTER *3 TC(0:15)
      DATA TC/'   ',' 1 ',' 2 ',' 3 ',' 4 ',' 5 ',' 6 ',' 7 ',
     &        'whi','gre','yel','ora','red','mag','cya','blu'/
C                123456789 123456789 123456789 123456789 123456789
      DATA THEA/'Cl. side  GeV  pads col   Cl. side  GeV  pads col'/
C                12   AB  12.34  12   12   12   AB  1234   12   12
      LOGICAL FNWRT
      FNWRT=.FALSE.
      CALL VZERO(ENH,40)
      CALL VZERO(NMH,40)
      CALL VZERO(NPH,40)
      CALL VZERO(NCH,40)
      CALL VZERO(JCOL,21)
      GO TO 1
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DSCHSC
CH
      ENTRY DSCHSC(JCOL)
CH
CH --------------------------------------------------------------------
CH
      FNWRT=.TRUE.
    1 CALL=DVSIC0(NUM)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_CE1,J_CE2,J_CCH'
      CALL DPARAM(40
     &  ,J_CE1,J_CE2,J_CCH)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(NUM.EQ.0) THEN
        CALL DWRT('Sical is empty.')
      ELSE
        IF(PARADA(4,J_CE1).GT.0.) THEN
          E1=PARADA(2,J_CE1)
        ELSE
          E1=0.
        END IF
        IF(PARADA(4,J_CE2).GT.0.) THEN
          E2=PARADA(2,J_CE2)
        ELSE
          E2=999.
        END IF
        NCL(1)=0
        NCL(2)=0
        DO K=1,NUM
          ES=DVSIC(IVENDV,K)
          IF(ES.GT.E1.AND.ES.LT.E2) THEN
            IAD=DVSIC(14,K)
            CALL SIDCOZ(IAD,NM,NZ,NF,NR)
            ICL=DVSIC(13,K)
            DO N=1,NCL(NM)
              IF(ICL.EQ.NCH(N,NM)) GO TO 926
            END DO
            NCL(NM)=MIN(NCL(NM)+1,20)
            N=NCL(NM)
            NCH(N,NM)=ICL
            NMH(N,NM)=NM
            ENH(N,NM)=0.
            NPH(N,NM)=0.
  926       ENH(N,NM)=ENH(N,NM)+ES
            NPH(N,NM)=NPH(N,NM)+1
            IF(NMH(N,NM).NE.NM) NMH(N,NM)=3
          END IF
        END DO
        CALL VZERO(NW,40)
        DO NM=1,2
          DO N1=1,NCL(NM)
            IF(NCH(N1,NM).GT.0) THEN
              DO N2=N1,NCL(NM)
                IF(NCH(N2,NM).GT.0) THEN
                  IF(NPH(N1,NM).LT.NPH(N2,NM)) THEN
                    NW(N1,NM)=NW(N1,NM)+1
                  ELSE
                    NW(N2,NM)=NW(N2,NM)+1
                  END IF
                END IF
              END DO
            END IF
          END DO
          DO N=1,NCL(NM)
            IF(PARADA(4,J_CCH).LE.0.) THEN
              JCOL(NCH(N,NM))=LCOL(NW(N,NM))
            ELSE
              JCOL(NCH(N,NM))=PARADA(2,J_CCH)
            END IF
          END DO
        END DO
        IF(FNWRT) RETURN
        CALL DWRT(THEA)
        DO N=1,MAX(NCL(1),NCL(2))
          WRITE(TSCR,1000) (NCH(N,NM),
     &                   TS(NMH(N,NM)),
     &                      ENH(N,NM),
     &                      NPH(N,NM),
     &              TC(JCOL(NCH(N,NM))),NM=2,1,-1)
 1000     FORMAT(I2,3X,A,F7.2,I4,2X,A,3X,I2,3X,A,F7.2,I4,2X,A)
          IF(N.GT.NCL(1)) TSCR(27:49)=' '
          IF(N.GT.NCL(2)) TSCR( 1:26)=' '
          CALL DWRT(TSCR)
        END DO
      END IF
      END
*DK DSCD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSCD
CH
      SUBROUTINE DSCD
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DATA PIDEG/57.29577951/
      CHARACTER *2 TPR,DT2
      CHARACTER *3 DT3,T3
      CHARACTER *35 T
      CHARACTER *2 TSID(0:2),T2
      DATA TSID/'AB','A ','B'/
      DATA DVS/6./,DHS/160./,DHSFZ/70./
      DIMENSION H(15),V(15),IM(12,2),JM(0:2),SM(1:3)
      DIMENSION XRO(9),YRO(9),ZRO(2),HRO(9,2),VRO(9,2)
      DATA IM/1,2,3, 1,2,3, 1,2,3, 1,2,3,
     &        3,2,1, 3,2,1, 3,2,1, 3,2,1/
      DATA JM/3,1,2/,SM/-2.,0.,2./
      DATA N14/14/,LS/8/,LR/12/,QQR/0.8/,QQF/0.8/,MO/1/
      DATA QQZ/0.7/,Q3/0.3333333/,QHST/1.1/,Q1/1.001/,DRO/0.01/
C      DATA NI/2/,DHD/3./,DVD/3./,J1/1/,J2/2/,QZM/2./,QRM/2./
      DATA QRM/2./
      DIMENSION ZM(2),DFZ(3),HRB(4),VRB(4),NFNM(2),ANG(2)
      DATA NFNM/0,16/,ANG/0.,0./,ZEPS/0.05/,DRR/0.26/
      DIMENSION ERZ(16,12,2),EFZ(32,12,2),EZH(12,2)
      DIMENSION EFR(0:99,16,2)
      DIMENSION IRZ(16,12,2),IFZ(32,12,2)
      DIMENSION IFR(0:99,16,2)
      LOGICAL FS(0:32,16,2),FRZ(16,2),FFZ(32,2)
      LOGICAL FFR(0:96,2)
      LOGICAL FMAX1,FMAX2,FIN,FCUTC,FCUTR
      CHARACTER *9 TNUM
      CHARACTER *4 TST
      DATA TST/'LIN'/,LA/7/
      DATA ZGAP/0.1/,WDIG/4.25/
      CHARACTER *6 TF(4:5)
      DATA TF/'(F4.1)','(F5.2)'/
C      DATA DFV/2./,DZH/2./,D1/3./,DL2/2./,DL1/1./,DS0/3./,DD2/3./
      DATA D1/3./,DL2/2./,DL1/1./,DS0/3./,DD2/3./
C      DATA QEN0/.8/,FIEN1/3./,N20/20/,N24/24/,DZNP/0.004/
      DATA QEN0/.8/,N20/20/,N24/24/,DZNP/0.004/
      DIMENSION I2PS(6)
C               1 2 3 4 5 6
      DATA I2PS/1,1,2,2,3,3/
      DIMENSION JCOL(0:21),NPRMX(0:20),EPMAX(0:20)
      DATA QPZ/1.01/,QPF/1.01/
      CHARACTER *1 TNR(16)
      DATA TNR/'1','2','3','4','5','6','7','8','9','0',
     &         '1','2','3','4','5','6'/
C      DATA DHTD/1./,DVTD/1./,VT1/4./,VT2/14./,VT3/24./,VT23/18./
      DATA DHTD/1./,VT1/4./,VT2/14./,VT3/24./,VT23/18./
      DIMENSION LENER(0:15),QDEAD(5:7)
      DATA LENER/8*8,12,12,12, 8, 8, 8,12, 8/,QDEAD/.5,.5,.1/
      DATA LAMBI/21/,LD7/7/,LD10/10/
      DIMENSION JRO(4)
      DATA JRO/1,4,5,8/,E0/.02/
      DATA CMPIX/32./
      DIMENSION NZT(27),NFT(27),NRT(27)
      DATA NZT/ 1, 2, 3,  1, 2, 3,  1, 2, 3,
     &          1, 2, 3,  1, 2, 3,  1, 2, 3,
     &          1, 2, 3,  1, 2, 3,  1, 2, 3/
      DATA NFT/31,31,31, 31,31,32, 31,32,32,
     &         32,32,32, 32,32, 1, 32, 1, 1,
     &          1, 1, 1,  1, 1, 2,  1, 2, 2/
      DATA NRT/ 1, 1, 1,  2, 2, 2,  4, 4, 4,
     &          6, 6, 6,  8, 8, 8, 10,10,10,
     &         12,12,12, 14,14,14, 16,16,16/
      DATA NMT/1/,EST/1./
      DATA RLN/0.4/,HLN/.1/,VLD/6./,DTD/41./
C      DATA LDEB/0/,KDEB/0/,MDEB/0/
      DATA KDEB/0/,MDEB/0/
      DATA DM1/0./
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CALL DQCL(IAREDO)
      CALL DPARGV(40,'CPR',2,CPR2)
      NPR=MAX(1.,CPR2)
      IF(NPR.GE.5.AND.NPR.LE.8) IZOMDO=1
      BORD=MOD(DFWIDU(IZOMDO),10.)
      CALL DQWIL(BORD)
      GO TO 1
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DSCD1
CH
      ENTRY DSCD1(TPR)
CH
CH --------------------------------------------------------------------
CH
      NPR=0
      IF(TPR.EQ.'RO') THEN
        CALL DPARGV(14,'PAB',2,PAB)
        IF(PAB.EQ.0.) THEN
          CALL DWRT(' select endcap A or B.')
          RETURN
        END IF
        NPR=20
      END IF
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
    1 TPARDA=
     &  'J_PFI,J_PDF,J_PTE,J_PAB,J_RAL,J_RES,J_REY'
      CALL DPARAM(10
     &  ,J_PFI,J_PDF,J_PTE,J_PAB,J_RAL,J_RES,J_REY)
      TPARDA=
     &  'J_CFL,J_CTL,J_CSX,J_CSR,J_CSH,J_CSF,J_CFH,J_CFM,J_CE1,J_CE2'
      CALL DPARAM(40
     &  ,J_CFL,J_CTL,J_CSX,J_CSR,J_CSH,J_CSF,J_CFH,J_CFM,J_CE1,J_CE2)
      TPARDA=
     &  'J_CCN,J_CNR,J_CNP,J_CCG,J_CSY,J_CZZ'
      CALL DPARAM(40
     &  ,J_CCN,J_CNR,J_CNP,J_CCG,J_CSY,J_CZZ)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(MDEB.EQ.0) THEN
        CALL=DVSIC0(NUM)
      ELSE
        NUM=27
      END IF
      MO=PARADA(2,J_CSY)
      NS=PARADA(2,J_PAB)
      IF(NS.EQ.-1) NS=2
      LA=PDCODD(2,LCSADD)
      LR=PDCODD(2,LCSRDD)
      LS=PDCODD(2,LCSSDD)
      LH=PDCODD(2,LCSHDD)
      LL=PDCODD(2,LCSLDD)
      JCOL(21)=LS
      IF(PDCODD(4,LCSADD).LT.0.) THEN
        TST='LIN'
      ELSE
        TST='A-L'
      END IF
      IF(PARADA(4,J_CE1).GT.0.) THEN
        E1=PARADA(2,J_CE1)
      ELSE
        E1=0.
      END IF
      IF(PARADA(4,J_CE2).GT.0.) THEN
        E2=PARADA(2,J_CE2)
      ELSE
        E2=999.
      END IF
      IF(PARADA(4,J_CCN).GT.0.) THEN
        CUTCL=PARADA(2,J_CCN)
        FCUTC=.TRUE.
      ELSE
        FCUTC=.FALSE.
      END IF
      IF(PARADA(4,J_CNR).GT.0.) THEN
        NRCUT=PARADA(2,J_CNR)
        FCUTR=.TRUE.
      ELSE
        FCUTR=.FALSE.
      END IF
      IF(IRUNDE(1).LT.2000) THEN
        MAB=1
        NAB=1
      ELSE IF(IRUNDE(1).LT.14850) THEN
        MAB=2
        NAB=-1
      ELSE
        MAB=1
        NAB=1
      END IF
      CALL DSCHSC(JCOL)
      CALL DGRSIC(DZ,DF,DR,RMIN)
C     ===================================================================== FR
      IF(NPR.EQ.2) THEN
        L1=PARADA(2,J_CFL)
        L2=PARADA(2,J_CTL)
        IF(L2.EQ.0) THEN
          L1=1
          L2=12
        ELSE
          IF(L1.EQ.0) L1=1
          IF(PARADA(4,J_CFL).GT.0.) THEN
            IF(PARADA(4,J_CTL).LT.0.) L2=L1
          ELSE
            IF(PARADA(4,J_CTL).GT.0.) THEN
              L1=L2
            ELSE
              L1=1
              L2=12
            END IF
          END IF
        END IF
        CALL VZERO(EFR,3200)
        CALL VZERO(FS,1056)
        CALL VZERO(FFR,194)
C       ...................................... FR find EMAX, and frame pattern
        EMAX=0.
        DO 801 K=1,NUM
          IF(MDEB.EQ.0) THEN
            IAD=DVSIC(N14,K)
            CALL SIDCOZ(IAD,NM,NZ,NF,NR)
            ES=DVSIC(IVENDV,K)
          ELSE
            NM=NMT
            NZ=NZT(K)
            NF=NFT(K)
            NR=NRT(K)
            ES=EST
          END IF
          IF(ES.GT.E1.AND.ES.LT.E2) THEN
            IF(FCUTC) THEN
              CLU=DVSIC(13,K)
              IF(CLU.NE.CUTCL) GO TO 801
            END IF
            ES=ES*Q3
            KF=3*NF-IM(NZ,MAB)
            DO I=0,2
              KFI=KF+I
              IF     (KFI.LE. 0) THEN
                KFI=96
              ELSE IF(KFI.GT.96) THEN
                KFI=1
              END IF
              NF1=(KFI+2)/3
              FS(NF1,NR,NM)=.TRUE.
              FFR(NF1,NM)=.TRUE.
              EFR(KFI,NR,NM)=EFR(KFI,NR,NM)+ES
              IF(EFR(KFI,NR,NM).GT.EMAX) THEN
                EMAX=EFR(KFI,NR,NM)
                NRMAX=NR
                NFMAX=NF+NFNM(NM)
              END IF
            END DO
          END IF
  801   CONTINUE
C       IF(EMAX.LE.0.) GO TO 99
        IF(PARADA(4,J_CSX).GT.0..OR.EMAX.LE.0.) THEN
          EMAX=PARADA(2,J_CSX)
        ELSE
          PARADA(2,J_CSX)=EMAX
        END IF
        DLINDD=PDCODD(2,LIDTDD)
C         .................................................... FR Set up range.
        CALL SIIDPO(1,1,1,16,ZDUM,FDUM,RMAX)
        ROFF=RMIN-ZGAP
        RTOF=RMAX-ROFF+ZGAP
        IF(PARADA(4,J_CFM).GT.0..OR.
     &     PARADA(4,J_CNR).GT.0.) THEN
          F0=PARADA(2,J_CFM)
        ELSE
          IF(NS.EQ.2) NFMAX=NFMAX-16
          F0=2.*(NFMAX-1.)*DF+DF
          PARADA(2,J_CFM)=F0
        END IF
        IF(IZOMDO.EQ.0) THEN
          DFRU=180.
        ELSE
          DFRU=PARADA(2,J_PDF)
        END IF
        F0=MOD(F0+3600.,360.)
        F1=F0-DFRU
        F2=F0+DFRU
C        IF(       F1.LT.0.) THEN
C          F0=F0+360.
C          F1=F1+360.
C          F2=F2+360.
C        ELSE IF(F1.GT.360.) THEN
C          F0=F0-360.
C          F1=F1-360.
C          F2=F2-360.
C        END IF
        IF(NS.EQ.0) THEN
          CALL DQRER(0,-RTOF,F1,RTOF,F2,HRB,VRB)
        ELSE
          CALL DQRER(0,   0.,F1,RTOF,F2,HRB,VRB)
        END IF
        CALL DQRU(HRB,VRB)
        IF(MO.EQ.3.OR.MO.EQ.4) THEN
          CALL DQPOC(0.,0.,HR1,VR,FIN)
          CALL DQPOC(DR,0.,HR2,VR,FIN)
          NDIG=ABS(HR2-HR1)/WDIG
          NDIG=MIN(MAX(1,NDIG),5)
        END IF
        CALL DODSIC('ZFR')
C       ........................................... FR draw all frame lines
        N1=1
        N2=2
        IF(NS.EQ.2) N1=2
        IF(NS.EQ.1) N2=1
        IF(PARADA(4,J_CCG).GT.0.) THEN
          LCG=PARADA(2,J_CCG)
          CALL DQPO0('LINE',LA,LCG,' ')
C          CALL DQPO0(TST,LA,LCG,' ')
          DLINDD=PDCODD(2,LIGLDD)
          DO NM=N1,N2
            DO NF=1,32
              DO NR=1,16
                CALL SIIDPO(NM,2,NF,NR,ZZ,FI,RO)
                RO=RO-ROFF
                IF(NS.EQ.0.AND.NM.EQ.2) THEN
                  FI=MOD(FI+180.,360.)
                  RO=-RO
                END IF
                FI=DFINXT(F0,FI)
                H(1)=RO-DR
                H(3)=RO+DR
                V(1)=FI-DF
                V(3)=FI+DF
                H(2)=H(3)
                H(4)=H(1)
                V(2)=V(1)
                V(4)=V(3)
                H(5)=H(1)
                V(5)=V(1)
                CALL DQPOL(5,H,V)
              END DO
            END DO
          END DO
        END IF
C       .............................................. FR Draw line at maximum
        CALL DQLEVL(LCSRDD)
        IF(NRMAX.GT.1.AND.NRMAX.LT.12) THEN
          CALL SIIDPO(1,2,NFMAX,NRMAX,ZZ,FI,RO)
          RO=RO-DR-ROFF
          CALL DQL2E( RO,F1, RO,F2)
          CALL DQL2E(-RO,F1,-RO,F2)
        END IF
C       ........................................................ FR draw frames
C        N1=1
C        N2=2
C        IF(NS.EQ.2) N1=2
C        IF(NS.EQ.1) N2=1
        CALL DQLEVL(LCSRDD)
        CALL DQPO0(TST,LA,LR,' ')
        DLINDD=PDCODD(2,LIGLDD)
        DO NM=N1,N2
          DO NF=1,32
            IF(FFR(NF,NM)) THEN
              DO NR=1,16
                IF(FS(NF,NR,NM)) THEN
                  CALL SIIDPO(NM,2,NF,NR,ZZ,FI,RO)
                  RO=RO-ROFF
                  IF(NS.EQ.0.AND.NM.EQ.2) THEN
                    FI=MOD(FI+180.,360.)
                    RO=-RO
                  END IF
                  FI=DFINXT(F0,FI)
                  H(1)=RO-DR
                  H(3)=RO+DR
                  V(1)=FI-DF
                  V(3)=FI+DF
                  H(2)=H(3)
                  H(4)=H(1)
                  V(2)=V(1)
                  V(4)=V(3)
                  H(5)=H(1)
                  V(5)=V(1)
                  CALL DQPOL(5,H,V)
                END IF
              END DO
            END IF
          END DO
        END DO
C       .......................................................... FR: DRAW NF
        IF(BORD.EQ.1.) THEN
          IF(NS.NE.0) THEN
            CALL DQLEVL(ICTXDD)
            DO NF=1,32
              IF(FFR(NF,NS)) THEN
                CALL SIIDPO(NS,2,NF,1,ZZ,FI,RO)
                FI=DFINXT(F0,FI)
                WRITE(T2,1010) NF
 1010           FORMAT(I2)
                CALL DQPOC(HLN,FI,HTD,VTD,FIN)
                IF(FIN) CALL DGTEXT(HTD-DTD,VTD,T2,2)
              END IF
            END DO
            CALL DQINV(IAREDO,0.,VLOWDG(IAREDO)+VLD,HDUM,FILOW)
            DO NR=1,16
              CALL SIIDPO(NS,1,1,NR,ZZ,FI,RO)
              RO=RO-ROFF-DRR
              CALL DQPOC(RO,FILOW,HTD,VTD,FIN)
              CALL DGTEXT(HTD,VMINDG(IAREDO)+VT2,TNR(NR),1)
            END DO
          END IF
        END IF
C       ................................... FR sum energy for selected layers.
        CALL VZERO(EFR,3200)
        CALL VZERO(FFR,194)
        CALL VZERO(IFR,3200)
        DO 700 K=1,NUM
          IF(MDEB.EQ.0) THEN
            IAD=DVSIC(N14,K)
            CALL SIDCOZ(IAD,NM,NZ,NF,NR)
            ES=DVSIC(IVENDV,K)
          ELSE
            NM=NMT
            NZ=NZT(K)
            NF=NFT(K)
            NR=NRT(K)
            ES=EST
          END IF
          IF(ES.GT.E1.AND.ES.LT.E2) THEN
            ES=ES*Q3
            IF(NZ.LT.L1.OR.NZ.GT.L2) GO TO 700
            CLU=DVSIC(13,K)
            IF(FCUTC.AND.CLU.NE.CUTCL) GO TO 700
            KF=3*NF-IM(NZ,MAB)
            DO I=0,2
              KFI=KF+I
              IF     (KFI.LE. 0) THEN
                KFI=96
              ELSE IF(KFI.GT.96) THEN
                KFI=1
              END IF
              FFR(KFI,NM)=.TRUE.
              EFR(KFI,NR,NM)=EFR(KFI,NR,NM)+ES
              KCL=CLU
              IF(IFR(KFI,NR,NM).EQ.0) THEN
                IFR(KFI,NR,NM)=KCL
              ELSE IF(KCL.NE.0.AND.IFR(KFI,NR,NM).NE.KCL) THEN
                IFR(KFI,NR,NM)=LAMBI
              END IF
            END DO
          END IF
  700   CONTINUE
C       ...................................................... FR draw energy.
        D3=DF/3.
        IF(MO.EQ.1) THEN
          SEMAX=SQRT(EMAX)
          QF=QQF/SEMAX
          D3QF=D3*QF
          QR=QQR/SEMAX
        ELSE
          QR=QQR/EMAX
        END IF
        DRQR=DR*QR
        DRMAX=QQR*DR*Q1
        DFMAX=QQF*D3*Q1
        R1=RMIN+DR
        PEMX=0.
        CEMX=0.
        ICL=99
        DO NM=N1,N2
          DO KF=1,96
            IF(FFR(KF,NM)) THEN
              NL=JM(MOD(KF,3))
              SS=SM(NL)
              NF=(KF+2)/3
              DO NR=1,16
                IF(EFR(KF,NR,NM).GT.0.) THEN
                  IF(ICL.NE.IFR(KF,NR,NM)) THEN
                    ICL=IFR(KF,NR,NM)
                    IF(MO.EQ.3.OR.MO.EQ.4) THEN
                      CALL DGLEVL(JCOL(ICL))
                    ELSE
                      CALL DQRA1(2,JCOL(ICL))
                    END IF
                  END IF
                  CALL SIIDPO(1,2,NF,NR,ZZ,FI,RO)
                  FIF=FI+SS*D3
                  RO=RO-ROFF
                  IF(NM.EQ.2.AND.NS.EQ.0) THEN
                    FIF=MOD(FIF+180.,360.)
                    RO=-RO
                  END IF
                  FIF=DFINXT(F0,FIF)
                  IF(MO.EQ.3) THEN
                    CALL DTN(EFR(KF,NR,NM),NDIG,TNUM)
                    CALL DQTXT(RO-DR,FIF,TNUM,NDIG)
                  ELSE IF(MO.EQ.4) THEN
                    IF(NDIG.LT.4) THEN
                      CALL DTN(EFR(KF,NR,NM),NDIG,TNUM)
                      CALL DQTXT(RO-DR,FIF,TNUM,NDIG)
                    ELSE
                      WRITE(TNUM,TF(NDIG)) EFR(KF,NR,NM)
                      IF(TNUM(1:2).EQ.' 0') TNUM(1:2)='  '
                      CALL DQTXT(RO-DR,FIF,TNUM,NDIG)
                    END IF
                  ELSE
                    IF(MO.EQ.1) THEN
                      SEXY=SQRT(EFR(KF,NR,NM))
                      DRE=DRQR*SEXY
                      DFE=D3QF*SEXY
                      IF(DFE.LE.DFMAX) THEN
                        FMAX2=.FALSE.
                      ELSE
                        FMAX2=.TRUE.
                        DFE=DFMAX
                      END IF
                    ELSE
                      DRE=DRQR*EFR(KF,NR,NM)
                      DFE=DFMAX
                      FMAX2=.FALSE.
                    END IF
                    IF(DRE.LE.DRMAX) THEN
                      FMAX1=.FALSE.
                    ELSE
                      FMAX1=.TRUE.
                      DRE=DRMAX
                    END IF
                    H(1)=RO-DRE
                    H(3)=RO+DRE
                    V(1)=FIF-DFE
                    V(3)=FIF+DFE
                    H(2)=H(3)
                    H(4)=H(1)
                    V(2)=V(1)
                    V(4)=V(3)
                    CALL DQRAR(H,V)
                    CALL DQPOC(RO,FIF,HDUM,VDUM,FIN)
                    IF(FMAX1.OR.FMAX2) THEN
                      CALL DGLEVS(N1)
                      CALL DQL2E(H(1),V(1),H(3),V(3))
                      CALL DGLEVS(0)
                      IF(FIN) CEMX=MAX(CEMX,EFR(KF,NR,NM))
                    ELSE
                      IF(FIN) PEMX=MAX(PEMX,EFR(KF,NR,NM))
                    END IF
                  END IF
                END IF
              END DO
            END IF
          END DO
        END DO
        CALL DSCMC(ROFF,NS)
C       ........................................................ FR draw scale
C          123456789 123456789
        T='Layer 12-12 side AB'
        T( 7: 8)=DT2(FLOAT(L1))
        T(10:11)=DT2(FLOAT(L2))
        T(18:19)=TSID(NS)
        CALL DQLEVL(ICTXDD)
        CALL DGTEXT(HHGHDG(IAREDO)-DHS,VMINDG(IAREDO)+DVS,T,19)
        CALL DQSCA('V',VRB(2),VRB(3),' deg',4,'&f',2)
        IF(NS.EQ.0) CALL DQSCA('H',HRB(1),HRB(2),'cm',2,'d&r',3)
      ELSE IF(NPR.LT.2.OR.NPR.EQ.9) THEN
        CALL DSCXY(NPR,NUM)
        GO TO 99
C     ==================================================================== RZ
      ELSE IF(NPR.EQ.3) THEN
C       .......................................................RZ store energy
        EMAX=0.
        EMXH=0.
        CALL VZERO(ERZ,384)
        CALL VZERO(FRZ,32)
        CALL VZERO(EZH,24)
        CALL VZERO(IRZ,384)
        DO 803 K=1,NUM
          IAD=DVSIC(N14,K)
          CALL SIDCOZ(IAD,NM,NZ,NF,NR)
          ES=DVSIC(IVENDV,K)
          IF(ES.GT.E1.AND.ES.LT.E2) THEN
            CLU=DVSIC(13,K)
            IF(FCUTC.AND.CLU.NE.CUTCL) GO TO 803
            FRZ(NR,NM)=.TRUE.
            ERZ(NR,NZ,NM)=ERZ(NR,NZ,NM)+ES
            EZH(   NZ,NM)=EZH(   NZ,NM)+ES
            EMAX=MAX(ERZ(NR,NZ,NM),EMAX)
            EMXH=MAX(EZH(   NZ,NM),EMXH)
            KCL=CLU
            IF(IRZ(NR,NZ,NM).EQ.0) THEN
              IRZ(NR,NZ,NM)=KCL
            ELSE IF(KCL.NE.0.AND.IRZ(NR,NZ,NM).NE.KCL) THEN
              IRZ(NR,NZ,NM)=LAMBI
            END IF
          END IF
  803   CONTINUE
C       IF(EMAX.LE.0.) GO TO 99
C       .................................................... RZ DRAW HISTOGRAM
        CALL SIIDPO(1, 1,1, 1,ZMIN,FI,RMIN)
        CALL SIIDPO(1,12,1,16,ZMAX,FI,RMAX)
        ZM(1)=ZMIN-ZGAP-DZ
        ZM(2)=-ZM(1)
        ZHGH=ZMAX-ZMIN+2.*(ZGAP+DZ)
        Z1=-ZHGH
        Z2= ZHGH
        IF(NS.EQ.1) Z1=0.
        IF(NS.EQ.2) Z2=ZEPS
        VHGH=VHGHDG(IAREDO)
        QH=PARADA(2,J_CFH)*0.01
        IF(QH.GT.0..AND.PARADA(4,J_CFH).GT.0.) THEN
          DLINDD=PDCODD(2,LIGLDD)
          VLOW=VLOWDG(IAREDO)
          VHST=VHGH-QH*(VHGH-VLOW)
          VLOWDG(IAREDO)=VHST
          IF(PARADA(4,J_CSH).GT.0.) THEN
            EMXH=PARADA(2,J_CSH)
          ELSE
            PARADA(2,J_CSH)=EMXH
          END IF
          VHE=EMXH*QHST
          CALL DQRER(0,Z1,0.,Z2,VHE,HRB,VRB)
          CALL DQRU(HRB,VRB)
          CALL DQLEVL(ICFRDD)
          CALL DQL2E(-ZHGH,0.,ZHGH,0.)
          CALL DQPO0(TST,LH,LL,' ')
          V(1)=0.
          V(2)=0.
          V(5)=0.
          DO NM=1,2
            DO NZ=1,12
              IF(EZH(NZ,NM).GT.0.) THEN
                CALL SIIDPO(NM,NZ,1,1,ZZ,FI,RO)
                ZZ=ZZ-ZM(NM)
                H(1)=ZZ-DZ
                H(3)=ZZ+DZ
                V(3)=EZH(NZ,NM)
                H(2)=H(3)
                H(4)=H(1)
                V(4)=V(3)
                H(5)=H(1)
                CALL DQPOL(5,H,V)
              END IF
            END DO
          END DO
          CALL DQSCA('V',VRB(2),VRB(3),'GeV',3,' ',0)
          CALL DQFR(IAREDO)
          VLOWDG(IAREDO)=VLOW
          VHGHDG(IAREDO)=VHST
          IF(QH.GT.0.97) GO TO 39
        END IF
C       ................................................. RZ SET UP USER RANGE
        RMIN=RMIN-QRM*DR
        RMAX=RMAX+QRM*DR
        IF(NS.EQ.0) THEN
          RMI=RMIN
        ELSE
          RMI=RMIN-RLN
        END IF
        CALL DQRER(0,Z1,RMI,Z2,RMAX,HRB,VRB)
        CALL DQRU(HRB,VRB)
        IF(MO.EQ.3.OR.MO.EQ.4) THEN
          CALL DQPOC(0.,0.,HR1,VR,FIN)
          CALL DQPOC(DZ,0.,HR2,VR,FIN)
          NDIG=ABS(HR2-HR1)/WDIG
          NDIG=MIN(MAX(1,NDIG),5)
        END IF
        CALL DODSIC('ZRZ')
        IF(PARADA(4,J_CSR).GT.0..OR.EMAX.LE.0.) THEN
          EMAX=PARADA(2,J_CSR)
        ELSE
          PARADA(2,J_CSR)=EMAX
        END IF
        CALL SIIDPO(1,1,1,1,ZMIN,FI,RO)
        DLINDD=PDCODD(2,LIGLDD)
C       ........................................... RZ draw all frame lines
        IF(PARADA(4,J_CCG).GT.0.) THEN
          LCG=PARADA(2,J_CCG)
          CALL DQPO0('LINE',LA,LCG,' ')
          DLINDD=PDCODD(2,LIGLDD)
          DO NM=1,2
            DO NR=1,16
              DO NZ=1,12
                CALL SIIDPO(NM,NZ,1,NR,ZZ,FI,RO)
                ZZ=ZZ-ZM(NM)
                H(1)=ZZ-DZ
                H(3)=ZZ+DZ
                V(1)=RO-DR
                V(3)=RO+DR
                H(2)=H(3)
                H(4)=H(1)
                V(2)=V(1)
                V(4)=V(3)
                H(5)=H(1)
                V(5)=V(1)
                CALL DQPOL(5,H,V)
              END DO
            END DO
          END DO
        END IF
C       ........................................................ RZ DRAW FRAME
        CALL DQPO0(TST,LA,LR,' ')
        DO NM=1,2
          DO NR=1,16
            IF(FRZ(NR,NM)) THEN
              DO NZ=1,12
                IF(ERZ(NR,NZ,NM).GT.0.) THEN
                  CALL SIIDPO(NM,NZ,1,NR,ZZ,FI,RO)
                  ZZ=ZZ-ZM(NM)
                  H(1)=ZZ-DZ
                  H(3)=ZZ+DZ
                  V(1)=RO-DR
                  V(3)=RO+DR
                  H(2)=H(3)
                  H(4)=H(1)
                  V(2)=V(1)
                  V(4)=V(3)
                  H(5)=H(1)
                  V(5)=V(1)
                  CALL DQPOL(5,H,V)
                END IF
              END DO
            END IF
          END DO
        END DO
C       .........................................................RZ: DRAW NR,NZ
        IF(NS.NE.0) THEN
          IF(BORD.EQ.1.) THEN
            CALL DQLEVL(ICTXDD)
            DO NR=1,16
              IF(FRZ(NR,NS)) THEN
                CALL SIIDPO(NS,2,1,NR,ZZ,FI,RO)
                CALL DQPOC(Z1+HLN,RO,HTD,VTD,FIN)
                WRITE(T2,1010) NR
                CALL DGTEXT(HMINDG(IAREDO)+DHTD,VTD,T2,2)
              END IF
            END DO
            CALL DQINV(IAREDO,0.,VLOWDG(IAREDO)+VLD,HDUM,ROLOW)
            ZNL=ZM(NS)+DZ
            DO NZ=1,12
              CALL SIIDPO(NS,NZ,1,1,ZZ,FI,RO)
              ZZ=ZZ-ZNL
              WRITE(T2,1010) NZ
              CALL DQTXT(ZZ,ROLOW,T2,2)
            END DO
          END IF
        END IF
C       ........................................................RZ: DRAW ENERGY
        IF(MO.EQ.1) THEN
          SEMAX=SQRT(EMAX)
          QR=QQR/SEMAX
          QZ=QQZ/SEMAX
          DZQZ=DZ*QZ
        ELSE
          QR=QQR/EMAX
        END IF
        DRQR=DR*QR
        DRMAX=QQR*DR*Q1
        DZMAX=QQF*DZ*Q1
        CEMX=0.
        PEMX=EMAX
        ICL=99
        DO NM=1,2
          DO NR=1,16
            IF(FRZ(NR,NM)) THEN
              DO NZ=1,12
                IF(ERZ(NR,NZ,NM).GT.0.) THEN
                  IF(ICL.NE.IRZ(NR,NZ,NM)) THEN
                    ICL=IRZ(NR,NZ,NM)
                    IF(MO.EQ.3.OR.MO.EQ.4) THEN
                      CALL DGLEVL(JCOL(ICL))
                    ELSE
                      CALL DQRA1(2,JCOL(ICL))
                    END IF
                  END IF
                  CALL SIIDPO(NM,NZ,1,NR,ZZ,FI,RO)
                  ZZ=ZZ-ZM(NM)
                  IF(MO.EQ.3) THEN
                    CALL DTN(ERZ(NR,NZ,NM),NDIG,TNUM)
                    CALL DQTXT(ZZ-DZ,RO,TNUM,NDIG)
                  ELSE IF(MO.EQ.4) THEN
                    IF(NDIG.LT.4) THEN
                      CALL DTN(ERZ(NR,NZ,NM),NDIG,TNUM)
                      CALL DQTXT(ZZ-DZ,RO,TNUM,NDIG)
                    ELSE
                      WRITE(TNUM,TF(NDIG)) ERZ(NR,NZ,NM)
                      IF(TNUM(1:2).EQ.' 0') TNUM(1:2)='  '
                      CALL DQTXT(ZZ-DZ,RO,TNUM,NDIG)
                    END IF
                  ELSE
                    IF(MO.EQ.1) THEN
                      SERZ=SQRT(ERZ(NR,NZ,NM))
                      DRE=DRQR*SERZ
                      DZE=DZQZ*SERZ
                      IF(DZE.LE.DZMAX) THEN
                        FMAX2=.FALSE.
                      ELSE
                        FMAX2=.TRUE.
                        DFE=DFMAX
                      END IF
                    ELSE
                      DRE=DRQR*ERZ(NR,NZ,NM)
                      DZE=DZMAX
                      FMAX2=.FALSE.
                    END IF
                    IF(DRE.LE.DRMAX) THEN
                      FMAX1=.FALSE.
                    ELSE
                      FMAX1=.TRUE.
                      DRE=DRMAX
                    END IF
                    H(1)=ZZ-DZE
                    H(3)=ZZ+DZE
                    V(1)=RO-DRE
                    V(3)=RO+DRE
                    H(2)=H(3)
                    H(4)=H(1)
                    V(2)=V(1)
                    V(4)=V(3)
                    CALL DQRAR(H,V)
                    IF(FMAX1.OR.FMAX2) THEN
                      CALL DGLEVS(N1)
                      CALL DQL2E(H(1),V(1),H(3),V(3))
                      CALL DGLEVS(0)
                      CEMX=MAX(CEMX,ERZ(NR,NZ,NM))
                    END IF
                  END IF
                END IF
              END DO
            END IF
          END DO
        END DO
        CALL DQSCA('V',VRB(2),VRB(3),'cm',2,'&r',2)
   39   CALL DQSCA('H',HRB(1),HRB(2),'cm',2, 'Z',1)
C          123456789
        T='side AB'
        T(6:7)=TSID(NS)
        CALL DQLEVL(ICTXDD)
        CALL DGTEXT(HHGHDG(IAREDO)-DHSFZ,VMINDG(IAREDO)+VT1,T,19)
        VHGHDG(IAREDO)=VHGH
C     ==================================================================== FZ
      ELSE IF(NPR.EQ.4) THEN
C       .......................................................FZ store energy
        CALL VZERO(EFZ,768)
        CALL VZERO(FFZ,64)
        CALL VZERO(IFZ,768)
        DO 804 K=1,NUM
          IAD=DVSIC(N14,K)
          CALL SIDCOZ(IAD,NM,NZ,NF,NR)
          ES=DVSIC(IVENDV,K)
          IF(ES.GT.E1.AND.ES.LT.E2) THEN
            CLU=DVSIC(13,K)
            IF(FCUTC.AND.CLU.NE.CUTCL) GO TO 804
            IF(FCUTR.AND.NR.NE.NRCUT) GO TO 804
            FFZ(NF,NM)=.TRUE.
            EFZ(NF,NZ,NM)=EFZ(NF,NZ,NM)+ES
            KCL=CLU
            IF(IFZ(NF,NZ,NM).EQ.0) THEN
              IFZ(NF,NZ,NM)=KCL
            ELSE IF(KCL.NE.0.AND.IFZ(NF,NZ,NM).NE.KCL) THEN
              IFZ(NF,NZ,NM)=LAMBI
            END IF
          END IF
  804   CONTINUE
C       ..........................................................FZ find EMAX
        EMAX=0.
        DO NM=1,2
          DO NF=1,32
            IF(FFZ(NF,NM)) THEN
              DO NZ=1,12
                IF(EFZ(NF,NZ,NM).GT.EMAX) THEN
                  EMAX=EFZ(NF,NZ,NM)
                  NFMAX=NF+NFNM(NM)
                END IF
              END DO
            END IF
          END DO
        END DO
C       IF(EMAX.LE.0.) GO TO 99
        IF(PARADA(4,J_CSF).GT.0..OR.EMAX.LE.0.) THEN
          EMAX=3.*PARADA(2,J_CSF)
        ELSE
          PARADA(2,J_CSF)=EMAX/3.
        END IF
C       .................................................. FZ SET UP USER RANGE
        CALL SIIDPO(1, 1,1, 1,ZMIN,FI,RO)
        CALL SIIDPO(1,12,1,16,ZMAX,FI,RO)
        ZHGH=ZMAX-ZMIN+2.*(ZGAP+DZ)
        IF(PARADA(4,J_CFM).GT.0..OR.
     &     PARADA(4,J_CNR).GT.0.) THEN
          F0=PARADA(2,J_CFM)
        ELSE
          IF(NS.EQ.2) NFMAX=NFMAX-16
          F0=2.*(NFMAX-1.)*DF+DF
          PARADA(2,J_CFM)=F0
        END IF
        IF(IZOMDO.EQ.0) THEN
          DFRU=180.
        ELSE
          DFRU=PARADA(2,J_PDF)
        END IF
        F0=MOD(F0+3600.,360.)
        F1=F0-DFRU
        F2=F0+DFRU
C        IF(       F0.LT.0.) THEN
C          F0=F0+360.
C          F1=F1+360.
C          F2=F2+360.
C        ELSE IF(F0.GT.360.) THEN
C          F0=F0-360.
C          F1=F1-360.
C          F2=F2-360.
C        END IF
        Z1=-ZHGH
        Z2= ZHGH
        IF(NS.EQ.1) Z1=0.
        IF(NS.EQ.2) Z2=ZEPS
        CALL DQRER(0,Z1,F1,Z2,F2,HRB,VRB)
        CALL DQRU(HRB,VRB)
        IF(MO.EQ.3.OR.MO.EQ.4) THEN
          CALL DQPOC(0.,0.,HR1,VR,FIN)
          CALL DQPOC(DZ,0.,HR2,VR,FIN)
          NDIG=ABS(HR2-HR1)/WDIG
          NDIG=MIN(MAX(1,NDIG),5)
        END IF
        CALL DODSIC('ZFZ')
        IF(PARADA(4,J_CCG).GT.0.) THEN
          LCG=PARADA(2,J_CCG)
          CALL DGLEVL(LCG)
          D2Z=2.*DZ
          ZM(1)=ZMIN-ZGAP-D2Z
          IF(NS.EQ.0.OR.NS.EQ.1) THEN
            DO NZ=1,12
              CALL SIIDPO(1,NZ,1,1,Z,FI,RO)
              ZZ=Z-ZM(1)
              CALL DQL2E(ZZ,F1,ZZ,F2)
              DO NF=1,32
                CALL SIIDPO(1,NZ,NF,1,Z,FI,RO)
                FI=DFINXT(F0,FI+DF)
                CALL DQL2E(ZZ-D2Z,FI,ZZ,FI)
              END DO
            END DO
          END IF
          IF(NS.EQ.0.OR.NS.EQ.2) THEN
            DO NZ=1,12
              CALL SIIDPO(2,NZ,1,1,Z,FI,RO)
              ZZ=Z+ZM(1)
              CALL DQL2E(ZZ,F1,ZZ,F2)
              DO NF=1,32
                CALL SIIDPO(1,NZ,NF,1,Z,FI,RO)
                FI=DFINXT(F0,FI+DF)
                CALL DQL2E(ZZ+D2Z,FI,ZZ,FI)
              END DO
            END DO
          END IF
        END IF
C       ........................................................FZ: DRAW FRAME
        ZM(1)=ZMIN-ZGAP-DZ
        ZM(2)=-ZM(1)
        CALL DQPO0(TST,LA,LR,' ')
        DLINDD=PDCODD(2,LIGLDD)
        DO NM=1,2
          DO NF=1,32
            IF(FFZ(NF,NM)) THEN
              DO NZ=1,12
                IF(EFZ(NF,NZ,NM).GT.0.) THEN
                  CALL SIIDPO(NM,NZ,NF,1,ZZ,FI,RO)
                  ZZ=ZZ-ZM(NM)
                  IF(NS.EQ.0.AND.NM.EQ.2) FI=MOD(FI+180.,360.)
                  FI=DFINXT(F0,FI)
                  H(1)=ZZ-DZ
                  H(3)=ZZ+DZ
                  V(1)=FI-DF
                  V(3)=FI+DF
                  H(2)=H(3)
                  H(4)=H(1)
                  V(2)=V(1)
                  V(4)=V(3)
                  H(5)=H(1)
                  V(5)=V(1)
                  CALL DQPOL(5,H,V)
                END IF
              END DO
            END IF
          END DO
        END DO
C       ..........................................................FZ: DRAW NF
        IF(BORD.EQ.1) THEN
          IF(NS.NE.0) THEN
            T3(3:3)=TSID(NS)(1:1)
            CALL DQLEVL(ICTXDD)
            DO NF=1,32
              IF(FFZ(NF,NS)) THEN
                CALL SIIDPO(NS,2,NF,1,ZZ,FI,RO)
                FI=DFINXT(F0,FI+ANG(NS))
                CALL DQPOC(Z1+HLN,FI,HTD,VTD,FIN)
                IF(FIN) THEN
                  WRITE(T2,1010) NF
                  CALL DGTEXT(HTD-DTD,VTD,T2,2)
                END IF
              END IF
            END DO
            CALL DQINV(IAREDO,0.,VLOWDG(IAREDO)+VLD,HDUM,FILOW)
            ZNL=ZM(NS)+DZ
            DO NZ=1,12
              CALL SIIDPO(NS,NZ,1,1,ZZ,FI,RO)
              ZZ=ZZ-ZNL
              CALL DQPOC(ZZ,FILOW,HTD,VTD,FIN)
              IF(FIN) THEN
                WRITE(T3(1:2),1010) NZ
                CALL DGTEXT(HTD,VMINDG(IAREDO)+VT1,T3,3)
              END IF
            END DO
          END IF
        END IF
C       .......................................................FZ: draw energy
        IF(MO.EQ.1) THEN
          SEMAX=SQRT(EMAX)
          QF=QQF/SEMAX
          QZ=QQZ/SEMAX
          DZQZ=DZ*QZ
        ELSE
          QF=QQF/EMAX
        END IF
        DFQF3=DF*QF/3.
        DFMAX=QQF*DF*Q1/3.
        DZMAX=QQF*DZ*Q1
        DFZ(3)=2.*DF/3.
        DFZ(1)=-DFZ(3)
        CEMX=0.
        PEMX=0.
        ICL=99
        DO NM=1,2
          DO NF=1,32
            IF(FFZ(NF,NM)) THEN
              DO NZ=1,12
                IF(EFZ(NF,NZ,NM).GT.0.) THEN
                  IF(ICL.NE.IFZ(NF,NZ,NM)) THEN
                    ICL=IFZ(NF,NZ,NM)
                    IF(MO.EQ.3.OR.MO.EQ.4) THEN
                      CALL DGLEVL(JCOL(ICL))
                    ELSE
                      CALL DQRA1(2,JCOL(ICL))
                    END IF
                  END IF
                  CALL SIIDPO(NM,NZ,NF,1,ZZ,FI,RO)
                  ZZ=ZZ-ZM(NM)
                  IF(MO.EQ.3) THEN
                    CALL DTN(EFZ(NF,NZ,NM),NDIG,TNUM)
                    IF(NS.EQ.0.AND.NM.EQ.2) FI=MOD(FI+180.,360.)
                    FI=DFINXT(F0,FI)
                    CALL DQTXT(ZZ-DZ,FI,TNUM,NDIG)
                  ELSE IF(MO.EQ.4) THEN
                    IF(NDIG.LT.4) THEN
                      CALL DTN(EFZ(NF,NZ,NM),NDIG,TNUM)
                      IF(NS.EQ.0.AND.NM.EQ.2) FI=MOD(FI+180.,360.)
                      FI=DFINXT(F0,FI)
                      CALL DQTXT(ZZ-DZ,FI,TNUM,NDIG)
                    ELSE
                      WRITE(TNUM,TF(NDIG)) EFZ(NF,NZ,NM)
                      IF(TNUM(1:2).EQ.' 0') TNUM(1:2)='  '
                      IF(NS.EQ.0.AND.NM.EQ.2) FI=MOD(FI+180.,360.)
                      FI=DFINXT(F0,FI)
                      CALL DQTXT(ZZ-DZ,FI,TNUM,NDIG)
                    END IF
                  ELSE
                    IF(MO.EQ.1) THEN
                      SEFZ=SQRT(EFZ(NF,NZ,NM))
                      DFE=DFQF3*SEFZ
                      DZE=DZQZ*SEFZ
                      IF(DZE.LE.DZMAX) THEN
                        FMAX1=.FALSE.
                      ELSE
                        FMAX1=.TRUE.
                        DRE=DRMAX
                      END IF
                    ELSE
                      DFE=DFQF3*EFZ(NF,NZ,NM)
                      DZE=DZMAX
                      FMAX1=.FALSE.
                    END IF
                    IF(DFE.LE.DFMAX) THEN
                      FMAX2=.FALSE.
                    ELSE
                      FMAX2=.TRUE.
                      DFE=DFMAX
                    END IF
                    DO I=1,3
                      H(1)=ZZ-DZE
                      H(3)=ZZ+DZE
                      FF=FI+DFZ(I)
                      IF(NS.EQ.0.AND.NM.EQ.2) FF=MOD(FF+180.,360.)
                      FF=DFINXT(F0,FF)
                      V(1)=FF-DFE
                      V(3)=FF+DFE
                      H(2)=H(3)
                      H(4)=H(1)
                      V(2)=V(1)
                      V(4)=V(3)
                      CALL DQRAR(H,V)
                      CALL DQPOC(ZZ,FF,HDUM,VDUM,FIN)
                      IF(FMAX1.OR.FMAX2) THEN
                        CALL DGLEVS(N1)
                        CALL DQL2E(H(1),V(1),H(3),V(3))
                        CALL DGLEVS(0)
                        IF(FIN) CEMX=MAX(CEMX,EFZ(NF,NZ,NM))
                      ELSE
                        IF(FIN) PEMX=MAX(PEMX,EFZ(NF,NZ,NM))
                      END IF
                    END DO
                  END IF
                END IF
              END DO
            END IF
          END DO
        END DO
        PEMX=PEMX/3.
        CEMX=CEMX/3.
        CALL DQSCA('H',HRB(1),HRB(2),'cm',2,'Z',1)
        CALL DQSCA('V',VRB(2),VRB(3),' deg',4,'&f',2)
      ELSE IF(NPR.GE.5.AND.NPR.LT.20) THEN
C       =================================================================== PU
C       .......................................................PU store energy
        CALL VZERO(EFZ,768)
        CALL VZERO(FFZ,64)
        CALL VZERO(EPMAX,21)
        EMAX=0.
        DO 805 K=1,NUM
          IAD=DVSIC(N14,K)
          CALL SIDCOZ(IAD,NM,NZ,NF,NR)
          IF(NS.NE.0.AND.NS.NE.NM) GO TO 805
          CLU=DVSIC(13,K)
          NDEAD=DVSIC(15,K)
          IF(NDEAD.NE.0) THEN
            IF(CLU.EQ.0.) GO TO 805
            ES=.0000001
          ELSE
            ES=DVSIC(IVENDV,K)
          END IF
          IF(ES.GT.E1.AND.ES.LT.E2) THEN
            IF(FCUTC) THEN
              IF(CLU.NE.CUTCL) GO TO 805
            END IF
            IF(FCUTR.AND.NR.NE.NRCUT) GO TO 805
            FFZ(NF,NM)=.TRUE.
            EFZ(NF,NZ,NM)=EFZ(NF,NZ,NM)+ES
            KCL=DVSIC(13,K)
            IF(ES.GT.EPMAX(KCL)) THEN
              EPMAX(KCL)=ES
              NPRMX(KCL)=NR
              IF(ES.GT.EMAX) THEN
                EMAX=ES
                NFMAX=NF+NFNM(NM)
              END IF
            END IF
          END IF
  805   CONTINUE
        NPRMX(0)=0
C       .................................................. PU SET UP USER RANGE
        CALL SIIDPO(1, 1,1, 1,ZMIN,FI,RO)
        CALL SIIDPO(1,12,1,16,ZMAX,FI,RO)
        ZHGH=ZMAX-ZMIN+2.*(ZGAP+DZ)
        IF(PARADA(4,J_CZZ).LE.0.) THEN
          IF(PARADA(4,J_CFM).GT.0..OR.
     &       PARADA(4,J_CNR).GT.0.) THEN
            F0=PARADA(2,J_CFM)
          ELSE
            IF(NS.EQ.2) NFMAX=NFMAX-16
            F0=2.*(NFMAX-1.)*DF+DF
            PARADA(2,J_CFM)=F0
          END IF
          IF(IZOMDO.EQ.0) THEN
            DFRU=180.
          ELSE
            DFRU=PARADA(2,J_PDF)
          END IF
          F0=MOD(F0+3600.,360.)
          F1=F0-DFRU
          F2=F0+DFRU
          Z1=-ZHGH
          Z2= ZHGH
          IF(NS.EQ.1) Z1=0.
          IF(NS.EQ.2) Z2=ZEPS
        ELSE
          F0=PARADA(2,J_CFM)
          Z0=PARADA(2,J_CZZ)
          DZD=DZNP*PARADA(2,J_CNP)
          Z21=(HHGHDG(IAREDO)-HLOWDG(IAREDO))*DZ*DZD
          F21=(VHGHDG(IAREDO)-VLOWDG(IAREDO))*DF*DZD/3.
          F1=F0-QPF*F21
          F2=F0+QPF*F21
          Z1=Z0-QPZ*Z21
          Z2=Z0+QPZ*Z21
          IF(Z0.GT.0.) THEN
            IF(Z1.LT.0.) THEN
              Z2=Z2+Z1
              Z1=0.
            END IF
            Z2=MIN(ZHGH,Z2)
          ELSE
            IF(Z2.GT.0.) THEN
              Z1=Z1-Z2
              Z2=0.
            END IF
            Z1=MAX(-ZHGH,Z1)
          END IF
        END IF
        CALL DQRER(0,Z1,F1,Z2,F2,HRB,VRB)
        CALL DQRU(HRB,VRB)
        IF(MO.EQ.3.OR.MO.EQ.4) THEN
          CALL DQPOC(0.,0.,HR1,VR,FIN)
          CALL DQPOC(DZ,0.,HR2,VR,FIN)
          NDIG=ABS(HR2-HR1)/WDIG
          NDIG=MIN(MAX(1,NDIG),5)
        END IF
        CALL DODSIC('ZFZ')
        CALL DQLEVL(LCSRDD)
        IF(KDEB.EQ.0) THEN
          D2Z=2.*DZ
          ZM(1)=ZMIN-ZGAP-D2Z
          IF(NS.EQ.0.OR.NS.EQ.1) THEN
            DO NZ=1,12
              CALL SIIDPO(1,NZ,1,1,Z,FI,RO)
              ZZ=Z-ZM(1)
              CALL DQL2E(ZZ,F1,ZZ,F2)
              DO NF=1,32
                CALL SIIDPO(1,NZ,NF,1,Z,FI,RO)
                FI=DFINXT(F0,FI+DF)
                CALL DQL2E(ZZ-D2Z,FI,ZZ,FI)
              END DO
            END DO
          END IF
          IF(NS.EQ.0.OR.NS.EQ.2) THEN
            DO NZ=1,12
              CALL SIIDPO(2,NZ,1,1,Z,FI,RO)
              ZZ=Z+ZM(1)
              CALL DQL2E(ZZ,F1,ZZ,F2)
              DO NF=1,32
                CALL SIIDPO(1,NZ,NF,1,Z,FI,RO)
                FI=DFINXT(F0,FI+DF)
                CALL DQL2E(ZZ+D2Z,FI,ZZ,FI)
              END DO
            END DO
          END IF
        END IF
        IF(KDEB.EQ.1) THEN
          ZL2=ZGAP+ZMAX-ZMIN+2*DZ
          DO NF=1,32
            CALL SIIDPO(1,2,NF,1,Z,FI,RO)
            FI=DFINXT(F0,FI+DF)
            IF(NS.EQ.0.OR.NS.EQ.1) CALL DQL2E( ZGAP,FI, ZL2,FI)
            IF(NS.EQ.0.OR.NS.EQ.2) CALL DQL2E(-ZGAP,FI,-ZL2,FI)
          END DO
        END IF
        IF(KDEB.GE.1) THEN
          ZM(1)=ZMIN-ZGAP-2.*DZ
          ZM(2)=-ZM(1)
          DO NM=1,2
            DO NZ=3,9,3
              CALL SIIDPO(NM,NZ,NF,1,Z,FI,RO)
              ZZ=Z-ZM(NM)
              CALL DQL2E(ZZ,F1,ZZ,F2)
            END DO
          END DO
        END IF
C       ........................................................PU: DRAW FRAME
        ZM(1)=ZMIN-ZGAP-DZ
        ZM(2)=-ZM(1)
        CALL DQPO0(TST,LA,LR,' ')
        DLINDD=PDCODD(2,LIGLDD)
        DO NM=1,2
          DO NF=1,32
            IF(FFZ(NF,NM)) THEN
              DO NZ=1,12
                IF(EFZ(NF,NZ,NM).GT.0.) THEN
                  CALL SIIDPO(NM,NZ,NF,1,ZZ,FI,RO)
                  ZZ=ZZ-ZM(NM)
                  IF(NS.EQ.0.AND.NM.EQ.2) FI=MOD(FI+180.,360.)
                  FI=DFINXT(F0,FI)
                  H(1)=ZZ-DZ
                  H(3)=ZZ+DZ
                  V(1)=FI-DF
                  V(3)=FI+DF
                  H(2)=H(3)
                  H(4)=H(1)
                  V(2)=V(1)
                  V(4)=V(3)
                  H(5)=H(1)
                  V(5)=V(1)
                  CALL DQPOL(5,H,V)
                END IF
              END DO
            END IF
          END DO
        END DO
C       ..........................................................PU: DRAW NF
        IF(BORD.EQ.1.) THEN
          IF(NS.NE.0) THEN
            T3(3:3)=TSID(NS)(1:1)
            CALL DQLEVL(ICTXDD)
            DO NF=1,32
              IF(FFZ(NF,NS)) THEN
                CALL SIIDPO(NS,2,NF,1,ZZ,FI,RO)
                FI=DFINXT(F0,FI+ANG(NS))
                CALL DQPOC(Z1+HLN,FI,HTD,VTD,FIN)
                IF(FIN) THEN
                  WRITE(T2,1010) NF
                  CALL DGTEXT(HMINDG(IAREDO)+DHTD,VTD,T2,2)
                END IF
              END IF
            END DO
            CALL DQINV(IAREDO,0.,VLOWDG(IAREDO)+VLD,HDUM,FILOW)
            ZNL=ZM(NS)+DZ
            DO NZ=1,12
              CALL SIIDPO(NS,NZ,1,1,ZZ,FI,RO)
              ZZ=ZZ-ZNL
              CALL DQPOC(ZZ,FILOW,HTD,VTD,FIN)
              IF(FIN) THEN
                WRITE(T3(1:2),1010) NZ
                CALL DGTEXT(HTD,VMINDG(IAREDO)+VT1,T3,3)
              END IF
            END DO
          END IF
        END IF
C       ......................................................... DRAW PUZZLE
C        CALL DBRSTO('PU',100)
        DLINDD=1.
        IF(NPR.GE.5.AND.NPR.LE.7) THEN
          IF(NPR.EQ.7) THEN
            LDEAD=LD7
          ELSE
            LDEAD=LD10
          END IF
C         ....................................................... PU 8 DOORS
C         DP=2.*DF/3.
          CALL SIIDPO(1,2,1,1,ZAU1,FU1,RDUM)
          ZAU1= ZAU1-ZM(1)
          ZBU1=-ZAU1
          FU0=FU1-2.*DF
          CALL DQPOC(ZAU1+2.*DZ,FU1     ,ZAD2,FD1  ,FIN)
          CALL DQPOC(ZAU1      ,FU0     ,ZAD1,FD0  ,FIN)
          CALL DQPOC(ZBU1      ,FU0+360.,ZBD1,FD360,FIN)
          DZD=ZAD2-ZAD1
          DZD2=DZD/2.
          DFD3=(FD1-FD0)/3.
          DFD6=DFD3/2.
          ZAD0=ZAD1-2.*DZD
          ZBD0=ZBD1+2.*DZD
C         DD360=FD360-FD0
C         DD180=DD360*0.5
          IF(NPR.EQ.5) THEN
            DS=MIN(DZD,DFD3)-DS0
          ELSE
            DS=DFD3-DS0
          END IF
          I2L=DS/16.
          IF(I2L.LE.0) I2L=1
          IF(I2L.GT.6) I2L=6
          I2P=I2PS(I2L)
          QEN=0.5*DZD/(EMAX+E0)
          IF(NPR.LE.6) THEN
            QEN=QEN*QEN0
            IF(I2P.EQ.1) THEN
              DLIN=DL2
            ELSE
              DLIN=DL1
            END IF
          ELSE
            DLIN=DL1
          END IF
          NFMID=1+F0/(2.*DF)
          CALL DQSET1(IAREDO,0.,0.)
          IF(NPR.EQ.5) THEN
C           .................................... PU: Vertical Lines
            DO 808 K=1,NUM
              CLU=DVSIC(13,K)
              NDEAD=DVSIC(15,K)
              IF(NDEAD.NE.0) THEN
                IF(CLU.EQ.0.) GO TO 808
                ES=EMAX*QDEAD(NPR)
              ELSE
                ES=DVSIC(IVENDV,K)
              END IF
              IF(ES.GT.E1.AND.ES.LT.E2) THEN
                IF(FCUTC.AND.CLU.NE.CUTCL) GO TO 808
                IAD=DVSIC(N14,K)
                CALL SIDCOZ(IAD,NM,NZ,NF,NR)
                IF(FCUTR.AND.NR.NE.NRCUT) GO TO 808
                IF(NS.EQ.0.OR.NS.EQ.NM) THEN
                  KCL=CLU
                  CALL DGLEVL(JCOL(KCL))
                  IF(NM.EQ.1) THEN
                    ZZ=ZAD0+NZ*DZD
                  ELSE
                    ZZ=ZBD0-NZ*DZD
                    IF(NS.EQ.0) THEN
                      IF(NF.LE.16) THEN
                        NF=NF+16
                      ELSE
                        NF=NF-16
                      END IF
                    END IF
                  END IF
                  IR=I2L*(NR-8)-I2L/2
                  FR=IR
                  ZR=ZZ+FR
                  IF(PARADA(4,J_CZZ).GT.0.) THEN
                    IF(ZR.LT.HLOWDG(IAREDO).OR.
     &                 ZR.GT.HHGHDG(IAREDO)) GO TO 808
                    IF(BORD.EQ.1..AND.I2L.GE.6) THEN
                      IF(NR.LT.10) THEN
                        CALL DGTEXT(ZR,VMINDG(IAREDO)+VT3 ,TNR(NR),1)
                      ELSE
                        CALL DGTEXT(ZR,VMINDG(IAREDO)+VT23,TNR(NR),1)
                      END IF
                    END IF
                  END IF
                  IF     (NF-NFMID.GT.16) THEN
                    NF=NF-32
                  ELSE IF(NFMID-NF.GT.16) THEN
                    NF=NF+32
                  END IF
                  MZ=MOD(NZ,3)
                  IF     (MZ.EQ.0) THEN
                    KF=3*NF-NAB
                  ELSE IF(MZ.EQ.1) THEN
                    KF=3*NF+NAB
                  ELSE
                    KF=3*NF
                  END IF
                  IF(NR.EQ.NPRMX(KCL)) THEN
                    DM=DM1
                  ELSE
                    DM=D1
                  END IF
                  FUP=FD0+DFD3*(KF+1)+DFD6-DM
                  FDW=FD0+DFD3*(KF-1)-DFD6+DM
                  DO I=1,I2P
                    CALL DQL2E(ZR,FDW,ZR,FUP)
                    ZR=ZR+1.
                  END DO
                END IF
              END IF
  808       CONTINUE
          END IF
C         ...................................... PU: Horizontal Lines
          PEMX=0.
          IR1=-I2L*8-I2L/2
          IR2= I2L*8-I2L/2
          FR1=IR1
          FR2=IR2
          DO 809 K=1,NUM
            CLU=DVSIC(13,K)
            NDEAD=DVSIC(15,K)
            IF(NDEAD.NE.0) THEN
              IF(CLU.EQ.0.) GO TO 809
              ES=EMAX*QDEAD(NPR)
            ELSE
              ES=DVSIC(IVENDV,K)
            END IF
            IF(ES.GT.E1.AND.ES.LT.E2) THEN
              IF(FCUTC.AND.CLU.NE.CUTCL) GO TO 809
              IAD=DVSIC(N14,K)
              CALL SIDCOZ(IAD,NM,NZ,NF,NR)
              IF(FCUTR.AND.NR.NE.NRCUT) GO TO 809
              IF(NS.EQ.0.OR.NS.EQ.NM) THEN
                KCL=CLU
                IF(NM.EQ.1) THEN
                  ZZ=ZAD0+NZ*DZD
                ELSE
                  ZZ=ZBD0-NZ*DZD
                  IF(NS.EQ.0) THEN
                    IF(NF.LE.16) THEN
                      NF=NF+16
                    ELSE
                      NF=NF-16
                    END IF
                  END IF
                END IF
                IF     (NF-NFMID.GT.16) THEN
                  NF=NF-32
                ELSE IF(NFMID-NF.GT.16) THEN
                  NF=NF+32
                END IF
                MZ=MOD(NZ,3)
                IF     (MZ.EQ.0) THEN
                  KF=3*NF-NAB
                ELSE IF(MZ.EQ.1) THEN
                  KF=3*NF+NAB
                ELSE
                  KF=3*NF
                END IF
                IR=I2L*(NR-8)-I2L/2
                FR=IR
                NCOL=JCOL(KCL)
                IF(NPR.EQ.7.AND.NDEAD.NE.0) NCOL=LDEAD
                CALL DGLEVL(NCOL)
                IF(NPR.LE.6) THEN
                  IF(NR.EQ.NPRMX(KCL)) THEN
                    ZD1=ZZ-DZD2+DM1
                    ZD2=ZZ+DZD2-DM1
                  ELSE
                    ZD1=ZZ-DZD2+D1
                    ZD2=ZZ+DZD2-D1
                  END IF
                  DLINDD=1.
                  DO M=-1,1
                    FI=FD0+DFD3*(KF+M)+FR
                    DO I=1,I2P
                      CALL DQL2E(ZD1,FI,ZD2,FI)
                      FI=FI+1.
                    END DO
                  END DO
                END IF
                ZE1=ZZ-QEN*(ES+E0)
                ZE2=ZZ+QEN*(ES+E0)
                DO M=-1,1
                  CALL DQPOC(ZZ,FI,HDUM,VDUM,FIN)
                  IF(FIN) PEMX=MAX(PEMX,ES)
                  DLINDD=DLIN
                  IF(NPR.LE.6) THEN
                    IF(NDEAD.EQ.0) THEN
                      CALL DGLEVL(LENER(JCOL(KCL)))
                    ELSE
                      CALL DGLEVL(LDEAD)
                    END IF
                  END IF
                  FI=FD0+DFD3*(KF+M)+FR
                  DO I=1,I2P
                    CALL DQL2E(ZE1,FI,ZE2,FI)
                    FI=FI+1.
                  END DO
                  IF(NPR.EQ.5) THEN
C                   ...................................... PU: Diagonal Line
                    FI0=FD0+DFD3*(KF+M)
                    DLINDD=DD2
                    CALL DQLEVL(LCSDDD)
                    CALL DQL2E(ZZ+FR1,FI0+FR1,ZZ+FR2,FI0+FR2)
                  END IF
                END DO
              END IF
            END IF
  809     CONTINUE
        ELSE IF(NPR.EQ.8) THEN
C         ....................................................... PE 2 DOORS
          CALL SIIDPO(1,2,1,1,ZAU1,FU1,RDUM)
          ZAU1= ZAU1-ZM(1)
          ZBU1=-ZAU1
          FU0=FU1-2.*DF
          CALL DQPOC(ZAU1+2.*DZ,FU1     ,ZAD2,FD1  ,FIN)
          CALL DQPOC(ZAU1      ,FU0     ,ZAD1,FD0  ,FIN)
          DZD=ZAD2-ZAD1
          I2L=DZD/N24
          IF(I2L.LE.0) I2L=1
          IF(I2L.GT.6) I2L=6
          I2P=I2PS(I2L)
          IR2=I2L*N20-I2L/2
          FR2=IR2
C         ...................................... PE: Lines
          DO 810 K=1,NUM
            ES=DVSIC(IVENDV,K)
            IF(ES.GT.E1.AND.ES.LT.E2) THEN
              IF(FCUTC) THEN
                CLU=DVSIC(13,K)
                IF(CLU.NE.CUTCL) GO TO 810
              END IF
              IAD=DVSIC(N14,K)
              CALL SIDCOZ(IAD,NM,NZ,NF,NR)
              IF(FCUTR.AND.NR.NE.NRCUT) GO TO 810
              IF(NS.EQ.0.OR.NS.EQ.NM) THEN
                KCL=DVSIC(13,K)
                CALL DGLEVL(JCOL(KCL))
                CALL SIIDPO(NM,NZ,NF,NR,ZZ,FI,RO)
                IF(NS.EQ.0.AND.NM.EQ.2) FI=MOD(FI+180.,360.)
                FI=DFINXT(F0,FI)
                ZZ=ZZ-ZM(NM)+DZ
                FI=FI+DF
                ZP2=ZZ-2.*DZ
                FP2=FI-2.*DF
                CALL DQPOC( ZZ, FI,ZZD,FFD,FIN)
                CALL DQPOC(ZP2,FP2,ZPD,FPD,FIN)
                CALL DQSET1(IAREDO,0.,0.)
                IR=I2L*(N20-NR)-I2L/2
                FR=IR
                ZDR=ZZD-FR
                FDR=FFD-FR
                DO I=1,I2P
                  CALL DQL2E(ZPD,FDR,ZDR,FDR)
                  CALL DQL2E(ZDR,FDR,ZDR,FPD)
                  ZDR=ZDR-1.
                  FDR=FDR-1.
                END DO
                CALL DGLEVL(LR)
                ZDR=ZZD-FR2
                FDR=FFD-FR2
                CALL DQL2E(ZPD,FDR,ZDR,FDR)
                CALL DQL2E(ZDR,FDR,ZDR,FPD)
                CALL DQL2E(ZDR,FDR,ZZD,FFD)
                CALL DQSET(IAREDO,0.,0.)
              END IF
            END IF
  810     CONTINUE
        END IF
        IF(BORD.EQ.1.) THEN
          IF(PARADA(4,J_CZZ).LE.0.)
     &      CALL DQSCA('H',HRB(1),HRB(2),'cm',2,'Z',1)
          CALL DQSCA('V',VRB(2),VRB(3),' deg',4,'&f',2)
        END IF
        DLINDD=PDCODD(2,LITRDD)
      ELSE
C       ================================================================= RO
        CALL SIIDPO(NS,6,1,1,Z0,FI,RO)
        SF=SIND(PARADA(2,J_PFI))
        CF=COSD(PARADA(2,J_PFI))
        ST=SIND(90.-PARADA(2,J_PTE))
        CT=COSD(90.-PARADA(2,J_PTE))
        SG=SIND(PARADA(2,J_RAL))
        CG=COSD(PARADA(2,J_RAL))
        ESRO= PARADA(2,J_RES)*CMPIX/AHSCDQ
        EYRO=-PARADA(2,J_REY)*CMPIX/AHSCDQ
        PEMX=0.
        DLINDD=PDCODD(2,LIGLDD)
        NP=9
        NA=1
        NB=2
        IF(NS.EQ.1) NB=1
        IF(NS.EQ.2) NA=2
        DZRO=DZ-DRO
        DRRO=DR-DRO
        QRO =DRO*PIDEG
        DO 950 K=1,NUM
          IAD=DVSIC(N14,K)
          CALL SIDCOZ(IAD,NM,NZ,NF,NR)
          IF(NM.NE.NS) GO TO 950
          IF(FCUTR.AND.NR.NE.NRCUT) GO TO 950
          ES=DVSIC(IVENDV,K)
          IF(ES.GT.E1.AND.ES.LT.E2) THEN
            CLU=DVSIC(13,K)
            IF(FCUTC) THEN
              IF(CLU.NE.CUTCL) GO TO 950
            END IF
            KCL=CLU
            CALL DGLEVL(JCOL(KCL))
            CALL SIIDPO(NM,NZ,NF,NR,ZZ,FI,RO)
            DFRO=DF-QRO/RO
            CALL DSECTR(2,FI,DFRO,RO,DRRO,XRO,YRO)
            ZZ=ZZ-Z0
            ZRO(1)=ZZ-DZRO
            ZRO(2)=ZZ+DZRO
            DO JZ=1,2
              Z1=ZRO(JZ)
              DO N=1,NP-1
                X1=XRO(N)
                Y1=YRO(N)
                X2= CF*X1+SF*Y1
                Y2=-SF*X1+CF*Y1
                H9= CT*X2+ST*Z1
                Z3=-ST*X2+CT*Z1
                V9= CG*Y2+SG*Z3
                IF(PARADA(4,J_RES).EQ.1.) THEN
                  W=-SG*Y2+CG*Z3
                  ESW=ESRO+W
                  H9=H9+(EYRO-H9)*W/ESW
                  V9=V9*ESRO/ESW
                END IF
                HRO(N,JZ)=H9
                VRO(N,JZ)=V9
              END DO
              HRO(NP,JZ)=HRO(1,JZ)
              VRO(NP,JZ)=VRO(1,JZ)
              DO N=1,NP-1
                CALL DQLIE(HRO(N,JZ),VRO(N,JZ))
              END DO
            END DO
            DO N=1,4
              J=JRO(N)
              CALL DQL2E(HRO(J,1),VRO(J,1),HRO(J,2),VRO(J,2))
            END DO
          END IF
  950   CONTINUE
      END IF
      IF(BORD.EQ.1.) THEN
        CALL DQLEVL(ICTXDD)
        IF(PEMX.GT.0.) THEN
          CALL   DGTEXT(HMINDG(IAREDO),VMINDG(IAREDO)+VT3,
     &      'MAX',3)
          CALL   DGTEXT(HMINDG(IAREDO),VMINDG(IAREDO)+VT2,
     &      '='//DT3(PEMX),4)
          IF(CEMX.GT.0.)
     &      CALL DGTEXT(HMINDG(IAREDO),VMINDG(IAREDO)+VT1,
     &      '<'//DT3(CEMX),4)
        END IF
      END IF
   99 IF(NPR.NE.0) THEN
        CALL DQFR(IAREDO)
        CALL DPCSAR
      END IF
      END
*DK DSCXY
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DSCXY
CH
      SUBROUTINE DSCXY(NPR,NUM)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DATA PIDEG/57.29577951/
      CHARACTER *2 DT2
      CHARACTER *3 DT3
      CHARACTER *4 DT4
      CHARACTER *35 T
      CHARACTER *2 TSID(0:2)
      DATA TSID/'AB','A ','B'/
      DATA DHS/160./
      DIMENSION H(15),V(15),IM(12,2),JM(0:2),SM(1:3)
      DIMENSION HP(-90:90),VP(-90:90)
      DATA IM/1,2,3, 1,2,3, 1,2,3, 1,2,3,
     &        3,2,1, 3,2,1, 3,2,1, 3,2,1/
      DATA JM/3,1,2/,SM/-2.,0.,2./
      DATA N14/14/,L7/7/,QQR/0.8/,QQF/0.8/,MO/1/
      DATA Q3/0.3333333/,Q1/1.001/
      DATA NI/2/,J1/1/,J2/2/
      DIMENSION HRB(4),VRB(4)
      DATA QHF0/0.49/,DFR/1./
      DIMENSION EXY(0:99,16),EFI(0:99),ERO(16,2)
      LOGICAL FS(0:32,16,2),FXYN(0:32),FXYK(0:96)
      DIMENSION IXY(0:99,16)
      LOGICAL FMAX1,FMAX2,FIN,FCUTC,FCUTR
      CHARACTER *4 TST
      DATA TST/'LIN'/,LA/7/
      DATA RTO15/15./,RTO18/18./
      DIMENSION JCOL(0:21),FIR(2)
      DATA VT1/4./,VT2/14./,VT3/24./,DHF/2./,DVF/20./
      DATA LAMBI/21/,DFRH/-7./
      DIMENSION NZT(27),NFT(27),NRT(27)
      DATA NZT/ 1, 2, 3,  1, 2, 3,  1, 2, 3,
     &          1, 2, 3,  1, 2, 3,  1, 2, 3,
     &          1, 2, 3,  1, 2, 3,  1, 2, 3/
      DATA NFT/31,31,31, 31,31,32, 31,32,32,
     &         32,32,32, 32,32, 1, 32, 1, 1,
     &          1, 1, 1,  1, 1, 2,  1, 2, 2/
      DATA NRT/ 1, 1, 1,  2, 2, 2,  4, 4, 4,
     &          6, 6, 6,  8, 8, 8, 10,10,10,
     &         12,12,12, 14,14,14, 16,16,16/
      DATA NMT/1/,EST/1./
      DATA BORD/0./
      DATA IDEB/0/,KDEB/0/,MDEB/0/
      DATA RI/0./
C     ............................................. STORE PARAMETERS
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PAB'
      CALL DPARAM(10
     &  ,J_PFI,J_PAB)
      TPARDA=
     &  'J_CFO,J_CFL,J_CTL,J_CSX,J_CR1,J_CE1,J_CE2,J_CCN,J_CNR,J_CCG'
      CALL DPARAM(40
     &  ,J_CFO,J_CFL,J_CTL,J_CSX,J_CR1,J_CE1,J_CE2,J_CCN,J_CNR,J_CCG)
      TPARDA=
     &  'J_CSY'
      CALL DPARAM(40
     &  ,J_CSY)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      MO=PARADA(2,J_CSY)
      NS=PARADA(2,J_PAB)
      IF(NS.EQ.-1) NS=2
      LA=PDCODD(2,LCSADD)
      LR=PDCODD(2,LCSRDD)
      LS=PDCODD(2,LCSSDD)
      LH=PDCODD(2,LCSHDD)
      LL=PDCODD(2,LCSLDD)
      JCOL(21)=LS
      IF(PDCODD(4,LCSADD).LT.0.) THEN
        TST='LIN'
      ELSE
        TST='A-L'
      END IF
      IF(PARADA(4,J_CE1).GT.0.) THEN
        E1=PARADA(2,J_CE1)
      ELSE
        E1=0.
      END IF
      IF(PARADA(4,J_CE2).GT.0.) THEN
        E2=PARADA(2,J_CE2)
      ELSE
        E2=999.
      END IF
      IF(PARADA(4,J_CCN).GT.0.) THEN
        CUTCL=PARADA(2,J_CCN)
        FCUTC=.TRUE.
      ELSE
        FCUTC=.FALSE.
      END IF
      IF(PARADA(4,J_CNR).GT.0.) THEN
        NRCUT=PARADA(2,J_CNR)
        FCUTR=.TRUE.
      ELSE
        FCUTR=.FALSE.
      END IF
      NDFRH=PARADA(2,J_CFO)-DFRH
      IF(IRUNDE(1).LT.2000) THEN
        MAB=1
        NAB=1
      ELSE IF(IRUNDE(1).LT.14850) THEN
        MAB=2
        NAB=-1
      ELSE
        MAB=1
        NAB=1
      END IF
      CALL DSCHSC(JCOL)
      CALL DGRSIC(DZ,DF,DR,RMIN)
      L1=PARADA(2,J_CFL)
      L2=PARADA(2,J_CTL)
      IF(L2.EQ.0) THEN
        L1=1
        L2=12
      ELSE
        IF(L1.EQ.0) L1=1
        IF(PARADA(4,J_CFL).GT.0.) THEN
          IF(PARADA(4,J_CTL).LT.0.) L2=L1
        ELSE
          IF(PARADA(4,J_CTL).GT.0.) THEN
            L1=L2
          ELSE
            L1=1
            L2=12
          END IF
        END IF
      END IF
C     ...................................................... XY Set up range.
      DLINDD=PDCODD(2,LIDTDD)
      IF(NPR.GT.0) THEN
        IF(NPR.EQ.9) THEN
          RTO=RTO18
        ELSE
          RTO=RTO15
        END IF
        IF(IZOMDO.EQ.0) THEN
          CALL DQRER(0,-RTO,-RTO,RTO,RTO,HRB,VRB)
        ELSE
          FIM=MOD(3600.+PARADA(2,J_PFI),360.)
          RFR=PARADA(2,J_CR1)
          RTO2=0.5*(RTO+RFR)
          DRXY=0.5*(RTO-RFR)
          IF     (FIM.LE.45..OR.FIM.GE.315.) THEN
            XM= RTO2
            YM= RTO2*TAND(FIM)
          ELSE IF(FIM.LT.135.) THEN
            XM= RTO2*TAND(90.-FIM)
            YM= RTO2
          ELSE IF(FIM.LE.225.) THEN
            XM=-RTO2
            YM=-RTO2*TAND(FIM)
          ELSE
            XM=-RTO2*TAND(270.-FIM)
            YM=-RTO2
          END IF
          CALL DQRER(0,XM-DRXY,YM-DRXY,XM+DRXY,YM+DRXY,HRB,VRB)
        END IF
        CALL DQRU(HRB,VRB)
        CALL DODSIC(' YX')
      END IF
      PEMX=0.
      CEMX=0.
      IF(NUM.LE.0) GO TO 99
C     ........................................... XY draw all frame lines
      IF(PARADA(4,J_CCG).GT.0.) THEN
        LCG=PARADA(2,J_CCG)
        CALL DQPO0('LINE',LA,LCG,' ')
        DLINDD=PDCODD(2,LIGLDD)
        NP=5+2*NI
        DO NF=1,32
          DO NR=1,16
            CALL SIIDPO(1,2,NF,NR,ZZ,FI,RO)
            CALL DSECTR(NI,FI,DF,RI+RO,DR,H,V)
            H(NP)=H(1)
            V(NP)=V(1)
            CALL DQPOL(NP,H,V)
          END DO
        END DO
      END IF
      CALL VZERO(EXY,1600)
      CALL VZERO(FS,1056)
      CALL VZERO(FXYN,33)
C     ...................................... XY find EMAX, and frame pattern
      EMAX=0.
      DO 802 K=1,NUM
        IF(MDEB.EQ.0) THEN
          IAD=DVSIC(N14,K)
          CALL SIDCOZ(IAD,NM,NZ,NF,NR)
          ES=DVSIC(IVENDV,K)
        ELSE
          NM=NMT
          NZ=NZT(K)
          NF=NFT(K)
          NR=NRT(K)
          ES=EST
        END IF
        IF(ES.GT.E1.AND.ES.LT.E2) THEN
          IF(FCUTC) THEN
            CLU=DVSIC(13,K)
            IF(CLU.NE.CUTCL) GO TO 802
          END IF
          ES=ES*Q3
          KF=3*NF-IM(NZ,MAB)
          DO I=0,2
            KFI=KF+I
            IF     (KFI.LE. 0) THEN
              KFI=96
            ELSE IF(KFI.GT.96) THEN
              KFI=1
            END IF
            NF1=(KFI+2)/3
            FS(NF1,NR,NM)=.TRUE.
            FXYN(NF1)=.TRUE.
            EXY(KFI,NR)=EXY(KFI,NR)+ES
            IF(EXY(KFI,NR).GT.EMAX) THEN
              EMAX=EXY(KFI,NR)
              NRMAX=NR
              NFMAX=NF
            END IF
          END DO
        END IF
  802 CONTINUE
      IF(PARADA(4,J_CSX).GT.0..OR.EMAX.LE.0.) THEN
        EMAX=PARADA(2,J_CSX)
      ELSE
        PARADA(2,J_CSX)=EMAX
      END IF
      IF(KDEB.GT.1) THEN
        DO JF=0,32
          FXYN(JF)=.TRUE.
          DO JR=1,16
            FS(JF,JR,1)=.TRUE.
          END DO
        END DO
      END IF
      DLINDD=PDCODD(2,LIDTDD)
C     ............................................ XY Draw circle at maximum
      IF(EMAX.GT.0.) THEN
        IF(L7.GE.0) THEN
          CALL DQLEVL(LCSRDD)
          CALL SIIDPO(1,2,NFMAX,NRMAX,ZZ,FI,RO)
          RMAX=RMIN+32.*DR
          FI=FI-DF
          SF=SIND(FI)
          CF=COSD(FI)
          H(1)=CF*RMIN
          V(1)=SF*RMIN
          H(2)=CF*RMAX
          V(2)=SF*RMAX
          CALL DQLIE(H,V)
          H(1)=-H(1)
          V(1)=-V(1)
          H(2)=-H(2)
          V(2)=-V(2)
          CALL DQLIE(H,V)
          IF(NRMAX.GT.1.AND.NRMAX.LT.16) THEN
            DF2=2.*DF/3.
            RO=RO-DR
            H(1)=RO*CF
            V(1)=RO*SF
            DO N=1,97
              FI=FI+DF2
              H(2)=RO*COSD(FI)
              V(2)=RO*SIND(FI)
              CALL DQLIE(H,V)
              H(1)=H(2)
              V(1)=V(2)
            END DO
          END IF
        END IF
      END IF
C     ........................................... XY draw frame areas
C      IF(LLL) CALL DQLEVL(LCSRDD)
      CALL DQPO0('AREA',LA,LR,' ')
      DLINDD=PDCODD(2,LIGLDD)
      NP=5+2*NI
      NA=1
      NB=2
      IF(NS.EQ.1) NB=1
      IF(NS.EQ.2) NA=2
      DO NF=1,32
        IF(FXYN(NF)) THEN
          DO NR=1,16
            IF(FS(NF,NR,NA).OR.FS(NF,NR,NB)) THEN
              CALL SIIDPO(1,2,NF,NR,ZZ,FI,RO)
              CALL DSECTR(NI,FI,DF,RI+RO,DR,H,V)
              H(NP)=H(1)
              V(NP)=V(1)
              CALL DQPOL(NP,H,V)
            END IF
          END DO
        END IF
      END DO
C     ................................... XY sum energy for selected layers.
      CALL VZERO(EXY,1600)
      CALL VZERO(FXYK,97)
      CALL VZERO(IXY,1600)
      CALL VZERO(EFI,100)
      CALL VZERO(ERO,32)
      DO 701 K=1,NUM
        IF(MDEB.EQ.0) THEN
          IAD=DVSIC(N14,K)
          CALL SIDCOZ(IAD,NM,NZ,NF,NR)
          ES=DVSIC(IVENDV,K)
        ELSE
          NM=NMT
          NZ=NZT(K)
          NF=NFT(K)
          NR=NRT(K)
          ES=EST
        END IF
        IF(ES.GT.E1.AND.ES.LT.E2) THEN
          ES=ES*Q3
          IF(NS.EQ.1.AND.NM.EQ.2) GO TO 701
          IF(NS.EQ.2.AND.NM.EQ.1) GO TO 701
          IF(NZ.LT.L1.OR.NZ.GT.L2) GO TO 701
          CLU=DVSIC(13,K)
          IF(FCUTC.AND.CLU.NE.CUTCL) GO TO 701
          KF=3*NF-IM(NZ,MAB)
          DO I=0,2
            KFI=KF+I
            IF     (KFI.LE. 0) THEN
              KFI=96
            ELSE IF(KFI.GT.96) THEN
              KFI=1
            END IF
            FXYK(KFI)=.TRUE.
            EXY(KFI,NR)=EXY(KFI,NR)+ES
            KCL=CLU
            IF(IXY(KFI,NR).EQ.0) THEN
              IXY(KFI,NR)=KCL
            ELSE IF(KCL.NE.0.AND.IXY(KFI,NR).NE.KCL) THEN
              IXY(KFI,NR)=LAMBI
            END IF
          END DO
        END IF
  701 CONTINUE
      IF(NPR.EQ.9) THEN
C       .......................................... draw  FI histogram
        EMXFI=0.
        DO KF=1,96
          IF(FXYK(KF)) THEN
            EFI(KF)=0.
            DO NR=1,16
              EFI(KF)=EFI(KF)+EXY(KF,NR)
            END DO
            IF(EFI(KF).GT.EMXFI) THEN
              EMXFI=EFI(KF)
              KFI=KF
            END IF
          END IF
        END DO
        ROH=RMIN+32.*DR
        QHF=QHF0*(RTO18-RTO15)/EMXFI
        QHF2=QHF*2.
        CALL DQRA0(3,LCSHDD)
        DO KF=1,96
          IF(EFI(KF).GT.0.) THEN
            NL=JM(MOD(KF,3))
            SS=SM(NL)
            NF=(KF+2)/3
            CALL SIIDPO(1,2,NF,16,ZZ,FI,RO)
            FIF=FI+SS*D3
            DRE=QHF*EFI(KF)
            RHST=ROH+DRE
            CALL DSECTR(N0,FIF,D3,RHST,DRE,H,V)
            CALL DQLEVL(LCSLDD)
            CALL DQRAR(H,V)
          END IF
        END DO
C       .......................................... draw  RO histogram
        IF(IDEB.NE.0) THEN
          LC1=PDCODD(2,LCSLDD)
          LC2=PDCODD(2,LCSHDD)
          CALL DQPO0('AR+L',LC2,LC1,'NSKP')
        END IF
        K1=KFI-24
        IF(K1.LT.1 ) K1=K1+48
        IF(K1.GT.48) K1=K1-48
        K2=K1+48
        DO NR=1,16
          ERO(NR,1)=0.
          ERO(NR,2)=0.
          DO KF=1,K1
            ERO(NR,1)=ERO(NR,1)+EXY(KF,NR)
          END DO
          DO KF=K1+1,K2
            ERO(NR,2)=ERO(NR,2)+EXY(KF,NR)
          END DO
          DO KF=K2+1,96
            ERO(NR,1)=ERO(NR,1)+EXY(KF,NR)
          END DO
        END DO
        NF1=(K1+2)/3+NDFRH
        IF(NF1.LT.1) NF1=NF1+32
        CALL SIIDPO(1,2,NF1,16,ZZ,FIR(1),RO)
        FIR(1)=FIR(1)+DF
        FIR(2)=FIR(1)+180.
        DR2=DR*2
        DO L=1,2
          R1=RMIN
          IF(IDEB.EQ.0) THEN
            SF=SIND(FIR(L))
            CF=COSD(FIR(L))
            DO NR=1,16
              ER=ERO(NR,L)*QHF2
              H(1)=R1*CF
              V(1)=R1*SF
              R1=R1+DR2
              H(2)=R1*CF
              V(2)=R1*SF
              H(3)=H(2)-ER*SF
              V(3)=V(2)+ER*CF
              H(4)=H(1)-ER*SF
              V(4)=V(1)+ER*CF
              CALL DQLEVL(LCSLDD)
              CALL DQRAR(H,V)
            END DO
          ELSE
            DO NR=1,16
              ER=ERO(NR,L)*QHF2
              R2=R1+DR2
              FI=FIR(L)
              KU=0
              KD=1
              DA=0.
              DO JJ=1,90
                SF=SIND(FI)
                CF=COSD(FI)
                KU=KU+1
                KD=KD-1
                HP(KU)=R1*CF
                VP(KU)=R1*SF
                HP(KD)=R2*CF
                VP(KD)=R2*SF
                FI=FI+DFR
                DA=DA+DF*R1/PIDEG
                IF(ER.LT.DA) GO TO 201
              END DO
  201         IF(KU.GT.1) CALL DQPOL(2*KU,HP(KD),VP(KD))
              R1=R2
            END DO
          END IF
C            123456789 123456789
          T='EMAX(PHI)=12.3 GeV'
          T(11:14)=DT4(EMXFI)
          CALL DQLEVL(ICTXDD)
          CALL DGTEXT(HLOWDG(IAREDO)+DHF,VHGHDG(IAREDO)-DVF,T,18)
          SF=SIND(FIR(L))
          CF=COSD(FIR(L))
          H(1)=RMIN*CF
          V(1)=RMIN*SF
          H(2)=ROH *CF
          V(2)=ROH *SF
          CALL DGLEVL(L7)
          CALL DQLIE(H,V)
        END DO
      END IF
C     ........................................... XY draw selected frame lines
C      IF(LLL) CALL DQLEVL(LCSRDD)
      CALL DQPO0('LINE',LA,LR,' ')
      DLINDD=PDCODD(2,LIGLDD)
      NP=5+2*NI
      NA=1
      NB=2
      IF(NS.EQ.1) NB=1
      IF(NS.EQ.2) NA=2
      DO NF=1,32
        IF(FXYN(NF)) THEN
          DO NR=1,16
            IF(FS(NF,NR,NA).OR.FS(NF,NR,NB)) THEN
              CALL SIIDPO(1,2,NF,NR,ZZ,FI,RO)
              CALL DSECTR(NI,FI,DF,RI+RO,DR,H,V)
              H(NP)=H(1)
              V(NP)=V(1)
              CALL DQPOL(NP,H,V)
              IF(NS.EQ.0.AND.FS(NF,NR,J2).AND.
     &          PARADA(4,J_CCG).LT.0.) THEN
                CALL DQL2E(H(1),V(1),H(7),V(7))
                IF(FS(NF,NR,J1)) CALL DQL2E(H(4),V(4),H(6),V(6))
              END IF
            END IF
          END DO
        END IF
      END DO
C     ...................................................... XY draw energy.
      D3=DF/3.
      IF(MO.EQ.1) THEN
        SEMAX=SQRT(EMAX)
        QF=QQF/SEMAX
        D3QF=D3*QF
        QR=QQR/SEMAX
      ELSE
        QR=QQR/EMAX
      END IF
      DRQR=DR*QR
      DRMAX=QQR*DR*Q1
      DFMAX=QQF*D3*Q1
      R1=RMIN+DR
      ICL=99
      DO KF=1,96
        IF(FXYK(KF)) THEN
          NL=JM(MOD(KF,3))
          SS=SM(NL)
          NF=(KF+2)/3
          DO NR=1,16
            IF(EXY(KF,NR).GT.0.) THEN
              IF(ICL.NE.IXY(KF,NR)) THEN
                ICL=IXY(KF,NR)
                CALL DQRA1(2,JCOL(ICL))
              END IF
              CALL SIIDPO(1,2,NF,NR,ZZ,FI,RO)
              IF(MO.EQ.1) THEN
                SEXY=SQRT(EXY(KF,NR))
                DRE=DRQR*SEXY
                DFE=D3QF*SEXY*R1/RO
                IF(DFE.LE.DFMAX) THEN
                  FMAX2=.FALSE.
                ELSE
                  FMAX2=.TRUE.
                  DFE=DFMAX
                END IF
              ELSE
                DRE=DRQR*EXY(KF,NR)
                DFE=DFMAX
                FMAX2=.FALSE.
              END IF
              IF(DRE.LE.DRMAX) THEN
                FMAX1=.FALSE.
              ELSE
                FMAX1=.TRUE.
                DRE=DRMAX
              END IF
              FIF=FI+SS*D3
              CALL DSECTR(N0,FIF,DFE,RO,DRE,H,V)
              CALL DQRAR(H,V)
              HM=0.5*(H(1)+H(3))
              VM=0.5*(V(1)+V(3))
              CALL DQPOC(HM,VM,HDUM,VDUM,FIN)
              IF(FMAX1.OR.FMAX2) THEN
                CALL DGLEVS(N1)
                CALL DQL2E(H(1),V(1),H(3),V(3))
                CALL DGLEVS(0)
                IF(FIN) CEMX=MAX(CEMX,EXY(KF,NR))
              ELSE
                IF(FIN) PEMX=MAX(PEMX,EXY(KF,NR))
              END IF
            END IF
          END DO
        END IF
      END DO
      CALL DSCMC(0.,NS)
C     ....................................................... XY draw scale
C        123456789 123456789
   99 T='Layer 12-12 side AB'
      T( 7: 8)=DT2(FLOAT(L1))
      T(10:11)=DT2(FLOAT(L2))
      T(18:19)=TSID(NS)
      CALL DQLEVL(ICTXDD)
      CALL DGTEXT(HHGHDG(IAREDO)-DHS,VMINDG(IAREDO)+VT1,T,19)
      IF(NPR.NE.0) THEN
        CALL DQSCA('H',HRB(1),HRB(2),'cm',2,'X',1)
        CALL DQSCA('V',VRB(2),VRB(3),'cm',2,'Y',1)
      END IF
      IF(BORD.EQ.1.) THEN
        CALL DQLEVL(ICTXDD)
        IF(PEMX.GT.0.) THEN
          CALL   DGTEXT(HMINDG(IAREDO),VMINDG(IAREDO)+VT3,
     &      'MAX',3)
          CALL   DGTEXT(HMINDG(IAREDO),VMINDG(IAREDO)+VT2,
     &      '='//DT3(PEMX),4)
          IF(CEMX.GT.0.)
     &      CALL DGTEXT(HMINDG(IAREDO),VMINDG(IAREDO)+VT1,
     &      '<'//DT3(CEMX),4)
        END IF
      END IF
      END
*DK DSECTR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DSECTR(NI,FI,DF,RO,DR,H,V)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Outputs   :H(*),V(*) = Trapez/sector polygon
C    input     :
C ---------------------------------------------------------------------
C
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(*),V(*),R(2)
      IF(NI.EQ.-1) THEN
        CALL DTRAPZ(FI,DF,RO,DR,H,V)
        RETURN
      END IF
      R(1)=RO-DR
      R(2)=RO+DR
      D3=2.*DF/FLOAT(NI+1)
      F=FI-DF
      I=0
      DO N=1,2
        F=F-D3
        DO K=1,2+NI
          I=I+1
          F=F+D3
          SF=SIND(F)
          CF=COSD(F)
          H(I)=CF*R(N)
          V(I)=SF*R(N)
        END DO
        D3=-D3
      END DO
      END
