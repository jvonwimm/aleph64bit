*DK DQHLP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQHLP
CH
      SUBROUTINE DQHLP(TINP)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modified by B.S. Nilsson                  Spring 1989 and Feb-1990
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DM
      COMMON /DMACRC/ FMACDM,NLETDM,FMOPDM,FMINDM,FANIDM,FNTADM,
     &  FINMDM
      LOGICAL FMACDM,FMOPDM,FMINDM,FANIDM,FNTADM,FINMDM
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
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
C------------------------------------------------------------------  DC
C     HARD WIRED IN P_DALB_ALEPH.FOR           !!!
C     ++++++++++++++++++++++++++ *12 TFILDC
      COMMON /DCFTVT/ TFILDC,TITLDC,TDEFDC,TNAMDC
      CHARACTER  *8 TFILDC
      CHARACTER *10 TITLDC
      CHARACTER  *3 TDEFDC
      CHARACTER  *4 TNAMDC
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C------------------------------------------------------------------- DO
      COMMON /DOPR1T/ TPLNDO(-2:11)
      COMMON /DOPR2T/ TAREDO( 0:14)
      COMMON /DOPR3T/ TZOODO( 0: 1)
      COMMON /DOPR4T/ TPICDO,THLPDO,TNAMDO
      CHARACTER *2 TPLNDO,TAREDO,TZOODO,TPICDO,THLPDO,TNAMDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
C       TINP =  <+   Switch main help ON
C               <-   Switch help OFF
C               <<   Toggle help
C               ??   Switch main help ON and page 0
C               HE   Type HELP, no store
C               XY   Store XY, if ON, type page 0 for XY, if not yet there
C               ?B   Swich help ON and type page B
C               [[   Ignore all commands except <<. Set during macro execution
C               ]]   Switch on and update.
      PARAMETER (MPTXT=14000,MPAGE=30)   ! change MPTXT in DQHLT also.
      CHARACTER *(*) TINH,TINP,TINPU,TBK,TCR,TFN(*),TLFN(*),TSTAT
      CHARACTER *3 TLAST,T3PGE(MPAGE),T3PGI
      CHARACTER *8 TCPGE(MPAGE),TCPGO
      CHARACTER *3 TCUR,TBACK,TSPA
      DATA TCUR/' '/,TBACK/' '/
      CHARACTER *7 TFI0
      DATA TFI0/'FI0c  8'/
      CHARACTER *48 TINC
      CHARACTER *47 T(MPTXT)

      DIMENSION LNGTH(MPTXT),LEVL(MPTXT)
C  FOF indicates that Help window is not updated.
C  ION is +1 or -1 if Help window is ON or OFF screen.
C  IONMAC is ON/OFF status (ION) when FOF was set to .TRUE.
      LOGICAL FOF,STATUS,FSTRT,FSC
      DATA FOF/.FALSE./,FSTRT/.FALSE./
      DATA ION/1/,NLTOT/48/,NLCOM/44/
      CHARACTER *47 TSEP
      DATA TSEP/'     =====|===================================='/
      CHARACTER *2 TCSC
      CHARACTER *35 TSC1,TSC2
      DATA LEV7/7/

      DATA HD/350./,VD/680./
      DATA NL1/1/

      DATA IDEB/0/,LDEB/0/
      IF(.NOT.FMACDM) FINMDM=.FALSE.
      IF(FINMDM.OR.ISTART.EQ.2) RETURN
      IF(TINP.EQ.'***'.OR.TINP(1:1).EQ.'U') RETURN
      IF(TINP.EQ.'[[') THEN
         IONMAC=ION
         FOF=.TRUE.
         RETURN
      END IF
      IF(TINP.EQ.']]') THEN
         FOF=.FALSE.
         ION=IONMAC
      END IF
      IF(FOF) ION=-1
C                                       Switch OFF
      IF(FOF) THEN
         IF((TINP.EQ.'<-').OR.(TINP.EQ.'<<'.AND.IONMAC.EQ.1)) THEN
            CALL DGHLP('OF',' ',0,0)
            IF(LDEB.EQ.1) CALL DWRT(' 1 OF')
            IONMAC=-1
            RETURN
         END IF
      ELSE
         IF((TINP.EQ.'<-').OR.(TINP.EQ.'<<'.AND.ION.EQ.1).OR.
     &      (TINP.EQ.']]'.AND.IONMAC.EQ.-1)) THEN
            CALL DGHLP('OF',' ',0,0)
            IF(LDEB.EQ.1) CALL DWRT(' 2 OF')
            ION=-1
            IONMAC=0
            RETURN
         END IF
      END IF
C                                        Switch ON and redraw
    1 IF(FOF) THEN
         IF(TINP.EQ.'<+'.OR.(TINP.EQ.'<<'.AND.IONMAC.EQ.-1)) THEN
            IONMAC=1
         END IF
      ELSE
         IF((TINP.EQ.'<+').OR.(TINP.EQ.'<<'.AND.ION.EQ.-1).OR.
     &     (TINP.EQ.']]'.AND.IONMAC.EQ.1)) THEN
            CALL DGHLP('ON',' ',0,0)
            IF(LDEB.EQ.1) CALL DWRT(' ON')
            TLAST=' '
            ION=1
            IONMAC=0
            IF(FSTRT) GO TO 10
         END IF
      END IF
C     ................................... BACKWARD COMPATIBLE WITH B.S.N.
      IF(TINP(1:1).EQ.'?') THEN
        TCUR(3:3)=TINP(2:2)
        IF(FSTRT) GO TO 10
      END IF
C     ................................... BACKWARD COMPATIBLE WITH HD
      L33=LENOCC(TINP)
      IF(L33.LT.3.OR.TINP(3:3).EQ.' ') THEN
        TCUR=TINP(1:2)//'0'
        IF(FSTRT) GO TO 10
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------  DQHL_MENU
CH
      ENTRY DQHL_MENU(TINPU)
CH
CH --------------------------------------------------------------------
CH
      IF(.NOT.FMACDM) FINMDM=.FALSE.
      IF(FINMDM.OR.TINPU.EQ.'***'.OR.TINPU(1:1).EQ.'U') RETURN
      FSTRT=.TRUE.
C     ..................................... TINPU = 'YX '
      IF(     TINPU(3:3).EQ.' ') THEN
        IF(TINPU(1:2).NE.TCUR(1:2)) TCUR=TINPU(1:2)//'0'
      ELSE IF(TINPU(1:2).EQ.' ') THEN
        TCUR(3:3)=TINPU(3:3)
      ELSE
        TCUR=TINPU
      END IF
   10 IF(FOF) RETURN
      IF(TCUR.NE.TLAST.AND.TLAST.NE.' ') TBACK=TLAST

C     ............................. changed when the bar was introduced 15.6.98
C     IF((.NOT.FWIZDW).AND.TCUR.EQ.TLAST) RETURN

      IF(TCUR.EQ.TLAST) THEN
        INV=0
      ELSE
        INV=1
      END IF
      TLAST=TCUR
C     CALL DGHLP('ER',' ',0,0)
      M1=1
      CALL DQHLT(TCUR,T,LNGTH,LEVL,KT,M1,INV,MSTRT)
C     CALL DGHLP('DO',' ',0,0)
      CALL DQH_SEND_ALL
      IF(LDEB.EQ.1) CALL DWRT('CALL DGHLP("DO"," ",0,0)')
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------------------------  DQHL_W
CH
      ENTRY DQHL_W
CH
CH --------------------------------------------------------------------
CH
CH..............---
      IF(FINMDM.OR.ISTART.EQ.2.OR.ION.EQ.-1) RETURN
C     CALL DGHLP('ER',' ',0,0)
      CALL DQHLT(TCUR,T,LNGTH,LEVL,KT,M1,0,MSTRT)
C     CALL DGHLP('DO',' ',0,0)
      CALL DQH_SEND_ALL

      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------------------------  DQHL_R
CH
      ENTRY DQHL_R
CH
CH --------------------------------------------------------------------
CH
CH..............---
      IF(FINMDM.OR.ISTART.EQ.2.OR.ION.EQ.-1) RETURN
C     CALL DGHLP('ER',' ',0,0)
      CALL DQHLT(TCUR,T,LNGTH,LEVL,KT,M1,1,MSTRT)
C     CALL DGHLP('DO',' ',0,0)
      CALL DQH_SEND_ALL
      RETURN
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DQHL0
CH
      ENTRY DQHL0
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
      ISTART=3
  911 IF(TGRADG.EQ.'GKS') THEN
         ISTART=2
         RETURN
      END IF
      CALL DPARGI(81,'USL',LEVUS)
      CALL DGOPEN(NUNIDU,TFILDC//'HELP',2,*996,ISTAT)
  913 IF(IN.EQ.0) THEN
         CALL DGHLP('IN',' ',NLTOT,NLCOM)
         IN=1
      END IF
      NPAGE=0
      KT=1

C 904 READ(NUNIDU,1002,END=909) LEVL(KT),T(KT)
  904 CALL D_READ_INC(TINC,*909)
      READ(TINC,1002) LEVL(KT),T(KT)
 1002 FORMAT(I1,A)
C              KTDEB=1473
C     IF(KT.EQ.KTDEB) CALL DTYANS('Stop in dqhlp','Y',NTDEB)
C     IF(LEVL.GT.LEVUS) GO TO 904
      IF(LEVL(KT).GT.LEV7) GO TO 904
      IF(T(KT)(5:8).EQ.'PAGE') THEN
        NPAGE=NPAGE+1
        IF(NPAGE.LE.MPAGE) THEN
          T3PGE(NPAGE)=T(KT)(1:3)
          TCPGE(NPAGE)=T(KT)(13:18)
        ELSE IF(NPAGE.EQ.MPAGE+1) THEN
          CALL DWRT('DQHLP: special page buffer too small.#')
        END IF
        GO TO 904
      END IF
      IF(T(KT)(11:22).EQ.'| filename 1')
     &   LFIL1=KT
      IF(T(KT)(11:25).EQ.'| last filename')
     &   LFIL2=KT
      IF(T(KT)(1:1).NE.'='.AND.T(KT).NE.';') THEN
         IF(T(KT)(1:2).EQ.'  ') T(KT)(1:2)=T(K1)(1:2)
         IF(T(KT)(3:3).EQ.' ' ) T(KT)(3:3)=T(K1)(3:3)
         IF(T(KT)(4:4).EQ.'+'.AND.T(KT)(5:10).NE.' ') THEN
C          .................. + DS   |   ... =>?A is turned into a command
           T(KT)(4:4)='.'
           T(KT)(46:47)=' '
         END IF
         DO 730 L=47,6,-1
            IF(T(KT)(L:L).NE.' ') GO TO 905
  730    CONTINUE
  905    LNGTH(KT)=L
      ELSE
         KT=KT-1
      END IF
      K1=KT
      KT=KT+1
      IF(KT.LE.MPTXT) GO TO 904
      CALL DWRT_SETUP('TERMINAL=ON')
      CALL DWRT('Too many lines in the help file; last lines:')
      CALL DWRT_SETUP('TERMINAL=LAST')
      DO LT=KT-9,KT-1   !  Upper limit was KT before / B.S.N.
        CALL DWRT(T(LT))
      END DO
      STOP
  909 KT=KT-1
      IF(LEVUS.EQ.7)THEN
        CALL DWRT_SETUP('TERMINAL=OFF')
        WRITE(TXTADW,1910) KT,MPTXT
 1910   FORMAT(I5,'<',I5,' lines in the help file.')
        CALL DWRC
        CALL DWRT_SETUP('TERMINAL=LAST')
      END IF
      CLOSE(UNIT=NUNIDU)
      IF(ISTART.EQ.3) GO TO 815
      GO TO 1
  996 ISTART=2
  815 CALL DPARGI(93,'CHB',IHB)
      CALL DPARGI(93,'CHF',IHF)
      CALL DGHLEV(IHF,IHB)
      RETURN
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQHGT
CH
      ENTRY DQH_STATUS(JON,TSTAT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      JON=ION
      TSTAT=TCUR
      RETURN
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQHGT
CH
      ENTRY DQHGT(TINH,STATUS)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by B.S. Nilsson                  23-Feb-1990
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C   Return Help window status.
C       TINH =  ++   STATUS is .TRUE. if HELP window is not iconized.
C       TINH =  --   STATUS is .TRUE. if HELP window is iconized.
      STATUS=.FALSE.
      IF(TINH.EQ.'++') THEN
        STATUS = ((.NOT. FOF) .AND. (ION.EQ.1)) .OR.
     &           ((      FOF) .AND. (IONMAC.EQ.1))
      ELSE IF(TINH.EQ.'--') THEN
        STATUS = ((.NOT. FOF) .AND. (ION.EQ.-1)) .OR.
     &           ((      FOF) .AND. (IONMAC.EQ.-1))
      ENDIF
      RETURN
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQH_PAGE
CH
      ENTRY DQH_PAGE(T3PGI,TCPGO)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      DO IPAGE=1,NPAGE
        IF(T3PGE(IPAGE).EQ.T3PGI) THEN
          TCPGO=TCPGE(IPAGE)
          RETURN
        END IF
      END DO
      TCPGO=' '
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------  DQHL_LAST_PAGE
CH
      ENTRY DQHL_LAST_PAGE(TBK,TCR)
CH
CH --------------------------------------------------------------------
CH
CH
      TBK=TBACK
      TCR=TCUR
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------  DQHL_CLEAN
CH
      ENTRY DQH_CLEAN
CH
CH ---------------------------------------------------------------------
C
      DO K=1,KT
        T(K)(12:12)=' '
      END DO
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------ DQHL_SCAN_COMMAND
CH
      ENTRY DQHL_SCAN_COMMAND(FSC)
CH
CH --------------------------------------------------------------------
CH
CH
      CALL DT_WRT('<CR>=return. Type command to search for')
      CALL DGETLN(TCSC,NASC,2)
      IF(NASC.NE.2) RETURN

      CALL DPARGI(81,'USL',LEVUS)
      CALL DPARGI(84,'OH3',IHD)
      CALL DPARGI(84,'OV3',IVD)
      CALL DQH_NEW_WINDOW('Search','Search',IHD,IVD,HD,VD,NL1)
      NLIN=0
      DO K=M1,M1+8
        IF(T(K)(4:4).EQ.'0') THEN
          CALL DQH_INDEX_LINE(FSC,NLIN,T(K),T(K+1),' ',' ',0)
          GO TO 201
        END IF
      END DO

  201 TXTADW='page | Local search for command '//TCSC
      IF(FSC) TXTADW(8:12)=' Full'
      CALL DQH_NEW_TEXT(9,TXTADW,0,' ')
      CALL DQH_NEW_TEXT(8,'-',0,' ')

      LEVPA=0
      DO K=1,KT
        IF(T(K)(29:30).EQ.'@@') LEVPA=LEVL(K)
        IF((FSC.OR.T(K)( 1: 2).EQ.THLPDO).AND.
     &     T(K)( 6: 7).EQ.TCSC.AND.
     &     T(K)(11:11).EQ.'|') THEN
          LMAX=MAX(LEVL(K),LEVPA)
          IF(LMAX.GT.LEVUS) THEN
            IF(LMAX.GE.7) GO TO 203
            LGY=1
          ELSE
            LGY=0
          END IF
          CALL DQH_INDEX_LINE(FSC,NLIN,T(K),T(K+1),'command',TCSC,LGY)
          IF(NLIN.EQ.-1) RETURN
          TSPA=T(K)(1:3)
        END IF
  203 END DO
      IF(NLIN.GT.1) THEN
        CALL DQH_NEW_TEXT(8,'-',0,' ')
        TXTADW='page | To get page type GG:page. Example: GG:'//
     &    TSPA(1:2)//'\'//TSPA(3:3)
        CALL DQH_NEW_TEXT(9,TXTADW,0,' ')
      ELSE
        CALL DQH_NEW_TEXT(8,'nothing found',0,' ')
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------ DQHL_SCAN_TEXT
CH
      ENTRY DQHL_SCAN_TEXT(FSC)
CH
CH --------------------------------------------------------------------
CH
CH
      CALL DGETLN(TSC1,LSTR,35)
      IF(LSTR.LT.2) RETURN

      CALL DPARGI(81,'USL',LEVUS)
      CALL DPARGI(84,'OH3',IHD)
      CALL DPARGI(84,'OV3',IVD)
      CALL DQH_NEW_WINDOW('Search','Search',IHD,IVD,HD,VD,NL1)

      NLIN=0
      DO K=M1,M1+8
        IF(T(K)(4:4).EQ.'0') THEN
          CALL DQH_INDEX_LINE(FSC,NLIN,T(K),T(K+1),' ',' ',0)
          GO TO 202
        END IF
      END DO

  202 TXTADW='page | Local search for text '//TSC1
      IF(FSC) TXTADW(8:12)=' Full'
      CALL DQH_NEW_TEXT(9,TXTADW,0,' ')
      CALL DQH_NEW_TEXT(8,'-',0,' ')

      LD=LSTR-1
      L2=35-LD
      CALL CUTOL(TSC1)

      LEVPA=0
      DO K=1,KT
        IF(T(K)(29:30).EQ.'@@') LEVPA=LEVL(K)
        IF((FSC.OR.T(K)(1:2).EQ.THLPDO).AND.T(K)(11:11).EQ.'|') THEN
          TSC2=T(K)(12:47)
          CALL CUTOL(TSC2)
          DO L=1,L2
            IF(TSC2(L:L+LD).EQ.TSC1(1:LSTR)) THEN
              LMAX=MAX(LEVL(K),LEVPA)
              IF(LMAX.GT.LEVUS) THEN
                IF(LMAX.GE.7) GO TO 200
                LGY=1
              ELSE
                LGY=0
              END IF
              CALL DQH_INDEX_LINE(FSC,NLIN,T(K),T(K+1),'text',
     &          TSC1(1:LSTR),LGY)
              IF(NLIN.EQ.-1) RETURN
              TSPA=T(K)(1:3)

              GO TO 200
            END IF
          END DO
        END IF
  200 END DO
      IF(NLIN.GT.1) THEN
        CALL DQH_NEW_TEXT(8,'-',0,' ')
        TXTADW='page | To get page type GG:page. Example: GG:'//
     &    TSPA(1:2)//'\'//TSPA(3:3)
        CALL DQH_NEW_TEXT(9,TXTADW,0,' ')
      ELSE
        CALL DQH_NEW_TEXT(8,'nothing found',0,' ')
      END IF

      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------ DQHL_STORE_PAGE
CH
      ENTRY DQHL_STORE_PAGE
CH
CH --------------------------------------------------------------------
CH
CH
      CALL DPARGI(84,'OH3',IHD)
      CALL DPARGI(84,'OV3',IVD)
      CALL DQH_NEW_WINDOW('Search','Search',IHD,IVD,HD,VD,NL1)
      DO K=M1,M1+8
        IF(T(K)(4:4).EQ.'0') THEN
          CALL DQH_INDEX_LINE(.TRUE.,NLIN,T(K),T(K+1),' ',' ',0)
          RETURN
        END IF
      END DO
      RETURN
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQH_PAGE
CH
      ENTRY DQH_FILE_NAMES(NFN,TLFN,TFN)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     .............................................. not used
      IF(LFIL1.EQ.0.OR.LFIL2.EQ.0) RETURN
      DO KFN=LFIL1,LFIL2+1
        T(KFN)='FI0X'
      END DO
      IFN=LFIL1+1
      DO KFN=1,NFN
        IF(IFN.GT.LFIL2) RETURN
        T(IFN)( 4:11)='. '//TLFN(KFN)//'   |'
        DO LFN=1,70
          IF(TFN(KFN)(LFN:LFN).EQ.' ') GO TO 701
        END DO
  701   LFN=LFN-1
        IF(LFN.LE.34) THEN
          T(IFN)(13:47)=TFN(KFN)(1:34)
        ELSE
          T(IFN)(13:47)=TFN(KFN)(LFN-33:LFN)
        END IF
        LNGTH(IFN)=LENOCC(T(IFN))
        L=INDEX(T(IFN)(13:47),' ')+11
        IFN=IFN+1
        T(IFN)=TFI0                         !  DATA TFI0/'FI0c  8'/
        T(IFN)(13:L)=T(IFN-1)(13:L)
        LNGTH(IFN)=LENOCC(T(IFN))
        IFN=IFN+1
      END DO
      END
*DK DQHLT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQHLT
CH
      SUBROUTINE DQHLT(TCH,T,LNGTH,LEVL,KT,M1,IN1,M)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DH
      COMMON /DHELPT/ TAN1DH,TPRGDH,TLT3DH
      CHARACTER *1 TPRGDH,TLT3DH
      CHARACTER *3 TAN1DH
C------------------------------------------------------------------- DW
      COMMON /DWIZCC/ FWIZDW,FCONDW
      LOGICAL FWIZDW,FCONDW
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C------------------------------------------------------------------- DW
      PARAMETER (MPOSDW=30,MPNWDW=12,MNUWDW=14)
      COMMON /DWINDC/NWINDW(0:MPNWDW,0:MPNWDW),
     &  NEXTDW(0:MPNWDW),LASTDW(0:MPNWDW),
     &  POSIDW(MPOSDW)
      COMMON /DWINDO/TWINDW(0:MNUWDW)
      CHARACTER *1 TWINDW
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DM
      COMMON /DMACRC/ FMACDM,NLETDM,FMOPDM,FMINDM,FANIDM,FNTADM,
     &  FINMDM
      LOGICAL FMACDM,FMOPDM,FMINDM,FANIDM,FNTADM,FINMDM
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION LNGTH(*),LEVL(*)
      CHARACTER *(*) T(*),TANSW
      CHARACTER BACKSLASH
C      PARAMETER (BACKSLASH=CHAR(92))
      PARAMETER (MPTXT=14000)        ! change MPTXT in DQHLP also.
      DIMENSION NACT(MPTXT)
      CHARACTER *1 TEQ,TSTAR
      DATA TSTAR/'*'/
      CHARACTER *2 TCOM(70),TPAGE(70),TSCO,TLC,TUP
      DATA TUP/'<='/
      CHARACTER *3 TCH,TLAST,TLSIN,TSPA,TBACK,TDUM
      CHARACTER *40 TNC
      CHARACTER *47 TT,TNAV,TCONT,TBUT
      DATA TCONT/'   . cw   |*continue Wizard'/
      CHARACTER *1 TNEW,T1,TB,TSEP
      DATA TB/'.'/,TNAV/' '/,TLAST/'GT0'/,TSEP/'.'/J1/1/,J2/0/,LALI0/0/
      DATA INV0/0/,LDEB/0/,KDEB/0/,MDEB/0/
      LOGICAL FCONT,FNBAK,FNPOS
      DATA FNBAK/.TRUE./
      BACKSLASH=CHAR(92)

      CALL DPARGI(81,'USL',LEVUS)
      TLSIN=TLAST
C     ...................... store line of last command
      IF(MDEB.EQ.0.AND.TLSIN.NE.TCH) THEN
        CALL DGHLP_LINE(LALI)
        IF(LALI.GT.0) THEN
          LALI=LALI-LALI0
          DO J=J1,J2
            IF(T(J)(4:4).NE.'C') THEN
              LALI=LALI-1
              IF(LALI.EQ.0) THEN
                IF(T(J)(11:12).EQ.'| '.OR.T(J)(5:6).EQ.'GB') GO TO 3
                GO TO 2
              END IF
            END IF
          END DO
        END IF
C       ............. when going into a new branch all old "*"'s are removed.
    3   DO K=1,KT
          T(K)(12:12)=' '
        END DO
      END IF
    2 NCOM=0
      IF(TCH(3:3).EQ.' ') TCH(3:3)='0'
    1 TLT3DH=TCH(3:3)
      IF(TCH(3:3).EQ.'0') THEN
        T(JNAV)(45:46)=TCH(1:2)
      ELSE
        T(JNAV)(45:46)='?'//TCH(3:3)
      END IF
      DO 700 M=M1,KT
         IF(T(M)(1:3).EQ.TCH) GO TO 27
  700 CONTINUE
      CALL DWRT('Help file: paragraph '//TCH//' not found!')
      TCH=TLAST
      GO TO 1
   27 TLAST=TCH
      IF(LEVUS.EQ.7) THEN
        IPTX=1
      ELSE
        CALL DPARGI(84,'OHL',IPTX)
      END IF
      FNPOS=.TRUE.
      IF(FNBAK) THEN
C       ............................ FNBAK=.FALSE. :  page is called from index
        DO LNA=1,29,7
          IF(TCH.EQ.TNAV(LNA:LNA+2)) THEN
            TNC(1:2)=TNAV(LNA+4:LNA+5)
            LNC=2
            GO TO 28
          END IF
        END DO
      END IF
      TNC='ba'
      LNC=2
   28 MW=0
      IF(FWIZDW) THEN
        CALL DWIZ_NEXT(LNC,TNC,FCONT)
        IF(FCONT) THEN
          IF(TCONT(4:4).NE.'X') THEN
            CALL DQH_SEND_LINE(TCONT,47,1)
            IF(LDEB.EQ.1) THEN
              WRITE(TXTADW,8419) 1,TCONT(1:47)
              CALL DWRC
 8419         FORMAT(I2,A)
            END IF
            MW=1
          END IF
        END IF
      END IF
      DO 710 J=M+MW,KT
        IF(LEVL(J).GT.LEVUS) THEN
          IF(J.EQ.M+MW) THEN
            WRITE(TT,1710) LEVL(J),LEVUS
 1710       FORMAT(20X,'User level =',I2, ' >',I2)
            CALL DWRT(
     &        'The user level of the selected page is to high.#')
            CALL DQH_SEND_LINE(TT,22,0)
          END IF
          GO TO 710
        END IF
        TT=T(J)
        LT=LNGTH(J)
        IF(TT(1:3).NE.TCH) THEN
          M1=M
          J1=M+MW
          J2=J
          RETURN
        END IF
        IF(     TT( 4: 8).EQ.'NAVIG'.AND.TCH.NE.TLSIN) THEN
C         ............. for the current page the commands leading to this
C         ............. page are flagged by a "*". Only 1 line is flagged.
C         ............. It is assumed that pages are increasing, example:
C         ............. YX\0 before YX\Z.
C         ............. Exception: chapter IN starts before calling chapter GT.
          JNAV=J
          TNAV=TT(13:47)
          IF(TNC(1:2).EQ.' ') THEN
C            IF(NBUT.EQ.1) TNC(1:2)=TNAV(33:34)
            TNC(1:2)=TNAV(33:34)
            LNC=2
          END IF
          KCUR=1
          DO LNA=1,29,7
            TSPA=TNAV(LNA:LNA+2)
            TSCO=TNAV(LNA+4:LNA+5)
            IF(TSPA.EQ.' ') GO TO 710
C           search page TSPA
            IF(TSPA(1:2).EQ.'IN') KCUR=1
            DO KSPA=KCUR,KT
              IF(T(KSPA)(1:3).EQ.TSPA) THEN
C               ............................ search page command
                IF(TSCO(1:1).EQ.'?') THEN
                  DO KSCO=KSPA,KT
C                   .................... command not found
                    IF(T(KSCO)(1:3).NE.TSPA) GO TO 281
                    IF(T(KSCO)(46:47).EQ.TSCO) THEN
C                     ................... only this line is flagged by "*"
                      T(KSCO)(12:12)=TSTAR
                    ELSE
C                     ....................... all others are reset
                      T(KSCO)(12:12)=' '
                    END IF
                  END DO
                ELSE
C                 ...................................... search command
                  DO KSCO=KSPA,KT
                    IF(T(KSCO)(1:3).NE.TSPA) GO TO 281
C                   .................... command not found
                    IF(T(KSCO)( 6: 7).EQ.TSCO) THEN
C                     ................... only this line is flagged by "*"
                      T(KSCO)(12:12)='*'
                    ELSE
C                     ....................... all others are reset
                      T(KSCO)(12:12)=' '
                    END IF
                  END DO
                END IF
C               ............. assumption: calling page is higher tha page
  281           KCUR=KSCO
              END IF
            END DO
          END DO
          GO TO 710
        ELSE IF(TT( 4: 8).EQ.'NAVIG') THEN
          GO TO 710
        ELSE IF(TT( 7: 7).EQ.'#'    ) THEN
          TT(7:7)=TWINDW(IAREDO)
        ELSE IF(TT(6:7).EQ.'w-') THEN
          LT=32+IAREDO
          TT(LT:LT)=TWINDW(IAREDO)
        ELSE IF(TT(29:30).EQ.'@@'   ) THEN
          CALL DBR_LAST_PROCESSOR(TT(29:30))
        ELSE IF(TT( 6: 7).EQ.'ba'.AND.KDEB.EQ.1) THEN
          CALL DQHL_LAST_PAGE(TBACK,TDUM)
          TT(44:47)=TBACK(1:2)//BACKSLASH//TBACK(3:3)
        END IF
        DO L=1,LNC,3
          TLC=TNC(L:L+1)
          IF(L.EQ.1.OR.TNC(L-1:L-1).NE.'=') THEN
            TEQ=TNC(L+2:L+2)
            IF(TLC.NE.' ') THEN
              IF(TLC(1:1).EQ.'?') THEN
                IF(TT(46:47).EQ.TLC) GO TO 41
              ELSE
                IF(TT( 6: 7).EQ.TLC) THEN
                  IF(TEQ.NE.'=') GO TO 41
                  IF(TT(8:10).EQ.TNC(L+2:L+4)) GO TO 41
                END IF
              END IF
            END IF
          END IF
        END DO
        INV=0
        IF(FWIZDW) THEN
          IF(TT(6:10).EQ.'=====') THEN
            TT(11:11)='='
          ELSE
            TT(11:11)=TSEP
          END IF
          IF(     TT(4:4).EQ.'.') THEN
            TT(4:4)=' '
          ELSE IF(TT(4:4).EQ.'+') THEN
            TT(4:10)='  =>   '
          END IF
        END IF
        GO TO 42
   41   IF(TT(11:11).EQ.'|') TT(12:12)='*'
        INV=IN1
        FNBAK=.TRUE.
   42   T1=TT(46:46)
        IF(T1.EQ.'>'.OR.T1.EQ.'?'.OR.T1.EQ.'(') THEN
          CALL DQHO2(TCH(1:2),TT(47:47),TNEW)
          TT(46:46)=' '
          IF(IPTX.EQ.0) THEN
            TT(47:47)=' '
          ELSE IF(IPTX.EQ.1) THEN
            TT(47:47)=TNEW
          END IF
          IF(TT(4:4).EQ.'+') TT(6:7)='?'//TNEW
C         ................................... INV=-1 : always inversed
C         ................................... INV= 0 : not inversed
C         ................................... INV= 1 : inversed + pointer
          IF(TT(44:45).EQ.TUP) TT(44:45)='up'
          IF(TT(4:4).NE.'X') CALL DQH_SEND_LINE(TT,LT,INV)
          IF(LDEB.EQ.1) THEN
            WRITE(TXTADW,8419) INV,TT(1:LT)
            CALL DWRC
          END IF
        ELSE
          IF(TT(6:11).NE.'-----|') THEN
            IF(TT(4:4).NE.'X') CALL DQH_SEND_LINE(TT  ,LT,INV)
            IF(LDEB.EQ.1) THEN
              WRITE(TXTADW,8419) INV,TT(1:LT)
              CALL DWRC
            END IF
          ELSE
            IF(TT(4:4).NE.'X') CALL DQH_SEND_LINE(TT  ,LT,INV0)
            IF(LDEB.EQ.1) THEN
              WRITE(TXTADW,8419) INV,TT(1:LT)
              CALL DWRC
            END IF
          END IF
          IF(TT(4:4).EQ.'.') THEN
            NCOM=NCOM+1
            TCOM (NCOM)=TT( 6: 7)
            TPAGE(NCOM)=TT(46:47)
          END IF
        END IF
C       IF(TT(12:12).EQ.'*') T(J)(12:12)=TB
        IF(TT(4:4).EQ.'T') GO TO 30
  710 CONTINUE
      M1=M
      M2=J
      RETURN
C     .................................. DRAW realigned description text
   30 DO L=J,KT
        IF(LEVL(L).LE.LEVUS) THEN
          IF(T(L)(1:3).NE.TCH) THEN
            CALL DQH_TEXT(0,' ')
            M1=M
            RETURN
          END IF
          CALL DQH_TEXT(LNGTH(L),T(L))
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
CH --------------------------------------------------------------------  DQHO2
CH
      ENTRY DQHLT_PAGE(TANSW)
CH
CH --------------------------------------------------------------------
CH
      IF(FMACDM.OR.FINMDM) RETURN
      CALL DGLBUT(IBUT)
      IF(TLAST(3:3).EQ.'1'.OR.
     &   TLAST(3:3).EQ.'2'.OR.
     &   TLAST(3:3).EQ.'3') THEN
         IF(IBUT.EQ.1) THEN
           IF(TANSW.NE.'ba') THEN
             DO I=1,NCOM
               IF(TPAGE(I)(1:1).EQ.BACKSLASH.AND.TCOM(I).EQ.TANSW) THEN
                 TANSW=TPAGE(I)
                 FNBAK=.FALSE.
                 CALL DWR_BACKSPACE_SLASH(2)
                 CALL DWR_ADD(TANSW)
                 RETURN
               END IF
             END DO
           END IF
         END IF
      END IF
      END
*DK DQH_TEXT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQ_TEXT
CH
      SUBROUTINE DQH_TEXT(LT,TT)
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
C ---------------------------------------------------------------------
C
C123456789 123456789 123456789 123456789 123456789
C   . GW   | Get processor of window   whole ___ // PP
C     123456789 123456789 123456789 123456789 123456789
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *(*) TT
      CHARACTER *1 TB
      CHARACTER *50 TOUT
      CHARACTER *90 TS
      DATA TB/' '/,TOUT/' '/,TS/' '/
      DATA L5/5/,L6/6/,L11/11/,L12/12/,L13/13/,L45/45/,L47/47/,L50/50/
      DATA LDEB/0/
      IF(LT.EQ.0) THEN
        IF(LS.NE.0) THEN
          TOUT(L6:L50)=TS(1:LS)
          IF(TOUT(4:4).NE.'X') CALL DQH_SEND_LINE(TOUT,LS+L5,0)
          IF(LDEB.EQ.1) THEN
            WRITE(TXTADW,8419) 'A',TOUT(1:LS+L5)
 8419       FORMAT(A,1X,A)
            CALL DWRC
          END IF
        END IF
        TS=' '
        LS=0
      ELSE
        IF(TT(L13:L47).EQ.' ') THEN
          IF(LS.NE.0) THEN
            TOUT(L6:L50)=TS(1:LS)
            IF(TOUT(4:4).NE.'X') CALL DQH_SEND_LINE(TOUT,LS+L5,0)
            IF(LDEB.EQ.1) THEN
              WRITE(TXTADW,8419) 'B',TOUT(1:LS+L5)
              CALL DWRC
              CALL DWRT('B')
            END IF
            TS=' '
            LS=0
            CALL DQH_SEND_LINE('      ',6,0)
          END IF
        ELSE
          IF(LS.EQ.0) THEN
            TS=TT(L13:L47)
            LS=LT-L12
          ELSE
            TS=TS(1:LS)//TB//TT(L13:L47)
            LS=LS+LT-L11
          END IF
          IF(     LS.EQ.L45) THEN
            TOUT(L6:L50)=TS(1:LS)
            IF(TOUT(4:4).NE.'X') CALL DQH_SEND_LINE(TOUT,LS+L5,0)
            IF(LDEB.EQ.1) THEN
              WRITE(TXTADW,8419) 'C',TOUT(1:LS+L5)
              CALL DWRC
            END IF
            LS=0
          ELSE IF(LS.GT.L45) THEN
            DO L=L45,1,-1
              IF(TS(L:L).EQ.' ') THEN
                TOUT(L6:L50)=TS(1:L-1)
                IF(TOUT(4:4).NE.'X')
     &            CALL DQH_SEND_LINE(TOUT,L-1+L5,0)
                IF(LDEB.EQ.1) THEN
                  WRITE(TXTADW,8419) 'D',TOUT(1:LS+L5)
                  CALL DWRC
                END IF
                TS=TS(L+1:LS)
                LS=LS-L
                GO TO 9
              END IF
            END DO
          END IF
        END IF
      END IF
    9 END
*DK DQH_SEND_LINE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++ DQH_SEND_LINE
CH
      SUBROUTINE DQH_SEND_LINE(T,LT,INV)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
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
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77

C     How to find graphical letters:
C     find font in DALB_X.C. Example

C     static char helpfontname[6][80] = {
C       "-misc-fixed-medium-r-normal--13-120-75-75-c-70-iso8859-1",

C     Type on a UNIX window:

C     AFAL11:~ xfd -fn -misc-fixed-medium-r-normal--13-120-75-75-c-70-iso8859-1

C     A xfd window appears: numbering of letters:
C      0 |  1 | ... | 15 |
C     16 | 17 | ... | 31 | ...   Example: 80 = P    T = CHAR(80) => T='P'

C     .......                   =     off      on      active
C     ....... variables         x      o        O       # =       :x
C     ....... variables         =      x        x       - =       =x
C     ....... active = bold,  non active = not bold

C     ....... modes 1<2         x      >        <        x        /
C     ....... modes 1<*         x      >        <        x        /
C     ....... On = bold,  off = not bold


C     ....... macros=others     x      x        x        x     M  M
C     ....... executables       x      x        x        x     X  X

      CHARACTER *(*) T,TP1(*),TP2(*),T2
      DIMENSION PP1(4,*),PP2(4,*)
      PARAMETER (MN=-19,MP=99)
      CHARACTER *2 TM,TM1,TM2,TCOM,TA(MN:MP),TVAR,TAR
      DATA TM1/'TX'/,TM2/'TX'/,TAR/'->'/
      DIMENSION P4(MN:MP)
      CHARACTER *47 TT,TTT
      PARAMETER (MSTOR=105)
      CHARACTER *50 TSTOR(MSTOR),TSC
      DIMENSION INVST(MSTOR)
      DATA TSTOR/MSTOR*' '/,INVST/MSTOR*0/
      CHARACTER *1 TC,TS,TB
      DATA TB/' '/

C          v                                     v    v
C      'D30+ ?1     Command list                   =>  '
C            =>

C       D30. GT   |  GO TO                             '
C      'D30+ Command list                   =>  |?1    '
C       123456789 123456789 123456789 123456789 12345678
C      'D30+  Command list                   =>  |?1    '


      DATA N1/5/,N2/11/,N3/47/,IDEB/0/
      DATA J1/42/

      DATA IVEQ/61/,IMON/2/,IMOF/183/,IBAR/2/
      DATA IEXE/120/,ISUP/183/,ISEP/32/
      LOGICAL FBOLD,FNF,FSTOR
      DATA FBOLD/.FALSE./,FSTOR/.FALSE./,NSTOR/0/

      TT=T

      TC=' '
      TCOM=TT(6:7)

      IF(FBOLD) THEN
        FBOLD=.FALSE.
        IF(TT(4:4).EQ.'c') THEN
          TT(4:4)='C'
          TS=' '
          GO TO 6
        END IF
      END IF

      IF(TT(4:4).EQ.'C'.OR.TT(4:4).EQ.'c') THEN
        TS=' '
        GO TO 6
      END IF

      IF(TT(5:5).NE.' ') THEN
        IF(TT(4:4).EQ.'.') THEN
          READ(TT(5:5),1001,ERR=3) KVAR
 1001     FORMAT(I1)
        ELSE
          READ(TT(4:5),1002,ERR=3) KVAR
 1002     FORMAT(I2)
        END IF
        RVAR=KVAR
        DO N=N_1_DA,N_2_DA
          IF(TPOPDA(N).EQ.TT(6:7)) THEN
            IF(PARADA(2,N).EQ.RVAR) THEN
              TC=CHAR(IMON)
              GO TO 3
            END IF
          END IF
        END DO
    3   TT(4:5)='. '
        TS=CHAR(ISUP)
      ELSE IF(TT(4:4).EQ.'.') THEN
        TS=CHAR(ISUP)
      ELSE
        TS=CHAR(ISEP)
      END IF

      IF(     TT(8:9).EQ.':x'.OR.T(8:9).EQ.'/x') THEN

        TS=CHAR(IVEQ)
        DO K=JACT,IACT
          IF(TCOM.EQ.TA(K)) THEN
            IF(P4(K).LT.0.) THEN
              TT( 8: 8)='='
              TC=CHAR(IMOF)
            ELSE
              TT( 8: 8)='='
              TC=CHAR(IMON)
              FBOLD=.TRUE.
            END IF
            GO TO 5
          END IF
        END DO

      ELSE IF(TT(8:9).EQ.': ') THEN

        DO K=JACT,IACT
          IF(TCOM.EQ.TA(K)) THEN
            IF(P4(K).LT.0.) THEN
              TC=CHAR(IMOF)
            ELSE
              TC=CHAR(IMON)
              FBOLD=.TRUE.
            END IF
            GO TO 5
          END IF
        END DO
        TT( 8: 8)=' '

      ELSE IF(TT(9:10).EQ.'ON') THEN

        TS=CHAR(IVEQ)
        DO K=JACT,IACT
          IF(TCOM.EQ.TA(K)) THEN
            IF(P4(K).LT.0.) THEN
              TC=CHAR(IMOF)
            ELSE
              TC=CHAR(IMON)
              FBOLD=.TRUE.
            END IF
            GO TO 5
          END IF
        END DO

      ELSE IF(TT(9:10).EQ.'OF') THEN

        TS=CHAR(IVEQ)
        DO K=JACT,IACT
          IF(TCOM.EQ.TA(K)) THEN
            IF(P4(K).LT.0.) THEN
              TC=CHAR(IMON)
              FBOLD=.TRUE.
            ELSE
              TC=CHAR(IMOF)
            END IF
            GO TO 5
          END IF
        END DO

      ELSE IF(TT(8:9).EQ.'=x') THEN

        TS=CHAR(IVEQ)

      ELSE IF(TT(8:9).EQ.'/ ') THEN

        DO K=JACT,IACT
          IF(TCOM.EQ.TA(K)) THEN
            IF(P4(K).GT.0.) THEN
              TC=CHAR(IMON)
              FBOLD=.TRUE.
            END IF
            TT( 8: 8)=' '
            GO TO 5
          END IF
        END DO
        TC=CHAR(IMOF)
        TT( 8: 8)=' '

      ELSE IF(TT(11:11).EQ.'x ') THEN

        TS=CHAR(IEXE)

      END IF

    5 IF(TT(8:8).EQ.'/') THEN
        TM=TM1
      ELSE
        TM=TM2
      END IF

    6 TXTADW=TT(1:N1-1)//TC//TT(N2+1:N3)//TS//TB//TT(N1+1:N2-1)
      IF(TXTADW(44:44).EQ.'?') TXTADW(42:43)=TAR
    7 NSTOR=NSTOR+1
      IF(TSTOR(NSTOR).NE.TXTADW) THEN
        FSTOR=.TRUE.
        TSTOR(NSTOR)=TXTADW
      END IF
      IF(INV.NE.INVST(NSTOR)) THEN
        FSTOR=.TRUE.
        INVST(NSTOR)=INV
      END IF

      IF(IDEB.EQ.1) CALL DWRC
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------- DQH_SEND_ALL
CH
      ENTRY DQH_SEND_ALL
CH
CH --------------------------------------------------------------------
CH
      IF(FSTOR.OR.NSTOR.NE.NSTL) THEN
        CALL DGHLP('ER',' ',0,0)
        DO N=1,NSTOR
          L=LENOCC(TSTOR(N))
          CALL DGHLP('TX',TSTOR(N),L,INVST(N))
        END DO
        CALL DGHLP('DO',' ',0,0)
      END IF
      FSTOR=.FALSE.
      NSTL=NSTOR
      NSTOR=0
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------- DQH_SEND_ALL_SLEEP
CH
      ENTRY DQH_SEND_ALL_SLEEP
CH
CH --------------------------------------------------------------------
CH
      CALL DGHLP('ER',' ',0,0)
      DO N=1,NSTL
        L=LENOCC(TSTOR(N))
        IF(TSTOR(N)(4:4).EQ.'.'.OR.TSTOR(N)(4:4).EQ.'+') THEN
          TSC=TSTOR(N)
          TSC(4:4)=' '
          TSC(J1:L)=' '
          CALL DGHLP('TX',TSC,  L,INVST(N))
        ELSE
          CALL DGHLP('TX',TSTOR(N),L,INVST(N))
        END IF
      END DO
      CALL DGHLP('DO',' ',0,0)
      FSTOR=.TRUE.
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------------- DQH_ON
CH
      ENTRY DQH_ON(T2)
CH
CH --------------------------------------------------------------------
CH
      IF(T2.EQ.' ') THEN
        JACT=1
        IACT=0
      ELSE
        JACT=JACT-1
        IF(JACT.LE.MN) THEN
          CALL DWRT('Error in DQH_ACTIVE: Call H.Drevermann#')
          RETURN
        END IF
        TA(JACT)=T2
        P4(JACT)=1.
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------------- DQH_ACTIVE
CH
      ENTRY DQH_ACTIVE(NPOS,NSP,IP1,NP1,TP1,PP1,IP2,NP2,TP2,PP2)
CH
CH --------------------------------------------------------------------
CH
      IACT=0
      DO K=IP1,NP1
        IF(TP1(K).NE.' ') THEN
          IF(IACT.GE.MP) THEN
            CALL DWRT('Error in DQH_ACTIVE: Call H.Drevermann#')
            RETURN
          END IF
          IACT=IACT+1
          TA(IACT)=TP1(K)
          P4(IACT)=PP1(4,K)
        END IF
      END DO
      DO K=IP2,NP2
        IF(TP2(K).NE.' ') THEN
          IF(IACT.GE.MP) THEN
            CALL DWRT('Error in DQH_ACTIVE: Call H.Drevermann#')
            RETURN
          END IF
          IACT=IACT+1
          TA(IACT)=TP2(K)
          P4(IACT)=PP2(4,K)
        END IF
      END DO


      IF(NPOS.GT.0) THEN
        IF(     NSP.EQ.1) THEN
          TVAR=TP1(NPOS)
        ELSE
          TVAR=TP2(NPOS)
        END IF
      ELSE
        TVAR='~~'
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------- DQH_ACTIVE
CH
      ENTRY DQH_DEPENDENT(T2,FNF)
CH
CH --------------------------------------------------------------------
CH
      DO K=1,IACT
        IF(T2.EQ.TA(K)) THEN
          IF(P4(K).GT.0.) THEN
            FNF=.FALSE.
            RETURN
          END IF
        END IF
      END DO
      DO K=JACT,0
        IF(T2.EQ.TA(K)) THEN
          FNF=.FALSE.
          RETURN
        END IF
      END DO
      FNF=.TRUE.
      END
*DK DQHO1
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQHO1
CH
      SUBROUTINE DQHO1(TSTRT)
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
      CHARACTER *1 TSTRT,TS,T1,TNEW
      CHARACTER *2 TP,TCH
      CHARACTER *3 T3
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DQHO2
CH
      ENTRY DQHO2(TP,TS,TNEW)
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
      TCH=TP
      TNEW=TS
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DQHO3
CH
      ENTRY DQHO3(T1,T3)
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
      T3=TCH//T1
      END
*DK DQHO1
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQHO1
CH
      SUBROUTINE dDQHO1(TSTRT)
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
      CHARACTER *1 TSTRT,TSTR,TS,T1,TNEW
      CHARACTER *2 TP
      CHARACTER *3 TORD(48:90),T3
      TSTR=TSTRT
      IF(TSTRT.EQ.'A') THEN
         DO L=48,90
            TORD(L)='   '
         END DO
         MAZ=65
         M19=49
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DQHO2
CH
      ENTRY dDQHO2(TP,TS,TNEW)
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
      IF(TSTR.EQ.'A') THEN
         TORD(MAZ)=TP//TS
         TNEW=CHAR(MAZ)
         TORD(48)=TORD(MAZ)
         MAZ=MAZ+1
         IF(MAZ.GT.90) MAZ=90
      ELSE
         TORD(M19)=TP//TS
         TNEW=CHAR(M19)
         M19=M19+1
         IF(M19.GE.58) M19=49
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DQHO3
CH
      ENTRY dDQHO3(T1,T3)
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
      J=ICHAR(T1)
      T3=TORD(J)
      IF(J.GT.64) TORD(48)=T3
      END
*DK D_READ_INC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ D_READ_INC
CH
      SUBROUTINE D_READ_INC(T,*)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
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
C------------------------------------------------------------------  DC
C     HARD WIRED IN P_DALB_ALEPH.FOR           !!!
C     ++++++++++++++++++++++++++ *12 TFILDC
      COMMON /DCFTVT/ TFILDC,TITLDC,TDEFDC,TNAMDC
      CHARACTER  *8 TFILDC
      CHARACTER *10 TITLDC
      CHARACTER  *3 TDEFDC
      CHARACTER  *4 TNAMDC
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *(*)T
      CHARACTER*80 TSCR
      LOGICAL FNC
      DATA FNC/.TRUE./

    1 IF(FNC) THEN
C       .................................................... read master file
        READ(NUNIDU,1000,END=99) T
 1000   FORMAT(A)
        IF(T(12:18).NE.'INCLUDE') RETURN

C       .................................................... open include file
        LI=LENOCC(T)
        TSCR=TFILDC//T(20:LI)
        CALL DGOPEN(NUNIDU+1,TSCR,2,*98,ISTAT)

      END IF
      FNC=.FALSE.
C     ...................................................... read include file
      READ(NUNIDU+1,1000,END=9) T
      RETURN

    9 CLOSE(UNIT=NUNIDU+1)
      FNC=.TRUE.
      GO TO 1

   98 TXTADW='not found:'//T(1:LI)
      FNC=.TRUE.
      GO TO 1

   99 RETURN 1
      END
*DK DQHL_OTHER_TEXT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++ DQHL_OTHER_TEXT
CH
      SUBROUTINE DQHL_OTHER_TEXT(NUM,T,NINV)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DH
      COMMON /DHELPT/ TAN1DH,TPRGDH,TLT3DH
      CHARACTER *1 TPRGDH,TLT3DH
      CHARACTER *3 TAN1DH
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *(*) T(*)
      CHARACTER *3 TSTAT

      IF(NUM.GT.0) THEN
        CALL DQH_STATUS(ION,TSTAT)
        IF(ION.LT.0) CALL DQHLP('<<')
C       CALL DGHLP('ER',' ',0,0)
        DO N=1,NUM
          LT=LENOCC(T(N))
          IF(N.EQ.NINV) THEN
            CALL DQH_SEND_LINE(T(N),LT,1)
          ELSE
            CALL DQH_SEND_LINE(T(N),LT,0)
          END IF
        END DO
C       CALL DGHLP('DO',' ',0,0)
        CALL DQH_SEND_ALL
        TAN1DH='OF'
      ELSE
        IF(ION.LT.0) THEN
          ION=0
          CALL DQHLP('<<')
        END IF
      END IF
      END
*DK DQH_NEW_WINDOW
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DQH_NEW_WINDOW
CH
      SUBROUTINE DQH_NEW_WINDOW(TITLE,TNAME,IHD,IVD,HD,VD,L1)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DM
      COMMON /DMACRC/ FMACDM,NLETDM,FMOPDM,FMINDM,FANIDM,FNTADM,
     &  FINMDM
      LOGICAL FMACDM,FMOPDM,FMINDM,FANIDM,FNTADM,FINMDM
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *(*) T1,T2,TITLE,TNAME
      CHARACTER *49 TLIN
      DATA TLIN/'-----|-------------------------------------------'/

      DATA H/3./,DV/15./
      DATA NEW/0/,NOLD/0/,IDALI/0/,IHLP/2/,JHL/0/
      DATA NEWS/3/

      IF(NEW.GT.0) THEN
        CALL DGDWIN(NEW,KEW)
        NEW=0
      END IF
      CALL DGQINF(MODW,IWIN)
      IF(NEW.LE.0) THEN
        IF(FMACDM) RETURN
        NEW=NEWS
        CALL DGCWIN(1.,1.,HD,VD,IHD,IVD,NEW,TITLE,TNAME)
      END IF
      V1=VD
      H1=HD
      GO TO 202
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------ DQH_NEW_AREA
CH
      ENTRY DQH_NEW_AREA(L1)
CH
CH --------------------------------------------------------------------
CH
      CALL DGSWIN(NEW,KEW,0)
  202 CALL DO_BAR_PS_1
      CALL DGLEVL(L1)
      CALL DQFAR(0.,0.,H1,V1)
      V=V1
      CALL DO_BAR_PS_2
      CALL DGSWIN(NOLD,KEW,0)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------ DQH_NEW_TEXT
CH
      ENTRY DQH_NEW_TEXT(NCOL1,T1,NCOL2,T2)
CH
CH --------------------------------------------------------------------
CH
      IF(NEW.NE.0) THEN
        CALL DO_BAR_PS_1

        CALL DGSWIN(NEW,KEW,0)

        CALL DGLEVL(NCOL1)
        L=LENOCC(T1)
        V=V-DV
        IF(T1.NE.'-') THEN
          CALL DGTEXT(H,V,T1,L)
        ELSE
          CALL DGTEXT(H,V,TLIN,49)
        END IF
        IF(NCOL2.GT.0) THEN
          CALL DGLEVL(NCOL2)
          L=LENOCC(T2)
          CALL DGTEXT(H,V,T2,L)
        END IF

        CALL DGSWIN(NOLD,KEW,0)
        CALL DO_BAR_PS_2
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------ DQH_NEW_WINDOW_OF
CH
      ENTRY DQH_NEW_WINDOW_OF
CH
CH --------------------------------------------------------------------
CH
      IF(NEW.NE.0) THEN
        CALL DGDWIN(NEW,KEW)
        NEW=0
      END IF
      END
*DK DQH_INDEX_LINE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DQH_INDEX_LINE
CH
      SUBROUTINE DQH_INDEX_LINE(FSC,NLIN,T1,T2,TEXT,TS,LGREY)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
C     Write line T1 to index window
C                T2 = color line if T2(4:4) = 'c' or .C
C     TEXT = 'command' or 'text'
C     TS = searched text or command
C     TC = current page
C     NLIN = # of line

      CHARACTER *(*) T1,T2,TEXT,TS
      CHARACTER *55,TS1,TS2,TI1,TI2,TP1,TP2
      CHARACTER *50 TCONT
      DATA TCONT/'------ Type C = continue , else STOP ------------#'/
      DATA MLIN/40/,ICOL1/28/,NL1/1/,L8/8/,L7/7/
      LOGICAL FSC

      NLIN=NLIN+1
      IF(NLIN.GT.MLIN) THEN
        CALL DQH_NEW_TEXT(8,'-',0,' ')
        CALL DQH_NEW_TEXT(9,'page | To get page type S:GG:page',0,' ')
        CALL DQH_NEW_TEXT(8,TCONT,0,' ')
        CALL DTYANS(TCONT,'C',NANSW)
        IF(NANSW.NE.1) THEN
          NLIN=-1
          RETURN
        END IF
        NLIN=2
        CALL DQH_NEW_AREA(NL1)
        CALL DQH_NEW_TEXT(KCOL1,TP1,KCOL2,TP2)
        TXTADW='page | Local search for '//TEXT//' '//TS
        IF(FSC) TXTADW(8:12)=' Full'
        CALL DQH_NEW_TEXT(9,TXTADW,0,' ')
      END IF

      TS1=T1
      TS1(3:4)='\'//TS1(3:3)

      IF(LGREY.EQ.0) THEN
        NCOL1=ICOL1
        NCOL2=0
        IF(T2(4:4).EQ.'c'.or.T2(4:4).EQ.'C') THEN
          READ(T2(6:7),2000,ERR=9) NCOL2
 2000     FORMAT(I2)
          TS2=T2
          TS2(1:7)=' '
          LS2=LENOCC(TS2)
          DO L=8,LS2
            IF(TS2(L:L).NE.' ') TS1(L:L)=' '
          END DO
          TI2='      '//TS2(12:47)
        END IF
      ELSE
        NCOL1=L7
        NCOL2=0
      END IF

      TI1=TS1(1:4)//' |'//TS1(12:47)//'  '//TS1(6:10)
    9 IF(TEXT.EQ.' ') THEN
        KCOL1=NCOL1
        KCOL2=NCOL2
        TP1=TI1
        TP2=TI2
      END IF
      CALL DQH_NEW_TEXT(NCOL1,TI1,NCOL2,TI2)
      END
