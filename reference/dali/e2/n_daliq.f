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
      INCLUDE 'DALI_CF.INC'
C       TINP =  <+   Switch main help ON
C               <-   Switch help OFF
C               <<   Toggle help
C               ??   Switch main help ON and page 0
C               HE   Type HELP, no store
C               XY   Store XY, if ON, type page 0 for XY, if not yet there
C               ?B   Swich help ON and type page B
C               [[   Ignore all commands except <<. Set during macro execution
C               ]]   Switch on and update.
      PARAMETER (MPTXT=20000,MPAGE=30)
      CHARACTER *(*) TINH,TINP,TINPU,TBK,TCR
      CHARACTER *3 TLAST,T3PGE(MPAGE),T3PGI
      CHARACTER *8 TCPGE(MPAGE),TCPGO
      CHARACTER *3 TCUR,TBACK
      DATA TCUR/' '/,TBACK/' '/
      CHARACTER *47 T(MPTXT)
      DIMENSION LNGTH(MPTXT)
C  FOF indicates that Help window is not updated.
C  ION is +1 or -1 if Help window is ON or OFF screen.
C  IONMAC is ON/OFF status (ION) when FOF was set to .TRUE.
      LOGICAL FCOL(MPTXT)
      LOGICAL FOF,STATUS,FSTRT
      DATA FOF/.FALSE./,FSTRT/.FALSE./
      DATA ION/1/
      CHARACTER *47 TSEP
      DATA TSEP/'     =====|===================================='/
      DATA IDEB/0/
      IF(.NOT.FMACDM) FINMDM=.FALSE.
      IF(FINMDM.OR.ISTART.EQ.2) RETURN
      IF(TINP.EQ.'***') RETURN
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
            IONMAC=-1
            RETURN
         END IF
      ELSE
         IF((TINP.EQ.'<-').OR.(TINP.EQ.'<<'.AND.ION.EQ.1).OR.
     &      (TINP.EQ.']]'.AND.IONMAC.EQ.-1)) THEN
            CALL DGHLP('OF',' ',0,0)
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
      IF(FINMDM.OR.TINPU.EQ.'***') RETURN
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
      IF((.NOT.FWIZDW).AND.TCUR.EQ.TLAST) RETURN
      TLAST=TCUR
      CALL DGHLP('ER',' ',0,0)
      M1=1
      CALL DQHLT(TCUR,T,LNGTH,KT,M1,1,FCOL,MSTRT)
      CALL DGHLP('DO',' ',0,0)
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
      CALL DGHLP('ER',' ',0,0)
      CALL DQHLT(TCUR,T,LNGTH,KT,M1,0,FCOL,MSTRT)
      CALL DGHLP('DO',' ',0,0)
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
      CALL DGHLP('ER',' ',0,0)
      CALL DQHLT(TCUR,T,LNGTH,KT,M1,1,FCOL,MSTRT)
      CALL DGHLP('DO',' ',0,0)
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
      CALL DGOPEN(NUNIDU,TFILDC//'HLP',2,*996,ISTAT)
  913 IF(IN.EQ.0) THEN
         CALL DGHLP('IN',' ',0,0)
         IN=1
      END IF
      NPAGE=0
      KT=1
  904 READ(NUNIDU,1002,END=909) LEVL,T(KT)
 1002 FORMAT(I1,A)
      IF(LEVL.GT.LEVUS) GO TO 904
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
      IF(LEVL.NE.LEVUS) THEN
         FCOL(KT)=.TRUE.
      ELSE
         FCOL(KT)=.FALSE.
      END IF
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
      SUBROUTINE DQHLT(TCH,T,LNGTH,KT,M1,IN1,FCOL,M)
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
      INCLUDE 'DALI_CF.INC'
      DIMENSION LNGTH(*)
      CHARACTER *(*) T(*),TANSW
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
      LOGICAL FCOL(1),FCONT,FNBAK,FNPOS
      DATA FNBAK/.TRUE./
C      CALL DGLBUT(NBUT)
C      CALL DGSBUT(1)
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
      CALL DPARGI(81,'USL',LEVUS)
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
      CALL DWRT('User level might be wrong. To change: GT:UL#')
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
          CALL DGHLP('TX',TCONT,47,1)
          IF(LDEB.EQ.1) THEN
            WRITE(TXTADW,8419) 1,TCONT(1:47)
            CALL DWRC
 8419       FORMAT(I2,A)
          END IF
          MW=1
        END IF
      END IF
      DO 710 J=M+MW,KT
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
          TT(44:47)=TBACK(1:2)//'\'//TBACK(3:3)
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
   42   IF(.NOT.FCOL(J)) THEN
          IF(TT(11:12).EQ.'| ') TT(11:12)='|>'
        END IF
        T1=TT(46:46)
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
          CALL DGHLP('TX',TT,LT,INV)
          IF(LDEB.EQ.1) THEN
            WRITE(TXTADW,8419) INV,TT(1:LT)
            CALL DWRC
          END IF
        ELSE
          IF(TT(6:11).NE.'-----|') THEN
            CALL DGHLP('TX',TT  ,LT,INV)
            IF(LDEB.EQ.1) THEN
              WRITE(TXTADW,8419) INV,TT(1:LT)
              CALL DWRC
            END IF
          ELSE
            CALL DGHLP('TX',TT  ,LT,INV0)
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
C     .................................. DRAW realigned descripion text
   30 DO L=J,KT
        IF(T(L)(1:3).NE.TCH) THEN
          CALL DQH_TEXT(0,' ')
          M1=M
          RETURN
        END IF
        CALL DQH_TEXT(LNGTH(L),T(L))
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
               IF(TPAGE(I)(1:1).EQ.'\'.AND.TCOM(I).EQ.TANSW) THEN
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
C      IF(IBUT.EQ.3   .AND.
C     &  TANSW.NE.'ba'.AND.
C     &  (.NOT.FMACDM).AND. 
C     &  (.NOT.FINMDM).AND.
C     &  NLETDM.EQ.2)       THEN
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
      INCLUDE 'DALI_CF.INC'
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
          CALL DGHLP('TX',TOUT,LS+L5,0)
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
            CALL DGHLP('TX',TOUT,LS+L5,0)
            IF(LDEB.EQ.1) THEN
              WRITE(TXTADW,8419) 'B',TOUT(1:LS+L5)
              CALL DWRC
              CALL DWRT('B')
            END IF
            TS=' '
            LS=0
            CALL DGHLP('TX','      ',6,0)
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
            CALL DGHLP('TX',TOUT,LS+L5,0)
            IF(LDEB.EQ.1) THEN
              WRITE(TXTADW,8419) 'C',TOUT(1:LS+L5)
              CALL DWRC
            END IF
            LS=0
          ELSE IF(LS.GT.L45) THEN
            DO L=L45,1,-1
              IF(TS(L:L).EQ.' ') THEN
                TOUT(L6:L50)=TS(1:L-1)
                CALL DGHLP('TX',TOUT,L-1+L5,0)
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
*DK DQRPO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQRPO
CH
      SUBROUTINE DQRPO
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
      CALL DGOPEN(NUNIDU,TFILDC//TGRADG,2,*4,ISTAT)
C                                       !TWINDW(0:MPNWDW) MPNWDW=12
C                                       !definition window (13)
    2 READ(NUNIDU,1011) TWINDW
 1011 FORMAT(15(1X,A))
C                                       !MPOSDW=30
      DO   710  N=1,MPOSDW
         READ(NUNIDU,1009) M,POSIDW(M)
 1009    FORMAT(I2,1X,F14.7)
  710 CONTINUE
      CLOSE(NUNIDU)
      RETURN
 4    CALL DWRT('File '//TFILDC//TGRADG//' not found.')
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQZOM
CH
      SUBROUTINE DQZOM(TMOD,TA,AS,HH,VV,BMODE,NYES)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modified by B.S. Nilsson                   16-Feb-1990
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
C     TMOD(MODE) MOD1DZ MOD2DZ
C      RS   Rubber band Select view before with cursor
C      RB   Execute Rubber band
C      NS   1     0      0    Normal rectangle (Macintosh) or square
C      NR   2     0      0    Normal rectangle (Macintosh) or square
C      US   3     1      0    Uncentered square
C      CS   4     1      1    Centered   square
C      UR   5     2      0    Uncentered rectangle free aspect ratio
C      CR   6     2      1    Centered rectangle with fixed aspect ratio
C      HP   7     3      0    Horizontal parallelogram
C      CP   8     3      1    Centered   horizontal parallelogram ??
C      UV   9     4      0    Vertical   parallelogram
C      CV  10     4      1    Centered   vertical parallelogram ??
C                  C      BR  Rotate base
C                  C      BI  Base inversed
C Rubber Band   Undo
C     RB         RU
C  Box is Cleared, redrawn, Fixed, New box is redrawn
C            BC      BX       BF      BN
C      COMMON/XZOOM/ QASRDZ,MOD1DZ,MOD2DZ,NDIRDZ,FSTRDZ,NWINDZ
C      LOGICAL FSTRDZ,FDUM,FB
      LOGICAL FDUM
      DIMENSION H(6),V(6),HH(4),VV(4)
      DIMENSION HLU(5),VLU(5),HLD(5),VLD(5)
      CHARACTER *2 TMOD(10),TA
      INTEGER BUTSTA
      COMMON /ASTCM1/ BUTSTA
      NYES=0
      CALL DO_STR('RB"CB"FB')
      CALL DO_STR_LIST(10,TMOD,'rubber modes')
      IF(TA.EQ.'RB') GO TO 10
      DO K=1,10
         IF(TA.EQ.TMOD(K)) THEN
            BMODE=K
            NYES=2
            IZOMDO=1
            RETURN
         END IF
      END DO
      IF(TA.EQ.'CB') THEN
         CALL DGZOOM(0,0,0,0)
         NYES=1
         RETURN
      END IF
C      IF(TA.EQ.'LB') THEN
C         CALL DGZOOM(4,0,0,0)
C         NYES=1
C         RETURN
C      END IF
      IF(TA.EQ.'FB') THEN
         CALL DGZOOM(5,0,0,0)
         NYES=1
         RETURN
      END IF
C      IF(TA.EQ.'UR') THEN
C         CALL UCOPY(HLU,HH,4)
C         CALL UCOPY(VLU,VV,4)
C         NYES=2
C         RETURN
C      END IF
      NYES=0
      RETURN
   10 MOD1DZ=BMODE
      CALL UCOPY(HH,HLU,4)
      CALL UCOPY(VV,VLU,4)
      NWINDZ=-99
      FSTRDZ=.TRUE.
      IF(AS.EQ.0.) THEN
         QASRDZ=0.5
      ELSE
         QASRDZ=0.5/AS
      END IF
      CALL DGZOOM(1,0,H,V)
      IF(BUTSTA.LT.0) THEN
C  Rubberband request terminated before completion.
         BUTSTA=0
         NYES=1
         RETURN
      ENDIF
C      IF(NWINDZ.LT.0) THEN
C         NYES=-1
C         RETURN
C      END IF
      RD=(H(3)-H(1))**2+(V(3)-V(1))
      IF(RD.LE.99.) THEN
         RD=(H(4)-H(2))**2+(V(4)-V(2))
         IF(RD.LE.99.) THEN
            NYES=-1
            RETURN
         END IF
      END IF
      HLD(1)=H(1)
      HLD(2)=H(2)
      HLD(3)=H(3)
      HLD(4)=H(4)
      VLD(1)=V(1)
      VLD(2)=V(2)
      VLD(3)=V(3)
      VLD(4)=V(4)
      IF(IPICDO.NE.0.AND.ISTODS(5,NWINDZ,IWUSDO).NE.IPICDO) RETURN
      CALL DQINV(NWINDZ,H(1),V(1),HH(1),VV(1))
      CALL DQINV(NWINDZ,H(2),V(2),HH(2),VV(2))
      CALL DQINV(NWINDZ,H(3),V(3),HH(3),VV(3))
      CALL DQINV(NWINDZ,H(4),V(4),HH(4),VV(4))
      NYES=3
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DQZBN
CH
      ENTRY DQZBN(HH,VV)
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
      IF(ISTODS(5,IAREDO,IWUSDO).NE.IPICDO) THEN
 1000    CALL DWRT('Select right window.')
      ELSE
         CALL DQSET(IAREDO,0.,0.)
         DO K=1,4
            CALL DQPOC(HH(K),VV(K),H(K),V(K),FDUM)
         END DO
         H(5)=H(1)
         V(5)=V(1)
         CALL DGZOOM(3,IAREDO,H,V)
      END IF
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQZRB
CH
      SUBROUTINE DQZRB(H0,V0,H2,V2,PH,PV,FSTOP)
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
C      NR   1     0      0    Normal rectangle (Macintosh) or square
C      NR   2     0      0    Normal rectangle (Macintosh) or square
C      US   3     1      0    Unrotated square
C      RS   4     1      1    Rotated   square
C      UR   5     2      0    Unrotated rectangle free aspect ratio
C      RR   6     2      1    Rotated   rectangle with fixed aspect ratio
C      HP   7     3      0    Horizontal parallelogram
C      VP   8     4      0    Vertical   parallelogram
      INCLUDE 'DALI_CF.INC'
C      COMMON/XZOOM/ QASRDZ,MOD1DZ,MOD2DZ,NDIRDZ,FSTRDZ,NWINDZ
      DIMENSION PH(5),PV(5)
C      LOGICAL FSTRDZ,FSTOP
      LOGICAL FSTOP
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PBM'
      CALL DPARAM(11
     &  ,J_PBM)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(FSTRDZ) THEN
         IF(NWINDZ.LT.0) THEN
            HM=0.5*(H0+H2)
            VM=0.5*(V0+V2)
            CALL DPOAR(HM,VM,NWINDZ)
         END IF
         IF(NWINDZ.LT.0) GO TO 996
         IF(ISTODS(5,NWINDZ,IWUSDO).NE.IPICDO) GO TO 997
         IF(ISTODS(6,NWINDZ,IWUSDO).NE.0.AND.
     &     (PSTODS(1,J_PBM,NWINDZ,IWUSDO).GT.4..AND.
     &     MOD1DZ.LT.5)) GO TO 998
         CALL DQSET(NWINDZ,0.,0.)
         CALL DQPOC(0.,0.,HCNT,VCNT,FO2)
         VPOS=VCNT+ABS(V0-VCNT)
         VNEG=VCNT-ABS(V0-VCNT)
         IF(MOD1DZ.EQ.5) THEN
            H0C=H0-HCNT
            V0C=V0-VCNT
            R5=V0C**2+H0C**2
            IF(R5.EQ.0.) GO TO 999
            QH5=H0C/R5
            QV5=V0C/R5
         ELSE IF(MOD1DZ.EQ.7) THEN
            IF(V0.EQ.VCNT) GO TO 999
            Q7=(H0-HCNT)/(V0-VCNT)
            PV(3)=V0
            PV(4)=V0
         END IF
         FSTRDZ=.FALSE.
      END IF
      GO TO (10,10,30,40,50,60,70,80),MOD1DZ
      GO TO 999
C                                              UR  1   Rectangle (Macintosh)
   10 PH(1)=H0
      PH(2)=H2
      PH(3)=H2
      PH(4)=H0
      PV(1)=V0
      PV(2)=V0
      PV(3)=V2
      PV(4)=V2
      GO TO 9
C                                                       Unrotated square
   30 H1=H0
      V1=V0
      DH=H2-H1
      DV=V2-V1
      R12=SQRT(DH**2+DV**2)
      IF(R12.LE.1.) THEN
         DO K=1,5
            PH(K)=H0
            PV(K)=V0
         END DO
         RETURN
      END IF
      GO TO 45
C                                                       Rotated square
   40 R01=SQRT((H0-HCNT)**2+(V0-VCNT)**2)
      R02=SQRT((H2-HCNT)**2+(V2-VCNT)**2)
      IF(R02.LE.R01) THEN
         DO K=1,5
            PH(K)=H0
            PV(K)=V0
         END DO
         RETURN
      END IF
      Q=R01/R02
      H1=HCNT+Q*(H2-HCNT)
      V1=VCNT+Q*(V2-VCNT)
C      DH=H2-H1
C      DV=V2-V1
C      R12=SQRT(DH**2+DV**2)
  45  HM=0.5*(H1+H2)
      VM=0.5*(V1+V2)
      S=0.5*MAX(ABS(H2-H1),ABS(V2-V1))
      PH(1)=HM-S
      PH(2)=HM+S
      PH(3)=PH(2)
      PH(4)=PH(1)
      PV(1)=VM-S
      PV(2)=PV(1)
      PV(3)=VM+S
      PV(4)=PV(3)
      GO TO 9
C                                               Unrotated rectangle
C   50 GO TO 999
C   50 HM=0.5*(H0+H2)
C      VM=0.5*(V0+V2)
C      DH=HM-HCNT
C      DV=VM-VCNT
C      RR=DH**2+DV**2
C      IF(RR.NE.0.) QR=1./RR
C      PH(1)=H0
C      PV(1)=V0
C      PH(3)=H2
C      PV(3)=V2
C      PH(4)=QR*((V2-V0)*DH*DV+H0*DV**2+H2*DH**2)
C      IF(DH.NE.0.) THEN
C        PV(4)=V0+(PH(4)-H0)*DV/DH
C      ELSE
C        PV(4)=V2
C      END IF
C      PH(2)=2.*HM-PH(4)
C      PV(2)=2.*VM-PV(4)
C                                               Unrotated rectangle
   50 H2C=H2-HCNT
      V2C=V2-VCNT
      HV=H2C*H0C+V2C*V0C
      HM=QH5*HV+HCNT
      VM=QV5*HV+VCNT
      DH=H2-HM
      DV=VM-V2
      PH(1)=HM-DH
      PH(2)=HM+DH
      PH(3)=H0+DH
      PH(4)=H0-DH
      PV(1)=VM+DV
      PV(2)=VM-DV
      PV(3)=V0-DV
      PV(4)=V0+DV
      GO TO 9
C                                                 Rotated rectangle
   60 R01=SQRT((H0-HCNT)**2+(V0-VCNT)**2)
      R02=SQRT((H2-HCNT)**2+(V2-VCNT)**2)
      IF(R02.LE.R01) THEN
         DO K=1,5
            PH(K)=H0
            PV(K)=V0
         END DO
         RETURN
      END IF
      Q=R01/R02
      H1=HCNT+Q*(H2-HCNT)
      V1=VCNT+Q*(V2-VCNT)
      DH=H2-H1
      DV=V2-V1
      R12=SQRT(DH**2+DV**2)
      B=R12*QASRDZ
      A=ATAN2(DV,DH)
      SA=SIN(A)*B
      CA=COS(A)*B
      PH(1)=H1-SA
      PV(1)=V1+CA
      PH(2)=H1+SA
      PV(2)=V1-CA
      PH(3)=H2+SA
      PV(3)=V2-CA
      PH(4)=H2-SA
      PV(4)=V2+CA
      GO TO 9
C                                     Horizontal parallelogram
C  130 IF(V2.GT.VCNT) THEN
C        V1=VPOS
C        IF(V2.LT.V1) V2=V1
C      ELSE
C        V1=VNEG
C        IF(V2.GT.V1) V2=V1
C      END IF
C      IF(V2.NE.VCNT) THEN
C        HV=(H2-HCNT)/(V2-VCNT)
C        H1=HCNT+(V1-VCNT)*(H2-HCNT)/(V2-VCNT)
C      ELSE
C        H1=H2
C      END IF
C      B=QAS*SQRT((H2-H1)**2+(V2-V1)**2)
CC      IF(V1.NE.V2) THEN
CC        DV=V2-V1
CC        B=QAS*ABS(DV+(H2-H1)**2/DV)
CC      ELSE
CC        B=0.
CC      END IF
C      PH(1)=H1-B
C      PH(2)=H1+B
C      PH(3)=H2+B
C      PH(4)=H2-B
C                                                    Horizontal parallelogram
C  70  PV(1)=V0
C      PV(2)=V0
C      PV(3)=V2
C      PV(4)=V2
C      PH(1)=H0
C      PH(3)=H2
C      HM=0.5*(H0+H2)-HCNT
C      VM=0.5*(V0+V2)-VCNT
C      IF(VM.NE.0.) THEN
C        DH=(V2-V0)*HM/VM
C      ELSE
C        DH=0.
C      END IF
C      PH(2)=H2-DH
C      PH(4)=H0+DH
C                                                    Horizontal parallelogram
   70 V2C=V2-VCNT
      PV(1)=V2
      PV(2)=V2
      HM=Q7*V2C+HCNT
C      HM=Q7*V2C+VCNT
      DH=ABS(H2-HM)
      PH(1)=HM-DH
      PH(2)=HM+DH
      PH(3)=H0+DH
      PH(4)=H0-DH
      GO TO 9
C                                                    Vertical parallelogram
  80  PV(1)=V1+B
      PV(2)=V1-B
      PV(3)=V2-B
      PV(4)=V2+B
      PH(1)=H1
      PH(2)=H1
      PH(3)=H2
      PH(4)=H2
    9 PH(5)=PH(1)
      PV(5)=PV(1)
      RETURN
  996 CALL DWRT('There is no full picture on the window!')
      GO TO 999
  997 CALL DWRT('Window / projection mismatch! Select right one.')
      GO TO 999
  998 CALL DWRT('Do not use SQ-mode on a rotated projection!')
  999 FSTOP=.TRUE.
      NWINDZ=0
      END
