*DK DOFON
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DOFON
CH
      SUBROUTINE DOFON(TA,P,NPOS,NYES)
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
      LOGICAL NYES
      DIMENSION P(4,1)
      CHARACTER *2 TA
      IF(NPOS.LE.0) THEN
        NYES=.FALSE.
      ELSE IF(P(4,NPOS).EQ.0.) THEN
        NYES=.FALSE.
      ELSE IF(TA.EQ.'ON') THEN
        P(4,NPOS)= ABS(P(4,NPOS))
        NYES=.TRUE.
      ELSE IF(TA.EQ.'OF') THEN
        P(4,NPOS)=-ABS(P(4,NPOS))
        NYES=.TRUE.
      ELSE
        NYES=.FALSE.
      END IF
      END
*DK DOP2LT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DOP2LT
CH
      SUBROUTINE DOP2LT(ALAST,NTYPE,NEXEC,A,F)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C    Modifications done by B.S. Nilsson in April-June, 1989 to interface
C    to a new version of DGETST. This provides support for DELETE and
C    function keys.
C    More changes done in Feb 1990 for macro recording.
C
C
C!:
C    Inputs    :
C    Outputs   :
C    NTYPE
C     1       1 Character
C     2       2 Characters
C     3       Absolute number
C     4       Increment
C     5       Nothing
C     6       Two letters with preceding point
C     7       Two letters with preceding ?
C    NEXEC
C     0       Nothing
C     1       Return = LIST
C     2       '  '   = EXECUTE
C    A
C     Input characters if any
C    F
C     Input number if any
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER ALAST*1, A*2, AA*1, APM*1, TLOG*80, AMAC*24

      INTEGER   IPTBUF,LENBUF
      CHARACTER INPBUF*12
      COMMON /CMIOBF/ IPTBUF,LENBUF,INPBUF
      LOGICAL FBL/.FALSE./

      LOGICAL FLGOOB, FLGDOP
      COMMON /OOBCMN/ FLGOOB(0:31),FLGDOP

      INTEGER IPNTDM
      CHARACTER TMACDM*2000
      COMMON/DMMACR/ IPNTDM,TMACDM

C***
C      CHARACTER *80 TESTMAC           ! Testing intrinsic macro
C***
      DATA IDEB/2/,ISBUT/0/
      CALL DGSBUT(ISBUT)
      IF(FLGOOB(18)) CALL DGETMD
      LENBUF=0
      IPTBUF=0
      IF(ALAST.NE.' '.AND.ALAST.NE.',') THEN
         AA=ALAST
         ALAST=' '
      ELSE
C................................................. 1. Letter
         FLGDOP=.TRUE.
    1    CALL DGETST(AA,LG,1,1)
         IF(AA.EQ.'`') THEN
           CALL DWR_BACKSPACE_SLASH(1)
           CALL DQ_TOGGLE_HELP_DALI
           GO TO 1
         END IF
         FLGDOP=.FALSE.
         IF(LG.EQ.-1001) THEN
            IF(FLGOOB(18)) CALL DGETMD
            GO TO 1
         END IF
C..............................................................
      END IF
   11 NTYPE=5
      MTYPE=0
C                                                       1.L.=  Return
      IF(LG.EQ.-13) THEN
         NEXEC=1
         FBL=.FALSE.
         GO TO 999
      ELSE IF(LG.EQ.-26) THEN
C                                                       1.L = CTRL-Z
         A='QU'
         NTYPE=2
         FBL=.FALSE.
         GO TO 999
      ELSE IF(LG.EQ.-127) THEN
C                                                       1.L = Delete
         FBL=.FALSE.
         GO TO 1
      ELSE IF(LG.LT.-255 .OR. LG.EQ.0) THEN
C                                                       1.L = Function key
C   We can only get here if the function key is not defined. Then
C   do nothing.
C
         NEXEC=0
         GO TO 999
      ELSE
         NEXEC=0
      END IF
C                                                       1.L. = /
      IF(AA.EQ.'/') THEN
        CALL DWR_BACKSPACE_SLASH(LENBUF)

C         DO I = 1, LENBUF
C            WRITE(*,'(a,$)') '+'//CHAR(8)//CHAR(32)//CHAR(8)
C         END DO

        LENBUF=0
        IPTBUF=0
        FBL=.FALSE.
        GO TO 1
      END IF
C                                                       1.L. = # (Bell)
      IF(AA.EQ.'#') THEN
         CALL DWR_BELL(1,.NOT.FMOPDM)
         IF(FLGOOB(12).AND..NOT.FMACDM) THEN
           TMACDM(IPNTDM:IPNTDM)='#'
           IPNTDM=IPNTDM+1
         END IF
         LENBUF=MAX(0,LENBUF-1)
         IPTBUF=MAX(0,IPTBUF-1)
         GO TO 1
      END IF
C                                                       1.L. = ( or )
      IF(AA.EQ.'(' .OR. AA.EQ.')') THEN
         IF(FLGOOB(12).AND..NOT.FMACDM) THEN
           TMACDM(IPNTDM:IPNTDM)=AA
           IPNTDM=IPNTDM+1
         END IF
         GO TO 1
      END IF
C                                                       1.L. = !
      IF(AA.EQ.'!') THEN
         CALL DWRT('Enter logfile comment')
         CALL DGETLN(TLOG,LTLOG,LEN(TLOG))
         IF(LTLOG.GT.0) THEN
            CALL DWRT('Comment: '//TLOG(1:LTLOG))
         ELSE
            CALL DWRT('Empty comment, nothing written')
         ENDIF
         CALL DWRT_END(IDEB)
         GO TO 1
      END IF
C                                                       1.L.= @
      IF(AA.EQ.'@') THEN
C        .................................. Macro invocation.
         CALL DGETST(AMAC,LG,24,0)
         IF(FLGOOB(12).AND..NOT.FMACDM) THEN
C           ........ Record macro invokation for macro learning (CTRL-L is ON).
C           ..... Allow only if there are enough characters free in the buffer.
            IF(IPNTDM+LG+1 .GT. 2000) THEN
              CALL DWRT('Overflow in macro save buffer.')
              CALL DWRT_END(IDEB)

C               WRITE(*,'(A,T51,A)')
C     &          ' Overflow in macro save buffer.','..:'

               FLGOOB(12)=.FALSE.
            ELSE
               IF(IPNTDM.EQ.2.AND.TMACDM(1:1).EQ.':') IPNTDM=1
               TMACDM(IPNTDM:IPNTDM+LG+1)='@'//AMAC(1:LG)//':'
               IPNTDM=IPNTDM+LG+2
            END IF
         END IF
         IF(.NOT.FMACDM) CALL DWR_ADD_SEMICOLON
         CALL DGETST(AMAC,LG,1,2)
         IF(.NOT.FMACDM) GO TO 1
         AA=AMAC
         GO TO 11
      END IF
C                                                       1.L.=  Blank
      IF(AA.EQ.' ') THEN
         IF(FBL) THEN
C                                                       Blank before
           CALL DWR_BACKSPACE_DO

C            WRITE(*,'(A,$)') '+'//CHAR(8)//'DO' ! Backspace and 'DO'

C          CALL DWR_ADD_TEXT_SETUP('^')
           NEXEC=2
           FBL=.FALSE.
           GO TO 999
         END IF
         FBL=.TRUE.
C.......................................... Second letter after blank
    2    CALL DGETST(A(2:2),LG,1,1)
C..............................................................
C                                                  Return after blank
         IF(LG.EQ.-13) THEN
            NEXEC=1
            GO TO 999
         ELSE IF(LG.EQ.-26) THEN
C                                                       2.L = CTRL-Z
            A='QU'
            NTYPE=2
            FBL=.FALSE.
            GO TO 999
         ELSE IF(LG.EQ.-127) THEN
C                                                       2.L = Delete
            CALL DWR_ADD_SEMICOLON_DELET
            LENBUF=MAX(LENBUF-1,0)
            IPTBUF=MAX(IPTBUF-1,0)
            FBL=.FALSE.
            GO TO 1
         ELSE IF(LG.LT.-255 .OR. LG.EQ.0) THEN
C                                                       2.L = Function key
C   We can only get here if the function key is not defined. Then
C   do nothing.
C
            NEXEC=0
            GO TO 999
         END IF
C                                                       2.L. = # (Bell)
         IF(A(2:2).EQ.'#') THEN
            CALL DWR_BELL(1,.NOT.FMOPDM)
            IF(FLGOOB(12).AND..NOT.FMACDM) THEN
              TMACDM(IPNTDM:IPNTDM)='#'
              IPNTDM=IPNTDM+1
            END IF
            GO TO 2
         END IF
C                                                       2.L. = ( or )
         IF(AA.EQ.'(' .OR. AA.EQ.')') THEN
            IF(FLGOOB(12).AND..NOT.FMACDM) THEN
              TMACDM(IPNTDM:IPNTDM)=AA
              IPNTDM=IPNTDM+1
            END IF
            GO TO 2
         END IF
C                                                  Blank after blank
         IF(A(2:2).EQ.' ') THEN
           CALL DWR_BLBL_TO_DO

C           WRITE(*,'(A,$)') '+'//CHAR(8)//CHAR(8)//'DO' ! Backspace and 'DO'

C          CALL DWR_ADD_TEXT_SETUP('^')
           NEXEC=2
           FBL=.FALSE.
           GO TO 999
         END IF
         CALL DWR_IC(A)
         CALL DWRT_END(IDEB)
         GO TO 1
      END IF
      FBL=.FALSE.
    3 N=ICHAR(AA)
C                First is letter or non-numeric (not digit, +, -)
      IF((N.GE.32.AND.N.LE.42).OR.(N.GE.58.AND.N.LE.127)) THEN
         A(1:1)=AA
C............................................... Get second letter
    4    CALL DGETST(A(2:2),LG,1,1)
C..............................................................
         IF(NLETDM.EQ.1) GO TO 11
C                                                         2.L = Return
         IF(LG.EQ.-13) THEN
            NTYPE=1
            NEXEC=1
         ELSE IF(LG.EQ.-127) THEN
C                                                         2.L = Delete
            CALL DWR_ADD_SEMICOLON_DELET
            LENBUF=MAX(LENBUF-1,0)
            IPTBUF=MAX(IPTBUF-1,0)
            GO TO 1
C
         ELSE IF(LG.LT.-255 .OR. LG.EQ.0) THEN
C                                                         2.L = Function key
            NEXEC=0
            GO TO 999
C                                                         2.L. = # (Bell)
         ELSE IF(A(2:2).EQ.'#') THEN
            CALL DWR_BELL(1,.NOT.FMOPDM)
            GO TO 4
         ELSE
C                                                         2.L =  /
            IF(A(2:2).EQ.'/') THEN
              CALL DWR_BACKSPACE_SLASH(LENBUF)

C              DO I = 1, LENBUF
C                WRITE(*,'(a,$)') '+'//CHAR(8)//CHAR(32)//CHAR(8)
C              END DO

              LENBUF=0
              IPTBUF=0
              GO TO 1
            END IF
C                                                    Accept second letter
C           CALL DWR_ADD_TEXT_SETUP('^')
            N2=ICHAR(A(2:2))
            IF(A.EQ.'DO') THEN
               A='  '
               NEXEC=2
               NTYPE=5
               GO TO 999
            END IF
            NTYPE=2+MTYPE
         END IF
         GO TO 999
      END IF
      SIGN=1.
      NUM=0
      NPOINT=0
      MPOINT=0
      NDIG=0
      APM='x'
C                                                             -
      IF(AA.EQ.'-') THEN
         SIGN=-1.
         APM='-'
         GO TO 5
      END IF
C                                                             .
      IF(AA.EQ.'+') THEN
         SIGN=1.
         APM='+'
         GO TO 5
      END IF
C
      IF(AA.EQ.'.') THEN
         MPOINT=1
         GO TO 5
      END IF
C                                                             Number
      IF(N.GE.48.AND.N.LE.57) THEN
         NUM=N-48
         GO TO 5
      END IF
      CALL DWR_IC(AA)
      CALL DWRT_END(IDEB)
      LENBUF=0
      IPTBUF=0
      GO TO 1
    5 NDIG=NDIG+1
C..............................................................
    6 CALL DGETST(AA,LG,1,1)
C..............................................................
      N=ICHAR(AA)
      IF(LG.EQ.-127) THEN
C
         CALL DWR_ADD_SEMICOLON_DELET
         LENBUF=MAX(LENBUF-1,0)
         IPTBUF=0
         GO TO 1
      END IF
C                                                       Letter = # (Bell)
      IF(AA.EQ.'#') THEN
         CALL DWR_BELL(1,.NOT.FMOPDM)
         GO TO 6
      END IF
C                                                       ++ or --
      IF(APM.EQ.AA) THEN
         A=APM//APM
         NTYPE=2
C        CALL DWR_ADD_TEXT_SETUP('^')
         GO TO 999
      ELSE
         APM='x'
      END IF
      IF(NDIG.EQ.1.AND.N.GE.65.AND.N.LE.90.AND.MPOINT.EQ.1) THEN
         MTYPE=4
         GO TO 3
      END IF
C                                                             :
      IF(AA.EQ.'/') THEN
        CALL DWR_BACKSPACE_SLASH(LENBUF)

C        DO I = 1, LENBUF
C          WRITE(*,'(a,$)') '+'//CHAR(8)//CHAR(32)//CHAR(8)
C        END DO

        LENBUF=0
        IPTBUF=0
        GO TO 1
      END IF
C                                                             .
      IF(AA.EQ.'.'.AND.MPOINT.EQ.1.AND.NDIG.EQ.1) THEN
         A='..'
         NTYPE=2
C        CALL DWR_ADD_TEXT_SETUP('^')
         GO TO 999
      END IF
C                                                             -
      IF(AA.EQ.'-'.OR.AA.EQ.'_') THEN
         IF(SIGN.EQ.-1..AND.NDIG.EQ.1) THEN
            A='--'
            NTYPE=2
         ELSE
            F=-NUM/10.**NPOINT
            NTYPE=4
         END IF
C        CALL DWR_ADD_TEXT_SETUP('^')
         GO TO 999
      END IF
C                                                             +
      IF(AA.EQ.'+') THEN
         F=NUM/10.**NPOINT
         NTYPE=4
C        CALL DWR_ADD_TEXT_SETUP('^')
         GO TO 999
      END IF
C                                                             .
      N=ICHAR(AA)
      IF(AA.EQ.'.'.AND.MPOINT.EQ.0) THEN
         MPOINT=1
         GO TO 5
      END IF
C                                                             Number
      IF(N.GE.48.AND.N.LE.57) THEN
         NUM=NUM*10+N-48
         NPOINT=NPOINT+MPOINT
         GO TO 5
      END IF
      F=NUM/10.**NPOINT*SIGN
      NTYPE=3
      IF(LG.LT.0) THEN
C                                         Return or other terminator
         NEXEC=1
         GO TO 999
      END IF
      IF(AA.EQ.','.OR.(N.GE.65.AND.N.LE.90)) THEN
         ALAST=AA
C        CALL DWR_ADD_TEXT_SETUP('*')
      ELSE
C        CALL DWR_ADD_TEXT_SETUP('^')
         IF(AA.EQ.' ') THEN
           CALL DWR_BACKSPACE_BLANK

C           WRITE(*,'(A,$)') '+'//CHAR(8)       ! Backspace on blank

           FBL=.TRUE.
         END IF
      END IF

C  Switch off input recording and make it a macro. (CTRL-R was issued).
  999 IF(FLGOOB(18)) CALL DGETMD

      IF(FLGOOB(12).AND..NOT.FMACDM) THEN
C  Record input to be used in a macro definition later (CTRL-L is ON).
C  Allow only if there are at least 20 characters free in the buffer.
         IF(IPNTDM+20.GT.2000) GO TO 1000
         IF(IPNTDM.EQ.2) THEN
            IF(TMACDM(1:1).EQ.':') IPNTDM=1
         END IF
         IF(NEXEC.EQ.2) THEN
            TMACDM(IPNTDM:IPNTDM+1)='DO'
            IPNTDM=IPNTDM+2
            GOTO 102
         ELSE IF(NEXEC.EQ.1.AND..NOT.(NTYPE.EQ.3.OR.NTYPE.EQ.4)) THEN
            TMACDM(IPNTDM:IPNTDM)=':'
            IPNTDM=IPNTDM+1
            GOTO 102
         END IF
         IF(NTYPE.EQ.2.OR.NTYPE.EQ.5) THEN
            IF(A.NE.' ') THEN
               TMACDM(IPNTDM:IPNTDM+1)=A(1:2)
            ELSE
               TMACDM(IPNTDM:IPNTDM+1)='DO'
            END IF
            IPNTDM=IPNTDM+2
         ELSE IF(NTYPE.EQ.3.OR.NTYPE.EQ.4) THEN
            WRITE(TLOG,'(F20.4)',ERR=102) F
            DO I=1,20
               IF(TLOG(I:I).NE.' ') THEN
                  I1=I
                  GO TO 100
               END IF
            END DO
            GO TO 102
  100       DO I=20,I1,-1
               IF(TLOG(I:I).NE.'0') THEN
                  I2=I
                  GO TO 101
               END IF
            END DO
            GO TO 102
  101       IF(TLOG(I2:I2).EQ.'.') I2=I2-1
            IF(NTYPE.EQ.3) THEN
               TMACDM(IPNTDM:IPNTDM+I2-I1+1)=TLOG(I1:I2)//':'
               IPNTDM=IPNTDM+I2-I1+2
            ELSE IF(NTYPE.EQ.4) THEN
               IF(TLOG(I1:I1).EQ.'-') THEN
                  TMACDM(IPNTDM:IPNTDM+I2-I1)=TLOG(I1+1:I2)//'-'
                  IPNTDM=IPNTDM+I2-I1+1
               ELSE
                  TMACDM(IPNTDM:IPNTDM+I2-I1+1)=TLOG(I1:I2)//'+'
                  IPNTDM=IPNTDM+I2-I1+2
               END IF
            END IF
         END IF
      END IF
  102 CONTINUE

C
C*** Testing intrinsic MACRO 8-Mar-1990
CD      IF(A.EQ.'IN') THEN
CD#ifndef ultrix
CD         WRITE(*,'(A,$)') ' Enter macro string: '
CD#else
CD         WRITE(*,'(/,A,$)') ' Enter macro string: '
CD#endif /* ultrix */
CD         CALL DGETLN(TESTMAC,LTMAC,LEN(TESTMAC))
CD         CALL DT018A('~')
CD         CALL DOPER
CD         DGINMA(TESTMAC)
CD         GO TO 1
CD      END IFC
      RETURN
 1000 CALL DWRT(' Overflow in macro save buffer.')
      FLGOOB(12)=.FALSE.
      RETURN
      END
*DK DOPER
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DOPER
CH
      SUBROUTINE DOPER(NTVI,NCOR,
     &  I1,N1,T1,P1,
     &  I2,N2,T2,P2,NEX,CHG,TA)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :NTVI,NCOR
C    Outputs   :
C       Special DOPER commands are : HE,ON,OF,NC,  ,++,--,..,GB,
C                                    CA,CC (IN DOP2LT)
C                                    GL,ZO,W#,WL,WR,(WM) ,WU,WD,WT
C       If NCOR=1 :                  SW,TP,T0,T1,T2,T3,E1,E2
C
C     NPOS= array position
C     NZ=0 global =1 zoom
C     NEX=execute flag        1=quit or go to other processor
C                             2=letters not found
C                             3= 1 return
C                             4=execute
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TOUT,TBK
      DIMENSION P1(4,1),P2(4,1)
      CHARACTER *1 T0789(-4:-1),TPRG
      DATA T0789/'0','7','8','9'/,TPRG/'\'/
      CHARACTER *2 T1(1),T2(1)
      CHARACTER *2 TA,TPOS,TLAST,TCUR,TCW,TANSW
      CHARACTER *3 THLP,TCR,TBACK
      CHARACTER *8 TINMA
      CHARACTER *4 TPTA
      CHARACTER *5 TIMA5,DT5
      CHARACTER *7 TIMA7
      DATA TIMA5/'(@..)'/
      DATA TIMA7/'(@....)'/
      CHARACTER *18 TIST
      DATA TLAST /' '/,TIST/' '/,TCUR/'QQ'/,TCW/'cw'/
      DATA NOPER/69/
      DATA NSP/1/
      CHARACTER *6 DT6
      CHARACTER *49 T49

      DATA DINCR/1./
      CHARACTER *50 TX1,TX2,TX3
C               123456789 123456789 123456789 123456789 123456789
      DATA TX1/'QU GB GG=GT=GO GC G# W# C# OF ON DO   FD NC OL OP'/
      DATA TX2/'           VX VD V0 V1 IT I0 I1 TP T0 T1 T2 T3'/
      DATA TX3/'           EC E0 E1 E2 E3 HC H0 H1 H2 MU M0 M1 M2'/
      LOGICAL NYES,CHG,F0,FSOL,FPRG
      DATA FPRG/.FALSE./
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PCF,J_PTE,J_PCT,J_PCC,J_PLA'
      CALL DPARAM(11
     &  ,J_PFI,J_PCF,J_PTE,J_PCT,J_PCC,J_PLA)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      FGETDO=.TRUE.
      NOCLDT=0
      DFLGDU=ABS(DFLGDU)
      IF(TBACK.EQ.' ') THEN
        IF(TAN1DH.EQ.'   ') THEN
          THLP=TPICDO//' '
        ELSE
          THLP=TAN1DH
          TAN1DH=' '
        END IF
      ELSE
        THLP=TBACK
        TBACK=' '
      END IF
      GO TO 4
    5 CALL DQHL_MENU(THLP)
      GO TO 3
    4 CALL DQHL_MENU(THLP)
      NTYPE=2
    1 IF(TLAST.EQ.' ') CALL DWRT_END(2)
    3 IF(.NOT.FMACDM) FINMDM=.FALSE.
      FSOL=FSOLD0
      FSOLD0=.FALSE.
    6 CALL DOP2LT(TLAST,NTYPE,NEXEC,TA,F)
      TANSW=TA
      KTYPE=NTYPE
C     ............................. check if button 1 is pressed
C     ......................... on the command list to show page
      IF(NEXEC.EQ.0) CALL DQHLT_PAGE(TA)
      IF(     TA(1:1).EQ.TPRG) THEN
C        IF(TA.EQ.'\\') THEN
C          CALL DWRT('Use right button to select a page on the index.#')
C          GO TO 3
C        END IF
        THLP(3:3)=TA(2:2)
        TA=' '
        GO TO 4
      END IF
      IF(FPRG) THEN
        FPRG=.FALSE.
      ELSE
        TPRGDH=' '
      END IF
C
C     NEXEC
C     0       Nothing
C     1       Return = LIST
C     2       '  '   = EXECUTE
C
    2 GO TO (1,20,30,40,51,60,70),NTYPE
C     NTYPE
C     1       1 Character
C     2       2 Characters
C     3       Absolute number
C     4       Increment
C     5       Nothing
C     6       Two letters with preceding point
C     7       Two letters with preceding ?
C                                                  QUIT
   20 IF(TA.EQ.'QU'.OR.TA.EQ.'EX') GO TO 173
      IF(TA(1:1).EQ.'G') THEN
        IF(TA.EQ.'GB'.OR.TA.EQ.'GT'.OR.TA.EQ.'GG') GO TO 173
        IF(TA.EQ.'GO') THEN
          TA='GT'
          GO TO 173
        END IF
        IF(TA.EQ.'G.') THEN
          K=IAREDO
          GO TO 170
        END IF
        IF(TA.EQ.'GC') THEN
          CALL DGCURG(HW,VW)
          CALL DPOAR(HW,VW,K)
          IF(K.GE.0) GO TO 170
        END IF
        CALL DAREA('G',TA,0,12,K,NYES)
        IF(NYES) GO TO 170
        DO LARE=-4,-1
          IF(TA(2:2).EQ.T0789(LARE)) THEN
            CALL DPCGVI(LARE,F0,IZOMDO)
            IF(F0) THEN
              CALL DWRT(' Virtual window '//T0789(LARE)//
     &          ' is empty.')
C             CALL DWR_ADD('>')
              GO TO 1
            END IF
            TA='GW'
            FGETDO=.FALSE.
            GO TO 172
          END IF
        END DO
      END IF
C
      DO 710 M=I1,N1
         IF(TA.EQ.T1(M)) THEN
            NSP=1
            NPOS=M
            TPOS=TA
            IF(P1(4,M).EQ.-1.) P1(4,M)=1.
            IF(P1(4,M).EQ.-2.) P1(4,M)=2.
            IF(NCOR.EQ.1.AND.TA.EQ.T1(J_PCC)) THEN
               P1(4,J_PCF)=1.
               P1(4,J_PCT)=1.
            END IF
            CHG=.TRUE.
            CALL DWR_ADD('=')
C           GO TO 3                           ! 20.5.96
            GO TO 5
         END IF
  710 CONTINUE
C                                                     2 PARAM. SET
      DO 720 M=I2,N2
         IF(TA.EQ.T2(M)) THEN
            NSP=2
            NPOS=M
            TPOS=TA
            IF(P2(4,M).EQ.-1.) P2(4,M)=1.
            IF(P2(4,M).EQ.-2.) P2(4,M)=2.
            CHG=.TRUE.
            CALL DWR_ADD('=')
C           GO TO 3                           ! 20.5.96
            GO TO 5
         END IF
  720 CONTINUE
      CALL DAREA('W',TA,0,12,IAREDO,NYES)
      IF(NYES) THEN
        CALL DQHL_W
        CALL DGZOOM(6,-1,0,0)
        CALL DGZOOM(6,IAREDO,0,0)
        IF(TA(1:1).EQ.'w') GO TO 3
        GO TO 1
      END IF
      IF(NCOR.NE.0) THEN
         CALL DAREA('C',TA,0,12,IAREDO,NYES)
         IF(NYES) THEN
           NEX=4
           RETURN
         END IF
         IF(TA(1:1).EQ.'C') THEN
           DO LARE=-3,-1
             IF(TA(2:2).EQ.T0789(LARE)) THEN
               CALL DPCSAV(0,LARE)
               CALL DWRT(TAREDO(IAREDO)//
     &           ' copied to virtual window '//TA(2:2))
               GO TO 1
             END IF
           END DO
         END IF
         IF(TA.EQ.'DB') IZOMDO=1
      END IF
      IF(NSP.EQ.1) THEN
         CALL DOFON(TA,P1,NPOS,NYES)
         IF(NCOR.EQ.1.AND.NPOS.EQ.J_PCC) THEN
           P1(4,J_PCF)=P1(4,J_PCC)
           P1(4,J_PCT)=P1(4,J_PCC)
         END IF
      ELSE
         CALL DOFON(TA,P2,NPOS,NYES)
      END IF
      IF(NYES) GO TO 50
      IF(TA.EQ.'OP'.AND.NCOR.NE.0) THEN
         PARADA(2,J_PFI)=MOD( PARADA(2,J_PFI)+180.,360.)
         PARADA(2,J_PTE)=180.-PARADA(2,J_PTE)
         GO TO 50
      END IF
      IF(TA.EQ.'NC'.AND.NTVI.EQ.1) THEN
         NOCL=1
         NOCLDT=1
         NEX=4
C        CALL DWR_ADD_TEXT_DO
CV         CALL DWR_ADD('|')
         RETURN
      END IF
      IF(TA.EQ.'OL'.AND.NTVI.EQ.1) THEN
         NOCL=1
         NOCLDT=2
         NEX=4
C        CALL DWR_ADD_TEXT_DO
CV         CALL DWR_ADD('|')
         RETURN
      END IF
      IF(TA.EQ.'FD'.AND.NTVI.EQ.1) THEN
C       ........................ DOFISH: set filling of complicated areas
        FFDRDP=.TRUE.
        DFLGDU=-ABS(DFLGDU)
        NEX=4
        RETURN
      END IF
      IF(TA.EQ.'T ') GO TO 110
      IF(NCOR.EQ.1) THEN
C        CALL DHTMO1(1,TA,NYES)
C         IF(NYES) GO TO 51
         CALL DDRLOG(NPOS,TA,NYES)
         IF(NYES) GO TO 51
         CALL DPLANE(NPOS,TA,NYES,NEXEC)
         IF(NYES) GO TO 50
         IF(NPOS.EQ.J_PLA) GO TO 29
      END IF
      IF(NPOS.NE.0) THEN
         IF(TA.EQ.'++') THEN
            IF(NSP.EQ.1) THEN
               P1(2,NPOS)=D3MIX(P1(1,NPOS),DINCR)
               IF(P1(4,NPOS).EQ.-1.) P1(4,NPOS)=1.
               IF(P1(4,NPOS).EQ.-2.) P1(4,NPOS)=2.
               IF(NCOR.EQ.1) CALL DOPCOR(NPOS,TPOS)
            ELSE
               P2(2,NPOS)=D3MIX(P2(1,NPOS),DINCR)
               IF(P2(4,NPOS).EQ.-1.) P2(4,NPOS)=1.
               IF(P2(4,NPOS).EQ.-2.) P2(4,NPOS)=2.
            END IF
            CHG=.TRUE.
            NOCLDT=NOCL
            NEX=4
C           CALL DWR_ADD_TEXT_DO
CV            CALL DWR_ADD('|')
            GO TO 190
         END IF
         IF(TA.EQ.'--') THEN
            IF(NSP.EQ.1) THEN
               P1(2,NPOS)=D3MIX(P1(1,NPOS),-DINCR)
               IF(P1(4,NPOS).EQ.-1.) P1(4,NPOS)=1.
               IF(P1(4,NPOS).EQ.-2.) P1(4,NPOS)=2.
               IF(NCOR.EQ.1) CALL DOPCOR(NPOS,TPOS)
            ELSE
               P2(2,NPOS)=D3MIX(P2(1,NPOS),-DINCR)
               IF(P2(4,NPOS).EQ.-1.) P2(4,NPOS)=1.
               IF(P2(4,NPOS).EQ.-2.) P2(4,NPOS)=2.
            END IF
            CHG=.TRUE.
            NOCLDT=NOCL
            NEX=4
C           CALL DWR_ADD_TEXT_DO
CV            CALL DWR_ADD('|')
            GO TO 190
         END IF
      END IF
   29 IF(TA.EQ.'T?') THEN
        CALL DATE(TIST( 1: 9))
        CALL TIME(TIST(11:18))
        CALL DWRT(TISTD0//' -> '//TIST)
        GO TO 1
      END IF
      IF(TA.EQ.TCUR) THEN
        CALL DMCU(HDUM,VDUM)
        TXTADW='HD = '//DT5(HDUM)//' VD = '//DT5(VDUM)
        CALL DWRC
        GO TO 1
      END IF
      IF(TA.EQ.'QW') THEN
        FWIZDW=.FALSE.
        CALL DQTIT(IFULDB)
        CALL DQHL_R
        GO TO 1
      END IF
      IF(TA.EQ.TCW) THEN
        FCONDW=.TRUE.
        GO TO 4
      END IF
      TPTA=TPICDO//TA
      CALL DO_INT_MAC(TPTA,NYES)
      IF(NYES) THEN
        FINMDM=.TRUE.
        TIMA7(3:6)=TPTA
        CALL DGINMA(TIMA7)
        GO TO 3
      END IF
      IF(TA(1:1).EQ.'*') THEN
        CALL DO_INT_MAC(TA,NYES)
        IF(NYES) THEN
          TIMA5(3:4)=TA
          CALL DGINMA(TIMA5)
          GO TO 3
        END IF
      END IF
      IF(TA.EQ.'X?'.OR.TA.EQ.'Y?'.OR.TA.EQ.'C?') THEN
        CALL DWRT('General commands :')
        CALL DWRT(TX1)
        IF(NCOR.NE.0) THEN
          CALL DPARGG(18,N18_1,N18_2)
          DO N18=N18_1,N18_2
            IF(TPOPDA(N18).NE.' ') THEN
              CALL DWRT('Mnemonics :')
              CALL DWRT(TX2)
              CALL DWRT(TX3)
              IF(TA.EQ.'Y?') THEN
                WRITE(NOPER,2000) TX2
                WRITE(NOPER,2000) TX3
 2000           FORMAT(1X,A)
              END IF
              GO TO 118
            END IF
          END DO
        END IF
  118   J=2
        T49=' '
        IF(N1-I1.GT.0.OR.N2-I2.GT.0) CALL DWRT('Parameters :')
        DO M=I1,N1
          IF(T1(M).NE.'  '.AND.T1(M).NE.'**') THEN
            T49(J:J+1)=T1(M)
            J=J+3
            IF(J.GT.48) THEN
              CALL DWRT(T49)
              IF(TA.EQ.'Y?') WRITE(NOPER,2000) T49
              J=2
              T49=' '
            END IF
          END IF
        END DO
        DO M=I2,N2
          IF(T2(M).NE.'  '.AND.T2(M).NE.'**') THEN
            T49(J:J+1)=T2(M)
            J=J+3
            IF(J.GT.50) THEN
              CALL DWRT(T49)
              IF(TA.EQ.'Y?') WRITE(NOPER,2000) T49
              J=2
              T49=' '
            END IF
          END IF
        END DO
        IF(J.GT.2) CALL DWRT(T49)
        CALL DO_INT_MAC_TYPE(TA)
        NEX=2
        CALL DO_RESET_COMMAND_LIST(TA)
        TA='X?'
        RETURN
      END IF
C     ................................. back to last page maybe processor
      IF(TA.EQ.'ba') THEN
        CALL DQHL_LAST_PAGE(TBACK,TCR)
        IF(TBACK.NE.TCR) THEN
          IF(     TBACK(1:2).EQ.TCR(1:2)) THEN
            THLP=TBACK
            TBACK=' '
            GO TO 5
          ELSE IF(TBACK(1:2).EQ.'GT') THEN
            TINMA='GT:\'//TBACK(3:3)//':'
            CALL DGINMA(TINMA(1:6))
          ELSE
            TINMA='GG:'//TBACK(1:2)//'\'//TBACK(3:3)//':'
            CALL DGINMA(TINMA(1:7))
          END IF
          TBACK=' '
        END IF
        GO TO 3
      END IF
      IF(TA.NE.'..') THEN
         NEX=2
C        CALL DWR_ADD_TEXT_DO
         RETURN
      END IF
   30 IF(NPOS.LE.0) THEN
        CALL DWRT('Define the parameter to be changed first.')
        TLAST=' '
        GO TO 1
      END IF
      IF(NSP.EQ.1) THEN
         IF(F.GE.P1(1,NPOS).AND.F.LE.P1(3,NPOS)) THEN
            P1(2,NPOS)=F
            IF(P1(4,NPOS).EQ.-1.) P1(4,NPOS)=1.
            IF(P1(4,NPOS).EQ.-2.) P1(4,NPOS)=2.
            IF(NCOR.EQ.1) THEN
              CALL DOPCOR(NPOS,TPOS)
              CALL DDRNUM(NPOS)
            END IF
            IF(TLAST.EQ.',') THEN
  501         IF(NPOS.LT.N1) THEN
                 NPOS=NPOS+1
                 IF(T1(NPOS).EQ.'  ') GO TO 501
                  IF(T1(NPOS).NE.'**') CALL DWR_ADD(T1(NPOS)//'=')
               ELSE
                  CALL DWRT('End of parameter list.#')
               END IF
               TLAST=' '
            END IF
            IF(NEXEC.NE.0) GO TO 110
            GO TO 3
         ELSE
           CALL DWRT('? '//T1(NPOS)//'='//DT6(F)//
     &       ' Choose value between '//DT6(P1(1,NPOS))//' and '//
     &       DT6(P1(3,NPOS)))
           TLAST=' '
           GO TO 1
         END IF
      ELSE
         IF(F.GE.P2(1,NPOS).AND.F.LE.P2(3,NPOS)) THEN
            P2(2,NPOS)=F
            IF(P2(4,NPOS).EQ.-1.) P2(4,NPOS)=1.
            IF(P2(4,NPOS).EQ.-2.) P2(4,NPOS)=2.
            IF(TLAST.EQ.',') THEN
  502         IF(NPOS.LT.N2) THEN
                NPOS=NPOS+1
                IF(T2(NPOS).EQ.'  ') GO TO 502
                IF(T2(NPOS).NE.'**') CALL DWR_ADD(T2(NPOS)//'=')
              ELSE
                CALL DWRT('End of parameter list.#')
              END IF
              TLAST=' '
            END IF
            IF(NEXEC.NE.0) GO TO 110
            GO TO 3
         ELSE
           CALL DWRT('? '//T2(NPOS)//'='//DT6(F)//
     &       ' Choose value between '//DT6(P2(1,NPOS))//' and '//
     &       DT6(P2(3,NPOS)))
           TLAST=' '
           GO TO 1
         END IF
      END IF
   40 IF(NPOS.LE.0) THEN
        CALL DWRT('Define the parameter to be changed first.')
        GO TO 1
      END IF
      IF(NSP.EQ.1) THEN
         P1(2,NPOS)=D3MIX(P1(1,NPOS),F)
         IF(P1(4,NPOS).EQ.-1.) P1(4,NPOS)=1.
         IF(P1(4,NPOS).EQ.-2.) P1(4,NPOS)=2.
         IF(NCOR.EQ.1) THEN
           CALL DOPCOR(NPOS,TPOS)
           CALL DDRNUM(NPOS)
         END IF
      ELSE
         P2(2,NPOS)=D3MIX(P2(1,NPOS),F)
         IF(P2(4,NPOS).EQ.-1.) P2(4,NPOS)=1.
         IF(P2(4,NPOS).EQ.-2.) P2(4,NPOS)=2.
      END IF
      DINCR=ABS(F)
   50 CHG=.TRUE.
C  51 IF(NEXEC.EQ.0) CALL DWR_ADD_TEXT_DO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   51 GO TO (1,110,120),NEXEC+1
   51 GO TO (4,110,120),NEXEC+1
C
C                                                     PICTURE ...
   60 CALL DWRT('DOPER: TELL TO H.DREVERMANN')
      GO TO 1
C   60 IF(TA.EQ.'GB') THEN
C         CALL DT010
C         GO TO 1
C      END IF
C   60 IF(TA.EQ.'PO'.OR.TA.EQ.'PI'.OR.TA.EQ.'PP') THEN
C         NEX=2
C         CALL DT010
C         RETURN
C      END IF
C      DO 740 M=MINPDP,MAXPDP
C         IF(TA.EQ.TPICDP(M)) THEN
C            NPIC=M
C            NPOS=0
C            NEX=1
C            TA='**'
C            CALL DT010
C            RETURN
C         END IF
C  740 CONTINUE
C      IARE=IAREDO
C      CALL DAREA('W',TA,0,12,NAR,NYES)
C      IF(NYES) GO TO 170
C      IF(NYES) THEN
C         CALL DPCGAR(NAR,F0,NPIC,IZOMDO)
C         IF(.NOT.F0) THEN
C            NPOS=0
C            NEX=1
C            TA='GW'
C            CALL DT010
C            RETURN
C         END IF
C      END IF
C      IAREDO=IARE
C      CALL DT009('?')
C      CALL DT010
C      GO TO 1
C  70 CALL DWR_ADD_TEXT_DO
   70 GO TO 1
  110 NEX=3
      IF(FSOL) THEN
        TA='GB'
      ELSE
        TA='  '
      END IF
      RETURN
  120 NEX=4
      NOCL=0
C     CALL DWR_ADD_TEXT_DO
      TA='DO'
  190 RETURN
  170 CALL DPCGAR(K,F0,IZOMDO)
      IF(F0) THEN
        CALL DWRT(' Window '//TWINDW(K)//' is empty.')
        GO TO 1
      END IF
      TPICDO=TPICDP(IPICDO)
      TA='GW'
      FGETDO=.FALSE.
      GO TO 172
  173 TPICDO=TA
  172 NEX=1
      NPOS=0
C     CALL DWR_ADD_TEXT_DO
      RETURN


CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DOPERS
CH
      ENTRY DOPERS(JSP,JPOS,DIN)
CH
CH --------------------------------------------------------------------
CH
      NSP=JSP
      NPOS=JPOS
      DINCR=DIN
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DOPERS
CH
      ENTRY DOPERG(ISP,IPOS)
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
      ISP=NSP
      IPOS=NPOS
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DOPERS
CH
      ENTRY DOPER_TANSW(TOUT,JTYPE)
CH
CH --------------------------------------------------------------------
CH
      TOUT=TANSW
      JTYPE=KTYPE
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DOPERS
CH
      ENTRY DOPER_HELP_BACK(TBK)
CH
CH --------------------------------------------------------------------
CH
      TBACK=TBK
      END
*DK DOPCOR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DOPCOR
CH
      SUBROUTINE DOPCOR(NP,TAP)
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
      CHARACTER TAP*2
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PCF,J_PTE,J_PDT,J_PCT,J_PDD,J_PCC'
      CALL DPARAM(11
     &  ,J_PFI,J_PDF,J_PCF,J_PTE,J_PDT,J_PCT,J_PDD,J_PCC)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(     NP.EQ.J_PDD) THEN
         PARADA(2,J_PDF)=PARADA(2,NP)
         PARADA(2,J_PDT)=PARADA(2,NP)
      ELSE IF(NP.EQ.J_PCC) THEN
         PARADA(2,J_PCF)=PARADA(2,NP)
         PARADA(2,J_PCT)=PARADA(2,NP)
      END IF
      IF(NP.EQ.J_PFI) PARADA(2,J_PFI)=MOD(3600.+PARADA(2,J_PFI),360.)
      IF(NP.EQ.J_PTE) CALL DMIX(0.,PARADA(2,J_PTE),180.)
      END
*DK DO_STR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++  DO_STR
CH
      SUBROUTINE DO_STR(T1)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    : Example:
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DATA N/0/
      DATA NOPER/69/
      CHARACTER *(*) T1,TL(*),TA,TPROJ
      PARAMETER (ML=196)
      CHARACTER *2 TLIST(ML),TXY
      CHARACTER *18 T1LT
      DATA T1LT/': 1 letter command'/
      IF(N.GE.ML) RETURN
      IF(TXY.EQ.'Y?') WRITE(NOPER,2000) T1
      IF(TXY.EQ.'C?') CALL DWRT(T1)
 2000 FORMAT(1X,A)
      LTC=LENOCC(T1)
      DO L=1,LTC-1,3
        N=N+1
        TLIST(N)=T1(L:L+1)
        LP2=L+2
        IF(LP2.GT.LTC.OR.T1(LP2:LP2).NE.'"') RETURN
        IF(N.GE.ML) RETURN
      END DO
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------ DO_STR_LIST
CH
      ENTRY DO_STR_LIST(LC,TL,T1)
CH
CH --------------------------------------------------------------------
      IF(N.GE.ML) RETURN
      DO I=1,LC
        IF(TL(I).NE.' '.AND.TL(I).NE.'**') THEN
          IF(N.GE.ML) RETURN
          N=N+1
          TLIST(N)=TL(I)
          TXTADW=TL(I)//': '//T1
          IF(TXY.EQ.'Y?') WRITE(NOPER,2000) TXTADW
          IF(TXY.EQ.'C?') CALL DWRC
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
CH ------------------------------------------------ DO_STR_1_LET_LIST
CH
      ENTRY DO_STR_1_LET_LIST(T1,LC,TL)
CH
CH --------------------------------------------------------------------
      IF(N.GE.ML) RETURN
      DO I=1,LC
        IF(TL(I).NE.' ') THEN
          IF(N.GE.ML) RETURN
          N=N+1
          TLIST(N)=T1//TL(I)
          IF(TXY.EQ.'Y?') WRITE(NOPER,2000) TLIST(N)//T1LT
          IF(TXY.EQ.'C?') CALL DWRT(TLIST(N)//T1LT)
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
CH ------------------------------------------- DO_RESET_COMMAND_LIST
CH
      ENTRY DO_RESET_COMMAND_LIST(TA)
CH
CH --------------------------------------------------------------------
      N=0
      TXY=TA
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------ DO_TY_COMMAND_LIST
CH
      ENTRY DO_TY_COMMAND_LIST(TPROJ)
CH
CH --------------------------------------------------------------------
      K1=1
      CALL DWRT('Processor commands :')
  100 K2=K1+15
      IF(N.LE.K2) THEN
        WRITE(TXTADW,1000) (TLIST(I),I=K1,N)
        IF(TXY.EQ.'Y?') WRITE(NOPER,2000) TXTADW(1:78)
 1000   FORMAT(16(A2,1X))
        CALL DWRC
      ELSE
        WRITE(TXTADW,1000) (TLIST(I),I=K1,K2)
        IF(TXY.EQ.'Y?') WRITE(NOPER,2000) TXTADW(1:78)
        CALL DWRC
        K1=K1+16
        GO TO 100
      END IF
      N=ML
      CALL DWRT(' ')
      IF(TXY.EQ.'Y?') THEN
        WRITE(NOPER,2003) TPROJ
 2003   FORMAT('*********** ',A,' *************')
        CALL DTYANS('Close commands on FOR069.DAT. YES=<CR>',
     &    'Y',NACT)
        IF(NACT.EQ.0) THEN
          CLOSE(UNIT=NOPER)
          CALL DWRT('FOR069.DAT closed.')
        END IF
      END IF
      END
*DK DO_INT_MAC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++  DO_INT_MAC
CH
      SUBROUTINE DO_INT_MAC(TIM,FYES)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Check if 4 letter macro exists
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      PARAMETER (MACMX=500, MACLEVMAX=8)
      CHARACTER TMNAME*20,TMAC*80,MACFIL*80,TMSAVE*20
      INTEGER LSCRN,KMAX,MACLEN,MACLEV,MACSAVE
      LOGICAL FMSAVE
      COMMON /MACCMN/ LSCRN,KMAX,MACLEN(MACMX),MACFIL,TMNAME(MACMX),
     & TMAC(MACMX),MACLEV,MACSAVE(3,MACLEVMAX),FMSAVE(2,MACLEVMAX),
     & TMSAVE(MACLEVMAX)
      CHARACTER *(*) TIM,TANSW
      LOGICAL FYES
      DATA NOPER/69/
      DO K=1,KMAX
        IF(TIM.EQ.TMNAME(K)) THEN
          FYES=.TRUE.
          RETURN
        END IF
      END DO
      FYES=.FALSE.
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------ DO_INT_MAC_TYPE
CH
      ENTRY  DO_INT_MAC_TYPE(TANSW)
CH
CH --------------------------------------------------------------------
      TXTADW='Macros:'
      L=9
      DO K=1,KMAX
        IF(TPICDO.EQ.TMNAME(K)(1:2)) THEN
          IF(TANSW.EQ.'X?') THEN
            TXTADW(L:L+1)=TMNAME(K)(3:4)
            IF(L.LT.46) THEN
              L=L+3
            ELSE
              TXTADW(L:L+1)='..'
            END IF
          ELSE
            DO I=1,77
              IF(TMAC(K)(I:I).EQ.'!') THEN
                TXTADW=TMNAME(K)(3:4)//':'//TMAC(K)(I+1:46)
                GO TO 11
              END IF
            END DO
            TXTADW=TMNAME(K)(3:4)
   11       IF(TANSW.EQ.'Y?') WRITE(NOPER,2000) TXTADW
 2000       FORMAT(1X,A)
            CALL DWRC
          END IF
        END IF
      END DO
      IF(TANSW.EQ.'X?') CALL DWRC
      END
