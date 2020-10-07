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
C     8       Event flag 8 was set (e.g., click in Top bar of main window)
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
C
C  First check if previous text/number input was terminated by a mouse click
C
      IF(LG.EQ.1008) THEN
        NTYPE=8
        LG=0
        GOTO 102
      END IF
      CALL DGSBUT(ISBUT)
      IF(FLGOOB(18)) CALL DGETMD
      LENBUF=0
      IPTBUF=0
      IF(ALAST.NE.' '.AND.ALAST.NE.',') THEN
        AA=ALAST
        ALAST=' '
        GO TO 11
      END IF
C................................................. 1. Letter
C      IF(AA.EQ.'`') THEN
C        CALL DWR_BACKSPACE_SLASH(1)
C        CALL DQ_TOGGLE_HELP_DALI
C        GO TO 1
C      END IF
      FLGDOP=.TRUE.
    1 CALL DGETST(AA,LG,1,1)
      FLGDOP=.FALSE.
      IF(LG.EQ.-1008) THEN
        NTYPE=8
        GOTO 102
      END IF
      IF(LG.EQ.-1001) THEN
        IF(FLGOOB(18)) CALL DGETMD
        GO TO 1
      END IF
C..............................................................
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
         CALL DT_WRT('Enter logfile comment.')
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
              CALL DWRT('Overflow in macro save buffer.#')
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
         IF(LG.EQ.-1008) THEN
           NTYPE=8
           GOTO 102
         END IF
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
         IF(LG.EQ.-1008) THEN
           NTYPE=8
           GOTO 102
         END IF
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
      IF(LG.EQ.-1008) THEN
CC        NTYPE=8
C Change LG here for uniqueness
        LG=1008
        NTYPE=3
        NEXEC=1
        F=NUM/10.**NPOINT*SIGN
        GOTO 102
      END IF
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
      IF(LG.LE.0) THEN
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

      RETURN
 1000 CALL DWRT(' Overflow in macro save buffer.#')
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
CH &+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DOPER
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
      DATA T0789/'0','7','8','9'/
C      CHARACTER BACKSLASH
C      PARAMETER (BACKSLASH=CHAR(92))
C      DATA TPRG/BACKSLASH/

      CHARACTER *2 T1(1),T2(1)
      CHARACTER *2 TA,TPOS,TLAST,TCUR,TCW,TANSW,TRET
      DATA TRET/'``'/
      CHARACTER *3 THLP,TCR,TBACK
      CHARACTER *8 TINMA
      CHARACTER *4 TPTA
      CHARACTER *5 TIMA5,DT5
      CHARACTER *7 TIMA7,dt7
      DATA TIMA5/'(@..)'/
      DATA TIMA7/'(@....)'/
      CHARACTER *11 TIMPI,TIMPR
      DATA TIMPI/'(GG:PI:T) :'/
      DATA TIMPR/'(GG:PR:T) :'/
      CHARACTER *20 TIST
      DATA TLAST /' '/,TIST/' '/,TCUR/'QQ'/,TCW/'cw'/
      DATA NOPER/69/
      DATA NSP/1/
      CHARACTER *6 DT6
      CHARACTER *49 T49
      DATA JDEB/0/,DPTO/0.1/,NCHG/0/
      DIMENSION ICHG1(MPARDA),ICHG2(40)
      DATA ICHG1/MPARDA*0/,ICHG2/40*0/

      DATA DINCR/1./
      CHARACTER *50 TX1
C               123456789 123456789 123456789 123456789 123456789
      DATA TX1/'QU GB GG=GT=GO GC G# W# C# OF ON DO PI +-   NC OP'/
      LOGICAL NYES,CHG,F0,FSOL,FPRG,FSC,FDELT,FCHP
      DATA FPRG/.FALSE./,FDELT/.FALSE./
      TPRG=CHAR(92) ! Backslash
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PCF,J_PTE,J_PCT,J_PLA,J_PFR,J_PTO'
      CALL DPARAM(11
     &  ,J_PFI,J_PCF,J_PTE,J_PCT,J_PLA,J_PFR,J_PTO)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      FGETDO=.TRUE.
      NOCLDT=0
      DFLGDU=ABS(DFLGDU)
      NCHG=NCHG+1
      IF(TBACK.EQ.' ') THEN
        IF(TAN1DH.EQ.'   ') THEN
          THLP=THLPDO//' '
        ELSE
          THLP=TAN1DH
          TAN1DH=' '
        END IF
      ELSE
        THLP=TBACK
        TBACK=' '
      END IF
      GO TO 4
C    5 CALL DQHL_MENU(THLP)
C      GO TO 3
C
C    4 CALL DQHL_MENU(THLP)
C      NTYPE=2
    5 GO TO 3
    4 NTYPE=2

    1 IF(TLAST.EQ.' ') CALL DWRT_END(2)
    3 IF(.NOT.FMACDM) FINMDM=.FALSE.
      FSOL=FSOLD0
      FSOLD0=.FALSE.

      CALL DO_BAR_DRAW(NPOS,NSP,
     &  I1,N1,T1,P1,
     &  I2,N2,T2,P2)

    9 IF(NPOS.NE.0) THEN
        IF(NSP.EQ.1) THEN
          CALL DO_NEW_WINDOW(T1(NPOS),P1(1,NPOS),DINCR)
        ELSE
          CALL DO_NEW_WINDOW(T2(NPOS),P2(1,NPOS),DINCR)
        END IF
      ELSE
        CALL DO_NEW_WINDOW(' ',0.,0.)
      END IF

      CALL DQH_ACTIVE(NPOS,NSP,
     &  I1,N1,T1,P1,
     &  I2,N2,T2,P2)
      
      IF(THLP.NE.'OF') CALL DQHL_MENU(THLP)

    6 CALL DGZOOM(6,IAREDO,0,0)
      CALL DOP2LT(TLAST,NTYPE,NEXEC,TA,F)
      CALL DGZOOM(6,-1,0,0)

      IF(JDEB.EQ.1) THEN
        WRITE(TXTADW,8745) TLAST,NTYPE,NEXEC,TA,F
 8745   FORMAT('"',A,'"',2I2,'"',A,'"',F10.1)
        CALL DWRC
      END IF

      CALL DO_RENAME_COMMAND(TA)

      TANSW=TA
      KTYPE=NTYPE
C     ............................. check if button 1 is pressed
C     ......................... on the command list to show page
      IF(NEXEC.EQ.0) CALL DQHLT_PAGE(TA)
      IF(     TA(1:1).EQ.TPRG) THEN
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
    2 GO TO (1,20,30,40,51,60,70,80),NTYPE
C     NTYPE
C     1       1 Character
C     2       2 Characters
C     3       Absolute number
C     4       Increment
C     5       Nothing
C     6       Two letters with preceding point
C     7       Two letters with preceding ?
C     8       mouse click
C     8       Event flag 8 was set (e.g., click in Top bar of main window)


   80 CALL DO_BAR_OP(IACT,TA)
      GO TO(20,3,173,110,20,120),IACT+1
C     .............................. iact=0: not found
C     .............................. iact=1: window change or intrinsic macro
C     .............................. iact=2: TANSW='GT' go to top level
C     .............................. iact=3: <cr>
C     .............................. iact=4: new 2 letter command with ntype=2
C     .............................. iact=5: DO

CH    ...................................... general command: QU,EX = quit,exit
   20 IF(TA.EQ.'QU'.OR.TA.EQ.'EX') GO TO 173

C      IF(TA.EQ.'AA') THEN
C        CALL DO_BAR_OP(IACT,TA)
C        IF(IACT.EQ.1) GO TO 3
C        IF(IACT.EQ.2) GO TO 173
C        IF(IACT.EQ.3) GO TO 110
C      END IF

      IF(TA(1:1).EQ.'G') THEN
CH    ....... general command: GB = go back
CH    ....... general command: GT,GO,GG = go to, get
        IF(TA.EQ.'GB'.OR.TA.EQ.'GT'.OR.TA.EQ.'GG') GO TO 173
        IF(TA.EQ.'GO') THEN
          TA='GT'
          GO TO 173
        END IF
CH      ..... general command: G. = get data of current window
        IF(TA.EQ.'G.') THEN
          K=IAREDO
          GO TO 170
        END IF
CH      ..... general command: GC get window where cursor is set to
        IF(TA.EQ.'GC') THEN
          CALL DGCURG(HW,VW)
          CALL DPOAR(HW,VW,K)
          IF(K.GE.0) GO TO 170
        END IF
CH      ..... general command: G# = get data of window #
        CALL DAREA('G',TA,0,12,K,NYES)
        IF(NYES) GO TO 170
        DO LARE=-4,-1
          IF(TA(2:2).EQ.T0789(LARE)) THEN
            CALL DPCGVI(LARE,F0,IZOMDO)
            IF(F0) THEN
              CALL DWRT(' Virtual window '//T0789(LARE)//
     &          ' is empty.#')
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
            IF(P1(4,M).GE.-2.AND.P1(4,M).LE.2.) P1(4,M)=-P1(4,M)
C           IF(P1(4,M).EQ.-1.) P1(4,M)=1.
C           IF(P1(4,M).EQ.-2.) P1(4,M)=2.
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
            IF(P2(4,M).GE.-2.AND.P2(4,M).LE.2.) P2(4,M)=-P2(4,M)
C           IF(P2(4,M).EQ.-1.) P2(4,M)=1.
C           IF(P2(4,M).EQ.-2.) P2(4,M)=2.
            CHG=.TRUE.
            CALL DWR_ADD('=')
C           GO TO 3                           ! 20.5.96
            GO TO 5
         END IF
  720 CONTINUE
CH    ....... general command: W# select window #
      CALL DAREA('W',TA,0,12,IAREDO,NYES)
      IF(NYES) THEN
        IF(TPICDO(1:1).NE.'U') CALL DQHL_W
C        CALL DGZOOM(6,-1,0,0)
C        CALL DGZOOM(6,IAREDO,0,0)
        IF(TA(1:1).EQ.'w') GO TO 3
        GO TO 1
      END IF
      IF(NCOR.NE.0) THEN
CH       .... general command: C# copy to window #
         CALL DAREA('C',TA,0,12,IAREDO,NYES)
         IF(NYES) THEN
           NEX=4
           CALL DO_BAR_DO
           GO TO 998
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
CH       .... general command: ON, OF = on, off
         CALL DOFON(TA,P1,NPOS,NYES)
      ELSE
         CALL DOFON(TA,P2,NPOS,NYES)
      END IF
      IF(NYES) GO TO 50
CH    ....... general command: OP = switch to opposite angle
      IF(TA.EQ.'OP'.AND.NCOR.NE.0) THEN
         PARADA(2,J_PFI)=MOD( PARADA(2,J_PFI)+180.,360.)
         PARADA(2,J_PTE)=180.-PARADA(2,J_PTE)
         GO TO 50
      END IF
CH    ....... general command: draw without clear
      IF(TA.EQ.'NC'.AND.NTVI.EQ.1) THEN
         NOCL=1
         NOCLDT=1
         NEX=4
         CALL DO_BAR_DO
         GO TO 998
      END IF
      IF(TA.EQ.'T '.OR.TA.EQ.TRET) GO TO 110

      CALL D_FORCED_DRAW(TA,NTVI,NYES,NEX)
      IF(NYES) GO TO 998

      IF(TA.EQ.'TT') GO TO 110

      IF(NCOR.EQ.1) THEN
        CALL DPLANE_0(NPOS,TA,NEXEC,NYES)
        IF(NYES) GO TO 110
      END IF

   28 IF(TA.EQ.'+-') THEN
C       ........... If clicked on the bar one gets here,  otherwise to label 40
        IF(NPOS.LE.0) THEN
          CALL DWRT('Define the parameter to be changed first.#')
          GO TO 1
        END IF
        FDELT=.TRUE.
        CALL DWR_BACKSPACE_SLASH(2)
        CALL DWR_ADD(' ±')
        GO TO 6
      END IF
      IF(TA.EQ.'++'.OR.TA.EQ.'--') THEN
        IF(NPOS.NE.0) THEN
CH         .... general command: ++ add 1 to current parameter and DO
          IF(TA.EQ.'++') THEN
            IF(NSP.EQ.1) THEN
              P1(2,NPOS)=D3MIX(P1(1,NPOS),DINCR)
              IF(P1(4,NPOS).EQ.-1.) P1(4,NPOS)=1.
              IF(P1(4,NPOS).EQ.-2.) P1(4,NPOS)=2.
              IF(NCOR.EQ.1) CALL DPCOR(NPOS,TPOS)
            ELSE
              P2(2,NPOS)=D3MIX(P2(1,NPOS),DINCR)
              IF(P2(4,NPOS).EQ.-1.) P2(4,NPOS)=1.
              IF(P2(4,NPOS).EQ.-2.) P2(4,NPOS)=2.
            END IF
            CHG=.TRUE.
            NOCLDT=NOCL
            NEX=4
            CALL DO_BAR_DO
            GO TO 998
          END IF
CH        .... general command: ++ subtract 1 from current parameter and DO
          IF(TA.EQ.'--') THEN
            IF(NSP.EQ.1) THEN
              P1(2,NPOS)=D3MIX(P1(1,NPOS),-DINCR)
              IF(P1(4,NPOS).EQ.-1.) P1(4,NPOS)=1.
              IF(P1(4,NPOS).EQ.-2.) P1(4,NPOS)=2.
              IF(NCOR.EQ.1) CALL DPCOR(NPOS,TPOS)
            ELSE
              P2(2,NPOS)=D3MIX(P2(1,NPOS),-DINCR)
              IF(P2(4,NPOS).EQ.-1.) P2(4,NPOS)=1.
              IF(P2(4,NPOS).EQ.-2.) P2(4,NPOS)=2.
            END IF
            CHG=.TRUE.
            NOCLDT=NOCL
            NEX=4
            CALL DO_BAR_DO
            GO TO 998
          END IF
        ELSE
          TXTADW=' '
          CALL DWRT('Define a parameter to be incremented.#')
          GO TO 6
        END IF
      END IF
      IF(TA(1:1).EQ.'_') THEN
        READ(TA(2:2),1029,ERR=29) LEVUS
 1029   FORMAT(I1)
        IF(LEVUS.LE.7) THEN
          CALL DPARSV(81,'USL',2,FLOAT(LEVUS))
          GO TO 3
        END IF
      END IF
CH    ....... general command: T? = for specialists : time of start of ATLANTIS
   29 IF(TA.EQ.'T?') THEN
        CALL DATE4(TIST( 1:11))
        CALL TIME(TIST(13:20))
        CALL DWRT(TISTD0//' -> '//TIST)
        GO TO 1
      END IF
CH    ....... general command: ?? = search command or text in DALIhelp.
      IF(TA.EQ.'??') THEN
        CALL DO_INDEX
        GO TO 1
      END IF

C      IF(TA.EQ.'AA') THEN
C        CALL DGGTCU(HC,VC,.FALSE.)
C        TYPE *,HC,VC
C        GO TO 6
C      END IF

CH    ....... general command: QQ = measure display position with stick pointer
      IF(TA.EQ.TCUR) THEN
        CALL DMCU(HDUM,VDUM)
        TXTADW='HD = '//DT5(HDUM)//' VD = '//DT5(VDUM)
        CALL DWRC
        GO TO 1
      END IF
CH    ....... general command: QW = quit from wizzard
      IF(TA.EQ.'QW') THEN
        FWIZDW=.FALSE.
        CALL DQ_HEADER
        CALL DQHL_R
        GO TO 1
      END IF
      IF(TA.EQ.TCW) THEN
        FCONDW=.TRUE.
        GO TO 4
      END IF
      IF(TPICDO.NE.'PI'.AND.TA.EQ.'PI') THEN
        CALL DPI_DO_0(' ',' ')
        CALL DPIM(0)
        GO TO 110
      END IF

      IF(TANSW.EQ.'M$') THEN
        CALL D_PERSONAL_MACRO
        GO TO 3
      END IF

      TPTA=TNAMDO//TA
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
CH    ....... general command: X? short command list on terminal
CH    ....... general command: Y?  long command list on terminal
CH    ....... general command: X?  long command list on FOR069.DAT
      IF(TA.EQ.'X?'.OR.TA.EQ.'Y?'.OR.TA.EQ.'C?') THEN
        CALL DWRT('General commands :')
        CALL DWRT(TX1)
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
 2000         FORMAT(1X,A)
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
        GO TO 998
      END IF
C     ................................. back to last page maybe processor
C     IF(TA.EQ.'ba'.OR.TA.EQ.'BB') THEN   changed 50-SEP-1999
      IF(TA.EQ.'ba') THEN
        CALL DQHL_LAST_PAGE(TBACK,TCR)
        IF(TBACK.NE.TCR) THEN
          IF(     TBACK(1:2).EQ.TCR(1:2)) THEN
            THLP=TBACK
            TBACK=' '
            GO TO 5
          ELSE IF(TBACK(1:2).EQ.'GT') THEN
            TINMA='GT:'//TPRG//TBACK(3:3)//':'
            CALL DGINMA(TINMA(1:6))
          ELSE
            TINMA='GG:'//TBACK(1:2)//TPRG//TBACK(3:3)//':'
            CALL DGINMA(TINMA(1:7))
          END IF
          TBACK=' '
        END IF
        GO TO 3
      END IF
      IF(TA(1:1).EQ.'p') THEN
        READ(TA(2:2),1200,ERR=200) ITA
 1200   FORMAT(I1)
        F=ITA
        CALL DWR_BACKSPACE_SLASH(2)
        CALL DWR_ADD(TA(2:2))
        GO TO 30
      END IF
      IF(TA(1:1).EQ.'m') THEN
        READ(TA(2:2),1200,ERR=200) ITA
        F=-ITA
        CALL DWR_BACKSPACE_SLASH(2)
        TA(1:1)='-'
        CALL DWR_ADD(TA)
        GO TO 30
      END IF
  200 IF(TA.NE.'..') THEN
         NEX=2
         CALL DO_BAR_COMMAND(TA)
         GO TO 998
      END IF
   30 IF(FDELT) THEN
        DINCR=F
        FDELT=.FALSE.
        TXTADW='In-, decrement by ±'//DT7(DINCR)
        CALL DWRC
        CALL DWR_ADD(':')
        GO TO 9
      END IF
      IF(NPOS.LE.0) THEN
        CALL DWRT('Define the parameter to be changed first.#')
        TLAST=' '
        GO TO 1
      END IF
      IF(NSP.EQ.1) THEN
         IF(F.GE.P1(1,NPOS).AND.F.LE.P1(3,NPOS)) THEN
            P1(2,NPOS)=F
            IF(P1(4,NPOS).EQ.-1.) P1(4,NPOS)=1.
            IF(P1(4,NPOS).EQ.-2.) P1(4,NPOS)=2.
            ICHG1(NPOS)=NCHG
            IF(NCOR.EQ.1) THEN
              CALL DPCOR(NPOS,TPOS)
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
     &       DT6(P1(3,NPOS))//'#')
           TLAST=' '
           GO TO 1
         END IF
      ELSE
         IF(F.GE.P2(1,NPOS).AND.F.LE.P2(3,NPOS)) THEN
            P2(2,NPOS)=F
            IF(P2(4,NPOS).EQ.-1.) P2(4,NPOS)=1.
            IF(P2(4,NPOS).EQ.-2.) P2(4,NPOS)=2.
            ICHG2(NPOS)=NCHG
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
     &       DT6(P2(3,NPOS))//'#')
           TLAST=' '
           GO TO 1
         END IF
      END IF

   40 IF(NPOS.LE.0) THEN
        CALL DWRT('Define the parameter to be changed first.#')
        GO TO 1
      END IF
C     ............................. if typing +- one gets to here
      IF(F.EQ.0.) THEN
        FDELT=.TRUE.
        CALL DWR_BACKSPACE_SLASH(2)
        CALL DWR_ADD(' ±')
        GO TO 6
      END IF

      IF(NSP.EQ.1) THEN
         P1(2,NPOS)=D3MIX(P1(1,NPOS),F)
         IF(P1(4,NPOS).EQ.-1.) P1(4,NPOS)=1.
         IF(P1(4,NPOS).EQ.-2.) P1(4,NPOS)=2.
         ICHG1(NPOS)=NCHG
         IF(NCOR.EQ.1) THEN
           CALL DPCOR(NPOS,TPOS)
           CALL DDRNUM(NPOS)
         END IF
      ELSE
         P2(2,NPOS)=D3MIX(P2(1,NPOS),F)
         IF(P2(4,NPOS).EQ.-1.) P2(4,NPOS)=1.
         IF(P2(4,NPOS).EQ.-2.) P2(4,NPOS)=2.
         ICHG2(NPOS)=NCHG
      END IF
      DINCR=ABS(F)
   50 CHG=.TRUE.

   51 GO TO (4,110,120),NEXEC+1
C
C                                                     PICTURE ...
   60 CALL DWRT('DOPER error 60: TELL TO H.DREVERMANN#')
      GO TO 1
   70 GO TO 1
  110 NEX=3
      IF(FSOL) THEN
        TA='GB'
      ELSE
        TA='  '
      END IF
      GO TO 998
  120 NEX=4
      NOCL=0
C     CALL DWR_ADD_TEXT_DO
      TA='DO'
      CALL DO_BAR_DO
      GO TO 998
  170 CALL DPCGAR(K,F0,IZOMDO)
      IF(F0) THEN
        CALL DWRT(' Window '//TWINDW(K)//' is empty.#')
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

  998 PARADA(2,J_PTO)=MAX(PARADA(2,J_PTO),PARADA(2,J_PFR)+DPTO,0.)
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
      ENTRY DOPER_CHG(JSP,JPOS,FCHP)
CH
CH --------------------------------------------------------------------
CH
      FCHP=.FALSE.
      IF(JSP.EQ.1) THEN
        IF(ICHG1(JPOS).EQ.NCHG) FCHP=.TRUE.
      ELSE
        IF(ICHG2(JPOS).EQ.NCHG) FCHP=.TRUE.
      END IF
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
*DK DO_RENAME_COMMAND
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++ DO_RENAME_COMMAND
CH
      SUBROUTINE DO_RENAME_COMMAND(TA)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TA
      PARAMETER (MP=555)
      CHARACTER *2 TP(MP),TNEW(MP),TOLD(MP)
      LOGICAL FOF
      DATA FOF/.FALSE./
      IF(     TA.EQ.'&*'.OR.TA.EQ.'@@') THEN
        CALL DGOPEN(NUNIDU,TFILDC//'RENAME_COMMANDS',2,*99,ISTAT)
        DO K=1,MP
    1     READ(NUNIDU,1000,END=9) TP(K),TOLD(K),TNEW(K)
 1000     FORMAT(3(1X,A2))
          IF(TP(K).EQ.' ') GO TO 1
        END DO
        K=K+1
    9   K=K-1
        CLOSE(UNIT=NUNIDU)
        CALL DWRT('RENAME_COMMANDS stored')
        IF(TA.EQ.'&*') TA='T '
      ELSE IF(TA.EQ.'**') THEN
        DO N=1,K
          IF(TP(N).EQ.'**'.OR.TP(N).EQ.TNAMDO) THEN
            TXTADW=TP(N)//': '//TOLD(N)//' => '//TNEW(N)
            CALL DWRC
          END IF
        END DO
        TA='T '
      ELSE
C       ............................. change old to new
        IF(FOF) RETURN
        DO N=1,K
          IF(TA.EQ.TNEW(N)) THEN
            IF(TP(N).EQ.TNAMDO.OR.TP(N).EQ.'**') THEN
              TA=TOLD(N)
              RETURN
            END IF
          END IF
        END DO
C       ............................. inhibit old
        DO N=1,K
          IF(TA.EQ.TOLD(N)) THEN
            IF(TP(N).EQ.TNAMDO.OR.TP(N).EQ.'**') THEN
              CALL CUTOL(TA)
              RETURN
            END IF
          END IF
        END DO
      END IF
      RETURN
   99 IF(TA.EQ.'@@') FOF=.TRUE.
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------ DO_RENAME_TEXT
CH
      ENTRY DO_RENAME_TEXT(TA)
CH
CH --------------------------------------------------------------------
      IF(FOF) RETURN
      DO N=1,K
        IF(TP(N).EQ.TNAMDO.AND.TA.EQ.TOLD(N)) THEN
          TA=TNEW(N)
          RETURN
        END IF
      END DO
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
C?    24.4.97
C?    DO L=1,LTC,3
      DO L=1,LTC-1,3
        N=N+1
        TLIST(N)=T1(L:L+1)
C?      IF(T1(L+2:L+2).NE.'"') RETURN
        LP2=L+2
C?      IF(T1(L+2:L+2).NE.'"') RETURN
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
CH
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
      DATA NOPER/69/
      LOGICAL FYES
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
      ENTRY DO_INT_MAC_TYPE(TANSW)
CH
CH --------------------------------------------------------------------
      TXTADW='Macros:'
      L=9
      DO K=1,KMAX
        IF(TNAMDO.EQ.TMNAME(K)(1:2)) THEN
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
*DK DOPRIN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DOPRIN
CH
      SUBROUTINE DOPRIN
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C_CG 10-May-1989 C.Grab  Adapted to CERNVM
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
      DIMENSION IDD(MPNPDD)
      EQUIVALENCE (IDD(1),MXSADD)
      DO 710 L=1,MPNPDD
         IDD(L)=L
  710 CONTINUE
      RETURN
      END
*DK DO_BAR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DO_BAR
CH
      SUBROUTINE DO_BAR(IACT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Input: IACT = 1 read file and store positions
C            IACT = 2 read file, store positions and resize,
C                     if no bar resize without bar,
C                     if no input file resize without bar,

      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TANSW,TP1(*),TP2(*),TSTAT,TOUT
      DIMENSION P1(4,*),P2(4,*)
      CHARACTER *1 TM,TW,TDUM
      DATA DVT/6./,DH/45./,DV/30./,DW/8./,DVM/4./
      DATA DUPL/38./,DUPS/17./DSP/8./,DLET/8./,DBMIN/56./,VMIN/696./
      DATA DF1/2./,DI1/2./,DI2/2./,LFIL/50/
      DIMENSION IGC(12)

      PARAMETER (MCOM=399,MPIC=100)
      DIMENSION ICOL(MCOM),ISIZ(MCOM),ION(MCOM)
      DIMENSION H1(MCOM),H2(MCOM),V1(MCOM),V2(MCOM)
      
      CHARACTER *2 TACT(MCOM),TDEP(MCOM),TDEPU
      CHARACTER *18 TXT(MCOM),TNAM(MPIC),TDO(MPIC)
      DATA TNAM/MPIC*' '/
C     CHARACTER *15 TCOM(MCOM)
      CHARACTER *13 TCOM(MCOM)
      CHARACTER *27 THLP(MCOM)
      CHARACTER *2  TPR(MCOM),TP,TPIC(MPIC)
      LOGICAL FNPCK(MCOM)

      CHARACTER *2 TZOM(0:1),TVAR
      CHARACTER *100 TWRK
      DATA TZOM/'NZ','ZO'/

      PARAMETER (MSTST=19)
      CHARACTER *2 TSTST(MSTST)

      PARAMETER (MLP=70,MLIN=6)
      CHARACTER *75 TAR(MLIN)
      DIMENSION HPLTX(6),VPLTX(6)  
      DATA HPLTX/  3.,450.,  3.,450.,  3.,450./
      DATA VPLTX/ 35., 35., 19., 19.,  3.,  3./

      CHARACTER *2 TUSL(0:7)
      DATA TUSL/'_0','_1','_2','_3','_4','_5','_6','_7'/

      LOGICAL FOUND,FN,FOF
      DATA KDEB1/0/
      DATA KDEB2/0/
      DATA KDEB3/0/
      DATA KDEB4/0/
      DATA KDEB5/0/
      DATA KDEB6/0/
      DATA JDEB/0/

      NACT=IACT
      NDO=0

      FN=.TRUE.
   22 CALL DPARGV_24(84,'OBA',DBAR,IBAR)
      IF(IBAR.LT.0.) THEN
        IF(NACT.EQ.2) THEN
          CALL DGMBOF(0.,0.,0.,0.)
          CALL DGNSIZ(1.,1.,POSIDW(8),POSIDW(9)+POSIDW(10))
        END IF
        RETURN
      END IF

      CALL DGOPEN(NUNIDU,TFILDC//'BAR',2,*99,ISTAT)
      FN=.FALSE.

      VM=POSIDW(9)+POSIDW(10)
C     VT=VM+DVT
      HL=1.
      HLW=HL
      HR=POSIDW(8)
      HRW=HR
      IF(VM.LT.VMIN) THEN
        DBAR=35.
        DUP=DUPS
      ELSE
        DUP=DUPL
      END IF
      NPIC=0
      IUP=1
      TP=' '

      IF(NACT.EQ.2) THEN
        CALL DGMBOF(0.,0.,0.,0.)
        CALL DGNSIZ(1.,1.,POSIDW(8),POSIDW(9)+POSIDW(10)+DBAR)
        CALL DGMBON(1.,POSIDW(9)+POSIDW(10),
     &       POSIDW(8),POSIDW(9)+POSIDW(10)+DBAR)
      END IF

      DO N=1,MCOM
    1   READ(NUNIDU,1000,END=9) TXTADW
 1000   FORMAT(A)
        IF(TXTADW(1:2).EQ.' ') GO TO 1
        IF(TXTADW(2:6).EQ.'color') THEN
          READ(TXTADW,1004) NC,IGC(NC)
 1004     FORMAT(6X,I3,I3)
          GO TO 1
        END IF

C L   0. 10  0 Rubber b.?        RB| RB           ZO| If pr., get rubber band
C 112345123123 12345678901234567812  123456789012312  1234


        READ(TXTADW,1001,ERR=8) TM,DHP,ICOL(N),ISIZ(N),
     &    TXT(N),TACT(N),TCOM(N),TDEP(N),THLP(N)
 1001   FORMAT(1X,A,F5.0,2I3,1X,2A,2X,2A,2X,A)
        GO TO 2
    8   CALL DWRC
        CALL DWRT('Wrong Format#')
        GO TO 1
    2   IF(TM.EQ.'U') THEN
          IF(TP.EQ.' ') THEN
            IF(TACT(N).EQ.' ') THEN
C             ........................................... first line with U
              HL2=HLW
              HR2=HRW
            ELSE
C             ........................................... second line with U
              HL2=HL
              HR2=HR
              NPIC=0
            END IF
          END IF

          NPIC=NPIC+1
          TPIC(NPIC)=TACT(N)
          TNAM(NPIC)=TXT(N)

          TP=TACT(N)
          HL=HL2
          HR=HR2
          IUP=2
          GO TO 1
        END IF
        
C        IF(TXT(N)(1:1).EQ.'!') THEN
C          READ(TXT(N)(2:3),1006) ICH
C 1006     FORMAT(I2)
C          TXT(N)(1:3)=CHAR(ICH)
C        END IF

        IF(IUP.EQ.1) THEN
          V1(N)=VM
          V2(N)=VM+DUP
          IF(DBAR.LT.DBMIN.AND.TCOM(N).NE.'W#:') THEN
            LTXT=LENOCC(TXT(N))
            DO L=1,LTXT
              IF(TXT(N)(L:L).EQ.'\'.OR.TXT(N)(L:L).EQ.';') THEN
                READ(NUNIDU,1000,END=9) TXTADW
                READ(TXTADW,1001,ERR=8) TDUM,DHP,ICOL(N),ISIZ(N),
     &            TXT(N),TACT(N),TCOM(N),THLP(N)
                LTXT=LENOCC(TXT(N))
                GO TO 11
              END IF
            END DO
   11       DHP=DSP+DLET*LTXT
            ISIZ(N)=1
          END IF
        ELSE
          V1(N)=VM+DUP
          V2(N)=VM+DBAR

          IF(TACT(N)(1:1).EQ.'%'.OR.
     &      TACT(N).EQ.'&c') THEN
            CALL DO_BAR_COL_0(TACT(N),DHP)
          ELSE
            LTXT=LENOCC(TXT(N))
            IF(TACT(N).EQ.'$F') LTXT=LFIL
            DHP=DSP+DLET*LTXT
          END IF

          ION(N)=ISIZ(N)
          ISIZ(N)=1
        END IF

        IF(     TM.EQ.'L') THEN
          IF(ION(N).EQ.-2) THEN
            H1(N)=H1(N-1)
            H2(N)=H2(N-1)
          ELSE
            IF(HL+DHP.GT.HR) GO TO 1
            IF(TACT(N).EQ.'W#'.AND.DBAR.LT.DBMIN) HLW=HL+DHP
            H1(N)=HL
            HL=HL+DHP
            H2(N)=HL
          END IF
        ELSE IF(TM.EQ.'R') THEN
          IF(ION(N).EQ.-2) THEN
            H1(N)=H1(N-1)
            H2(N)=H2(N-1)
          ELSE
            H2(N)=HR
            HR=HR-DHP
            IF(HR.LT.HL) GO TO 1
            H1(N)=HR
            IF(TACT(N).EQ.'W#'.AND.DBAR.LT.DBMIN) HRW=HR
          END IF
        ELSE
          GO TO 1
        END IF
        TPR(N)=TP
        IF(TACT(N).EQ.'DO') NDO=N
      END DO
      CALL DWRT('Too many commands in the file *.BAR.#')
    9 NCOM=N-1
      CALL DO_WINDOW_TXT_0(IGC(7),IGC(8))
      CLOSE(UNIT=NUNIDU)
      TVAR='??'
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------------------- DO_BAR_DRAW
CH
      ENTRY DO_BAR_DRAW(NPOS,NSP,I1,L1,TP1,P1,I2,L2,TP2,P2)
CH
CH --------------------------------------------------------------------
CH
      IF(FN.OR.FMACDM) RETURN
      CALL DO_BAR_PS_1
      IF(NPOS.EQ.0) THEN
        TVAR='??'
      ELSE
        IF(NSP.EQ.1) THEN
          TVAR=TP1(NPOS)
        ELSE
          TVAR=TP2(NPOS)
        END IF
        CALL CUTOL(TVAR)
      END IF
  100 CALL DGLEVL(IGC(1))
      CALL DPARGI(81,'USL',LEVUS)
      CALL DQFAR(1.,VM,POSIDW(8)+DI2,VM+DBAR-1.)
      CALL DGLEVL(IGC(4))
      CALL DQDAR(1.,VM+DI1,POSIDW(8),VM+DBAR)
      CALL DGLEVL(IGC(2))
      VU=VM+DUP
      CALL DQDRAW(HLW,VU,HRW,VU)

      DO N=1,NCOM
        IF(TPR(N).EQ.' '.OR.TPR(N).EQ.TNAMDO) THEN
          IF(TDEP(N).NE.' '.AND.TACT(N).NE.'$O') THEN
            FNPCK(N)=.TRUE.

C           ........... first letter of TDEP = upper case: accept if on or set
            I=ICHAR(TDEP(N)(1:1))
            IF(I.LE.90) THEN

              DO K=I1,L1
                IF(TP1(K).EQ.TDEP(N)) THEN
                  IF(P1(4,K).LT.0.) THEN
                    GO TO 301
                  ELSE
                    GO TO 303
                  END IF
                END IF
              END DO

              DO K=I2,L2
                IF(TP2(K).EQ.TDEP(N)) THEN
                  IF(P2(4,K).LT.0.) THEN
                    GO TO 301
                  ELSE
                    GO TO 303
                  END IF
                END IF
              END DO

C             .......................... ZO,NZ, ...
              DO K=1,NSTST
                IF(TSTST(K).EQ.TDEP(N)) GO TO 303
              END DO

              IF(TACT(N).EQ.'$g') THEN
                FNPCK(N)=.FALSE.
                CALL DO_TXT_SIZE(IGC(12),ISIZ(N),TXT(N),H1(N),V1(N)+DVT)
              END IF
              GO TO 301

            ELSE
C             . first letter of TDEP = lower case : accept if not on or not set

              TDEPU=TDEP(N)
              CALL=CLTOU(TDEPU)
              DO K=I1,L1
                IF(TP1(K).EQ.TDEPU) THEN
                  IF(P1(4,K).LT.0.) THEN
                    GO TO 303
                  ELSE
                    GO TO 301
                  END IF
                END IF
              END DO

              DO K=I2,L2
                IF(TP2(K).EQ.TDEPU) THEN
                  IF(P2(4,K).LT.0.) THEN
                    GO TO 303
                  ELSE
                    GO TO 301
                  END IF
                END IF
              END DO

C             .......................... ZO,NZ, ...
              DO K=1,NSTST
                IF(TSTST(K).EQ.TDEPU) THEN

                  IF(TACT(N).EQ.'$g') THEN
                    FNPCK(N)=.FALSE.
                    CALL DO_TXT_SIZE(
     &                IGC(12),ISIZ(N),TXT(N),H1(N),V1(N)+DVT)
                  END IF
                  GO TO 301
                END IF
              END DO
              GO TO 303
            END IF
          END IF

  303     FNPCK(N)=.FALSE.
          IF(     TACT(N).EQ.'W#') THEN
C           ............................................. windows
            H=H1(N)
            HW=H
            DO K=1,5
              CALL DO_BAR_WINDOWS(K,H,VM+DVM,DH,DV,IGC(5),IGC(6))
              H=H+DH+DW
            END DO
          ELSE IF(TACT(N)(1:1).EQ.'%'.OR.TACT(N).EQ.'&c') THEN
            CALL DO_BAR_COL(TACT(N),H1(N),V1(N))
C           .............................................. color bar
          ELSE IF(TACT(N)(1:1).EQ.'_') THEN
            IF(TACT(N).EQ.TUSL(LEVUS)) THEN
              ION(N)=ABS(ION(N))
              CALL DO_TXT_SIZE(ICOL(N),ISIZ(N),TXT(N),H1(N),V1(N)+DVT)
            ELSE
              ION(N)=-ABS(N)
            END IF
          ELSE
  302       NCOL=ICOL(N)
            IF(     TACT(N).EQ.TNAMDO.OR.
C    &              TACT(N).EQ.TZOM(IZOMDO).OR.
     &              TACT(N).EQ.TVAR) THEN
              NCOL=IGC(9)
            ELSE IF(TACT(N).EQ.'$O') THEN

              DO K=I1,L1
                IF(TP1(K).EQ.TCOM(N)(1:2)) THEN
                  IF(P1(4,K).GT.0.) NCOL=IGC(9)
                  GO TO 300
                END IF
              END DO

              DO K=I2,L2
                IF(TP2(K).EQ.TCOM(N)(1:2)) THEN
                  IF(P2(4,K).GT.0.) NCOL=IGC(9)
                  GO TO 300
                END IF
              END DO

              DO K=1,NSTST
                IF(TSTST(K).EQ.TDEP(N)) THEN
                  NCOL=IGC(9)
                  GO TO 300
                END IF
              END DO

            ELSE IF(ION(N).LT.0) THEN
              GO TO 301
            ELSE IF(TACT(N).EQ.'$F') THEN
              IF(LNINDE(2).GT.0) THEN
                NFIL=2
              ELSE
                NFIL=1
              END IF
              LTXT=LENOCC(TFINDE(NFIL))
              TWRK='| '//TFINDE(NFIL)(1:LTXT)//' |'
              CALL DO_TXT_SIZE(NCOL,ISIZ(N),TWRK,H1(N),V1(N)+DVT)
              GO TO 301
            END IF
  300       CALL DO_BAR_USE(TCOM(N),TACT(N),NPOS,IGC,NCOL)
            IF(TCOM(N).EQ.'do') THEN

C             ...................... DO is overwritten with NCOL>0
              IF(NCOL.GT.0) 
     &          CALL DO_TXT_SIZE(NCOL,ISIZ(NDO),TXT(NDO),
     &          H1(NDO),V1(NDO)+DVT)

              GO TO 301
            END IF
            CALL DO_TXT_SIZE(NCOL,ISIZ(N),TXT(N),H1(N),V1(N)+DVT)
          END IF
        END IF
  301 END DO

      IF(KDEB1.EQ.0) CALL DGCHKX
      CALL DO_BAR_PS_2
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------------------- DO_BAR_DO
CH
      ENTRY DO_BAR_DO
CH
CH --------------------------------------------------------------------
CH
      IF(FN) RETURN
      CALL DO_BAR_PS_1
      CALL DGLEVL(IGC(3))
      CALL DQDAR(1.,VM+DI1,POSIDW(8),VM+DBAR)
      IF(KDEB4.EQ.0) CALL DGCHKX
      CALL DO_BAR_PS_2
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------------------- DO_BAR_COMMAND
CH
      ENTRY DO_BAR_COMMAND(TANSW)
CH
CH --------------------------------------------------------------------
CH
      IF(FN.OR.FMACDM) RETURN
      DO N=1,NCOM
        IF(TACT(N).EQ.TANSW) THEN
          CALL DO_BAR_PS_1
          CALL DO_TXT_SIZE(
     &      IGC(9),ISIZ(N),TXT(N),H1(N),V1(N)+DVT)
          IF(KDEB3.EQ.0) CALL DGCHKX
          CALL DO_BAR_PS_2
          RETURN
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
CH ----------------------------------------------------------- DO_BAR_STATUS_0
CH
      ENTRY DO_BAR_STATUS_0
CH
CH --------------------------------------------------------------------
CH
      CALL DPARGI(81,'USL',LEVUS)

      DO N=1,NCOM
        ION(N)=-ABS(ION(N))
C       IF(ION(N).GT.0) ION(N)=-ION(N)
      END DO

      CALL DQH_ON(' ')

      NSTST=1
      TSTST(NSTST)=TUSL(LEVUS)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------------------- DO_BAR_STATUS
CH
      ENTRY DO_BAR_STATUS(TSTAT,N1,TOUT)
CH
CH --------------------------------------------------------------------
CH
      IF(TSTAT.EQ.' ') THEN
        DO N=1,NCOM
          IF(ION(N).GT.0) ION(N)=-ION(N)
        END DO
        CALL DQH_ON(' ')
        NSTST=0
      ELSE
        CALL DQH_ON(TSTAT)

        NSTST=MIN(NSTST+1,MSTST)
        TSTST(NSTST)=TSTAT

        IF(N1.GT.0) TOUT(N1:N1+1)=TSTAT
        DO N=1,NCOM
          IF(TNAMDO.EQ.TPR(N).AND.TACT(N).EQ.TSTAT) THEN
            ION(N)=ABS(ION(N))
            RETURN
          END IF
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
CH ----------------------------------------------------------- DO_BAR_OP
CH
      ENTRY DO_BAR_OP(IACT,TANSW)
CH
CH --------------------------------------------------------------------
CH
C                                        intrinsic macro
C     output :IACT=0 error                
C             IACT=1 window was changed   no              start with DOP2LT
C             IACT=1 else                 yes             start with DOP2LT
C             IACT=2 select top level   TANSW='GT'        go to nex=1 return
C             IACT=3 if command = cr      no              go to <cr>
C             IACT=4 tansw=2 2-letter comand
C             IACT=5 DO

      IACT=0
      IF(FN) RETURN
      DLINDD=DF1
      IF(FMACDM) RETURN

C     CALL DGGCUR(HC,VC)
C     CALL DGCURG(HC,VC)
      CALL DGGTCU(HC,VC,.FALSE.)          ! false = window is not raised

      IF(VC.GT.VM) THEN
        DO L=1,NCOM
          IF(TPR(L).EQ.' '.OR.TPR(L).EQ.TNAMDO) THEN
            IF(FNPCK(L).OR.ION(L).LT.0.OR.TACT(L).EQ.'$$') GO TO 401
            IF(HC.GE.H1(L).AND.HC.LT.H2(L).AND.
     &         VC.GE.V1(L).AND.VC.LT.V2(L)) THEN
              IF(     TCOM(L).EQ.'W#:') THEN
                H=HW
                DO K=1,5
                  CALL DO_BAR_PICK(K,H,VM+DVM,DH,DV,HC,VC,FOUND)
                  IF(FOUND) THEN
C                    CALL DGZOOM(6,-1,0,0)
C                    CALL DGZOOM(6,IAREDO,0,0)
                    IACT=1
                    RETURN
                  END IF
                  H=H+DH+DW
                END DO
                CALL DWRT('window ?#')
              ELSE IF(TCOM(L).EQ.'<') THEN
                CALL DQHLP('<<')
                IACT=1
              ELSE IF(TCOM(L).EQ.'help') THEN
                DO N=1,NCOM
                  IF(ION(N).GE.0.AND.
     &              TACT(N)     .NE.'W#'.AND.
     &              TACT(N)(1:1).NE.'%' ) THEN
                    IF(TPR(N).EQ.' '.OR.TPR(N).EQ.TNAMDO)
     &                CALL DO_BAR_HELP(ICOL(N),TXT(N),THLP(N))
                  END IF
                END DO
                IACT=1
              ELSE IF(TCOM(L).EQ.'GT:') THEN
                IACT=2
                TANSW='GT'
              ELSE IF(TACT(L).EQ.'&c') THEN
                CALL DO_BAR_COL_BY_NAME(TACT(L),TXT(L),HC,IACT,TANSW)
              ELSE IF(TACT(L)(1:1).EQ.'%') THEN
                CALL DO_BAR_COL_OP(TACT(L),TXT(L),HC,IACT)
                RETURN
              ELSE IF(TCOM(L).EQ.'cr') THEN
                IACT=3
                RETURN
              ELSE IF(TCOM(L)(3:3).EQ.' '.
     &            AND.TCOM(L)(2:2).NE.' ')THEN
                TANSW=TCOM(L)(1:2)
                CALL DWR_ADD(TANSW)
                IF(TCOM(L).EQ.'DO') THEN
                  IACT=5
                ELSE
                  IACT=4
                END IF
              ELSE
                LTXT=LENOCC(TCOM(L))
                IF(TNAMDO.EQ.'GT'.AND.TCOM(L)(1:3).EQ.'GG:') THEN
                  CALL DGINMA(TCOM(L)(4:LTXT))
                  IF(JDEB.EQ.1) THEN
                    TXTADW='dginma 2: "'//TCOM(L)(4:LTXT)//'"'
                    CALL DWRC
                  END IF
                ELSE
                  CALL DGINMA(TCOM(L)(1:LTXT))
                  IF(JDEB.EQ.1) THEN
                    TXTADW='dginma 2: "'//TCOM(L)(1:LTXT)//'"'
                    CALL DWRC
                  END IF
                END IF
                IACT=1
              END IF
              RETURN
            END IF
          END IF
  401   END DO
      END IF
      IACT=0
      RETURN
   99 IF(IACT.EQ.2) THEN
        CALL DGMBOF(0.,0.,0.,0.)
        CALL DGNSIZ(1.,1.,POSIDW(8),POSIDW(9)+POSIDW(10))
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------------------- DO_BAR_ANSWER
CH
      ENTRY DO_BAR_ANSWER(HT1,HT2,TANSW,ISL)
CH
CH --------------------------------------------------------------------
CH
      IF(.NOT.FN) THEN
        CALL DO_BAR_PS_1
        CALL DGLEVL(IGC(1))
        CALL DQFAR(1.,VM,POSIDW(8)+DI2,VM+DBAR-1.)
        CALL DGLEVL(IGC(3))
        CALL DQDAR(1.,VM+DI1,POSIDW(8),VM+DBAR)
        IF(TANSW.NE.' ') THEN
          VU=VM+DUP
          CALL DGLEVL(IGC(10))
          CALL DGTEXT(HT1,VU,TANSW,LENOCC(TANSW))
        END IF
        IF(TXTADW.NE.' ') THEN
          CALL DO_TXT_SIZE(IGC(11),ISL,TXTADW,HT2,VM+DVT)
        END IF
        IF(KDEB5.EQ.0) CALL DGCHKX
        CALL DO_BAR_PS_2
      END IF
      CALL DQH_SEND_ALL_SLEEP
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------------------- DO_BAR_WRT
CH
      ENTRY DO_BAR_WRT(HT2,ISL,TANSW)
CH
CH --------------------------------------------------------------------
CH
      IF(FN) RETURN
      CALL DO_BAR_PS_1
      CALL DGLEVL(IGC(1))
      CALL DQFAR(1.,VM,POSIDW(8)+DI2,VM+DBAR-1.)
      CALL DGLEVL(IGC(3))
      CALL DQDAR(1.,VM+DI1,POSIDW(8),VM+DBAR)
      CALL DO_TXT_SIZE(IGC(11),ISL,TANSW,HT2,VM+DVT)
      IF(KDEB6.EQ.0) CALL DGCHKX
      CALL DO_BAR_PS_2
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------------------- DO_BAR_ANSWER
CH
      ENTRY DO_BAR_ANSWER_PLATFORM_TEXT(TANSW)
CH
CH --------------------------------------------------------------------
CH
      CALL DQH_SEND_ALL_SLEEP
      IF(FN) RETURN
      CALL DO_BAR_PS_1
      CALL DGLEVL(IGC(1))
      CALL DQFAR(1.,VM,POSIDW(8)+DI2,VM+DBAR-1.)
      CALL DGLEVL(IGC(3))
      CALL DQDAR(1.,VM+DI1,POSIDW(8),VM+DBAR)
      CALL DW_GET_PLATFORM_TEXT_ARRAY(TANSW,MLP,MLIN,TAR,NLIN)
      DO N=1,NLIN
        READ(TAR(N)(1:5),1010,ERR=10) IPLTX,IPLCO
 1010   FORMAT(I2,I3)
        CALL DGLEVL(IPLCO)
        L=LENOCC(TAR(N))
        CALL DGTEXT(HPLTX(IPLTX),VM+VPLTX(IPLTX),TAR(N)(7:L),L-6)
   10 END DO
      IF(KDEB6.EQ.0) CALL DGCHKX
      CALL DO_BAR_PS_2
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------------------- DO_BAR_NAME
CH
      ENTRY DO_BAR_NAME(TOUT)
CH
CH --------------------------------------------------------------------
CH
      DO K=1,NPIC
        IF(TNAMDO.EQ.TPIC(K)) THEN
          TOUT=TNAM(K)
          RETURN
        END IF
      END DO
      TOUT=' '
      END
*DK DO_BAR_WINDOWS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DO_BAR_WINDOWS
CH
      SUBROUTINE DO_BAR_WINDOWS(NP,HL,VL,DH,DV,IC_AR,IC_LI)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(7),V(5)
      DATA D1/1./

      DLINDD=D1
      CALL DQFFAR(HL,VL,HL+DH,VL+DV,IC_AR,IC_LI)


      DHS=DH/6.
      DVS=DV/4.

      H(1)=HL
      DO K=2,7
        H(K)=H(K-1)+DHS
      END DO

      V(1)=VL
      DO K=2,5
        V(K)=V(K-1)+DVS
      END DO

      IF(     NP.EQ.1) THEN

        CALL DQDRAW(H(1),V(3),H(7),V(3))  ! long  horizontal
        CALL DQDRAW(H(3),V(1),H(3),V(5))  ! left  vertical
        CALL DQDRAW(H(5),V(1),H(5),V(5))  ! right vertical

C       CALL DGLEVL(IC_TX)
        CALL DO_WINDOW_TXT(H(2),V(4),'1')
        CALL DO_WINDOW_TXT(H(2),V(2),'2')
        CALL DO_WINDOW_TXT(H(4),V(4),'3')
        CALL DO_WINDOW_TXT(H(4),V(2),'4')
        CALL DO_WINDOW_TXT(H(6),V(4),'5')
        CALL DO_WINDOW_TXT(H(6),V(2),'6')

      ELSE IF(NP.EQ.2) THEN
        CALL DQDRAW(H(3),V(1),H(3),V(5))  ! left  vertical
        CALL DQDRAW(H(5),V(1),H(5),V(5))  ! right vertical

C       CALL DGLEVL(IC_TX)
        CALL DO_WINDOW_TXT(H(2),V(3),'L')
        CALL DO_WINDOW_TXT(H(4),V(3),'M')
        CALL DO_WINDOW_TXT(H(6),V(3),'R')

      ELSE IF(NP.EQ.3) THEN
        CALL DQDRAW(H(1),V(3),H(5),V(3))  ! short horizontal
        CALL DQDRAW(H(5),V(1),H(5),V(5))  ! right vertical

C       CALL DGLEVL(IC_TX)
        CALL DO_WINDOW_TXT(H(3),V(4),'U')
        CALL DO_WINDOW_TXT(H(3),V(2),'D')
        CALL DO_WINDOW_TXT(H(6),V(3),'R')

      ELSE IF(NP.EQ.4) THEN
        CALL DQDRAW(H(5),V(1),H(5),V(5))  ! right vertical

C       CALL DGLEVL(IC_TX)
        CALL DO_WINDOW_TXT(H(3),V(3),'S')
        CALL DO_WINDOW_TXT(H(6),V(3),'R')

      ELSE IF(NP.EQ.5) THEN

C       CALL DGLEVL(IC_TX)
        CALL DO_WINDOW_TXT(H(4),V(3),'W')

      END IF

      END

*DK DO_BAR_PICK
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DO_BAR_PICK
CH
      SUBROUTINE DO_BAR_PICK(NP,HL,VL,DH,DV,HC,VC,FOUND)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(7),V(5)
      LOGICAL FOUND
      FOUND=.FALSE.

      IF(HC.LE.HL.OR.HC.GE.HL+DH) RETURN
      IF(VC.LE.VL.OR.VC.GE.VL+DV) RETURN

      DHS=DH/6.
      DVS=DV/4.

      H(1)=HL
      DO K=2,7
        H(K)=H(K-1)+DHS
      END DO

      V(1)=VL
      DO K=2,5
        V(K)=V(K-1)+DHS
      END DO

      IF(     NP.EQ.1) THEN
        CALL DO_PICK(HC,VC,H(1),V(3),H(3),V(5), 1,FOUND)
        CALL DO_PICK(HC,VC,H(1),V(1),H(3),V(3), 2,FOUND)
        CALL DO_PICK(HC,VC,H(3),V(3),H(5),V(5), 3,FOUND)
        CALL DO_PICK(HC,VC,H(3),V(1),H(5),V(3), 4,FOUND)
        CALL DO_PICK(HC,VC,H(5),V(3),H(7),V(5), 5,FOUND)
        CALL DO_PICK(HC,VC,H(5),V(1),H(7),V(3), 6,FOUND)

      ELSE IF(NP.EQ.2) THEN

        CALL DO_PICK(HC,VC,H(1),V(1),H(3),V(5), 9,FOUND)
        CALL DO_PICK(HC,VC,H(3),V(1),H(5),V(5),10,FOUND)
        CALL DO_PICK(HC,VC,H(5),V(1),H(7),V(5),11,FOUND)

      ELSE IF(NP.EQ.3) THEN

        CALL DO_PICK(HC,VC,H(1),V(3),H(5),V(5), 7,FOUND)
        CALL DO_PICK(HC,VC,H(1),V(1),H(5),V(3), 8,FOUND)
        CALL DO_PICK(HC,VC,H(5),V(1),H(7),V(5),11,FOUND)

      ELSE IF(NP.EQ.4) THEN

        CALL DO_PICK(HC,VC,H(1),V(1),H(5),V(5),12,FOUND)
        CALL DO_PICK(HC,VC,H(5),V(1),H(7),V(5),11,FOUND)

      ELSE IF(NP.EQ.5) THEN

        IAREDO=0
        FOUND=.TRUE.

      END IF

      END


*DK DO_TXT_SIZE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DO_TXT_SIZE
CH
      SUBROUTINE DO_TXT_SIZE(ICOL,NSIZ,TXT,H,V)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TXT
      DATA DV1/12./,DV2/-2./,DVS/-3./
      LTXT=LENOCC(TXT)
      ISIZ=MIN(6,NSIZ)
      IF(ISIZ.GE.3) THEN
        CALL DGTXTF(ICOL,ISIZ,TXT,0.,H,V,LTXT)
      ELSE
        CALL DGLEVL(ICOL)
        IF(ISIZ.EQ.2) THEN
          DO L=1,LTXT
            IF(TXT(L:L).EQ.';') THEN
              CALL DGTEXT(H,V+DV1,TXT,L-1)
              CALL DGTEXT(H,V+DV2,TXT(L+1:LTXT),LTXT-L)
              RETURN
            END IF
          END DO
        ELSE
          CALL DGTEXT(H,V+DVS,TXT,LTXT)
        END IF
      END IF
      END
*DK DO_WINDOW_TXT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++ DO_WINDOW_TXT
CH
      SUBROUTINE DO_WINDOW_TXT(H,V,T)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) T
      DATA DHT/5./,DVT/7./
      IF(T.EQ.TWINDW(IAREDO)) THEN
        CALL DGLEVL(IC_AW)
      ELSE
        CALL DGLEVL(IC_TX)
      END IF
      CALL DGTEXT(H-DHT,V-DVT,T,1)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------- DO_WINDOW_TXT_0
CH
      ENTRY DO_WINDOW_TXT_0(ICTX,ICAW)
CH
CH --------------------------------------------------------------------
CH
      IC_TX=ICTX
      IC_AW=ICAW
      END
*DK DO_PICK
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DO_PICK
CH
      SUBROUTINE DO_PICK(HC,VC,H1,V1,H2,V2,IAR,FOUND)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      LOGICAL FOUND
      IF(FOUND) RETURN
      IF(H1.LT.HC.AND.HC.LT.H2.AND.
     &   V1.LT.VC.AND.VC.LT.V2) THEN
        IAREDO=IAR
        FOUND=.TRUE.
      ELSE
        FOUND=.FALSE.
      END IF
      END
*DK DO_BAR_COL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DO_BAR_COL
CH
      SUBROUTINE DO_BAR_COL(TACT,H1,V)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TACT,TXT,TANSW
      CHARACTER *10 TMAC
      CHARACTER *2 TC(0:15),TCN(0:15)
      DATA TCN/'**','**','**','**','**','**','**','GY','WH','GN',
     &         'YE','BR','RD','MA','CY','BL'/
      DATA TC/'0 ','1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 ',
     &        '10','11','12','13','14','15'/
      CHARACTER *3 TRET
      DATA TRET/':` '/

      DIMENSION LC(0:15)
      DATA LC/0,1,0,0,0,0,0,7,8,9,10,11,12,13,14,15/,L8/8/
      DATA NCS/10/,NCL/16/
      
      DATA DHN/14./,DSN/15./,DV1/2./,DV2/18./,D2/2./,DF/1./,D1/1./
      DATA DHT/20./,DST/21./,HT/0./,VT/3./,DBL/8./
      DATA JDEB/0/

      CALL DPARGV(84,'OCI',2,CI)
      CI=0.01*CI
      IF(TACT(2:2).EQ.'t'.OR.
     &   TACT(2:2).EQ.'T') THEN 
        DH=DHT
        DS=DST
      ELSE
        DH=DHN
        DS=DSN
      END IF

      H=H1-DS
      HP=H1
      DLINDD=D1
      DO L=0,15
        IF(LC(L).NE.0.OR.
     &    TACT(2:2).EQ.'C'.OR.
     &    TACT(2:2).EQ.'T') THEN
          H=H+DS
          CALL DGLEVL(L)
          CALL DQFAR(H,V+DV1,H+DH,V+DV2)
          IF(GRCODD(L).GT.CI) THEN
            CALL DGLEVL(1)
          ELSE
            CALL DGLEVL(8)
          END IF
          CALL DQDRAW(H+DH,V+DV1,H+DH,V+DV2)
          IF(TACT(2:2).EQ.'t'.OR.
     &       TACT(2:2).EQ.'T')
     &       CALL DGTEXT(H+HT,V+VT,TC(L),2)
        END IF
      END DO
      DLINDD=DF
      CALL DGLEVL(L8)
      CALL DQDAR(H1,V+DV1,H+DH,V+DV2)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------- DO_BAR_COL_0
CH
      ENTRY DO_BAR_COL_0(TACT,DHP)
CH
CH --------------------------------------------------------------------
CH
      IF(     TACT(2:2).EQ.'c')THEN
        DHP=NCS*DSN+DBL
      ELSE IF(TACT(2:2).EQ.'C')THEN
        DHP=NCL*DSN+DBL
      ELSE IF(TACT(2:2).EQ.'t')THEN
        DHP=NCS*DST+DBL
      ELSE IF(TACT(2:2).EQ.'T')THEN
        DHP=NCL*DST+DBL
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------- DO_BAR_COL_BY_NAME
CH
      ENTRY DO_BAR_COL_BY_NAME(TACT,TXT,HC,IACT,TANSW)
CH
CH --------------------------------------------------------------------
CH
      H=HP
      DO L=0,15
        IF(LC(L).NE.0) THEN
          H=H+DS
          IF(HC.LE.H) THEN
            IF(L.EQ.1) THEN
              TANSW=TXT
            ELSE
              TANSW=TCN(L)
            END IF
            IACT=4
            RETURN
          END IF
        END IF
      END DO
      IACT=3
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------- DO_BAR_COL_OP
CH
      ENTRY DO_BAR_COL_OP(TACT,TXT,HC,IACT)
CH
CH --------------------------------------------------------------------
CH
      H=HP
      DO L=0,15
        IF(LC(L).NE.0.OR.
     &    TACT(2:2).EQ.'C'.OR.
     &    TACT(2:2).EQ.'T') THEN
          H=H+DS
          IF(HC.LE.H) THEN
            LTXT=LENOCC(TXT)
            IF(TC(L)(2:2).EQ.' ') THEN
              TMAC=TXT(1:LTXT)//TC(L)(1:1)//TRET
            ELSE
              TMAC=TXT(1:LTXT)//TC(L)//TRET
            END IF

c            IF(TC(L)(1:1).EQ.' ') THEN
c              TMAC=TC(L)(2:2)//TXT(1:LTXT)
c            ELSE
c              TMAC=TC(L)     //TXT(1:LTXT)
c            END IF
            CALL DGINMA(TMAC)
            IF(JDEB.EQ.1) THEN
              TXTADW='dginma 2: "'//TMAC//'"'
              CALL DWRC
            END IF
            IACT=1
            RETURN
          END IF
        END IF
      END DO
      IACT=3
      END
*DK DO_BAR_HELP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DO_BAR_HELP
CH
      SUBROUTINE DO_BAR_HELP(ICOL,TXT,TCOM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TXT,TCOM
C      CHARACTER *2 TC(7:15)
C      DATA TC/'Gy','Wh','Gr','Ye','Br','Rd','Ma','Cy','Bl'/

C     IF(ICOL.GE.7.AND.ICOL.LE.15) THEN
C        TXTADW=TC(ICOL)//':'
C      ELSE
C        TXTADW='  :'
C      END IF

      DO L1=1,18
        IF(TXT(L1:L1).NE.' ') GO TO 1
      END DO

    1 TXTADW=TXT(L1:18)

      DO L=4,21
        IF(TXTADW(L:L).EQ.';'.OR.TXTADW(L:L).EQ.'|') TXTADW(L:L)=' '
      END DO

      TXTADW(23:49)=TCOM

      CALL DWRC
      END
*DK DO_BAR_USE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DO_BAR_USE
CH
      SUBROUTINE DO_BAR_USE(TCOM,TDO,NPOS,IGC,NCOL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TCOM,TDO
      DIMENSION IGC(*)
      IF(     TCOM.EQ.'G.') THEN
        IF(IDSTO(4,IAREDO).EQ.0) GO TO 10

C     ELSE IF(TCOM.EQ.'ZO'.OR.TCOM.EQ.'NZ') THEN
C       CALL DPARGV(11,'PBP',2,PBP)
C       IF(PBP.EQ.PICNDO) GO TO 10

      ELSE IF(TCOM.EQ.'do') THEN
        IF(     TDO.EQ.'no') THEN
          NCOL=IGC(1)
          GO TO 9
        ELSE IF(TDO.EQ.'dr') THEN
          DO   M=MPNWDW,0,-1
            IF(NWINDW(M,IAREDO).EQ.-2.OR.NWINDW(M,IAREDO).EQ.1) THEN
              IF(IDSTO(4,M).GE.1) GO TO 9
            END IF
          END DO
          GO TO 10
        ELSE IF(TDO.EQ.'ev') THEN
          IF(TFINDE(1).EQ.' ') GO TO 10
        END IF
      ELSE IF(TCOM.EQ.'RB') THEN

        CALL DPARGV(11,'PBP',2,PBP)
        IF(PBP.EQ.PICNDO) GO TO 10

        CALL DPCIVEC(IAREDO,5,IPIC)
        IF(     TPICDP(IPIC).EQ.'YX') THEN
          CALL DPCGV(IAREDO,11,'PDS',4,P4)
          IF(P4.GT.0.) GO TO 10
          CALL DPCGV(IAREDO,11,'PFF',4,P4)
          IF(P4.GT.0.) GO TO 10

        ELSE IF(TPICDP(IPIC).EQ.'RZ') THEN
          CALL DPCGV(IAREDO,11,'PDS',4,P4)
          IF(P4.GT.0.) GO TO 10

        ELSE IF(TPICDO.EQ.'FR') THEN
          CALL DPCGV(IAREDO,13,'PSK',4,P4)
          IF(P4.GT.0.) GO TO 10
        END IF
      ELSE IF(TCOM.EQ.'++'.OR.TCOM.EQ.'--'.OR.TCOM.EQ.'+-') THEN
        IF(NPOS.EQ.0) GO TO 10
      END IF
    9 RETURN
   10 NCOL=IGC(12)
      END
*DK DO_NEW_WINDOW
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DO_NEW_WINDOW
CH
      SUBROUTINE DO_NEW_WINDOW(TPR,PR,DINCR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TPR,TXT
      DIMENSION PR(4)
      CHARACTER *2 TPIL,TPRL
      CHARACTER *5 DT5
      CHARACTER *6 DT6
      CHARACTER *7 DT7
      CHARACTER *8 DT8
      CHARACTER *20 TNAM

      DIMENSION HT(8),VT(8)
      DATA HT/194., 2.,  2.,180.,  2.,2.,180.,110./
      DATA VT/ 74.,66., 40.,  0., 18.,3., 18., 18./
      
      DATA NEWS/2/

      CHARACTER *9 TITLE
      CHARACTER *4 TNAME
      DATA TITLE/'Parameter'/,TNAME/'Par.'/
      DATA NLET/42/,DLET/8./,DSP/8./,L1/1/,L8/8/,L9/9/,L14/14/,L10/10/
      DATA ISIZ/5/,LSIZ/4/

      DATA H1/220./,V1/90./,HTXT/2./

      DATA NEW/0/,NOLD/0/,IDALI/0/,IHLP/2/,JHL/0/

      CALL DPARGI_24(84,'OH2',IH2,IH4)
      IF(IH4.LT.0) THEN
        IF(NEW.GT.0) THEN
          CALL DGDWIN(NEW,KEW)
          NEW=0
        END IF
        RETURN
      END IF
      CALL DGQINF(MODW,IWIN)
      IF(NEW.LE.0) THEN
        IF(FMACDM) RETURN
        NEW=NEWS
        CALL DPARGI(84,'OV2',IV2)
        CALL DGCWIN(1.,1.,H1,V1,IH2,IV2,NEW,TITLE,TNAME)
      ELSE
        CALL DGSWIN(NEW,KEW,0)
      END IF

      CALL DO_BAR_PS_1
      CALL DGLEVL(L1)
      CALL DQFAR(0.,0.,H1,V1)

      LON=L8
      IF(TPIL.NE.TNAMDO) THEN
        TPIL=TNAMDO
        CALL DO_BAR_NAME(TNAM)
        TPRL=' '
        PRL4=-999.
      END IF

      CALL DGLEVL(L8)
      CALL DGTEXT(HT(1),VT(1),TNAMDO,2)
      
      IF(TNAM.EQ.' ') TNAM=TNAMDO
      L=LENOCC(TNAM)
      CALL DGTXTF(L9,ISIZ,TNAM,0.,HT(2),VT(2),L)

      IF(TPR.EQ.' ') GO TO 9

      IF(TPRL.NE.TPR) THEN
        TPRL=TPR
        PRL4=-PR(4)
        PRL2=PR(2)
      END IF

      TXTADW=TPR//' = '//DT8(PR(2))

      IF(PR(4).NE.0.) THEN
        IF(PR(4).GT.0.) THEN
          TXTADW(4:4)=':'
          CALL DGTXTF(L8,ISIZ,'On' ,0.,HT(4),VT(3),2)
        ELSE
          TXTADW(4:4)='/'
          CALL DGTXTF(L8,ISIZ,'Off',0.,HT(4),VT(3),3)
        END IF
      END IF
      CALL DGTXTF(L8,ISIZ,TXTADW,0.,HT(3),VT(3),13)

      CALL DGLEVL(L14)
      TXTADW='last='//DT8(PRL2)

      IF(     PRL4.GT.0.) THEN
        TXTADW(5:5)=':'
      ELSE IF(PRL4.LT.0.) THEN
        TXTADW(5:5)='/'
      END IF
      CALL DGTEXT(HT(5),VT(5),TXTADW,13)

      TXTADW='Min='//DT6(PR(1))//'  Max='//DT7(PR(3))
      CALL DGTEXT(HT(6),VT(6),TXTADW,23)

      IF(     PRL4.GT.0.) THEN
        CALL DGTXTF(L10,LSIZ,'On',0.,HT(7),VT(7),2)
      ELSE IF(PRL4.LT.0.) THEN
        CALL DGTXTF(L10,LSIZ,'Off',0.,HT(7),VT(7),3)
      END IF

      IF(DINCR.NE.0.) THEN
        TXTADW='±'//DT6(DINCR)
        CALL DGTEXT(HT(8),VT(8),TXTADW,7)
      END IF

C   9 IF(IDEB.EQ.1) CALL DGCHKX
    9 CALL DGSWIN(NOLD,KEW,0)

      IF(MODW.EQ.2) CALL DGPOP('HPOP')

      CALL DO_BAR_PS_2
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------------- DO_NEW_WINDOW_TEXT_0
CH
      ENTRY DO_NEW_WINDOW_TEXT_0
CH
CH --------------------------------------------------------------------
CH
      IF(NEW.GT.0) THEN
        CALL DGSWIN(NEW,KEW,0)
        CALL DO_BAR_PS_1
        CALL DGLEVL(L1)
        CALL DQFAR(0.,0.,H1,V1)
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------------- DO_NEW_WINDOW_TEXT
CH
      ENTRY DO_NEW_WINDOW_TEXT(VTXT,ICTXT,ISTXT,TXT)
CH
CH --------------------------------------------------------------------
CH
      IF(NEW.GT.0) THEN
        L=LENOCC(TXT)
        CALL DGTXTF(ICTXT,ISTXT,TXT,0.,HTXT,VTXT,L)
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------------- DO_NEW_WINDOW_TEXT_1
CH
      ENTRY DO_NEW_WINDOW_TEXT_1
CH
CH --------------------------------------------------------------------
CH
      CALL DGSWIN(NOLD,KEW,0)
      IF(MODW.EQ.2) CALL DGPOP('HPOP')
      CALL DO_BAR_PS_2
      END

*DK DO_BAR_PS_1
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DO_BAR_PS_1
CH
      SUBROUTINE DO_BAR_PS_1
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_UIS.INC'
      LOGICAL FRES
      DATA FRES/.FALSE./
      IF(FPSWDU) THEN
        FRES=.TRUE.
        FPSWDU=.FALSE.
        CALL DGSPME
      ELSE
        FRES=.FALSE.
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------- DO_BAR_PS_2
CH
      ENTRY DO_BAR_PS_2
CH
CH --------------------------------------------------------------------
CH
      IF(FRES) THEN
        FRES=.FALSE.
        FPSWDU=.TRUE.
        CALL DGRSME
      END IF
      END
*DK DO_INDEX
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DO_INDEX
CH
      SUBROUTINE DO_INDEX
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      LOGICAL FSC
      CALL DO_BAR_PS_1
      FSC=.FALSE.
    1 CALL DTYANS('L=local, F=full search, P=store page <CR>=return',
     &  'LFP',NSC)
      IF(     NSC.EQ.1) THEN
        FSC=.FALSE.
      ELSE IF(NSC.EQ.2) THEN
        FSC=.TRUE.
      ELSE IF(NSC.EQ.3) THEN
        CALL DQHL_STORE_PAGE
        RETURN
      ELSE IF(NSC.EQ.0) THEN
        CALL DQH_NEW_WINDOW_OF
        CALL DWRT('Search stopped.')
        RETURN
      ELSE
        CALL DWRT('Wrong command.#')
        GO TO 1
      END IF
    2 CALL DTYANS('C=scan command, T=scan text   else RETURN','CT',NSC)
      IF(     NSC.EQ.1) THEN
        CALL DT_WRT('<CR>=return. Type 2 letter command ')
        CALL DQHL_SCAN_COMMAND(FSC)
      ELSE IF(NSC.EQ.2) THEN
        CALL DT_WRT('<CR>=return. Type textstring ')
        CALL DQHL_SCAN_TEXT(FSC)
      ELSE IF(NSC.EQ.0) THEN
        CALL DQH_NEW_WINDOW_OF
        CALL DWRT('Search stopped.')
        RETURN
      ELSE
        CALL DWRT('Wrong command.#')
        GO TO 2
      END IF
      CALL DWRT('Type "GG:" and the page name "xx\x"')
      END
