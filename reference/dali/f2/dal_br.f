*DK DBR1
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBR0
CH
      SUBROUTINE DBR0
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    : from common : TPICDO = prcessor name
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C INVOKED BY TANSW.EQ.'GO'   (DBR1)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TLPR
      CHARACTER *15 TRN,TDN,TRB,TDB,TRW
      CHARACTER *25 TMD
      CHARACTER *40 TSD,TSF,TSB,TSG
      DATA TRN/'(RN:GB:T) :'/
      DATA TDN/'(DN:GB:T) :'/
      DATA TRB/'(RB:GB:T) :'/
      DATA TDB/'(DB:GB:T) :'/
      DATA TRW/'(RW:DO:GB:T) :'/
      DATA TMD/'(TD:GB:T) :'/
      DATA TSD/         '(M0:OP:SA:CF:DC:M1:GB:T) :'/
      DATA TSF/      '(PP:M0:OP:SA:CF:DC:M1:GB:T) :'/
      DATA TSB/'(FG=OF:BW:M0:OP:SA:CF:DC:M1:GB:T) :'/
      DATA TSG/'(FG=ON:BW:M0:OP:SA:CF:DC:M1:GB:T) :'/
C               123456789 123456789 123456789 123456789
      CHARACTER *2 T1,T2,TIN,TPIC,TGT,TA
      DATA T1/'YX'/,T2/'RZ'/
      LOGICAL F_DBRSET
      FSPIDB=.TRUE.
      TPICDO='YX'
      GO TO 1
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DBR1
CH
      ENTRY DBR1
CH
CH --------------------------------------------------------------------
CH
C     The menue is called via tpicdo see below or direct via TAN1DH:
C     ................. TAN1DH='LM' in P_DALIC
C     ................. TAN1DH='LD'
C     ................. TAN1DH='LL'
C     ................. TAN1DH='LS'
C     ................. TAN1DH='LF'
C     ................. TAN1DH='SB'
C     ................. TAN1DH='LA'
C     ................. TAN1DH='LV'
C     ................. TAN1DH='LI'
C     ................. TAN1DH='LN'
C     ................. TAN1DH='LP'
C     ................. TAN1DH='LC'
C     ................. TAN1DH='LE'
C     ................. TAN1DH='LU'
C     ................. TAN1DH='LH'
C     ................. TAN1DH='LR'
C     ................. TAN1DH='FR'
C     ................. TAN1DH='ED' in P_DALIJ
C     ................. TAN1DH='PI' in P_DALIP
C     =========================================================== start
  800 CALL DWRT_SETUP('TERMINAL=OFF')
      CALL DGETM0('ST ',1)
C     ........... Terminal switched on in "GT:PU:S?"
      TPICDO='GT'
CCC      TLAST='YX'
C     =========================================================== G...
C
  900 TIN=TPICDO
      IF(TPICDO.EQ.'GG'.OR.TPICDO.EQ.'GT') THEN
C       ................................................ GT, GG
        TGT=TPICDO
        CALL DGT
        IF(TPICDO.EQ.'GB') THEN
          TPICDO=T1
          CALL DBR_GB
        END IF
        GO TO 900
C
      ELSE IF(TPICDO.EQ.'GB') THEN
C       ................................................ GB
        TPICDO=T2
        GO TO 900
      END IF
C
C     =========================================================== PROJECTIONS
C
C     ....... a negative number means that also XY is accepted ...
    1 IF(F_DBRSET('XY',1).OR.F_DBRSET('YX',1)) THEN
C       ......................................................... YX XY
        TPICDO='YX'
        THLPDO=TPICDO
        TNAMDO=TPICDO
        CALL DB_JUMP_TO_COMMAND_LIST
        CALL DKYX
        GO TO 910
      END IF
C
      IF(F_DBRSET('ZY',2).OR.F_DBRSET('YZ',2)) THEN
C       ......................................................... YZ ZY
        TPICDO='YZ'
        THLPDO=TPICDO
        TNAMDO=TPICDO
        CALL DKYZ
        GO TO 910
      END IF
C
      IF(F_DBRSET('ZR',3).OR.F_DBRSET('RZ',3)) THEN
C       ......................................................... RZ ZR
        TPICDO='RZ'
        THLPDO=TPICDO
        TNAMDO=TPICDO
        CALL DB_JUMP_TO_COMMAND_LIST
        CALL DKRZ
        GO TO 910
      END IF
C
      IF(F_DBRSET('TF',4).OR.F_DBRSET('FT',4)) THEN
C       ......................................................... FT TF
        TPICDO='FT'
        THLPDO=TPICDO
        TNAMDO=TPICDO
        CALL DB_JUMP_TO_COMMAND_LIST
        CALL DKFT
        GO TO 910
      END IF
C
      IF(F_DBRSET('ZF',5).OR.F_DBRSET('FZ',5)) THEN
C       ......................................................... FZ ZF
        TPICDO='FZ'
        THLPDO=TPICDO
        TNAMDO=TPICDO
        CALL DB_JUMP_TO_COMMAND_LIST
        CALL DKFZ
        GO TO 910
      END IF
C
      IF(F_DBRSET('RF',6).OR.F_DBRSET('FR',6)) THEN
C       ......................................................... FR RF
        TPICDO='FR'
        THLPDO=TPICDO
        TNAMDO=TPICDO
        CALL DKFR
        GO TO 910
      END IF
C
      IF(F_DBRSET('RO',7)) THEN
C       ....................................................... RO 3D rotation
        CALL DKRO
        GO TO 910
      END IF
C
      IF(F_DBRSET('RS',10)) THEN
C       ......................................................... RS residuals
        CALL DKRS
        GO TO 910
      END IF
C
      IF(F_DBRSET('HF',11)) THEN
C       ...................................................... HF histogram FI
        CALL DKHF
        GO TO 910
      END IF
C
      IF(F_DBRSET('TD',12)) THEN
C       .................................................... TD track distance
        CALL DKTD
        GO TO 910
      END IF
C
      IF(F_DBRSET('EC',13)) THEN
C       ........................................................ EC endcaps
        CALL DKEC
        GO TO 910
      END IF
C
      IF(F_DBRSET('HZ',14)) THEN
C       ...................................................... HZ histogram RZ
        CALL DKHZ
        GO TO 910
      END IF
C
      IF(F_DBRSET('LT',15)) THEN
C       ...................................................... LT laser tracks
        CALL DKLT
        GO TO 910
      END IF
C
      IF(F_DBRSET('DL',16)) THEN
C       ....................................................... DL display list
        CALL DKDL
        GO TO 910
      END IF
C
      IF(F_DBRSET('GP',17)) THEN
C       ................................................. AP all projections
        CALL DKAP
        GO TO 910
      END IF
C
      IF(F_DBRSET('SC',18)) THEN
C       ................................................. SC  Sical
        CALL DKSC
        GO TO 910
      END IF
C
      IF(F_DBRSET('EF',19)) THEN
C       .............................................. EF energy flow
        CALL DKEF
        GO TO 910
      END IF
C
C      IF(F_DBRSET('PH',20)) THEN
CC       .............................................. IP interactive physics
C        CALL DIP
C        GO TO 910
C      END IF
C
      IF(F_DBRSET('LK',21)) THEN
C       ................................................. LK look
        CALL DLK
        GO TO 910
      END IF
C
      IF(F_DBRSET('BC',22)) THEN
C       ................................................. BC BCAL
        CALL DKBC
        GO TO 910
      END IF
C
      IF(F_DBRSET('AL',23)) THEN
C       ..................................... AL
        CALL DB_JUMP_TO_COMMAND_LIST
        CALL DAC
        GO TO 910
      END IF
C
      IF(F_DBRSET('AP',24)) THEN
C       ..................................... AP
        CALL DB_JUMP_TO_COMMAND_LIST
        CALL DAD
        GO TO 910
      END IF
C
      IF(F_DBRSET('TC',26)) THEN
C       ..................................... CT color tracks individually
        CALL DVX
        GO TO 910
      END IF
C
      IF(F_DBRSET('CS',28)) THEN
C       .......................................... color sequence
        CALL D_DEF_PAR(5,'color sequence')
        GO TO 910
      END IF
C
      IF(F_DBRSET('CC',29)) THEN
C       .......................................... constant data colors
        CALL D_DEF_PAR(3,'constant data colors')
        GO TO 910
      END IF
C
      IF(F_DBRSET('SY',30)) THEN
C       .......................................... SY symbols
        CALL D_DEF_PAR(1,'data symbols')
        GO TO 910
      END IF
C
      IF(F_DBRSET('CF',31)) THEN
C       .......................................... CO color function
        CALL D_DEF_PAR(2,'data color functions')
        GO TO 910
      END IF
C
      IF(F_DBRSET('CD',32)) THEN
C       .......................................... CD color definition
        CALL DCD
        GO TO 910
      END IF
C
      IF(F_DBRSET('DD',33)) THEN
C       .......................................... DD displayed data
        CALL DB_JUMP_TO_COMMAND_LIST
        CALL DHT
        GO TO 910
      END IF
C
      IF(F_DBRSET('PM',34)) THEN
C       .......................................... PM parameter change
        CALL DUPCHG
        GO TO 910
      END IF
C
      IF(F_DBRSET('SU',35)) THEN
C       .......................................... SU  set up workstation
        CALL DSU
        GO TO 910
      END IF
C
      IF(F_DBRSET('MA',36)) THEN
C       .......................................... MA macros
        CALL DGETMR
        GO TO 920
      END IF
C
      IF(F_DBRSET('WC',37)) THEN
C       .......................................... WC workstation colors
        CALL DWC
        GO TO 910
      END IF
C
      IF(F_DBRSET('RP',38)) THEN
C       .......................................... RP rotate picture ??
        CALL DMW
        GO TO 910
      END IF
C
      IF(F_DBRSET('TE',39)) THEN
C       .......................................... TE track extrapolation
        CALL DTE
        GO TO 910
      END IF
C
      IF(F_DBRSET('MD',40)) THEN
C       .......................................... MD modify display size
        CALL DGINMA(TMD)
        CALL DWS
        CALL DDISPA
        GO TO 930
      END IF
C
      IF(F_DBRSET('SW',40)) THEN
C       .......................................... SW setup windows
        CALL DWS
        GO TO 910
      END IF
C
      IF(F_DBRSET('VX',41)) THEN
C       .......................................... VX secondary vertices
        CALL DVX
        GO TO 910
      END IF
C
      IF(F_DBRSET('TX',42)) THEN
C       .......................................... TX set text
        CALL DTX
        GO TO 910
      END IF
C
      IF(F_DBRSET('BT',44)) THEN
C       .......................................... BT brain test
        CALL DBT
        GO TO 910
      END IF
C
      IF(F_DBRSET('AE',46)) THEN
C       .......................................... AE add events
        CALL DAE
        GO TO 910
      END IF
C
      IF(F_DBRSET('MS',47)) THEN
C       .......................................... MS message
        CALL DMESS
        GO TO 910
      END IF
C
      IF(F_DBRSET('SI',48)) THEN
C       .......................................... SI simulation
        CALL DSIM
        GO TO 910
      END IF
C
      IF(F_DBRSET('IT',49)) THEN
C       .......................................... IT interactive tracking
        CALL US9
        GO TO 930
      END IF
C
      IF(F_DBRSET('U0',50)) THEN
C       .......................................... U0
        CALL US0
        GO TO 920
      END IF
C
      IF(F_DBRSET('U1',51)) THEN
C       .......................................... U1
        CALL US1
        GO TO 920
      END IF
C
      IF(F_DBRSET('U2',52)) THEN
C       .......................................... U2
        CALL US2
        GO TO 920
      END IF
C
      IF(F_DBRSET('U3',53)) THEN
C       .......................................... U3
        CALL US3
        GO TO 920
      END IF
C
      IF(F_DBRSET('U4',54)) THEN
C       .......................................... U4
        CALL US4
        GO TO 920
      END IF
C
      IF(F_DBRSET('U5',55)) THEN
C       .......................................... U5
        CALL US5
        GO TO 920
      END IF
C
      IF(F_DBRSET('U6',56)) THEN
C       .......................................... U6
        CALL US6
        GO TO 920
      END IF
C
      IF(F_DBRSET('U7',57)) THEN
C       .......................................... U7
        CALL US7
        GO TO 920
      END IF
C
      IF(F_DBRSET('U8',58)) THEN
C       .......................................... U8
        CALL US8
        GO TO 920
      END IF
C
      IF(F_DBRSET('U9',59)) THEN
C       .......................................... U9
        CALL US9
        GO TO 920
      END IF
C
      IF(F_DBRSET('NE',60)) THEN
C       .......................................... NE
        CALL DNEW
        GO TO 920
      END IF
C
      IF(F_DBRSET('MM',61)) THEN
C       .......................................... MM moving markers
        CALL DM3D
        GO TO 930
      END IF
C
      IF(F_DBRSET('CL',62)) THEN
C       .......................................... CL clear
        CALL DCLD
        GO TO 930
      END IF
C
      IF(F_DBRSET('SP',63)) THEN
C       ............................................. SPAWN
        CALL DSPAWN
        GO TO 930
      END IF
C
      IF(F_DBRSET('BW',64)) THEN
C       .......................................... BW black and white
        CALL DCDBWT
        GO TO 930
      END IF
C
      IF(F_DBRSET('DC',65).OR.
     &   F_DBRSET('XC',65).OR.
     &   F_DBRSET('PC',65)) THEN
C       .......................................... PC DC print/display colors
        TPIC=TPICDO
        CALL DCDPC(TPIC)
        GO TO 930
      END IF
C
      IF(F_DBRSET('TA',66)) THEN
C       ........................................ TA picture set to wire frame
        CALL DCDTWF
        GO TO 930
      END IF
C
      IF(F_DBRSET('CW',67)) THEN
C       .......................................... CW clear workstation
        CALL DTYANS(
     &    'Do you want to clear the workstation? <cr>=no,Yes',
     &    'Y',NANSW)
        IF(NANSW.NE.1) GO TO 930
        CALL DGCLWK
        GO TO 930
      END IF
C
      IF(F_DBRSET('UL',68)) THEN
C       .......................................... UL set user level
        CALL DUL
        GO TO 930
      END IF
C
      IF(F_DBRSET('RH',69)) THEN
C       .......................................... RH Read help file
        CALL DQHL0
        GO TO 930
      END IF
C
      IF(F_DBRSET('LF',70)) THEN
C       .......................................... LF output log file
        CALL DGETCD(NDUM1,NDUM2,NDUM3)
        GO TO 930
      END IF
C
      IF(F_DBRSET('R0',71)) THEN
C       .......................................... R0 read DALI_??.PAR_D0
        CALL DCLD
        CALL DPARST(TFILDC//'PAR_D0')
        GO TO 930
      END IF
C
      IF(F_DBRSET('R1',72).OR.
     &   F_DBRSET('R2',72).OR.
     &   F_DBRSET('R3',72).OR.
     &   F_DBRSET('R4',72).OR.
     &   F_DBRSET('R5',72).OR.
     &   F_DBRSET('R6',72).OR.
     &   F_DBRSET('R7',72).OR.
     &   F_DBRSET('R8',72).OR.
     &   F_DBRSET('R9',72)) THEN
C       .......................................... R# read ....._??.PAR_D#
        CALL DCLD
        CALL DPARST_MODIF(TFILDC//'PAR_D'//TPICDO(2:2))
        GO TO 930
      END IF
C
      IF(F_DBRSET('HD',73)) THEN
C       .......................................... read bar file
        CALL DW_READ_PLATFORM_TEXT
        CALL DO_BAR(1)
        CALL DQ_HEADER
        GO TO 930
      END IF
C
      IF(F_DBRSET('EV',74)) THEN
C       .......................................... EV read event
        CALL DJEV
        GO TO 910
      END IF
C
      IF(F_DBRSET('FI',75)) THEN
C       .......................................... FI input file
        CALL DQHLP('FI ')
        IF(FMACDM) CALL DQHLP(']]')
        CALL DJFIL
        IF(FMACDM) CALL DQHLP('[[')
        GO TO 920
      END IF
C
      IF(F_DBRSET('RN',76)) THEN
C       .......................................... RN read next event
        CALL DGINMA(TRN)
        CALL DJEV
        GO TO 930
      END IF
C
      IF(F_DBRSET('RB',77)) THEN
C       .......................................... RB read backwrds
        CALL DGINMA(TRB)
        CALL DJEV
        GO TO 930
      END IF
C
      IF(F_DBRSET('DN',78)) THEN
C       .......................................... DN display next
        CALL DGINMA(TDN)
        CALL DJEV
        GO TO 930
      END IF
C
      IF(F_DBRSET('DB',79)) THEN
C       .......................................... DB display bachwards
        CALL DGINMA(TDB)
        CALL DJEV
        GO TO 930
      END IF
C
      IF(F_DBRSET('RW',80)) THEN
C       .......................................... RW rewind
        CALL DGINMA(TRW)
        CALL DJEV
        GO TO 930
      END IF
C
      IF(F_DBRSET('PS',81)) THEN
C       .......................................... PS interactive
        CALL DMPS(' ',' ',' ')
        GO TO 910
      END IF
C
      IF(F_DBRSET('SF',82)) THEN
C       .......................................... SF print colors
        CALL DGINMA(TSF)
        CALL DMPS('C','Y','Y')
        GO TO 930
      END IF
C
      IF(F_DBRSET('SB',83)) THEN
C       .......................................... SB black/white
        CALL DGINMA(TSB)
        CALL DMPS('B','Y','Y')
        GO TO 930
      END IF
C
      IF(F_DBRSET('SG',84)) THEN
C       .......................................... SG gray
        CALL DGINMA(TSG)
        CALL DMPS('G','Y','Y')
        GO TO 930
      END IF
C
      IF(F_DBRSET('SD',85)) THEN
C       .......................................... SD display
        CALL DGINMA(TSD)
        CALL DMPS('C','Y','Y')
        GO TO 930
      END IF
C
      IF(F_DBRSET('PU',86)) THEN
C       .......................................... log
        CALL DB_PR_USE_OP
        GO TO 910
      END IF
C
      IF(F_DBRSET('DW',87)) THEN
C       ............................. clear workstation and redraw all
        CALL DDISPA
        GO TO 930
      END IF
C
      IF(F_DBRSET('WZ',88)) THEN
C       ............................. Wizard
        CALL DWIZ
        GO TO 910
      END IF
C
      IF(F_DBRSET('PP',89)) THEN
C       .......................................... CO color function
        CALL D_DEF_PAR(4,'other parameters')
        GO TO 910
      END IF
C
      IF(F_DBRSET('CJ',90)) THEN
C       .......................................... Check J_parameters
        CALL DCJ
        GO TO 930
      END IF
C
      IF(F_DBRSET('IN',91)) THEN
C       .......................................... Introduction
        CALL DIN
        GO TO 910
      END IF
C
      IF(F_DBRSET('PA',92)) THEN
C       .......................................... Introduction
        CALL DPA
        GO TO 910
      END IF
C
      IF(F_DBRSET('PH',96)) THEN
C       .......................................... print help.file
        CALL DPH
        GO TO 930
      END IF
C
      IF(F_DBRSET('QU',119).OR.F_DBRSET('EX',119)) THEN
C       .......................................... QU
        CALL DTYANS('Do you want to quit ? Y = yes','YI',NANSW)
        IF(NANSW.EQ.1) THEN
          CALL DBRLOG('WRITE')
          RETURN
        END IF
        IF(NANSW.EQ.2) THEN
          CALL DGETLN(TA,NA,1)
          IF(NA.EQ.1.AND.TA.EQ.'T') THEN
            CALL DBRLOG('WRITE')
            RETURN
          END IF
        END IF
        TPICDO=T1
        CALL DBR_GB
        GO TO 900
      END IF
C
      IF(.NOT.FSPIDB) THEN
        CALL DWRT('Wrong processor '//TPICDO//'#')
        TPICDO='GT'
        GO TO 900
      END IF
C
      FSPIDB=.FALSE.
      RETURN
C     .................................. interactive processors
  910 IF(TIN.NE.T1) THEN
        T2=T1
        T1=TIN
      END IF
      GO TO 990
C     .................................. unknown processors
  920 IF(FSPIDB.OR.TPICDO.EQ.TIN) THEN
        TPICDO=T1
        CALL DBR_GB
      ELSE IF(TIN.NE.T1) THEN
        T2=T1
        T1=TIN
      END IF
      GO TO 990
C     .................................. NON interactive processors
  930 IF(TGT.EQ.'GG') THEN
        TPICDO=T1
      ELSE
        TPICDO='GT'
      END IF
      CALL DBR_GB
  990 GO TO 900
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DBR_GB
CH
      ENTRY DBR_LAST_PROCESSOR(TLPR)
CH
CH --------------------------------------------------------------------
CH
      IF(TIN.NE.T1) THEN
        TLPR=T1
      ELSE
        TLPR=T2
      END IF
      END
*DK DBR2
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBR2
CH
      SUBROUTINE DBR2
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    : from common : IPICDO = prcessor number
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
C     IF(FPIKDP) CALL DPICK_TR_EF_1
      PICNDO=IPICDO
      TPICDO=TPICDP(IPICDO)
      IF(TPICDO.EQ.'YX') THEN
C       ......................................................... YX
        CALL DYXD
      ELSE IF(TPICDO.EQ.'YZ') THEN
C       ......................................................... YZ
        CALL DYZD
      ELSE IF(TPICDO.EQ.'RZ') THEN
C       ......................................................... RZ
        CALL DRZD
      ELSE IF(TPICDO.EQ.'FT') THEN
C       ......................................................... FT
        CALL DFTD
      ELSE IF(TPICDO.EQ.'FZ') THEN
C       ......................................................... FZ
        CALL DFZD
      ELSE IF(TPICDO.EQ.'FR') THEN
C       ......................................................... FR
        CALL DFRD
      ELSE IF(TPICDO.EQ.'RS') THEN
C       ......................................................... RS residuals
        CALL DRSD
      ELSE IF(TPICDO.EQ.'RO') THEN
C       ....................................................... RO 3D rotation
        CALL DROD
      ELSE IF(TPICDO.EQ.'HF') THEN
C       ...................................................... HF histogram FI
        CALL DHFD
      ELSE IF(TPICDO.EQ.'TD') THEN
C       .................................................... TD track distance
        CALL DTDD
      ELSE IF(TPICDO.EQ.'VD') THEN
C       .................................................... VD VDET
        CALL DFZVDD
      ELSE IF(TPICDO.EQ.'EC') THEN
C       ........................................................ EC endcaps
        CALL DECD
      ELSE IF(TPICDO.EQ.'HZ') THEN
C       ...................................................... HZ histogram RZ
        CALL DHZD
      ELSE IF(TPICDO.EQ.'LT') THEN
C       ...................................................... LT laser tracks
        CALL DLTD
      ELSE IF(TPICDO.EQ.'DL') THEN
C       ....................................................... DL display list
        CALL DDLD
      ELSE IF(TPICDO.EQ.'AP') THEN
C       ..................................................... AD ALPHA DISPLAY
        CALL DADD(.TRUE.)
      ELSE IF(TPICDO.EQ.'BC') THEN
C       ..................................................... BC BCAL
        CALL DBCD
      ELSE IF(TPICDO.EQ.'AL') THEN
C       ..................................................... AC ALPHA CALC
        CALL DACD(IDUM,.FALSE.)
      ELSE IF(TPICDO.EQ.'GP') THEN
C       ................................................. AP all projections
        CALL DAPD
      ELSE IF(TPICDO.EQ.'SC') THEN
C       ................................................. SC  Sical
        CALL DSCD
      ELSE IF(TPICDO.EQ.'LK') THEN
C       ................................................. LK look
        CALL DLKD
      ELSE IF(TPICDO.EQ.'EF') THEN
C       .............................................. EF energy flow
        CALL DEFD
C
C     =========================================================== USER
C
      ELSE IF(TPICDO.EQ.'U0') THEN
C       .......................................... U0
        CALL US0D
C
      ELSE IF(TPICDO.EQ.'U1') THEN
C       .......................................... U1
        CALL US1D
C
      ELSE IF(TPICDO.EQ.'U2') THEN
C       .......................................... U2
        CALL US2D
C
      ELSE IF(TPICDO.EQ.'U3') THEN
C       .......................................... U3
        CALL US3D
C
      ELSE IF(TPICDO.EQ.'U4') THEN
C       .......................................... U4
        CALL US4D
C
      ELSE IF(TPICDO.EQ.'U5') THEN
C       .......................................... U5
        CALL US5D
C
      ELSE IF(TPICDO.EQ.'U6') THEN
C       .......................................... U6
        CALL US6D
C
      ELSE IF(TPICDO.EQ.'U7') THEN
C       .......................................... U7
        CALL US7D
C
      ELSE IF(TPICDO.EQ.'U8') THEN
C       .......................................... U8
        CALL US8D
C
      ELSE IF(TPICDO.EQ.'U9') THEN
C       .......................................... U9
        CALL US9D
C
      ELSE IF(TPICDO.EQ.'NE') THEN
C       .......................................... NE
        CALL DNEWD
      END IF
C     IF(FPIKDP) CALL DPICK_TR_EF_2
      END
*DK DBR3
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBR3
CH
      SUBROUTINE DBR3(H,V,FIN)
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
      IF(TPICDO.EQ.'YX'.OR.TPICDO.EQ.'XY') THEN
         CALL DYXMO(IAREDO,FPOSDT,H,V,FIN)
      ELSE IF(TPICDO.EQ.'FT'.OR.TPICDO.EQ.'TF') THEN
         CALL DFTMO(IAREDO,FPOSDT,TPOSDT,H,V,FIN)
      ELSE IF(TPICDO.EQ.'RZ'.OR.TPICDO.EQ.'ZR'
     &     .OR.TPICDO.EQ.'HZ') THEN
         CALL DRZMO(IAREDO,TPOSDT,H,V,FIN)
      ELSE IF(TPICDO.EQ.'FZ'.OR.TPICDO.EQ.'ZF'.OR.
     &     TPICDO.EQ.'FR'.OR.TPICDO.EQ.'RF') THEN
         CALL DFZMO(IAREDO,FPOSDT,H,V,FIN)
      END IF
      END
*DK F_DBRSET
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ F_DBRSET
CH
      LOGICAL FUNCTION F_DBRSET(T,IPIC)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :T=Name of processor ,ipic=number of processor
C               Set up TPICDP if fspidb=.true => F_DBRSET=.FALSE.
C               If fspidb=.false. and T=name of processor => F_DBRSET=.TRUE.
C ---------------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) T
      IF(FSPIDB) THEN
        TPICDP(IPIC)=T(1:2)
        F_DBRSET=.FALSE.
      ELSE IF(T.EQ.TPICDO) THEN
        F_DBRSET=.TRUE.
        CALL DBRSET(IPIC)
        TPICDO=T
        THLPDO=TPICDO
        TNAMDO=TPICDO
      ELSE
        F_DBRSET=.FALSE.
      END IF
      END
*DK DBRSET
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DBRSET
CH
      SUBROUTINE DBRSET(IPIC)
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
      CHARACTER *(*) TMODE
      CHARACTER *2 TANSW
      CHARACTER *1 T1(0:7,0:14),TT1(119)
      EQUIVALENCE (T1(1,0),TT1)
      DATA T1/120*'_'/
      DIMENSION LOG1(0:119)
      DIMENSION LOG2(0:7,0:14)
      EQUIVALENCE (LOG1,LOG2)
      DATA LOG1/120*0/
      CHARACTER *2 TPIC(0:7,0:14),TP(0:7)
      CHARACTER *9 TIM1,TIM2,TIS0,TIS1,TINEW
      DATA TIM1,TIM2,TINEW/' ',' ',' '/
      DATA TIS0/'    99999'/
      DATA TIS1/'    99998'/
      EQUIVALENCE (TPIC,TPICDP(0))
      CHARACTER *37 TYA
      DATA TYA/'               Default="_"  Check="*"'/
      CHARACTER *49 TOPER
      DATA TOPER/'P?: List processor use'/
      CHARACTER *16 TFIL
C                123456789 123456
      DATA TFIL/'DALI.STAT_NOV_88'/
      DATA N5/5/,I5/5/
      LOGICAL FLOG,FF
      DATA FLOG/.TRUE./
      IPICDO=IPIC
      PICNDO=IPIC
C     TPICDO=T
      IF(FLOG) THEN
        IP=MIN(IPIC,119)
        IPOLD=IP
        LOG1(IP)=MIN(LOG1(IP)+1,99999)
        IF(TT1(IP).EQ.'.') TT1(IP)='_'
      END IF
      FLOG=.TRUE.
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DBR_GB
CH
      ENTRY DBR_GB
CH
CH --------------------------------------------------------------------
CH
      FLOG=.FALSE.
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------  DB_PR_USE_OP
CH
      ENTRY DB_PR_USE_OP
CH
CH -------------------------------------------------------------------
CH
  930 CALL DTYPT('TYPE',TPICDO, 0    ,  0   ,  0   ,  ' ' ,TOPER)
  936 CALL DOPER(1,1,
     &  1,0,' ',0,
     &  1,0,' ',0,
     &  NEXEC,CHG,TANSW)
      GO TO (910,920,930,940),NEXEC
  910 RETURN
  920 IF(TANSW.EQ.'S?') then
        CALL DWRT_SETUP('TERMINAL=ON')
        CALL DSETUP_TIM(ISEC1)
        WRITE(TIS1,1007) ISEC1
        IF(TIS0.NE.'    99999') THEN
          CALL DWRT(TIS1//' seconds. Minimum = '//TIS0)
        ELSE
          CALL DWRT('Start DALI.STAT .')
        END IF
        READ(TIS0,1007) ISEC0
        IF(ISEC0.GT.0) ISEC1=MIN(ISEC0,ISEC1)
        WRITE(TIS0,1007) ISEC1
 1007   FORMAT(I9)
        GO TO 936
      END IF
      K=0
      DO I=0,14
        DO N=0,7
          IF(TPIC(N,I).EQ.TANSW) THEN
            WRITE(TYA(1:14),2000) LOG2(N,I),T1(N,I),TANSW,K
 2000       FORMAT(I6,2A,' = ',I2)
            CALL DTYANS(TYA,'DC',Nansw)
            IF(NANSW.LE.0) GO TO 930
            IF(NANSW.EQ.1) THEN
              T1(N,I)='_'
            ELSE
              T1(N,I)='*'
            END IF
            TYA(7:7)=T1(N,I)
            CALL DWRT(TYA(1:14))
            GO TO 930
          END IF
          K=K+1
        END DO
      END DO
      IF(TANSW.EQ.'N?') THEN
        K=0
        DO I=0,14
          WRITE(TXTADW,1000) (K+N      ,T1(N,I),TPIC(N,I),N=0,7)
          K=K+8
          CALL DWRC
        END DO
        GO TO 930
      END IF
      CALL DWRT('Processor not found.#')
      GO TO 930
  940 IF(TANSW.NE.'DO') GO TO 920
      WRITE(TXTADW,1001) TIM1,TIM2,TIS1
      CALL DWRC
      DO I=0,14
        WRITE(TXTADW,1000) (LOG2(N,I),T1(N,I),TPIC(N,I),N=0,7)
        FF=.FALSE.
        DO K=7,72,9
          IF(TXTADW(K:K+3).EQ.'0_  '.OR.
     &       TXTADW(K:K+3).EQ.'0/  ') THEN
             TXTADW(K:K+3)=   ' .  '
          ELSE IF(TXTADW(K-1:K).EQ.' 0') THEN
            TXTADW(K:K)=' '
            FF=.TRUE.
          ELSE
            FF=.TRUE.
          END IF
        END DO
        IF(FF) CALL DWRC
      END DO
      GO TO 930
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DBRLOG
CH
      ENTRY DBRLOG(TMODE)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
      IF(TMODE.EQ.'READ') THEN
        CALL DATE2(TINEW)
        CALL DGOPEN(NUNIDU   ,TFIL(1:9),2,*97,ISTAT)
        READ(NUNIDU,1001) TIM1,TIM2,TIS0
        TIS1=TIS0
        DO I=0,14
          READ(NUNIDU,1000,ERR=96,END=96)
     &      (LOG2(N,I),T1(N,I),TP(N),N=0,7)
          DO N=0,7
            IF(TP(N).NE.'  '.AND.TP(N).NE.TPIC(N,I)) LOG2(N,I)=0
          END DO
        END DO
   96   CLOSE(UNIT=NUNIDU)
        LOG1(0)=LOG1(0)+1
        IF(TIM2(4:6).NE.TINEW(4:6)) THEN
          CALL DPARGV(81,'BST',4,PST)
          IF(PST.EQ.1.) THEN
C           monthly output for H.Drevermann
            TFIL(11:13)=TIM2(4:6)
            TFIL(15:16)=TIM2(8:9)
            CALL DGOPEN(NUNIDU,TFIL,N5,*99,ISTAT)
            GO TO 55
          END IF
        END IF
      ELSE IF(TMODE.EQ.'WRITE') THEN
        CALL DGOPEN(NUNIDU,TFIL(1:9),I5,*99,ISTAT)
        GO TO 55
      END IF
      RETURN
   55 TIM2=TINEW
      WRITE(NUNIDU,1001) TIM1,TIM2,TIS0
      DO I=0,14
        WRITE(NUNIDU,1000) (LOG2(N,I),T1(N,I),TPIC(N,I),N=0,7)
      END DO
      CLOSE(UNIT=NUNIDU)
      IF(TMODE.EQ.'READ') THEN
        CALL DWRT(TFIL//' stored.')
        DO I=1,119
          IF(TPICDP(I).NE.'  '.AND.TT1(I).EQ.'_') TT1(I)='.'
        END DO
        TIS0=TIS1
      END IF
      RETURN
   97 CALL DATE2(TIM1)
      LOG1(0)=LOG1(0)+1
      RETURN
   99 TPIC(0,0)='# '
 1001 FORMAT(3(5X,A))
 1000 FORMAT(1X,8(I6,A1,A2))
      END
*DK DB_JUMP_TO_COMMAND_LIST
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++ DB_JUMP_TO_COMMAND_LIST
CH
      SUBROUTINE DB_JUMP_TO_COMMAND_LIST
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW
      DATA LBUT/1/
      CALL DOPER_TANSW(TANSW,ITYPE)
      IF(TANSW.NE.'G*'.OR.ITYPE.NE.2) THEN
        CALL DGLBUT(IBUT)
        LBUT=IBUT
      ELSE
        IBUT=LBUT
      END IF
      IF(IBUT.EQ.2) THEN
        TAN1DH=TPICDO//'1'
        CALL DGSBUT(1)
      END IF
      END
