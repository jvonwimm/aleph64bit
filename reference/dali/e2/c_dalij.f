*DK DJFIL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DJFIL
CH
      SUBROUTINE DJFIL
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C     Processor to open EDIR,EPIO,NATIVE files.
C
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TENAM
      CHARACTER *50 TERR
      CHARACTER *70 T,TNAM,TEDIR
      CHARACTER *70 TCFLM
      CHARACTER *10 T10
      CHARACTER *5 TRU
      CHARACTER *2 TMIN
      EQUIVALENCE (TRU,T10)
C      DATA J3/3/
      LOGICAL FNOFI
C     .......................................... See commands in DALI___.HLP
      CALL DQHL_MENU('FI0')
C     .......................................................... Type header
    1 IF(IFILDA.EQ.0) THEN
        CALL DWR_LINE(1,'DEFAULT')
        CALL DWR_HIGH_LIGHT('FI:'//TFINDE(1)(1:LNINDE(1)),1,2)
C          123456789 123456789 123456789 123456789 123456789
        T='Enter filename, <CR>=open file above, GB=Go Back'
        IF(TFINDE(1).EQ.' ') T(16:37)=' '
        CALL DWRT(T)
        CALL DPARGI(81,'RSQ',INSEQ)
        IF(INSEQ.EQ.1) CALL DWRT('Edir file is nonsequential.')
        
C       ....................................................................
        CALL DW_GETLN(T,LNIN,80)
      ELSE
        LNIN=0
      END IF
      TMIN=T(1:2)
      TRU=' '
      TEDIR=' '
C     =======================================================================
C     .............................................................. 1 LETTER
      IF(LNIN.EQ.1) THEN
C       ...................................................... HELP ...... <
        IF(T(1:1).EQ.'<') CALL DQHLP('<<')
C       ...................................................................\
        IF(T(1:1).EQ.'\') CALL DJLIFI(T,LNIN,0,0)
        GO TO 251
C     =======================================================================
C     .............................................................. 2 LETTER5
      ELSE IF (LNIN.EQ.2) THEN
C       ................................................................ GB
        IF(T.EQ.'GB'.OR.T.EQ.'gb') THEN
          TPICDO='GB'
          CALL DGSWPT(-5)
          RETURN
C       ................................................................ QU
        ELSE IF(T.EQ.'GT'.OR.T.EQ.'gt') THEN
          TPICDO='GT'
          CALL DGSWPT(-5)
          RETURN
C       ................................................................ QU
        ELSE IF(T.EQ.'QU'.OR.T.EQ.'qu') THEN
          TPICDO='QU'
          CALL DGSWPT(-5)
          RETURN
C       ................................................................ QU
        ELSE IF(T.EQ.'TS'.OR.T.EQ.'ts') THEN
          CALL DPAR_TOGGLE(81,'RSQ',2)
        GO TO 1
C       ................................................................ D?
        ELSE IF(T.EQ.'D?'.OR.T.EQ.'d?') THEN
          CALL D_JDEB
          GO TO 1
        ELSE
C         .................. set flags to get debug output when reading EDIR.
          IF(T.EQ.'O0') NDEB=0
          IF(T.EQ.'O-') NDEB=-1
          IF(T.EQ.'O6') NDEB=6
          IF(T.EQ.'O8') NDEB=8
        END IF
        GO TO 251
C     =======================================================================
C     ............................................................ <4 LETTER5
C     .................................... List file with current file names.
      ELSE IF(LNIN.LE.3.AND.T(1:1).EQ.'\') THEN
        CALL DJLIFI(T,LNIN,TFINDE(1),LNINDE(1))
        GO TO 251
C     =======================================================================
C     ............................................................ <7 LETTER5
C     ........................................ Get name via .D,.P,.M COMMANDS
      ELSE IF((LNIN.LE.7).AND.(T(1:1).EQ.'.').AND.
     &   (T(2:2).NE.'.').AND.(T(2:2).NE.'/')) THEN
        IF(TPLFD0.NE.'VMS') CALL DGUPCS(T,T)
        CALL DJGTFN(NUNIDU,T,LNIN,TNAM,TRU)
        IF(TNAM.EQ.' ') GO TO 251
        T=TNAM
C     =======================================================================
C     .............................................................. RETURN
C     ............................................. ACCEPT NAME = DO = <CR>
      ELSE IF(LNIN.LE.0) THEN
        T=TFINDE(1)
      END IF
C     ***************************************************** START EXECUTION.
      CALL DJ_LENOFF(T,LNIN)
      CALL DGSWPT(5)
C     ................................................................ EDIR
C     ................................................................ ====
    2 IF(LNIN.GE.6.AND.((T(LNIN-4:LNIN).EQ.'.EDIR').OR.
     & (T(LNIN-4:LNIN).EQ.'.edir'))) THEN
        CALL DJ_CHANGE_NAME('ED',LNIN-4,LNIN,T,*900)
        TEDIR=T(1:LNIN)
        CALL DGOPEN(NUNIDU,TEDIR,2,*907,ISTAT)
        IF(TRU.EQ.' '.AND.IFILDA.EQ.0) THEN
C           ........ In case of files AB----.EDIR list runs from RUNCARTS.LIST
C          IF(LNIN.GT.10)
C     &      CALL DJGTRN(89,T(LNIN-10:LNIN-5))
CC         ....................................................... Specify run.
          CALL DGSWPT(-5)
          CALL DWRT(
     &      'Do you want to select a special run. "GB",<CR>=no')
          CALL DGETLN(TRU,LRU,10)
C         ............................................................... GB
          IF(LRU.EQ.2.AND.TRU.EQ.'GB') THEN
            CLOSE(UNIT=NUNIDU)
            GO TO 251
          END IF
          CALL DGSWPT(5)
          IF(LRU.LE.0) TRU=' '
        END IF
C       .................................................. read EDIR
        CALL DJRDED(NUNIDU,TRU,NDEB,IR,LCLSDE,TERR)
        CLOSE(UNIT=NUNIDU)
        IF(TERR.EQ.'STOP') GO TO 251
        FNOFDF=.TRUE.
        IE=0
        IF(TERR.NE.' ') THEN
C         ............................................................ error
          CALL DWRT(TERR)
          IF(TERR(5:5).EQ.'!') THEN
C           .................................... If serious error, clear all.
            CALL DWRT('DALI EDIR tables are destroyed. Read again.')
            IF(RNCLDU.EQ.0) THEN
              CALL DQCL(0)
              CALL DGCLWK
            END IF
          END IF
          IF(TERR(1:1).NE.' ') GO TO 908
        END IF
        CALL DPARGI(81,'USL',LEVUS)
C       IF(USLVDU.LT.7..OR.IFILDA.NE.0) THEN
        IF(LEVUS.LT.7.OR.IFILDA.NE.0) THEN
          IFILDA=0
C         ................... If userlevel < 7 read first event of EDIR/EPIO
C         CALL DJPOSE(0.,0.,TCFLM,JPOS,LCLSDE,JDUM,TERR)
          CALL DJPOSN(1,TCFLM,JPOS,LCLSDE,TERR)
          IF(TERR.NE.' ') THEN
            CALL DWRT(TERR)
            CALL DWRT('Current event is still stored.')
            GO TO 908
          END IF
          CALL DJ_RD_EPED_EV(TCFLM,JPOS)
          CALL ABRUEV(IR,IE)
        ELSE
          IF(RNCLDU.EQ.0) THEN
            CALL DQCL(0)
            CALL DGCLWK
          END IF
          TFINDE(1)=T
          CALL DJ_LENOFF(T,LNINDE(1))
          IRUNDE(1)=IR
          IEVTDE(1)=IE
          IRUNDE(2)=IR
          IEVTDE(2)=IE
          CALL DJFIW(T)
          TPICDO='EV'
          CALL DWRT('No event stored yet. Type: "RF" or "DF" etc..')
          CALL DGSWPT(-5)
          RETURN
        END IF
C     =============================================== EPIO, SL, NATIVE
      ELSE
        IF(LNIN.GE.6.AND.((T(LNIN-4:LNIN).EQ.'.EPIO').OR.
     &    (T(LNIN-4:LNIN).EQ.'.epio'))) THEN
          CALL DJ_CHANGE_NAME('EP',LNIN-4,LNIN,T,*900)
        END IF
        CALL D_LKOPRD(LNIN,T,IR,IE,FNOFI,IER)
        IF(FNOFI) THEN
          CALL DWRT('Wrong name or command ?#')
          GO TO 908
        END IF
        IFILDA=0
        IF(IER.NE.0) THEN
          CALL DWRT('File cannot be opened.')
          GO TO 908
        END IF
        IF(RNCLDU.EQ.0) THEN
          CALL DQCL(0)
          CALL DGCLWK
        END IF
      END IF
  100 TFINDE(1)=T
      LNINDE(1)=LENOCC(T)
      IRUNDE(1)=IR
      IEVTDE(1)=IE
      IRUNDE(2)=IR
      IEVTDE(2)=IE
      CALL DJFIW(T)
      CALL DEVNEW
      CALL DEVSET(1)
      TPICDO='EV'
      CALL DGSWPT(-5)
      RETURN
  900 TFINDE(1)=T
      LNINDE(1)=LNIN
      GO TO 1
  907 CALL DWRT('EDIR-file not found.')
  908 CALL DGSWPT(-5)
  251 IFILDA=0
      GO TO 1
C
C
C        CALL D_OPSEQ(T(1:LNIN),IER)
C        DO J=1,J3
C          CALL D_LKGTEVT(IR0,IE0,IER,'DJFIL')
C          CALL ABRUEV(IR,IE)
C          IF(IR.GT.0) GO TO 100
C          IF(IER.NE.0) THEN
C            WRITE(TXTADW,2000) IER
C 2000       FORMAT('Subroutine DJFIL: Error in LKGTEVT = ',I3)
C            CALL DWRC
C            GO TO 908
C          END IF
C        END DO
C
C
C        IF(TPLFD0.EQ.'VMS') THEN
C          IF(T(1:2).EQ.'AB'.OR.T(1:2).EQ.'AA') THEN
C            T='[]'//T
C            LNIN=LNIN+2
C          END IF
C        ELSE
C          IF(T(1:2).EQ.'./') THEN
CC           ......... EPIO file names cannot start with ./ Why????  
CC           ......... B.S.N, 13-Jan-1994
C            T=T(3:)
C            LNIN=LNIN-2
C          END IF
C        END IF
C
C
C
C
CH..............---
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++  DJ_FILE_EDIR_NAME
CH
      ENTRY DJ_FILE_EDIR_NAME(LENAM,TENAM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      TENAM=TEDIR(1:LENAM)
      END
*DK DJ_LENOFF
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DJ_LENOFF
CH
      SUBROUTINE DJ_LENOFF(T,LEN)
CH 
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CHARACTER *(*) T
      LEN=LENOCC(T)
      DO L=LEN,4,-1
        IF(T(L:L).EQ.';') GO TO 1
      END DO
      RETURN
    1 LEN=L-1
      END
*DK DJLIFI
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DJLIFI
CH
      SUBROUTINE DJLIFI(T,LNIN,TOUT,LOUT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) T,TOUT
      CHARACTER *79 TIN
      CHARACTER *12 TFIL
      TFIL=TFILDC//'FILE'
      CALL DGOPEN(NUNIDU,TFIL,2,*99,ISTAT)
    8 IF(LNIN.EQ.1) THEN
   10   READ(NUNIDU,'(A)',END=98) TIN
C""     IF(TPLFD0.EQ.'VMS') CALL CLTOU(TIN)

C#ifdef VMS
C        CALL CLTOU(TIN)
C#endif /* VMS */

        IF(TIN(1:2).NE.'  ') THEN
          LN=LENOCC(TIN)
          CALL DWRT(TIN(1:LN))
        END IF
        GO TO 10
      END IF
   20 READ(NUNIDU,'(A)',END=97) TIN
C""    IF(TPLFD0.EQ.'VMS') CALL CLTOU(TIN)

C#ifdef VMS
C      CALL CLTOU(TIN)
C#endif /* VMS */

      IF(TIN(1:2).EQ.T(2:3)) THEN
        LN=LENOCC(TIN)
        LOUT=LN
        DO L=LN,1,-1
          IF(TIN(L:L).EQ.'!') LOUT=LENOCC(TIN(1:L-1))
        END DO
        TOUT=TIN(4:LOUT)
        IF(LOUT.LT.LN) CALL DWRT(TIN(LOUT+1:LN))
        GO TO 98
      END IF
      GO TO 20
   97 CALL DWRT(' Index not found')
   98 CLOSE(UNIT=NUNIDU)
      RETURN
   99 CALL DWRT(TFIL//' not found')
      END
*DK DJFIR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DJFIR
CH
      SUBROUTINE DJFIR
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TNAM
      CHARACTER *80 T
      CHARACTER *8 TTIME
      CHARACTER *9 TDATE
      CALL DGOPEN(NUNIDU,TFILDC//'IFI',2,*9,ISTAT)
      READ(NUNIDU,1000,ERR=8,END=8) T
 1000 FORMAT(A)
      TFINDE(2)=T
      LNINDE(2)=LENOCC(TFINDE(2))
      TFINDE(1)=T
      LNINDE(1)=LNINDE(2)
      READ(NUNIDU,1000,ERR=8,END=8) T
    8 CLOSE(UNIT=NUNIDU)
      FNEWDW=.FALSE.
      RETURN
    9 FNEWDW=.TRUE.
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DJFIW
CH
      ENTRY DJFIW(TNAM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C
C ---------------------------------------------------------------------
      CALL TIME(TTIME)
      CALL DATE(TDATE)
      CALL DGOPEN(NUNIDU,TFILDC//'IFI',5,*90,ISTAT)
      WRITE(NUNIDU,'(A)') TNAM
      WRITE(NUNIDU,'(A)') '!Saved : '//TTIME//' - '//TDATE
      CLOSE(UNIT=NUNIDU)
   90 END
*DK DJGTFN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DJGTFN
CH
      SUBROUTINE DJGTFN(NUNIT,T,LENT,TFIL,TRU)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C     GET FILENAME OF EDIR FOR GIVEN RUN FROM RUNCARTS.LIST
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) T,TFIL,TRU
      CHARACTER *1 TSL
      CHARACTER *3 TSEL(5)
      DATA TSEL/'RAW','POT','DST','MIN','NAN'/
      CHARACTER *7 TR
      CHARACTER *24 TBOOK
      LR=LENT-2
      IF(LR.LE.0) RETURN
      DO K=1,5
        IF(T(2:2).EQ.TSEL(K)(1:1)) GO TO 1
      END DO
      RETURN
    1 NFP=K      
      TRU=' '
      TR=' '
      TR(8-LR:7)=T(3:LENT)
      CALL DW_GET_PLATFORM_TEXT('BR',TBOOK,24)
      LBOOK=LENOCC(TBOOK)
      CALL DGOPEN(NUNIT,TBOOK(1:LBOOK),2,*7,ISTAT)
      READ(NUNIT,1000) TXTADW
 1000 FORMAT(A)
      READ(NUNIT,1000) TXTADW
      LR2=INDEX(TXTADW,'RAW')-2
      LD1=INDEX(TXTADW,TSEL(NFP))
      CALL DWRC
    2 READ(NUNIT,1000,END=90) TXTADW
      IF(TXTADW(1:LR2).NE.TR) GO TO 2
      CALL DW_GET_PLATFORM_TEXT('ED',TFIL,20)
      LFIL=LENOCC(TFIL)+1
      LD2=INDEX(TXTADW(LD1:),' ')+LD1-2
      TFIL(LFIL:)=TXTADW(LD1:LD2)//'.EDIR'
      CALL DW_GET_PLATFORM_TEXT('SL',TSL,1)
      IF(TSL.EQ.'S') CALL CUTOL(TFIL(LFIL:LFIL+LD2-LD1+5))
      TRU=T(3:LENT)
      CALL DWRC
    9 CLOSE(UNIT=NUNIT)
      RETURN
    7 CALL DWRT('File '//TBOOK(1:LBOOK)//' not found.')
      TFIL=' '
      RETURN
   90 CLOSE(UNIT=NUNIT)
      CALL DWRT('Non existent run on '//TBOOK(1:LBOOK))
      CLOSE(UNIT=NUNIT)
      TFIL=' '
      END
*DK DJGTRN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DJGTRN
CH
      SUBROUTINE DJGTRN(NUNIT,TFL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C ---------------------------------------------------------------------
      CHARACTER *(*) TFL
      DIMENSION LD1(5),LD2(5)
      CHARACTER *4 TSEL(6)
      DATA TSEL/'RAW ','POT ','DST ','MINI','NANO','| RQ'/
      CHARACTER *6 TFIL
      CHARACTER *24 TBOOK
      CHARACTER *80 TX
      TFIL=TFL
      CALL CLTOU(TFIL)
      LF=LENOCC(TFIL)
      IF(TFIL(1:1).NE.'A') RETURN
      CALL DW_GET_PLATFORM_TEXT('BR',TBOOK,24)
      LBOOK=LENOCC(TBOOK)
      CALL DGOPEN(NUNIT,TBOOK(1:LBOOK),2,*7,ISTAT)
      READ(NUNIT,1000) TX
 1000 FORMAT(A)
      READ(NUNIT,1000) TX
      DO K=1,5
        LD1(K)=INDEX(TX,TSEL(K  ))
        LD2(K)=INDEX(TX,TSEL(K+1))-2
      END DO
      READ(NUNIT,1000) TX
      L=7
    1 READ(NUNIT,1000,END=9) TX
      DO I=1,5
        IF(TX(LD1(I):LD2(I)).EQ.TFIL) THEN
          CALL DWRT(' File = '//TSEL(I))
          L=L+1
          IF(L.GT.6) THEN
            L=1
            CALL DWRT('RUNS = +')
          END IF
          CALL DWR_ADD(TX(1:7))
C         ............ now we know I and speed up reading by not looping on I
C         ............ and reading only up to TNAM(I)
   11     READ(NUNIT,1000,END=9) TX
          IF(TX(LD1(I):LD2(I)).EQ.TFIL) THEN
            L=L+1
            IF(L.GT.6) THEN
              L=1
              CALL DWRT('RUNS = +')
            END IF
            CALL DWR_ADD(TX(1:7))
          END IF
          GO TO 11
        END IF
      END DO
      GO TO 1
    9 CLOSE(UNIT=NUNIT)
    7 END
*DK DJRDED
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DJRDED
CH
      SUBROUTINE DJRDED(NUNIT,TRU,NDEB,NCRUN,KCLS,TERR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C ---------------------------------------------------------------------
C     Read edir file: There are three modes:
C           PARAMETER (MAXEV=30000,MAXRU=15000,MAXEV2=15001)
C     1: TRU=' ' All events on the EDIR are stored up to MAXRU events.
C        output to TPOS (MAXRU),NRUN(MAXRU),
C                  NEVT (MAXRU),LCLS(MAXRU),
c                  NFILM(MAXRU),TFILM(100)
C        Up to 100 films can be stored.
C        Position, run and film are stored in ASCII left adjusted.
C        The film name is found in TFILM(NFILM)
C            KRUN=0, FRUN1=.FALSE. , FRUNQ=.FALSE.
C        IF all MAXRU records have the same run number, further records are
C        stored up to MAXEV or the change of the run number, whatever is first.
C
C     2: a run is selected: TRU#' ': Only events of run=TRU are stored.
C        output to TPOS(MAXEV),KRUN,
C                  NEVT(MAXEV),LCLS(MAXEV),
C                  TFILM(1)
C        It is assumed that all events of 1 run are grouped together on 1 film.
C            KRUN = Run number > 0, FRUN1=.TRUE. , FRUNQ=.FALSE.
C
C     3: A run is interactively selected, while reading the EDIR file, this is
C        more time consuming. KRUN=0, FRUN1=.TRUE. , FRUNQ=.TRUE. When a run is
C        accepted: KRUN= # of run, FRUN1=.TRUE., FRUNQ=.FALSE.
C
C     INPUTS: NUNIT = UNIT # FOR READING. THE FILE IS CLOSED AFTER READING.
C             NRUN = RUN NUMBER
C             NDEB = 0     NO DEBUG OUTPUT
C                   -1     FIRST AND LAST RECORDS ARE LISTED ON THE TERMINAL
C                    6     EXPLICIT DEBUG OUTPUT ON TERMINAL
C     OUTPUT: TERR= ERROR
C              TERR=' ' NO ERROR
C              TERR=' NME. RU=12345 EV=12345 No more events accepted.'
C
C             CURRENT STATUS UNCHANGED:
C              TERR='FNND: Film name not defined.'
C              TERR='RNTB: Run number too big.'
C              TERR='RNFE: The EDIR file does not contain the run.'
C
C             CURRENT STATUS IS CHANGED. READ GOOD EDIR FILE.
C              TERR='WSP2! Wrong EDIR-structure '//T12(LL+KR0-9:LL+KR0+7)
C              TERR='WSP1! Wrong EDIR-structure '//T12(LL+KP2-9:LL+KP2+7)
C              TERR='WSR2! Wrong EDIR-structure '//T0(KR1:KR1+7)
C              TERR='WSE2! Wrong EDIR-structure '//T0(KE1:KE1+7)
C              TERR='TMFI! More than 100 films are not accepted'
C              TERR='WSP2! Wrong EDIR-structure '//T0(KP1:KP1+9)
C              TERR='WSCL! Wrong EDIR-structure '//T0(KC1-1!KC1+4)
C              TERR='CEC4! Conversion error of class word-4.'
C              TERR='CEC8! Conversion error of class word-8.'
C              TERR='UEOF! Unexpected end of file on EDIR.'
C              TERR='CEEV! Conversion error of event.'
C     .................................................................
      INCLUDE 'DALI_CF.INC'
      PARAMETER (MAXEV=30000,MAXRU=15000,MAXEV2=15001,MAXFL=100)
      COMMON /DEDIR1/ NEVT(MAXEV),LCLS(MAXEV)
      COMMON /DEDIR2/ JTOT,JCUR,JCURL,KRUN,NCLS,FCLS,FOR
      COMMON /DEDIRT/ TPOS(MAXEV),TFILM(MAXFL)
      CHARACTER *8 TPOS
      LOGICAL FCLS,FOR
      DIMENSION NRUN(MAXRU),NFILM(MAXRU)
      CHARACTER *69 TFILM
      EQUIVALENCE (NEVT(MAXEV2),NRUN),(LCLS(MAXEV2),NFILM)
C     .................................................................
      CHARACTER *10 TRUS,TRUS1,TRLST
      CHARACTER *81 T0,TLAST
      CHARACTER *162 T12
      CHARACTER *(*) TERR,TRU
      LOGICAL FRUN1,FRUNQ
      DATA TLAST/' '/
C
      S0=SECNDS(0.)
      CALL DWRT(' --- WAIT ---')
      JFILM=0
      S1=0.
      J=1
      MAXRC=MAXRU
      READ(TRU,1008,ERR=10) KRUN
 1008 FORMAT(BN,I10)
C     ................................................. MODE 1
      IF(TRU.EQ.' ') THEN
        FRUNQ=.FALSE.
        FRUN1=.FALSE.
        GO TO 302
      END IF
C     ................................................. MODE 2
      FRUNQ=.FALSE.
      FRUN1=.TRUE.
      MAXRC=MAXEV
      LRU=LENOCC(TRU)
      IF(LRU.GT.5) THEN
        TERR='RNTB: Run number too big.'
        GO TO 999
      END IF
      TRUS=' '
      LRUN0=LRU-1
      TRUS(2:2+LRUN0)=TRU(1:LRU)
      LRUN0=LRUN0+2
      LRUN1=LRUN0+1
      TLAST=' '
      READ(NUNIT,1000) T0
      IF(T0(2:5).NE.'FILM') THEN
        TERR='FNND: Film name not defined.'
        GO TO 999
      END IF
      CALL DJFILM(T0,TFILM(1))
C      IF(TFILM(1)(1:2).EQ.'AB'.OR.TFILM(1)(1:2).EQ.'AA') THEN
C        TERR='WRED: EDIT EDIR TO [....]AB____.EPIO'
C        GO TO 999
C      END IF
C      GO TO 602

      GO TO 1
C     ................................................. MODE 3
   10 FRUNQ=.TRUE.
      FRUN1=.TRUE.
      TRLST=' '
      GO TO 302
C     .......................................................... FAST SEARCH
    1 READ(NUNIT,1000,END=992) T0
 1000 FORMAT(A)
      IF(T0(2:5).EQ.'FILM') CALL DJFILM(T0,TFILM(1))
      DO KR0=1,74
        IF(T0(KR0:KR0+LRUN0).EQ.TRUS(1:LRUN1)) GO TO 2
      END DO
      TLAST=T0
      GO TO 1
C     .......................................................................
    2 LL=LENOCC(TLAST)
      T12=TLAST(1:LL)//T0
      DO KP2=LL+KR0-1,1,-1
        IF(T12(KP2:KP2).EQ.' ') GO TO 3
      END DO
      TERR='WSP2: Wrong EDIR-structure '
      GO TO 990
C     .......................................... check if number = run number
    3 IF(T12(KP2+1:KP2+1).NE.'Z') GO TO 1
C     ......................................................... end of search
    9 S1=SECNDS(S0)
      DO KP1=KP2,KP2-9,-1
        IF(T12(KP1:KP1).EQ.'-') GO TO 101
      END DO
      TERR='WSP1: Wrong EDIR-structure '
      GO TO 990
  101 TPOS(J)=T12(KP1+1:KP2-1)
      KC1=KP2+2
      KC2=KC1+3
      IF(T12(KC2:KC2).EQ.' ') THEN
        READ(T12(KC1:KC2),3004,ERR=993) LCLS(J)
 3004   FORMAT(Z4)
      ELSE
        KC2=KC2+4
        READ(T12(KC1:KC2),3008,ERR=994) LCLS(J)
 3008   FORMAT(Z8)
      END IF
      KR1=KR0+1

C      IF(NDEB.GE.6) WRITE(NDEB,4001) T0(2:79)

 4001 FORMAT(1X,A)
C     ................................... FIND END OF RUN-WORD
  200 DO KR2=KR1,KR1+7
        IF(T0(KR2:KR2).EQ.' ') GO TO 201
      END DO
      TERR='WSR2: Wrong EDIR-structure '
      T12=TLAST//T0
      GO TO 990
  201 IF(FRUN1) THEN
        IF(FRUNQ) THEN
          IF(T0(KR1:KR2-1).NE.TRLST.AND.T0(KR1-1:KR2).NE.' 0 ') THEN
            TRLST=T0(KR1:KR2-1)
C
            CALL DTYANS('Run '//TRLST//' ? Stop/Y/N=<CR>','YNS',NANSW)
            IF(NANSW.LT.0.OR.NANSW.GE.3) THEN
              JCUR=0
              JTOT=0
              TERR='STOP'
              CALL DWRT('Reading of EDIR stopped; nothing stored.')
              RETURN
            ELSE IF(NANSW.EQ.1) THEN
              TRUS=T0(KR1-1:KR2)
              FRUNQ=.FALSE.
              READ(T0(KR1:KR2-1),1008) KRUN
            END IF
          END IF
        ELSE
          IF(T0(KR1-1:KR2).NE.TRUS.AND.T0(KR1-1:KR2).NE.' 0 ')
     &       GO TO 400
        END IF
      ELSE
        READ(T0(KR1:KR2-1),3000) NRUN(J)
        NFILM(J)=JFILM
      END IF
      KE1=KR2+1
C     .................................................. READ NEXT LINE
      IF(KE1.GT.80.OR.T0(KE1:80).EQ.' ') THEN
        READ(NUNIT,1000,END=995) T0

C        IF(NDEB.GE.6) WRITE(NDEB,4001) T0(2:79)

        KE1=2
      END IF
C     .................................................................
C     ................................... FIND END OF EVENT WORD
      DO KE2=KE1+1,KE1+7
        IF(T0(KE2:KE2).EQ.' ') GO TO 202
      END DO
      TERR='WSE2: Wrong EDIR-structure '
      T12=TLAST//T0
      GO TO 990
  202 IF(T0(KE1:KE2-1).NE.'0') THEN
        READ(T0(KE1:KE2-1),3000,ERR=996) NEVT(J)
 3000   FORMAT(I7)

C        IF(NDEB.GE.6) THEN
C          IF(FRUN1) THEN
C            WRITE(NDEB,4002) J,TPOS(J),LCLS(J),TRUS,NEVT(J)
C 4002       FORMAT(1X,I5,2X,A,2X,Z8,1X,2I9,I4)
C          ELSE
C            WRITE(NDEB,4002) J,TPOS(J),LCLS(J),NRUN(J),NEVT(J),NFILM(J)
C          END IF
C        END IF

        IF(J.GE.MAXRC) THEN
          DO IC1=2,MAXRC
            IF(NRUN(1).NE.NRUN(IC1)) GO TO 991
          END DO
          FRUN1=.TRUE.
          KRUN=NRUN(1)
          WRITE(TRUS1,1007) NRUN(1)
 1007     FORMAT(I5)
          DO L1=1,9
            IF(TRUS1(L1:L1).NE.' ') THEN
              DO L2=L1,10
                IF(TRUS1(L2:L2).EQ.' ') GO TO 207
              END DO
            END IF
          END DO
  207     TRUS=' '//TRUS1(L1:L2-1)
          MAXRC=MAXEV
        END IF
        IF(.NOT.FRUNQ) J=J+1
      END IF
C     ................................... FIND START OF POSITION WORD '-'
  301 DO KP1=KE2+1,80
        IF(T0(KP1:KP1).EQ.'-') GO TO 303
      END DO
C     .................................................. READ NEXT LINE
  302 READ(NUNIT,1000,END=400) T0

C      IF(NDEB.GE.6) WRITE(NDEB,4001) T0(2:79)

      IF(T0(2:2).EQ.'E') GO TO 302
      IF(T0(2:5).EQ.'FILM') THEN
        IF(FRUN1.AND.(.NOT.FRUNQ)) GO TO 400
        IF(JFILM.GE.MAXFL) THEN
          TERR='TMFI: More than 100 films are not accepted'
          GO TO 999
        END IF
        JFILM=JFILM+1
        CALL DJFILM(T0,TFILM(JFILM))
        IF(TRU.EQ.'?') JFILM=JFILM-1
        GO TO 302
      END IF
      KE2=-1
      GO TO 301
C     ................................... FIND END OF POSITION WORD
  303 DO KP2=KP1+1,KP1+9
        IF(T0(KP2:KP2).EQ.' ') GO TO 203
      END DO
      TERR='WSP2: Wrong EDIR-structure '
      T12=TLAST//T0
      GO TO 990
  203 TPOS(J)=T0(KP1+1:KP2-1)
      KC1=KP2+2
C     .................................................. READ NEXT LINE
      IF(KC1.GT.80.OR.T0(KC1:80).EQ.' ') THEN
        READ(NUNIT,1000,END=995) T0

C        IF(NDEB.GE.6) WRITE(NDEB,4001) T0(2:79)

        KC1=3
      END IF
C     .................................................................
      IF(T0(KC1-1:KC1-1).NE.'Z') THEN
        TERR='WSCL: Wrong EDIR-structure '
        T12=TLAST//T0
        GO TO 990
      END IF
C     ................................... FIND END OF CLASS WORD
      KC2=KC1+3
      IF(T0(KC2+1:KC2+1).EQ.' ') THEN
        READ(T0(KC1:KC2),3004,ERR=993) LCLS(J)
      ELSE
        KC2=KC2+4
        READ(T0(KC1:KC2),3008,ERR=994) LCLS(J)
 3001   FORMAT(Z8)
      END IF
C     .................................................. READ NEXT LINE
      KR1=KC2+2
      IF(KR1.GT.80.OR.T0(KR1:80).EQ.' ') THEN
        READ(NUNIT,1000,END=995) T0

C        IF(NDEB.GE.6) WRITE(NDEB,4001) T0(2:79)

        KR1=2
      END IF
C     .................................................................
      GO TO 200
  400 IF(FRUNQ) GO TO 992
      S2=SECNDS(S0)-S1
      WRITE(TXTADW,5003) S1,S2
 5003 FORMAT(' ',F8.1,' +',F8.1,' seconds to read EDIR')
      CALL DWRC
      JTOT=J-1
C     .................................................................
      IF(NDEB.LT.0) THEN
        DO M=1,9
          WRITE(TXTADW,6000) M,TPOS(M),LCLS(M),NEVT(M)
 6000     FORMAT(I5,2X,A,2X,Z8,I9)
          CALL DWRC
        END DO
        DO M=JTOT-8,JTOT
          WRITE(TXTADW,6000) M,TPOS(M),LCLS(M),NEVT(M)
          CALL DWRC
        END DO
      END IF
C     .................................................................
      TERR=' '
      IF(KRUN.EQ.0) THEN
        NCRUN=NRUN(1)
      ELSE
        NCRUN=KRUN
      END IF
      GO TO 999
  990 CALL DWRT(T12)
      CALL DWRT('EDIR must not contain double blanks!')
      CALL DWRT('Classword must contain 4 or 8 characters!')
      GO TO 999
C           123456789 123456789 123456789
  991 TERR=' NME: RU=12345 EV=12345 = last event accepted.'
      CALL DWRT('The Dali event buffer is full. It may not')
      CALL DWRT('contain the event you are looking for. In')
      CALL DWRT('that case read the EDIR file again and')
      CALL DWRT('define the run. Read bottom of DALIhelp.##')
      JTOT=J
      IF(FRUN1) THEN
        WRITE(TERR(10:14),1007) KRUN
      ELSE
        WRITE(TERR(10:14),1007) NRUN(J)
      END IF
      WRITE(TERR(19:23),1007) NEVT(J)
      GO TO 999
  992 TERR='RNFE: The EDIR file does not contain the run.'
      GO TO 999
  993 TERR='CEC4! Conversion error of class word-4.'
      GO TO 999
  994 TERR='CEC8! Conversion error of class word-8.'
      GO TO 999
  995 TERR='UEOF! Unexpected end of file on EDIR.'
      GO TO 999
  996 TERR='CEEV! Conversion error of event.'

C  999 IF(NDEB.GT.6) CLOSE(UNIT=NDEB)

  999 IF(TERR(4:4).NE.':') THEN
        JCUR=0
        JCURL=1
        KCLS=0
      END IF
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DJFILM
CH
      SUBROUTINE DJFILM(T0,TFILM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      CHARACTER *(*) T0,TFILM
      CHARACTER *1 TUD
      DATA TUD/''''/
      LL=LENOCC(T0)
      L2=LL+1
      DO L1=1,LL
        IF(T0(L1:L1).EQ.TUD) THEN
          DO L2=L1+1,LL
            IF(T0(L2:L2).EQ.TUD) GO TO 1
          END DO
        END IF
      END DO
    1 LMAX=MIN(100,L2-1-L1)
      TFILM=T0(L1+1:L2-1)
      END
*CA DJEV
CH..............---
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DJEV
CH
      SUBROUTINE DJEV
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C
C INVOKED BY TANSW.EQ.'EV'  (DEV)
      INCLUDE 'DALI_CF.INC'
      CHARACTER *49 T
      CHARACTER *6 DT6
      CHARACTER *7 DT7
      PARAMETER (MPR1=35)
      CHARACTER *2 TANSW,TPR1(MPR1),TPR2(MBNCDB)
      DATA TPR1/'RU','NU','SC','AC',31*'**'/
      DATA TPR2/
     &  '**','FK','IT','PA','TB',
     &  'TP','ES','HS','HT','MH',
     &  'FR','FT','**','**','PY',
     &  'FD','LS','EO','EW','**',
     &  'VZ','VX','CR','NR','VC',
     &  'VP','YK'/
      DIMENSION PR1(4,MPR1)
      DATA (PR1(J,1),J=1,4)/0.,1.,100000.,0./
      DATA (PR1(J,2),J=1,4)/0.,1.,100000.,0./
      DATA (PR1(1,J),J=3,34)/32*0./
      DATA (PR1(2,J),J=3,34)/32*0./
      DATA (PR1(3,J),J=3,34)/32*31./
      DATA (PR1(4,J),J=3,34)/32*-1./
      LOGICAL FCHG,FCL,FCS,FEDIR,FOUND
C      CHARACTER *49 TX1,TX2,TX3
C               123456789 123456789 123456789 123456789 123456789
C      DATA TX1/'RN RF RB RS DN DF DB DS DO DW ZC SA SO LE LC LL'/
C      DATA TX2/'FI RE TY DD ED DC EC DA EA CE CD CT CL CC CS LB'/
C      DATA TX3/'IS IN    For debugging: T6 LI SB AD'/
      IF((TFINDE(1)(LNINDE(1)-4:LNINDE(1)).EQ.'.EDIR').OR.
     &   (TFINDE(1)(LNINDE(1)-4:LNINDE(1)).EQ.'.edir')) THEN
        FEDIR=.TRUE.
      ELSE
        FEDIR=.FALSE.
      END IF
      PR1(2,1)=IRUNDE(2)
      PR1(2,2)=IEVTDE(2)
  930 CALL DWR_LINE(1,'DEFAULT')
      T='EV:RU=1234567 NU=1234567 AL$EDIR:D0005375.EDIR   '
      T( 7:13)=DT7(FLOAT(IRUNDE(1)))
      IF(TPLFD0.EQ.'VMS') THEN
        T(18:24)=DT7(FLOAT(IEVTDE(1)))
        L1=MAX(1,LNINDE(1)-23)
        T(26:49)=TFINDE(1)(L1:LNINDE(1))
      ELSE
        L1=LENOCC(T(1:13))
        T(L1+1:)=' NU='//DT7(FLOAT(IEVTDE(1)))
        L1=LENOCC(T(1:L1+11))
        L2=MAX(1,LNINDE(1)-(48-L1-1))
        IF(L2.NE.1) THEN
          T(L1+2:49)='...'//TFINDE(1)(L2+3:LNINDE(1))
        ELSE
          T(L1+2:49)=TFINDE(1)(L2:LNINDE(1))
        END IF
      END IF

C#ifdef VMS
C      T(18:24)=DT7(FLOAT(IEVTDE(1)))
C      L1=MAX(1,LNINDE(1)-23)
C      T(26:49)=TFINDE(1)(L1:LNINDE(1))
C#else
C      L1=LENOCC(T(1:13))
C      T(L1+1:)=' NU='//DT7(FLOAT(IEVTDE(1)))
C      L1=LENOCC(T(1:L1+11))
C      L2=MAX(1,LNINDE(1)-(48-L1-1))
C      IF(L2.NE.1) THEN
C         T(L1+2:49)='...'//TFINDE(1)(L2+3:LNINDE(1))
C      ELSE
C         T(L1+2:49)=TFINDE(1)(L2:LNINDE(1))
C      ENDIF
C#endif /* VMS */

      CALL DWR_HIGH_LIGHT(T,1,2)
      IF(FEDIR) THEN
        TPR1(3)='SC'
        TPR1(4)='AC'
        DO K=3,MPR1
          PR1(2,K)=-1.
        END DO
        CALL DJCLTX(T,FCL)
C                   123456789 123456789 123456789 123456789 123456789
C                   15995| 12345| 123456789 0123456789 0123456789 012
        IF(FCL.OR.LCLSDE.NE.0) THEN
          CALL DWRT(
     &      '   class                1111111111 2222222222 33')
          IF(FCL)
     &      CALL DWRT('selected ('//T(37:39)//')'//T(1:35))
          IF(LCLSDE.NE.0) THEN
            CALL DJCLEV(LCLSDE,T)
            CALL DWRT('of event      '//T(1:35))
          END IF
        END IF
      ELSE
        TPR1(3)='**'
        TPR1(4)='**'
      END IF
      IF(IFIX(PR1(2,1)).NE.IRUNDE(1).OR.
     &   IFIX(PR1(2,2)).NE.IEVTDE(1)) THEN
        T='   RU=1234567 NU=1234567  to be selected'
        T( 7:13)=DT7(PR1(2,1))
        T(18:24)=DT7(PR1(2,2))
        CALL DWRT(T)
      END IF
  935 IF(FNOFDF) CALL DWRT('EPIO file NOT open!')
  936 FCHG=.FALSE.
      CALL DOPER(1,0,
     &  1,MPR1,TPR1,PR1,
     &  1,MBNCDB,TPR2,BNUMDB,
     1  NEXEC,FCHG,TANSW)
      IF(TPR1(3).EQ.'SC'.AND.FCHG) CALL DJCLIN(PR1(1,3))
      GO TO (910,920,931,940) NEXEC
  910 IRUNDE(2)=PR1(2,1)
      IEVTDE(2)=PR1(2,2)
      CALL DGSWPT(-5)
      RETURN
  931 IF(TANSW.NE.'OF'.AND.TANSW.NE.'ON') GO TO 930
      TANSW='TY'
  920 CALL DO_STR('DW')
      IF(TANSW.EQ.'DW') GO TO 940
      CALL DO_STR('RN"RB"RL"RF"RS"DN"DB"RL"DF"DS"RW')
      IF(TANSW(1:1).EQ.'R'.OR.TANSW(1:1).EQ.'D') THEN
        CALL DGSWPT( 5)
        IF(FEDIR) THEN
          CALL DJ_RD_EPED(TANSW,PR1,FOUND,LCLSDE)
        ELSE
          IF(FNOFDF) GO TO 935
          CALL DJ_RD_EPIO_OR_NATIVE(TANSW,PR1,FOUND)
        END IF
        CALL DGSWPT(-5)
        IF(FOUND) GO TO 930
      END IF
      IF(FEDIR) THEN
        CALL DO_STR('ZC')
        IF(TANSW.EQ.'ZC') THEN
          CALL DJCL00
          GO TO 930
        END IF
        IF(TANSW.EQ.'SA') THEN
          CALL DO_STR('SA')
          CALL DJCLMO('AND')
          GO TO 930
        END IF
        CALL DO_STR('SD')
        IF(TANSW.EQ.'SO') THEN
          CALL DJCLMO('OR')
          GO TO 930
        END IF
        CALL DO_STR('LE"LC')
        IF(TANSW.EQ.'LE'.OR.TANSW.EQ.'LC') THEN
          IF(TANSW.EQ.'LE') THEN
            FCS=.FALSE.
          ELSE
            CALL DJCLTX(T,FCS)
          END IF
  100     TAN1DH='EVL'
          CALL DJLIED(PR1,FCS)
          TAN1DH=' '
          IF(TPICDO.EQ.'GB'.OR.TPICDO.EQ.'EV') THEN
            TPICDO='EV'
            GO TO 930
          END IF
          GO TO 910
        END IF
      END IF
      CALL DO_STR('TY')
      IF(TANSW.EQ.'TY') THEN
        CALL DEVTYP(1)
        GO TO 930
      END IF
      CALL DO_STR('T6')
      IF(TANSW.EQ.'T6') THEN
        CALL DJSIW6
        GO TO 930
      END IF
      CALL DO_STR('LB')
      IF(TANSW.EQ.'LB') THEN
        CALL DJLBNK(MUN6DU,MUN9DU)
        GO TO 930
      END IF
      CALL DO_STR('LI')
      IF(TANSW.EQ.'LI') THEN
        CALL DJDRLI
        GO TO 930
      END IF
      CALL DO_STR('FI')
      IF(TANSW.EQ.'FI') THEN
        TPICDO='FI'
        GO TO 910
      END IF
      CALL DO_STR('SB')
      IF(TANSW.EQ.'SB') THEN
        CALL DGDINT(IRUNDE(1))
        CALL DWRT('Data base of run '
     &    //DT6(FLOAT(IRUNDE(1)))//' stored.')
        GO TO 936
      END IF
      CALL DO_STR('DD')
      IF(TANSW.EQ.'DD') THEN
        DO K=1,MBNCDB
          BNUMDB(4,K)=-1.
        END DO
        CALL DEVTYP(1)
        CALL DWRT('All banks are disabled!! Type "ED" to enable.')
        GO TO 936
      END IF
      CALL DO_STR('ED')
      IF(TANSW.EQ.'ED') THEN
        DO K=1,MBNCDB
          BNUMDB(4,K)= 1.
        END DO
        CALL DEVTYP(1)
        GO TO 936
      END IF
      CALL DO_STR('LL')
      IF(TANSW.EQ.'LL') THEN
        CALL DJSTT
        GO TO 936
      END IF
      CALL DO_STR('DC')
      IF(TANSW.EQ.'DC') THEN
        RNCLDU=1
        CALL DWRT('Clear with new event disabled.')
        GO TO 936
      END IF
      CALL DO_STR('EC')
      IF(TANSW.EQ.'EC') THEN
        RNCLDU=0
        CALL DWRT('Clear with new event enabled.')
        GO TO 936
      END IF
      CALL DO_STR('DA')
      IF(TANSW.EQ.'DA') THEN
        CHTFDU=0
        CALL DWRT('With new event theta and phi are not udated.')
        GO TO 936
      END IF
      CALL DO_STR('EA')
      IF(TANSW.EQ.'EA') THEN
        CHTFDU=1
        CALL DWRT('With new event theta and phi are udated.')
        GO TO 936
      END IF
      CALL DO_STR('CE"CC"CD"CT"CS"CL')
      IF(TANSW(1:1).EQ.'C') THEN
        IRUNDE(2)=PR1(2,1)
        IEVTDE(2)=PR1(2,2)
        CALL DJLSFN(TANSW,FOUND)
        IF(FOUND) GO TO 936
      END IF
      CALL DO_STR('PC: read private cards file')
      IF(TANSW.EQ.'PC') THEN
        CALL DJ_READ_PRIVATE_CARDS
        GO TO 930
      END IF
      IF(FEDIR) THEN
        CALL DO_STR('IS"IN"IP')
        IF(TANSW.EQ.'IS'.OR.TANSW.EQ.'IN'.OR.TANSW.EQ.'IP') THEN
          CALL DJINPE(TANSW,PR1(2,1),PR1(2,2))
          IF(TANSW.EQ.'IP') THEN
            CALL DGSWPT( 5)
            CALL DJ_RD_EPED('RS',PR1,FOUND,LCLSDE)
            CALL DMPS(0,3)
            CALL DGSWPT(-5)
          END IF
          GO TO 930
        END IF
      END IF
      CALL DO_STR('AD')
      IF(TANSW.EQ.'AD') THEN
        CALL DJADBS
        GO TO 936
      END IF
      CALL DO_STR('D?')
      IF(TANSW.EQ.'D?') THEN
        CALL D_JDEB
        GO TO 936
      END IF
      IF(TANSW.EQ.'X?') THEN
C        CALL DWRT(TX1)
C        CALL DWRT(TX2)
C        CALL DWRT(TX3)
        CALL DO_TY_COMMAND_LIST('DJEV')
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 930
  940 CALL DDISPA
      GO TO 930
      END
CH
*DK DJ_RD_EPED
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++  DJ_RD_EPED
CH
      SUBROUTINE DJ_RD_EPED(TANSW,PR1,FOUND,LCLS)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C     IF(TANSW(2:2).EQ.'RS') THEN
C     IF(TANSW(2:2).EQ.'DS') THEN
C     IF(TANSW(2:2).EQ.'RN') THEN
C     IF(TANSW(2:2).EQ.'DN') THEN
C     IF(TANSW(2:2).EQ.'RB') THEN
C     IF(TANSW(2:2).EQ.'DB') THEN
C     IF(TANSW(2:2).EQ.'RW') THEN
C     IF(TANSW(2:2).EQ.'RF') THEN
C     IF(TANSW(2:2).EQ.'DF') THEN
C     IF(TANSW(2:2).EQ.'RL') THEN
C     IF(TANSW(2:2).EQ.'DL') THEN
C
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW
      CHARACTER *50 TERR
      CHARACTER *70 TCFLM
      DIMENSION PR1(4,*)
      LOGICAL FOUND
C     ........... EDIR was read before : get position of existing run/event
      IF(     TANSW(2:2).EQ.'S') THEN
        CALL DJPOSE(PR1(2,1),PR1(2,2),TCFLM,JPOS,LCLS,JCUR,TERR)
      ELSE IF(TANSW(2:2).EQ.'N') THEN
        CALL DJPOSN( 1,TCFLM,JPOS,LCLS,TERR)
      ELSE IF(TANSW(2:2).EQ.'B') THEN
        CALL DJPOSN(-1,TCFLM,JPOS,LCLS,TERR)
      ELSE IF(TANSW(2:2).EQ.'F'.OR.TANSW .EQ.'RW') THEN
        CALL DJPOSE(    0.,    0.,TCFLM,JPOS,LCLS,JCUR,TERR)
      ELSE IF(TANSW(2:2).EQ.'L') THEN
        CALL DJPOSE(99999.,99999.,TCFLM,JPOS,LCLS,JCUR,TERR)
      ELSE
        FOUND=.FALSE.
        RETURN
      END IF
      FOUND=.TRUE.
      IF(TERR(1:1).NE.' ') THEN
        CALL DWRT(TERR)
        CALL DWRT('Current event is still stored.')
        RETURN
      ELSE
        CALL DJ_RD_EPED_EV(TCFLM,JPOS)
      END IF
      CALL ABRUEV(IRUNDE(1),IEVTDE(1))
      PR1(2,1)=IRUNDE(1)
      PR1(2,2)=IEVTDE(1)
      IF(RNCLDU.EQ.0) THEN
        CALL DQCL(0)
        CALL DGCLWK
      END IF
      CALL DEVNEW
      CALL DEVSET(1)
      IF(TANSW(1:1).EQ.'D') CALL DDISPP
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DJ_OPEN_EPED
CH
      SUBROUTINE D_DD_JEDOP(TIN,IER)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    OPEN EPIO FILM DEFINED IN EDIR
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      LOGICAL FNOFI
      CHARACTER *(*) TIN
C     tin is the name of the EPIO file which is not used.
      CALL D_LKOPRD(0,' ',IRUN,IEVT,FNOFI,IER)
      IF(IER.NE.0) CALL DWR_PLATFORM_TEXT('S1')
      END
*DK DJ_RD_EPIO_OR_NATIVE
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++  DJ_RD_EPIO_OR_NATIVE
CH
      SUBROUTINE DJ_RD_EPIO_OR_NATIVE(TANSW,PR1,FOUND)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     IF(TANSW(2:2).EQ.'RS') THEN
C     IF(TANSW(2:2).EQ.'DS') THEN
C     IF(TANSW(2:2).EQ.'RN') THEN
C     IF(TANSW(2:2).EQ.'DN') THEN
C     IF(TANSW(2:2).EQ.'RW') THEN
C
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW,DT2
      DIMENSION PR1(4,*)
      LOGICAL FOUND
      DATA J3/3/,N5/5/
      IF(     TANSW(2:2).EQ.'S') THEN
        CALL DWRT('-------------------- WAIT --------------------')
        IRUN=PR1(2,1)
        IEVT=PR1(2,2)
        CALL D_LKGTEVT(IRUN,IEVT,IER,'DJNAEP 1')
      ELSE IF(TANSW(2:2).EQ.'N') THEN
        CALL D_LKGTEVT(0,0,IER,'DJNAEP 2')
      ELSE IF(TANSW.EQ.'RW') THEN
        CALL DWRT('-------------------- WAIT --------------------')
        CALL D_BRWND
        DO J=1,J3
          CALL D_LKGTEVT(IR,IE,IER,'DJNAEP 3')
          CALL ABRUEV(IR,IE)
          IF(IR.GT.0) GO TO 80
          IF(IER.NE.0) THEN
            CALL DWRT('ERROR: Try to open the file again.')
            FOUND=.TRUE.
            RETURN
          END IF
        END DO
   80   IER=0
      ELSE
        FOUND=.FALSE.
        RETURN
      END IF
      FOUND=.TRUE.
      CALL ABRUEV(IRUNDE(1),IEVTDE(1))
      IF(IER.EQ.N5) THEN
        CALL DWRT('---------------------------------------------')
        CALL DWRT('FILE CLOSED = NO FILE OPEN !!!')
        CALL DWRT('If you want to continue, OPEN FILE: "GT:FI".')
        CALL DWRT('---------------------------------------------')
        IRUNDE(1)=0
        IEVTDE(1)=0
      ELSE IF(IER.NE.0) THEN
        CALL DWRT(' LKGTEVT error :'//DT2(FLOAT(IER)))
      END IF
      PR1(2,1)=IRUNDE(1)
      PR1(2,2)=IEVTDE(1)
      IF(RNCLDU.EQ.0) THEN
        CALL DQCL(0)
        CALL DGCLWK
      END IF
      CALL DEVNEW
      CALL DEVSET(1)
      IF(TANSW(1:1).EQ.'D'.AND.IER.EQ.0) CALL DDISPP
      END
C ---------------------------------------------------------------------
*DK DJLIED
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DJLIED
CH
      SUBROUTINE DJLIED(PR1,FCL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      PARAMETER (MAXEV=30000,MAXRU=15000,MAXEV2=15001,MAXFL=100)
      COMMON /DEDIR1/ NEVT(MAXEV),LCLS(MAXEV)
      COMMON /DEDIR2/ JTOT,JCUR,JCURL,KRUN,NCLS,FCLS,FOR
      COMMON /DEDIRT/ TPOS(MAXEV),TFILM(MAXFL)
      CHARACTER *8 TPOS
      LOGICAL FCHG,FCLS,FOR
      DIMENSION NRUN(MAXRU),NFILM(MAXRU)
      CHARACTER *69 TFILM
      EQUIVALENCE (NEVT(MAXEV2),NRUN),(LCLS(MAXEV2),NFILM)
      DATA FOR/.TRUE./
C
      CHARACTER *2 TANSW
      CHARACTER *35 T
      CHARACTER *2 TMP(-1:1)
      CHARACTER *42 TSET
      LOGICAL FDUM,FCL,FIN
      DATA TMP/'|-','|:','|+'/
C     DATA NTIM/7/
      DATA NDO/1/
      PARAMETER (MPR2=3)
      DIMENSION PR1(4,*),PR2(4,MPR2)
      CHARACTER *2 TPR2(MPR2)
      DATA TPR2/'LI','SL','NL'/
      DATA PR2/1.,1.,99999.,0.,
     &         1.,1.,99999.,0.,
     &         1.,7.,   99.,0./
      CALL DWRT_SETUP('LOGFILE=OFF')
      CALL DOPERS(2,2,0.)
      FIN=.TRUE.
      JLIN=MAX(1,JCURL)
      IF(JLIN.LT.JTOT) THEN
        NDO=1
      ELSE
        NDO=-1
      END IF
      CALL DWR_LINE(14,'----')
    1 IF(FCL) THEN
        CALL DJCLTX(TSET,FDUM)
        WRITE(TXTADW,1011) 'selected class'//TSET,TMP(NDO)
        CALL DWRC
      END IF
      WRITE(TXTADW,1011)
     &  '  Run| Event|   CLASS   1111111111 2222222222 33   Line'
     &  ,TMP(NDO)
      CALL DWRC
      GO TO 940
C---------------------------------------------------------------------------
  936 FIN=.FALSE.
      FCHG=.FALSE.
      PR2(2,1)=-1.
      PR2(2,2)=-1.
      PR23=PR2(2,3)
      CALL DOPER(1,0,
     &  1,0,' ',0,
     &  1,MPR2,TPR2,PR2,
     1  NEXEC,FCHG,TANSW)
C     .............................. PRIORITY: GT > NL > SL > LI > others
      IF(NEXEC.EQ.1) GO TO 910
C     ................................................................ NL
      IF(PR2(2,3).NE.PR23) GO TO 940
C     ................................................................ SL
      IF(PR2(2,2).GE.1.) THEN
        JLIN=IFIX(PR2(2,2))-NDO*(NTIM/2)
        JLIN=MAX(1,MIN(JTOT,JLIN))
        GO TO 940
      END IF
C     ................................................................ LI
      IF(PR2(2,1).GE.1.) THEN
        JCURL=PR2(2,1)
        JCURL=MIN(JTOT,JCURL)
        IF(KRUN.EQ.0) THEN
          PR1(2,1)=NRUN(JCURL)
        ELSE
          PR1(2,1)=KRUN
        END IF
        PR1(2,2)=NEVT(JCURL)
        GO TO 910
      END IF
      GO TO (910,920,910,940) NEXEC
  910 CALL DWR_LINE(14,'----')
      CALL DOPERS(1,0,0.)
      CALL DWRT_SETUP('LOGFILE=ON')
      RETURN
  920 IF(TANSW.EQ.'LE') THEN
        FCL=.FALSE.
        GO TO 1
      END IF
      IF(TANSW.EQ.'LC') THEN
        FCL=.TRUE.
        GO TO 1
      END IF
      IF(TANSW.EQ.'HD') THEN
        IF(FCL) WRITE(TXTADW,1011) 'selected class'//TSET
     &    ,TMP(NDO)
        CALL DWRC
        WRITE(TXTADW,1011)
     &    '  Run| Event|   CLASS   1111111111 2222222222 33   Line'
     &    ,TMP(NDO)
        CALL DWRC
        GO TO 936
      END IF
      IF(TANSW.EQ.'BB') THEN
        NDO=-1
        GO TO 940
      END IF
      IF(TANSW.EQ.'FF') THEN
        NDO= 1
        GO TO 940
      END IF
      IF(TANSW.EQ.'FE') THEN
        JLIN=1
        NDO=1
        GO TO 940
      END IF
      IF(TANSW.EQ.'LE') THEN
        JLIN=JTOT
        NDO=-1
        GO TO 940
      END IF
  929 WRITE(TXTADW,1011) 'Wrong command.'
     &    ,TMP(NDO)
      CALL DWRC
      GO TO 936
  940 NTIM=PR2(2,3)
      JTIM=0
      DO K=1,JTOT
        IF(FCL) THEN
          JCLS=LCLS(JLIN)
          KCLS=IAND(JCLS,NCLS)
          IF(FOR) THEN
            IF(KCLS.EQ.0) GO TO 943
          ELSE
            IF(KCLS.NE.NCLS) GO TO 943
          END IF
          JLAST=JLIN
        END IF
        CALL DJCLEV(LCLS(JLIN),T)
        IF(KRUN.EQ.0) THEN
          WRITE(TXTADW,1010) NRUN(JLIN),NEVT(JLIN),T,JLIN,TMP(NDO)
          CALL DWRC
        ELSE
          WRITE(TXTADW,1010) KRUN,NEVT(JLIN),T,JLIN,TMP(NDO)
          CALL DWRC
        END IF
        JTIM=JTIM+1
  943   JLIN=JLIN+NDO
        IF(.NOT.FIN) THEN
          IF(NDO.EQ.1.AND.JLIN.GT.JTOT) THEN
            JLIN=JTOT
            IF(FCL) THEN
              WRITE(TXTADW,1011) 'Last record of selected class',
     &          TMP(NDO)
              CALL DWRC
            ELSE
              WRITE(TXTADW,1011) 'Last record',TMP(NDO)
              CALL DWRC
            END IF
            GO TO 936
          END IF
          IF(NDO.EQ.-1.AND.JLIN.LT.1) THEN
            JLIN=1
            IF(FCL) THEN
              WRITE(TXTADW,1011) 'First record of selected class',
     &          TMP(NDO)
              CALL DWRC
            ELSE
              WRITE(TXTADW,1011) 'First record',TMP(NDO)
              CALL DWRC
            END IF
            GO TO 936
          END IF
        END IF
        IF(JTIM.GE.NTIM) GO TO 936
      END DO
      IF(JTIM.EQ.0.AND.FIN) THEN
        IF(NDO.EQ.1) THEN
          NDO=-1
          JLIN=MAX(1,JCURL)
          GO TO 940
        ELSE
          WRITE(TXTADW,1011) 'No events of selected class.',TMP(NDO)
          CALL DWRC
          GO TO 910
        END IF
      END IF
      GO TO 936
 1010 FORMAT(I5,'|',I6,'| ',A,I6,A)
 1011 FORMAT(A,T56,A)
      END
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DJCLIN
CH
      SUBROUTINE DJCLIN(PR)
CH
CH --------------------------------------------------------------------
CH
C     .................................................................
      PARAMETER (MAXEV=30000,MAXRU=15000,MAXEV2=15001,MAXFL=100)
      COMMON /DEDIR1/ NEVT(MAXEV),LCLS(MAXEV)
      COMMON /DEDIR2/ JTOT,JCUR,JCURL,KRUN,NCLS,FCLS,FOR
      COMMON /DEDIRT/ TPOS(MAXEV),TFILM(MAXFL)
      CHARACTER *8 TPOS
      LOGICAL FCLS,FOR
      DIMENSION NRUN(MAXRU),NFILM(MAXRU)
      CHARACTER *69 TFILM
      EQUIVALENCE (NEVT(MAXEV2),NRUN),(LCLS(MAXEV2),NFILM)
C     .................................................................
      CHARACTER *(*) T,TAO
      DIMENSION PR(4,32)
      LOGICAL FCL
      CHARACTER *1 TNUM(0:9)
      DATA TNUM /'0','1','2','3','4','5','6','7','8','9'/
      CHARACTER *39 TSET
C                123456789 123456789 123456789 123456789
C                          1111111111 2222222222 333
C                123456789 0123456789 0123456789 012 AND
      CHARACTER *35 TP
      DATA TP  /'......... __________ .......... __ '/
      DATA TSET/'......... __________ .......... __  OR '/
      IF(PR(2,1).GE.0.) THEN
        FCLS=.FALSE.
        NCLS=0.
        TSET(1:35)=TP
      END IF
      DO K=1,33
        ICLS=PR(2,K)
        IF(ICLS.GT.0.AND.ICLS.LE.32) THEN
          IF(.NOT.FCLS) THEN
            NCLS=0
            FCLS=.TRUE.
          END IF
          NCLS=IOR(NCLS,2**(ICLS-1))
          MCLS=MOD(ICLS,10)
          NOFS=ICLS+ICLS/10
          TSET(NOFS:NOFS)=TNUM(MCLS)
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
CH --------------------------------------------------------------------  DJCLMO
CH
      ENTRY DJCL00
CH
CH --------------------------------------------------------------------
CH
      FCLS=.FALSE.
      NCLS=0.
      TSET(1:35)=TP
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DJCLMO
CH
      ENTRY DJCLMO(TAO)
CH
CH --------------------------------------------------------------------
CH
      FOR=.FALSE.
      TSET(37:39)=TAO
      IF(TAO.EQ.'AND') THEN
        FOR=.FALSE.
      ELSE
        FOR=.TRUE.
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DJCLTX
CH
      ENTRY DJCLTX(T,FCL)
CH
CH --------------------------------------------------------------------
CH
      T=TSET
      FCL=FCLS
      END
*DK DJCLEV
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DJCLEV
CH
      SUBROUTINE DJCLEV(ICLS,T)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
      CHARACTER *(*) T
      CHARACTER *1 TP,T1(4)
      DATA T1/'.','_','.','_'/
      JCLS=ICLS
C          123456789 123456789 123456789 1234
      T=  '123456789 0123456789 0123456789 01'
      L=1
      M=1
      TP=T1(M)
      DO K=1,31
        IF(K.EQ.10.OR.K.EQ.20.OR.K.EQ.30) THEN
          L=L+1
          M=M+1
          TP=T1(M)
        END IF
        IF(MOD(JCLS,2).EQ.0) T(L:L)=TP
        JCLS=JCLS/2
        L=L+1
      END DO
      END
*DK DJ_RD_EVENT_FROM_EPIO
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DJ_RD_EPED_EV
CH
      SUBROUTINE DJ_RD_EPED_EV(TFILM,IPOS)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TFILM
      CHARACTER *60 TFN,TFT*6,TFD*40
      IER=0
C     .............................................. file not yet open
      IF(FNOFDF) THEN
        LLEN = MIN(80,LENOCC(TFILM))
        TXTADW='Open '//TFILM(1:LLEN)
        CALL DWRC
        CALL DJ_INQUIRE(TFILM,LLEN,TFN,TFT,TFD,IER)
        IF(IER.NE.0) THEN
          CALL DWRT('File not open.')
          CALL DWR_PLATFORM_TEXT('S1')
          RETURN
        END IF
        CALL DPARGI(81,'RLK',IRLK)
        IF(IRLK.EQ.1) THEN 
          CALL D_LKOPRD(0,' ',IRUN,IEVT,FNOFI,IER)
        ELSE
          CALL D_OPREC(TFN,TFT,TFD,IER)
        END IF
        IF(IER.NE.0) THEN
          CALL DWRT('File not open.')
          RETURN
        END IF
      END IF
      CALL D_ARDIR(IRLK,IPOS,IER)
      IF(IER.NE.0.AND.IER.NE.1) THEN
C       .... when going back to read the first event 'RF', one gets ier=1,
C       .... even if reading was o.k.
        WRITE(TXTADW,3335) IER
 3335   FORMAT('DALI:djrdep  Read error ',I8,' on EPIO file')
        CALL DWRC
        GO TO 98
      END IF
      RETURN
   98 LFILM=LENOCC(TFILM)
      CALL DWRT(TFILM(1:48))
      IF(LFILM.GT.48) CALL DWRT(TFILM(49:LFILM))
      END
*DK DJLSFN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DJLSFN
CH
      SUBROUTINE DJLSFN(TM,FOUND)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C     LIST EDIR-FILENAMES FOR A LIST OF RUNS FROM RUNCARTS.LIST
C     TM: CE      CL    CT    CS      CC         CD             CF
C          Enter, List, Type, Store , Clear all, Delet 1 entry , Copy file
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TM
      PARAMETER (MENTR=199)
      CHARACTER *1 T1(MENTR)
      CHARACTER *1 TUD
      DATA TUD/''''/
      CHARACTER *9 TD(MENTR)
      CHARACTER *6 TCARD(4)
      DATA TCARD/'COPY  ',
     &           'DEBU 0',
     &           'NSEQ  ',
     &           'ENDQ  '/
      CHARACTER *2 TF(6)
      DATA TF/'RA','PO','DS','MI','NA','| '/
      DIMENSION IRUN(MENTR),IEVT(MENTR)
      DATA N/0/
      LOGICAL FOUND,FRET
      CHARACTER *24 TBOOK
      CALL DW_GET_PLATFORM_TEXT('BR',TBOOK,24)
      LBOOK=LENOCC(TBOOK)
      FOUND=.TRUE.
C     .......................................... Enter event into Cards file
      IF(TM.EQ.'CE') THEN
        IF(N.GE.MENTR) THEN
          CALL DWRT('No more events are accepted in the C-list.')
          RETURN
        END IF
        CALL DJLSCE(N,IRUNDE(2),IEVTDE(2),IRUN,IEVT,FRET)
        IF(FRET) RETURN
        WRITE(TXTADW,1002) IRUNDE(2),IEVTDE(2),' entered into C-list.'
 1002   FORMAT('   RU=',I5,'   NU=',I5,2X,A)
        CALL DWRC
C     ...................................................... enter SEVT list
      ELSE IF(TM.EQ.'CF') THEN
        N=0
        CALL DWRT('C-list cleared.')
        CALL DJ_SEVT_IN(NUMR)
        DO K=1,NUMR
          CALL DJ_SEVT_RUN(K,NRUN,NUME)
          DO I=1,NUME
            CALL DJ_SEVT_EV(I,NEV)
            IF(N.GE.MENTR) THEN
              CALL DWRT('No more entries accepted.')
              GO TO 9
            END IF
            CALL DJLSCE(N,NRUN,NEV,IRUN,IEVT,FRET)
          END DO
        END DO
    9   CALL DJ_SEVT_END
        WRITE(TXTADW,1009) N
 1009   FORMAT(I3,' entries in the C-list from DSEVT.CARDS')
        CALL DWRC
C     ...................................................... Clear Cards file
      ELSE IF(TM.EQ.'CC') THEN
        IF(N.GT.0) THEN
          CALL DTYANS('Clear C-list ?  Y, NO=<CR>','Y',NANSW)
          IF(NANSW.EQ.1) THEN
            N=0
            CALL DWRT('C-list is empty.')
          ELSE
            CALL DWRT('C-LIST not cleared.')
          END IF
        ELSE
          CALL DWRT('C-list already empty.')
        END IF
C     ............................................. Delet event in Cards file
      ELSE IF(TM.EQ.'CD') THEN
        L=0
        DO I=1,N
          IF(IRUN(I).EQ.IRUNDE(2).AND.IEVT(I).EQ.IEVTDE(2)) THEN
            WRITE(TXTADW,1002) IRUN(I),IEVT(I),' deleted in the C-list.'
            CALL DWRC
          ELSE
            L=L+1
            IRUN(L)=IRUN(I)
            IEVT(L)=IEVT(I)
          END IF
        END DO
        IF(N.EQ.L) CALL DWRT('                    not in the C-list.')
        N=L
C     ........................................... Type list without file name
      ELSE IF(TM.EQ.'CT') THEN
        IF(N.EQ.0) THEN
          CALL DWRT('C-list is empty.')
        ELSE
          DO I=1,N
            WRITE(TXTADW,1000) I,IRUN(I),IEVT(I)
            CALL DWRC
          END DO
        END IF
C     ...................................................... List Cards file
      ELSE IF(TM.EQ.'CL'.OR.TM.EQ.'CS') THEN
        IF(N.EQ.0) THEN
          CALL DWRT('C-list is empty.')
        ELSE
          CALL DGOPEN(NUNIDU,TBOOK(1:LBOOK),2,*99,ISTAT)
          CALL DTYANS('Raw, Pot, Dst, Mini, Nano = ','RPDMN',NANSW)
          IF(NANSW.LE.0) RETURN
          DO K=1,MENTR
            TD(K)=' '
            T1(K)=' '
          END DO
          I=1
          READ(NUNIDU,1001,END=19) TXTADW
          READ(NUNIDU,1001,END=19) TXTADW
          LR2=INDEX(TXTADW,'RAW')-2
          LD1=INDEX(TXTADW,TF(NANSW))
          LD2=INDEX(TXTADW,TF(NANSW+1))-2
   11     READ(NUNIDU,1001,END=19) TXTADW
 1001     FORMAT(A)
          READ(TXTADW(1:LR2),1011,ERR=11) NRUN
 1011     FORMAT(I7)
   10     IF(NRUN-IRUN(I)) 11,12,13
   12     TD(I)=TXTADW(LD1:LD2)
          IF(I.GT.1) THEN
            DO K=1,I-1
              IF(TD(I).EQ.TD(K)) THEN
                T1(I)='*'
                GO TO 13
              END IF
            END DO
          END IF
   13     IF(I.GE.N) GO TO 19
          I=I+1
          GO TO 10
   19     CLOSE(UNIT=NUNIDU)
          DO I=1,N
            WRITE(TXTADW,1000) I,IRUN(I),IEVT(I),TD(I),T1(I)
C                  123456789 123456789 123456789 123456
C                  123  Run=12345  event=12345  AB1234+
 1000       FORMAT(I2,' RU=',I5,'   NU=',I5,2X,2A)
            CALL DWRC
          END DO
        END IF
        IF(TM.EQ.'CS') THEN
C       ........................................ Store events into Cards file
          OPEN(UNIT=NUNIDU,FILE='DALPHA.CARDS',STATUS='UNKNOWN',ERR=90)
          CALL DW_GET_PLATFORM_TEXT('ED',TXTADW,24)
          LEDR=LENOCC(TXTADW(1:24))
          DO I=1,N
            IF(T1(I).EQ.' '.AND.TD(I).NE.' ') THEN
              LTD=LENOCC(TD(I))
              WRITE(NUNIDU,2000) ' FILI ',TUD,TXTADW(1:LEDR),
     &          TD(I)(1:LTD),'.EDIR',TUD
 2000         FORMAT(6A)
            END IF
          END DO
          WRITE(NUNIDU,2000) ' FILO ',TUD,'DALPHA.EPIO',TUD
          WRITE(NUNIDU,2002) TCARD(1)
 2002     FORMAT(1X,A)
          DO I=1,N
            IF(TD(I).NE.' ') THEN
              WRITE(NUNIDU,2003) IRUN(I),IEVT(I)
 2003         FORMAT(' SEVT',2I6)
            END IF
          END DO
          DO K=2,4
            WRITE(NUNIDU,2002) TCARD(K)
          END DO
          CLOSE(UNIT=NUNIDU)
          OPEN(UNIT=NUNIDU,FILE='DALPHA.CARDS',STATUS='OLD',ERR=90)
          DO K=1,203
            READ(NUNIDU,3000,END=29) TXTADW
 3000       FORMAT(A)
            CALL DWRC
          END DO
   29     CLOSE(UNIT=NUNIDU)
          CALL DWR_PLATFORM_TEXT('AL')
        END IF
      ELSE
        FOUND=.FALSE.
      END IF
      RETURN
   99 CALL DWRT(TBOOK(1:LBOOK)//' not found.')
      RETURN
   90 CALL DWRT('DALPHA.CARDS cannot be opened. In use ?')
      END
*DK DJLSCE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DJLSCE
CH
      SUBROUTINE DJLSCE(N,IR,IE,IRUN,IEVT,FRET)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DIMENSION IRUN(*),IEVT(*)
      LOGICAL FRET
      FRET=.TRUE.
      IF(N.EQ.0) THEN
        N=1
        IRUN(N)=IR
        IEVT(N)=IE
      ELSE
        DO I=N,1,-1
          IF(IR.EQ.IRUN(I).AND.IE.EQ.IEVT(I)) THEN
            WRITE(TXTADW,1002) IRUN(I),IEVT(I),
     &        ' already in the C-list.'
 1002       FORMAT('   RU=',I5,'   NU=',I5,2X,A)
            CALL DWRC
            RETURN
          END IF
          IF(IR.GT.IRUN(I)) GO TO 3
          IF(IR.EQ.IRUN(I).AND.IE.GT.IEVT(I)) GO TO 3
        END DO
    3   IF(I.EQ.N) THEN
          N=N+1
          IRUN(N)=IR
          IEVT(N)=IE
        ELSE
          I=I+1
          DO L=N,I,-1
            IRUN(L+1)=IRUN(L)
            IEVT(L+1)=IEVT(L)
          END DO
          N=N+1
          IRUN(I)=IR
          IEVT(I)=IE
        END IF
      END IF
      FRET=.FALSE.
      END
*DK DJSTR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DJSTR
CH
      SUBROUTINE DJSTR
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    : IR = RUN# IE=EVENT #
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      DIMENSION IRSTR(6),IESTR(6)
      IF(IRUNDE(1).EQ.IRSTR(6).AND.IEVTDE(1).EQ.IESTR(6)) RETURN
      DO N=1,5
        IRSTR(N)=IRSTR(N+1)
        IESTR(N)=IESTR(N+1)
      END DO
      IRSTR(6)=IRUNDE(1)
      IESTR(6)=IEVTDE(1)
      RETURN
CH
CH
CH
      ENTRY DJSTT
CH
CH
CH
      WRITE(TXTADW,1000) 'Ru',IRSTR
      CALL DWRC
      WRITE(TXTADW,1000) 'Ev',IESTR
      CALL DWRC
 1000 FORMAT(A,6I8)
      END
*DK DJPOSE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DJPOSE
CH
      SUBROUTINE DJPOSE(RU,EV,TCFLM,JPOS,ICLS,JE,TERR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C GET POSITION OF EVENT IN THE EPIO-FILE FROM EDIR
C IF RUN   IS NOT FOUND NEXT RUN   IS SEARCHED FOR EXCLUDING CURRENT ONE.
C IF EVENT IS NOT FOUND NEXT EVENT IS SEARCHED FOR EXCLUDING CURRENT ONE.
C NEV=0:     FIRST EVENT OF THIS RUN
C NEV=99999: LAST  EVENT OF THIS RUN
C NRU=NRU+1, NEV=0 : FIRST EVENT ON NEXT RUN
C NRU=NRU-1, NEV=0     : FIRST EVENT ON NEXT RUN
C NRU=NRU-1, NEV=99999 : LAST  EVENT ON NEXT RUN
C NRU=0    ,NEV=0    : FIRST EVENT ON EDIR
C NRU=99999,NEV=99999: LAST  EVENT ON EDIR
C CLASS IS NOT USED, BUT WRONG CLASS IS FLAGGED.
C IT IS ASSUMED THAT ALL EVENTS OF A SINGLE RUN ARE GROUPED TOGETHER

C      TERR='NORU: No other run.')
C      TERR='NOEV: No other event.')
C      TERR='CUEV: This is the current event.'
C      TERR='FIEV: First event.'
C      TERR('LAEV: Last event.'
C      TERR='ERED! Error in EDIR file. Cannot read event.'
C
C      TERR='RUNA: Run not accepted.'
C      TERR='EVNA: Event not accepted.'

C      TERR=' '    New event is found.
C ---------------------------------------------------------------------
C     .................................................................
      PARAMETER (MAXEV=30000,MAXRU=15000,MAXEV2=15001,MAXFL=100)
      CHARACTER *(*) TCFLM,TERR
      COMMON /DEDIR1/ NEVT(MAXEV),LCLS(MAXEV)
      COMMON /DEDIR2/ JTOT,JCUR,JCURL,KRUN,NCLS,FCLS,FOR
      COMMON /DEDIRT/ TPOS(MAXEV),TFILM(MAXFL)
      CHARACTER *8 TPOS
      LOGICAL FCLS,FOR
      DIMENSION NRUN(MAXRU),NFILM(MAXRU)
      CHARACTER *69 TFILM
      EQUIVALENCE (NEVT(MAXEV2),NRUN),(LCLS(MAXEV2),NFILM)
C     .................................................................
      CHARACTER *18 TCS
      IRU=RU
      IEV=EV
      JR=JCUR
      IF(KRUN.EQ.0.AND.(JCUR.EQ.0.OR.IRU.NE.NRUN(JCUR))) THEN
        JDO=MAX(1,JCUR)
        DO JR=JDO,JTOT
          IF(IRU.EQ.NRUN(JR)) GO TO 10
        END DO
        DO JR=JDO,1,-1
          IF(IRU.EQ.NRUN(JR)) GO TO 10
        END DO
        ID=999999
        DO J=1,JTOT
          MD=ABS(NRUN(J)-IRU)
          IF(MD.NE.0.AND.MD.LT.ID) THEN
            JR=J
            ID=MD
          END IF
        END DO
        IF(ID.EQ.999999) THEN
          TERR='NORU: No other run.'
          RETURN
        END IF
        IF(IRU.NE.0.AND.IRU.NE.99999.AND.
     &     IEV.NE.0.AND.IEV.NE.99999) THEN
C               123456789 123456789 123456789 123456789 123456789
          TERR='Run not found. Select nearest run 12345 ? Y/N'
          WRITE(TERR(35:39),1000) NRUN(JR)
 1000     FORMAT(I5)
          CALL DTYANS(TERR,'Y',NANSW)
          IF(NANSW.NE.1) THEN
            TERR='RUNA: Run not accepted.'
            RETURN
          END IF
          TERR=' '
        END IF
   10   IRU=NRUN(JR)
      END IF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(IEV.EQ.NEVT(JR)) THEN
        JE=JR
      ELSE
        ID=999999
        DO JE=JR,JTOT
          IF(KRUN.EQ.0.AND.IRU.NE.NRUN(JE)) GO TO 11
          IF(IEV.EQ.NEVT(JE)) GO TO 20
          MD=ABS(NEVT(JE)-IEV)
          IF(MD.NE.0.AND.MD.LT.ID) THEN
            JJ=JE
            ID=MD
          END IF
        END DO
   11   DO JE=JR,1,-1
          IF(KRUN.EQ.0.AND.IRU.NE.NRUN(JE)) GO TO 12
          IF(IEV.EQ.NEVT(JE)) GO TO 20
          MD=ABS(NEVT(JE)-IEV)
          IF(MD.NE.0.AND.MD.LT.ID) THEN
            JJ=JE
            ID=MD
          END IF
        END DO
   12   IF(ID.EQ.999999) THEN
          TERR='NOEV: No other event.'
          IRET=2
          RETURN
        END IF
        JE=JJ
        IF(IRU.NE.0.AND.IRU.NE.99999.AND.
     &     IEV.NE.0.AND.IEV.NE.99999) THEN
C               123456789 123456789 123456789 123456789 123456789
          TERR='Event not found. Select nearest event 12345 ? Y/N'
          WRITE(TERR(39:43),1000) NEVT(JE)
          CALL DTYANS(TERR,'Y',NANSW)
          IF(NANSW.NE.1) THEN
            TERR='EVNA: Event not accepted.'
            RETURN
          END IF
        END IF
        TERR=' '
      END IF
C   20 IF(JE.EQ.JCUR) THEN
C        TERR='CUEV: This is the current event.'
C      ELSE
   20 LPOS=LENOCC(TPOS(JE))
      READ(TPOS(JE)(1:LPOS),3000,ERR=999) JPOS
 3000 FORMAT(I8)
      IF(KRUN.EQ.0) THEN
        TCFLM=TFILM(NFILM(JE))
      ELSE
        TCFLM=TFILM(1)
      END IF
      ICLS=LCLS(JE)
      JCUR=JE
      TERR=' '
C     END IF
      RETURN
  999 TERR='ERED! Error in EDIR file. Cannot read event.'
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DJPOSN
CH
      ENTRY DJPOSN(IDIR,TCFLM,JPOS,ICLS,TERR)
CH
CH --------------------------------------------------------------------
CH
C IDIR= 1 : NEXT EVENT FORWARD
C IDIR=-1 : NEXT EVENT BACKWARD
C IF NO MORE EVENT: IRET=1
      TCS='.'
      JC=JCUR
  100 JC=JC+IDIR
      IF(JC.LT.1) THEN
        TERR='FIEV: First event.'
        TERR='NOLE: No lower event'//TCS
        RETURN
      END IF
      IF(JC.GT.JTOT) THEN
        TERR='LAEV: Last event.'
        TERR='NOHE: No higher event'//TCS
        RETURN
      END IF
      IF(FCLS) THEN
        TCS=' of selected class.'
        KCLS=IAND(NCLS,LCLS(JC))
        IF(FOR) THEN
          IF(KCLS.EQ.0) GO TO 100
        ELSE
          IF(KCLS.NE.NCLS) GO TO 100
        END IF
      END IF
      JCUR=JC
      ICLS=LCLS(JCUR)
      LPOS=LENOCC(TPOS(JCUR))
      READ(TPOS(JCUR)(1:LPOS),3000,ERR=999) JPOS
      IF(KRUN.EQ.0) THEN
        TCFLM=TFILM(NFILM(JCUR))
      ELSE
        TCFLM=TFILM(1)
      END IF
      TERR=' '
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------  DJ_GET_RUN_EVENT
CH
      ENTRY DJ_GET_RUN_EVENT(NEWR,NEWE,NCUR)
CH
CH --------------------------------------------------------------------
CH
      IF(KRUN.EQ.0) THEN
        NEWR=NRUN(JCUR)
      ELSE
        NEWR=KRUN
      END IF
      NEWE=NEVT(JCUR)
      NCUR=JCUR
      END
*DK DJINPE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DJINPE
CH
      SUBROUTINE DJINPE(TANSW,RUN,EVT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      PARAMETER (MAXE=200)
      CHARACTER *(*) TANSW
      CHARACTER *3 DT3
      CHARACTER *60 TL(MAXE),T
      DIMENSION NRE(MAXE)
      DATA IREL/0/,NEV/0/
      IRUN=RUN
      IEVT=EVT
      IF(IRUN.EQ.0.OR.NEV.EQ.0) THEN
        CALL DGOPEN(NUNIDU,TFILDC//'IN_EV',2,*99,ISTAT)
        DO NEV=1,MAXE
    1     READ(NUNIDU,1000,END=9,ERR=99) IR,IE,T
 1000     FORMAT(I7,1X,I6,2X,A)
          IF(IR.EQ.0) GO TO 1
          IRE=10000*IR+IE
          IF(IRE.EQ.IREL) GO TO 1
          IREL=IRE
          NRE(NEV)=IRE
          TL(NEV)=T
        END DO
    9   CLOSE(UNIT=NUNIDU)
        CALL DWRT(DT3(FLOAT(NEV))//' events in the list.')
        IF(NEV.EQ.0) RETURN
        IEVT=0
        MD=0
      END IF
      IF(TANSW.EQ.'IS') THEN
        JDL=99999999
        IRE=IRUN*10000+IEVT
        DO N=1,NEV
          JD=NRE(N)-IRE
          IF(JD.GT.0.AND.JD.LT.JDL) THEN
            JDL=JD
            MD=N
          END IF
        END DO
      ELSE
        MD=MD+1
        IF(MD.GT.NEV) THEN
          CALL DWRT('Start list again.')
          MD=1
        END IF
        JDL=0
      END IF
      IF(JDL.NE.99999999) THEN
        RUN=NRE(MD)/10000
        EVT=MOD(NRE(MD),10000)
        LL=LENOCC(TL(MD))
        DO L=1,LL
          IF(TL(MD)(L:L).EQ.'!') THEN
            CALL DWRT(TL(MD)(L+1:LL))
            GO TO 3
          END IF
        END DO
      ELSE
        CALL DWRT('No more event.')
      END IF
    3 RETURN
   99 CALL DWRT('Generate '//TFILDC//'IN_EV in the following format',
     &  'TL',':')
      CALL DWRT(' 23544, 101,   ! Comment')
      END
*DK DJ_CHANGE_NAME
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++ DJ_CHANGE_NAME
CH
      SUBROUTINE DJ_CHANGE_NAME(TS,LP,LT,T,*)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     the filename is modified according to LOOK:
C     A?1234.EPIO -> AL$DATA:A?1234.EPIO
C     A?1234.EDIR -> AL$EDIR:A?1234.EDIR
C     independent what is in front of A
C     
C     RETURN to alternate return if name is changed
C    
C     AL$DATA: is defined in DALI_vs.PLATFORM_TEXT with INDEX 'EP'
C     AL$EDIR: is defined in DALI_vs.PLATFORM_TEXT with INDEX 'ED'
C     the leading letter(s)         "              with INDEX 'L1'            
C
C     INPUT : TS = INDEX name
C              LP = POSITION OF "." in the name
C     INPUT,OUTPUT : LT = length of filename, T = filename         
C
C     ...............................................................
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) T,TS
      CHARACTER *5 TA,TL
      CHARACTER *20 TST
      CHARACTER *70 TIN
C      CALL DW_GET_PLATFORM_TEXT('L1',TA,5)
C      IF(TA.NE.' ') THEN
C        TL=TA
C        CALL CUTOL(TL)
C        LA=LENOCC(TA)
C        L1=LP-6
C        TIN=T
C        IF(T(L1:L1-1+LA).EQ.TA(1:LA).OR.
C     &     T(L1:L1-1+LA).EQ.TL(1:LA)) THEN
C          CALL DW_GET_PLATFORM_TEXT(TS,TST,20)
C          LS=LENOCC(TST)
C          L2=LENOCC(T)
C          T=TST(1:LS)//T(L1:L2)
      TIN=T
      CALL ALGTENV(TIN,T)
      IF(TIN.EQ.T) RETURN
      CALL DWRT('Filename modified following LOOK conventions!')
      CALL DWRT('Rename your file, if you do not want it.#') 
      LT=LENOCC(T)
      RETURN 1
      END
