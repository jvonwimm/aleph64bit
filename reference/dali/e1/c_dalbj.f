*DK D_JDEB
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  D_JDEB
CH
      SUBROUTINE D_JDEB
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      INCLUDE 'A_BCS.INC'
      CHARACTER *5 TFIL
      CHARACTER *6 TD
      DATA TD/'D_JDEB'/
      LOGICAL FCLOS
      DATA FCLOS/.TRUE./
      CALL DPARGV(81,'JDB',2,DEB2)
      LUNIT=DEB2
      IF(FCLOS) THEN
    9   CALL DTYANS('Open the debug-file? <CR>=no',' ',N)
        IF(N.EQ.0) RETURN
        TFIL=TXTADW(1:1)//'.FOR'
        CALL DGOPEN(LUNIT,TFIL,1,*9,ISTAT)
        FCLOS=.FALSE.
        CALL DWRT('Write to '//TFIL)
        CALL DPARSV(81,'JDB',4,1.)
        CALL DWRD( 'C     '//TFILDC,TD)
        CALL DWRD( 'C     @ALEPHREAD ',TD)
        CALL DWRD( '      PARAMETER (LBOS=2000000)',TD)
        CALL DWRD( '      COMMON /BCS/ IW(LBOS)',TD)
        CALL DWRD( '      CHARACTER *70,T0,T1,T2,T3,T4',TD)
        CALL DWRD( '      COMMON /DCFTVT/ TFILDC,TITLDC,TDEFDC',TD)
        CALL DWRD( '      CHARACTER*8 TFILDC',TD)
        CALL DWRDT('      TFILDC=',TFILDC,TD)
        CALL DWRD( '      CALL DJBOP(6)',TD)
        CALL DWRD( '      CALL MINFMT',TD)
        CALL DWRD( 'C     ',TD)
        CALL DPARSV(81,'JDB',4,1.)
        IW6=IW(6)
        IW(6)=6
        CALL LKSTOUT(6)
        CALL DWRT('Reset:  IW(6)=6') 
      ELSE
        IW(6)=IW6
        CALL LKSTOUT(IW6)
        CALL DWRD('      END','D_JDEB')
        LUNIT=DEB2
        CLOSE(UNIT=LUNIT)
        FCLOS=.TRUE.
        CALL DWRT(TFIL//' closed.')
        WRITE(TXTADW,1000) IW(6)
 1000   FORMAT('Type D? again to open the file. IW(6)=',I2)
        CALL DWRC
        CALL DPARSV(81,'JDB',4,-1.)
      END IF
      END
*DK DJ_INQUIRE
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DJ_INQUIRE
CH
      SUBROUTINE DJ_INQUIRE(TFILM,LFILM,TFN,TFT,TFD,IER)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Subroutine built after      
C     AL1$USER3:[RANJARD]RWALEPH.FOR;97 24-APR-1995 16:56:51.49  :
C     PROGRAM RWALEPH
C     ......
C     CHARACTER*80 STRING,FN*60,FT*6,FD*40   
C     ......
C
C - check DAFILM routine
C
C     STRING ='ALDATA | EPIO | CART AB5072.1.SL '  
C     STRING = 'AB6033.EPIO | GIME PUBXU 459'
C     CALL DAFILM (STRING,FN,FT,FD,IRET)
C     WRITE (6,*) ' STRING= ',STRING, ' FN= ',FN, ' FT= ',FT,
C    &            ' FD= ',FD
C     IF (FD(1:4).EQ.'CART' .OR. FD(1:4).EQ.'8MM ') THEN 
C        IST = ALSTGQRY (FD(5:))
C        WRITE(6,*) ' ALSTGQRY status = ',IST 
C         IF (IST .EQ.1)  CALL AOPEN (21,FN,FT,FD,IER)
C        IF (IER.NE.0) WRITE(6,*) ' Cannot open file IER=',ier
C     ENDIF
C      STOP
C
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TFILM,TFN,TFT,TFD
      INTEGER ALSTGQRY
      CALL DPARGV(81,'JDB',4,DEB)
      IF(DEB.EQ.1.) THEN
        CALL DWRDT('      T1 =',TFILM,'DJ_INQUIRE')
        CALL DWRD ('      CALL DAFILM(T1,T2,T3,T4,IRET)','D_INQUIRE')
      END IF
      I=INDEX(TFILM(1:LFILM),'SIZE')-1
      IF(I.LE.0) I=LFILM
      CALL DAFILM(TFILM(1:I),TFN,TFT,TFD,IRET)
      IF(DEB.EQ.1.) THEN
        CALL DWRDT('C     T2 =',TFN,'DJ_INQUIRE')
        CALL DWRDT('C     T3 =',TFT,'DJ_INQUIRE')
        CALL DWRDT('C     T4 =',TFD,'DJ_INQUIRE')
        CALL DWRDI('C     IRET =',IRET,'DJ_INQUIRE')
      END IF
      IER=0
C     ............................ CART on VMS, 8MM ON ??, SMCF on Unix.
      IF(TFD(1:4).EQ.'CART'.OR.
     &   TFD(1:4).EQ.'8MM '.OR.
     &   TFD(1:4).EQ.'SMCF') THEN 
        TXTADW='Inquire file '//TFD
        CALL DWRC
        IF(DEB.EQ.1.) THEN

CH
CH
CH
CH
CH          CALL DWRDT('      T1 =',TFD(5:),'DJ_INQUIRE')
          CALL DWRD ('      IST=ALSTGQRY(T1)','D_INQUIRE')
        END IF
C       ... ALSTGQRY - integer function to make a stagequery on VAX or UNIX
C       ............... IER = ALSTGQRY (fname)   i.e. - fname = ab1234.1.sl
C       .................. IER =0 if file is NOT staged, =1 if it is staged
        IST = ALSTGQRY (TFD(6:))
        IF(DEB.EQ.1.) CALL DWRDI('C     IST =',IST,'DJ_INQUIRE')
        IF(IST.NE.1) IER=1
      END IF
      END
*DK D_OPSEQ
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ D_OPSEQ
CH
      SUBROUTINE D_OPSEQ(T1,IER)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) T1
      IF(LUNEDB.GT.100) CALL D_ACLOSE(LUNEDB,IRCL)
      LUNEDB=MIN(999,LUNEDB+1)
      CALL DPARGV(81,'JDB',4,DEB)
      IF(DEB.EQ.1.) THEN
        CALL DWRDI('      LUNEDB=',LUNEDB,'D_OPSEQ')
        CALL DWRD ('      CALL NUNIAL(''INP'',4,LUNEDB)','D_OPSEQ')
        CALL DWRDT('      T1 =',T1,'D_OPSEQ')
        CALL DWRD ('      CALL DOPSEQ(LUNEDB,T1,IER)','D_OPSEQ')
      END IF
      CALL NUNIAL('INP',4,LUNEDB)
      CALL DOPSEQ(LUNEDB,T1,IER)
      CALL DWRDI(  'C     IER=',IER,'D_JSEQ')
      END
*DK D_OPREC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ D_OPREC
CH
      SUBROUTINE D_OPREC(TNAM,TTYP,TDEV,IER)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TNAM,TTYP,TDEV
      CALL DPARGV(81,'JDB',4,DEB)
      IF(LUNEDB.GT.100) CALL D_ACLOSE(LUNEDB,IRCL)
      CALL DPARGV(81,'JDB',4,DEB)
      LUNEDB=MIN(999,LUNEDB+1)
      IF(DEB.EQ.1.) THEN
        CALL DWRDI('      LUNEDB =',LUNEDB,'D_OPREC')
        CALL DWRD ('      CALL NUNIAL(''INP'',4,LUNEDB)','D_OPREC')
        CALL DWRDT('C     T1 =',TNAM ,'D_OPREC')
        CALL DWRDT('C     T2 =',TTYP ,'D_OPREC')
        CALL DWRDT('C     T3 =',TDEV ,'D_OPREC')
        CALL DWRDI('      LUNEDB=',LUNEDB,'D_OPREC')
        CALL DWRD ('      CALL DOPREC(LUNEDB,T1,T2,T3,IER)','D_OPREC')
      END IF
      CALL NUNIAL('INP',4,LUNEDB)
      CALL DOPREC(LUNEDB,TNAM,TTYP,TDEV,IER)
      CALL DWRDI(    'C     IER=',IER ,'D_OPREC')
      IF(IER.EQ.0) FNOFDF=.FALSE.
      END
*DK D_ARDIR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ D_ARDIR
CH
      SUBROUTINE D_ARDIR(IRLK,IREC,IER)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *16 T1
      CHARACTER *12 T2
      CHARACTER *5 TNSEQ
      DATA T1/'/RUN '/,T2/' '/,TNSEQ/'/NSEQ'/
      CALL DPARGV(81,'JDB',4,DEB)
      IER=0
C     =====================================================================
      IF(IRLK.EQ.0) THEN
        IF(DEB.EQ.1.) THEN
          CALL DWRDI('      LUNEDB =',LUNEDB,'D_ARDIR')
          CALL DWRDI('      IREC=',IREC,'D_ARDIR')
          CALL DWRD ('      CALL DARDIR(LUNEDB,''AL '',IREC,IER)'
     &      ,'D_ARDIR')
        END IF
        CALL DARDIR(LUNEDB,'AL ',IREC,IER)
        CALL DWRDI(  'C     IER=',IER,'D_ARDIR')
      ELSE
C       =====================================================================
        CALL DPARGI(81,'RSQ',INSEQ)
        CALL DJ_GET_RUN_EVENT(IRUN,IEVT,ICUR)
        CALL ABRUEV(NR,NE)
        IF(DEB.EQ.1.) THEN
          CALL DWRD ('      CALL ABRUEV(NR,NE)','D_ARDIR')
          CALL DWRDI('C     NR=',NR,'D_ARDIR')
          CALL DWRDI('C     NE=',NE,'D_ARDIR')
        END IF
C       ......................................... READ RUN
        IF(IRUN.NE.NR) THEN
          T1(6:16)=' '
          CALL DTINT(IRUN,6,11,T1)
          IF(INSEQ.EQ.1) THEN
            LX=LENOCC(T1)
            T1(LX+1:LX+5)=TNSEQ
          END IF
          IF(DEB.EQ.1.) THEN
            CALL DWRDT('      T1 =',T1 ,'D_ARDIR')
            CALL DWRD ('      CALL LKRREC(T1)','D_ARDIR')
          END IF
          CALL LKRREC(T1)
          CALL ABRUEV(NR,NE)
          IF(DEB.EQ.1.) THEN
            CALL DWRD ('      CALL ABRUEV(NR,NE)','D_ARDIR')
            CALL DWRDI('C     NR=',NR,'D_ARDIR')
            CALL DWRDI('C     NE=',NE,'D_ARDIR')
          END IF
          IF(IRUN.NE.NR) GO TO 9
        END IF
        IF(IEVT.NE.NE) THEN
          IF(ICUR.EQ.1.AND.IEVT.LT.NE) THEN
            IF(DEB.EQ.1.) THEN
              CALL DWRD('      CALL LKRWND(''REWIND/EDIR'')','D_ARDIR')
            END IF
            CALL LKRWND('REWIND/EDIR')
          ELSE
            T2=' '
            CALL DTINT(IEVT,2,7,T2)
            IF(INSEQ.EQ.1) THEN
              LX=LENOCC(T2)
              T2(LX+1:LX+5)=TNSEQ
            END IF
            IF(DEB.EQ.1.) THEN
              CALL DWRDT('      T2 =',T2 ,'D_ARDIR')
              CALL DWRD ('      CALL LKRREC(T2)','D_ARDIR')
            END IF
            CALL LKRREC(T2)
          END IF
          CALL ABRUEV(NR,NE)
          IF(DEB.EQ.1.) THEN
            CALL DWRD ('      CALL ABRUEV(NR,NE)','D_ARDIR')
            CALL DWRDI('C     NR=',NR,'D_ARDIR')
            CALL DWRDI('C     NE=',NE,'D_ARDIR')
          END IF
          IF(IEVT.NE.NE) GO TO 9
        END IF
      END IF
      RETURN
    9 IER=1
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++  D_LKOPRD
CH
      SUBROUTINE D_LKOPRD(LFIL,TFIL,IRUN,IEVT,FNOFI,IER)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     OPEN EDIR, EPIO (=SL) OR NATIVE FILE
C     INPUTS: LFIL = length of file name,  
C       if LFIL=0  EDIR file TFIL not used but DJ_FILE_EDIR_NAME
C       if LFIL#0  EPIO or NATIVE assumed. TFIL = filename
C       Decision with Marco Cattaneo 6.12.1995:
C     
C               files *.NAT* and /NATIVE *.* are NATIVE
C               files *.EDIR are EDIR
C               all others are EPIO
C     
C     OUTPUT: FNOFI = TRUE if no filename
C             IRUN  = run    IEVT = event
C             IER   = error from LKOPRD.
C
      INCLUDE 'DALI_CF.INC'
      LOGICAL FNOFI
      CHARACTER *(*) TFIL
      CHARACTER *6 TY
      CHARACTER *70 TNAM
      IF(LFIL.EQ.0) THEN
        CALL DJ_FILE_EDIR_NAME(70,TNAM)
        LNAM=LENOCC(TNAM)
        TY='EDIR'
        LT=4
      ELSE
        TNAM=TFIL
        CALL CLTOU(TNAM)
        IF(     LFIL.GE.15.AND.TNAM(1:7).EQ.'/NATIVE') THEN
          DO L=8,15
            IF(TNAM(L:L).NE.' ') GO TO 2
          END DO
    2     TFIL=TFIL(L:LFIL)
          LFIl=LFIL-L+1
          TY='NATIVE'
          LT=6
        ELSE IF(LFIL.GE.8.AND.TNAM(LFIL-6:LFIL-3).EQ.'.NAT') THEN
          TY='NATIVE'
          LT=6
        ELSE
          TY='EPIO'
          LT=4
        END IF
C
C        ELSE IF(LFIL.GE.6.AND.TNAM(LFIL-4:LFIL).EQ.'.EPIO'  ) THEN
C          TY='EPIO'
C          LT=4
C        ELSE IF(LFIL.GE.4.AND.TNAM(LFIL-2:LFIL).EQ.'.SL'    ) THEN
C          TY='EPIO'
C          LT=4
C        ELSE
C          FNOFI=.TRUE.
C          RETURN
C
        FNOFI=.FALSE.
        TNAM=TFIL
        LNAM=LFIL
      END IF
      CALL DWRT('-------------------- WAIT --------------------')
      CALL DPARGV(81,'JDB',4,DEB)
C     +
      IF(DEB.EQ.1.) THEN
        CALL DWRDT('      T1 =',TNAM(1:LNAM) ,'D_LKOPRD')
        CALL DWRDT('      T2 =',TY(1:LT) ,'D_LKOPRD')
        CALL DWRDT('      T3 =',TY,'D_LKOPRD')
        CALL DWRD ('      CALL AUNPCK_IN(T3)','D_LKOPRD')
        CALL DWRD ('      CALL LKOPRD(T1,T2,IER)','D_LKOPRD')
      END IF
C     +
      CALL AUNPCK_IN(TY)
      CALL LKOPRD(TNAM(1:LNAM),TY(1:LT),IER)
C     +
      CALL DWRDI('C     IER=',IER ,'D_LKOPRD')
C     +
      IF(IER.EQ.0) THEN
        CALL ABRUEV(IRUN,IEVT)
        CALL DWRD ('      CALL ABRUEV(IRUN,IEVT)','D_LKOPRD')
        FNOFDF=.FALSE.
      ELSE
        TY='ERROR'
        IRUN=0
        IEVT=0
        FNOFDF=.TRUE.
      END IF
C     +
      IF(DEB.EQ.1.) THEN
        CALL DWRDI('C     NR=',IRUN,'D_LKOPRD')
        CALL DWRDI('C     NE=',IEVT,'D_LKOPRD')
      END IF
C     +
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  D_LKGTEVT
CH
      SUBROUTINE D_LKGTEVT(IR,IE,IER,TPOS)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      CHARACTER *(*) TPOS
      CALL DPARGV(81,'JDB',4,DEB)
      IF(DEB.EQ.1.) THEN
        CALL DWRDI('      IR =',IR,TPOS)
        CALL DWRDI('      IE =',IE,TPOS)
        CALL DWRD ('      CALL LKGTEVT(.TRUE.,''E'',''AL '',IR,IE,IER)'
     &    ,TPOS)
      END IF
      CALL LKGTEVT(.TRUE.,'E','AL ',IR,IE,IER)
      CALL DWRDI(  'C     IER=',IER,TPOS)
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  D_LKREW
CH
      SUBROUTINE D_LKREW
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C      CHARACTER *(*) TPOS
C      CALL DPARGV(81,'JDB',4,DEB)
C      IF(DEB.EQ.1.) THEN
C        CALL DWRDI('      IR =',IR,TPOS)
C        CALL DWRDI('      IE =',IE,TPOS)
C        CALL DWRD ('      CALL LKGTEVT(.TRUE.,''E'',''AL '',IR,IE,IER)'
C     &    ,TPOS)
C      END IF
C      CALL LKGTEVT(.TRUE.,'E','AL ',IR,IE,IER)
C      CALL DWRDI(  'C     IER=',IER,TPOS)
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  D_BRWND
CH
      SUBROUTINE D_BRWND
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CALL DWRD ('      CALL LKRWND(''REWIND/DATA'')','D_BRWND')
      CALL LKRWND('REWIND/DATA')
C      CALL DWRDI('C     LUNEDB=',LUNEDB,'BRWND')
C      CALL DWRD ('      CALL BRWND(LUNEDB)','BRWND')
C      CALL BRWND(LUNEDB)
C      CALL DWRD ('C     ','BRWND')
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  D_ACLOSE
CH
      SUBROUTINE D_ACLOSE(LUN,IRCL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CALL DWRDI('      LUN =',LUN,'ACLOSE')
      CALL DWRD ('      CALL ACLOSE(LUN,IRCL)','ACLOSE')
      CALL ACLOSE(LUN,IRCL)
      CALL DWRDI('C     IRCL=',IRCL,'ACLOSE')
      END
*DK DJADBS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DJADBS
CH
      SUBROUTINE DJADBS
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'A_BCS.INC'
      CHARACTER *69 T
      DATA JVER/1/
      KDBS=NAMIND('ADBS')
      JDBS=IW(KDBS)
      NDBS=JDBS+2+JVER
      IVER=IW(NDBS)
      WRITE(T,1000) KDBS,NDBS,IVER
 1000 FORMAT('NAMIND(''ADBS'')=',I6,' JDBS+3=',I6,' version=',I4)
      CALL DWRT(T)

      KTLFC=NAMIND('TLFC')
      JTLFC=IW(KTLFC)

      KTLCP=NAMIND('TLCP')
      JTLCP=IW(KTLCP)

      WRITE(T,1001) JTLFC,JTLCP
 1001 FORMAT(I6,' = JTLFC  ',I6,' = JTLCP (in DALBJ:DJADBS)')
      CALL DWRT(T)
      END
*DK DJLBNK
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DJLBNK
CH
      SUBROUTINE DJLBNK(N6,N9)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C
C
C    Called by :DEV
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      IW6=IW(6)
      IF(N6.NE.0) THEN
        IW(6)=N6
        CALL AUBLIS('E')
        CALL AUBLIS('S')
      END IF
      IF(N9.NE.0) THEN
        IW(6)=N9
        CALL AUBLIS('E')
        CALL AUBLIS('S')
      END IF
      IW(6)=IW6
      END
*DK DJDRLI
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DJDRLI
CH
      SUBROUTINE DJDRLI
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C
C
C    Called by :DEV
C     DROP LIST
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      CHARACTER *1 T1
      CHARACTER *14 TL
      CHARACTER *10 TG
      DATA NUNIT/51/
      DATA TL/' list dropped.'/
      DATA TG/'CALL BGARB'/
      DATA  IW6/6/
      IW(6)=IW6
    1 CALL DTYANS('BOSBK=1, close=2, go back = <CR>, which list ?',
     &  '12',NOUT)
      IF(NOUT.EQ.0) THEN
        IW(6)=IW6
        RETURN
      END IF
      IF(NOUT.EQ.1) THEN
        IW(6)=NUNIT
        WRITE(NUNIT,1111)
        CALL BOSBK(IW)
        WRITE(NUNIT,1111)
 1111   FORMAT(' ',/,' ',70('*'),/,' ')
        GO TO 1
      END IF
      IF(NOUT.EQ.2) THEN
        CLOSE(UNIT=nunit)
        CALL DWRT('FOR051 closed.')
        GO TO 1
      END IF
      CALL DTYATX(T1)
    2 IW(6)=51
      CALL AUBLIS(T1)
      IW(6)=6
      CALL AUBLIS(T1)
    3 CALL DTYANS('Drop '//T1//'-list ? Yes + Garb, return=<CR>',
     &  'YG',NOUT)
      IF(NOUT.LT.0) GO TO 3
      IF(NOUT.EQ.0) THEN
        CALL DWRT('No'//TL)
        WRITE(NUNIT,1000) 'No',TL
        GO TO 1
      ELSE
        CALL BDROP(IW,T1)
        CALL DWRT(T1//TL)
        WRITE(51,1000) T1,TL
        IF(NOUT.EQ.2) THEN
          CALL BGARB(IW)
          CALL DWRT(TG)
          WRITE(NUNIT,1000) TG
        END IF
        GO TO 2
      END IF
 1000 FORMAT('DALI message: ',2A)
      END
*DK DJSIW6
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DJSIW6
CH
      SUBROUTINE DJSIW6
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C
C
C    Called by :DEV
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      DATA IW6/90/
      CHARACTER *3 DT3
      IF(IW(6).NE.6) THEN
        IW6=IW(6)
        IW(6)=6
        CALL LKSTOUT(6)
      ELSE
        IW(6)=IW6
        CALL LKSTOUT(IW6)
      END IF
      CALL DWRT('IW(6)='//DT3(FLOAT(IW(6))))
      END
*DK DJ_READ_PRIVATE_CARDS
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++ DJ_READ_PRIVATE_CARDS
CH
      SUBROUTINE DJ_READ_PRIVATE_CARDS
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C  Author : H.Drevermann
C           Read an private input cards file into DALI.
C
C    Called by :DEV
C------------------------------------------------------------------
C
C Input logical units: (17=cards)
      INCLUDE 'DALI_CF.INC'
      CHARACTER *70 TC
      INCLUDE 'A_BCS.INC'
C  ------------------------------------------------------
C
C Log.unit to read card file must be redefined:
      IW(5)  = NUNIDU
      CALL DWRT('<CR> = go back, name of private cards file = ')
      CALL DGETLN(TC,LC,70)
      IF(LC.LT.3) RETURN
C  ------------------------------------------------------
      CALL DGOPEN(NUNIDU,TC(1:LC),12,*10,IER)
      IF(IER.LT.0) GO TO 10
      CALL BREADC
      CALL ACLOSE(NUNIDU,IER)
      RETURN
   10 CALL DWRT(TC(1:LC)//' not found.#')
      END
