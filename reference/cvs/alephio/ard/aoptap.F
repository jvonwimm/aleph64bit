      SUBROUTINE AOPTAP(LUN,ANAME,ATYPE,FDEVI,IER )
C----------------------------------------------------------------------
CKEY ALREAD OPEN READ TAPE CARTRIDGE
C   Author: T. Wildish (IBM code)   November 1988
C           J. Hilgart (VAX code)   July 1989
C                       modified by M.Rumpf for MOUNT feb 90
C                       ALWS STAGE added U.Schaefer June 93
C!  Purpose   : OPEN  BOS-file from tape or cartridge
C   Inputs    : LUN      logical unit
C
C               FNAME    DSN of tape file, can be ' ' if NL tape
C                        is used or if you wish to ignore the dsn
C
C               ATYPE    'NATIVE'  for native fortran
C                        'EPIO'    for EPIO
C               FDEVI     'TAPE tapeid options' or
C                         'CART tapeid options'
C
C               where 'tapeid' is 'vsn.fseq.label' and 'options' are
C               any staging options as required
C
C   Outputs   : IER  = 0  successful opening
C
C ==== IBM example ===============
C  e.g. FILI 'DSNAME |EPIO | CART AC0021.1.SL'       for staging
C  e.g. FILI 'DSNAME |EPIO | CART AC0021.1.SL MOUNT' mount - no staging
C   or  FILI '       |NATIVE|TAPE 03558W.1.NL RECFM VBS BLOCK 32760'
C
C ==== VAX example ===============
C
C             where FOR VXCERN 'tapeid' is 'vsn.fseq.label' and
C             'options' are any staging options as required.
C             Note that the node is checked, and only if it is VXCERN
C             a stage operation performed. On any other machine the
C             dummy routine
C             MYTAPE is called, allowing other sites to implement their
C             own fortran tape handling routines at this point.
C             MYTAPE has the following arguments:
C   Inputs    : LUN      logical unit
C
C               FNAME    DSN of tape file, can be ' ' if NL tape
C                        is used or if you wish to ignore the dsn
C
C               ATYPE    'NATIVE'  for native fortran
C                        'EPIO'    for EPIO
C               FDEVI     'TAPE my_options'
C                         'CART my_options'
C
C               where 'my_options' are any options required
C
C   Outputs   : IER  = 0  successful opening
C
C Description : User routine which can be filled in w/ code for
C site-specific handling of tape drives on VAX w/in fortran. Note
C the file opening is still handled by AOPTAP. This version is a
C dummy which does nothing.
C
C  e.g. FILI 'DSNAME |EPIO | CART AC0021.1.SL'
C             is translated by this routine (for the VXCERN user) to
C  $STAGE/NAM=DSNAME/SIZ=200 AC0021 AC0021 LUN /NUM=1/LAB=EBCDIC
C
C           For VXCERN I believe that any option allowed by STAGE
C           can be implemented via the options, and multiple input
C           files ARE supported. Another example:
C           if VSN # VID
C           and only the first 10 Mbytes is desired (by default I ask
C           to stage the entire disk or cassette), use a card such as
C
C   FILI 'DSNAME |EPIO | CART VSN.1.SL  VSN VID FOR020 /SIZ=10'
C
C --------------------------------------------------------------------
#ifndef DOC
#if defined(ALEPH_DEC) &&  ! defined(UNIX)
C Set-up things for a system service call to SYS$TRNLNM
      INCLUDE '($LNMDEF)'
      INCLUDE '($SSDEF)'
      INTEGER*2 ITMLS(8) / 10, LNM$_STRING,
     &   6*0 /
      EQUIVALENCE (ITMLS(3),IADDR)
      EQUIVALENCE (ITMLS(5),LEN)
      INTEGER SYS$TRNLNM, LIB$SPAWN, LIB$GET_SYMBOL
      INTEGER STATUS
      CHARACTER*3   CHLABL
      CHARACTER*10  VSN,FSEQ,NODE
      CHARACTER*80  CHTEM,SPACES
      CHARACTER*160  CHSTG
#endif
#if defined(UNIX)
      PARAMETER (NCHOUT=20)
      DIMENSION CHOUT(NCHOUT)
      CHARACTER*(25) CHOUT
      INTEGER SYSTEM
#if ! defined(ALEPH_GFORT)
      EXTERNAL SYSTEM
#endif
#endif
C
#include "bcs.h"
#include "alrcom.h"
#include "albitw.h"
#include "gtchdecl.h"
      LOGICAL FDROP
      DATA FDROP/.FALSE./
C----------------------------------------------------------------------
C
      SUBR = ' AOPTAP '
      FNAME = ANAME
      IER = 0
#include "prfname.h"
C
C - get medium, tape no. and option list
#include "gtvsn.h"
C - Verify that we have a tape or cartridge request
      IF ( INDEX (TAPPAR,CHMED) .EQ. 0) THEN
C       something nasty has happened if we got here!
         IER = 7
         RETURN
      ENDIF
C - get file type and record length
C   close previous file and send file name to EPIO
#include "gtftyp.h"
C - get logical unit as character variable
#include "gtchunit.h"
C
#if defined(IBM)
C ---------------------------------------------------------------
C   IBM      IBM       IBM       IBM       IBM       IBM
C ---------------------------------------------------------------
C NO staging - first TAPE/CART already mounted
C SETUP done by ALPHARUN
C
      IF (INDEX(FDEVI,'MOUNT') .NE. 0) THEN
         CALL AMOUNT(LUN,ANAME,ATYPE,FDEVI,IER)
         RETURN
      ENDIF
C
C     SIZE  and WAIT should  be specified
      IF (LNBLNK(CHOPT).EQ.0) THEN
           CHOPT = ' WAIT SIZE 200 '
      ENDIF
      IF ( INDEX(CHOPT,'WAIT').EQ.0 ) THEN
           CHOPT = CHOPT(1:LNBLNK(CHOPT))//' WAIT'
      END IF
      IF ( INDEX(CHOPT,'SIZE').EQ.0 ) THEN
           CHOPT = CHOPT(1:LNBLNK(CHOPT))//' SIZE 200'
      ENDIF
C
C     ROBOT CARTRIDGE REQUEST
      IF ( CHMED.EQ.'SMCF' ) THEN
C     look for DENsity 38K, if absent, put it in!
         IF ( INDEX(CHOPT,' DEN ').EQ.0 ) THEN
            CHOPT = CHOPT(1:LNBLNK(CHOPT))//' DEN 38K'
            IF (CHMED .EQ. 'SMCF') THEN
               CHOPT = CHOPT(1:LNBLNK(CHOPT))//' DEV SMCF'
            ENDIF
         END IF
C
C     EXABYTE REQUEST
      ELSEIF ( CHMED.EQ.'8MM ' ) THEN
C     look for DENsity 43200 (Model 8200 ), if absent, put it in!
         IF ( INDEX(CHOPT,' DEN ').EQ.0 ) THEN
            CHOPT = CHOPT(1:LNBLNK(CHOPT))//' DEN 8200 DEV 8MM'
         END IF
C
C     TAPE REQUEST
      ELSEIF ( CHMED.EQ.'TAPE' ) THEN
C     look for DENsity 6250, if absent, put it in!
         IF ( INDEX(CHOPT,' DEN ').EQ.0 ) THEN
            CHOPT = CHOPT(1:LNBLNK(CHOPT))//' DEN 6250'
         END IF
C
      ENDIF
C
C     tape DSN, if given in FNAME, must be appended to options
      IF ( LNBLNK(FNAME).NE.0 ) THEN
           CHOPT = CHOPT(1:LNBLNK(CHOPT))//' DSN '//FNAME
      END IF
C
C     Force RECFM F for EPIO files if RECFM is not given
      IF (FTYP3.EQ.'EPI' .AND. INDEX(CHOPT,'RECFM').EQ.0) THEN
           CHOPT = CHOPT(1:LNBLNK(CHOPT))//' RECFM F'
      END IF
C
C     DSNAME will be used for dropping the stage disk at the next call
C     drop any previous staged data disk
      IF ( FDROP ) THEN
           MSG = 'EXEC STAGE DROP '//DSNAME
#include "prmsg.h"
           CALL  VMCMS (MSG(1:LNBLNK(MSG)),IST)
           FDROP = .FALSE.
      END IF
C
      MSG = 'EXEC STAGE PRE '//DSNAME
C
      MSG = MSG(1:LNBLNK(MSG))//' '//CHTAPE(1:LNBLNK(CHTAPE))//' '//
     &      CHOPT(1:LNBLNK(CHOPT))
C
C     Now for the STAGE request!
#include "prmsg.h"
      CALL VMCMS (MSG(1:LNBLNK(MSG)),IST)
C
C     Fatal error ?
      IF ( IST.GT.20 ) GOTO 98
C
C     Non-fatal error? Set flag for dropping disk next time
      IF ( IST.EQ.1 )  THEN
         WRITE (IW(6),*) ' +++AOPTAP+++ warning: staged file is ',
     &                   'truncated - adjust SIZE parameter.'
         IER = 0
       ELSEIF (IST.NE.0) THEN
        WRITE(IW(6),*) ' +++AOPTAP+++  warning while opening. IST= ',IST
        IER=0
      ENDIF
      FDROP = .TRUE.
C
C     Flag use of staging for dropping at end of job
      CALL ASTAGE(-LUN,' ',IERROR)
C -------------------------------------------------------------------
C     END OF IBM CODE                 END OF IBM CODE
C --------------------------------------------------------------------
#endif
#if defined(UNIX)
C --------------------------------------------------------------------
C       UNIX      UNIX         UNIX      UNIX       UNIX
C --------------------------------------------------------------------
C
      MSG = ' -V '
C
C     decode tape number
      LT     = LNBLNK (CHTAPE)
      I1P    = INDEX(CHTAPE,'.')
      I2P = INDEX (CHTAPE(I1P+1:LT),'.') + I1P
      CALL CUTOL (CHTAPE(I2P+1:LT))
C
      MSG = MSG(1:LNBLNK(MSG))//' '// CHTAPE(1:I1P-1)//
     &                       ' -q '// CHTAPE(I1P+1:I2P-1)//
     &                       ' -l '// CHTAPE(I2P+1:LT)
C
C     tape DSN, if given in FNAME, must be appended to options
      IF ( LNBLNK(FNAME).NE.0 ) THEN
         MSG = MSG(1:LNBLNK(MSG)) //' -f '//FNAME
      END IF
C
C            add remaning options
C            remove 'SIze' and 'Wait'
C
C     decode CHOPT
      CALL ALSTRI(CHOPT,' ',NCHOUT,CHOUT,NWORD)
C
      DO I=1,NWORD
         IF(CHOUT(I)(1:2).EQ.'SI') THEN
            CHOUT(I)=' '
            CHOUT(I+1)=' '
         ELSEIF (CHOUT(I)(1:1).EQ.'W') THEN
            CHOUT(I)=' '
         ENDIF
         LOUT = LNBLNK(CHOUT(I))
         IF (LOUT.GT.0) MSG = MSG(1:LNBLNK(MSG))//' '//CHOUT(I)(1:LOUT)
      ENDDO
C
C     Now for the STAGE request!
C   add the block length
      CHRECL = ' '
      CALL CSETDI (KRECL,CHRECL,1,5)
      MSG = MSG(1:LNBLNK(MSG))//' -F F -L '//CHRECL
C   add the name of the staged file in the form 'cartno_fseq.atype'
C   for the time being do not include fseq when fseq=1
      IF (CHTAPE(I1P:I1P+2).EQ.'.1.') THEN
         FNAME = CHTAPE(1:I1P)//ATYPE
      ELSE
         FNAME = CHTAPE(1:I2P)//ATYPE
         FNAME(I1P:I1P) = '_'
      ENDIF
      LF = LNBLNK(FNAME)
      CALL CUTOL(FNAME(1:LF))
      CHDEF = 'alstagein '//MSG(1:LNBLNK(MSG)+1)//FNAME(1:LF)
      MSG = CHDEF(1:LNBLNK(CHDEF))
#include "prmsg.h"
      IST = SYSTEM (MSG) / 256
      IF (IST.NE.0) GOTO 98
C   get the full filename of the staged file
      CALL GETENV ('ALSTAGE',MSG)
      MSG = MSG(1:LNBLNK(MSG))//'/'//FNAME(1:LF)
#include "prmsg.h"
      FNAME = MSG(1:LNBLNK(MSG))
      IF (FTYP3 .EQ. 'EPI') THEN
C      send EPIO filename to EPIO and set UNIX flag
C      file is opened by rfio through EPIO
         CALL EPSETW (LUN,33,2,IST)
         LE = LNBLNK(FNAME)
         CALL EPDEFU (LUN,FNAME(1:LE),IST)
         IF (IST.NE.0) GOTO 98
      ELSE
C      open native data file  in direct access mode
         OPEN (LUN,STATUS='OLD',FILE=FNAME,
     &      ACCESS='DIRECT',RECL=MRECL,ERR=98,IOSTAT=IST)
      ENDIF
C
C     Flag use of staging for dropping at end of job
      CALL ASTAGE(-LUN,' ',IERROR)
C -----------------------------------------------------------------
C       END OF UNIX CODE                   END OF UNIX CODE
C ------------------------------------------------------------------
#endif
#if defined(ALEPH_DEC) &&  ! defined(UNIX)
C
C----------------------------------------------------------------------
C     VAX   VAX  VAX  VAX  VAX  VAX
C --------------------------------------------------------------------
C
C Fix up options string by removing extra spaces, capitalizing
         CHOPT = SPACES(CHOPT(1:LNBLNK(CHOPT)),1)
         CALL CLTOU(CHOPT)
C
C Here we remove all spaces after the '/' character because they are not
C needed
         DO 91 ID = 1, LNBLNK(CHOPT)
            IF (CHOPT(ID:ID).EQ.'/') THEN
               IF (ID+2.GT.LNBLNK(CHOPT))  GOTO 92
               IF (CHOPT(ID+1:ID+1).EQ.' ')  CHOPT =
     &            CHOPT(1:ID)//CHOPT(ID+2:LNBLNK(CHOPT))
            ENDIF
   91    CONTINUE
   92    CONTINUE
C Learn if we are at VXCERN
         IADDR = %LOC(NODE)
         LEN = %LOC(LENNA)
         STATUS = SYS$TRNLNM(,'LNM$SYSTEM_TABLE','SYS$CLUSTER_NODE',
     &   ,ITMLS)
C
C      IF (.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
         IF (LIB$GET_SYMBOL('ALEPH$STAGE',MSG).EQ.1) THEN
           MSG='ALEPH$STAGE'//' '//FNAME(1:LNBLNK(FNAME))//' '
     &       //DSNAME//'  '//ATYPE//' '//FDEVI
#include "prmsg.h"
           STATUS = LIB$SPAWN (MSG,,,,,,IST)
#if defined(ARDEB)
           WRITE(6,*)SUBR,'STATUS OF LIB$SPAWN : ',IST
#endif
           IF (IST.NE.1) GOTO 98
           GOTO 900
         ENDIF
         IF (NODE(1:6).NE.'VXCERN')  THEN
            CALL MYTAPE(LUN,FNAME,ATYPE,FDEVI,IER )
            GOTO 900
         ENDIF
C
C Translate IBM-style syntax into VAX syntax for the STAGE command
C
         CHSTG = '$STAGE '
         IF (CHMED.EQ.'CART')  CHSTG = CHSTG(1:LNBLNK(CHSTG))//
     &    '/GEN=CT1 '
C
C     tape DSN, if given in FNAME, must be appended to options
         IF ( LNBLNK(FNAME).NE.0 ) THEN
            CHSTG = CHSTG(1:LNBLNK(CHSTG))//'/NAM='//
     &              FNAME(1:LNBLNK(FNAME))
         END IF
C
C Size spec given should be large enough to fetch whole cartridge, unles
C user has specified some other value
         IF (INDEX(CHOPT,'/SI').EQ.0) THEN
C
C We are here if no /SIZE qualifier has been specified. Put one in
            IF (CHMED.EQ.'CART') THEN
               CHSTG = CHSTG(1:LNBLNK(CHSTG))//'/SIZ=200'
            ELSE
               CHSTG = CHSTG(1:LNBLNK(CHSTG))//'/SIZ=150'
            ENDIF
         ENDIF
C
C Parse CHTAPE to get out VSN, FSEQ, and LABEL
         IND = INDEX(CHTAPE,'.')
         VSN = CHTAPE(1:IND-1)
         IL = LNBLNK(VSN)
C
C Check if CHOPT has different VSN and VID
         I2 = INDEX(CHOPT(1:LNBLNK(CHOPT)),' '//VSN(1:IL))
         IF (I2.EQ.0) THEN
C
C The normal case when VID = VSN
C Add the VSN twice and
C Add the fortran unit no. as 3rd word (logical-name)
            VSN = ' '//VSN(1:IL)
            CHSTG = CHSTG(1:LNBLNK(CHSTG))//VSN(1:IL+1)//VSN(1:IL+1)//
     &             ' FOR0'//CHUNIT
         ENDIF
C
C File no. on tape
         IND1 = INDEX(CHTAPE(IND+1:LNBLNK(CHTAPE)),'.')
         FSEQ = CHTAPE(IND+1:IND+IND1-1)
         CHSTG = CHSTG(1:LNBLNK(CHSTG))//' /NUM='//FSEQ(1:LNBLNK(FSEQ))
C
C Is this an IBM standard-labelled tape?
         CHLABL = CHTAPE(IND+IND1+1:IND+IND1+2)
         IF (CHLABL.EQ.'SL')  THEN
            CHSTG = CHSTG(1:LNBLNK(CHSTG))//'/LAB=EBCDIC'
         ELSEIF (CHLABL.EQ.'NL') THEN
            CHSTG = CHSTG(1:LNBLNK(CHSTG))//'/LAB=NONE'
         ENDIF
C
C Add in the options now
         CHSTG = CHSTG(1:LNBLNK(CHSTG))//
     &           CHOPT(INDEX(CHOPT,' '):LNBLNK(CHOPT))
C
C
C Now CHSTG can be sent directly to VXCERN
         MSG = CHSTG(1:LNBLNK(CHSTG))
#include "prmsg.h"
C And here is the actual STAGE request...
         STATUS = LIB$SPAWN(CHSTG)
         IF (.NOT.STATUS)  CALL LIB$SIGNAL(%VAL(STATUS))
         IER = 0
C
 900  CONTINUE
C - open the staged file through EPIO or directly
      IF (FTYP3.EQ.'EPI') THEN
C     EPIO file send logical name through EPDEFU
         CALL EPDEFU (LUN,DSNAME(1:6),IST)
      ELSE
C     NATIVE file open it with READONLY
         OPEN (LUN,STATUS='OLD',FORM='UNFORMATTED',READONLY,
     &         RECORDTYPE='FIXED',ERR=98,IOSTAT=IST)
      ENDIF
C --------------------------------------------------------------------
C    END OF VAX CODE                 END OF VAX CODE
C --------------------------------------------------------------------
#endif
C
C----------------------------------------------------------------------
C                  define BOS file format  (FORT,EPIO,TEXT)
C----------------------------------------------------------------------
      BFORM2 = 'SEQ'
#if defined(UNIX)
      IF (DIRECT) BFORM2 = 'DIR'
#endif
#include "stbforma.h"
C
      GOTO 999
C
C - error during opening
 98   CONTINUE
      IER = 7
      CALL PRSTAT ('AOPTAP',FNAME,IST)
C
 999  CONTINUE
#if defined(UNIX)
      DIRECT = .FALSE.
#endif
      END
#endif
