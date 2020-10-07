      SUBROUTINE RERROR(CALID,MNI,MSG)
C-------------------------------------------------------------------
C! JULIA error reporting routine
C! derived from TPC error reporting routine TREPRT
C!
C!    Author:  R. Johnson     23/11/87
C!    Modified:J.Knobloch     10/10/88
C!    Modified:J.Knobloch     15/12/88
C!    Modified:J.Knobloch     22/05/89
C!    Modified :- E. Lancon             24-FEB-1993
C!       routine autoinitialized but still backward compatible
C!    Modified:M.Cattaneo     21/06/96
C!       Fix bug: error was not reported on autoinitialising first call
C!
C!    Input:
C!        - CALID:  Character string giving the name
C!                  of the program module which is calling RERROR.
C!                  This should be no longer than 6 characters.
C!        - MNI:    Integer error code (unique for given CALID).
C!                  entered negative for fatal error
C!        - MSG:    Character string of any length for the message
C!                  giving details of what error has occured.
C!                  Ampersands (&) should separate individual lines.
C!                  Lines longer than 125 characters will be truncated.
C!                  This message should neither give the program name
C!                  nor the run nor event numbers, as all that will
C!                  be printed anyway.
C!
C!    Description
C!    ===========
C!    This routine is used within the JULIA offline code to report
C!    non-fatal run-time errors.  For each unique error code, MN,
C!    the associated message will be printed a limited number of
C!    times.  The message also is saved, and a count is kept of the
C!    total number of times that the error is reported during the
C!    job.  These results then are summarized at the end of the
C!    job by RPERRS.  Note that the statistics and error messages
C!    are saved in the BOS common IW in banks RREP. The number of
C!    times an error is printed can be set by a data card
C!    ERRP nprint
C-------------------------------------------------------------------
#ifndef DOC
C
#include "tparac.h"
#include "bcs.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "rrepjj.h"
#include "rflags.h"
C
      CHARACTER CALID*(*), MSG*(*),CHAINT*4,CAL8*8
      PARAMETER (MXLEN=130,MXSTO=48)
      CHARACTER TEXT*140,TXT1*4,MSGL*48
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA MXMES/150/
C
C---------------------------------------------------------------------
C
#include "bmacro.h"
C
C---------------------------------------------------------------------
C
C++   On the first call, create all banks to store error codes
C
      IF (FIRST .OR. CALID.EQ.'RINERR') THEN
        NRREP=NAMIND('RREP')
        LENG=MXMES*LRREPA+LMHLEN
        CALL AUBOS('RREP',0,LENG,KRREP,IGARB)
        IF (IGARB.NE.0) GOTO 995
        FIRST=.FALSE.
        IW(KRREP+LMHCOL)=LRREPA
        IW(KRREP+LMHROW)=0
        LENG= (MXSTO+3)/4
        DO 1 N=1,MXMES
          CALL AUBOS('RREP',N,LENG,KRREP,IGARB)
          IF (IGARB.NE.0) GOTO 995
    1   CONTINUE
        MXPRT=LMXPRT
        MXPRF=999999
        KERRP=IW(NAMIND('ERRP'))
        IF(KERRP.NE.0) THEN
          IF(IW(KERRP).GT.0)MXPRT=IW(KERRP+1)
          IF(IW(KERRP).GT.1)MXPRF=IW(KERRP+2)
        ENDIF
        IF( CALID.EQ.'RINERR' ) GOTO 999
      ENDIF
C
C     set ERROR bits
C
      MN=IABS(MNI)
      IND=2
      IBIT=JCMORF
      IF(IBIT.GT.31)THEN
        IBIT=IBIT-32
        IND=1
      ENDIF
      IF(MNI.LT.0) THEN
        JFBIRF(IND)=IBSET(JFBIRF(IND),IBIT)
      ELSE
        JEBIRF(IND)=IBSET(JEBIRF(IND),IBIT)
      ENDIF
      KRREP=IW(NRREP)
      IF(KRREP.EQ.0) GOTO 999
C
C++   Check whether we've seen this message before
C
      CAL8=CALID
      I14 = INTCHA(CAL8(1:4))
      I58 = INTCHA(CAL8(5:8))
      DO 113 I=1,IW(KRREP+LMHROW)
        ICODE=ITABL(KRREP,I,JRREMC)
        IF (ICODE.EQ.MN.AND.ITABL(KRREP,I,JRREB1).EQ.I14
     1                 .AND.ITABL(KRREP,I,JRREB2).EQ.I58)THEN
          NMES=I
          GO TO 101
        ENDIF
  113 CONTINUE
C
C++     New message - check for maximum number
C
      NMES=IW(KRREP+LMHROW)+1
      IF (NMES.GT.MXMES) THEN
        NMES=MXMES
        WRITE(LOUTRL,'(A)')
     +     ' RERROR - Maximum number of errors exceeded'
        GOTO 999
      ENDIF
      IW(KRREP+LMHROW)=NMES
C
C++     Store message code and module name and initialize no. of calls
C
      IW(KROW(KRREP,NMES)+JRREMC)=MN
      IW(KROW(KRREP,NMES)+JRREB1)=I14
      IW(KROW(KRREP,NMES)+JRREB2)=I58
      IW(KROW(KRREP,NMES)+JRRENC)=0
C
C++     Store the message in a bank with NR= message number
C
      LENG= (MXSTO+3)/4
      KRREP = NLINK('RREP',NMES)
      MSGL = MSG
      DO 200 I=1,LENG
        IB1= I*4 - 3
        IB4=MIN(IB1+3,MXSTO)
        TXT1=MSGL(IB1:IB4)//'    '
        IW(KRREP+I)= INTCHA(TXT1)
  200 CONTINUE
C
      KRREP=IW(NRREP)
  101 CONTINUE
C
C++   Increment number of calls for this message
C
      IW(KROW(KRREP,NMES)+JRRENC)=ITABL(KRREP,NMES,JRRENC)+1
C
C++   Print the warning message no more than MXPRT times
C
      IF (ITABL(KRREP,NMES,JRRENC).GT.MXPRT.AND.MNI.GT.0) GO TO 999
C
C++   Print the FATAL ERRORS no more than MXPRF times
C
      IF (ITABL(KRREP,NMES,JRRENC).GT.MXPRF.AND.MNI.LE.0) GO TO 999
C
C++   Print out the header
C
      LC = LEN(CALID)
      LC = MIN(LC,59)
      IF(MNI.LT.0) THEN
        TEXT = '('' Fatal error number '',I3,'' '//
     &           'reported by '
     &           //CALID(1:LC)//
     &           ' at run '',I5,'', event '',I6,'':'')'
      ELSE
        TEXT = '('' Warning message number '',I3,'' '//
     &           'reported by '
     &           //CALID(1:LC)//
     &           ' at run '',I5,'', event '',I6,'':'')'
      ENDIF
      LT = LC + 85
      LT=MIN(LT,MXLEN)
      WRITE (LOUTRL,FMT=TEXT(1:LT)) MN,IRUNRC,IEVTRC
C
C++   Parse the message into separate lines separated by ampersands
C
      IFRST=1
      LM=LEN(MSG)
      DO 100 I=1,LM
        IF (MSG(I:I).EQ.'&') THEN
          LENMS=MIN(MXLEN-5,I-IFRST)
          IF (LENMS.GT.0) THEN
            ILST=IFRST+LENMS-1
            TEXT= '('' +++ '//MSG(IFRST:ILST)//''')'
            WRITE (LOUTRL,FMT=TEXT(1:LENMS+9))
          ENDIF
          IFRST=I+1
        ENDIF
  100 CONTINUE
      LENMS=MIN(MXLEN-5,LM-IFRST+1)
      IF (LENMS.GT.0) THEN
        ILST=IFRST+LENMS-1
        TEXT= '('' +++ '//MSG(IFRST:ILST)//''')'
        WRITE (LOUTRL,FMT=TEXT(1:LENMS+9))
      ENDIF
      WRITE (LOUTRL,'(/)')
C
      GOTO 999
C
C Initialisation error
  995 WRITE(LOUTRL,'(A)')
     +     ' RERROR - PROBLEM AT INITIALIZATION !!!!!!!!!'
C
  999 CONTINUE
      RETURN
      END
#endif
