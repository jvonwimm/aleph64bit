      SUBROUTINE REPORT(CALID,MSG,ITRAP)
C-------------------------------------------------------------------
C! Central error reporting routine
C!
C!    Author:  R. Johnson     21/4/87
C!
C!    Input:
C!        - CALID:  Character string of any length giving the name
C!                  of the program module which is calling REPORT.
C!        - MSG:    Character string of any length for the message
C!                  giving details of what error has occured.
C!                  Ampersands (&) should separate individual lines.
C!                  Lines longer than 125 characters will be truncated.
C!                  This message should neither give the program name
C!                  nor the run nor event numbers nor whether the
C!                  error is fatal, as all that will be printed anyway.
C!        - ITRAP:  An integer set to zero if the error is to be
C!                  nonfatal or set to 1 for a fatal error.
C-------------------------------------------------------------------
#ifndef DOC
C
#include "rlunit.h"
#include "rcurnt.h"
C
      CHARACTER CALID*(*), MSG*(*)
C
      PARAMETER (MXLEN=130)
      CHARACTER TEXT*140
C
C++   Print out the header
C
      LC = LEN(CALID)
      LC = MIN(LC,74)
      IF (ITRAP.EQ.0) THEN
        TEXT = '('' Error reported by '//CALID(1:LC)//
     &           ' at run '',I5,'', event '',I6,'':'')'
        LT = LC + 62
      ELSE
        TEXT = '('' Fatal error reported by '//CALID(1:LC)//
     &           ' at run '',I5,'', event '',I6,'':'')'
        LT= LC + 58
      ENDIF
      LT=MIN(LT,MXLEN)
      WRITE (LDEBRL,'(/)')
      WRITE (LDEBRL,FMT=TEXT(1:LT)) IRUNRC,IEVTRC
      WRITE (LDEBRL,'(1X)')
C
C++   Parse the message into separate lines separated by ampsersands
C
      IFRST=1
      LM=LEN(MSG)
      DO 100 I=1,LM
        IF (MSG(I:I).EQ.'&') THEN
          LENMS=MIN(MXLEN-5,I-IFRST)
          IF (LENMS.GT.0) THEN
            ILST=IFRST+LENMS-1
            TEXT= '('' +++ '//MSG(IFRST:ILST)//''')'
            WRITE (LDEBRL,FMT=TEXT(1:LENMS+9))
          ENDIF
          IFRST=I+1
        ENDIF
  100 CONTINUE
      LENMS=MIN(MXLEN-5,LM-IFRST+1)
      IF (LENMS.GT.0) THEN
        ILST=IFRST+LENMS-1
        TEXT= '('' +++ '//MSG(IFRST:ILST)//''')'
        WRITE (LDEBRL,FMT=TEXT(1:LENMS+9))
      ENDIF
      WRITE (LDEBRL,'(/)')
C
C      IF (ITRAP.NE.0) CALL EXIT
C
      RETURN
      END
#endif
