      SUBROUTINE RPERRS
C
C-------------------------------------------------------------------
C! Print summary of non fatal errors
C! Derived from TPRSUM , filled by calls to RERROR
C!
C!    Author:  R. Johnson     03-06-87
C!    Modified:J.Knobloch     10-10-88
C!
C!------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "rrepjj.h"
#include "tstate.h"
#include "tptime.h"
#include "rlunit.h"
#include "tflags.h"
C
      PARAMETER (MXLEN=130,LMXTX=400)
      CHARACTER TEXT*140,TBUF*(LMXTX),CHAINT*4,CALID*8
C
C--------------------------------------------------------------------
C
#include "bmacro.h"
C
C--------------------------------------------------------------------
C
      WRITE(LOUTRL,20)
   20 FORMAT(//1X,T20,'Summary of errors during reconstruction')
      WRITE(LOUTRL,21)
   21 FORMAT(  1X,T20,'------- -- ------ ------ --------------'/)
C++   Print summary of error counts
C
      NRREP=NAMIND('RREP')
      KRREP=IW(NRREP)
      IF (KRREP.EQ.0) THEN
        WRITE(LOUTRL,69)
   69   FORMAT(/' There are no warning or',
     &          ' error messages from this job',
     &          ' from calls to RERROR'/)
      ELSE
        WRITE(LOUTRL,70)
   70   FORMAT(/' Summary of error and warning',
     &          ' messages from this job:')
        DO 200 IM=1,IW(KRREP+LMHROW)
          CALID(1:4)= CHAINT(ITABL(KRREP,IM,JRREB1))
          CALID(5:8)= CHAINT(ITABL(KRREP,IM,JRREB2))
          WRITE(LOUTRL,90) ITABL(KRREP,IM,JRREMC),
     &                     ITABL(KRREP,IM,JRRENC),CALID(1:6)
   90     FORMAT(/'   Error number ',I3,' occured ',I6,' time(s)',
     &            ' in subroutine ',A6,':')
C
C++       Extract the message from the BOS bank
C
          IND=NLINK('RREP',IM)
          IF (IND.EQ.0) GO TO 200
          LCHTX=0
          DO 93 J=1,IW(IND)
            IB1= J*4 - 3
            IF (IB1.GT.LMXTX) GO TO 94
            IB2= MIN(LMXTX,IB1+3)
            TBUF(IB1:IB2)=CHAINT(IW(IND+J))
            LCHTX=LCHTX+4
   93     CONTINUE
   94     CONTINUE
C
C++       Parse the message into separate lines separated by ampersands
C
          IFRST=1
          DO 100 I=1,LCHTX
            IF (TBUF(I:I).EQ.'&') THEN
              LENMS=MIN(MXLEN-5,I-IFRST)
              IF (LENMS.GT.0) THEN
                ILST=IFRST+LENMS-1
                TEXT= '('' +++ '//TBUF(IFRST:ILST)//''')'
                WRITE (LOUTRL,FMT=TEXT(1:LENMS+9))
              ENDIF
              IFRST=I+1
            ENDIF
  100     CONTINUE
          LENMS=MIN(MXLEN-5,LCHTX-IFRST+1)
          IF (LENMS.GT.0) THEN
            ILST=IFRST+LENMS-1
            TEXT= '('' +++ '//TBUF(IFRST:ILST)//''')'
            WRITE (LOUTRL,FMT=TEXT(1:LENMS+9))
          ENDIF
  200   CONTINUE
        CALL BDROP(IW,'RREP')
      ENDIF
C
      RETURN
      END
#endif
