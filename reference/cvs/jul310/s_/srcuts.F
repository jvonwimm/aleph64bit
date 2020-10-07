      SUBROUTINE SRCUTS
C----------------------------------------------------------------------
C! Fill SATR reconstruction cuts common
C!
C!    Author:     H. Meinhard       23-Mar-1987
C!    Modified:   H. Meinhard       13-Mar-1990  (4)
C!
C!    Description
C!    ===========
C!    SATR reconstruction cut parameters are filled into common block
C!    SRECPC.
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rcurnt.h"
#include "rlunit.h"
#include "srecpc.h"
#include "srktjj.h"
      INTEGER ALGTDB
      EXTERNAL ALGTDB
#include "bosext.h"
#include "bmacro.h"
C----------------------------------------------------------------------
      ISTAT = ALGTDB(LRCONS,'SRKT',IRUNRC)
      IF (ISTAT .EQ. 0)                                     GOTO 901
C
      KSRKT = IW(NAMIND('SRKT'))
C
      TLOWSR = RTABL(KSRKT,1,JSRKTL)
      TUPPSR = RTABL(KSRKT,1,JSRKTU)
      RUPPSR = RTABL(KSRKT,1,JSRKRU)
      SIGMSR = RTABL(KSRKT,1,JSRKSI)
      DTHCSR = RTABL(KSRKT,1,JSRKDT)
      DSECSR = RTABL(KSRKT,1,JSRKDS)
      DGASSR = RTABL(KSRKT,1,JSRKDG)
      NPARSR = ITABL(KSRKT,1,JSRKNP)
      CHPRSR = RTABL(KSRKT,1,JSRKCH)
      ACTRSR = RTABL(KSRKT,1,JSRKCT)
      ACPASR = RTABL(KSRKT,1,JSRKCP)
      IF (IW(KSRKT) .GE. LSRKTA) THEN
        ZONWSR = RTABL(KSRKT,1,JSRKZW)
        SIG2SR = RTABL(KSRKT,1,JSRKST)
        NPA2SR = ITABL(KSRKT,1,JSRKMP)
        NDR2SR = ITABL(KSRKT,1,JSRKMD)
      ELSE
C bank SRKT incomplete (old dbase version?); set default values
        ZONWSR = 1.1
        SIG2SR = .09
        NPA2SR = 4
        NDR2SR = 1
      ENDIF
C
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      CALL RERROR('SRCUTS',-IER,'No SRKT bank from database')
  999 CONTINUE
      RETURN
      END
#endif
