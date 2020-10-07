      SUBROUTINE YHUSER
C
C----------------------------------------------------------*
C!    BOOK HISTOS FOR USER ROUTINE
CKEY YTOP BOOK HISTOGRAMS
C!    AUTHOR :     G. LUTZ  AUGUST 1991
C!
C!
C!    Description
C!    ===========
C!    THIS ROUTINE BOOKS HISTOGRAMS FOR THE USER ROUTINES
C!    IT IS CALLED FROM ROUTINE YTIJOB
C!
C!
C!---------------------------------------------------------*
#ifndef DOC
      SAVE
#include "bcs.h"
#include "ydimto.h"
#include "ydumto.h"
C!---------------------------------------------------------*
C
#if defined(YDEBUG)
      LOGICAL LDMP1,LDMP2
#endif
C
#include "bmacro.h"
C
C-- Define the logical unit for printout
C
      LOUT = IW(6)
C
C
C  ENTRY ONCE PER EVENT
C
#if defined(YDEBUG)
      IF(IDUMPP(39).GE.1) THEN
        LDMP1=.TRUE.
      ELSE
        LDMP1=.FALSE.
      ENDIF
C
      IF(IDUMPP(40).GE.1) THEN
        LDMP2=.TRUE.
      ELSE
        LDMP2=.FALSE.
      ENDIF
C
      IDUMPP(39)=IDUMPP(39)-1
      IDUMPP(40)=IDUMPP(40)-1
C
#endif
C
C
C
      RETURN
      END
#endif
