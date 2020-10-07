      SUBROUTINE LINIJO
C----------------------------------------------------------------------
C!   Initialisation of Lcal job
C!
C!   Author   : P. H. Hansen  870401
C!   Modified : P. H. Hansen  880315
C!   Modified : H. Meinhard   14-Mar-1990   LUPA initialization added
C!
C!   Description :
C!   ===========
C?   Set name indices for Lcal banks
C?   Set bank formats for printing
C?   Reset reconstruction job statistics
C?   Initialize the LUPA stuff
C!=====================================================================
#ifndef DOC
#include "bcs.h"
#include "lcnamc.h"
#include "bhabjj.h"
      COMMON/DWNSCL/ SCALE(2)
      EXTERNAL NAMIND
#include "bmacro.h"
C----------------------------------------------------------------------
C
C Name indices and printing formats
      CALL LBINDX
C
C Downscale factors
      SCALE(1) = 1000.*2000.
      SCALE(2) = 1000.*1000.
C
C Reset reconstruction statistics
      LEN = LMHLEN + 5
      CALL AUBOS('LSTA',0,LEN,KLSTA,IGARB)
      IF(IGARB.EQ.2)                               GOTO 998
      CALL VZERO(RW(KLSTA+1),LEN)
C
C LUPA initialization
      CALL LUPAIN
C
      GOTO 999
C
  998 CALL REPORT('LINIJO','AUBOS failure',1)
  999 CONTINUE
      END
#endif
