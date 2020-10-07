      SUBROUTINE HDPREC
C*************************************************************
C! Pattern reconstruction                                    *
C!                                                           *
C! AUTHOR  : G. BAGLIESI  871116                             *
C!                                                           *
C!                                                           *
C! VERSION 1.0                                               *
C!                                                           *
C!                                                           *
C?
C!======================================================================
#ifndef DOC
#include "rlunit.h"
#include "rflags.h"
#include "rparac.h"
#include "bcs.h"
#include "hdpcut.h"
C
C   PATTERN RECOGNITION
C
       CALL HRCPAT(NPATT)
C
C  The relevant pattern quantities are calculated
C
       CALL HPATCO(NPATT)
C
C  Satellites are included
C
       CALL HRCOMP
C
C  Run statistics, Hist. filling, Debugging
C
                             CALL HDGSUM
      IF(JHISRF(JULHC).GE.1) CALL HDGFIL
      IF(JDBDRF(JULHC).GE.3) CALL HDGDEB
      RETURN
      END
#endif
