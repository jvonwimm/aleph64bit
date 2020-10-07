      SUBROUTINE HDSPRE
C*************************************************************
C! SUPERPATTERN AND EXCOB RECONSTRUCTION                     *
C! (EXCOB NOW MOVED TO ECHCGL)                               *
C! AUTHOR  : G. BAGLIESI, R. TENCHINI 881005                 *
C!                                                           *
C!                                                           *
C! VERSION 1.0                                               *
C!                                                           *
C!                                                           *
C?
C!======================================================================
#ifndef DOC
#include "rcurnt.h"
#include "rflags.h"
#include "rparac.h"
#include "bcs.h"
#include "hdpcut.h"
#include "hspcmd.h"
C
C superpatterns recognition
C
      CALL HSPARE(NSPA,KISPHD)
      NSPATT=NSPA
C
C we compute the relevant superpatterns quantity
C
      IF(KISPHD.EQ.1)   THEN
         CALL HSPTRE(NSPA)
      ELSEIF(KISPHD.EQ.2)   THEN
         CALL HFLNOI(NSPA)
      ENDIF
      RETURN
      END
#endif
