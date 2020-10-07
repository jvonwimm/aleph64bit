      SUBROUTINE HDGSUM
C*************************************************************
C! Digital Pattern statistics                                *
C!                                                           *
C! AUTHOR :  R. Tenchini      890126                         *
C!                                                           *
C! INPUT   HPCO                                              *
C!                                                           *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "alcons.h"
#include "rparac.h"
#include "rflags.h"
#include "hpcojj.h"
#include "hcjosu.h"
#include "bmacro.h"
C
      IHPCO=NLINK('HPCO',0)
      IF(IHPCO.EQ.0) THEN
         NPATT = 0
      ELSE
         NPATT = LROWS(IHPCO)
      ENDIF
      NPATHC=NPATHC+NPATT
      DO 100 I=1,NPATT
         NHIT   = ITABL(IHPCO,I,JHPCND)
         NHITHC = NHITHC + NHIT
 100  CONTINUE
      RETURN
      END
#endif
