      SUBROUTINE ESLOWC
C----------------------------------------------------
C!    Handle slow control for ECAL
C SLOW CONTROL TEMPORARILY USED FOR ECAL PRESCAN
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
      KEPRS=IW(NAMIND('EPRS'))
      IF(KEPRS.NE.0) CALL EPADCL
      END
#endif
