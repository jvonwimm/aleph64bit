      SUBROUTINE CMONI(EPPOS,EPTOT)
C***********************************************************************
C! Summ PCPA energies                                                  *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  900130                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : PCPA                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!   EPPOS and EPTOT are the sums of neutral calorimeter particles :   *
C!                                                                     *
C!   EPPOS = Sum of energies of particles when energy is >0            *
C!   EPTOT = Sum of energies of all particles                          *
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "pcpajj.h"
#include "bmacro.h"
      KPCPA=NLINK('PCPA',0)
      EPPOS=0.
      EPTOT=0.
      IF(KPCPA.EQ.0)GO TO 999
      NPCPA=LROWS(KPCPA)
      DO 10 IPCPA=1,NPCPA
      EP=RTABL(KPCPA,IPCPA,JPCPEN)
      IF(EP.GT.0)EPPOS=EPPOS+EP
      EPTOT=EPTOT+EP
   10 CONTINUE
  999 CONTINUE
      RETURN
      END
#endif
