      SUBROUTINE ENLCAL(CLAS7,CLAS8)
C----------------------------------------------------------------------
CKEY EDIR DEF CLASS7->8
C! Luminosity from LCAL.
C-
C   Input  : None
C   Output : CLAS7 = Class 7 logical flag
C            CLAS8 = Class 8 logical flag
C-
C   Called by   : SELEVT
C   Calls  : None
C   Input banks : LUPA
C-
C                                        Author: M. Talby September 89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "lupajj.h"
C --
      LOGICAL CLAS7,CLAS8
C --
#include "bmacro.h"
C --
      ELCALA = 0.
      ELCALB = 0.
      CLAS7 = .FALSE.
      CLAS8 = .FALSE.
C --
      KLUPA=IW(NAMIND('LUPA'))
      IF(KLUPA.LE.0) GOTO 999
C --
C   Loop on LCAL clusters and get separately the energy
C   of the LCAL sides A and B
C --
      NLCTW = LROWS(KLUPA)
      DO 40 NLT = 1,NLCTW
        ELCALA = ELCALA+RTABL(KLUPA,NLT,JLUPEC)
        ELCALB = ELCALB+RTABL(KLUPA,NLT,JLUPEC+1)
   40 CONTINUE
C --
      IF(ELCALA.GT.15 .AND. ELCALB.GT.15.) CLAS7 = .TRUE.
      IF(ELCALA.GT.15 .OR. ELCALB.GT.15.)  CLAS8 = .TRUE.
C --
  999 RETURN
      END
#endif
