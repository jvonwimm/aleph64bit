      SUBROUTINE ECERAS
C----------------------------------------------------------------------
C!  - Erase Ecal E-flow Pot bank when reprocessing is done from POT
C!   Recalculate uncorrected raw energy ETDI EWHE
C!   Recreate EWDI bank
C!
C!   Author  : M.N. Minard                 08/01/91
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
      KRPRO = IW(NAMIND('REPR'))
      IF ( KRPRO.NE.0 ) THEN
C
C-      Save LCAL info and Erase Ecal Pot bank
C
        CALL LCSAVE
C
C
        CALL BDROP(IW,'PESTPECOPRPW')
C
C       Erase E-Flow pot banks
C
        CALL BDROP(IW,'EFOLECTEHCTEPRTMPMSKDHEADMJT')
C
C       Recalculate EWDI bank
C
        CALL EWDIFP
C
C       Correct ETDI , EWHE for already corrected energy
C
        CALL ETDIFP
        CALL BDROP(IW,'PEWIPWEI')
      ENDIF
      RETURN
      END
#endif
