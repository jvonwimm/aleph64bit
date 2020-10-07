      SUBROUTINE HPREDA
C--------------------------------------------------------------------
C! Prepare Hcal Data (steering routine)
C
C      G.Capon               date : 861001
C
C      Modified R.Tenchini 890720 to reprocess the POT
C
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rparac.h"
#include "hdebug.h"
#include "rlunit.h"
      LOGICAL REPR
C
      REPR = IW(NAMIND('REPR')).NE.0
C
      CALL BLIST(IW,'T+','CHRL')
      CALL BLIST(IW,'T+','HSDAHSTO')
      CALL BLIST(IW,'T+','HSDAHSTOHCSRHSPHHSPE')
      CALL BLIST(IW,'E+','HCLU')
      CALL BLIST(IW,'E+','HTUB')
      CALL BLIST(IW,'E+','HMAD')
      CALL BLIST(IW,'E+','HPCO')
      CALL BLIST(IW,'E+','HPDS')
      CALL BLIST(IW,'E+','CEXT')
      CALL BLIST(IW,'E+','PHST')
      CALL BLIST(IW,'E+','PHHY')
      CALL BLIST(IW,'E+','PHOB')
      CALL BLIST(IW,'E+','PHTO')
      CALL BLIST(IW,'E+','PPOB')
      CALL BLIST(IW,'E+','PPDS')
      CALL BLIST(IW,'E+','PPHY')
C
C CHEKING IF WE READ A POT
C
      KHTUB=NLINK('HTUB',0)
      KPHST=NLINK('PHST',0)
      IF( REPR ) THEN
         CALL BDROP(IW,'PPDSPPOBPHHYPHCOPPHY')
         CALL HPRANP
         CALL BDROP(IW,'PHST')
      ELSE
C
C?               prepare analog data
C
         CALL HPRANA(NHVST)
C
C?              prepare digital data
C
         CALL HPRDIG(NHTUB)
C
         IF(IHPRIN.GE.1)WRITE(LDEBRL,77)NHVST,NHTUB
      ENDIF
      IF(IHPRIN.GE.3) THEN
         CALL PRTABL('HSDA',0)
         CALL PRTABL('HSTO',0)
         CALL PRTABL('HTUB',0)
      ENDIF
      RETURN
 77   FORMAT(I5,' HCAL STOREYS AND',I5,' HCAL TUBES HITS IN THIS EVENT')
      END
#endif
