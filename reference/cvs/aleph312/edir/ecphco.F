      SUBROUTINE ECPHCO(IPHCO,IGOO)
C-----------------------------------------------------------------------
CKEY EDIR DIGITAL PATTERN
C! Look id digital pattern linkes to PHCO object.
C-
C   Input  IPHCO : Index of actual PHCO
C   Output IGOO  : = 1 if at least 1 digital pattern linked
C-
C   Called by   : ESWEH
C   Calls  : MAKLIS
C   Input banks : PPOB,PCRL
C-
C                                    Author: M.N.Minard  - 910400
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "pcrljj.h"
C --
      PARAMETER(LENMAX=1000)
      DIMENSION IPLIS(LENMAX)
      EXTERNAL NLINK
C --
#include "bmacro.h"
C --
      IGOO = 0
      IPPOB=NLINK('PPOB',0)
      IF(IPPOB.EQ.0) GO TO 999
      IPCRL=NLINK('PCRL',0)
      IF(IPCRL.EQ.0) GO TO 999
      CALL MAKLIS(IPCRL,JPCRPH,JPCRPP,IPHCO,NMATCH,IPLIS,IER)
      IF(IER.NE.0) THEN
         IF(IER.GT.0) THEN
           IF(IW(6).GT.0) WRITE(IW(6),*) 'ECPHCO_error in MAKLIS ',IER
           GO TO 999
         ENDIF
      ENDIF
      IF(NMATCH.GT.0) IGOO = 1
C --
 999  RETURN
      END
#endif
