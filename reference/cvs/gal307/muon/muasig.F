*DK muasig
      SUBROUTINE MUASIG
C -------------------------------------------------------------------------
C - F.Ranjard - 860626
C - End of tracking : reduce size of named banks
C - Called by   ASPEVE                              from this .HLB
C               AUBPRS                              from this .HLB
C ----------------------------------------------------------
#ifndef DOC
#include "jobcom.h"
#include "iocom.h"
#include "bcs.h"
#include "munamc.h"
C ------------------------------------------------------------------------
C
C - Reduce bank size to real one
C
      CALL AUBPRS ('MUHT')
C
C - Debug
C
      IF (FDEBJO .AND. IPRIJO(8).EQ.1) THEN
         CALL MUPRHT
      ENDIF
C
   10 RETURN
      END
#endif
