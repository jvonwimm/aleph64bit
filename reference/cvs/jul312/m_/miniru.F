      SUBROUTINE MINIRU
C--------------------------------------------------------------------
C! Initialise the MUON run
C! G.Capon
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "ajobjj.h"
#include "bcs.h"
#include "rflags.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "bmacro.h"
C--------------------------------------------------------------------
C
C      initialise muon geometry description library
C
      CALL MGINIG (LRGEOM,IRUNRC,IERR,NEWNR)
      IF (IERR.NE.0) THEN
         CALL RERROR('MINIRU',1,'error reading DataBase')
         GO TO 999
      ENDIF
C
  999 CONTINUE
      RETURN
      END
#endif
