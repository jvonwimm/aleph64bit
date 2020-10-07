      SUBROUTINE ECFCLU(IER)
C***********************************************************************
C! FIND ASSIGNED CLUSTERS (IE CLUSTERS + TRACKS ASSOCIATION )
C!     STEERING ROUTINE
C!     CALL PRINTING OF BANKS FOR DEBUG (ECDEBG)
C!     CLEAR CLUSTER ARRAY AFTER PROCESSING (ERESET)
C!
C!
C!       IER = return code ( 0 if normal)
C!
C***********************************************************************
#include "eclcom.h"
#include "rparac.h"
#include "rflags.h"
#ifndef DOC
      IER=0
      CALL ECCLUS(IER)
      IF (IER .NE. 0) GOTO 888
C
      CALL ECMESP
      CALL EFICLP
      CALL ECTHRE
C
      CALL ECTPC(IER)
      IF (IER .NE. 0) GOTO 888
      CALL ECLTRK(IER)
C
      CALL ECDEBG
C ZEROS ARRAY OF CLUST/ASSOC
 888  CALL ERESET
C
C
 999  RETURN
      END
#endif
