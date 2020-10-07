      SUBROUTINE YTIRUN
C
C----------------------------------------------------------*
C!    Initialize the run for TOPOLOGY reconstruction
C!    from JULIA
C!
C!    Author :     M. Bosman    1/2/89
C!
C!    Description
C!    ===========
C!    This routine is called once at each RUN beginning to
C!    initialize the TOPOLOGY part of the reconstruction
C!    program.
C!    It calls the routine YTOIRU
C!
C!---------------------------------------------------------*
#ifndef DOC
#include "rcurnt.h"
#include "rconds.h"
      CALL YTOIRU(IRUNRC,FIELRC)
      RETURN
      END
#endif
