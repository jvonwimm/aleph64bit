      SUBROUTINE ITRAK
C-----------------------------------------------------------------------
C! ITC tracking prog.
C!
C!    Author      :- J. Sedgbeer 88/03/29
C!    Modified    :- J. Sedgbeer 88/05/09
C!    Modified    :- I. Tomalin  88/08/25
C!
C!   Input:
C!      commons:     /IWBNKT/, /BCS/
C!   Output:
C!      commons:     /ITTIME/
C!
C!   called by : ITCTRK
C!   calls     : ITLNK2,ITREE,IFIND2
C!
C!   Libraries required: none
C!
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
C I/O Commons etc.
#include "iwbnkt.h"
#include "ittime.h"
#include "bcs.h"
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C Initialise
C
      ITRY  = 3
C
C Find all links with respectable curvatures.
C
      IF( IFITTI ) CALL ALTIME ( TIME1 )
      CALL ITLNK2(ITRY,IERR)
      IF( IFITTI ) CALL ALTIME ( TIME2 )
      TTLNK2=TTLNK2+(TIME2-TIME1)
      IF (IERR.NE.0) GOTO 999
C
C Form elementary trees from the links.
C
      IF( IFITTI ) CALL ALTIME ( TIME1 )
      CALL ITREE(ITRY,2,IERR)
      IF( IFITTI ) CALL ALTIME ( TIME2 )
      TTREE2=TTREE2+(TIME2-TIME1)
      IF (IERR.NE.0) GOTO 999
C
C Find paths
C
      IF( IFITTI ) CALL ALTIME ( TIME1 )
      CALL IFIND2(ITRY)
      IF( IFITTI ) CALL ALTIME ( TIME2 )
      TFIND2=TFIND2+(TIME2-TIME1)
C
  999 CONTINUE
      END
#endif
