      SUBROUTINE OINIRU
C----------------------------------------------------------------------
C!  - Initialisation for BOM analysis
C!
C!   Author:  R.W.Forty  23-May-91
C?
C!======================================================================
*
* June 17, 1996: O. Schneider: call routine OSLOWC to look in the run 
*                              header for banks that are normally found 
*                              in slow control records. 
#ifndef DOC
C
#include "bcs.h"
#include "bomflg.h"
#include "rflags.h"
#include "rparac.h"
C.......................................................................
C
C?   if MC switch off the BOM
C
      IF ( IW(NAMIND('AJOB')).NE.0 ) THEN
        FDETRF(JULBO) = .FALSE.
        GOTO 999
      ENDIF
C
C  Read banks from Data Base for this run
C
      CALL OMDATA
C
C  Calculate optics if Banks have been read
C
      IF (XINIFL) CALL OMBOPT
C
C     Get corrector and separator data
C
      CALL OSLOWC
C
  999 RETURN
      END
#endif
