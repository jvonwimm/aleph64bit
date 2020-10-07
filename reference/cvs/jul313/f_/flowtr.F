      SUBROUTINE FLOWTR(XTRACK)
C========================================================================
C!   Calculate Track energy
C-
C-   Authors : M.N. Minard , M. Pepe       15/09/89
C-
C
C?
C!======================================================================
#ifndef DOC
#include "ecnmsk.h"
#include "frftjj.h"
#include "alcons.h"
#include "ectcom.h"
#include "rparac.h"
#include "bcs.h"
      DIMENSION PV(4)
      DIMENSION EVECT(4)
      EXTERNAL NAMIND
#include "bmacro.h"
C
C
      IERR = 0
C
C-    Loop on tracks
C
      KFRFT = IW (NAMIND('FRFT'))
      IF ( KFRFT.EQ.0 ) THEN
         GO TO 550
      ENDIF

      XTRACK = 0.
C
C-    Loop on tracks
C
      NTRAK = LROWS ( KFRFT )
      DO 500 ITRAK = 1, NTRAK
      JFRFT = KROW ( KFRFT , ITRAK )
C
C-    Check if track is of good quality
C
      CALL ECHTAG( ITRAK,IFLAG,EVECT)
      IF ( IFLAG.NE.0)  THEN
           GO TO 500
      ENDIF
C
C-    Get track momentum
C

      RIN    = -RW   ( JFRFT+JFRFIR)
      TLAM   =  RW   ( JFRFT+JFRFTL)
      PHI0   =  RW   ( JFRFT+JFRFP0)
      Q      =  SIGN ( 1.,RIN)
      R0     =  Q/RIN
      PV(1)  =  R0 * COS(PHI0)
      PV(2)  =  R0 * SIN(PHI0)
      PV(3)  =  R0 * TLAM
C
C-    Convert from cm to GeV
C
      DO 270 I = 1,3
       PV(I) = PV(I)*AAAKEC*FILDEC
 270  CONTINUE

      PTOT  =  SQRT(PV(1)**2+PV(2)**2+PV(3)**2)
      XTRACK = XTRACK+ PTOT
C
C-    Sum track energy
 500  CONTINUE
 550  CONTINUE
      RETURN
      END
#endif
