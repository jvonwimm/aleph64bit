      SUBROUTINE QJMDCL (NJETS,CNAM,ICLASS,ALPHA,DELTA,ETA,EVIS)
CKEY JETS /USER
C----------------------------------------------------------------------
C   Author   : P. Perez       28-MAR-1989
C
C   Description
C   ===========
C!   Set up and call QGMDCL Jet finding routine.
C  input   : ALPHA           weight
C            DELTA           half opening angle (degrees)
C            ETA             energy cut-off for jets(in units of Evis)
C            EVIS            visible energy
C           (if EVIS=0., it is computed from the input part. energies)
C   output: NJETS and QCTBUF.INC
C======================================================================
#ifndef DOC
      CHARACTER *(*) CNAM
#include "qctbuf.h"
C-----------------------------------------------------------------------
      CALL QJSETU(ICLASS)
C
      CALL QGMDCL(ALPHA,DELTA,ETA,EVIS)
C
      NJETS = KTBO
      IF (CNAM.NE.' ')  THEN
        IDR=0
        DO 1 IJET = NJETS, 1, -1
          CALL QJSAVE (CNAM,IJET,ICLASS,IDR,0,1)
          IDR=1
    1   CONTINUE
      ENDIF
C
      END
#endif
