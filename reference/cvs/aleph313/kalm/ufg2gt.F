      SUBROUTINE UFG2GT(CF,CW,TW,IE,FP)
C!  Read the UFG2 bank for the double Gaussian parameters from Dbase or
C!  data cards, or return defaults if no UFG2 found
C!  Returns:     CF(R*4)     Fraction of scatters in the core Gaussian
C!               CW(R*4)     Width of the core Gaussian,
C!                           normalized to default = 1
C!               TW(R*4)     Width of the tail Gaussian
C!               IE(I*4)     If 0, No energy loss, if 1, energy loss by
C!                           particle,
C!               FP(R*4)     Probability cut to filter (remove) a
C!                           coordinate
C!  If no UFG2 card, do a single Gaussian, no energy loss or filtering
#ifndef DOC
#include "ufg2jj.h"
#include "bcs.h"
      DATA NAUFG2/0/
#include "bmacro.h"
C ---------------------------------------------------------------------
C
      IF (NAUFG2.EQ.0) THEN
         NAUFG2 = NAMIND('UFG2')
      ENDIF
C
      KUFG2 = IW(NAUFG2)
C  If no UFG2 bank (card) found, read from database
      IF (KUFG2.LE.0) THEN
        KUFG2= MDARD (IW,JUNIDB(0),'UFG2',0)
        IF(KUFG2 .LE. 0)THEN
          CF = 1.0
          CW = 1.0
          TW = 2.0
          IE = 0
          FP = 0.
          CALL ALTELL('UFG2GT - no UFG2 bank; using single Gaussian',
     &      1,'RETURN')
          RETURN
        ENDIF
      ENDIF
      CF = MIN(MAX(RTABL(KUFG2,1,JUFGCF),0.),1.)
      CW = MAX(RTABL(KUFG2,1,JUFGCW),0.01)
      TW = MAX(RTABL(KUFG2,1,JUFGTW),0.01)
      FP = MIN(MAX(RTABL(KUFG2,1,JUFGFP),0.),1.)
      IE = MIN(MAX(ITABL(KUFG2,1,JUFGDE),0),1)

      RETURN
      END
#endif
