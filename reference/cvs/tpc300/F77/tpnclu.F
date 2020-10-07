*DK tpnclu
      SUBROUTINE TPNCLU(NCL)
C-----------------------------------------------------------------------
C!  Return the size of a cluster from a non-delta-ray primary.
C
C  Called from:  TSDEDX
C  Calls:        RNDM
C
C  Inputs:  /TPCONS/     --EBINC,  table of probabilities of cluster
C                                  sizes, from Lapique and Puiz.
C                        --MXBINC, number of slots in EBINC
C                        --CONCLU,
C                          POWERC, constants used to calculate probabi-
C                                  lities (in CONCLU/E**POWERC) of the
C                                  cluster size distribution.
C
C  Outputs:  PASSED:     --NCL, the number of electron in the cluster
C
C  A. Caldwell
C  Modifications :
C    1.- P. Janot    18 May 1988   --Change the analytic form of cluster
C                                    size distribution from 1/E to
C                                    (1/E)**(1+x). x is determined to
C                                    reproduce TPC90 data.
C-----------------------------------------------------------------------
#include "tpcons.h"
C
 1    X = RNDM(F)
C
      DO 2 I = 1,MXBINC
C
         IF ( X .LE. EBINC(I) ) THEN
            NCL = I
            RETURN
         ENDIF
C
 2    CONTINUE
C
      SUMPRB = EBINC(MXBINC)
C
      DO 3 I = MXBINC+1,MXCL
C
          SUMPRB = SUMPRB + CONCLU/FLOAT(I)**POWERC
C
          IF ( X .LE. SUMPRB ) THEN
              NCL = I
              RETURN
          ENDIF
C
 3    CONTINUE
C
C  We've exceeded largest cluster, go dice again
C
      GOTO 1
C
      END
