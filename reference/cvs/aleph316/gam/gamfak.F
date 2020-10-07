      SUBROUTINE GAMFAK( IGAM, PFAKE )
C.----------------------------------------------------------------------
C   M.Verderi       Creation 26/09/94
C!   Returns the real array PFAKE(4) = output of fake photon analysis
C   Input : IGAM  photon number of last gampex call.
C   Output:
C           PFAKE (01) Fake photon likelihood with electro. hypothesis
C           PFAKE (02) Fake photon likelihood with hadroni. hypothesis
C           PFAKE (03) Float ( 1000 * Iwarn(Electro) + IWarn(Hadron) )
C           PFAKE (04) Float ( Gampex photon number of parent derived
C                      the electrom. analysis)
C
C   Called by USER
C.----------------------------------------------------------------------
#ifndef DOC
#include "gafakp.h"
      REAL PFAKE(4)
C
C
      PFAKE ( 1) = PF (  1, IGAM )
      PFAKE ( 2) = PF (  4, IGAM )
      PFAKE ( 3) = 1000. * PF (  3, IGAM ) + PF (  5, IGAM )
      PFAKE ( 4) = PF (  2, IGAM )
      RETURN
      END
#endif
