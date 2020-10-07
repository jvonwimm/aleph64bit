      SUBROUTINE TCOTHR (IPADA, RPULS, PRFS2, RPHI, IERY)
C
C---------------------------------------------------------------------
C!  TPC coordinates finding : threshold algorithm, using two "2 pads
C!  algorithms" (C). Uses 3 pads.
C!
C!  Author:    T. Pun      12-06-86
C!  Modified:  R. Johnson   8-04-88
C!
C!  INPUT :
C!    I4/  IPADA      : indice of 1st pad of the three, in [1 .. NPADS].
C!    R4/  RPULS(ip): p heights for this cluster (in [1 ... 255] !).
C!    R4/  PRFS2    : width**2 (>0 !) of PRF.
C!
C!  INPUT from COMMON /TPCCUT/ :
C!    R4/  ALGPTP   : threshold (>1) to choose ratio.
C!                   Assuming C2>C3>C1: (C2/C3) > ALGPTP, uses $C1,C3;
C!                                         "    <=   "  , uses $C2,C3.
C!  OUTPUT :
C!    R4/  RPHI     : coordinate, in [1..NPADS].
C!    I4/  IERY     : error flag = 131: threshold <= 1.
C!
C!  Called by TCOALG
C!
C!-------------------------------------------------------------------
#ifndef DOC
C
#include "tpccut.h"
C
      DIMENSION RPULS(*)
C
      IERY = 0
      RPHI = 0.0
C
      IF (ALGPTP .LE. 1.0) THEN
         IERY = 131
         RETURN
      ENDIF
C
      IPADB = IPADA + 1
      IPADC = IPADA + 2
C
      PUL1 = RPULS(IPADA)
      PUL2 = RPULS(IPADB)
      PUL3 = RPULS(IPADC)
C
      IF (PUL1 .GE. PUL3) THEN
         IF ((PUL2/PUL1) .GT. ALGPTP) THEN
            RPHI = FLOAT(IPADA) + 1.0 + 0.5*PRFS2*
     1      ( ALOG(RPULS(IPADC)) - ALOG(RPULS(IPADA)) )
         ELSE
            RPHI = FLOAT(IPADA) + 0.5 + PRFS2*
     1      ( ALOG(RPULS(IPADB)) - ALOG(RPULS(IPADA)) )
         ENDIF
      ELSE
         IF ((PUL2/PUL3) .GT. ALGPTP) THEN
            RPHI = FLOAT(IPADA) + 1.0 + 0.5*PRFS2*
     1      ( ALOG(RPULS(IPADC)) - ALOG(RPULS(IPADA)) )
         ELSE
            RPHI = FLOAT(IPADB) + 0.5 + PRFS2*
     1      ( ALOG(RPULS(IPADC)) - ALOG(RPULS(IPADB)) )
         ENDIF
      ENDIF
C
      RETURN
      END
#endif
