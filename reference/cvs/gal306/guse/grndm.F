*DK grndm
      SUBROUTINE GRNDM(RNDMV,LRNDV)
C-----------------------------------------------------------------------
C - Author : F.RANJARD - 890608
C! GEANT user routine to get random numbers
C act as a server of 100 numbers from one RANECU call
CKEY GUSER RNDM RANECU / INTERNAL
C - Structure : subprogram
C               User Entry Name :GRNDM
C               External references :RANECU
C               Comdecks references : ARNDMC
C - Input  : LRNDV  = number of output random numbers
C - Output : RNDMV  = vector of random numbers in the range [0.,1.]
C
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
      REAL RNDMV(*)
#include "arndmc.h"
C
      IF(NRVEC+LRNDV.GT.LRVEC) THEN
#include "ranecu.h"
         NRVEC=0
      ENDIF
C
      IF (LRNDV.EQ.1) THEN
         RNDMV(1) = RVEC(NRVEC+1)
      ELSEIF (LRNDV.EQ.5) THEN
         RNDMV(1) = RVEC(NRVEC+1)
         RNDMV(2) = RVEC(NRVEC+2)
         RNDMV(3) = RVEC(NRVEC+3)
         RNDMV(4) = RVEC(NRVEC+4)
         RNDMV(5) = RVEC(NRVEC+5)
      ELSEIF (LRNDV.EQ.2) THEN
         RNDMV(1) = RVEC(NRVEC+1)
         RNDMV(2) = RVEC(NRVEC+2)
      ELSEIF (LRNDV.EQ.3) THEN
         RNDMV(1) = RVEC(NRVEC+1)
         RNDMV(2) = RVEC(NRVEC+2)
         RNDMV(3) = RVEC(NRVEC+3)
      ELSE
         DO 1 I=1,LRNDV
            RNDMV(I)=RVEC(NRVEC+I)
 1       CONTINUE
      ENDIF
      NRVEC = NRVEC +LRNDV
      RETURN
      END
#endif
