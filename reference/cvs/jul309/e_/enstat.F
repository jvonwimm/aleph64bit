      SUBROUTINE ENSTAT(IAD,IFL,IMEA)
C-----------------------------------------------------------------------
C! Returns noise status of tower address IAD
CKEY ECAL CLEAN / JULIA USER
C        B. Bloch    January 90
C  Structure : SUBROUTINE
C           External references:LOCTAB    (ALEPHLIB)
C           Comdecks references:BCS,BMACRO
C
C - Input  : IAD    = tower address ( theta,phi packed as in ETDI)
C
C - Output : IFL(3) = Stack Status flag : 0    ok
C                                         1   noisy ,to be reset
C                                         2   offset ,to be shifted
C            IMEA(3)= Offset value  for each stack if needed (Kev)
C - Banks  : input  - JFWORK       work bank of means and sigmas
C?
C!======================================================================
#ifndef DOC
      DIMENSION IFL(3),IMEA(3)
#include "etdipt.h"
      PARAMETER ( SIGI = 1.500   , SIGF = 20.00   , IMEX = 20000)
#include "bcs.h"
#include "bmacro.h"
C--------------------------------------------------------------------
       DO 8 IS = 1,3
       IMEA(IS) = 0
  8    IFL(IS) = 0
C      Look if the address IAD is in JFWORK
       IF (JFWORK.GT.0) THEN
          NFWORK = LROWS(JFWORK)
          IF (NFWORK.GT.0) THEN
             I = LOCTAB(IW(JFWORK+LMHLEN+1),LCOLS(JFWORK),NFWORK,1,IAD)
             IF (I.GT.0) THEN
C     This is,ok, fill flags and means
                DO 20 IS = 1,3
                   IMEA(IS) = NINT(RTABL(JFWORK,I,IS+1)*1000.)
                   IF( RTABL(JFWORK,I,IS+4).LT.SIGI .OR.
     $                 RTABL(JFWORK,I,IS+4).GT.SIGF ) THEN
                       IFL(IS) = 1
                   ELSE
                       IF (ABS(IMEA(IS)).GT.IMEX) IFL(IS) = 2
                   ENDIF
  20           CONTINUE
            ENDIF
         ENDIF
      ENDIF
 999  RETURN
      END
#endif
