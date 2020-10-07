      SUBROUTINE CCTRMI(ITR,IFMIP)
C***********************************************************************
C! Find if track is MIP at Ec exit (exact)                             *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870831                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : KINE,VERT                                              *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   ITR     : Geant track number (should be a primary track)          *
C!   IFMIP   : 1 if track is MIP                                       *
C!             0 if track is not MIP                                   *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "bcs.h"
      DIMENSION XV(3)
      LHEV3=3
      KKINE=NLINK('KINE',ITR)
      NVERT=IW(KKINE+LHEV3)
C
C?   say that the track is MIP
C
      IFMIP=1
      IF(NVERT.NE.1)THEN
         LPK=IW(KKINE+2)
C
C?   loop on secondary vertices
C
         DO 10 I=2,NVERT
            IVERT=IW(KKINE+LHEV3+LPK+I)
            KVERT=NLINK('VERT',IVERT)
            DO 11 IX=1,3
               XV(IX)=RW(KVERT+LHEV3+IX)
   11      CONTINUE
         CALL EX3IJK(XV,IP,JT,K,NR,IFL,KS,MO)
C
C?   If one vertex of the track is in the active region of Ecal,
C?              the track is not MIP
C
         IF(IFL.NE.4)THEN
            IFMIP=0
            GO TO 999
         ENDIF
   10    CONTINUE
      ENDIF
  999 CONTINUE
      RETURN
      END
#endif
