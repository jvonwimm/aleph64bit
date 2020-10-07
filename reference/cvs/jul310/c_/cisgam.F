      SUBROUTINE CISGAM(IEC,IFGAM)
C***********************************************************************
C!Say if an ecob is gamma                                              *
C!                                                                     *
C!  TEMPORARY
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   :
C!    OUTPUT  :
C!    CREATED :
C!                                                                     *
C!   iec   = ecob number
C!   ifgam = 0 if not gamma
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "ecnamc.h"
#include "bcs.h"
#include "ecobjj.h"
#include "bmacro.h"
      IFGAM=0
C   ECOB bank.
      KECOB=IW(NAECOB)
C
      IT = ITABL(KECOB,IEC,JECOPF)
      IF( IT .EQ. 0 .OR. IT .EQ. 2 ) IFGAM = 1
      RETURN
      END
#endif
