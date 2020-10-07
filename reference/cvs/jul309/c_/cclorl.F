      SUBROUTINE CCLORL(XRL3)
C
C***********************************************************************
C!CLOSE TRK-ECOB-HCLU RELATION                                         *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870615                                    *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : XRL3                                                   *
C!    OUTPUT  : XRL3                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C!   UPDATE THE LENGTH OF CLUSTER-TRACK RELATIONS BANK FROM            *
C!   ITS ORIGINAL MAXIMUM LENGTH TO ITS ACTUAL LENGTH                  *
C!                                                                     *
C!     INPUT  = NUMBER OF RELATIONS                                    *
C!     OUTPUT = SIZE OF XRL3 BANK                                      *
C!                                                                     *
C***********************************************************************
#ifndef DOC
C
#include "bcs.h"
      CHARACTER*(*) XRL3
#include "bmacro.h"
      KRLCT=NLINK(XRL3,0)
      NREL=LROWS(KRLCT)
      NCOLS=LCOLS(KRLCT)
      CALL AUBOS(XRL3,0,NREL*NCOLS+LMHLEN,KERL3,IBID)
  999 RETURN
      END
#endif
