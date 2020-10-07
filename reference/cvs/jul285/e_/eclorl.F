      SUBROUTINE ECLORL
C
C***********************************************************************
C!CLOSE TRK-CLUSTER RELATION                                           *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  860615                                    *
C!  MODIFIED : A. BONISSENT  861108                                    *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ERL3                                                   *
C!    OUTPUT  : ERL3                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C!   UPDATE THE LENGTH OF CLUSTER-TRACK RELATIONS BANK FROM            *
C!   ITS ORIGINAL MAXIMUM LENGTH TO ITS ACTUAL LENGTH                  *
C!                                                                     *
C!     INPUT  = NUMBER OF RELATIONS                                    *
C!     OUTPUT = SIZE OF RLCT BANK                                      *
C!                                                                     *
C***********************************************************************
#ifndef DOC
C
#include "bcs.h"
#include "erl3jj.h"
#include "ecnamc.h"
#include "bmacro.h"
      KRLCT=IW(NAERL3)
      NREL=LROWS(KRLCT)
      NCOLS=LCOLS(KRLCT)
      CALL AUBOS('ERL3',0,NREL*NCOLS+LMHLEN,KERL3,IBID)
  999 RETURN
      END
#endif
