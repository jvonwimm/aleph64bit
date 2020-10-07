      SUBROUTINE CALSUP(ISUPP,KBANK)
C!**********************************************************************
C!Suppress element isupp in bank at index kbank                        *
C!                                                                     *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : BANK                                                   *
C!    OUTPUT  : BANK                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   isupp   = number of the line to suppress                          *
C!   kbank   = index of the bank which has relations with the ecob bank*
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "bmacro.h"
      NLIN=LROWS(KBANK)-1
      NCOLS=LCOLS(KBANK)
      DO 10 I= ISUPP, NLIN
   10    CALL UCOPY(IW(KROW(KBANK,I+1)),IW(KROW(KBANK,I)),NCOLS)
      IW(KBANK+LMHROW)=NLIN
      RETURN
      END
#endif
