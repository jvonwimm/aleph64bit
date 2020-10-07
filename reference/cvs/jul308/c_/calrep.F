      SUBROUTINE CALREP(ISUPP,IREP,KBANK,L)
C!**********************************************************************
C!Replace calobj isupp by irep in bank                                 *
C!                                                                     *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : BANK                                                   *
C!    OUTPUT  : BANK                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   isupp   = number of the ecob to suppress                          *
C!   irep    = number by which isupp will be replaced                  *
C!   kbank   = index of the bank which has relations with the ecob bank*
C!   l       = offset of the colums which carries the relation         *
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "bmacro.h"
      NLIN=LROWS(KBANK)
      DO 10 IL=1,NLIN
      ICALO=ITABL(KBANK,IL,L)
      IF(ICALO-ISUPP)10,20,30
   20    IW(KROW(KBANK,IL)+L)=IREP
      GO TO 10
   30    IW(KROW(KBANK,IL)+L)=ICALO-1
   10 CONTINUE
      RETURN
      END
#endif
