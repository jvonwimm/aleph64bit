      SUBROUTINE ERESET
C **********************************************************************
C!RESET  ARRAY OF STOREYS                                              *
C!                                                                     *
C!    CLEAR IARRAY(6144)  AND WORK BANK USED FOR CLUSTERING            *
C!  AUTHOR   : A. BONISSENT  861108                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C***********************************************************************
#ifndef DOC
C
#include "bcs.h"
#include "eclcom.h"
#include "ecnamc.h"
#include "esdajj.h"
#include "bmacro.h"
C
C ZERO THE COMPLETE IARRAY  (ESDA CHANGED BY DEAD STOREY)
      CALL VZERO(IARRAY,6144)
C? DROP WORK BANK
      CALL WDROP(IW,KEWLOC)
      RETURN
      END
#endif
