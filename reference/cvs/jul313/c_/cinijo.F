      SUBROUTINE CINIJO
C***********************************************************************
C! Initialise job for calo process                                     *
C!   set up bank formats and name indices                              *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870831                                    *
C!                                                                     *
C!                                                                     *
C!    OUTPUT  : NONE                                                   *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "ccnamc.h"
#include "ecnamc.h"
#include "pcnamc.h"
C? Book banks for POT creation
      NAPCRL = NAMIND('PCRL')
      NAPCOB = NAMIND('PCOB')
      NAPCHY = NAMIND('PCHY')
      NAPCPA = NAMIND('PCPA')
      NAPEOB = NAMIND('PEOB')
      NAPEHY = NAMIND('PEHY')
      NAPEOT = NAMIND('PEOT')
      NAPEST = NAMIND('PEST')
      NAPKST = NAMIND('PKST')
      NAPECO = NAMIND('PECO')
      NAPEPT = NAMIND('PEPT')
      NAPHCO = NAMIND('PHCO')
C
      NACCT1=NAMIND('CCT1')
      NACTC2=NAMIND('CTC2')
      NACRL3=NAMIND('CRL3')
      NACOCR=NAMIND('COCR')
      NACALO=NAMIND('CALO')
      NACHYP=NAMIND('CHYP')
      NACPAR=NAMIND('CPAR')
      NACHRL=NAMIND('CHRL')
      NACHPR=NAMIND('CHPR')
C
C? Get run condition for calrun
C
      CALL CGRUNC
      RETURN
      END
#endif
