      SUBROUTINE IBNKIN
C-----------------------------------------------------------------------
C! Initialise ITC name indices and bank formats.
C!
C!    Author       : J. Sedgbeer  87/03/05
C!    Modified     : I. Tomalin   88/08/25
C!    Last Modified: J. Sedgbeer  90/01/31
C!
C!   Input:  none
C!
C!   Output:
C!     common /ITNAMC/   pointers to ITC banks.
C!
C!   called by : IINIJO
C!   calls     : none
C!
C!   libraries:  BOS
C!
C-----------------------------------------------------------------------
#ifndef DOC
C I/O Commons
#include "itnamc.h"
      EXTERNAL NAMIND
C-----------------------------------------------------------------------
C
      NAITDI=NAMIND('ITDI')
C
      NAITCO=NAMIND('ITCO')
C
      NAIDCR=NAMIND('IDCR')
C
      NAPIDI=NAMIND('PIDI')
C
      NAITFT=NAMIND('ITFT')
C
      NAIGTL=NAMIND('IGTL')
C
      NAIGCL=NAMIND('IGCL')
C
      NAIPJT=NAMIND('IPJT')
C
      NAIRJT=NAMIND('IRJT')
C
      END
#endif
