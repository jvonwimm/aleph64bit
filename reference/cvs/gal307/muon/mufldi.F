*DK mufldi
      SUBROUTINE MUFLDI(IEMD,IBUS,LCLST,ILAST)
C
C*******************************************************************************
C
C T.Wang -860117
C
C           - modified for final dbase
C             by A. Antonelli, F. Bossi 1 July 89
C!      Routine to fill a digit into BOS bank 'MUDG'
C
C       Input:
C         IEMD  -- Atros number of the current digit
C         IBUS  -- Bus number of the current digit (0-25)
C         LCLST -- the length of cluster of the current digit
C         ILAST -- the last address of the cluster of the current digit (0-...)
C       Output:
C         in BOS bank 'MUDG'
C
C       Called by MUDGTZ
C       Calls ALBOS         in this .HLB
C
C*******************************************************************************
C
#ifndef DOC
      SAVE
#include "mudgpr.h"
#include "munamc.h"
#include "bcs.h"
#include "jobcom.h"
#include "iocom.h"
#include "mudgjj.h"
      INTEGER IWORD(4)
      INTEGER IMUD
#include "bmacro.h"
C
C       Get index for BOS bank 'MUDG'
C
          JMUDG = IW(NAMUDG)
C
          KMUDG = KNEXT(JMUDG)
C
          IWORD(1) = ILAST
          IWORD(2) = LCLST
          IWORD(3) = IBUS
          IWORD(4) = IEMD
          IMUD = IWORD(1)
          CALL MVBITS(IWORD(2),0,8,IMUD,8)
          CALL MVBITS(IWORD(3),0,8,IMUD,16)
          CALL MVBITS(IWORD(4),0,8,IMUD,24)
          IW(KMUDG + JMUDEA) = IMUD
          IW(JMUDG + LMHROW) = LROWS(JMUDG) + 1
      RETURN
      END
#endif
