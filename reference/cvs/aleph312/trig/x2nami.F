      SUBROUTINE X2NAMI
C
C --------------------------------------------------------
C! Assign level2 trigger bos bank name indices.
C
C    Author : T.Medcalf  10/9/87
C    Modified T.M.       21/10/88
C
C? initialses name indices for track,hit,theta bin and mask
C? bos banks. Also sets the bank format.
C
C --------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "x2namc.h"
C
      EXTERNAL NAMIND
C
      NAX2DF = NAMIND('X2DF')
      CALL BKFMT('X2DF','I')
      NAX2TB = NAMIND('X2TB')
      CALL BKFMT('X2TB','I')
C
      NAX2MS = NAMIND('X2MS')
      CALL BKFMT('X2MS','I')
C
C  --- set workbank indices to zero.
C
      CALL VZERO (JWORKB,NUMWB)
      CALL VZERO (JX2TRK,NZONE)
      CALL VZERO (JX2SOR,NZONE)
C
      RETURN
      END
#endif
