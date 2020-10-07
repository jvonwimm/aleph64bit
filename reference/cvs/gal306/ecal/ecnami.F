*DK ecnami
      SUBROUTINE ECNAMI
C --------------------------------------------------------
C - F.Ranjard - 860422
C - define formats and name-indices of EC BOS banks
C   set working bank indices to 0
C - Called from   ECIRUN                       from ECAL lib
C - Calls         NAMIND, BKFMT                from BOS77.hlb
C -----------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "ecnamc.h"
C ------------------------------------------------------------
C
C define name-index
C
       NAETHT = NAMIND('ETHT')
       NAEWHT = NAMIND('EWHT')
       NAESHI = NAMIND('ESHI')
       NAEWHI = NAMIND('EWHI')
       NAETDI = NAMIND('ETDI')
       NAEWDI = NAMIND('EWDI')
       NAEWHE = NAMIND('EWHE')
       NAETTR = NAMIND('ETTR')
       NAEWTR = NAMIND('EWTR')
       NAENDI = NAMIND ('ENDI')
C
C define formats
C
       CALL BKFMT ('EWHT','(I)')
       CALL BKFMT ('ETHT','(I)')
       CALL BKFMT ('ESHI','(I)')
       CALL BKFMT ('EWHI','(I)')
       CALL BKFMT ('ETDI','(I)')
       CALL BKFMT ('EWDI','(I)')
       CALL BKFMT ('EWHE','(I)')
       CALL BKFMT ('ETTR','(I)')
       CALL BKFMT ('EWTR','(I)')
C
C set index of working banks to 0
C
       IDPSIG = 0
       IDEWTM = 0
       IDETTM = 0
       RETURN
       END
#endif
