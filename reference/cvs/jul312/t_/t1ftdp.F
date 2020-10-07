      SUBROUTINE T1FTDP(IUNIT)
C
C----------------------------------------------------------------------
C! Dump BOS bank T1FT
C!
C!    Author:    D. Cinabro 29.5.91
C!
C!    Input:
C!         - IUNIT   /I    Fortran output logical unit number
C!
C!---------------------------------------------------------------------
#ifndef DOC
C
#include "tparac.h"
#include "t1ftjj.h"
#include "bcs.h"
#include "rcurnt.h"
C
C-----------------------------------------------------------------------
C
#include "bmacro.h"
C
C-----------------------------------------------------------------------
C
      KT1FT=IW(NAMIND('T1FT'))
      IF (KT1FT.EQ.0) RETURN
      KTGFT=IW(NAMIND('TGFT'))
      NT=IW(KTGFT+LMHROW)
      WRITE(IUNIT,103) NT,IRUNRC,IEVTRC,KT1FT
  103 FORMAT(//' Dump of ',I3,' Tpc Fit before linking for Tracks ',
     &         '  Run ',I5,'  Event ',I6/
     &         ' KT1FT=',I8)
      WRITE(IUNIT,102)
  102 FORMAT(/2X,'IT',5X,'RINV',7X,'TLAM',7X,'PHI0',7X,'D0  ',7X,'Z0',
     &        7X,'DIA.ERR')
      DO 21 IT=1,NT
        WRITE(IUNIT,101) IT,(RW(KROW(KT1FT,IT)+J),J=1,5),
     &         RW(KROW(KT1FT,IT)+JT1FEM),RW(KROW(KT1FT,IT)+JT1FEM+2),
     &         RW(KROW(KT1FT,IT)+JT1FEM+5),RW(KROW(KT1FT,IT)+JT1FEM+9),
     &         RW(KROW(KT1FT,IT)+JT1FEM+14)
  101   FORMAT(1X,I3,2X,10G11.3)
   21 CONTINUE
  104 FORMAT('------------- End of T1FT dump  -------------'/)
C
      RETURN
      END
#endif
