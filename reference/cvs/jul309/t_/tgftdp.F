      SUBROUTINE TGFTDP(IUNIT)
C
C----------------------------------------------------------------------
C! Dump BOS bank TGFT
C!
C!    Author:    D. Schlatter 10.6.87
C!
C!    Input:
C!         - IUNIT   /I    Fortran output logical unit number
C!
C!    Called by TPCREC at TPC debug level of 2
C!
C!---------------------------------------------------------------------
#ifndef DOC
C
#include "tparac.h"
#include "tgftjj.h"
#include "bcs.h"
#include "rcurnt.h"
C
C-----------------------------------------------------------------------
C
#include "bmacro.h"
C
C-----------------------------------------------------------------------
C
      KTGFT=IW(NAMIND('TGFT'))
      IF (KTGFT.EQ.0) RETURN
      NT=IW(KTGFT+LMHROW)
      WRITE(IUNIT,103) NT,IRUNRC,IEVTRC,KTGFT
  103 FORMAT(//' Dump of ',I3,' Tpc Geometry-Fits for Tracks ',
     &         '  Run ',I5,'  Event ',I6/
     &         ' KTGFT=',I8)
      WRITE(IUNIT,102)
  102 FORMAT(/2X,'IT',5X,'RINV',7X,'TLAM',7X,'PHI0',7X,'D0  ',7X,'Z0',
     &        7X,'DIA.ERR')
      DO 21 IT=1,NT
        WRITE(IUNIT,101) IT,(RW(KROW(KTGFT,IT)+J),J=1,5),
     &         RW(KROW(KTGFT,IT)+JTGFEM),RW(KROW(KTGFT,IT)+JTGFEM+2),
     &         RW(KROW(KTGFT,IT)+JTGFEM+5),RW(KROW(KTGFT,IT)+JTGFEM+9),
     &         RW(KROW(KTGFT,IT)+JTGFEM+14)
  101   FORMAT(1X,I3,2X,10G11.3)
   21 CONTINUE
  104 FORMAT('------------- End of TGFT dump  -------------'/)
C
      RETURN
      END
#endif
