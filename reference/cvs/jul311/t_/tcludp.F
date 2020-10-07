      SUBROUTINE TCLUDP(ISECT,IUNIT)
C----------------------------------------------------------------------
C! Dump BOS bank TCLU
C!
C!    Author:    R. Johnson  23-10-86
C!    Modified:  R. Johnson   6-05-87
C!
C!    Called by TBPRNT
C!
C!    Input:
C!         - ISECT   /I    Number of the sector to dump
C!         - IUNIT   /I    Fortran output logical unit number
C!
C!---------------------------------------------------------------------
#ifndef DOC
C
#include "tparac.h"
#include "tclujj.h"
#include "bcs.h"
#include "rcurnt.h"
C
C-----------------------------------------------------------------------
C
#include "bmacro.h"
C
C-----------------------------------------------------------------------
C
      KTCLU=NLINK('TCLU',ISECT)
      IF (KTCLU.EQ.0) RETURN
      NC=IW(KTCLU+LMHROW)
      WRITE(IUNIT,103) NC,ISECT,IRUNRC,IEVTRC,KTCLU
  103 FORMAT(//' Dump of ',I3,' TPC clusters for sector ',I2,
     &         '  Run ',I5,'  Event ',I6/
     &         ' KTCLU=',I8)
      WRITE(IUNIT,102)
  102 FORMAT(/2X,'IC',3X,'TPUL',2X,'NP',2X,'BKMN',1X,'BKMX',1X,'PMIN',
     &        1X,'PMAX',3X,'row')
      DO 21 IC=1,NC
        WRITE(IUNIT,101) IC,(IW(KROW(KTCLU,IC)+J),J=1,7)
  101   FORMAT(1X,I3,2X,I4,2X,I2,2X,I3,2X,I3,2X,I3,2X,I3,2X,I5)
   21 CONTINUE
      WRITE(IUNIT,104) ISECT
  104 FORMAT('------------- End of TCLU dump for sector ',I2,
     &      ' -------------'/)
      RETURN
      END
#endif
