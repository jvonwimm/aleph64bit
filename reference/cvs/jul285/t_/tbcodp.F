      SUBROUTINE TBCODP(IUNIT)
C
C----------------------------------------------------------------------
C! Dump TPC bad coordinates (bank TBCO) to IUNIT
C!
C!    Author:    R. Johnson  05-08-87
C!
C!    Called by TCODRV
C!
C!    Input:
C!         - IUNIT   /I    Fortran output logical unit number
C!
C!---------------------------------------------------------------------
#ifndef DOC
C
#include "tparac.h"
#include "tbcojj.h"
#include "bcs.h"
#include "rcurnt.h"
C
C----------------------------------------------------------------------
C
#include "bmacro.h"
C
C----------------------------------------------------------------------
C
      KTBCO=IW(NAMIND('TBCO'))
      IF (KTBCO.EQ.0) RETURN
      NC=IW(KTBCO+LMHROW)
      WRITE(IUNIT,103) NC,IRUNRC,IEVTRC,KTBCO
  103 FORMAT(//' Dump of ',I3,' BAD TPC coordinates for run ',I5,
     &         ' event ',I6,/' KTBCO=',I8)
      WRITE(IUNIT,102)
  102 FORMAT(/2X,'IC',2X,'sec.',2X,' row',2X,' pad',5X,
     &       'radius',5X,' phi',6X,
     &       '   z',7X,'RPhi Wid',5X,' Z Wid',
     &       2X,'clust')
      DO 21 IC=1,NC
        IROW=IW(KROW(KTBCO,IC)+JTBCIN)/100000
        IPAD=MOD(IW(KROW(KTBCO,IC)+JTBCIN),1000)
        ISEC=MOD(IW(KROW(KTBCO,IC)+JTBCIN),100000)/1000
        WRITE(IUNIT,101) IC,ISEC,IROW,IPAD,(RW(KROW(KTBCO,IC)+J),J=2,4),
     &                   (IW(KROW(KTBCO,IC)+J),J=5,7)
  101   FORMAT(1X,I3,2X,I4,2X,I4,2X,I4,3(1X,F10.4),
     &         2(1X,I10),2X,I4)
   21 CONTINUE
      WRITE(IUNIT,104)
  104 FORMAT('-------------------------- End of TBCO dump',
     &      ' ---------------------------------------------------'/)
      RETURN
      END
#endif
