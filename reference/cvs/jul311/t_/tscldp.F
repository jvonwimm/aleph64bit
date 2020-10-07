      SUBROUTINE TSCLDP(ISECT,IUNIT)
C
C-----------------------------------------------------------------------
C! Dump TPC subclusters
C!
C!    Author:    R. Johnson    9-1-87
C!
C!    Input:
C!       - ISECT     /I     Sector number to be dumped
C!       - IUNIT     /I     Fortran logical unit number for output
C!
C!    Called by TBPRNT
C!
C!----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "rcurnt.h"
C
C-------------------------------------------------------------
C
#include "bmacro.h"
C
C-------------------------------------------------------------
C
      KTSCL=NLINK('TSCL',ISECT)
      IF (KTSCL.LE.0) RETURN
      NSCLU=IW(KTSCL+LMHROW)
      WRITE(IUNIT,101) NSCLU,ISECT,IEVTRC,IRUNRC,
     &                  IW(KTSCL),KTSCL,IW(KTSCL-1)
      WRITE(IUNIT,121)
      DO 80 I=1,NSCLU
        WRITE(IUNIT,122) I,(IW(KROW(KTSCL,I)+J),J=1,8),
     &      (RW(KROW(KTSCL,I)+J),J=9,11),IW(KROW(KTSCL,I)+12)
   80 CONTINUE
      WRITE(IUNIT,104)
      RETURN
  122 FORMAT(1X,I4,7(1X,I4),1X,I6,3(1X,E11.4),1X,I4)
  121 FORMAT(/'    I','  NS   PO   FP   NC   CO   TC   AW    QU  ',
     &       6X,'SP',10X,'SB',10X,'CR',8X,'CL')
  101 FORMAT(//' Dump of ',I5,' TPC subclusters from BOS bank TSCL',
     &       ' for sector',I2,' of event ',I6,' of run ',I5/
     &       ' Number of data words=',I5,'; index=',I6,
     &       '; index of next bank=',I6/)
  104 FORMAT('--------- End of TSCL dump ----------------------',
     &       '-------------------------------------------------'/)
      END
#endif
