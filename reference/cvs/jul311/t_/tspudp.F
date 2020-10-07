      SUBROUTINE TSPUDP(ISECT,IUNIT)
C
C-----------------------------------------------------------------------
C! Dump TPC subpulses
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
      KTSPU=NLINK('TSPU',ISECT)
      IF (KTSPU.LE.0) RETURN
      NSPUL=IW(KTSPU+LMHROW)
      WRITE(IUNIT,101) NSPUL,ISECT,IEVTRC,IRUNRC,
     &                  IW(KTSPU),KTSPU,IW(KTSPU-1)
      WRITE(IUNIT,103)
      DO 80 I=1,NSPUL
        WRITE(IUNIT,102) I,IW(KROW(KTSPU,I)+1),
     &         (RW(KROW(KTSPU,I)+J),J=2,6),(IW(KROW(KTSPU,I)+J),J=7,9)
   80 CONTINUE
      WRITE(IUNIT,104)
      RETURN
  101 FORMAT(//' Dump of ',I5,' TPC subpulses from BOS bank TSPU',
     &       ' for sector',I2,' of event ',I6,' of run ',I5/
     &       ' Number of data words=',I5,'; index=',I6,
     &       '; index of next bank=',I6/)
  103 FORMAT(/'    I',5X,'TC ',7X,'PC',10X,'PT',10X,'IT',10X,'T1',10X,
     &               'T2',7X,'QF',5X,'PN',5X,'SC')
  102 FORMAT(1X,I4,3X,I5,5(1X,E11.4),3(1X,I6))
  104 FORMAT('--------- End of TSPU dump ----------------------',
     &       '-------------------------------------------------'/)
      END
#endif
