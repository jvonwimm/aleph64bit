      SUBROUTINE TPULDP(ISECT,IUNIT)
C----------------------------------------------------------------------
C!    Dump BOS bank TPUL to IUNIT
C!
C!    Author:    R. Johnson  23-10-86
C!    Modified:  R. Johnson  18-12-86
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
#include "tpuljj.h"
#include "bcs.h"
#include "rcurnt.h"
C
C----------------------------------------------------------------------
C
#include "bmacro.h"
C
C----------------------------------------------------------------------
C
      KTPUL=NLINK('TPUL',ISECT)
      IF (KTPUL.EQ.0) RETURN
      NP=IW(KTPUL+LMHROW)
      WRITE(IUNIT,103) NP,ISECT,IRUNRC,IEVTRC,KTPUL,IW(KTPUL),
     &                                                 IW(KTPUL-1)
  103 FORMAT(//' Dump of ',I4,' TPC pulses for sector ',I2,
     &         '  Run ',I5,'  Event ',I6/
     &         ' KTPUL=',I8,' # data words=',I6,
     &         ' index of next bank=',I8)
      WRITE(IUNIT,102)
  102 FORMAT(/3X,'IP',4X,'TPAD off.',1X,'TPDI off.',1X,'# samples',1X,
     &       ' cluster')
      DO 21 IP=1,NP
        WRITE(IUNIT,101) IP,(IW(KROW(KTPUL,IP)+JJ),JJ=1,4)
  101   FORMAT(1X,I4,4(3X,I7))
   21 CONTINUE
      WRITE(IUNIT,104) ISECT
  104 FORMAT('------------- End of TPUL dump for sector ',I2,
     &      ' ------------------------------'/)
      RETURN
      END
#endif
