      SUBROUTINE HPRUNC
C
C***********************************************************************
C! GET RUN CONDITIONS FOR HDPREC                                       *
C!                                                                     *
C!  AUTHORS  :  G. BAGLIESI, A. GIASSI                                 *
C!                                                                     *
C!  MODIFIED R.TENCHINI 890126                                         *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : HDRC                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "alcons.h"
#include "rparac.h"
#include "rlunit.h"
#include "rflags.h"
#include "bcs.h"
#include "hdpcut.h"
#include "bmacro.h"
C
C First we check if there are cards that overwrite the Database
C
                                   IFLAG=1
      NHDRX=NAMIND('HDRC')
      KHDRC=IW(NHDRX)
C
C  Database reading
C
      IF(KHDRC.EQ.0)THEN
                                   IFLAG=2
         KHDRC=MDARD(IW,LRCONS,'HDRC',0)
      ENDIF
C
C  If also the Database is missing we set default values
C
      IF(KHDRC.EQ.0) THEN
                                   IFLAG=3
         DMIXHD = 10.5
         DMIYHD = 10.5
         BMIXHD = 10.5
         BMIYHD = 10.5
         CMIXHD = 16.5
         CMIYHD = 16.5
         PHI1HD = 10.*PI/180.
         YSZ1HD = 50./300.
         PHI2HD = 5.*PI/180.
         YSZ2HD = 25./300.
         DMI0HD = 80.
         KISPHD = 2
         NSACU1 = 10
         NSACU2 = 1
      ELSE
         DMIXHD = RW(KHDRC+LMHLEN+1)
         DMIYHD = RW(KHDRC+LMHLEN+2)
         BMIXHD = RW(KHDRC+LMHLEN+3)
         BMIYHD = RW(KHDRC+LMHLEN+4)
         CMIXHD = RW(KHDRC+LMHLEN+5)
         CMIYHD = RW(KHDRC+LMHLEN+6)
         PHI1HD = RW(KHDRC+LMHLEN+7)
         YSZ1HD = RW(KHDRC+LMHLEN+8)
         PHI2HD = RW(KHDRC+LMHLEN+9)
         YSZ2HD = RW(KHDRC+LMHLEN+10)
         DMI0HD = RW(KHDRC+LMHLEN+11)
         KISPHD = IW(KHDRC+LMHLEN+12)
         IF(LCOLS(KHDRC).EQ.14) THEN
            NSACU1 = IW(KHDRC+LMHLEN+13)
            NSACU2 = IW(KHDRC+LMHLEN+14)
         ELSE
            NSACU1 = 10
            NSACU2 = 1
         ENDIF
      ENDIF
      IF(JDBDRF(JULHC).GE.1) THEN
         WRITE(LDEBRL,*) ' *** HCAL DIG. PATTERNS RUN CONDITIONS ***'
         IF(IFLAG.EQ.1) WRITE(LDEBRL,*) ' HDRC READ FROM DATA CARDS '
         IF(IFLAG.EQ.2) WRITE(LDEBRL,*) ' HDRC READ FROM DATABASE   '
         IF(IFLAG.EQ.3) WRITE(LDEBRL,*) ' HDRC VALUES SET BY DEFAULT'
         WRITE(LDEBRL,*) ' DMIXHD',DMIXHD,' DMIYHD',DMIYHD
         WRITE(LDEBRL,*) ' BMIXHD',BMIXHD,' BMIYHD',BMIYHD
         WRITE(LDEBRL,*) ' CMIXHD',CMIXHD,' CMIYHD',CMIYHD
         WRITE(LDEBRL,*) ' PHI1HD',PHI1HD,' YSZ1HD',YSZ1HD
         WRITE(LDEBRL,*) ' PHI2HD',PHI2HD,' YSZ2HD',YSZ2HD
         WRITE(LDEBRL,*) ' DMI0HD',DMI0HD,' KISPHD',KISPHD
         WRITE(LDEBRL,*) ' NSACU1',NSACU1,' NSACU2',NSACU2
         WRITE(LDEBRL,*) ' *************************************** '
      ENDIF
      RETURN
      END
#endif
