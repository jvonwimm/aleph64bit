         SUBROUTINE MUPRGE
C-----------------------------------------------------------------
C!  print Muon geometry and readout data
C!
C!  Weimin Wu/Giorgio Capon   9th June 1987
C!
C
C-----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "mupar1.h"
#include "mbg1da.h"
#include "mmg1da.h"
#include "mcg1da.h"
#include "msg1da.h"
#include "mrddat.h"
C
C
      LOUT=IW(6)
      WRITE(LOUT,77)
C
C     For barrel
C
      WRITE(LOUT,81)
      DO 21 NSLOT=1,NSLBAR
 21   WRITE(LOUT,71)NSLOT,WIDTHB(NSLOT),ZWIDTB(NSLOT),THICKB,
     +RINTEB(NSLOT),CENDVB(NSLOT),INPHIB(NSLOT)
C
C     For middle angle
C
      WRITE(LOUT,82)
      DO 22 NSL=1,NSLMDA
         NSLOT=NSL+NSLBAR
 22   WRITE(LOUT,72)NSLOT,WIDTHM(NSL),ZWIDTM(NSL),THICKM, RINTEM
     +(NSL),CENDVM(NSL),INPHIM(NSL),ZMODUL(NSL)
      WRITE(LOUT,83)
C
C       For endcap
C
      DO 23 NSL=1,NSLEND
         NSLOT=NSL+NSLBAR+NSLMDA
 23   WRITE(LOUT,73)NSLOT,XWIDTH(NSL),YWIDTH(NSL),THICKC, XCENTR
     +(NSL),YCENTR(NSL),ZENCAP(NSL)
      WRITE(LOUT,74)XPITCH,YPITCH
      WRITE(LOUT,76)WDEIMU,(WD16MU(K),K=1,3),OFTBMU
C
C     For MUON common values
C
      WRITE(LOUT,85)
      DO 25 ISUBC=1,NSUBCO
         DO 25 LAYER=1,2
 25   WRITE (LOUT,75)ISUBC,LAYER,XXOFFS(ISUBC,LAYER), YYOFFS(ISUBC,
     +LAYER),ZZOFFS(ISUBC,LAYER)
C
C     For special barrel
C
      WRITE(LOUT,86)
      DO 30 NSLOT=1,NSLBSM
 30   WRITE(LOUT,79)NSLOT,WTPLBN(NSLOT),XTPYBT(NSLOT)
C
      RETURN
C
 86   FORMAT(/,'  MSG1   -   NSLOT   WTPLBN   XTPYBT',/)
 79   FORMAT(10X,I8,2F8.1)
 77   FORMAT(1X,10('----------'),/,20X,'MUON  GEOMETRY  DATA',/,
     +1X,10('----------'))
 71   FORMAT(10X,I8,5F8.1,I8)
 81   FORMAT (/,'  MBG1   -   NSLOT   WIDTH  ZWIDTH   THICK    RINT',
     + '  CENDEV   INPHI',/)
 72   FORMAT(10X,I8,5F8.1,I8,F8.1)
 82   FORMAT (/,'  MMG1   -   NSLOT   WIDTH  ZWIDTH   THICK    RINT',  
     + 'CENDEV   INPHI  ZMODUL',/)
 73   FORMAT(10X,I8,5F8.1,8X,F8.1)
 83   FORMAT (/,'  MCG1   -   NSLOT  XWIDTH  YWIDTH   THICK   XCENT',
     + '   YCENT          ZENCAP',/)
 74   FORMAT(/,10X,'X STRIPS PITCH =',F6.1,5X,'Y STRIPS PITCH =',F6.1,/)
 76   FORMAT(/,10X,'8FOLD WIDTH =',F6.1,5X,'16FOLD WIDTH =',3F6.1,5X,
     + 'TUBE OFFSET IN 8FOLD =',F6.1,/)
 75   FORMAT (10X,2I8,3F8.1)
 85   FORMAT (/,10X,'   ISUBC   LAYER XOFFSET YOFFSET ZOFFSET',/)
      END
#endif
