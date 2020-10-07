      SUBROUTINE MBSPOL(NSLOT,ISUBC,LAYER,XX,YY,RHO,THETA,PHI)
C------------------------------------------------------------------
C! muon special barrel : convert local XX,YY to polar coordinates
C!
C!   G.Capon/R.Xu                 871029
C!
C------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "mupar1.h"
#include "mbg1da.h"
#include "msg1da.h"
#include "mrddat.h"
#include "alcons.h"
C
C     Transform X coordinate of slot # 79,80 (posision # 1,2)
C     to X of MUB1 4 local reference system.
C     Transform Y coordinate of slot # 81,82,83 (posision # 3,4,5)
C     to Y of MUB1 9 local reference system.
C     Transform Y coordinate of slot # 84,85,86 (posision # 6,7,8)
C     to Y of MUB1 11 local reference system.
C     Transform X coordinate of slot # 87,88 (posision # 9,10)
C     to X of MUB2 4 local reference system.
C     Transform Y coordinate of slot # 89,90,91 (posision # 11,12,13)
C     to Y of MUB2 9 local reference system.
C     Transform X coordinate of slot # 92,93,94 (posision # 14,15,16)
C     to Y of MUB2 11 local reference system.
C
      IF(NSLOT.LE.80) THEN
          NSLT=4
          GOTO 100
      ELSE IF(NSLOT.LE.83) THEN
          NSLT=9
          GOTO 200
      ELSE IF(NSLOT.LE.86) THEN
          NSLT=11
          GOTO 200
      ELSE IF(NSLOT.LE.88) THEN
          NSLT=16
          GOTO 100
      ELSE IF(NSLOT.LE.91) THEN
          NSLT=21
          GOTO 200
      ELSE
      NSLT=23
      ENDIF
 200  XCS=WIDTHB(NSLT)/2.
      DXCC=XX-XCS+CENDVB(NSLT)
      DHEMS=YY-WTPLBN(NSLOT-78)/2.+XTPYBT(NSLOT-78)
      GOTO 300
 100  XCS=WTPLBN(NSLOT-78)/2.
      DXCC=XX-XCS+CENDVB(NSLT)+XTPYBT(NSLOT-78)
      DHEMS=YY-ZWIDTB(NSLT)/2.
 300  RINT=RINTEB(NSLT)+0.5*THICKB+ZZOFFS(ISUBC,LAYER)
      DFI=ATAN2(DXCC,RINT)
      PHI=PI/6.*FLOAT(INPHIB(NSLT)-1)
      PHI=PHI+DFI
      RR=RINT/COS(DFI)
      RHO = SQRT(RR*RR + DHEMS*DHEMS)
      THETA=ACOS(DHEMS/RHO)
C
      END
#endif
