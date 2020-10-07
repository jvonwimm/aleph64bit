      SUBROUTINE QFMCMD
CKEY FILL MC /INTERNAL
C-----------------------------------------------------------------------
C! Fill mother - daughter relation for MC particles
C!                                                  H.Albrecht 18.10.88
C called from QFILL
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE INIT
#include "qcdesh.h"
      DATA INIT /0/
C
      JQVRT(I,N) = KOQVRT + I * KCQVRT + N
      JQVEC(I,N) = KOQVEC + I * KCQVEC + N
      IQVEC(I,N) = IW (KOQVEC + I * KCQVEC + N)
      RQVEC(I,N) = RW (KOQVEC + I * KCQVEC + N)
      IMOTH(I)   = IW (KOQLIN + IQVEC(I,JQVEOL) + 1)
C-----------------------------------------------------------------------
C
C       Set number of daughters for particles and vertices
C
      DO 10 ITK=KFMCT,KLMCT
        IF (IQVEC(ITK,JQVENO) .NE. 0)  IW(JQVEC(IMOTH(ITK),JQVEND)) =
     &    IW(JQVEC(IMOTH(ITK),JQVEND)) + 1
   10 IF (IQVEC(ITK,JQVEOV) .NE. 0)
     &    IW(JQVRT(IQVEC(ITK,JQVEOV),JQVRND)) =
     &    IW(JQVRT(IQVEC(ITK,JQVEOV),JQVRND)) + 1
C
C       Number of pointers in QLIN
C
      DO 20 ITK=KFMCT,KLMCT
        NDAU = IQVEC(ITK,JQVEND)
        IF (NDAU .EQ. 0)  GO TO 20
        IW(JQVEC(ITK,JQVEDL)) = KNQLIN - NDAU
C         reason for the next statement : see routine QADAU
        IF (NDAU .GT. 8)  NDAU = ((NDAU + 7) / 8) * 8
        KNQLIN = KNQLIN + NDAU
   20 CONTINUE
C
      DO 30 IVR=KFMCV,KLMCV
        IW(JQVRT(IVR,JQVRDL)) = KNQLIN - IW(JQVRT(IVR,JQVRND))
   30 KNQLIN = KNQLIN + IW(JQVRT(IVR,JQVRND))
C
      IF (KNQLIN .GE. IW(KOQLIN))  CALL QSBANK ('QLIN', KNQLIN + 500)
C
C       fill QLIN with mother -> daughter relation
C
      DO 40 ITK=KFMCT,KLMCT
        IF (IQVEC(ITK,JQVENO) .NE. 0)  THEN
          IW(JQVEC(IMOTH(ITK),JQVEDL)) =
     &    IW(JQVEC(IMOTH(ITK),JQVEDL)) + 1
          IW(KOQLIN+IW(JQVEC(IMOTH(ITK),JQVEDL))+
     &             IW(JQVEC(IMOTH(ITK),JQVEND))) = ITK
        ENDIF
        IVR = IQVEC(ITK,JQVEOV)
        IF (IVR .NE. 0)  THEN
          IM = KOQVRT + IVR * KCQVRT
          IW(JQVRT(IVR,JQVRDL)) = IW(JQVRT(IVR,JQVRDL)) + 1
          IW(KOQLIN+IW(JQVRT(IVR,JQVRDL))+IW(JQVRT(IVR,JQVRND))) = ITK
        ENDIF
   40 CONTINUE
C
   90 END
#endif
