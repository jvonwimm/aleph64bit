      SUBROUTINE FDETCO(DET,RCO,ITK,ICOOR)
C -----------------------------------------------------------------------
C!  Find DET coordinate number for coordinate at radius RCO on track ITK
C - F.Ranjard from D.Casper - 970218
C
C - Input  :  DET   / A = detector 'TP','VD', 'IT' or 'TG'
C             RCO   / R = radius
C             ITK   / I = track #
C - Output :  ICOOR / I = coordinate #
C ------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
#include "frtljj.h"
#include "tpcojj.h"
#include "ftcljj.h"
#include "vdcojj.h"
#include "fvcljj.h"
#include "iccojj.h"
#include "ficljj.h"
#include "tgcljj.h"
#include "tgtljj.h"
#include "bcs.h"
#include "bosext.h"
#include "bmacrod.h"
C
      CHARACTER*(*) DET
      REAL RCO
      INTEGER ITK, ICOOR
C
      INTEGER KTKCL,KTKPL,KCOOR,JCOORA,IND,IOTKCL,NCO,JTKPOF,N
      REAL    RCOOR
      INTEGER NFRTL,NFTCL,NFVCL,NTPCO,NVDCO,NFICL,NICCO,NTGCL,NTGTL
      DATA    NFRTL,NFTCL,NFVCL,NTPCO,NVDCO,NFICL,NICCO,NTGCL,NTGTL/9*0/

#include "bmacro.h"
C ------------------------------------------------------------------------
C
C - 1st entry
      IF(NFRTL.EQ.0)THEN
        NFRTL = NAMIND('FRTL')
        NFTCL = NAMIND('FTCL')
        NTPCO = NAMIND('TPCO')
        NFVCL = NAMIND('FVCL')
        NVDCO = NAMIND('VDCO')
        NFICL = NAMIND('FICL')
        NICCO = NAMIND('ICCO')
        NTGTL = NAMIND('TGTL')
        NTGCL = NAMIND('TGCL')
      ENDIF
C
C - select the detector
C     TKPL is the TrackPointList (FRTL or TGTL)
C     TKCL is the TrackCoordinateList (FVCL, FICL, FTCL or TGCL)
C     COOR is the coordinate bank (VDCO, ICCO, TPCO)
C     JTKPOF is the TKPL HAC parameter of the offset to TKCL
C     JCOORA is the COOR HAC parameter of the coordinate radius
C
      IF (DET(1:2).EQ.'TG') THEN
        KTKPL  = IW(NTGTL)
        KTKCL  = IW(NTGCL)
        JTKPOF = JTGTIO
        KCOOR  = IW(NTPCO)
        JCOORA = JTPCRV
      ELSEIF (DET(1:1).EQ.'T') THEN 
        KTKPL  = IW(NFRTL)
        KTKCL  = IW(NFTCL)
        JTKPOF = JFRTIT
        KCOOR  = IW(NTPCO)
        JCOORA = JTPCRV
      ELSEIF (DET(1:1).EQ.'V') THEN
        KTKPL  = IW(NFRTL)
        KTKCL  = IW(NFVCL)
        JTKPOF = JFRTIV
        KCOOR  = IW(NVDCO)
        JCOORA = JVDCR0
      ELSEIF (DET(1:1).EQ.'I') THEN
        KTKPL  = IW(NFRTL)
        KTKCL  = IW(NFICL)
        JTKPOF = JFRTII
        KCOOR  = IW(NICCO)
        JCOORA = JICCRV
      ENDIF
C
C - check banks
      IF (KTKPL.LE.0 .OR. KTKCL.LE.0 .OR. KCOOR.LE.0)THEN
        CALL RERROR('FDETCO',1,'Missing track bank(s)')
        GOTO 998
      ENDIF
C
C - loop on track coordinates
      IOTKCL = ITABL(KTKPL,ITK,JTKPOF)
      NCO = ITABL(KTKPL,ITK,JTKPOF+1)
      DO N =1,NCO
        ICOOR = ABS(ITABL(KTKCL,IOTKCL+N,1))
        RCOOR = RTABL(KCOOR,ICOOR,JCOORA)
        IF (ABS(RCO - RCOOR).LE.0.1) GOTO 999
      ENDDO

C - coordinate not found
 998  ICOOR = 0

C - end
 999  CONTINUE
      RETURN
      END
#endif

