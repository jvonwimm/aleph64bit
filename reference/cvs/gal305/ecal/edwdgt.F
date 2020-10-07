*DK edwdgt
      SUBROUTINE EDWDGT
C-----------------------------------------------------------------------
C
C!    DIGITIZE WIRE SIGNALS
C
C     AUTHOR : B.MICHEL , D.PALLIN , 05/89
C     REVISED: 06/06/90
C
C     Banks created : EWDI,EWHE
C
C     Called by : ECDIGI
C     Calls ALBOS, BLIST from BOS lib.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "ehpash.h"
#include "ecnamc.h"
#include "bcs.h"
#include "ecsize.h"
#include "ewdijj.h"
#include "ewhejj.h"
#include "ewtmco.h"
C
C CONPLA CONSUM = CONVERSION CONSTANTS IN KEV/ADC COUNT
C SATPLA SATSUM = SATURATION LEVELS IN ADC COUNTS
C ICUTMO        = MODULE THRESHOLD IN KEV
      PARAMETER (NTMODU = LCOMP * LMODU)
      PARAMETER (CONPLA=1600.,SATPLA=3500.,CONSUM=25600.,SATSUM=3000.)
      PARAMETER ( ICUTMO=50000)
      DIMENSION SWIRES(LEWDIA,NTMODU)
      DIMENSION ISUMDG(LCOMP*LMODU),IMODH(LCOMP*LMODU)

#include "bmacro.h"
C
      DO 11 I=1,NTMODU
   11 IMODH(I) = 0
C Nber of hit modules : NMODH
      NMODH=0
      DO 1 J = 1,NTMODU
      ISUMDG(J)=0
C
      DO 2 I = 2,LWPLA+1
      IF(I.LT.35)    THEN
        GAIN=8.
      ELSE
        GAIN=16.
      ENDIF
      SWIRES(I,J)=FLOAT(IWIRES(I,J))*GAIN/CONPLA
      IF(SWIRES(I,J).GT.SATPLA)    THEN
        GAIN=GAIN/8.
        SWIRES(I,J)=SWIRES(I,J)/8.
      ENDIF
      IWIRES(I,J)=NINT(SWIRES(I,J))*IFIX(CONPLA/GAIN)
C
C  Correction for stack 3
C  ST3BA2 and ST3EC2 are the values put in the ROC.
      CONST  = 1.
      IF ( I.GT.34 ) THEN
         IF (J.GT.12. AND .J.LT.25) THEN
            CONST = ST3BA2
         ELSE
            CONST = ST3EC2
         ENDIF
      ENDIF
      IWIRES(I,J)=NINT(REAL(IWIRES(I,J))*CONST)
    2 ISUMDG(J)=ISUMDG(J)+IWIRES(I,J)
C
C If IMODH(J)=1 module J above 50  MEV.
      IF(ISUMDG(J).GT.ICUTMO)     THEN
         IMODH(J)=1
         NMODH=NMODH+1
      ENDIF
C Timing signals treatment
      ISATUR=0
      GAIN=8.
      DO 3 I = JEWDSS,LEWDIA
      SWIRES(I,J)=FLOAT(IWIRES(I,J))*GAIN/CONSUM
    3 IF(SWIRES(I,J).GT.SATSUM)ISATUR=1
C If saturation we change gain 8 to 1
      IF(ISATUR.EQ.1)THEN
         DO 4 I = JEWDSS,LEWDIA
    4    SWIRES(I,J)=SWIRES(I,J)/GAIN
      ENDIF
C
      DO 5 I = JEWDSS,LEWDIA
    5 IWIRES(I,J)=NINT(SWIRES(I,J))
    1 CONTINUE
C
C CREATE OUTPUT BANKS FOR WIRES
C
C  === Create bank EWDI,nr=0 for Wires digit output
C
      IF(NMODH.EQ.0)        GOTO 9
      CALL ALBOS('EWDI',0,LMHLEN+NMODH*LEWDIA,JEWDI,IGARB)
      CALL BLIST (IW,'E+','EWDI')
      IW(JEWDI+LMHCOL) = LEWDIA
      IW(JEWDI+LMHROW) = NMODH
      IEW=0
      DO 6 J=1,NTMODU
      IF(IMODH(J).EQ.1)THEN
          IEW=IEW+1
          KLINE=KROW(JEWDI,IEW)
          IW(KLINE+1)=IWIRES(1,J)
          DO 7 I=2,LEWDIA
 7        IW(KLINE+I)=IWIRES(I,J)
          ENDIF
 6    CONTINUE

 9    CONTINUE
C
C  === Create bank EWHE ,nr=0 :  Wires digits sum /module
C
      CALL ALBOS('EWHE',0,LMHLEN+NTMODU*LEWHEA,JEWHE,IGARB)
      CALL BLIST (IW,'E+','EWHE')
      IW(JEWHE+LMHCOL) = LEWHEA
      IW( JEWHE+LMHROW) = NTMODU
      DO 8 J=1,NTMODU
      KLINE=KROW(JEWHE,J)
      IW(KLINE+1)=ISUMDG(J)
      IW(KLINE+2)=J
 8    CONTINUE
C
      RETURN
      END
#endif
