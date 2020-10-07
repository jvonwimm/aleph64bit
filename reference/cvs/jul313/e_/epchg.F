      SUBROUTINE EPCHG
C***********************************************************************
C!PROPAGATE CHARGED TRACKS                                             *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  861128                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : TPTE,ETP1                                              *
C!    OUTPUT  : ETP1                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!    INPUT  : NONE                                                    *
C!                                                                     *
C!    OUTPUT : NONE                                                    *
C!  FOR ALL TRACKS :                                                   *
C!      FIND LAST TRACK ELEMENT                                        *
C!      INITIALISE HELIX EXTRAPOLATION                                 *
C!      MAKE HELIX EXTRAPOLATION                                       *
C!      FILL  ONE LINE IN ETP1 BANK                                    *
C!                                                                     *
C***********************************************************************
#ifndef DOC
      EXTERNAL NLINK
#include "bcs.h"
#include "rlunit.h"
#include "ehlcom.h"
#include "ectcom.h"
#include "ecnamc.h"
#include "etp1jj.h"
      DOUBLE PRECISION RAD,CFILD,STPEH,PXY
      DIMENSION X(8)
#include "bmacro.h"
C
      KETP1=IW(NAETP1)
      NTRAK=LROWS(KETP1)
      KTPTE=NLINK('TPTE',0)
      IF(KTPTE.EQ.0)THEN
        CALL RERROR('EPCHG',1,' TPTE BANK DOES NOT EXIST ')
      GO TO 51
      ENDIF
      NTKE=LROWS(KTPTE)
      ITRKP=ITABL(KTPTE,1,1)
      NTKEP=NTKE+1
      DO 10 ITKE=1,NTKEP
C
C   THIS IS NEEDED TO PROCESS THE LAST TRACK ELEMENT
C
      IF(ITKE.NE.NTKEP)THEN
         ITRAK=ITABL(KTPTE,ITKE,1)
      ENDIF
      ITKEP=ITKE-1
      IF(ITRAK.NE.ITRKP.OR.ITKE.EQ.NTKEP)THEN
C
C  IF LAST TRACK ELEMENT FOR TRACK ITRKP
C
         STEP=RTABL(KTPTE,ITKEP,JETPEF)
         IADD=KROW(KETP1,ITRKP)
         IF(STEP.NE.0)THEN
C?
C?       INITIALIZE HELIX PARAMETERS
C?
          STPEH=DBLE(STEP)
          CFILD=DBLE(FILDEC)
          DO 20 I=1,7
   20     POINEH(I)=DBLE(RTABL(KTPTE,ITKEP,I+1))
          POINEH(JETPCH)=DBLE(RTABL(KTPTE,ITKEP,12))
         PXY=DSQRT(POINEH(4)**2+POINEH(5)**2)
         RAD=POINEH(7)/(DBLE(AAAKEC)*POINEH(8)*CFILD)*PXY
         SINUEH=DSIN(STPEH*PXY/RAD)
         STXYEH=SINUEH*RAD/PXY
         STZZEH=STPEH
          COSIEH=DSQRT(1.D0-SINUEH**2)
          CONSEH=(1.D0-COSIEH)/SINUEH
C
           CALL EHLIX(X)
           DO 30 I=1,6
   30      RW(IADD+I)=X(I)
           RW(IADD+7)=SNGL(POINEH(7))
C           WRITE(LDEBRL,*)' EPCHG --> TRACK ',ITRKP,' CHGED,PROPAGED'
        ELSE
           DO 40 I=1,7
   40      RW(IADD+I)=RTABL(KTPTE,ITKE,I+1)
C           WRITE(LDEBRL,*)' EPCHG --> TRACK ',ITRKP,' CHGED,PROPAGED ST
        ENDIF
C
C?   SET EXTRAP FLAG TO 2 IF IT WAS # ZERO BEFORE
C      ELSE, LEAVE IT AS IT WAS (IE 0)
C
        IF(IW(IADD+JETPEF).NE.0)IW(IADD+JETPEF)=2
        ITRKP=ITRAK
      ENDIF
   10 CONTINUE
C
C?   SUPPRESS ALL  CHARGED TRACKS WHICH HAVE NO TRACK ELEMENT IN TPC
C
   51 DO 50 I=1,NTRAK
      IF(RTABL(KETP1,I,JETPCH).NE.0.)THEN
         KLOC=KROW(KETP1,I)+JETPEF
         IF(IW(KLOC).EQ.2)THEN
            IW(KLOC)=1
         ELSE
            IW(KLOC)=0
         ENDIF
      ENDIF
   50 CONTINUE
C
C  Special treatment for the alphas :
C   Simulate a particle with charge +-1, with
C   momentums in x-y plane adjusted accordingly
C
      DO 60 I=1,NTRAK
      IF(RTABL(KETP1,I,JETPCH).NE.0.)THEN
         Q=RTABL(KETP1,I,JETPCH)
         QABS=ABS(Q)
         IF(QABS.NE.1.)THEN
            PT=RTABL(KETP1,I,JETPPT)
            PX=RTABL(KETP1,I,JETPP3)*PT
            PY=RTABL(KETP1,I,JETPP3+1)*PT
            PZ=RTABL(KETP1,I,JETPP3+2)*PT
            PX=PX/QABS
            PY=PY/QABS
            Q=Q/QABS
            PT=SQRT(PX**2+PY**2+PZ**2)
            PX=PX/PT
            PY=PY/PT
            PZ=PZ/PT
            KLOC=KROW(KETP1,I)
            RW(KLOC+JETPP3)=PX
            RW(KLOC+JETPP3+1)=PY
            RW(KLOC+JETPP3+2)=PZ
            RW(KLOC+JETPPT)=PT
            RW(KLOC+JETPCH)=Q
         ENDIF
      ENDIF
   60 CONTINUE
      RETURN
      END
#endif
