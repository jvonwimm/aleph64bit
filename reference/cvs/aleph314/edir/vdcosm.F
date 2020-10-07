      LOGICAL FUNCTION VDCOSMJ(IDUMMY)
C-----------------------------------------------------------------------
CKEY EDIR VDET COSMIC
C! Select cosmic in vdet acceptance
C-
C     Input  : None
C     Output : VDCOSMJ - true if event passes VDET cosmic selection
C-
C     Called by  : SELEVT
C     Calls      : nothing
C     Banks used : PFRF, PFRT
C-
C!                                    Author D.Rousseau  15/03/95
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "pfrfjj.h"
#include "pfrtjj.h"
C --
      EXTERNAL NAMIND, NLINK
C --
      PARAMETER (PI=3.1415926535,TWOPI=2.*PI)
      PARAMETER(NTPCUT=10,D0MAX=10.0,Z0MAX=50,PMIN=10.0,
     & ANGCUT=1./180.*PI,
     + D0MIN=1., Z0MIN=2.,VDCOST=0.95,DD0MAX=0.5)
C --
      INTEGER ITK_(2)
      LOGICAL OK
      PARAMETER (QQIRP=.00029979)
      EXTERNAL ALFIEL
      DATA NAPFRT / 0 / 
C --
#include "bmacro.h"
C-----------------------------------------------------------------------

      VDCOSMJ=.FALSE.

C -- access PFRF 0 bank, (maybe swapped to PFRF 3 if ALPHA job
      KPFRF=NLINK('PFRF',0)
      IF (KPFRF.EQ.0) KPFRF=NLINK('PFRF',3)
      IF (KPFRF.EQ.0) KPFRF=NLINK('PFRF',2)
      IF (KPFRF.EQ.0) GOTO 999
      IF( NAPFRT .EQ. 0 ) NAPFRT = NAMIND('PFRT')
      KPFRT=IW(NAPFRT)
      IF (KPFRT.EQ.0) GOTO 999

      NPFRF=LROWS(KPFRF)
      QMFLD = ALFIEL (QMFLD)*QQIRP
C
C -- Select cosmic track candidate


C -- at least one charged track
      IF (NPFRF.LE.1) GOTO 999
      CALL VZERO(ITK_,2)
      NTK=0

      DO 20 IPFRF=1,NPFRF
        OMEGA=RTABL(KPFRF,IPFRF,JPFRIR)
        TANL= RTABL(KPFRF,IPFRF,JPFRTL)
        PHI= RTABL(KPFRF,IPFRF,JPFRP0)
        D0= RTABL(KPFRF,IPFRF,JPFRD0)
        Z0= RTABL(KPFRF,IPFRF,JPFRZ0)

        IF (ABS(D0).GT.D0MAX) GOTO 20
C
        IF (ABS(Z0).GT.Z0MAX) GOTO 20

        COST=SQRT(TANL**2/(1+TANL**2))
C in VDET acceptance
        IF (COST.GT.VDCOST) GOTO 20

        IF (OMEGA.EQ.0.) GOTO 20
        P =ABS(-QMFLD/OMEGA*SQRT(1+TANL**2))
        IF (P.LT.PMIN) GOTO 20

C some TPC hits
        IPFRT = KROW(KPFRT,IPFRF)
        NTPC  = IW(IPFRT+JPFRNT)

        IF (NTPC.LT.NTPCUT) GOTO 20


C good track found
        NTK=NTK+1
        IF (NTK.GT.2) GOTO 999
        ITK_(NTK)=IPFRF

 20   CONTINUE

C two good tracks
      IF (NTK.NE.2) GOTO 999

C acolinearity
      PHI1= RTABL(KPFRF,ITK_(1),JPFRP0)
      PHI2= RTABL(KPFRF,ITK_(2),JPFRP0)+PI
      IF (PHI2.GT.TWOPI) PHI2=PHI2-TWOPI
      DPHI=ABS(PHI1-PHI2)
      IF (DPHI.GT.PI) DPHI=TWOPI-DPHI
      IF (ABS(DPHI).GT.ANGCUT) GOTO 999

      DTHETA=ATAN(RTABL(KPFRF,ITK_(1),JPFRTL))+
     &      ATAN(RTABL(KPFRF,ITK_(2),JPFRTL))
      IF (ABS(DTHETA).GT.ANGCUT) GOTO 999

C tracks should match in d0 (z0 not usable because of bad tpc timing)
      DD0=ABS(RTABL(KPFRF,ITK_(1),JPFRD0)+
     &        RTABL(KPFRF,ITK_(2),JPFRD0))
      IF (DD0.GT.DD0MAX) GOTO 999

C at least one track inconsistent with primary
      OK=.FALSE.
      DO I=1,2
        IF (ABS(RTABL(KPFRF,ITK_(I),JPFRD0)).GT.1) OK=.TRUE.
        IF (ABS(RTABL(KPFRF,ITK_(I),JPFRZ0)).GT.2) OK=.TRUE.
      ENDDO
      IF (.NOT.OK) GOTO 999

      VDCOSMJ=.TRUE.

  999 CONTINUE
      END
#endif
