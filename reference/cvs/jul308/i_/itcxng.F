      SUBROUTINE ITCXNG(VV0,CLWNUM,XTK,TTK,DFN)
C-----------------------------------------------------------------------
C! Compute track crossings of the ITC sense wire planes.
C! This routine has some similarity to IXHITS by I. Tomalin.
C!
C!    Author  :  W. B. Atwood  14-Oct-89
C!    Modified:  J.Sedgbeer    16/01/90  Rules. etc.
C!
C!   Input:
C!     VV0(5) /R : Track params 1/r,tanL,phi0,d0,z0
C!     commons:    /ITWICC/ for ITC geom.
C!
C! IMPORTANT: If you wish to use this subroutine, then at the beginning
C!            of each run, you must call the ALEPHLIB routine, IRDDAF,
C!            to fill commons with the ITC geometry etc.
C!
C!   Output:
C!     CLWNUM /I4 : Array of ITC wire numbers for each of the 8
C!                  ITC planes. A zero entry indicates no crossing
C!     XTK    /R4 : Array of space points for each crossing.
C!                  (dimension... (3,8))
C!     TTK    /R4 : Array of direction cosines.. for each XTK
C!     DFN    /R4 : Array of scaled (-1,1) drift distances expected
C!
C!   calls     : none
C!
C!   Libraries required: BOS
C!
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
#include "itwicc.h"
#include "alcons.h"
C-----------------------------------------------------------------------
      REAL  VV0(*), XTK(3,*), TTK(3,*), DFN(*)
      INTEGER CLWNUM(*)
      INTEGER LSTWR(8)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
C Compute last wire no. in each layer
      IF(FIRST) THEN
        DO 10 I=1,8
           LSTWR(I) = IWIRIT(I) + NWIRIT(I)
   10   CONTINUE
        FIRST=.FALSE.
      ENDIF
C
C Get the parameters of the track
      R0   = 1.0/VV0(1)
      TANL = VV0(2)
      PHI0 = VV0(3)
      D0   = VV0(4)
      D0Q  = D0*D0
      Z0   = VV0(5)
C
      SINTH = 1./SQRT(1.+TANL**2)
      COSTH = SINTH * TANL
C
      IF(FDEBRF.AND.JDBDRF(JULIT).GT.5) THEN
        WRITE(LDEBRL,1001) R0, TANL, PHI0, D0, Z0
 1001   FORMAT(/5X,'**** ITCXNG: R0=',F8.1,' TANL=',F7.3,
     *                     ' PHI0=',F7.3,' D0=',F8.4,' Z0=',F6.1)
      ENDIF
C
C Loop over the layers of the ITC and find their intersection points
C                          with the track.
      DO 50 LAY = 1,8
        CLWNUM(LAY)=0
        DFN(LAY)   =0.
        RAD   = RWIRIT(LAY)
C Check that there is an intersection.
        IF (ABS(D0).GT.RAD.OR.ABS(2.0*R0-D0).LT.RAD) GOTO 50
        RADQ  = RAD*RAD
        FACT1 = RADQ - D0Q
        FACT2 = 0.5*FACT1/(R0-D0)
C Get the phi coordinate of the intercept.
        SINA  = (FACT2-D0)/RAD
        IF(ABS(SINA).GT.1.0) GOTO 50
        PHIT = PHI0 + ASIN(SINA)
C Get the z coordinate of the intercept.
        SPSI2 = 0.5*FACT2/R0
        IF (SPSI2.LT.0.0.OR.SPSI2.GT.1.0) GOTO 50
        PSI = ASIN(SQRT(SPSI2))
        ZT  = Z0 + 2.0*PSI*ABS(R0)*TANL
        IF(ABS(ZT).GT.WZMXIT) GOTO 50
C
C       IF(FDEBRF.AND.JDBDRF(JULIT).GT.5) THEN
C         WRITE(LDEBRL,1003)  LAY, RAD, PHIT,PSI,ZT,TANL
C1003     FORMAT(10X,'***  LAY=',I2,' RAD=',F8.3,
C    *          ' PHIT=',F8.4,' PSI=',F8.4,' ZT=',F8.3,' TANL=',F8.3)
C       ENDIF
C
C Translate spherical co-ordinates to cartesian and direction cosine
        XTK(1,LAY) = RAD*COS(PHIT)
        XTK(2,LAY) = RAD*SIN(PHIT)
        XTK(3,LAY) = ZT
C
        PHITR = PHI0 + 2.*SIGN(PSI,R0)
        TTK(1,LAY) = COS(PHITR)*SINTH
        TTK(2,LAY) = SIN(PHITR)*SINTH
        TTK(3,LAY) =             COSTH
C
C       IF(FDEBRF.AND.JDBDRF(JULIT).GT.5) THEN
C         WRITE(LDEBRL,1004) (XTK(I,LAY),I=1,3),(TTK(I,LAY),I=1,3)
C1004     FORMAT(19X,' XTK=',3F8.4,' TTK=',3F7.4)
C       ENDIF
C
C Find wire number at the intersection.
C
C Note cell size and phi shift at the beginning of the 1st cell.
        DLN  = TWOPI/FLOAT(NWIRIT(LAY))
        SHFN = PHWRIT(LAY) - DLN*0.5
C
        DFIT = PHIT - SHFN
        DFIT = AMOD(DFIT+TWOPI,TWOPI)
        IWIRE = INT(DFIT/DLN) + IWIRIT(LAY) + 1
        IF(IWIRE.GT.LSTWR(LAY)) IWIRE = IWIRE - NWIRIT(LAY)
C
C       IF(FDEBRF.AND.JDBDRF(JULIT).GT.5) THEN
C         WRITE(LDEBRL,2021) DLN, SHFN, DFIT, IWIRE
C2021     FORMAT(20X,'DLN, SHFN = ',2F8.4,' DFIT=',F8.4,' IWIRE=',I4)
C       ENDIF
C
C  Make fine adjustments to take into account survey...
        CALL UTSWCO(LAY,IWIRE,ZT,RSW,FSW)
        CALL ITROTN(RSW,FSW,ZT)
        DFI = PHIT - FSW
C
C       IF(FDEBRF.AND.JDBDRF(JULIT).GT.5) THEN
C         WRITE(LDEBRL,1012) LAY,IWIRE,RSW,FSW,ZT,DFI
C1012     FORMAT(20X,'LAY,IWIRE=',2I4,' RSW=',F8.4,' FSW=',F8.4,
C    *             ' ZT=',F8.4,' DFI=',F8.4)
C       ENDIF
C
        IF(ABS(DFI).GT.PI) THEN
           IF(PHIT.LT.FSW) PHIT = PHIT + TWOPI
           IF(FSW.LT.PHIT) FSW  = FSW  + TWOPI
           DFI = PHIT - FSW
        ENDIF
        ICELL = SIGN((ABS(DFI/DLN) + .5),DFI)
        DFN(LAY) = (DFI - ICELL*DLN) * 2./DLN
        IWIRE = IWIRE + ICELL
        IF(IWIRE.GT.LSTWR(LAY))  IWIRE = IWIRE - NWIRIT(LAY)
        IF(IWIRE.LE.IWIRIT(LAY)) IWIRE = IWIRE + NWIRIT(LAY)
        CLWNUM(LAY) = IWIRE
C
C       IF(FDEBRF.AND.JDBDRF(JULIT).GT.5) THEN
C         WRITE(LDEBRL,1010) LAY,IWIRE, DFI, ICELL
C1010     FORMAT(10X,'*** Lay, iwire =',2I6,' DFI=',F8.4,' ICELL=',I4)
C       ENDIF
C
   50 CONTINUE
C
      END
#endif
