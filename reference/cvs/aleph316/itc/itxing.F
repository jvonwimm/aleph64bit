      SUBROUTINE ITXING(VV0,IZTST,NXNG,ICLW,XHT,XDC,DRF)
C-----------------------------------------------------------------------
CKEY ITC
C! Compute track crossings of the ITC sense wire planes.
C!
C!    Author  :  W. B. Atwood  14-Oct-89
C!    Modified:  J.Sedgbeer    20/04/90 To go into ALEPHLIB.
C!
C!   Input:
C!     VV0(5) /R : Track params 1/r,tanL,phi0,d0,z0
C!     IZTST  /I : Z Test flag:
C!                  IZTST = 0  Test for crossing in r-phi and Z
C!                  Otherwise only test for crossing in R-phi
C!     commons:    /ITWICC/ for ITC geom.
C!     params.:    ALCONS
C!
C!   Output:
C!     NXNG     /I  : Number of layers crossed
C!     ICLW(8)  /I  : Array of ITC wire numbers for each of the 8
C!                   ITC planes. A zero entry indicates no crossing
C!     XHT(3,8) /R  : Array of space points (x,y,z) for each crossing.
C!     XDC(3,8) /R  : Array of direction cosines.. for each XHT
C!     DRF(8)   /R  : Array of scaled (-1,1) drift distances expected
C!
C!   calls :  UTSWCO   - Alephlib
C!            ITROTN   - Alephlib
C!
C!   Libraries required: none
C!
C! ITXING: Given the 5 track parameters VV0(5) this routine calculates
C! the coordinate (x,y,z),on the track, where the track crosses each of
C! the 8 ITC layers (at the nominal radius). This coord. is stored in
C! XHT. The direction cosines of the track at the point XHT are stored
C! in XDC. The number of the sense wire closest to the crossing point
C! is stored in ICLW. Note that this wire number is adjusted for the
C! ITC alignment. The expected, scaled drift distance, in R-phi, is
C! stored in DRF. DRF is the distance, in R-phi, between the point XHT
C! and the sense wire position (corrected for alignment) as a fraction
C! of the cell half-width. Note that DRF is signed.
C! If a track does not intersect a given layer then ICLW(layer) = 0
C! and the contents of XHT, XDC and DRF, for that layer, are undefined.
C! NXNG gives the number of layers for which ICLW is non-zero.
C!
C? Loop over 8 ITC layers
C?   Check that track crosses this layer in R-phi - if not then skip
C?   Calculate Phi of track crossing
C?   Calculate Z of track crossing
C?   If (IZTST > 0) Check that Z coord is within active length
C?                              of chamber - if not then skip
C?   Fill XHT and XDC
C?   Find wire (cell) number at intersection
C?   Find Rad and Phi of the sense wire (at Z of intersection) UTSWCO
C?   Correct Rad and Phi of sense wire for ITC alignment - ITROTN
C?   Correct wire number for alignment
C?   Calculate expected scaled drift distance DRF
C? Endloop
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "itwicc.h"
#include "alcons.h"
C-----------------------------------------------------------------------
      REAL  VV0(*), XHT(3,*), XDC(3,*), DRF(*)
      INTEGER IZTST,NXNG,ICLW(*)
      INTEGER LSTWR(8)
      LOGICAL FIRST,DEB
      DATA FIRST/.TRUE./,DEB/.FALSE./
C-----------------------------------------------------------------------
C Compute last wire no. in each layer
      IF(FIRST) THEN
        LPR = 6
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
      SINTH = 1./SQRT(1.+TANL**2)
      COSTH = SINTH * TANL
C
      IF(DEB) THEN
        WRITE(LPR,1001) R0, TANL, PHI0, D0, Z0, IZTST
 1001   FORMAT('0 ITXING: R0=',F8.1,' TANL=',F7.3,
     +          ' PHI0=',F7.3,' D0=',F8.4,' Z0=',F6.1,' Ztest',I4)
      ENDIF
C-----------------------------------------------------------------------
C Loop over the layers of the ITC and find their intersection points
C                          with the track.
      NXNG = 0
      DO 50 LAY = 1,8
        ICLW(LAY) = 0
        DRF(LAY)  = 0.
        RAD   = RWIRIT(LAY)
C Check that there is an intersection in R-phi
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
C If required test for intersection in Z
        IF(IZTST.EQ.0.AND.ABS(ZT).GT.WZMXIT) GOTO 50
C
C Translate spherical co-ordinates to cartesian and direction cosine
        XHT(1,LAY) = RAD*COS(PHIT)
        XHT(2,LAY) = RAD*SIN(PHIT)
        XHT(3,LAY) = ZT
C
        PHITR = PHI0 + 2.*SIGN(PSI,R0)
        XDC(1,LAY) = COS(PHITR)*SINTH
        XDC(2,LAY) = SIN(PHITR)*SINTH
        XDC(3,LAY) =            COSTH
C
        IF(DEB) THEN
          WRITE(LPR,1002)  LAY, RAD, PHIT,PSI,ZT,TANL
 1002     FORMAT(3X,'>>> LAY=',I2,' RAD=',F8.3,
     *          ' PHIT=',F8.4,' PSI=',F8.4,' ZT=',F8.3,' TANL=',F8.3)
          WRITE(LPR,1003) (XHT(I,LAY),I=1,3),(XDC(I,LAY),I=1,3)
 1003     FORMAT(10X,' XHT=',2F8.4,F8.2,' XDC=',3F7.3)
        ENDIF
C
C Find wire number at the intersection.
C
C Note cell size and phi shift at the beginning of the 1st cell.
        DLN  = TWOPI/FLOAT(NWIRIT(LAY))
        SHFN = PHWRIT(LAY) - DLN*0.5
        DFIT = PHIT - SHFN
        DFIT = AMOD(DFIT+TWOPI,TWOPI)
        IWIRE = INT(DFIT/DLN) + IWIRIT(LAY) + 1
        IF(IWIRE.GT.LSTWR(LAY)) IWIRE = IWIRE - NWIRIT(LAY)
C
C  Make fine adjustments to take into account survey...
        CALL UTSWCO(LAY,IWIRE,ZT,RSW,FSW)
        CALL ITROTN(RSW,FSW,ZT)
        DFI = PHIT - FSW
C
        IF(DEB) THEN
          WRITE(LPR,1004) DLN, SHFN, DFIT,IWIRE
 1004     FORMAT(10X,'DLN, SHFN = ',2F8.4,' DFIT=',F8.4,' IWIRE',I4)
          WRITE(LPR,1005) RSW,FSW,ZT,DFI
 1005     FORMAT(10X,' RSW=',F8.4,' FSW=',F8.4,' ZT=',F8.2,
     +                            ' DFI=',F8.4)
        ENDIF
C
        IF(ABS(DFI).GT.PI) THEN
           IF(PHIT.LT.FSW) PHIT = PHIT + TWOPI
           IF(FSW.LT.PHIT) FSW  = FSW  + TWOPI
           DFI = PHIT - FSW
        ENDIF
        ICELL = SIGN((ABS(DFI/DLN) + .5),DFI)
        DRF(LAY) = (DFI - ICELL*DLN) * 2./DLN
        IWIRE = IWIRE + ICELL
        IF(IWIRE.GT.LSTWR(LAY))  IWIRE = IWIRE - NWIRIT(LAY)
        IF(IWIRE.LE.IWIRIT(LAY)) IWIRE = IWIRE + NWIRIT(LAY)
        NXNG = NXNG + 1
        ICLW(LAY) = IWIRE
C
        IF(DEB) THEN
          WRITE(LPR,1006) LAY,IWIRE, DFI, ICELL,DRF(LAY)
 1006     FORMAT(3X,'*** Lay, iwire =',2I6,' DFI=',F8.4,' ICELL=',I4,
     +              ' DRF',F7.2)
        ENDIF
C
   50 CONTINUE
C
      END
#endif
