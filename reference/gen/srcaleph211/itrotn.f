      SUBROUTINE ITROTN(R,PHI,Z)
C-----------------------------------------------------------------------
C! Rotate coord to ALEPH frame
C!
CKEY IPREDATA ITC /INTERNAL
C!    Author     :- J. Sedgbeer
C!    Modified   :- J. Sedgbeer   89/08/06
C!    Modified   :- J. Sedgbeer   91/01/07 Phi in range 0 - twopi
C!
C!    Input:
C!      R      /R  : Radius    (cm.)
C!      PHI    /R  : Phi value (radians.)
C!      Z      /R  : Z         (cm.)
C!      commons    : /IALIGC/  ITC alignment constants
C!                   /IALIGG/  Global alignment of ITC+TPC wrt ALEPH
C!      parameters : ALCONS
C!
C!    Output:
C!      R      /R  : Radius
C!      PHI    /R  : Phi
C!      Z      /R  : Z
C!
C!    called by : ICRCOO (Alephlib)
C!    calls     : none
C!
C!    Libraries required : none
C!
C! ITC Alephlib routine - part of package to make ITC coords from digis.
C! See ICRCOO.
C-----------------------------------------------------------------------
      SAVE
C I/O commons etc.
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (JXYZIA=3)
      COMMON/IALIGC/ROTNIA(JXYZIA,JXYZIA),DXYZIA(JXYZIA)
      PARAMETER (JGTRIA=3)
      COMMON/IALIGG/GROTIA(JGTRIA,JGTRIA),GTRNIA(JGTRIA)
      REAL VEC(3) ,VECG(3)
C-----------------------------------------------------------------------
      X   = R*COS(PHI)
      Y   = R*SIN(PHI)
C
C First transform from local ITC frame to ITC+TPC frame
C
      DO 10 I=1,JXYZIA
        VEC(I) = ROTNIA(1,I)*X +
     +           ROTNIA(2,I)*Y +
     +           ROTNIA(3,I)*Z + DXYZIA(I)
   10 CONTINUE
C
C Now transform from ITC+TPC frame to global ALEPH frame.
C
      DO 20 I=1,JGTRIA
        VECG(I) = GROTIA(I,1)*VEC(1) +
     +            GROTIA(I,2)*VEC(2) +
     +            GROTIA(I,3)*VEC(3) + GTRNIA(I)
   20 CONTINUE
C
      R    = SQRT(VECG(1)**2 + VECG(2)**2)
      PHI  = ATAN2(VECG(2),VECG(1))
      Z    = VECG(3)
C
      IF(PHI.LT.0.)    PHI=PHI+TWOPI
      IF(PHI.GE.TWOPI) PHI=PHI-TWOPI
C
      END
