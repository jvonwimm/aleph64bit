      REAL FUNCTION IPHCOR(IC,IAMB,ENANG,DTCOR)
C-----------------------------------------------------------------------
C!  Apply drift-time correction to phi value.
C!
C!    Author      :- J. Sedgbeer 87/02/09
C!    Modified    :- I. Tomalin  89/08/17
C!    Modified    :- J. Sedgbeer 89/10/14 Use Fine-tune d-t corrs.(IEDD)
C!    Modified    :- J. Sedgbeer 89/11/02
C!    Modified    :- J. Sedgbeer 93/07/01 Use func. IDDIST for
C!                                        calc. of drift-distance.
C!
C!   Input:
C!      IC   /I  :  Coord number in ITCO bank
C!      IAMB /I  :  Ambiguity in ITCO bank:
C!                   =  0 => use parameter offset JITCP2
C!                   = -1 => use     "       "    JITCP1
C!      ENANG/R  :  entrance angle of track segment
C!      DTCOR/R  :  drift time in ITCO bank to be increased by DTCOR
C!     commons: /BCS/
C!              /ITWICC/ ITC wire geom.
C!     params : ITCOJJ
C!              IEDDJJ
C!              ALCONS
C!
C!   Output:
C!      IPHCOR /R : Corrected phi value
C!
C!   calls:   IDDIST (Alephlib)
C!   Libraries required: none
C!
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
#include "bcs.h"
#include "alcons.h"
#include "itcojj.h"
#include "ieddjj.h"
#include "itwicc.h"
      INTEGER LAY
      REAL DTIM,DDDT,IDDIST
      EXTERNAL NAMIND,NLINK,IDDIST
      LOGICAL FIRST
      INTEGER NITCO
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
      IF(FIRST) THEN
        NIEDD = NAMIND('IEDD')
        NITCO = NAMIND('ITCO')
        FIRST = .FALSE.
      ENDIF
      KITC1 = IW(NITCO)
      IF ( KITC1        .NE. 0 .AND.
     &     IW(KITC1-2)  .LT. 1 ) THEN
        KITC1 = IW(KITC1-1)
      ENDIF
      JITC1 = KROW(KITC1,IC)
      LAY   = IW(JITC1+JITCWN)/1000
      RLAY  = RW(JITC1+JITCRA)
      PHI   = RW(JITC1+JITCP2+IAMB)
      DTIM  = RW(JITC1+JITCDT)
                    SG =  1.
      IF(IAMB.EQ.0) SG = -1.
C
C Calculate original drift-distance (use drift-relation).
      DIST = IDDIST(LAY,DTIM,DDDT)
      IF(DIST.LT.0.) DIST = 0.
      FRAC = DIST/(0.5*CELWIT(LAY))
C
C Correct the distance using look-up table in IEDD bank
C
      DCOR = 0.
      KIEDD = IW(NIEDD)
      IF(KIEDD.GT.0) THEN
        KK = KROW(KIEDD,LAY)
        NB = IW(KK+JIEDNB)
        IF(NB.GT.1) THEN
          BL = RW(KK+JIEDBL)
          BH = RW(KK+JIEDBH)
          BW = RW(KK+JIEDBW)
          DISTS = SG*FRAC
          IF (DISTS.LE.BL) THEN
            DCOR = RW(KK+JIEDDD)
          ELSEIF (DISTS.GE.BH) THEN
            DCOR = RW(KK+JIEDDD-1+NB)
          ELSE
            I = (DISTS-BL)/BW + 1
            D = DISTS - BL - (I-1)*BW
            DCOR = RW(KK+JIEDDD-1+I) +
     +               (RW(KK+JIEDDD+I) - RW(KK+JIEDDD-1+I))*D/BW
          ENDIF
        ENDIF
      ENDIF
      DIST =  DIST - DCOR
      IF(DIST.LT.0.) DIST = 0.
C
C Calculate the new drift-distance.
      IF (ABS(DTCOR).LT.0.005) THEN
        DISTN = DIST
      ELSE
        DTIMN = MAX(DTIM+DTCOR,0.0)
        DISTN = IDDIST(LAY,DTIMN,DDDT)
        IF(DISTN.LT.0.) DISTN = 0.
        FRAC = DISTN/(0.5*CELWIT(LAY))
        DCOR = 0.
        KIEDD = IW(NIEDD)
        IF(KIEDD.GT.0) THEN
          IF(NB.GT.1) THEN
            DISTS = SG*FRAC
            IF (DISTS.LE.BL) THEN
              DCOR = RW(KK+JIEDDD)
            ELSEIF (DISTS.GE.BH) THEN
              DCOR = RW(KK+JIEDDD-1+NB)
            ELSE
              I = (DISTS-BL)/BW + 1
              D = DISTS - BL - (I-1)*BW
              DCOR = RW(KK+JIEDDD-1+I) +
     +                 (RW(KK+JIEDDD+I) - RW(KK+JIEDDD-1+I))*D/BW
            ENDIF
          ENDIF
        ENDIF
        DISTN =  DISTN - DCOR
        IF(DISTN.LT.0.) DISTN = 0.
      ENDIF
C
C Note the change in the effective drift-distance (include protection
C against the singularity).
C
      DELD = DISTN/MAX(ABS(COS(ENANG)),0.05) - DIST
      DELP = DELD/RLAY
      IPHCOR = PHI + SG*DELP
      IF(IPHCOR.GT.TWOPI) IPHCOR = IPHCOR-TWOPI
      IF(IPHCOR.LT.0.0)   IPHCOR = IPHCOR+TWOPI
C
      END
#endif
