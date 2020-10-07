      SUBROUTINE ECOIDP(ICLUS)
C-----------------------------------------------------
C! INITIALIZE IDENTIFICATION PARAMETER AND
C! FILL COMMON EANCLU  FOR SUBSEQUENT CLUSTER ANALYSIS
C!
C!  AUTHOR   : J.P. ALBANESE
C!  MODIFIED : F. ETIENNE      87/08/27
C!
C!  BANKS :
C!    INPUT   : ESDA,ECLU,ECT1
C!    OUTPUT  : NONE
C!    CREATED : NONE
C!
C!
C-----------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "eanclu.h"
#include "ecnamc.h"
#include "esdajj.h"
#include "eclujj.h"
#include "ect1jj.h"
#include "erl3jj.h"
#include "etp1jj.h"
      EXTERNAL VSUM
      EXTERNAL IUCOMP
#include "bmacro.h"
C
C? Fill cluster number
C
      ICLNEA=ICLUS
C
C? A CHARGED TRACK IS ASSOCIATED
C
      ICHREA = 1
C
C?   FIND NUMBER OF MODULES DETECTORS AND REGIONS
      KESDA = IW(NAESDA)
      KECLU = IW(NAECLU)
C
C    RESET MIP PARTICLE FLAG
      IFLGMP = 0
C
      NBMOEA = 0
      NBDEEA = 0
      NBRGEA = 0
      CALL VZERO (LMOLEA,3)
      CALL VZERO (ECLSEA,9)
      CALL VZERO (LDETEA,2)
      CALL VZERO (LREGEA,5)
      CALL VZERO (ENRGEA,5)
C
      ISTO = ITABL(KECLU,ICLNEA,JECLES)
   20 IF (ISTO.EQ.0)            GO TO 160
         ENSTO = RTABL(KESDA,ISTO,JESDME)
         KNSTO = ITABL(KESDA,ISTO,JESDDK)
C
C?   COUNT NUMBER OF MODULES
         NMOD = ITABL(KESDA,ISTO,JESDER)
         IF (NBMOEA.EQ.0)       GO TO 65
         JMOD = IUCOMP(NMOD,LMOLEA,NBMOEA)
         IF (JMOD.NE.0) GO TO 70
   65    IF (NBMOEA.GT.3) THEN
            CALL RERROR(' ECOIDP',1,' MORE THAN 3 MODULES FOUND')
         GO TO 999
         ENDIF
         NBMOEA = NBMOEA + 1
         LMOLEA(NBMOEA) = NMOD
         JMOD = NBMOEA
   70    ECLSEA(KNSTO,JMOD) = ECLSEA(KNSTO,JMOD) + ENSTO
C
C?   COUNT NUMBER OF DETECTORS
         NDET = 1 +(NMOD-1)/12
         IF (NBDEEA.EQ.0)       GO TO 75
         IF (IUCOMP(NDET,LDETEA,NBDEEA).NE.0) GO TO 81
   75    IF (NBDEEA.GT.2) THEN
            CALL RERROR(' ECOIDP',2,' MORE THAN 2 DETECTORS FOUND')
         GO TO 999
         ENDIF
         NBDEEA = NBDEEA + 1
         LDETEA(NBDEEA) = NDET
C
C?   COUNT NUMBER OF REGIONS
   81    NREG = ITABL(KESDA,ISTO,JESDSC)
         IF (NBRGEA.EQ.0)       GO TO 85
         JREG = IUCOMP(NREG,LREGEA,NBRGEA)
         IF (JREG.NE.0) GO TO 90
   85    IF (NBRGEA.GT.5) THEN
            CALL RERROR(' ECOIDP',3,' MORE THAN 5 REGIONS FOUND')
         GO TO 999
         ENDIF
         NBRGEA = NBRGEA + 1
         JREG = NBRGEA
         LREGEA(NBRGEA) = NREG
   90    ENRGEA(JREG) = ENRGEA(JREG) + ENSTO
C
      ISTO = ITABL(KESDA,ISTO,JESDES)
      GO TO 20
C
  160  CONTINUE
C
C?   REORDER MODULES ACCORDING TO DECREASING ENERGY
      IF (NBMOEA.GT.1) THEN
         I1 = 1
  210    EMMAX = 0.
         IMMAX = 0
         DO 280 IM = I1, NBMOEA
         EMMOD = VSUM(ECLSEA(1,IM),3)
         IF (EMMOD.GT.EMMAX) THEN
            IMMAX = IM
            EMMAX = EMMOD
         ENDIF
  280    CONTINUE
         IF (IMMAX.NE.I1) THEN
            ISAVE         = LMOLEA(I1)
            LMOLEA(I1)    = LMOLEA(IMMAX)
            LMOLEA(IMMAX) = ISAVE
            XSAVE           = ECLSEA(1,I1)
            ECLSEA(1,I1)    = ECLSEA(1,IMMAX)
            ECLSEA(1,IMMAX) = XSAVE
            XSAVE           = ECLSEA(2,I1)
            ECLSEA(2,I1)    = ECLSEA(2,IMMAX)
            ECLSEA(2,IMMAX) = XSAVE
            XSAVE           = ECLSEA(3,I1)
            ECLSEA(3,I1)    = ECLSEA(3,IMMAX)
            ECLSEA(3,IMMAX) = XSAVE
         ENDIF
         IF (I1.LT.NBMOEA-1) THEN
            I1 = I1 + 1
            GO TO 210
         ENDIF
      ENDIF
C
C?   REORDER REGIONS ACCORDING TO DECREASING ENERGY
      IF (NBRGEA.GT.1) THEN
         I1 = 1
  310    ERMAX = 0.
         IDMAX = 0
         DO 380 IDET = I1, NBRGEA
         ENREG = ENRGEA(IDET)
         IF (ENREG.GT.ERMAX) THEN
            IDMAX = IDET
            ERMAX = ENREG
         ENDIF
  380    CONTINUE
         IF (IDMAX.NE.I1) THEN
            ISAVE         = LREGEA(I1)
            LREGEA(I1)    = LREGEA(IDMAX)
            LREGEA(IDMAX) = ISAVE
            XSAVE         = ENRGEA(I1)
            ENRGEA(I1)    = ENRGEA(IDMAX)
            ENRGEA(IDMAX) = XSAVE
         ENDIF
         IF (I1.LT.NBRGEA-1) THEN
            I1 = I1 + 1
            GO TO 310
         ENDIF
      ENDIF
C
C?   GET ENERGY OF THE CLUSTER
      ECLTEA = RTABL(KECLU,ICLNEA,JECLE4)
C
C?   COMPUTE THE BARYCENTER OF THE CLUSTER
      RO = RTABL(KECLU,ICLNEA,JECLR4)
      TH = RTABL(KECLU,ICLNEA,JECLT4)
      PH = RTABL(KECLU,ICLNEA,JECLF4)
      ROSIN     = RO*SIN(TH)
      XYZBEA(1) = ROSIN*COS(PH)
      XYZBEA(2) = ROSIN*SIN(PH)
      XYZBEA(3) = RO*COS(TH)
C
C?   DEFINE REGION CODE
C  Region code.
        KSEX = 0
C   Cracks and overlap.
      IF( NBMOEA .EQ. 1 ) THEN
        NVEX = 0
        KREX = 0
      ELSE
        IF( NBDEEA .EQ. 1 ) THEN
C   It is a crack.
          NVEX = 0
                IF( LMOLEA(1) .GT. LMOLEA(2) .OR.
     +          LMOLEA(2) - LMOLEA(1) .EQ. 11 ) THEN
            KREX = 1
          ELSE
            KREX = 2
          ENDIF
        ELSE
C   Overlap
          IF( LDETEA(1) .EQ. 2 ) THEN
C   The main detector is the barrel.
            NVEX = 1
          ELSE
C   The main detector is the endcap.
            NVEX = 2
          ENDIF
          IF( NBMOEA .EQ. 2 ) THEN
C   It is not a crack inside the overlap.
            KREX = 0
          ELSE
C   Overlap and crack.
            LDET2 = ( LMOLEA(2) - 1 ) / 12
            LDET3 = ( LMOLEA(3) - 1 ) / 12
            IF( LDET2 .EQ. LDET3 ) THEN
C               NVEX   KREX              KSEX
C   Ba Ec Ec     1      1 : #Ec2>#Ec3     1 : Ba>Ec2+Ec3
C                       2 : #Ec2<#Ec3     0 : Ba<Ec2+Ec3
C   Ec Ba Ba     2      1 : #Ba2>#Ba3     1 : Ec>Ba2+Ba3
C                       2 : #Ba2<#Ba3     0 : Ec<Ba2+Ba3
              SOM1 = ECLSEA(1,1) + ECLSEA(2,1) + ECLSEA(3,1)
              SOM2 = ECLSEA(1,2) + ECLSEA(2,2) + ECLSEA(3,2)
              SOM3 = ECLSEA(1,3) + ECLSEA(2,3) + ECLSEA(3,3)
              IF( SOM1 .GT. SOM2 + SOM3 ) KSEX = 1
              IF( LMOLEA(2) .GT. LMOLEA(3) .OR.
     +        LMOLEA(3) - LMOLEA(2) .EQ. 11 ) THEN
               KREX = 1
              ELSE
               KREX = 2
              ENDIF
            ELSE
            LDET1 = ( LMOLEA(1) - 1 ) / 12
              IF( LDET1 .EQ. LDET2 ) THEN
C   Ba Ba Ec     if      #Ba1>#Ba2       KREX = 1
C   Ec Ec Ba     if      #Ec1>#Ec2       KREX = 1
                IF( LMOLEA(1) .GT. LMOLEA(2) .OR.
     +          LMOLEA(2) - LMOLEA(1) .EQ. 11 ) THEN
                  KREX = 1
                ELSE
                  KREX = 2
                ENDIF
              ELSE
C   Ba Ec Ba     if      #Ba1>#Ba3       KREX = 1
C   Ec Ba Ec     if      #Ec1>#Ec3       KREX = 1
                IF( LMOLEA(1) .GT. LMOLEA(3) .OR.
     +          LMOLEA(3) - LMOLEA(1) .EQ. 11 ) THEN
                  KREX = 1
                ELSE
                  KREX = 2
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C   Limits of regions.
      IF(NBRGEA .LE. 1) THEN
        LREX = 0
      ELSE
        IF(LREGEA(1) .GT. LREGEA(2)) THEN
          LREX = 1
        ELSE
          LREX = 2
        ENDIF
      ENDIF
C   Region code.
      ICREEA = LREX + 4 * KSEX + 16 * KREX + 64 * NVEX
C
C?   KLASEA NOT YET DEFINED
C
      RETURN
C
  999 CALL RERROR(' ECOIDP',4,' exit')
      RETURN
      END
#endif
