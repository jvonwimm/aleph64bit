*DK sishow
      SUBROUTINE SISHOW
C.----------------------------------------------------------------
C  J.Rander & B.Bloch-Devaux  January 92
C! SCAL : Shower parametrisation
C     - Called by SIHIT
C     - Calls     SIDFPA,SILONG,SITRAN,SITRA3,CAHIST from this lib
C                 SIZFRI,SIXTOA                      from Alephlib
C                 RANNOR,RNDM                        fron Cern libs
C.----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "alcons.h"
#include "jobcom.h"
#include "iocom.h"
#include "trkcom.h"
#include "sistat.h"
#include "siparm.h"
#include "sigeco.h"
#include "sinamc.h"
      DATA EMNCT / 0./
C
C--- calculation of parameters which depends upon energy
C
      CALL SIDFPA (TRKELE(8))
C
C--- track enters active area on front face or from inner side
      Z = ABS(TRKELE(3))
      COSTH = ABS(TRKELE(6))
      NUMOD =1
      IF ( TRKELE(3).LT.0.) NUMOD = 2
C
C--- compute which active plane will be first hit
C    ZWIDSI   width between two successive z planes (CM)
C    ZWFLSI   width in front of first Si layer      (CM)
C    ZWRFSI   Number of radiation length before first Si layer
C    ZWRLSI   Number of radiation length per layer
      IF ( Z .LE.Z0SNSI(NUMOD) ) THEN
         IFIR = 1
         S0 =(Z0SNSI(NUMOD)-Z)*ZWRFSI/(COSTH*ZWFLSI)
         ZL0=(Z0SNSI(NUMOD)-Z)/COSTH
      ELSE
         IFIR =(Z - Z0SNSI(NUMOD))/ZWIDSI +2
         S0 =(Z0SNSI(NUMOD)+(IFIR-1)*ZWIDSI-Z)*ZWRLSI/(COSTH*ZWIDSI)
         ZL0=(Z0SNSI(NUMOD)+(IFIR-1)*ZWIDSI-Z)/COSTH
      ENDIF
C
C--- Loop on layers
      DO 20 ISTK = IFIR,NZBNSI
C
C--- Track length in radiation length(S),  CM(ZLON)
        S = S0+ (ISTK-1)*ZWRLSI/COSTH
        ZLON = ZL0 +(ISTK-1)* ZWIDSI/COSTH
        SIGNL = SILONG(S) * SINORM
        CALL RANNOR(ALEA,ALEB)
        SIGNL = SIGNL + SIFLUC*ALEA*SQRT(SIGNL)
           SIGNL = SIGNL * ZWRLSI
        IF(SIGNL.LT.EMNCT) SIGNL=0.0
C
C==> SIMULATE RADIAL DEPOSITION AND FILL HITS INTO SICAL PADS
        NHITS=IFIX(SIGNL*SIPERG)
        IF (NHITS.GT.0) THEN
           CALL SIZFRI(ZHIT,ISTK,NUMOD)
           XHIT0=TRKELE(1)+(ZHIT-TRKELE(3))*TRKELE(4)/TRKELE(6)
           YHIT0=TRKELE(2)+(ZHIT-TRKELE(3))*TRKELE(5)/TRKELE(6)
C
           DO 11 NH =  1,NHITS
              IF ( ICSIJO(4).EQ.0) CALL SITRA3(ISTK,RADIS)
              IF ( ICSIJO(4).EQ.1) CALL SITRAN(ISTK,RADIS)
C------generate shower hit in stack layer
              APHI=TWOPI*RNDM(DUMMY)
              XHIT=XHIT0+RADIS*COS(APHI)
              YHIT=YHIT0+RADIS*SIN(APHI)
C------FIND OUT WHICH PAD HIT
              ZPOS = ZHIT +SIGN ( 0.1,TRKELE(3))
              CALL SIXTOA(IAD,IOR,XHIT,YHIT,ZPOS   ,IOK)
              IF (IOK.NE.0 ) THEN
C-    Count energy elements Lost outside SICAL
                 ESICOU(7) = ESICOU(7) +1000./SIPERG
                 ESICOU(7+NUMOD) = ESICOU(7+NUMOD) +1000./SIPERG
                 IF (NSIPRT.GT.1) WRITE(LOUTIO,1000) XHIT,YHIT,ZHIT,
     $                            SQRT(XHIT*XHIT+YHIT*YHIT),IOK
              ELSE
C-    Deposit energy on the relevant address (MEV)
                 IDE = IFIX(1000./SIPERG)
                 CALL CAHIST(NASIHI,IAD,IOR+1,IDE)
C-    Count energy elements entering SICAL
                 ESICOU(4) = ESICOU(4) +1000./SIPERG
                 ESICOU(4+NUMOD) = ESICOU(4+NUMOD) +1000./SIPERG
                 IF (NSIPRT.GT.2) WRITE(LOUTIO,1100) IAD,IOR+1,IDE
              ENDIF
   11      CONTINUE
C
        ENDIF
 20   CONTINUE
      RETURN
 1000 FORMAT (' ++++ SISHOW : the following space point is outside the'
     &      ,' sensitive area of crystals:X,Y,Z,R', 4F9.4,I4)
 1100 FORMAT (' ++++ SISHOW : Triplet address ',I8,' position ',I5,
     &        ' Energy deposit ',I10,' Mev ')
      END
#endif
