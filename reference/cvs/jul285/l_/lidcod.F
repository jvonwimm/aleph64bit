      SUBROUTINE LIDCOD
C-----------------------------------------------------------------------
C! Set acceptance code and quality flag
C!
C!   Author   : P. H. Hansen  890101
C!   Modified : H. Meinhard   05-Mar-1990
C!   Modified : P. H. Hansen  01-May-1996 Fix reference MC energy
C!
C!     Input  : 'LIDT'           Lumi electron candidate bank
C!              'SFTR'           Lumi track bank
C!              'BHAB'           Cuts from data cards
C!     Output :
C!              'LIDT'  IFlag    Track Quality flag
C!                               bit0:    track or patch
C!                               bit1:    track
C!                               bit2:    SATR region
C!              'LIFL'  K1rg     Flag for small fiducial area
C!                               bit0-32: cuts 1-32
C!              'LIFL'  K2rg     Flag for large fiducial area
C!                               bit0-32: cuts 1-32
C!              'LIFL'  KHrg     Flag for high energy threshold
C!                               bit0-32: cuts 1-32
C!              'LIFL'  KLrg     Flag for low energy threshold
C!                               bit0-32: cuts 1-32
C!
C!   Description :
C!   ===========
C?   Encode quality flag in LIDT
C?   Encode Bhabha acceptance code LIFL
C?   Six different acceptance definitions are considered:
C?      1) Tracks used to determine accepted fraction
C?         in SATR region. Cluster centroid used outside.
C?      2) Track required on small side
C?         Either track or cluster is used on the other side.
C?      3) Track required on both sides.
C?      4) Cluster centroid used on both sides.
C?      5) Pad boundaries used on small side.
C?      6) Restricted version of 5)
C?   The definitions are chosen according to the
C?   Method number and the track quality flag in bank BHAB.
C-----------------------------------------------------------------------
#ifndef DOC
#include "rconds.h"
#include "bcs.h"
#include "rflags.h"
#include "rcurnt.h"
#include "lcnamc.h"
#include "lsdajj.h"
#include "lidtjj.h"
#include "lifljj.h"
#include "bhabjj.h"
#include "lalijj.h"
#include "sanamc.h"
#include "sftrjj.h"
#include "srecpc.h"
#include "lcrejj.h"
#include "alcons.h"
      DIMENSION MASK(32)
      LOGICAL BTEST,REJFL
      DATA MASK/1,2,4,8,16,32,64,128,256,512,1024,2048,
     &          4096,8192,16384,32768,16*0/
      DATA MAXM/16/
#include "bmacro.h"
#include "lcmacr.h"
C---------------------------------------------------------------------
C
C Check existence of Lcal electron candidates
      KLIDT = IW(NALIDT)
      IF(KLIDT.LE.0)                                GOTO 999
      NIDT = LROWS(KLIDT)
      IF(NIDT.LE.0)                                 GOTO 999
      KLALI = IW(NALALI)
C
C Book bank for acceptance flags
      LEN = LMHLEN + LLIFLA*NIDT
      CALL AUBOS('LIFL',0,LEN,KLIFL,IGARB)
      IF(IGARB.EQ.2)                                GOTO 997
      CALL BLIST(IW,'T+','LIFL')
      IW(KLIFL+1) = LLIFLA
      IW(KLIFL+2) = NIDT
      KLIDT = IW(NALIDT)
      KLCRE = IW(NALCRE)
      KSFTR = IW(NAMIND('SFTR'))
      KLSDA = IW(NALSDA)
C
C
C Bhabha selection criteria
      KBHAB = IW(NABHAB)
      IF(KBHAB.LE.0)                                GOTO 999
      NMETH = MIN0(MAXM,LROWS(KBHAB))
      IF(NMETH.LE.0)                                GOTO 999
C
C Take beam energy
       EBEAM = ENERRC/2.
C
C For all lumi electron candidates
      DO 21 I=1,NIDT
        JTRK = ITABL(KLIDT,I,JLIDST)
        EC = RTABL(KLIDT,I,JLIDEC)
        ILSDA = ITABL(KLIDT,I,JLIDLS)
        IADDR = ITABL(KLSDA,ILSDA,JLSDAD)
C
C Transform cluster theta to subdetector system
        PHCL = RTABL(KLIDT,I,JLIDPC)
        THCL = RTABL(KLIDT,I,JLIDTC)
        IF (THCL .LT. PIBY2) THEN
          X0 = (RTABL(KLALI,3,JLALDX)+RTABL(KLALI,4,JLALDX))/2.
          Y0 = (RTABL(KLALI,3,JLALDX+1)+RTABL(KLALI,4,JLALDX+1))/2.
          XL = RTABL(KLCRE,1,JLCRZC)*TAN(THCL)*COS(PHCL) - X0
          YL = RTABL(KLCRE,1,JLCRZC)*TAN(THCL)*SIN(PHCL) - Y0
          ZL = RTABL(KLCRE,1,JLCRZC)
        ELSE
            X0 = (RTABL(KLALI,1,JLALDX)+RTABL(KLALI,2,JLALDX))/2.
            Y0 = (RTABL(KLALI,1,JLALDX+1)+RTABL(KLALI,2,JLALDX+1))/2.
            XL = -RTABL(KLCRE,1,JLCRZC)*TAN(THCL)*COS(PHCL) - X0
            YL = -RTABL(KLCRE,1,JLCRZC)*TAN(THCL)*SIN(PHCL) - Y0
            ZL = -RTABL(KLCRE,1,JLCRZC)
        ENDIF
        THCL = ATAN(SQRT(XL**2+YL**2)/ABS(ZL))
C
C Fill 3 quality bits
        IFLAG = 0
        IF(JTRK.NE.0) THEN
          IFLAG = IFLAG+MASK(1)
          IF(ITABL(KSFTR,JTRK,JSFTQF).NE.0)
     &      IFLAG=IFLAG+MASK(2)
        ENDIF
        TSA = RTABL(KLCRE,1,JLCRSA)
        IF(RTABL(KLIDT,I,JLIDTC).LT.TSA.OR.
     &      PI-RTABL(KLIDT,I,JLIDTC).LT.TSA) THEN
          IFLAG=IFLAG+MASK(3)
        ENDIF
C
C Fill Bhabha acceptance code
        ICODH = 0
        ICODL = 0
        ICOD1 = 0
        ICOD2 = 0
C
C Loop over all methods
        DO 15 IM=1,NMETH
          MTYP = ITABL(KBHAB,IM,JBHAME)
          IQUA = ITABL(KBHAB,IM,JBHAQF)
C
C Use track if it exists, otherwise cluster (Method 1 or 2)
          IF(MTYP.EQ.1.OR.MTYP.EQ.2) THEN
            IF(BTEST(IFLAG,IQUA)) THEN
              THETA = RTABL(KSFTR,JTRK,JSFTTH)
              PHI = RTABL(KSFTR,JTRK,JSFTPH)
              X0 = RTABL(KSFTR,JTRK,JSFTX0)
              Y0 = RTABL(KSFTR,JTRK,JSFTY0)
              IF (THETA .LT. PIBY2) THEN
                PHI = PHI + RTABL(KLCRE,1,JLCRZC) * DPHBSR
                XT = X0 + RTABL(KLCRE,1,JLCRZC) * TAN(THETA) * COS(PHI)
                YT = Y0 + RTABL(KLCRE,1,JLCRZC) * TAN(THETA) * SIN(PHI)
                ZT = RTABL(KLCRE,1,JLCRZC)
              ELSE
                PHI = PHI - RTABL(KLCRE,1,JLCRZC) * DPHBSR
                XT = X0 - RTABL(KLCRE,1,JLCRZC) * TAN(THETA) * COS(PHI)
                YT = Y0 - RTABL(KLCRE,1,JLCRZC) * TAN(THETA) * SIN(PHI)
                ZT = -RTABL(KLCRE,1,JLCRZC)
              ENDIF
              PH = ATAN2(YT,XT)
              TH = ATAN(SQRT(XT**2+YT**2)/ZT)
              DX = RTABL(KLIDT,I,JLIDDA+2)
              DY = RTABL(KLIDT,I,JLIDDA+3)
            ELSE
              PH = RTABL(KLIDT,I,JLIDPC)
              TH = RTABL(KLIDT,I,JLIDTC)
              DX = RTABL(KLIDT,I,JLIDDA)
              DY = RTABL(KLIDT,I,JLIDDA+1)
            ENDIF
C
C Use track (Method 3)
          ELSEIF(MTYP.EQ.3) THEN
            IF(BTEST(IFLAG,IQUA)) THEN
              THETA = RTABL(KSFTR,JTRK,JSFTTH)
              PHI = RTABL(KSFTR,JTRK,JSFTPH)
              X0 = RTABL(KSFTR,JTRK,JSFTX0)
              Y0 = RTABL(KSFTR,JTRK,JSFTY0)
              IF (THETA .LT. PIBY2) THEN
                PHI = PHI + RTABL(KLCRE,1,JLCRZC) * DPHBSR
                XT = X0 + RTABL(KLCRE,1,JLCRZC) * TAN(THETA) * COS(PHI)
                YT = Y0 + RTABL(KLCRE,1,JLCRZC) * TAN(THETA) * SIN(PHI)
                ZT = RTABL(KLCRE,1,JLCRZC)
              ELSE
                PHI = PHI - RTABL(KLCRE,1,JLCRZC) * DPHBSR
                XT = X0 - RTABL(KLCRE,1,JLCRZC) * TAN(THETA) * COS(PHI)
                YT = Y0 - RTABL(KLCRE,1,JLCRZC) * TAN(THETA) * SIN(PHI)
                ZT = -RTABL(KLCRE,1,JLCRZC)
              ENDIF
              PH = ATAN2(YT,XT)
              TH = ATAN(SQRT(XT**2+YT**2)/ZT)
              DX = RTABL(KLIDT,I,JLIDDA+2)
              DY = RTABL(KLIDT,I,JLIDDA+3)
            ELSE
              GOTO 15
            ENDIF
C
C Use cluster (Method 4,5,6)
          ELSEIF(MTYP.GE.4) THEN
            DX = RTABL(KLIDT,I,JLIDDA)
            DY = RTABL(KLIDT,I,JLIDDA+1)
            PH = PHCL
            TH = THCL
C
C Use pad boundaries for small area (Method 5,6,7)
            IF(MTYP.GE.5) THEN
              CALL LACCEP(MTYP,IADDR,REJFL)
            ENDIF
          ENDIF
          IF(TH.GT.PIBY2) TH = PI-TH
C
C Take MC energy from BHAB (protect against old DB)
          IF(LCOLS(KBHAB).GE.25) THEN
             IF(IRUNRC.GE.40000) THEN
               ESTAN = RTABL(KBHAB,9,JBHAEG)/2.
             ELSE
               ESTAN = RTABL(KBHAB,IM,JBHAEG)
             ENDIF
          ELSE
             IF(EBEAM.LT.48.) THEN
               ESTAN = 45.6
             ELSE
               ESTAN = 80.
             ENDIF
          ENDIF
C
C Test energy thresholds
          IF(EC/EBEAM.GT.RTABL(KBHAB,IM,JBHAEL)/ESTAN)
     &      ICODL = ICODL+MASK(IM)
          IF(EC/EBEAM.GT.RTABL(KBHAB,IM,JBHAEH)/ESTAN)
     &      ICODH = ICODH+MASK(IM)
C
C Test large fiducial area
          IF(DX.LT.RTABL(KBHAB,IM,JBHAXL+1))            GOTO 14
          IF(DY.LT.RTABL(KBHAB,IM,JBHAYL+1))            GOTO 14
          IF(ABS(COS(PH)).LT.
     &         RTABL(KBHAB,IM,JBHACL+1))                GOTO 14
          IF(ABS(COS(PH)).GT.
     &         RTABL(KBHAB,IM,JBHACH+1))                GOTO 14
          IF(TH.LT.RTABL(KBHAB,IM,JBHATL+1))            GOTO 14
          IF(TH.GT.RTABL(KBHAB,IM,JBHATH+1))            GOTO 14
          ICOD2 = ICOD2 + MASK(IM)
   14     CONTINUE
C
C Test small fiducial area
          IF(MTYP.GE.5) THEN
            IF(.NOT.REJFL) ICOD1 = ICOD1 + MASK(IM)
            GOTO 15
          ENDIF
          IF(DX.LT.RTABL(KBHAB,IM,JBHAXL))              GOTO 15
          IF(DY.LT.RTABL(KBHAB,IM,JBHAYL))              GOTO 15
          IF(ABS(COS(PH)).LT.
     &         RTABL(KBHAB,IM,JBHACL))                  GOTO 15
          IF(ABS(COS(PH)).GT.
     &         RTABL(KBHAB,IM,JBHACH))                  GOTO 15
          IF(TH.LT.RTABL(KBHAB,IM,JBHATL))              GOTO 15
          IF(TH.GT.RTABL(KBHAB,IM,JBHATH))              GOTO 15
          ICOD1 = ICOD1 + MASK(IM)
   15   CONTINUE
C
C Fill track quality in LIDT
        KIDT = KROW(KLIDT,I)
        IW(KIDT+JLIDIF) = IFLAG
C
C Fill acceptance codes in LIFL
        KIFL = KROW(KLIFL,I)
        IW(KIFL+JLIFKH) = ICODH
        IW(KIFL+JLIFKL) = ICODL
        IW(KIFL+JLIFK1) = ICOD1
        IW(KIFL+JLIFK2) = ICOD2
   21 CONTINUE
      GOTO 999
C
  997 CALL REPORT('LIDCOD','AUBOS failure',1)
  999 CONTINUE
      END
#endif
