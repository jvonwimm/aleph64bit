      SUBROUTINE LCSHOW
C--------------------------------------------------------------
C! Shower parametrisation in LCal
C. - J.Dines Hansen & P.Hansen - 860417
C.                               modified by F.Ranjard - 890317
C.                               modified by P.Hansen -  940701
C. - Sets GEANT stop flag
C.   ********************New for 94*****************************
C. - Use longitudinal parametrization:
C.     dN/dS   = (ETOT/ECRTLC)*B*(B*S)**(A-1)*exp(-B*S)/gamma(A)
C. - The P-terms are from fit to Lcal data,
C. - the DP-terms from Ecal test beam fit:
C.      A/B    = PAR1LC + DPR1LC*alog(E)
C.      1/B    = PAR2LC + DPR2LC*alog(E)    for E>0.1GeV
C.      A/B    = PAR3LC + DPR3LC*alog(E)
C.      1/B    = PAR4LC + DPR4LC*alog(E)    for E<0.1GeV
C. - Use lateral parametrization:           R in cm
C.     d(dN/dS)/dR = (dN/dS)*exp(-R/R0)/R0
C.         R0      = PAR5LC + DPR5LC*S*B/A
C. - PAR1LC is fluctuated by the relative amount PAR6LC
C. - PAR2LC is fluctuated by the relative amount PAR8LC
C. - R0     is fluctuated by the relative amount PAR7LC
C.***********************New for 96*****************************
C.                               modified my.M.N.Minard - 960527
C.   introduce geantino parametrisation.
C.
C. - Called by  LCHIT                            from this .HLB
C. - Calls      LCSTRT, LCFRAL, LCXYPA, CAHIST   from this .HLB
C.              RNDM                             from   CERNLIB
C.              POISSN                           from    GENSEC
C -----------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION XIN(3),DXIN(3),XOUT(3)
      DIMENSION XYZ(3),XYAV(3)
      EXTERNAL RNDM
#include "alcons.h"
#include "iocom.h"
#include "bcs.h"
#include "lccomc.h"
#include "lcnamc.h"
#include "capajj.h"
#include "capano.h"
      CHARACTER*4 CHAINT
      CHARACTER*4 EXNAME
#include "tmacrod.h"
#include "bmacro.h"
C -------------------------------------------------------------
C
      ETOT = TRKELE(8)
C
C- Very small energies are absorbed before entering the gas
C
      IF ( ETOT.LT.ECRTLC ) THEN
        ITRKEL(9) = NOMOR
        GO TO 999
      ENDIF
C
C
C-   Geantino enters the volume or is created in the volume
C    Deposit shower only at the first call
C    Define ISTYP = 0 (already tracked in Lcal)
C                 = 1 (Geantino parameter to be set & Lcal tracking)
C                 = 2 (Geantino from other calorimeter do Lcal tracking)
      ISTYP = -1
      JCAPA = NLINK('CAPA',ITRKEL(1))
      IF (JCAPA.EQ.0) THEN
         CALL ALTELL ('LCSHOW: no CAPA bank - STOP',9,'FATAL')
      ELSE
         ISTYP = 0
         KCAPA = JCAPA +LMHLEN
         EXNAME = CHAINT(IW(KCAPA+JCAPNA))
         TINOX0 = RW (KCAPA+JCAPTX)
         IF (EXNAME.NE.'LCAL') ISTYP =1
         IF (TINOX0.GT.0.001.AND.ISTYP.EQ.1) ISTYP =2
      ENDIF
C
C - Geantino already tracked
      IF (ISTYP.LE.0) GO TO 999
C
      EMNAME = 'LCAL'
      IF (ISTYP.EQ.1) THEN
C
C - Geantino is created in LCAL
C    Compute longidutinal shower parameters
        IF(ETOT.GT.0.1) THEN
          E1  = PAR1LC + DPR1LC*ALOG(ETOT)
          E2  = PAR2LC + DPR2LC*ALOG(ETOT)
        ELSE
          EN  = AMAX1(ETOT,0.03)
          E1  = PAR3LC + DPR3LC*ALOG(EN)
          E2  = PAR4LC + DPR4LC*ALOG(EN)
        ENDIF
C    fluctuate A/B and 1/B
        CALL RANNOR(RN1,RN2)
        IF(ABS(RN1).LT.4.) E1 = E1*(1.+PAR6LC*RN1)
        CALL RANNOR(RN3,RN4)
        IF(ABS(RN3).LT.4.) E2 = E2*(1.+PAR8LC*RN3)
        A  = E1/E2
        B  = 1./E2
        EMALFA = A
        EMBETA = B
        EMALM1 = A-1.
        S0 = 0.
      ELSE
C
C - Geantino from other calorimeter
        S0 = TINOX0
        A  = EMALFA
        B  = EMBETA
      ENDIF
C
      CALL CAPSTO(ITRKEL(1))
C
C - Transform trackelement into local system
      CALL LCFRAL(IFB,MODU,XIN,XOUT,DXIN)
C - Add the track energy to run summary bank
      KWS  = IW(NALCWS)
      IF(KWS.NE.0) THEN
         IW(KWS+LMHLEN+4+IFB) = IW(KWS+LMHLEN+4+IFB)+INT(ETOT*1000.)
      ENDIF
C
C - Find storey and layer no. where shower starts
      IF(MODU.LE.2) THEN
        ZABS = -XIN(3)
      ELSE
        ZABS = XIN(3)
      ENDIF
      CALL LCSTRT(ZABS,ISTMN,LAYER,LMIN,ZWIRE)
C
C - Move starting point to preceeding wire-plane
      LAYER = LAYER - 1
      DL = (ZWIRE-ZABS)/ABS(DXIN(3))
      XIN(1) = XIN(1)+DXIN(1)*DL
      XIN(2) = XIN(2)+DXIN(2)*DL
      XIN(3) = XIN(3)+DXIN(3)*DL
C
C - Compute normalization
      ZFIR = AMAX1(ZABS,0.)-
     &       FLOAT(INT(AMAX1(ZABS,0.)/ZSTPLC(1)))*ZSTPLC(1)
      SNORM = SSTPLC(2)*ETOT*(B**A)/(ECRTLC*GAMMA(A))
      DL    = 0.
      S     = -ZFIR*SSTPLC(1)/ZSTPLC(1) + S0
      KLDWP = IW(NAMIND('LDWP'))
      KLDST = IW(NAMIND('LDST'))
C
C - Step through storeyes
      DO 300 ISTOR = ISTMN,3
         LMAX  = LCNLAY(ISTOR)
         SSTEP = SSTPLC(ISTOR)
         STEPL = ZSTPLC(ISTOR)/ABS(DXIN(3))
C
C - Step through layers
       DO 400 LAY = LMIN,LMAX
         LAYER = LAYER + 1
         S     = S  + SSTEP
         DL    = DL + STEPL
C - Number of hits in the layer
         AVHIT = SNORM*S**(A-1.)*EXP(-B*S)
         IF (AVHIT.GT.0.) THEN
            CALL POISSN(AVHIT,NHIT,IER)
         ELSE
           WRITE (LOUTIO,*)' LCSHOW: AVHIT=<0.,B,S ',AVHIT,B,S
           GOTO 999
         ENDIF
         IF (NHIT.EQ. 0)                         GOTO 400
         NWHIT = 0
C - Coordinates of shower centroid
         XYAV(1) = XIN(1) + DL*DXIN(1)
         XYAV(2) = XIN(2) + DL*DXIN(2)
         XYAV(3) = XIN(3) + DL*DXIN(3)
C - Compute radial parameter
         R0    = PAR5LC + DPR5LC*S*B/A
C - Same fluctuation in all planes
         R0    = R0*(1.+RN2*PAR7LC)
C - Loop over hits
         DO 500 IHIT = 1,NHIT
C - Choose x,y coordinates of hit
            R    = -R0*ALOG(RNDM(DUMMY))
            PHI  = RNDM(DUMMY)*TWOPI
            XYZ(1) = XYAV(1) + R*COS(PHI)
            XYZ(2) = XYAV(2) + R*SIN(PHI)
            XYZ(3) = XYAV(3)
C - Find tower address of hit
            CALL LCXYPA(MODU,LAYER,XYZ,IPAD)
C - If inside the acceptance
            IF(IPAD.EQ.0)                        GOTO 500
C - Update number of hits in this plane (if alive)
            IFALI = 1
            IF(KLDWP.GT.0) THEN
              DO 510 I=1,LROWS(KLDWP)
                IF(ITABL(KLDWP,I,4).EQ.(MODU-1)*64+LAYER) THEN
                  IFALI = 0
                  GOTO 500
                ENDIF
  510         CONTINUE
            ENDIF
            CALL POISSN(16.,N16,IER)
            NWHIT = NWHIT + N16
            CALL CAHIST(NALSHI,IPAD,ISTOR,N16)
  500    CONTINUE
C - Add the hits to the wire-plane history bank
         IF (NWHIT .GT. 0) CALL CAHIST(NALWHI,MODU,LAYER,NWHIT)
  400 CONTINUE
      LMIN = 1
  300 CONTINUE
C
C - end
  999 RETURN
      END
#endif
