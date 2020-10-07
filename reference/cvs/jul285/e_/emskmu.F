      SUBROUTINE EMSKMU( NETP1, EXTRAP, EMIP, IMUON)
C=======================================================================
C! Calculates expected energy lost by muons in Ecal and Hcal
C-
C-    Authors : M.N. Minard , M. Pepe       26/02/89
C-
C- If muon is identified, calculates energy to be removed (EMIP)
C- in the three ECAL stacks and in HCAL else EMIP = 0
C- Input : NETP1       Track index
C-         EXTRAP      Extrapolation point in the 5 stacks
C- OUTPUT: EMIP(1-5)   Expected energy lost by the muon in the 5 stacks
C-         IMUON  = 1  Muon candidate found
C                   0  Not a muon candidate
C?
C!======================================================================
#ifndef DOC
#include "hsdajj.h"
#include "eminic.h"
#include "etp1jj.h"
#include "emskex.h"
#include "hmadjj.h"
#include "mcadjj.h"
#include "trexjj.h"
#include "alcons.h"
#include "bcs.h"
      EXTERNAL NAMIND
      DIMENSION EXTRAP(3,5), EPOIN(3), EMIP(5)
      EXTERNAL EFNDSC
      INTEGER EFNDSC
      PARAMETER ( NPLEC=22, NPLBAR=23, NPLFR=12 )
C
C- Angular dependance of energy lost by a muon parametrized
C- as EMIP(5) = EMUON +SLOPE*ABS(TAN(THNOR)) where THNOR
C- is the angle between track and normal to wire
C
      DATA NAMCAD /0/
#include "bmacro.h"
C
C
C
      IF (NAMCAD.EQ.0) THEN
        NAMCAD = NAMIND ('MCAD')
        NAHMAD = NAMIND ('HMAD')
        NAETP1 = NAMIND ('ETP1')
      ENDIF
C
      KETP1 = IW(NAETP1)
C
      CALL VZERO(EMIP,5)
      IMUON = 0
      IFLMCA = 0
      IFLHMA = 0
      ISEX = 0
      ENDEP = 0.
      NPLAN = 0
C
      KMCAD = IW(NAMCAD)
      IF ( KMCAD.LE.0) THEN
        GO TO 100
      ENDIF
C
      NTRACK = LROWS(KMCAD)
C
C
C- track loop
C
      DO 50 IT = 1,NTRACK
C
        JMCAD = KROW(KMCAD,IT)
        ITRACK = IW(JMCAD+JMCATN)
C
C- check whether track has been associated to muon chambers
C
        IF(ITRACK.NE.NETP1) GO TO 50
C
C- association has taken place
C
        IFLMCA = 1
        GO TO 100
C
   50 CONTINUE
C
C- examine here HMAD bank
C
C
  100 CONTINUE
C
      KHMAD = IW (NAHMAD)
      IF ( KHMAD.LE.0) THEN
        GO TO 200
      ENDIF
      NTRACK = LROWS(KHMAD)
C
C
C- track loop
C
      DO 150 IT = 1,NTRACK
C
        JHMAD = KROW(KHMAD,IT)
        ITRACK = IW(JHMAD+JHMATN)
C
C- check whether track has been identified in HCAL
C
        IF(ITRACK.NE.NETP1) GO TO 150
        IF(IW(JHMAD+JHMAIF).NE.1) GO TO 150
        NPLAN = IW(JHMAD+JHMANF)
        ENDEP = RW(JHMAD+JHMAED)
        ENDPL = ENDEP/NPLAN
C
C- association has taken place
C
        IFLHMA = 1
        GO TO 200
C
  150 CONTINUE
  200 CONTINUE
C
C- "or" information from HMAD and MCAD
      IF(IFLMCA.EQ.0.AND.IFLHMA.EQ.0) RETURN
C
C-     Set Muon Flag
C
      IMUON = 1
C
C- get incidence angle of muon from TREX bank
C
      JTREX = NLINK('TREX',NETP1)
      IF(JTREX.EQ.0) THEN
        CALL RERROR ('EMSKMU',1,'TREX bank not found')
        GO TO 1000
      ENDIF
C
      NVETT=LROWS(JTREX)
C
      DO 300 N=1,NVETT
C
        MEDIUM = ITABL(JTREX,N,JTRERG)
        IF(MEDIUM.LT.3000) GO TO 300
C- we are in HCAL
C
C- as there is a difference between barrel and end-cap modules
C- first find which subcomponent we are in
C
        KSC = MEDIUM*.01 - 30
C
        IF(KSC.NE.1.AND.KSC.NE.3.AND.KSC.NE.4) GO TO 300
C
C- we are in end-cap or barrel
C
        X = RTABL(JTREX,N,JTREX3)
        Y = RTABL(JTREX,N,JTREX3+1)
        Z = RTABL(JTREX,N,JTREX3+2)
        PX = RTABL(JTREX,N,JTREP3)
        PY = RTABL(JTREX,N,JTREP3+1)
        PZ = RTABL(JTREX,N,JTREP3+2)
        PTOT = SQRT(PX**2+PY**2+PZ**2)
C
C- angle of incidence
C
        IF(PTOT.LE.0.) THEN
          CALL RERROR ('EMSKMU',2,' Momentum from TREX is zero')
          GO TO 1000
        ENDIF
C
        COSTHE = PZ/PTOT
C- phi angle
        IF(X.EQ.0.) THEN
          CALL RERROR ('EMSKMU',3,'X of extrapol. is zero in TREX')
          GO TO 1000
        ENDIF
        PHI = ATAN2(Y,X)
        IF(PHI.LT.0.) PHI =  PHI  + TWOPI
        IF(PHI.GT.TWOPI) PHI = PHI - TWOPI
        PHIDG = PHI*RADEG
        NPLTOT = NPLBAR
C
        IF(KSC.EQ.3 .OR. KSC.EQ.4) THEN
C
C- we are in the end-cap
C
          NPLTOT = NPLEC
          COSTHE  = PX/PTOT
C
C- isex=1 in the endcap sextants with tubes inclined at 60 degrees
C
          IF(PHI.GT.PIBY3.AND.PHI.LT.2.*PIBY3) ISEX=1
          IF(PHI.GT.4.*PIBY3.AND.PHI.LT.5.*PIBY3) ISEX=1
C
          IF(ISEX.EQ.1) THEN
C
C- end-cap A
            COSTHE = (PX*COS(PIBY3)+PY*SIN(PIBY3))/PTOT
C
            IF(Z.LT.0.) THEN
C- end-cap B
              COSTHE = (-PX*COS(PIBY3)+PY*SIN(PIBY3))/PTOT
            ENDIF
          ENDIF
C
        ENDIF
C
        IF(ABS(COSTHE).GT.1.) COSTHE = SIGN(1.,COSTHE)
        THE = ACOS(COSTHE)
        IF(THE.LT.PIBY2) THEN
          THNOR = PIBY2 - THE
        ELSE
          THNOR = THE - PIBY2
        ENDIF
C
C- expected energy loss in HCAL
C
        THEDG = THNOR*RADEG
        XX = ABS(TAN(THNOR))
        ENGY    =  EMUON + SLOPE * XX
        EMIP(4) =  ENGY
        EMIP(5) =  ENGY - EMIP(4)
C
C
        IF(IFLHMA.GT.0) THEN
          EMIP(4) = EMIP(4) * FLOAT(NPLAN)/FLOAT(NPLTOT)
          EMIP(5) = EMIP(5) * FLOAT(NPLAN)/FLOAT(NPLTOT)
        ENDIF
C
        GO TO 500
C
  300 CONTINUE
C
C
  500 CONTINUE
C
C
C- Now calculate expected energy in ECAL
C
      JETP1 = KROW ( KETP1 , NETP1 )
      PMOM  = RW(JETP1+JETPPT)
      DO 800 I =1,3
        EPOIN(1) = EXTRAP(1,I)
        EPOIN(2) = EXTRAP(2,I)
        EPOIN(3) = EXTRAP(3,I)
        IF(EPOIN(1).EQ.0.) GO TO 800
        PHIECA = ATAN2(EPOIN(2),EPOIN(1))
        R = SQRT(EPOIN(1)**2+EPOIN(2)**2+EPOIN(3)**2)
        THEECA = ACOS(EPOIN(3)/R)
C
        KSC = EFNDSC(EPOIN)
        IF(KSC.EQ.0) THEN
          GO TO 1000
        ENDIF
        IF(KSC.EQ.2) THEN
C- we are in the ECAL barrel
          IF(PHIECA.LT.0.) PHIECA =  PHIECA  + TWOPI
          IF(PHIECA.GT.TWOPI) PHIECA = PHIECA - TWOPI
          PHIDIS = PHIECA - TILT
          PHIOFF = AMOD(PHIDIS,PIBY6)
          PHICOR = ABS(PHIOFF - PIBY12)
          THECOR = ABS(PIBY2 - THEECA)
        ELSE
C- we are in the ECAL end-cap
          PHICOR = 0.
          THECOR = THEECA
        ENDIF
C
        EMIP(I) = ABS(ENFMIP(I)/(COS(THECOR)*COS(PHICOR)))
        EMIP(I) = EMIP(I)*FUDGE(I)
C- correct the standard MIP energies for the small energy dependence
        IF(PMOM.LT.45.) THEN
          EMIP(I) = EMIP(I) - EXP(-(ENDPA(I,1)+ENDPA(I,2)*PMOM))
        ENDIF
  800 CONTINUE
C
 1000 CONTINUE
      RETURN
      END
#endif
