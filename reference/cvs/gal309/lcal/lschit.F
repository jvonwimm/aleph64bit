      SUBROUTINE LSCHIT
C--------------------------------------------------------------
C! Deposit hit signal in Lcal scintillator
C  The trackelement is not stopped
C. - P.Hansen - 970301
C.   Modified - P. Hansen 980301
C. - Called by  GUSTEP
C -----------------------------------------------
#ifndef DOC
      SAVE
C
      INTEGER KLSNT,KLCAL,KLCRA,ISAT,MODU,NNEG,NPOS,KMOD,LE
      INTEGER IFB,MLCAL,NSCI,NPMT,NPHO1,NPHO2
      REAL XIN(3),XOUT(3),DXIN(3),R1,R2,REFF
      REAL SCPOS(3),SHEI,SWID,ATTL,EMIP,EFLU,ADCM(4),THRS
      REAL X,Y,Z,BINV,EDEP,RN1,RN2,PM1,PM2,S1,S2,E1,E2
      INTEGER NAMIND,NLCRA
      LOGICAL INIT
      DATA NCLRA /0/
      DATA INIT/ .TRUE. /
#include "bcs.h"
#include "jobcom.h"
#include "iocom.h"
#include "lcaljj.h"
#include "lsntjj.h"
#include "gckine.h"
#include "tmacrod.h"
#include "bmacro.h"

C -------------------------------------------------------------
C
C Read geometrical and other constants
      IF(NLCRA.EQ.0) THEN
        KLSNT = IW(NAMIND('LSNT'))
        KLCAL = IW(NAMIND('LCAL'))
        IF(KLSNT.NE.0 .AND. KLCAL.NE.0 ) THEN
C
C Nominal x,y,z position of scintillator
          SCPOS(1) = RW(KLCAL+LMHLEN+JLCANP)
          SCPOS(2) = RW(KLCAL+LMHLEN+JLCANP+1)
          SCPOS(3) = RW(KLCAL+LMHLEN+JLCANP+2)
C
C Scintillator height
          SHEI     = RW(KLCAL+LMHLEN+JLCAND+1)
C
C Scintillator width
          SWID     = RW(KLCAL+LMHLEN+JLCAND)
C
C Scintillator attenuation length and average attenuation
          ATTL     = RW(KLSNT+LMHLEN+JLSNAL)
          REFF     = SWID/ATTL/(1.-EXP(-SWID/ATTL))
C
C Number of photoelectr resulting from one mip in scintillator
          EMIP     = RW(KLSNT+LMHLEN+JLSNMM)
C
C Relative fluctuation in measured signal
          EFLU     = RW(KLSNT+LMHLEN+JLSNFL)
C
C Digitizings per photoelectron
          ADCM(1)  = RW(KLSNT+LMHLEN+JLSNMA)
          ADCM(2)  = RW(KLSNT+LMHLEN+JLSNMA+1)
          ADCM(3)  = RW(KLSNT+LMHLEN+JLSNMA+2)
          ADCM(4)  = RW(KLSNT+LMHLEN+JLSNMA+3)
C
C Momentum threshold for detecting a particle
          THRS     = RW(KLSNT+LMHLEN+JLSNTH)
C
C Saturation count (max digitizing)
          ISAT     = IW(KLSNT+LMHLEN+JLSNSA)
C
C Number of scintillators
          NSCI     = IW(KLSNT+LMHLEN+JLSNNS)
C
C Number of phototubes per scintillator
          NPMT     = IW(KLSNT+LMHLEN+JLSNNP)
        ELSE
          IF(KLCAL.EQ.0) WRITE(LOUTIO,'(//,2X,A)')
     &     '+++ LSCHIT +++ No LCAL bank'
          IF(KLSNT.EQ.0) WRITE(LOUTIO,'(//,2X,A)')
     &     '+++ LSCHIT +++ No LSNT bank'
          SCPOS(1) = 0.0
          SCPOS(2) = 30.75
          SCPOS(3) = 307.80
          SHEI     = 26.6
          SWID     = 4.6
          ATTL     = 3.0
          REFF     = 1.955
          EMIP     = 0.6
          EFLU     = 0.6
          ADCM(1)  = 500.
          ADCM(2)  = 500.
          ADCM(3)  = 500.
          ADCM(4)  = 500.
          THRS     = 0.002
          ISAT     = 32000
          NSCI     = 4
          NPMT     = 2
        ENDIF
        CALL BKFMT('LCRA','I')
        NLCRA = NAMIND('LCRA')
      ENDIF
C
C - Signal is deposited at first entrance of a track element
      IF(ITRKEL(8).NE.1)                               RETURN
C
C - Transform track to local coordinates
      CALL LCFRAL(IFB,MLCAL,XIN,XOUT,DXIN)
C
C - Find the scintillator number
      X=XIN(1)
      Y=XIN(2)
      Z=XIN(3)
      IF( MLCAL .LE. 2) THEN
        MODU = 2
        IF( Y .GT. 0.) MODU = 1
      ELSE
        MODU = 3
        IF( Y .GT. 0.) MODU = 4
      ENDIF
C
C - The average number of detected scintillator photons (EDEP)
C   for an incoming geantino is estimated to be n(mip)(2+15*E(GeV))
      IF(IGTRTY .EQ. 6) THEN
        EDEP = EMIP*(2. + 15.*TRKELE(8))
C   otherwise apply the PDB factor for low beta to n(mip)
      ELSE
        BINV    = TRKELE(8)/TRKELE(7)
        EDEP    = EMIP*BINV**(5/3)
      ENDIF
C
C - Find effective light attenuation factors
      IF(ABS(X).LT.SWID/2.) THEN
        R1 = EXP(-(SWID/2.-X)/ATTL)
        R2 = EXP(-(SWID/2.+X)/ATTL)
      ELSEIF(X.GT.0.) THEN
        R1 = 1.
        R2 = 0.
      ELSE
        R1 = 0.
        R2 = 1.
      ENDIF
C
C - Find the number of photons detected by each sensor
      NPHO1 = 0
      NPHO2 = 0
      E1 = EDEP*REFF*R1
      E2 = EDEP*REFF*R2
      IF(E1.GT.0.1) CALL POISSN(E1,NPHO1,IERR)
      IF(E2.GT.0.1) CALL POISSN(E2,NPHO2,IERR)
      S1 = FLOAT(NPHO1)
      S2 = FLOAT(NPHO2)
C
C - Fluctuate the PM pulse
      CALL RANNOR(RN1,RN2)
      IF(S1.GT.0.1) THEN
        PM1 = S1*(1.+RN1*EFLU/SQRT(S1))
      ELSE
        PM1 = 0.
      ENDIF
      IF(S2.GT.0.1) THEN
        PM2 = S2*(1.+RN2*EFLU/SQRT(S2))
      ELSE
        PM2 = 0.
      ENDIF
C
C - Digitize the signal
      NPM1 = NINT(PM1*ADCM(MODU))
      NPM2 = NINT(PM2*ADCM(MODU))
      IF(NPM1.LE.0.AND.NPM2.LE.0) RETURN
C
C - If output bank exists, add the signal until saturation
      KLCRA = IW(NLCRA)
      IF(KLCRA.NE.0) THEN
        KMOD = KROW(KLCRA,MODU)
        IW(KMOD+1) = MIN( IW(KMOD+1) + NPM1 , ISAT )
        IW(KMOD+2) = MIN( IW(KMOD+2) + NPM2 , ISAT )
C - Otherwise create bank and add signal until saturation
      ELSE
        LE = LMHLEN+NPMT*NSCI
        CALL ALBOS('LCRA',0,LE,KLCRA,IGARB)
        CALL BLIST(IW,'E+','LCRA')
        IW(KLCRA+1) = NPMT
        IW(KLCRA+2) = NSCI
        KMOD = KROW(KLCRA,MODU)
        IW(KMOD+1) = MIN( NPM1 , ISAT )
        IW(KMOD+2) = MIN( NPM2 , ISAT )
      ENDIF
  999 RETURN
      END
#endif
