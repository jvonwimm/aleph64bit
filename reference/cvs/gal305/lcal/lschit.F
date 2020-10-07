*DK lschit
      SUBROUTINE LSCHIT
C--------------------------------------------------------------
C! Deposit hit signal in Lcal scintillator
C  The trackelement is not stopped
C. - P.Hansen - 970301
C. - Called by  GUSTEP
C -----------------------------------------------
#ifndef DOC
      SAVE
C
      INTEGER KLSNT,KLCAL,KLCRA,ISAT,MODU,NNEG,NPOS,KMOD,LE
      INTEGER IFB,MLCAL,NSCI,NPMT
      REAL XIN(3),XOUT(3),DXIN(3)
      REAL SCPOS(3),SHEI,SWID,ATTL,EMIP,EFLU,ADCM(4),THRS
      REAL X,Y,Z,BINV,EDEP,RN1,RN2,DLIGH,SADC,C1,R1,SPOS,SNEG
      INTEGER NAMIND
      EXTERNAL NAMIND
      LOGICAL INIT
      DATA INIT/ .TRUE. /
C
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
      IF(INIT) THEN
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
C Scintillator attenuation length
          ATTL     = RW(KLSNT+LMHLEN+JLSNAL)
C
C Energy deposited by one mip in lead+scint
          EMIP     = RW(KLSNT+LMHLEN+JLSNMM)
C
C Relative fluctuation in energy deposit
          EFLU     = RW(KLSNT+LMHLEN+JLSNFL)
C
C Digitizings per MeV
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
          ATTL     = 100.
          EMIP     = 2.
          EFLU     = 0.3
          ADCM(1)  = 1000.
          ADCM(2)  = 1000.
          ADCM(3)  = 1000.
          ADCM(4)  = 1000.
          THRS     = 0.002
          ISAT     = 6250
          NSCI     = 4
          NPMT     = 2
        ENDIF
        INIT = .FALSE.
        NLCRA = NAMIND('LCRA')
        CALL BKFMT('LCRA','(I)')
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
        MODU = 1
        IF( Y .GT. 0.) MODU = 2
      ELSE
        MODU = 3
        IF( Y .GT. 0.) MODU = 4
      ENDIF
C
C - Make sure the particle hits the scintillator
      IF(ABS(X) .GT. SWID/2. .OR.
     &   ABS(ABS(Y)-SCPOS(2)) .GT. SHEI/2.)            RETURN
C
C - Apply a minimum momentum
      IF (TRKELE(7).LT.THRS)                           RETURN
C
C - Find average energy deposition
C   If geantino deposit 3% of energy in MeV
      IF(IGTRTY .EQ. 6) THEN
        EDEP = 0.03*TRKELE(7)*1000.
C   otherwise apply the PDB factor for low beta
      ELSE
        BINV    = TRKELE(8)/TRKELE(7)
        EDEP    = EMIP*BINV**(5/3)
      ENDIF
C
C - Fluctuate the energy (by at most 0.5 downwards)
      CALL RANNOR(RN1,RN2)
      IF (RN1*EFLU .GT. -0.5) THEN
        EDEP = EDEP*(1.+RN1*EFLU)
      ELSE
        EDEP = 0.5*EDEP
      ENDIF
C
C - Attenuate the light reaching the two photomultipliers
      DLIGH = ABS(Y)-SCPOS(2)+SHEI/2.
      SADC  = EDEP*EXP(-DLIGH/ATTL)
C
C - Divide it between them according to the cosine factor
      C1    = SWID**2+DLIGH**2+X**2
      R1    = SQRT( (C1-2.*SWID*X)/(C1+2.*SWID*X))
      SNEG  = SADC*R1/(1.+R1)
      SPOS  = SADC/(1.+R1)
C
C - Digitize the signal
      NNEG = NINT(SNEG*ADCM(MODU))
      NPOS = NINT(SPOS*ADCM(MODU))
C
C - If output bank exists, add the signal until saturation
      KLCRA = IW(NLCRA)
      IF(KLCRA.NE.0) THEN
        KMOD = KROW(KLCRA,MODU)
        IW(KMOD+1) = MIN( IW(KMOD+1) + NNEG , ISAT )
        IW(KMOD+2) = MIN( IW(KMOD+2) + NPOS , ISAT )
C - Otherwise create bank and add signal until saturation
      ELSE
        LE = LMHLEN+NPMT*NSCI
        CALL AUBOS('LCRA',0,LE,KLCRA,IGARB)
        CALL BLIST(IW,'E+','LCRA')
        IW(KLCRA+1) = NPMT
        IW(KLCRA+2) = NSCI
        KMOD = KROW(KLCRA,MODU)
        IW(KMOD+1) = MIN( NNEG , ISAT )
        IW(KMOD+2) = MIN( NPOS , ISAT )
      ENDIF
  999 RETURN
      END
#endif
