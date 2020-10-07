      SUBROUTINE X1OUTP
C ----------------------------------------------------------------------
C.
C. - Author   : A. Putzer  - 86/08/08  FOR GALEPH 13.0
C. - Modified : A. Putzer  - 87/04/04  FOR GALEPH 17.0
C. - Modified : A. Putzer  - 87/10/28  FOR GALEPH 19.3
C. - Modified : C. Geweniger - 88/10/11  for GALEPH 20.1
C. - Modified : E. Blucher - 89/15/2 for ALEPHLIB
C. - Modified : C. Geweniger - 89/09/00 for ALEPHLIB 9.9
C.
C! - Fill Level 1 Trigger ADC bank
C.
C? - The energy values are converted into ADC counts.
C?
C? - The trigger output bank 'X1AD' is filled.
C.
C.
#ifndef DOC
      SAVE
#include "bcs.h"
#include "x1tcom.h"
#include "x1namc.h"
C.
C ----------------------------------------------------------------------
C.
      PARAMETER (LXT1=1,NSEG2=NSEGM/2,NHCT=1,NHCW=NHCT+NSEG2)
      PARAMETER (NECT=NHCW+NSEG2, NECW=NECT+NSEG2, NXT1=NECW+NSEG2)
      PARAMETER (NPADC=96,NCADC=4096)
      PARAMETER (NFSET=5,NFSHW=150,NFSHT=5,NFSEW=10)
      PARAMETER (NFTET=40,NFTHT=40,NFTEW=80,NFSLT=40)
      PARAMETER (I2P16=65536)
C.
#include "bmacro.h"
C ----------------------------------------------------------------------
C.
C  - Convert segment energies into ADC counts and fill them into the
C    bank X1AD
C.
      KX1AD=IW(NAX1AD)+LMHLEN
C.
C.
C -  HC tower trigger
C.
C.
      KX1AD=KX1AD+1
      IW(KX1AD)=INTCHA('HCT ')
      DO 101 I = 1,NSEGM,2
C -
C -  ADC values :  0.0 GeV = channel NPADC
C -                add 1000/NFSHT channels/GeV
C -                (Input values in MeV)
C -
        KX1AD = KX1AD + 1
        IW(KX1AD) = MIN((IHTSUM(I)/NFSHT + NPADC),NCADC)*I2P16 +
     +              MIN((IHTSUM(I+1)/NFSHT + NPADC),NCADC)
  101 CONTINUE
C.
C.
C -  HC wire trigger
C
C.
      KX1AD=KX1AD+1
      IW(KX1AD)=INTCHA('HCW ')
      DO 201 I = 1,NSEGM,2
C.
C -  ADC values :  0 planes = channel NPADC
C -                for each plane fired NFSHW channels are added
C.
        KX1AD = KX1AD + 1
        IW(KX1AD) = MIN((IHWSUM(I)*NFSHW + NPADC),NCADC)*I2P16 +
     +              MIN((IHWSUM(I+1)*NFSHW + NPADC),NCADC)
  201 CONTINUE
C.
C.
C -  EC Tower Trigger
C.
C.
      KX1AD=KX1AD+1
      IW(KX1AD)=INTCHA('ECT ')
      DO 301 I = 1,NSEGM,2
C -
C -  ADC values :  0.0 GeV = channel NPADC
C -                add 1000/NFSET channels/GeV
C -                (Input values in MeV)
C -
        KX1AD = KX1AD + 1
        IW(KX1AD) = MAX(MIN((IETSUM(I)/NFSET + NPADC),NCADC),0)*I2P16 +
     +              MAX(MIN((IETSUM(I+1)/NFSET + NPADC),NCADC),0)
  301 CONTINUE
C.
C.
C -  EC wire trigger
C.
C.
      KX1AD=KX1AD+1
      IW(KX1AD)=INTCHA('ECW ')
      DO 401 I = 1,NSEGM,2
C -
C -  ADC values :  0.0 GeV = channel NPADC
C -                add 1000/NFSEW channels/GeV
C -                (Input values in MeV)
C -
        KX1AD = KX1AD + 1
        IW(KX1AD) = MIN((IEWSUM(I)/NFSEW + NPADC),NCADC)*I2P16 +
     +              MIN((IEWSUM(I+1)/NFSEW + NPADC),NCADC)
  401 CONTINUE
C.
C.
C -  LC tower trigger
C.
C.
      KX1AD=KX1AD+1
      IW(KX1AD)=INTCHA('LCT ')
      DO 501 I = 1,NSEGL,2
C -
C -  ADC values :  0.0 GeV = channel NPADC
C -                add 1000/NFSLT channels/GeV
C -                (Input values in MeV)
C -
        KX1AD=KX1AD+1
        IW(KX1AD) = MIN((ILTSUM(I)/NFSLT + NPADC),NCADC)*I2P16 +
     +              MIN((ILTSUM(I+1)/NFSLT + NPADC),NCADC)
  501 CONTINUE
C
  999 CONTINUE
      RETURN
      END
#endif
