      SUBROUTINE LPRCAL
C-----------------------------------------------------------------------
C! Print energy calibration
C!
C!   Author : P.H. Hansen 891210
C!   Input    : LEHI,LVHI
C!   Output   : Calibration print
C!
C!   Description :
C!   ===========
C?   Calculate and print mean and rms of energy calibration
C?   and vertex histograms.
C!
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "rconds.h"
#include "lcnamc.h"
#include "lphist.h"
#include "lehijj.h"
#include "lvhijj.h"
#include "bmacro.h"
C---------------------------------------------------------------------
C
      KLEHI = IW(NALEHI)
      IF(KLEHI.LE.0)                                 GOTO 999
      BS = RTABL(KLEHI,1,JLEHBI)
      EL = RTABL(KLEHI,1,JLEHEL)
      NB = LCOLS(KLEHI)-2
      WRITE(LOUTRL,'(/,1X,A,F5.1,A,F5.1,A,I3)')
     &  'Energy histograms: Binsize =',BS,'  Elow =',EL,'  Nbin =',NB
C
C At this beam energy, we expect to measure an average module energy
      EPRED=0.95*ENERRC/2.
      WRITE(LOUTRL,'(2X,A,F8.1,A,/)') 'Expected energy:',EPRED,' GeV'
      DO 84 MODU=1,4
        KL = KROW(KLEHI,2*(MODU-1)+1)
        ESTOR = 0.
        ESRMS = 0.
        ANSTO = 0.
        DO 82 J=1,NB
          ESTOR = ESTOR + RW(KL+J)*(EL+BS*(FLOAT(J)-0.5))
          ESRMS = ESRMS + RW(KL+J)*(EL+BS*(FLOAT(J)-0.5))**2
          ANSTO = ANSTO + RW(KL+J)
   82   CONTINUE
        KL = KROW(KLEHI,2*(MODU-1)+2)
        EWIRE = 0.
        EWRMS = 0.
        ANWIR = 0.
        DO 83 J=1,NB
          EWIRE = EWIRE + RW(KL+J)*(EL+BS*(FLOAT(J)-0.5))
          EWRMS = EWRMS + RW(KL+J)*(EL+BS*(FLOAT(J)-0.5))**2
          ANWIR = ANWIR + RW(KL+J)
   83   CONTINUE
        ANSTO = AMAX1(ANSTO,1.)
        ANWIR = AMAX1(ANWIR,1.)
        ESTOR = ESTOR/ANSTO
        ESRMS = SQRT(AMAX1(ESRMS/ANSTO - ESTOR**2,0.))
        EWIRE = EWIRE/ANWIR
        EWRMS = SQRT(AMAX1(EWRMS/ANWIR - EWIRE**2,0.))
        WRITE(LOUTRL,'(1X,A,I2,A,2F5.1,A,2F5.1,A,I5)')
     &   '  Module',MODU,'  Estor(mean,rms):',ESTOR,ESRMS,
     &   '  Ewire(mean,rms):',EWIRE,EWRMS,'  Nevt:',INT(ANSTO)
   84 CONTINUE
      KLVHI = IW(NALVHI)
      IF(KLVHI.LE.0)                                 GOTO 999
      BS = RTABL(KLVHI,1,JLVHBI)
      XL = RTABL(KLVHI,1,JLVHXL)
      NB = LCOLS(KLVHI)-2
      WRITE(LOUTRL,'(/,1X,A,F5.2,A,F5.1,A,I3)')
     &  'Vertex histograms: Binsize =',BS,'  Xlow =',XL,'  Nbin =',NB
      DO 94 MM=1,2
        KL = KROW(KLVHI,2*(MM-1)+1)
        XAV = 0.
        XRMS = 0.
        ANX = 0.
        DO 92 J=1,NB
          XAV = XAV + RW(KL+J)*(XL+BS*(FLOAT(J)-0.5))
          XRMS = XRMS + RW(KL+J)*(XL+BS*(FLOAT(J)-0.5))**2
          ANX = ANX + RW(KL+J)
   92   CONTINUE
        KL = KROW(KLVHI,2*(MM-1)+2)
        YAV = 0.
        YRMS = 0.
        ANY = 0.
        DO 93 J=1,NB
          YAV = YAV + RW(KL+J)*(XL+BS*(FLOAT(J)-0.5))
          YRMS = YRMS + RW(KL+J)*(XL+BS*(FLOAT(J)-0.5))**2
          ANY = ANY + RW(KL+J)
   93   CONTINUE
        ANX = AMAX1(ANX,1.)
        ANY = AMAX1(ANY,1.)
        XAV = XAV/ANX
        XRMS = SQRT(AMAX1(XRMS/ANX - XAV**2,0.))
        YAV = YAV/ANY
        YRMS = SQRT(AMAX1(YRMS/ANY - YAV**2,0.))
        WRITE(LOUTRL,'(1X,A,I2,A,2F5.2,A,2F5.2,A,I5)')
     &   '  Module',MM,'  Xver(mean,rms):',XAV,XRMS,
     &   '  Yver(mean,rms):',YAV,YRMS,'  Nevt:',INT(ANX)
   94 CONTINUE
C
  999 CONTINUE
      END
#endif
