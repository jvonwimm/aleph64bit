      SUBROUTINE UFREIN
C!  Read the UFG2 bank for the double Gaussian parameters and initialize
C!  If no UFG2 bank or card, do a single Gaussian, no energy loss or
C!  filtering
C!
C!  ENTRY UF2SCA(CF,CW,TW,IE)    sets scattering parameters to user-defined
C!                               values
C!  Parameters:  CF(R*4)         Fraction of scatters in the core Gaussian
C!               CW(R*4)         Width of the core Gaussian,
C!                               normalized to default = 1
C!               TW(R*4)         Width of the tail Gaussian
C!               IE(I*4)         If 0, No energy loss, if 1, energy loss by
C!                               particle,
C!
C!  ENTRY UFTFIL sets the filtering probability to a user defined value
C!  *and returns the old value*
C!  Input:       FP(R*4)         Probability cut to filter (remove) a
C!                               coordinate (MODIFIED BY CALL)
C   Returns:     FP(R*4)         Old probability cut
C!
#ifndef DOC
#include "uftcom.h"
      LOGICAL INIT/.TRUE./
C ---------------------------------------------------------------------
C
C Read UFG2 from Dbase, cards, or return default values - once only!
      IF (INIT) THEN
        CALL UFG2GT(CF,CW,TW,IE,FP)
        INIT = .FALSE.
      ENDIF

      GOTO 100

      ENTRY UF2SCA(CFU,CWU,TWU,IEU)

      IF (INIT) THEN
        CALL UFG2GT(CF,CW,TW,IE,FP)
        INIT = .FALSE.
      ENDIF
      CF = MIN(MAX(CFU,0.),1.)
      CW = MAX(CWU,0.01)
      TW = MAX(TWU,0.01)
      IE = MIN(MAX(IEU,0),1)

      GOTO 100

      ENTRY UFTFIL(FPU)

      IF (INIT) THEN
        CALL UFG2GT(CF,CW,TW,IE,FP)
        INIT = .FALSE.
      ENDIF
      FPTEMP = FP
      FP = MIN(MAX(0.,FPU),1.)
      FPU = FPTEMP

      GOTO 200

C! Set up internal (REAL*8) variables after full initialization or new
C! scattering parameters

 100  CONTINUE
      CORE_FRACT = DBLE(CF)
      TAIL_FRACT = 1.D0 - CORE_FRACT
      CORE_WIDTH = DBLE(CW)**2
      TAIL_WIDTH = DBLE(TW)**2
      DO_ELOSS = IE
C
C ---------------------------------------------------------------------

C!  Initialize coord filtering prob and chi^2 cuts
 200  CONTINUE
      FILTER_PROB = DBLE(FP)
      IF ( FILTER_PROB .LT. 0. ) FILTER_PROB = 0.
      IF ( FILTER_PROB .GT. 1. ) FILTER_PROB = 1.
      CH2_1D = 1.E30
      CH2_2D = 1.E30
      IF ( FILTER_PROB .GT. 0. )  THEN
        CH2_1D = CHISIN(1.-FILTER_PROB,1)
        CH2_2D = -2.*ALOG(FILTER_PROB)
      ENDIF

      RETURN
      END
#endif
