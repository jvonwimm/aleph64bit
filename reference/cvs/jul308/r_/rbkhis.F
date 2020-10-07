      SUBROUTINE RBKHIS
C--------------------------------------------------------------------
C! The histograms are booked here.
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
      IF (JHISRF(JULVD).GE.1) CALL VDBOOK
      IF (JHISRF(JULEC).GE.1) CALL ECBOOK
      IF (JHISRF(JULCA).GE.1) CALL CABOOK
      IF (JHISRF(JULHC).GE.1) CALL HCBHIS
      IF (JHISRF(JULIT).GE.1) CALL ITBOOK
      IF (JHISRF(JULMU).GE.1) CALL MUBOOK
      IF (JHISRF(JULTP).GE.1) CALL TPBOOK
      IF (JHISRF(JULLC).GE.1) CALL LCBOOK
      IF (JHISRF(JULSA).GE.1) CALL SABOOK
      IF (JHISRF(JULSI).GE.1) CALL SIBOOK
      IF (JHISRF(JULYR).GE.1.OR.JHISRF(JULYT).GE.1) CALL YYBOOK
      IF (JHISRF(JULSK).GE.1) CALL RHBOOK
      IF (JHISRF(JULBO).GE.1) CALL OMBOOK
      IF (JHISRF(JULEF).GE.1) CALL EFBOOK
      CALL USBOOK
C
      END
#endif
