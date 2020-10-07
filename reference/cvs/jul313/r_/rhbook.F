      SUBROUTINE RHBOOK
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++====
C! Book histograms for the skeleton
C
C  Author         S. Orteu       11-feb-1987
C
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
      JOFF = JULSK*1000
      JOFF = JOFF + 1
      CALL HBOOK1(JOFF,'EVENT RECORD SIZE$',50,50.,300.,0.)
      END
#endif
