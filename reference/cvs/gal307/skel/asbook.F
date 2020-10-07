*DK asbook
      SUBROUTINE ASBOOK
C ----------------------------------------------------------------------
C. - F.Ranjard - 850611
C! Book histograms for the job
C. - Called by ASIRUN                                  from this .HLB
C. - calls     detector booking routine                from this .HLB
C ------------------------------------------------------------------------
#ifndef DOC
#include "jobcom.h"
C --------------------------------------------------------------------
C
      IF (FHISJO(1)) CALL VDBOOK
      IF (FHISJO(4)) CALL ECBOOK
      IF (FHISJO(6)) CALL SABOOK
      IF (FHISJO(8)) CALL MUBOOK
      IF (FHISJO(9)) CALL SIBOOK
      IF (FHISJO(11)) THEN
         CALL X1BOOK
         CALL X2BOOK
      ENDIF
C
      END
#endif
