      SUBROUTINE EFBOOK
C=======================================================================
C!    Booking of histograms for mask analysis
C-
C-    Authors : M.N. Minard , M. Pepe       26/02/89
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
      NHIS = 1000*JULEF
      IF (( FDETRF(JULEC).OR.FDETRF(JULHC)).AND. FDETRF(JULTP)) THEN
        CALL HBOOK1(NHIS+1,'Efitted using masks$',60,0.,120.,0.)
        CALL HBOOK1(NHIS+7,'Word for rejected tks$',50,0.,50.,0.)
        CALL HBOOK2(NHIS+8,'PFRFT vs Emask       $',
     &    50,0.,50.,50,0.,50.,0.)
        CALL HBOOK2(NHIS+9,'PFRFT vs Emask muons $',
     &    50,0.,25.,50,0.,50.,0.)
      ENDIF
      IF ( FDETRF(JULEC).OR.FDETRF(JULHC)) THEN
        CALL HBOOK1(NHIS+2,'Efitted from calorimetry$',60,0.,120.,0.)
        CALL HBOOK1(NHIS+10,'Energy using masked tracks$',100,0.,120.,
     &0.)
        CALL HBOOK1(NHIS+11,'Energy using calorimetry$',100,0.,120.,0.)
        CALL HBOOK1(NHIS+12,'Thrust with masks$',20,0.,1.,0.)
        CALL HBOOK2(NHIS+13,'Energy vs Thrust angle$',
     &  50,0.,90.,50,0.,120.,0.)
        CALL HBOOK1(NHIS+14,'# reconstructed jets$',20,0.,20.,0.)
      ENDIF
      RETURN
      END
#endif
