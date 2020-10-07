*****************************************************************
      SUBROUTINE HEMI_C(HAXIS,NC,NPART,LHEMI1,LHEMI2)
*****************************************************************
*     Aim = defines alpha's  hemispheres
*
*     inputs :  - NC
*               - Npart(NC)
*
*     outputs:  - LHEMI1(nc)     1 if H1 / 0 if H2
*               - LHEMI2(nc)     1 if H2 / 0 if H1
*****************************************************************
#include "qcde.h"
       DIMENSION HAXIS(3),NPART(10),LHEMI1(10),LHEMI2(10)
#include "qmacro.h"
C
C    Initialization
C
      do i = 1,10
         lhemi1(i) = 0
         lhemi2(i) = 0
      enddo
C*********
C    Hemispheres' definition from HAXIS in BTAGRAW common
C    Allows to be consistent with HJET option.
C
C     IHAXIS = KVNEW(DUMMY)
C     CALL QVSET3(IHAXIS,HAXIS)
        DO IC = 1,NC
           IPART = NPART(IC)
           XDOT = QX(IPART)*HAXIS(1) + QY(IPART)*HAXIS(2)
     $          + QZ(IPART)*HAXIS(3)
           IF (XDOT.GT.0.) THEN
               LHEMI1(IC) = 1
           ELSE
               LHEMI2(IC) = 1
           ENDIF
        ENDDO
C
C       made
C
  99    CONTINUE
        RETURN
        END
C
