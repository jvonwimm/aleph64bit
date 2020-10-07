      SUBROUTINE ECLGEO
C----------------------------------------------------------------------
C!  - Geometrical stacks parameters.
C!  Called one time by run to fill / EZLUGE /.
C!
C!  AUTHOR   : J.Badier  22/02/88
C!
C!  BANKS :
C!    INPUT   : NONE
C!    OUTPUT  : NONE
C!    CREATED : NONE
C!
C?
C!======================================================================
#ifndef DOC
      DIMENSION ST(3),PVC(2),COU(2)
#include "ezluge.h"
#include "alcons.h"
#include "estage.h"
C
      NBJTEZ = 228
      IDOUEZ = 12
      LIMREZ(1) = 8
      LIMREZ(2) = 24
      LIMREZ(3) = 40
      LIMREZ(4) = 50
C ----- ESTAGE common initialization for statistic.
      EVNTES = 0.
      DO 2 I = 1,3
        ECALES(I) = 0.
        ECLTES(I) = 0.
      DO 2 J = 1,3
      DO 2 K = 1,3
        CLURES(I,J,K) = 0.
    2 CONTINUE
C?  Compute the stack length
C  ******* To be extract from data base ********
C ----- Barrel radiation lengths
      COU(1) =.5
      PVC(1) = 18.4
      RADLEZ(1) = 2.12
      RARAEZ(1) = 1.33
C ----- End cap radiation lengths
      COU(2) =.635
      PVC(2) = 8.9
      RADLEZ(2) = 2.31
      RARAEZ(2) = 1.41
C ----- Overlap.
      RADAEZ = 15.
C  ******************************************
C ----- Stacks limits
      DO 6 I = 1,4
      IS = 2 * I - 1
      IF( I .EQ. 4 ) THEN
         IS = 6
         STAK = 3.9999
      ELSE
         STAK = I
      ENDIF
      CALL ESRPT( 'ALEPH' , 114., 16., STAK , ST )
      STACEZ( IS , 1) = SQRT( ST(1)**2 + ST(2)**2 )
      IF (I.EQ.1) STA1 = STACEZ( IS , 1 )
      STACEZ( IS , 1) = STACEZ( IS , 1) - STA1
      CALL ESRPT( 'ALEPH' , 40., 8., STAK , ST )
      STACEZ( IS , 2) = ST(3)
      IF (I.EQ.1) STA2 = STACEZ( IS , 2 )
      STACEZ( IS , 2) = STACEZ( IS , 2) - STA2
    6 CONTINUE
C ----- Stacks limits in radiation lengths
      DO 7 J = 1,2
      ST2 = STACEZ(5,J) - STACEZ(3,J) - COU(J)
      ST3 = STACEZ(6,J) - STACEZ(5,J) - COU(J)
      STACEZ(2,J) = ( STACEZ(3,J) - COU(J)) / RADLEZ(J)
      STACEZ(3,J) = STACEZ(2,J) + COU(J) / PVC(J)
      STACEZ(4,J) = STACEZ(3,J) + ST2 / RADLEZ(J)
      STACEZ(5,J) = STACEZ(4,J) + COU(J) / PVC(J)
      STACEZ(6,J) = STACEZ(5,J) + ST3 / RARAEZ(J)
    7 CONTINUE
C
C ----- Normales aux modules
C ----- Origin of the  modules.
      PHIBA = - PIBY12 / 8.
C ----- Cosinus directeurs des modules du baril.
      DO 1 I = 1,12
         UVWMEZ ( 1 , I) = COS ( PHIBA )
         UVWMEZ ( 2 , I) = SIN ( PHIBA )
         UVWMEZ ( 3 , I) =.0
         PHIBA = PHIBA + PIBY6
    1 CONTINUE
      END
#endif
