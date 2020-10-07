      SUBROUTINE ECFMIP
C--------------------------------------------------------------
C!- Routine to tag cluster created by minimum ionising particle
C!  Compares the energy in each stack with that expected for a
C!  MIP and calculates a Chisqd.
C!  Takes into account the angle and energy dependence.
C!  Works for all particles of momentum > 0.5 GeV.
C!      IFLAMP = 1 FOR A MINION
C!
C!    Author : R. Edgecock   870604
C!    MODIFIED :D PALLIN 881222
C-------------------------------------------------------------
#ifndef DOC
#include "eminic.h"
C
      REAL ENDPA(3,2),CCLUA(3),ENCOR(3),ENCOM(3)
      EXTERNAL EFNDSC
      INTEGER EFNDSC
C? energy dependence parameters:
      DATA ENDPA/ 3.92,3.79,4.13,1.59,0.412,0.316/
C? phi tilt and width of the barrel modules
      DATA TILT,PHIMD/-0.0327,0.5236/
C
      IFLAMP=0
C
C? check the momentum of the charged track
C
      IF (PTPCCL(1) .LE. 0.5) GOTO 999
C
C? correct energy deposition for theta and phi:
C? as there is a difference between barrel and end-cap modules
C? first find which region we are in
C
      CCLUA(1) = BARYCL(1)*SIN(BARYCL(2))*COS(BARYCL(3))
      CCLUA(2) = BARYCL(1)*SIN(BARYCL(2))*SIN(BARYCL(3))
      CCLUA(3) = BARYCL(1)*COS(BARYCL(2))
      KSC = EFNDSC(CCLUA)
      IF (KSC .EQ. 0) GOTO 999
C
      IF (KSC .EQ. 2) THEN
C? we're in the barrel
         PHIAN = BARYCL(3)
         IF (BARYCL(3) .LT. 0.) PHIAN = 6.28319 + BARYCL(3)
         PHDIS = PHIAN - TILT
         PHOFF = AMOD(PHDIS,PHIMD)
         PHCOR = ABS(PHOFF - PHIMD*0.5)
         THCOR = ABS(1.57080 - BARYCL(2))
      ELSE
C? we're in the end-cap
         PHCOR = 0.
         THCOR = BARYCL(2)
      ENDIF
C
      DO 10 I=1,3
         ENCOR(I) = ABS(ENLVCL(I)*COS(THCOR)*COS(PHCOR))
   10 CONTINUE
C
C? correct the standard MIP energies for the (small) energy dependence
C
      IF (PTPCCL(1).GE.45.) THEN
         DO 15 I=1,3
            ENCOM(I) = ENFMIP(I)
   15    CONTINUE
      ELSE
         DO 20 I=1,3
            ENCOM(I) = ENFMIP(I)-EXP(-(ENDPA(I,1)+ENDPA(I,2)*PTPCCL(1)))
   20    CONTINUE
      ENDIF
C
C? calculate the chisqd
C
      CHI2 = ( ENCOR(1) - ENCOM(1) )**2 / ER2ST1 +
     1       ( ENCOR(2) - ENCOM(2) )**2 / ER2ST2 +
     2       ( ENCOR(3) - ENCOM(3) )**2 / ER2ST3
      IF (CHI2 .LT. ECHIMN) IFLAMP = 1
C
  999 CONTINUE
      RETURN
      END
#endif
