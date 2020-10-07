      SUBROUTINE QGTHRU
C-----------------------------------------------------------------------
CKEY SHAPE /INTERNAL
C!   topological event analysis: thrust
C                                                   H. Albrecht. feb 82
C-----------------------------------------------------------------------
#ifndef DOC
#include "qctbuf.h"
C-----------------------------------------------------------------------
      IF (KTBI .LE. 2)  GO TO 60
C
      VMAX = 0.
      DO 30 I=1,KTBI-1
        DO 20 J=I+1,KTBI
          VCX = QTBIY(I) * QTBIZ(J) - QTBIY(J) * QTBIZ(I)
          VCY = QTBIZ(I) * QTBIX(J) - QTBIZ(J) * QTBIX(I)
          VCZ = QTBIX(I) * QTBIY(J) - QTBIX(J) * QTBIY(I)
          VLX = 0.
          VLY = 0.
          VLZ = 0.
C
          DO 10 L=1,KTBI
            IF (L .EQ. I .OR. L .EQ. J)  GO TO 10
            IF (QTBIX(L) * VCX + QTBIY(L) * VCY +
     &          QTBIZ(L) * VCZ .GE. 0.)  THEN
              VLX = VLX + QTBIX(L)
              VLY = VLY + QTBIY(L)
              VLZ = VLZ + QTBIZ(L)
            ELSE
              VLX = VLX - QTBIX(L)
              VLY = VLY - QTBIY(L)
              VLZ = VLZ - QTBIZ(L)
            ENDIF
   10     CONTINUE
C
C --- make all four sign-combinations for I,J
C
          VNX = VLX + QTBIX(J) + QTBIX(I)
          VNY = VLY + QTBIY(J) + QTBIY(I)
          VNZ = VLZ + QTBIZ(J) + QTBIZ(I)
          VNEW = VNX*VNX + VNY*VNY + VNZ*VNZ
          IF (VNEW .GT. VMAX)  THEN
            VMAX = VNEW
            VMX = VNX
            VMY = VNY
            VMZ = VNZ
          ENDIF
C
          VNX = VLX + QTBIX(J) - QTBIX(I)
          VNY = VLY + QTBIY(J) - QTBIY(I)
          VNZ = VLZ + QTBIZ(J) - QTBIZ(I)
          VNEW = VNX*VNX + VNY*VNY + VNZ*VNZ
          IF (VNEW .GT. VMAX)  THEN
            VMAX = VNEW
            VMX = VNX
            VMY = VNY
            VMZ = VNZ
          ENDIF
C
          VNX = VLX - QTBIX(J) + QTBIX(I)
          VNY = VLY - QTBIY(J) + QTBIY(I)
          VNZ = VLZ - QTBIZ(J) + QTBIZ(I)
          VNEW = VNX*VNX + VNY*VNY + VNZ*VNZ
          IF (VNEW .GT. VMAX)  THEN
            VMAX = VNEW
            VMX = VNX
            VMY = VNY
            VMZ = VNZ
          ENDIF
C
          VNX = VLX - QTBIX(J) - QTBIX(I)
          VNY = VLY - QTBIY(J) - QTBIY(I)
          VNZ = VLZ - QTBIZ(J) - QTBIZ(I)
          VNEW = VNX*VNX + VNY*VNY + VNZ*VNZ
          IF (VNEW .GT. VMAX)  THEN
            VMAX = VNEW
            VMX = VNX
            VMY = VNY
            VMZ = VNZ
          ENDIF
   20   CONTINUE
   30 CONTINUE
C
      IF(VMZ. LT. 0.) THEN
        VMX = -VMX
        VMY = -VMY
        VMZ = -VMZ
      ENDIF
C -- sum momenta of all particles and iterate
C
      DO 50 ITER=1,4
        QTBOX(1) = 0.
        QTBOY(1) = 0.
        QTBOZ(1) = 0.
        DO 40  I=1,KTBI
          IF (VMX * QTBIX(I) + VMY * QTBIY(I) +
     +        VMZ * QTBIZ(I) .GE. 0.)  THEN
            QTBOX(1) = QTBOX(1) + QTBIX(I)
            QTBOY(1) = QTBOY(1) + QTBIY(I)
            QTBOZ(1) = QTBOZ(1) + QTBIZ(I)
          ELSE
            QTBOX(1) = QTBOX(1) - QTBIX(I)
            QTBOY(1) = QTBOY(1) - QTBIY(I)
            QTBOZ(1) = QTBOZ(1) - QTBIZ(I)
          ENDIF
   40   CONTINUE
        VNEW = QTBOX(1)**2 + QTBOY(1)**2 + QTBOZ(1)**2
        IF (VNEW .EQ. VMAX)  GO TO 70
        VMAX = VNEW
        VMX = QTBOX(1)
        VMY = QTBOY(1)
        VMZ = QTBOZ(1)
   50 CONTINUE
C
C --- 2 tracks or less
C
   60 IF (KTBI .EQ. 2)  THEN
        IF (QTBIX(1) * QTBIX(2) + QTBIY(1) * QTBIY(2) +
     &      QTBIZ(1) * QTBIZ(2) .GE. 0)  THEN
          QTBOX(1) = QTBIX(1) + QTBIX(2)
          QTBOY(1) = QTBIY(1) + QTBIY(2)
          QTBOZ(1) = QTBIZ(1) + QTBIZ(2)
        ELSE
          QTBOX(1) = QTBIX(1) - QTBIX(2)
          QTBOY(1) = QTBIY(1) - QTBIY(2)
          QTBOZ(1) = QTBIZ(1) - QTBIZ(2)
        ENDIF
      ELSE IF (KTBI .EQ. 1)  THEN
        QTBOX(1) = QTBIX(1)
        QTBOY(1) = QTBIY(1)
        QTBOZ(1) = QTBIZ(1)
      ELSE
        QTBOX(1) = 0.
        QTBOY(1) = 0.
        QTBOZ(1) = 0.
        QTBOE(1) = 0.
        QTBOR(1) = 0.
        KTBO = 0
        GO TO 90
      ENDIF
      VNEW = QTBOX(1)**2 + QTBOY(1)**2 + QTBOZ(1)**2
C
C --- normalize thrust -division by total momentum-
C
   70 VSUM = 0.
      DO 80 I=1,KTBI
        KTBOF(I) = 1
   80   VSUM = VSUM + SQRT (QTBIX(I)**2 + QTBIY(I)**2 + QTBIZ(I)**2)
      QTBOR(1) = SQRT (VNEW) / VSUM
      QTBOE(1) = SQRT (QTBOX(1)**2 + QTBOY(1)**2 + QTBOZ(1)**2)
      KTBO = 1
C
   90 END
#endif
