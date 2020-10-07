      SUBROUTINE GAGRID(ITPIC,JFPIC,KSPIC,LST33)
C.----------------------------------------------------------------------
CKEY GAMPACK GRID / INTERNAL
C   J.C.Brient      Creation  1/10/91
C! Create the matrix around the peak for stack KSPIC
C   Input :
C           ITPIC   I row   of the energy peak   INTEGER
C           JFPIC   J collumn of the energy peak INTEGER
C           KSPIC   stack number                 INTEGER
C   Output:
C           LST33   matrix around the peak       REAL
C   Calls: None
C   Called by GAMPEX
C.----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "ecoxa.h"
#include "alcons.h"
C
C--for INPUT DIMENSION
      DIMENSION LST33(3,6,3)

      DIMENSION ITT(3) , JFF(3) , POINT(3)
      DIMENSION LBOREG(6)
      DIMENSION NPREG(4)
      LOGICAL VOISTO
      DATA LBOREG /8,9,24,25,40,41/
      LOGICAL FIRST
      DATA FIRST/.TRUE./
#include "bmacro.h"

      IF(FIRST) THEN
        ITLIS=ITOV2+2
        ITLIP=ITOV3-2
      ENDIF

      IBORFL=0
      DO 1 I = 1 , 3
        DO 1 J = 1 , 6
          DO 1 K = 1 , 3
            LST33(I,J,K) = 0
    1 CONTINUE

      ITT(1) = ITPIC - 1
      ITT(2) = ITPIC
      ITT(3) = ITPIC + 1
      CALL ECRWRG(ITPIC,IREG,MXCOL)
      IF (ITT(1).GE.1) THEN
         CALL ECRWRG(ITT(1),IREG1,MX1)
      ELSE
         IREG1 = IREG
         MX1 = MXCOL
      ENDIF
      IF (ITT(3).LE.228) THEN
         CALL ECRWRG(ITT(3),IREG3,MX3)
      ELSE
         IREG3 = IREG
         MX3 = MXCOL
      ENDIF
C
C first barrel
C ------------
      IF(ITT(1) .GE. ITLIS .AND. ITT(2) .LE. ITLIP) THEN
        JFF(3) = JFPIC + 1
        JFF(2) = JFPIC
        IF(JFF(3) .EQ. MXCOL+1) JFF(3) = 1
        JFF(1) = JFPIC - 1
        IF(JFF(1) .EQ. 0) JFF(1) = MXCOL
        DO I = 1 , 3
          DO J = 1 , 3
            LST33(I,J,1) = ITT(I)
            LST33(I,J,2) = JFF(J)
            LST33(I,J,3) = KSPIC
          ENDDO
        ENDDO
        GO TO 99
      ENDIF
C
C END CAP
C -------
C
C here this is not a border region (ireg1 = ireg3)
C -------------------------------------------------
      IF(IREG1 .EQ. IREG3) THEN
        JFF(3) = JFPIC + 1
        JFF(2) = JFPIC
        KPP = MXCOL + 1
        IF(JFF(3) .EQ. KPP) JFF(3) = 1
        JFF(1) = JFPIC - 1
        IF(JFF(1) .EQ. 0) JFF(1) = MXCOL
        DO I = 1 , 3
          DO J = 1 , 3
            LST33(I,J,1) = ITT(I)
            LST33(I,J,2) = JFF(J)
            LST33(I,J,3) = KSPIC
          ENDDO
        ENDDO
        GO TO 99
      ENDIF
C
C limit beetwen 2 regions of end cap
C ----------------------------------
      IBORFL=1
      IF(IREG .EQ. IREG1) THEN
        JFF(2) = JFPIC
        JFF(1) = JFPIC - 1
        IF(JFF(1) .EQ. 0) JFF(1) = MXCOL
        JFF(3) = JFPIC + 1
        IF(JFF(3) .EQ. MXCOL+1) JFF(3) = 1
        DO I = 1 , 2
          DO J = 1 , 3
            LST33(I,J,1) = ITT(I)
            LST33(I,J,2) = JFF(J)
            LST33(I,J,3) = KSPIC
          ENDDO
        ENDDO
        FIP  = FLOAT(JFF(3)) * TWOPI /FLOAT(MXCOL)
        FIM  = FLOAT(JFF(1)) * TWOPI /FLOAT(MXCOL)
        NREM  = NINT(FLOAT(MX3)*ABS(FIM-FIP)/TWOPI)
        JREM  = NINT(FLOAT(MX3)*FIM/TWOPI)-1
        NREM  = 3*MX3/MXCOL

        K= JREM-1
        DO J = 1 , NREM
          K = K + 1
          IF(K .GT. MX3) K=K-MX3
          LST33(3,J,1) = ITT(3)
          LST33(3,J,2) = K
          LST33(3,J,3) = KSPIC
        ENDDO
      ENDIF

      IF(IREG  .EQ. IREG3) THEN
        JFF(2) = JFPIC
        JFF(1) = JFPIC -1
        IF(JFF(1) .EQ. 0) JFF(1) = MXCOL
        JFF(3) = JFPIC + 1
        IF(JFF(3) .EQ. MXCOL+1) JFF(3) = 1
        DO I = 2 , 3
          DO J = 1 , 3
            LST33(I,J,1) = ITT(I)
            LST33(I,J,2) = JFF(J)
            LST33(I,J,3) = KSPIC
          ENDDO
        ENDDO
        FIP  = FLOAT(JFF(3)) * TWOPI /FLOAT(MXCOL)
        FIM  = FLOAT(JFF(1)) * TWOPI /FLOAT(MXCOL)
        NREM  = NINT(FLOAT(MX1)*ABS(FIM-FIP)/TWOPI)
        JREM  = NINT(FLOAT(MX1)*FIM/TWOPI)-1
        NREM  = 3*MX1/MXCOL

        K= JREM-1
        DO J = 1 , NREM
          K = K + 1
          IF(K .GT. MX1) K=K-MX1
          LST33(1,J,1) = ITT(1)
          LST33(1,J,2) = K
          LST33(1,J,3) = KSPIC
        ENDDO
      ENDIF

   99 CONTINUE

      RETURN
      END
#endif
