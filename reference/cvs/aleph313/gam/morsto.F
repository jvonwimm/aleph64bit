      INTEGER FUNCTION MORSTO(ITPIC,JFPIC,LSTK,NSTK,LST33)
C.----------------------------------------------------------------------
CKEY GAMPACK DEAD STOREY / INTERNAL
C   J.C.Brient      Creation  1/10/91
C!  Test if there is deads storeys in the central matrix around the peak
C   Input :
C           ITPIC   I row   of the energy peak   INTEGER
C           JFPIC   J collumn of the energy peak INTEGER
C           LSTK    List of storeys (all stack)  INTEGER
C           LST33   central matrix of the photon INTEGER
C           NSTK    stack number                 INTEGER
C   Output:
C           Function = 1 if there is dead(s) storey(s) in central matrix
C   Calls: None
C   Called by GAMPEX
C.----------------------------------------------------------------------
#ifndef DOC
#include "ecoxa.h"
#include "deadst.h"
C
C--for INPUT DIMENSION
      DIMENSION LST33(3,6,3) , LSTK(3,*)
      DIMENSION IDEST(3) , ICLST(3)
      LOGICAL VOISTO
C ---------------------------------------------------------------
      MORSTO = 0
C
      IF(ITPIC .GE. ITOV1 .AND. ITPIC .LE. ITOV2) GO TO 200
      IF(ITPIC .GE. ITOV4 .AND. ITPIC .LE. ITOV3) GO TO 200
C
C not overlap treatement here
C ---------------------------
      DO 100 ID = 1 , NSTMOR
        KSD = IMORT(3,ID)
        ITD = IMORT(1,ID)
        JFD = IMORT(2,ID)
        DO I  = 1 , 3
          DO J = 1 , 5
            IT = LST33(I,J,1)
            JF = LST33(I,J,2)
            KS = LST33(I,J,3)
            IF(KS .EQ. KSD .AND. IT .EQ. ITD .AND.
     &          JF .EQ. JFD) THEN
              MORSTO = 1
              RETURN
            ENDIF
          ENDDO
        ENDDO
  100 CONTINUE
      RETURN
C
  200 CONTINUE
C
C special overlap treatement
C --------------------------
C
      DO 300 ID = 1 , NSTMOR
        IDEST(3) = IMORT(3,ID)
        IDEST(1) = IMORT(1,ID)
        IDEST(2) = IMORT(2,ID)

        IF(IMORT(1,1) .EQ. 0 ) GO TO 300
        DO IS = 1 , NSTK
          ICLST(1) = LSTK(1,IS)
          ICLST(2) = LSTK(2,IS)
          ICLST(3) = LSTK(3,IS)
          IF(VOISTO(IDEST,ICLST) ) THEN
            MORSTO = 1
            RETURN
          ENDIF
        ENDDO
  300 CONTINUE
C
      RETURN
      END
#endif
