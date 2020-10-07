      INTEGER FUNCTION ISPARK(MODUL,NEWPLA)
C.----------------------------------------------------------------------
CKEY GAMPACK SPARK/ INTERNAL
C   J.C.Brient      Creation  1/10/91
C! search for spark in ECAL
C  Looks the energy wires in the 45 planes to flag spark in module
C  number MODUL (input)
C   Input :
C           MODUL   ECAL module number           INTEGER
C           NEWPLA  # OF WIRES PLANES IN ALEPH   INTEGER
C   Output:
C           Function = 1 if The module have a "spark" wires distribution
C   Calls: VASUM,VMAXA,LVMAXA
C   Called by GAMPEK
C.----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
      PARAMETER (IPLNEC=45)
C array used in routine
      DIMENSION  EEMM(IPLNEC)
      DIMENSION  EODEV(2)
#include "bmacro.h"
      ISPARK = 0
      NPEWI  = 0
      JPEWI  = IW(NAMIND('PEWI'))
      IF ( JPEWI.EQ.0) JPEWI = IW(NAMIND('PWEI'))
      IF(JPEWI .GT. 0 ) NPEWI = LROWS(JPEWI)
      IF(NPEWI .LE. 0 ) THEN
        ISPARK = 1
        RETURN
      ENDIF
C
C read wires energy for module number MODUL
C -----------------------------------------
      DO 1   I = 1 , NPEWI
        IMODO = ITABL(JPEWI,I,1)
        IF(IMODO .NE. MODUL ) GO TO 1
        DO K = 2 , NEWPLA + 1
          J=K-1
          EEMM(J) = 0.
          IEXX = ITABL(JPEWI,I,K)
          EPL = 0.
          IF(IEXX .GT. 0 ) EPL = FLOAT(IEXX)/1000.
          IF(EPL  .GT. 0.5) EEMM(J) = EPL/1000.
        ENDDO
    1 CONTINUE
      EMOTO = VASUM(EEMM,NEWPLA)
      IF( EMOTO .LT. 0.05) THEN
        ISPARK = -1
        RETURN
      ENDIF
      EMMX  = VMAXA(EEMM,NEWPLA)
      LLMM  = LVMAXA(EEMM,NEWPLA)
      EODEV(1) = 0.
      EODEV(2) = 0.
      EMOYN = 0.
      NMOYN = 0.
      DO K = 1 , NEWPLA
        IF(EEMM(K) .GT. 0.001) THEN
          KIMP = MOD(K,2) + 1
          NMOYN = NMOYN + 1
          IF(K .NE. LLMM) EMOYN = EMOYN + EEMM(K)
          EODEV(KIMP) = EODEV(KIMP) + EEMM(K)
        ENDIF
      ENDDO
C
C less than 3 planes, it is a spark
C ---------------------------------
      IF(NMOYN .LT. 3 ) THEN
        ISPARK = 1
        RETURN
      ENDIF
      RC = 999.
      IF(EODEV(2) .GT. 0.00001) RC = EODEV(1)/EODEV(2)
      IF(RC .GT. 2.0) THEN
        ISPARK = 1
        RETURN
      ENDIF

      RETURN
      END
#endif
