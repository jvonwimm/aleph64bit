      INTEGER FUNCTION EFNDMD(SC,X)
C.----------------------------------------------------------------------
CKEY ECALDES MODULE / USER
C     H.Videau      Creation 15/12/85   modification 30/01/88
C! Finds module number
C  Looks for the module of the subcomponent SC containing the point X.
C  We deliberately consider theoretical module slots such that a point i
C  a subcomponent is always in one module and one only.
C  We do not make a precise test of the module limits.
C  This is done when looking for a plane, column, row, by calling
C  the routine ETZSMD or checking with the planes limiting the volume
C   Input :
C            SC       subcomponent #            INTEGER
C            X        point coordinates
C   Output:
C            EFNDMD   Module # 1<= EFNDMD <= 12 INTEGER
C   Calls: EPLSCC.
C   Called by USER
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C     Input variables
      REAL X(ECALLS-1)
      INTEGER SC
C    Functions
      INTEGER EPLSCC
C
      EFNDMD=EPLSCC(ESCOMD(1,1,SC),ECALMD,EMODPC(1,1),X)
      END
#endif
