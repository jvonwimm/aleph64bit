      SUBROUTINE EDEFSC
C.----------------------------------------------------------------------
CKEY ECALDES SUBCOMPONENT INITIALISATION / INTERNAL
C     H.Videau      Creation 15/12/85   Modification 26/01/88
C! Defines sub components geometry:
C                 end cap 1   z>0  SC=1
C                 Barrel           SC=2
C                 end cap 2   z<0  SC=3
C  The numbering of the sub components follows theta
C  We create the displacement matrices corresponding to the position
C  of the subcomponents in the calorimeter frame: ETSCDM
C   Called by EXPNEC
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C   Locales
      INTEGER I,J,K
      REAL CS,SN
C
C     Les deux bouchons sont tournes de -PI/12
C     Le baril de 0.
C
      DO 1 I=1,ECALSY
        DO 2 J=1,ECALLS
          DO 2 K=1,ECALLS
            IF(K.EQ.J)THEN
              ETSCDM(K,J,I) = 1.
            ELSE
              ETSCDM(K,J,I) = 0.
            END IF
    2   CONTINUE
        CS=COS(ETSCAF(I))
        SN=SIN(ETSCAF(I))
        ETSCDM(1,1,I)= CS
        ETSCDM(1,2,I)= SN
        ETSCDM(2,1,I)=-SN
        ETSCDM(2,2,I)= CS
 1    CONTINUE
      END
#endif
