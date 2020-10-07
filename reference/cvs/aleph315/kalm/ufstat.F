      INTEGER FUNCTION UFSTAT(R,X_PRED,RES_PRED,GAIN,X_FILT)
C! Calculate filtered state vector
#ifndef DOC
      DOUBLE PRECISION R,X_PRED(5),RES_PRED(2),GAIN(5,2),X_FILT(5)
      DOUBLE PRECISION ALP
#include "uftcom.h"

      DO  I = 1, 5
        X_FILT(I) = X_PRED(I) + GAIN(I,1)*RES_PRED(1)
     +                          + GAIN(I,2)*RES_PRED(2)
      ENDDO

      ALP = X_FILT(1)/R
      CALL UBA2PI(ALP)
      X_FILT(1) = ALP*R

      ALP = X_FILT(3)
      CALL UBA2PI(ALP)
      X_FILT(3) = ALP

      ALP = X_FILT(4)
      CALL UBAPI2(ALP)

      IF (ABS(ALP).GT. ONE_PI) THEN
        UFSTAT = 24
        RETURN
      ENDIF
      IF (ALP.LT.-HALF_PI) ALP = ALP+ONE_PI
      IF (ALP.GT.HALF_PI) ALP = ALP-ONE_PI
      X_FILT(4) = ALP
      UFSTAT = 0

      RETURN
      END
#endif
