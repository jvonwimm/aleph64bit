      INTEGER FUNCTION UFSTAT(R,X_PRED,RES_PRED,GAIN,X_FILT)
C! Calculate filtered state vector
C! Modified: 2000-12-04 M.Cattaneo - protect against crazy values of ALP
#ifndef DOC
      DOUBLE PRECISION R,X_PRED(5),RES_PRED(2),GAIN(5,2),X_FILT(5)
      DOUBLE PRECISION ALP
#include "uftcom.h"

      DO  I = 1, 5
        X_FILT(I) = X_PRED(I) + GAIN(I,1)*RES_PRED(1)
     +                          + GAIN(I,2)*RES_PRED(2)
      ENDDO

      ALP = X_FILT(1)/R
      IF( ABS(ALP) .GT. 1.0E10 ) THEN
        UFSTAT = 25
        RETURN
      ENDIF
      CALL UBA2PI(ALP)
      X_FILT(1) = ALP*R

      ALP = X_FILT(3)
      IF( ABS(ALP) .GT. 1.0E10 ) THEN
        UFSTAT = 25
        RETURN
      ENDIF
      CALL UBA2PI(ALP)
      X_FILT(3) = ALP

      ALP = X_FILT(4)
      IF( ABS(ALP) .GT. 1.0E10 ) THEN
        UFSTAT = 25
        RETURN
      ENDIF
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
