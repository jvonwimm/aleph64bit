      SUBROUTINE KLUZET(ZB)
C------------------------------------------------------------------     
C! Transfer the content of fragmention information  to ZB array         
C    B.Bloch-Devaux January   1991                                      
C     comdecks referenced : LUN7COM                                     
C---------------------------------------------------------------------- 
#ifndef DOC
      SAVE
#include "lun7com.h"
      DIMENSION ZB(LJNPAR)
      IF (MSTU(90).GT.0) THEN
         DO 10 I = 1,MSTU(90)
         J = MSTU(91+I-1)
 10      ZB(J) = PARU(91+I-1)
      ENDIF
      RETURN
      END 
#endif
