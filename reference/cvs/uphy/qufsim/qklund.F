      INTEGER FUNCTION QKLUND(KALPHA)
C----------------------------------------------------------------
C G. Ganis. Give the LUND Code correspondind to an stable particle
C           Input : KALPHA, the ALEPH internal code
C           Output: QKLUND, the LUND code
C
C Modifications :
C   o Patrick Janot -- 21 Oct 1993. Interface with JETSET 7.3
C----------------------------------------------------------------
#include "coddat.h"
C
      qklund= 0
      DO l1=1,istab
        IF ( kalpha .EQ. kcoal(l1) ) THEN
          qklund= kcolu1(l1)
          GOTO 10
        ENDIF
      ENDDO
10    CONTINUE
C
      IF ( qklund .EQ. 0)
     .WRITE(6,*) ' *** QKLUND *** Unknown stable particle',kalpha
C
      RETURN
      END
