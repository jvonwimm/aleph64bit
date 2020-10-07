      SUBROUTINE MUPRHT
C
C-----------------------------------------------------------------
C
C T.Wang
C
C! print out 'MUHT'
C
#ifndef DOC
      SAVE
C
#include "bcs.h"
#include "bmacro.h"
C --------------------------------------------------------------
C
      LOUT = IW(6)
C
      KMUHT = NLINK ('MUHT',0)
      IF( KMUHT .NE. 0 ) THEN
         WRITE(LOUT,101)(IW(KMUHT+I),I=0,LMHROW)
         NSGNL = LROWS (KMUHT)
         IF( NSGNL .EQ. 0 )GOTO 900
         LWRDS = LCOLS (KMUHT)
         WRITE(LOUT,102)
         DO 10 J=1,NSGNL
         KPNT = KROW (KMUHT,J)
         WRITE(LOUT,103)(IW(KPNT+I),I=1,LWRDS)
   10    CONTINUE
  101    FORMAT(2X,'+++MUPRHT+++  ******** Bank MUHT data : ********'//
     *       2X,'# of words        -- ',I8/
     *       2X,'# of words/signal -- ',I8/
     *       2X,'# of signals      -- ',I8//)
  102    FORMAT(2X,'  Track #   electronics     strip plane  strip #'/
     *       2X,'            module  #           #               '/)
  103    FORMAT(3X,I5,8X,I5,8X,I5,8X,I5)
      ENDIF
C
  900 RETURN
      END
#endif
