      SUBROUTINE LKSCENTER(LLIN,N)
C ---------------------------------------------------------------
C - A.Bonissent
C! Center a character string
C Try to put as many spaces before and after nom blank characters
C ---------------------------------------------------------------
#ifndef DOC
      CHARACTER*(*)LLIN
      CHARACTER*132 LOCAL
C ---------------------------------------------------------------
      NC=LEN(LLIN)
      DO IC=1,NC
        LOCAL(IC:IC)=' '
      ENDDO
      NBL=(NC-N)/2
      WRITE(LOCAL(NBL+1:NBL+N),'(A)')LLIN(1:N)
   10 CONTINUE
      LLIN(1:NC)=LOCAL(1:NC)
      RETURN
      END
#endif
