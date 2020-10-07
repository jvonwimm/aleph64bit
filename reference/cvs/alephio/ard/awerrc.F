      SUBROUTINE AWERRC (LUN, CSUBR, CNAME, IER)
C
C----------------------------------------------------------------------
C! print messages for return codes from card reading routines
C  only input parameters.
C  LUN   logical unit for print output
C  CSUBR name of the calling subroutine
C  CNAME data set name
C  IER   error code from ACDARG, AFILIN, etc.
C                                                   H.Albrecht 20.11.88
C----------------------------------------------------------------------
#ifndef DOC
      CHARACTER * (*) CSUBR,CNAME
      CHARACTER * 50 CT(7)
      CHARACTER CS*16
      DATA CT /'Warning : unknown file type',
     2         'GIME was not successful',
     3         'Contradictory parameters on input card',
     4         'Names or parameters on input card are too long',
     5         'Wrong ALEPH type',
     6         'Input file does not exist',
     7         'This file is expected to be on DISK'/
C
      IF (IER .LE. 0)  GO TO 100
      WRITE (LUN,1001)  CNAME
      L = MIN (LEN(CSUBR), 6)
      CS = ' +++ ' // CSUBR(1:L) // ' +++ '
      IF (IER .LE. 7)  THEN
        WRITE (LUN,1002) CS,CT(IER)
      ELSE
        WRITE (LUN,1003) CS
      ENDIF
 1001 FORMAT (' +++ Data set +++ ',A)
 1002 FORMAT (2A)
 1003 FORMAT (' +AWERRC+ called from ',A,' with unknown error code')
  100 RETURN
      END
#endif
