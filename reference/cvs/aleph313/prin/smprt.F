      SUBROUTINE SMPRT(V,N,TEXT)
C
C     SUBROUTINE SMPRT
C     ----------------
C!    PRINTS THE SYMMETRIC N-BY-N MATRIX V
C
C                   - - ----
C        CALL SMPRT(V,N,TEXT)
C
C        AUTHOR: VOLKER BLOBEL
C
#ifndef DOC
      SAVE
      REAL V(1)
      CHARACTER*(*) TEXT
      WRITE(6,101) N,N,TEXT
      IF(N.LE.0) GOTO 100
      II=0
      DO 10 I=1,N
      WRITE(6,102) I,(V(II+J),J=1,I)
   10 II=II+I
      WRITE(6,102)
  100 RETURN
  101 FORMAT('0',I4,' BY',I3,' SYMMETRIC MATRIX ',A/)
  102 FORMAT(1X,I4,5X,10G12.5/(10X,10G12.5/))
      END
#endif
