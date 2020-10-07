      SUBROUTINE AUBLIS (LIST)
C -----------------------------------------------------------
C - F.Ranjard - 860704
C!  Print the list of banks which are on the 'LIST' list.
C - Calls   NLISTB,NAMIND                        from BOS77.lib
C  ------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      EXTERNAL NLISTB, NAMIND
      CHARACTER LIST*(*)
      PARAMETER (LBANK=20)
      CHARACTER*4 NLISTB,NAME(LBANK)
C -------------------------------------------------------------
C
      LOUT = IW(6)
      WRITE (LOUT,810) LIST
C
      K=0
      I=0
 1    I=I+1
      K=K+1
      NAME(K) = NLISTB (IW,I,LIST)
      IF (NAME(K) .EQ. ' ') GOTO 2
      IF (IW(NAMIND(NAME(K))) .EQ. 0) K=K-1
      IF (K .EQ. LBANK) THEN
          WRITE(LOUT,811) (NAME(J),J=1,K)
          K=0
      ENDIF
      GOTO 1
 2    K = K-1
 3    WRITE (LOUT,811) (NAME(J),J=1,K)
C
      RETURN
 810  FORMAT (/1X,'+++ AUBLIS +++ ',A4,' list contains the ',
     &            'following banks: ')
 811  FORMAT (20(1X,A4))
      END
