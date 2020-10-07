      INTEGER FUNCTION MINVSN(DUMMY)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Mini-DST version number (of current library).
C
C     This can be overrode by use of MVSN card which allows a new
C     version to be started without changing MINI code.
C     Useful if other code has major modifications.
C-----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
      LOGICAL FIRST
      SAVE MVER,FIRST
      DATA FIRST / .TRUE. /
      DATA MVER / 102 /
C
C++   Check for card input.
C
      IF (FIRST) THEN
         KMVSN = NLINK('MVSN',0)
         IF (KMVSN.GT.0) MVER = IW(KMVSN+1)
         FIRST = .FALSE.
      ENDIF
C
      MINVSN = MVER
C
      RETURN
      END
