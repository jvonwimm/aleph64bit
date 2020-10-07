      INTEGER FUNCTION LENLIS(LIST)
C-----------------------------------------------------------------------
C!    Determine the total data content of banks in list
C     INPUT LIST character string e.g. 'E'
C     OUTPUT LENLIS total length in words
C          J.Knobloch 20-10-89
C-----------------------------------------------------------------------
#ifndef DOC
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      CHARACTER*4 NAME,NLIST
      CHARACTER*1 LIST
      N=0
      LTOT=0
    1 N=N+1
      NAME=NLIST(IW,N,LIST)
      IF (NAME.NE.' ') THEN
         IND=NAMIND(NAME)+1
    2    IND=IW(IND-1)
         IF (IND.NE.0) THEN
            LTOT=LTOT+IW(IND)+4
            GOTO 2
         ENDIF
         GOTO 1
      ENDIF
      LENLIS=LTOT
      RETURN
      END
#endif
