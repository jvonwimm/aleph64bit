      INTEGER FUNCTION JUNIAL(INOUT,NUMBER,NDUMMY)
C-----------------------------------------------------------------------
C      D.SCHLATTER
C!   return  logical units for input or output files
CKEY ALEF IO UNIT
C
C     INPUT:     INOUT   /C       = 'INput'  or
C                                 = 'OUtput'
C                NUMBER  /I       = index  of file [1,6]
C                             for INPUT:
C                                   MOD(index,3) = 1 for data
C                                   MOD(index,3) = 2 for 1. select file
C                                   MOD(index,3) = 3 for 2. select file
C                             for OUTPUT:
C                                   MOD(index,2) = 1 for data
C                                   MOD(index,2) = 2 for select file
C
C     OUTPUT     JUNIAL  /I       = logical unit
C
C     ENTRY:     NUNIAL           redefine logical units
C-----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      CHARACTER*(*) INOUT
      INTEGER INUNIT(9),UTUNIT(10)
C-----------------------------------------------------------------------
      DATA INUNIT/20,30,40,21,31,41,22,32,42/
C         20,21,,,  at  MOD(ind,3)=1  for input data
C         30,31,,,  at  MOD(ind,3)=2  for 1. select file
C         40,41,,,  at  MOD(ind,3)=3  for 2. select file
      DATA UTUNIT/50,60,51,61,52,62,53,63,54,64/
C         50,51,,,  at  MOD(ind,2)=1  for output data
C         60,61,,,  at  MOD(ind,2)=2  for 1. select file
C-----------------------------------------------------------------------
C
      JUNIAL = 0
C           up to now only 2 input and 5 output streams supported
      MM=2*3
      IF(INOUT(1:2).EQ.'OU') MM=5*2
      IF(NUMBER.LT.1.OR.NUMBER.GT.MM) THEN
        WRITE(IW(6),'(A)') ' _JUNIAL_  too many data streams requested'
        RETURN
      ENDIF

      IF(INOUT(1:2).EQ.'OU') THEN
        JUNIAL=UTUNIT(NUMBER)
      ELSE
        JUNIAL=INUNIT(NUMBER)
      ENDIF
      RETURN
C
      ENTRY NUNIAL(INOUT,NUMBER,NEWNUM)
      NUNIAL=0
C!            set logical numbers
      MM=2*3
      IF(INOUT(1:2).EQ.'OU') MM=5*2
      IF(NUMBER.LT.1.OR.NUMBER.GT.MM) THEN
        WRITE(IW(6),'(A)') ' _NUNIAL_  too many data streams requested'
        RETURN
      ENDIF

      IF(INOUT(1:2).EQ.'OU') THEN
        UTUNIT(NUMBER)=NEWNUM
      ELSE
        INUNIT(NUMBER)=NEWNUM
      ENDIF

      END
