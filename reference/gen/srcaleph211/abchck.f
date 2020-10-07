      SUBROUTINE ABCHCK (IRET)
C-----------------------------------------------------------------------
CKEY ALPHARD CHECK CARDS
C  AUTHOR :      H. Albrecht            Nov 89
C  modified by : F.Ranjard              900927
C! check  SEVT, SRUN, IRUN, NEVT, TIME, CLAS cards.
C  determine the limits of the search MAXRUN,MAXEVT,NNMIN,NNMAX
C
C  Called from ABRSEL
C  Calls : none.
C
C  Output:
C          IRET = 0      : syntax ok
C               = 11     : syntax error detected
C
C
C-----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      LOGICAL START, RUNS
      DATA  START /.TRUE./, IBIG /99999999/
C --------------------------------------------------------------------
C
C - 1st entry
C
      IF (START)  THEN
         START = .FALSE.
         NASEVT = NAMIND ('SEVT')
         NASRUN = NAMIND ('SRUN')
         NAIRUN = NAMIND ('IRUN')
         NANEVT = NAMIND ('NEVT')
         NACLAS = NAMIND ('CLAS')
         NATIME = NAMIND ('TIME')
      ENDIF
C
C - next entry
C
      IRET = 0
      MAXRUN = 0
      MAXEVT = 0
      NNMIN  = 0
      NNMAX  = IBIG
C       get largest run / event number and check syntax
C
C       SRUN and IRUN are mutually exclusive :
      IF (IW(NASRUN) .NE. 0 .AND. IW(NAIRUN) .NE. 0)  IRET = 11
C
C       loop over SRUN and IRUN :
C
      INS = NASRUN + 1
      DO 40 J=1,2
   10   INS = IW(INS-1)
        IF (INS .EQ. 0)  GO TO 40
        IF (IW(INS) .GT. 0)  GO TO 20
C       syntax error : SRUN without argumnts :
        IRET = 11
        GO TO 10
C       syntax error : SRUN -15 (1st number must be positive)
   20   IF (IW(INS+1) .LE. 0)  IRET = 11
        IF (J .EQ. 1)  MAXRUN = MAX0 (MAXRUN, IW(INS+1))
        DO 30 I=2,IW(INS)
C       syntax error : SRUN 0 (run number must be non-zero) :
          IF (IW(INS+I) .EQ. 0)  IRET = 11
          IF (IW(INS+I) .GT. 0)  GO TO 30
C       syntax error : SRUN -10 -12 (two consecutive neg. numbers)
C       syntax error : SRUN 12 -10  (not a range)
          IF (IW(INS+I-1) .LT. 0 .OR. IW(INS+I) + IW(INS+I-1) .GT. 0)
     +  IRET = 11
   30   IF (J .EQ. 1)  MAXRUN = MAX0 (MAXRUN, IABS(IW(INS+I)))
        GO TO 10
   40 INS = NAIRUN + 1
C
C       SEVT cards :
C
      INS = NASEVT + 1
  110 INS = IW(INS-1)
      IF (INS .EQ. 0)  GO TO 150
      IF (IW(INS) .GT. 1)  GO TO 120
C       syntax error : give one run number and at least one event number
      IRET = 11
      GO TO 110
C       syntax : run number and 1st event number must be .GE. 0
  120 CONTINUE
      IF (IW(INS+1) .LT. 0 .OR. IW(INS+2) .LT. 0)  IRET = 11
      IF (MAXRUN .GT. IW(INS+1))  GO TO 130
      MAXRUN = IW(INS+1)
      MAXEVT = MAX0 (MAXEVT, IABS(IW(INS+2)))
  130 DO 140 I=3,IW(INS)
        IF (IW(INS+I) .GT. 0)  GO TO 140
C       syntax error : SEVT 15 -10 -12 (two consecutive neg. numbers)
C       syntax error : SEVT 15 12 -10  (not a range)
        IF (IW(INS+I-1) .LT. 0 .OR. IW(INS+I) + IW(INS+I-1) .GT. 0)
     +  IRET = 11
  140 IF (MAXRUN .EQ. IW(INS+1))
     +  MAXEVT = MAX0 (MAXEVT, IABS(IW(INS+I)))
      GO TO 110
C
  150 IF (MAXRUN .EQ. 0)  MAXRUN = IBIG
      IF (MAXEVT .EQ. 0)  MAXEVT = IBIG
C
C       NEVT card
C
      INS = IW(NANEVT)
      IF (INS .NE. 0)  THEN
C         syntax error : NEVT 12 13 14 (more than 2 numbers)
C                        NEVT -15 (1st number must be non-negative)
C                        more than one NEVT card is not allowed.
        IF (IW(INS) .GT. 2 .OR. IW(INS+1) .LT. 0 .OR.
     +      IW(INS-1) .NE. 0)  IRET = 11
        NNMAX = IW(INS+1)
        IF (IW(INS) .EQ. 2)  THEN
C         syntax error : NEVT 12 -10 (not a range)
C                        NEVT 15 16 (2nd number must be neg. if given)
          IF (IW(INS+2) .GE. 0 .OR. IW(INS+1) + IW(INS+2) .GT. 0)
     +      IRET = 11
          NNMIN = IW(INS+1)
          NNMAX = -IW(INS+2)
        ENDIF
      ENDIF
C
C       CLAS card
C
      INS = IW(NACLAS)
      IF(INS .NE. 0)  THEN
C       syntax error : only one CLAS card allowed
        IF (IW(INS-1) .NE. 0)  IRET = 11
        DO 160 I=1,IW(INS)
C       syntax error : arguments must be in the range 1 ... 30
  160   IF (IW(INS+I) .LE. 0 .OR. IW(INS+I) .GT. 30)  IRET = 11
C       no need to call BCLASR here, it will be overwritten anyway !!
      ENDIF
C
C       TIME card
C
      INS = IW(NATIME)
      IF(INS .NE. 0)  THEN
C       syntax error : only one TIME card with one parameter allowed
        IF (IW(INS-1) .NE. 0 .OR. IW(INS) .NE. 1)  IRET = 11
C       syntax error : argument must be integer
        IF (IW(INS+1) .NE. IFIX(FLOAT(IW(INS+1))))  IRET = 11
      ENDIF
C
C - set NNMIN, NNMAX, MAXRUN and MAXEVT in ABSEVT
C
      CALL ABSMAX (MAXRUN,MAXEVT)
      CALL ABSLIM (NNMIN,NNMAX)
C
      END
