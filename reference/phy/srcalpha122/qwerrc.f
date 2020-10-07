      SUBROUTINE QWERRC (IER, CSUBR, CTYPE)
CKEY ERROR CARDS /INTERNAL
C----------------------------------------------------------------------
C! print messages for return codes from card reading routines
C called from user QMCARD,QUIHIS
C                                                   H.Albrecht 20.09.88
C----------------------------------------------------------------------
      CHARACTER * (*) CSUBR,CTYPE
      CHARACTER CS*10, CT*10
C----------------------------------------------------------------------
      CT = CSUBR
      CS = '0_' // CT(1:LENOCC(CT)) // '_ '
      CT = CTYPE
C
      GO TO (10,20,30,40,50,60,70), IER
      GO TO 100
C
   10 CALL QWMESS (CS //
     +    'Warning: unknown file type; expected : ' // CT)
      GO TO 100
   20 CALL QWSYNT (CS // 'GIME was not successful')
      GO TO 100
   30 CALL QWSYNT (CS //
     +    'Contradictory parameters found on input card')
      GO TO 100
   40 CALL QWSYNT (CS // 'Parameters on input card are too long')
      GO TO 100
   50 CALL QWSYNT (CS // 'Wrong file type; should be ' // CT)
      GO TO 100
   60 CALL QWSYNT (CS // 'Input file does not exist')
      GO TO 100
   70 CALL QWSYNT (CS // 'This file is expected to reside on DISK')
  100 CONTINUE
      END
