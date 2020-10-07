      SUBROUTINE BDAWA(LUN)
*
*     Start delay of da writing - create bank for name,nr
*
      IF( NLINK('+DAW',LUN).NE.0) GOTO 100
      IND=NBANK('+DAW',LUN,501)
  100 RETURN
      END
