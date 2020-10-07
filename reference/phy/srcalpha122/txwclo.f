      SUBROUTINE TXWCLO
CKEY TOOLS / USER
C-----------------------------------------------------------------------
C! Auxiliary to TXTREE : write the end of the LaTex file and close it
C  Author   : A. Bonissent October 1992
C-----------------------------------------------------------------------
      COMMON / TXTRLU / LUNTXT
C
      LUN = LUNTXT
      WRITE (LUN,900) '\\end{tabbing}'
      WRITE (LUN,900) '\\end{document}'
  900 FORMAT (A)
      CLOSE (LUN)
C
      RETURN
      END
