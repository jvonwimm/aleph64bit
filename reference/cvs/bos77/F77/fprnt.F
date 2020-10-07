      SUBROUTINE FPRNT(XW,NWRD,FMTX)

*     print array using format

      REAL XW(NWRD)
      CHARACTER*(*) FMTX
*     translate format
      CALL TRFMT(FMTX,IFMT,NFMT)
      IF(NFMT.EQ.0) GOTO 100
      CALL FPRNN(XW,NWRD,IFMT,NFMT)
  100 RETURN
      END
