      SUBROUTINE UTAHDR(IBKNA,NRBOS,IMFLG,NCOLS,NROWS,IMODE,LUNPR)
C ---------------------------------------------------------------------
C.
C! - Print the table header
C.
C. - Author   : A. Putzer  - 87/08/14
C. - Modified : A. Putzer  - 89/03/14
C.
C.
C.   Arguments: -  IBKNA CHA*4 (input) Name of the bank
C.              -  NRBOS INTE  (input) BOS NR
C.              -  IMFLG INTE  (input)
C.                             1 = Table taken from d/a file
C.                             2 = Table taken from memory
C.              -  NCOLS INTE  (input) Number of columns
C.              -  NROWS INTE  (input) Number of rows
C.              -  IMODE INTE  (input) Print mode
C.                             1 = horizontally
C.                             2 = vertically
C.                             3 = vertically (continued)
C.              -  LUNPR INTE  (input) Unit for print out
C.
C ---------------------------------------------------------------------
      SAVE
      CHARACTER*4 IBKNA,IVERS
      CHARACTER*12 IPMOD(2)
      CHARACTER*21 IFMOD(2)

      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      EXTERNAL MDARD,NDANR,NDROP
C...............
      DATA IVERS/' 2.0'/
      DATA IPMOD/'horizontally','vertically '/
      DATA IFMOD/'[taken from d/a file]',
     +           '[taken from memory]  '/
C
      IF (IMODE.LT.3) THEN
C
C --   Print the complete header part of a table
C
        WRITE(LUNPR,6001) IVERS,IBKNA,NRBOS,IFMOD(IMFLG),
     +                    NCOLS,NROWS,IPMOD(IMODE)
 6001   FORMAT(///,1X,'|',         30('----'),            '--|',/,
     +             1X,'|', 98X,'UTAPRI version ',A4,5X,     '|',/,
     +             1X,'|',122X,                             '|',/,
     +             1X,'|',8X,'Table name          :',2X,A4,6X,
     +                     '   ( NR  = ',I6,' )',9X,A21,32X,'|',/,
     +             1X,'|',122X,                             '|',/,
     +             1X,'|',8X,'Number of columns   :',I6,87X,'|',/,
     +             1X,'|',8X,'Number of rows      :',I6,87X,'|',/,
     +             1X,'|',122X,                             '|',/,
     +             1X,'|',8X,'Columns are printed ',A12,82X,'|',/,
     +             1X,'|',122X,                             '|',/,
     +             1X,'|',         30('----'),            '--|')
      ELSE
C
C --   Print a short header part if a table print is continued
C
        WRITE(LUNPR,6002) IBKNA,NRBOS
 6002   FORMAT(///,1X,'|',         30('----'),            '--|',/,
     +             1X,'|',122X,                             '|',/,
     +             1X,'|',8X,'Table name          :',2X,A4,6X,
     +                       '   ( NR  = ',I6,' )',
     +                  '       ....... continued  ',   36X,'|',/,
     +             1X,'|',122X,                             '|',/,
     +             1X,'|',         30('----'),            '--|')
      ENDIF
      RETURN
      END
