      SUBROUTINE UTDBCO
C ---------------------------------------------------------------------
C.
C! - Print global information on the database content
CKEY PRINT DBASE / USER
C. - Author   : A. Putzer  - 89/03/18
C.
C.
C ---------------------------------------------------------------------
      SAVE
C - Column numbers in the ADBS table for
C           Table IVERS,IDATE
      PARAMETER (JTABIV = 1, JTABTE = 2)
C - Column numbers in the .TAB table for
C           Table ID, Table name
      PARAMETER (JTABID = 1, JTABNA = 2)
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      EXTERNAL MDARD,NDANR,NDROP
      CHARACTER*4 CTABL,CHAINT
      CHARACTER*4 ITANA,ICOL2,ITAB2,IADBS
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C - Lth CHAR*4 element of the NRBOSth row of the bank with index ID
      CTABL(ID,NRBOS,L) = CHAINT(IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L))
C
      DATA ICOL2/'COL2'/,ITAB2/'TAB2'/,IADBS/'ADBS'/
      IFLAG = 0
      LUNDA = JUNIDB(0)
      LUNPR = IW(6)
C
C - Get Bank .TAB from d/a file into memory
C
      INDT = MDARD(IW,LUNDA,'.TAB',0)
C
C - Is table information in this daf at all?
C
      IF (INDT.EQ.0) THEN
        WRITE(LUNPR,6000)
 6000   FORMAT(//' * UTDBCO * --> No table information on this DAF',//)
        GO TO 999
      ENDIF
      NROWT = LROWS(INDT)
C
C - Get Bank .COL from d/a file into memory
C
      INDC = MDARD(IW,LUNDA,'.COL',0)
      IF (INDC.EQ.0) THEN
        WRITE(LUNPR,6001)
 6001   FORMAT(//' * UTDBCO * -> No Column information on this DAF',//)
        GO TO 999
      ENDIF
C
C - Loop over all tables
C
      ITABC = 0
      DO 100 I = 1,NROWT
        ITAID = ITABL(INDT,I,JTABID)
        ITANA = CTABL(INDT,I,JTABNA)
C
C - Tables  'COL2' and 'TAB2' should not be printed
C
        IF (ITANA.EQ.ICOL2.OR.ITANA.EQ.ITAB2) GO TO 100
C
C - Check if bank is already in memory
C
          INDB = IW(NAMIND(ITANA))
          IF (INDB.GT.0) THEN
            NRLO = IW(INDB-2)
            IMFLG = 2
          ELSE
C
C - Get wanted bank from d/a file into memory
C
            NR0 = 0
            NRLO = NDANR(LUNDA,ITANA,'GE',NR0)
            INDB = MDARD(IW,LUNDA,ITANA,NRLO)
            IMFLG = 1
          ENDIF
C
C - Does this bank exist ?
C
          IF (INDB.EQ.0) THEN
            WRITE(LUNPR,6003) ITANA
 6003       FORMAT(//' * UTDBCO * --> Table ',A4,' not on this DAF',//)
            GO TO 999
          ELSE
            NCOLB = LCOLS(INDB)
            NROWB = LROWS(INDB)
C-
            IF (I.EQ.1) THEN
              IDVERS = ITABL(INDB,1,JTABIV)
              IDDATE = ITABL(INDB,1,JTABTE)
              WRITE(LUNPR,6005) IDVERS,IDDATE
 6005         FORMAT(1H1,//,10X,'|',         14('----'),       '-|',/,
     +                   10X,'|', 57X,                        '|',/,
     + 10X,'|',10X,' Content of the Aleph Data Base                |',/,
     +                   10X,'|', 57X,                        '|',/,
     + 10X,'|   Version = ',I5,'    Date of last change = ',I8,5X,'|',
     +                 /,10X,'|', 57X,                        '|',/,
     +                   10X,'|', 57X,                        '|',/,10X,
     +  '|  Table Nr | Table Name |    NR    | Columns  |   Rows   |',
     +  /,10X,'|',         14('----'),            '-|')
            ELSE
              ITABC = ITABC + 1
              WRITE(LUNPR,6004) ITABC,ITANA,NRLO,NCOLB,NROWB
 6004         FORMAT(10X,'|  ',I7,'  |',4X,A4,4X,'|',3(2X,I6,'  |'))
            ENDIF
C
C - If bank was taken from d/a file : drop bank from memory
C
              IF (IMFLG.EQ.1) IXX = NDROP(ITANA,NRLO)
C
          ENDIF
 100  CONTINUE
      WRITE(LUNPR,6007)
 6007 FORMAT(10X,'|',         14('----'),            '-|'/,1H1)
 999  CONTINUE
      RETURN
      END
