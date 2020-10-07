      SUBROUTINE UTDBPR(IBKNA,IBSNR,IRWNR,LUNDA,LUNPR,IFLAG)
C ---------------------------------------------------------------------
C.
C! - Print the content of tables on d/a files
C.
C. - Author   : A. Putzer  - 87/10/01
C. - Modified : A. Putzer  - 87/11/20
C. - Modified : A. Putzer  - 88/10/13
C. - Modified : A. Putzer  - 89/03/14
C.
C.
C.   Arguments: -  IBKNA (input) Name of the bank wanted (or 'ALL ')
C.              -                (or 'X   ' for all banks starting
C.              -                with X)
C.              -  IBSNR (input) BOS NR (or 0 for all)
C.                               ignored if more than one table
C.                               requested
C.              -  IRWNR (input) Row NR (or 0 for all)
C.                               ignored if more than one table
C.                               requested
C.              -  LUNDA (input) Unit number for the d/a file
C.              -  LUNPR (input) Unit for print out
C.              -  IFLAG(output) Error flag  = 0 ok
C.                                            -1 No format info on d/a
C.                                            -2 No info for this table
C.                                            -3 Table not on this d/a
C.                                            -4 Table empty
C.
C ---------------------------------------------------------------------
#ifndef DOC
      SAVE
C - Column numbers in the .TAB table for
C           Table ID, Table name
      PARAMETER (JTABID = 1, JTABNA = 2)
C - Maximum number of colums which can be printed horizontally
      PARAMETER (NCOMX = 9)
C
      CHARACTER*4 IBALL,IBKNA,ITANA,IBKXX,ICOL2,ITAB2
      CHARACTER*1 IBKN1(4),BLK1,ITAN1(4)
      LOGICAL WILDF
#include "bcs.h"
      EQUIVALENCE (IBKXX,IBKN1(1))
      EQUIVALENCE (ITANA,ITAN1(1))
      EXTERNAL MDARD,NDANR,NDROP
      CHARACTER*4 CTABL,CHAINT
#include "bmacro.h"
C - Lth CHAR*4 element of the NRBOSth row of the bank with index ID
      CTABL(ID,NRBOS,L) = CHAINT(IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L))
C..........
      DATA IOUDA/0/,BLK1/' '/
      DATA IBALL/'ALL '/
      DATA ICOL2/'COL2'/,ITAB2/'TAB2'/
C
      IFLAG = 0
C
C - Drop banks .TAB and .COL if the d/a unit has changed
C - otherwise BOS will not copy the banks from d/a to memory!
C
      IF (IOUDA.NE.LUNDA) THEN
        IND = NDROP('.TAB',0)
        IND = NDROP('.COL',0)
      ENDIF
      IOUDA = LUNDA
C
C - Get Bank .TAB from d/a file into memory
C
      INDT = MDARD(IW,LUNDA,'.TAB',0)
C
C - Is table information in this daf at all?
C
      IF (INDT.EQ.0) THEN
        IFLAG = -1
        WRITE(LUNPR,6000)
 6000   FORMAT(//' * UTDBPR * --> No table information on this DAF',//)
        GO TO 999
      ENDIF
      NROWT = LROWS(INDT)
C
C - Get Bank .COL from d/a file into memory
C
      INDC = MDARD(IW,LUNDA,'.COL',0)
      IF (INDC.EQ.0) THEN
        IFLAG = -1
        WRITE(LUNPR,6001)
 6001   FORMAT(//' * UTDBPR * -> No Column information on this DAF',//)
        GO TO 999
      ENDIF
C
C - Check if a wild character is used ?
C
      IBKXX = IBKNA
      IF(IBKN1(2).EQ.BLK1) THEN
        WILDF = .TRUE.
      ELSE
        WILDF = .FALSE.
      ENDIF
C
C - Loop over all tables on which information exists on d/a file
C
      ICOUN = 0
      DO 100 I = 1,NROWT
        ITAID = ITABL(INDT,I,JTABID)
        ITANA = CTABL(INDT,I,JTABNA)
C
C - Tables  'COL2' and 'TAB2' should not be printed
C
        IF (ITANA.EQ.ICOL2.OR.ITANA.EQ.ITAB2) GO TO 100
C
        IF (ITANA.EQ.IBKNA.OR.IBKNA.EQ.IBALL.OR.
     +      (WILDF.AND.IBKN1(1).EQ.ITAN1(1))) THEN
          ICOUN = ICOUN + 1
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
            IF (IBKNA.EQ.IBALL.OR.WILDF) GO TO 100
            IFLAG = -3
            WRITE(LUNPR,6003) IBKNA
 6003       FORMAT(//' * UTDBPR * --> Table ',A4,' not on this DAF',//)
            GO TO 999
          ELSE
            NCOLB = LCOLS(INDB)
            NROWB = LROWS(INDB)
C
C- Check if bank is filled
C
            IF (NROWB.EQ.0) THEN
              IF (IBKNA.EQ.IBALL.OR.WILDF) GO TO 100
              IFLAG = -4
              WRITE(LUNPR,6004) IBKNA
 6004         FORMAT(//' * UTDBPR * --> Table ',A4,' empty',//)
              GO TO 999
C
C- check if bank contains too many rows or columns
C
            ELSE IF (NROWB.GT.255.OR.NCOLB.GT.255) THEN
              IF (IBKNA.EQ.IBALL.OR.WILDF) THEN
                WRITE(LUNPR,6005) ITANA
 6005           FORMAT(//' * UTDBPR * --> Table ',A4,' has more than',
     X          ' 255 rows or columns .. only printed on request',//)
                GO TO 100
              ENDIF
            ENDIF
C
C - Decide on print format (horizontal or vertical)
C
 200          IF (NCOLB.LE.NCOMX) THEN
                CALL UTAHOR(ITANA,ITAID,NRLO,IMFLG,INDB,INDC,LUNPR)
              ELSE
                CALL UTAVER(ITANA,ITAID,NRLO,IMFLG,INDB,INDC,LUNPR)
              ENDIF
C
C - If bank was taken from d/a file : drop bank from memory
C
              IF (IMFLG.EQ.1) IXX = NDROP(ITANA,NRLO)
C
          ENDIF
        ENDIF
 100  CONTINUE
C
C - Check if column information was stored for a selected table
C
      IF (ICOUN.EQ.0) THEN
        IF (IBKNA.EQ.IBALL.OR.WILDF) GO TO 999
        IFLAG = -2
        WRITE(LUNPR,6002) IBKNA
 6002   FORMAT(//' * UTDBPR * -> No Column info. for table ',A4,//)
      ENDIF
 999  CONTINUE
      RETURN
      END
#endif
