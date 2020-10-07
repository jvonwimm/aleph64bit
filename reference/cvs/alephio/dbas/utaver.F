      SUBROUTINE UTAVER(ITANA,ITAID,NRLO,IMFLG,INDB,INDC,LUNPR)
C ---------------------------------------------------------------------
C.
C! - Print the the table body vertically
C.
C. - Author   : A. Putzer  - 87/08/08
C. - Modified : A. Putzer  - 89/07/20
C.
C.
C.   Arguments: -  ITANA CHA*4 (input) Name of the bank wanted
C.              -  ITAID INTE  (input) Table (ADAMO) ID
C.              -  NRLO  INTE  (input) NR (BOS) for the current table
C.              -  IMFLG INTE  (input)
C.                             1 = Table taken from d/a file
C.                             2 = Table taken from memory
C.              -  INDB  INTE  (input) INDEX (BOS) for the current table
C.              -  INDC  INTE  (input) INDEX (BOS) for the .COL table
C.              -  LUNPR INTE  (input) Unit number for print output
C.
C ---------------------------------------------------------------------
#ifndef DOC
      SAVE
C - Column numbers in the .COL table for
C           Column Name, Type, Format, TableID
      PARAMETER (JCOLNA = 2, JCOLTY = 6, JCOLFO = 7, JCOLTI = 9)
      DIMENSION ISTOR(33),FMT(63),FMT2(33)
C
      CHARACTER*4 ITANA,ICOLT,FMT,IOPBR,ICLBR,IIMPL,ISTOR
      CHARACTER*4 IIGEN,ICH16,ICHA8
      CHARACTER*6 JROW
      CHARACTER*12 IDASH
      CHARACTER*16 ICONT
      CHARACTER*48 IFVER
#include "bcs.h"
      CHARACTER*4 CTABL ,CHAINT
#include "bmacro.h"
C - Lth CHAR*4 element of the NRBOSth row of the bank with index ID
      CTABL(ID,NRBOS,L) = CHAINT(IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L))
      DATA IOPBR/'(1X,'/,ICLBR/'   )'/
      DATA IIGEN/'GEN '/,ICH16/'CH16'/,IIMPL/'IMPL'/,ICHA8/'CHA8'/
      DATA IFVER/'''|'',I8,1X,''|'', 1X,4A4,2X,''|'',1X,A4,A3,''|'''/
      DATA JROW/' Row ='/
      DATA IDASH/'-----------|'/
      DATA ICONT/'    =           '/
C
      IFLAG = 0
      CALL UTCBLK(ISTOR,33)
      CALL UTCBLK(FMT  ,63)
      FMT( 1) = IOPBR
      FMT(63) = ICLBR
C
      NCOLB = LCOLS(INDB)
      NROWB = LROWS(INDB)
      NCOLC = LCOLS(INDC)
      NROWC = LROWS(INDC)
C
C - Print header

      CALL UTAHDR(ITANA,NRLO,IMFLG,NCOLB,NROWB,2,LUNPR)
      CALL UTCCOP(IFVER,FMT(2),12)
      IRMIN = -6
C- Get colum specs. for this table from .COL
 300  CONTINUE
        IF(IRMIN.GT.0) CALL UTAHDR(ITANA,NRLO,IMFLG,NCOLB,NROWB,3,LUNPR)
        IRMIN = IRMIN + 7
        IRMAX = IRMIN + 6
        IF (IRMAX.GT.NROWB) IRMAX = NROWB
C
C - Print additional header for vertical print
C
      WRITE(LUNPR,6002) ( JROW,IROW ,IROW=IRMIN,IRMAX)
 6002   FORMAT(1X,'| Col.Nr. | Column Name       | Format |',
     +             7(A6,I4,' |'))
        WRITE(LUNPR,6003) ((IDASH),IROW=IRMIN,IRMAX)
 6003   FORMAT(1X,'|---------|-------------------|--------|',7A12)
        IDC = 0
        NCC = 0
        JJ = 2
        LL = INDC + LMHLEN - NCOLC
 301    IDC = IDC + 1
        LL = LL + NCOLC
        IF (ITABL(INDC,IDC,JCOLTI).NE.ITAID) GO TO 301
        ICOLT = CTABL(INDC,IDC,JCOLTY)
        IF (ICOLT.EQ.IIMPL) GO TO 301
        NCC = NCC + 1
        DO 635 IJ = 1,4
 635      ISTOR(IJ) = CTABL(INDC,IDC,JCOLNA+IJ-1)
        IF (ICOLT.EQ.ICH16) ICOLT=IIGEN
C
C - Fill the format for each column into FMT
C
        ISTOR(5) = CTABL(INDC,IDC,JCOLFO)
        ISTOR(6) = CTABL(INDC,IDC,JCOLFO+1)
        CALL UTCOFO(ICOLT,ISTOR(5),FMT(14))
        II0 = 1
        ISS = 6
        DO 302 III = IRMIN,IRMAX
          IF (III.GT.IRMIN) CALL UTCCOP(FMT(14),FMT(5*II0+14),5)
          II0 = II0 + 1
          ISS = ISS + 1
          ISTOR(ISS) = CTABL(INDB,III,NCC)
 302    CONTINUE
        WRITE(LUNPR,FMT,ERR=399) NCC,(ISTOR(JJJ),JJJ=1,ISS)
 399    CONTINUE
        IF (ICOLT.EQ.IIGEN.OR.ICOLT.EQ.ICHA8) THEN
          CALL UTCCOP(ICONT,ISTOR(1),4)
          CALL UTCCOP(ICONT,ISTOR(5),2)
          IINDX = 1
          IF (ICOLT.EQ.IIGEN) IINDX = 3
          DO 303 JJ3 = 1,IINDX
            ISS = 6
            DO 304 III = IRMIN,IRMAX
              ISS = ISS + 1
              ISTOR(ISS) = CTABL(INDB,III,NCC+1)
 304        CONTINUE
            NCC = NCC + 1
            WRITE(LUNPR,FMT,ERR=398) NCC,(ISTOR(JJJ),JJJ=1,ISS)
 398        CONTINUE
 303      CONTINUE
        ENDIF
        CALL UTCBLK(ISTOR,33)
        CALL UTCBLK(FMT(16),45)
        IF (NCC.LT.NCOLB) THEN
          GO TO 301
        ENDIF
        WRITE(LUNPR,6003) ((IDASH),IROW=IRMIN,IRMAX)
        IF (IRMAX.LT.NROWB) GO TO 300
      RETURN
      END
#endif
