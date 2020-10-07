      SUBROUTINE UTAFMT(ITANA,ITAID,INDC,NCOLB,IFLAG,BOSFMT)
C ---------------------------------------------------------------------
C.
C! - Find bank format from ADAMO tables - store it in BOSFMT
C.
C. - from     : A. Putzer  - routine UTAVER
C. - modified by M.Rumpf   - April 88
C. - modified by F.Ranjard - March 90
C
C.   Input:     -  ITANA / CHA*4  = Name of the bank wanted
C.              -  ITAID / INTE   = Table (ADAMO) ID
C.              -  INDC  / INTE   = INDEX (BOS) for the .COL table
C.   Output:    -  NCOLB / INTE   = number of columns in the bank
C.              -  IFLAG / INTE   = 0=OK
C.              -  BOSFMT/ CHA*512= bank bos format
C ---------------------------------------------------------------------
#ifndef DOC
#include "col2jj.h"
      PARAMETER (NCOLMX = 10000)
      CHARACTER * 512 STRING
      CHARACTER * 2 HEAD
      CHARACTER * 4 ISTOR (NCOLMX),IPROV,IWFMT
      CHARACTER*4 ITANA,ICOLT,IIMPL,IIGEN,ICH16
      CHARACTER IOPBR,ICLBR
      CHARACTER KSTOR(NCOLMX),GFMT,FFMT,AFMT,IFMT,COMMA,BLANK,CHOLD,EFMT
      CHARACTER CHS
      CHARACTER * (*) BOSFMT
#include "bcs.h"
      CHARACTER*4 CW(1000)
      EQUIVALENCE (CW(1),IW(1))
      CHARACTER*4 CTABL
C - Lth CHAR*4 element of the NRBOSth row of the bank with index ID
      CTABL(ID,NRBOS,L) = CW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
      DATA IOPBR/'('/,ICLBR/')'/
      DATA IIGEN/'GEN '/,ICH16/'CH16'/,IIMPL/'IMPL'/
      DATA AFMT,FFMT,EFMT,GFMT,IFMT/'A','F','E','G','I'/
      DATA COMMA,BLANK/',',' '/
      DATA HEAD/'2I'/
C
#include "bmacro.h"
C ---------------------------------------------------------------
C
      IFLAG = 0
      BOSFMT = BLANK
C
C - look for 1st and last column description in .COL
C
      NCOLC = LCOLS(INDC)
      NROWC = LROWS(INDC)
      NCOLB = 0
      DO 1 I =1,NROWC
        IF (ITABL(INDC,I,JCOLTI).EQ.ITAID) THEN
           IF (NCOLB.EQ.0) IFROW = I
           NCOLB = NCOLB+1
        ELSE
           IF(NCOLB.GT.0)GOTO 2
        ENDIF
 1    CONTINUE
 2    CONTINUE
      ILROW = IFROW + NCOLB -1
      LUNPR = IW(6)
C
        CALL UTCBLK(ISTOR,NCOLB)
C
C- Get colum specs. for this table from .COL
C
        NCC = 0
        DO 301 IDC = IFROW,ILROW
           ICOLT = CTABL(INDC,IDC,JCOLTY)
           IF (ICOLT.EQ.IIMPL) GO TO 301
C
C - Fill the format for each row into BOSFMT
C _ Put in KSTOR format for each column: A,I or F
C
           IF (ICOLT.EQ.ICH16) ICOLT=IIGEN
           IF (ICOLT.EQ.IIGEN) THEN
C         CH16 means 4 columns
              IF (NCC+4.GT.NCOLMX) GOTO 998
              DO 100 JJ = 1,4
                 NCC = NCC + 1
                 ISTOR(NCC) = AFMT
  100         CONTINUE
           ELSE IF (ICOLT.EQ.'CHA8') THEN
C         CHA8 means 2 columns
              IF (NCC+2.GT.NCOLMX) GOTO 998
              NCC = NCC + 1
              ISTOR(NCC) = AFMT
              NCC = NCC + 1
              ISTOR(NCC) = AFMT
           ELSE
              IF (NCC+1.GT.NCOLMX) GOTO 998
              NCC = NCC+1
              LL = KROW (INDC,IDC)
              CALL UTCCOP(CW(LL+JCOLFO),ISTOR(NCC),1)
           ENDIF
 301    CONTINUE
        NCOLB = NCC
C
C - IF bank without any column THEN format=2I
C
        IF (NCOLB.EQ.0) THEN
           BOSFMT = HEAD
           GOTO 999
        ENDIF
C
C - bank with NCOLB columns
C
        DO 101 K=1,NCOLB
          KSTOR (K) = ISTOR(K)(1:1)
          IF(KSTOR(K).EQ.GFMT.OR.KSTOR(K).EQ.EFMT) KSTOR(K) = FFMT
  101   CONTINUE
C       PRINT * ,' BANK ',ITANA, ' no. of col ', NCOLB
C       PRINT *,' ISTOR ',(ISTOR(I),I=1,NCOLB)
C       PRINT *,' KSTOR ',(KSTOR(I),I=1,NCOLB)
C
C Create Bos style format for BKFMT
C Count identical consecutive items (max = 9999)
C
      CHOLD = BLANK
      BOSFMT = BLANK
      N=1
      NSAME = 1
      IF (NCOLB+1.GT.NCOLMX) GOTO 998
      KSTOR(NCOLB + 1) = BLANK
      DO 200 I=1,NCOLB + 1
      IF(NSAME.GT.9999) GO TO 996
      IF(KSTOR(I).NE.CHOLD)   THEN
        IF(NSAME.GT.1)        THEN
           NN=1
           IF(NSAME.GT.9) NN=2
           IF(NSAME.GT.99) NN=3
           IF(NSAME.GT.999) NN=4
           WRITE(CHS,'(I1)') NN
           IWFMT = '(I'//CHS//')'
           WRITE(IPROV,IWFMT) NSAME
           BOSFMT = BOSFMT(:N-2)//COMMA//IPROV(:NN)//CHOLD
           N = N + NN
           NSAME = 1
        ENDIF
        BOSFMT = BOSFMT(:N)//COMMA//KSTOR(I)
        N = N + 2
      ELSE
        NSAME = NSAME + 1
      ENDIF
      CHOLD = KSTOR(I)
  200 CONTINUE
C
C End count - Now add header format and brackets
C
      STRING = BOSFMT(3:N-2)
      BOSFMT = HEAD//COMMA//IOPBR//STRING(1:N-4)//ICLBR
      IF (INDEX(BOSFMT,'A').EQ.0 .AND. INDEX(BOSFMT,'F').EQ.0
     &    .AND. INDEX(BOSFMT,'B').EQ.0) BOSFMT = 'I'
      LFMT   = LENOCC(BOSFMT)
C     PRINT *,'NAME NCOLB LEN FMT',ITANA,NCOLB,LFMT,BOSFMT
C
  999 RETURN
C
C Errors
C
  996 WRITE (LUNPR,1996) NSAME
      IFLAG = -1
      GO TO 999
  998 WRITE (LUNPR,1998) ITANA,NCC,NCOLMX
      IFLAG = -1
 1996 FORMAT(1X,' +++ UTAFMT +++ Too many identical items ',
     &       I7,' in format')
 1998 FORMAT(1X,' +++ UTAFMT +++ Bank ',A4,' has ',I5,' columns ',
     &       ' Maximum allowed :',I5,' Format not found')
      END
#endif
