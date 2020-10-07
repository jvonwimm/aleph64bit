*DK DZLBKN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZLBKN
CH
      SUBROUTINE DZLBKN(TBNK,NRLEN,NUBA,NUM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     23-JUN-1992
C
C!: List all of bank numbers
C    Inputs    :
C         TBNK : The bank name
C   Outputs    :
C         NUBA : Array contain all NR of the bank
C          NUM : The number of bank NR
C    Called by : DALB6
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      CHARACTER *4 TBNK,TBNK1
      DIMENSION NUBA(*)
C     EXTERNAL NAMIND,NLINK
C     INTEGER  NAMIND,NLINK
      NUM=0
C     ...................................... NAMI is the name index of the bank
      NAMI=NAMIND(TBNK)
      TBNK1=TBNK
C     .............................. Change TBNK1 from upper case to lower case
      CALL DZUTOL(TBNK1,4)
      NAMI1=NAMIND(TBNK1)
C     ............................... Take the bank with the lowest bank number
      IBNK=IW(NAMI)
      IF (IBNK.EQ.0) THEN
        TBNK=TBNK1
        NAMI=NAMI1
        IBNK=IW(NAMI)
        IF (IBNK.EQ.0) THEN
          RETURN
        END IF
      END IF
C     ........................................................ Get bank numbers
      IBNK=NAMI+1
  666 IBNK=IW(IBNK-1)
      IF(IBNK.GT.0)THEN
        NR=IW(IBNK-2)
        NUM=NUM+1
        IF(NUM.LE.NRLEN) THEN
          NUBA(NUM)=NR
          GO TO 666
        ELSE
          NUM=NRLEN
          RETURN
        END IF
      END IF
      RETURN
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZLOOK
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZLOOK
CH
      SUBROUTINE DZLOOK(NTLEN,TA,TBNK,NR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   12-DEC-1991
C
C!   Modified by Rongfen XU                    15-DEC-1991
C
C!: List bank
C    Inputs    :
C           TA : The user command
C                TA='LB' Look a single bank with it's number
C                TA='LA' Look a set of bank with total number
C         TBNK : The bank name
C           NR : bank number
C        NTLEN : The width of one line for listing bank
C    Called by : DALB6
C ---------------------------------------------------------------------
C         IBNK : The index of bank
C        NWRDS : The total words # of the bank
C        NCOLS : The total columns # of the bank
C        NROWS : The total rows # of the bank
C       INDCDZ : Indicate which columns will be changed from radian to degree
C            INDCDZ(I)=1 column I (angle) will be changed from radian to degree
C            INDCDZ(I)=0 column I (angle) will be reset to radian
      INCLUDE 'A_BCS.INC'
      PARAMETER(NRLEN=1000)
      PARAMETER(NRWLN=20000)
      PARAMETER(NFMTL=5000)
      COMMON/DZXULB/INDCDZ(NFMTL),MARKDZ(NFMTL),NCSTDZ
      CHARACTER *4 TBNK,TBNKO,TBNK1
      CHARACTER *2 TA
      CHARACTER *49 T49
      DIMENSION NUBA(NRLEN),NUMID(2,NRWLN)
      EXTERNAL NAMIND,NLINK
      INTEGER  NAMIND,NLINK
      IF(NTLEN.GT.140)NTLEN=140
C      IF(NTLEN.GT.132)NTLEN=132
C     ....... 'JCAR' and 'OROP' are no-tabular format banks, could not be read
      IF(TBNK.EQ.'JCAR'.OR.TBNK.EQ.'OROP')THEN
        NAMI=NAMIND(TBNK)
        IBNK=IW(NAMI)
        IF(IBNK.NE.0)THEN
C         ........................ Setting a character variable T49 equal empty
          T49=' '
          T49(1:45)='The special format bank '//TBNK//
     &    ' can not be read.'
          CALL DWRT(T49)
        END IF
        RETURN
      END IF
C     ...................................... If bank is changed set INDCDZ(I)=0
      IF(TBNK.NE.TBNKO)THEN
        DO I=1,NFMTL
          INDCDZ(I)=0
          MARKDZ(I)=0
        END DO
        NCSTDZ=1
      END IF
      IF(TA.EQ.'LB')THEN
        IBNK=NLINK(TBNK,NR)
        IF(IBNK.EQ.0) THEN
          GO TO 666
        ELSE
C         ................................... List bank with it's single number
          CALL DZLISB(NTLEN,IBNK,TBNK)
          TBNKO=TBNK
          RETURN
        END IF
      END IF
C     ...................................... NAMI is the name-index of the bank
C     ........................................... IBNK is the index of the bank
  666 NAMI=NAMIND(TBNK)
      TBNK1=TBNK
C     .............................. Change TBNK1 from upper case to lower case
      CALL DZUTOL(TBNK1,4)
      NAMI1=NAMIND(TBNK1)
C     ............................... Take the bank with the lowest bank number
      IBNK=IW(NAMI)
      IF (IBNK.EQ.0) THEN
        TBNK=TBNK1
        NAMI=NAMI1
        IBNK=IW(NAMI)
        IF (IBNK.EQ.0) THEN
C          CALL DWRT('Bank not found.')
          RETURN
        END IF
      END IF
C     ...................................... If the bank has just single number
      IF(IW(IBNK-1).EQ.0)THEN
C       ......................................................... List the bank
        CALL DZLISB(NTLEN,IBNK,TBNK)
        TBNKO=TBNK
        RETURN
      END IF
C     ........................................................ Get bank numbers
      CALL DZGTNM(NAMI,TBNK,NUM,NUBA,NUMRW,NUMID,NDG,NRDG)
      IF(TA.NE.'LB')THEN
C       ..................................... List bank with it's total numbers
        CALL DZLISA(NTLEN,TBNK,NUMRW,NUMID,NUM,NDG,NRDG)
      ELSE
        CALL DZLISB(NTLEN,IBNK,TBNK)
      END IF
      TBNKO=TBNK
      RETURN
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZLOKA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZLOKA
CH
      SUBROUTINE DZLOKA(NTLEN,TA,TBNK,NR,N1,N2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   12-DEC-1991
C
C!   Modified by Rongfen XU                    15-DEC-1991
C
C!: List bank on above window
C    Inputs    :
C           TA : The user command
C                TA='LB' Look a single bank with it's number
C                TA='LA' Look a set of bank with total number
C         TBNK : The bank name
C           NR : bank number
C        NTLEN : The width of one line for listing bank
C           N1 : The number of the first line to be list
C           N2 : The number of the last line to be list
C    Called by : DALB6
C ---------------------------------------------------------------------
C         IBNK : The index of bank
C        NWRDS : The total words # of the bank
C        NCOLS : The total columns # of the bank
C        NROWS : The total rows # of the bank
C       INDCDZ : Indicate which columns will be changed from radian to degree
C            INDCDZ(I)=1 column I (angle) will be changed from radian to degree
C            INDCDZ(I)=0 column I (angle) will be reset to radian
      INCLUDE 'A_BCS.INC'
      PARAMETER(NRLEN=1000)
      PARAMETER(NRWLN=20000)
      PARAMETER(NFMTL=5000)
      COMMON/DZXULB/INDCDZ(NFMTL),MARKDZ(NFMTL),NCSTDZ
      CHARACTER *4 TBNK,TBNKO,TBNK1
      CHARACTER *2 TA
      DIMENSION NUBA(NRLEN),NUMID(2,NRWLN)
C      IF(NTLEN.GT.132)NTLEN=132
      IF(NTLEN.GT.140)NTLEN=140
      IF(TA.EQ.'LB')THEN
        IBNK=NLINK(TBNK,NR)
        IF(IBNK.EQ.0) THEN
          GO TO 666
        ELSE
C         ................................... List bank with it's single number
          CALL DZLKBN(NTLEN,IBNK,TBNK,N1,N2)
          RETURN
        END IF
      END IF
C     ...................................... NAMI is the name-index of the bank
C     ........................................... IBNK is the index of the bank
  666 NAMI=NAMIND(TBNK)
      TBNK1=TBNK
C     .............................. Change TBNK1 from upper case to lower case
      CALL DZUTOL(TBNK1,4)
      NAMI1=NAMIND(TBNK1)
C     ............................... Take the bank with the lowest bank number
      IBNK=IW(NAMI)
      IF (IBNK.EQ.0) THEN
        TBNK=TBNK1
        NAMI=NAMI1
        IBNK=IW(NAMI)
        IF (IBNK.EQ.0) THEN
          RETURN
        END IF
      END IF
C     ...................................... If the bank has just single number
      IF(IW(IBNK-1).EQ.0)THEN
C       ......................................................... List the bank
        CALL DZLKBN(NTLEN,IBNK,TBNK,N1,N2)
        TBNKO=TBNK
        RETURN
      END IF
C     ........................................................ Get bank numbers
      CALL DZGTNM(NAMI,TBNK,NUM,NUBA,NUMRW,NUMID,NDG,NRDG)
      IF(TA.NE.'LB')THEN
C       ..................................... List bank with it's total numbers
        CALL DZLKBA(NTLEN,TBNK,NUMRW,NUMID,NUM,NDG,NRDG,N1,N2)
      ELSE
        CALL DZLKBN(NTLEN,IBNK,TBNK,N1,N2)
      END IF
      RETURN
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZGTNM
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZGTNM
CH
      SUBROUTINE DZGTNM(NAMI,TBNK,NUM,NUBA,NUMRW,NUMID,NDG,NRDG)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C
C!: Get bank numbers
C    Inputs    :
C         NAMI : The name index of the bank
C         TBNK : The bank name
C    Outputs   :
C         NUBA : Array contain the numbers of the bank
C          NUM : NUBA's dimension
C        NUMID : Array
C                NUMID(1,I)=INDEX of the bank for I-th row
C                NUMID(2,I)=NUMBER of the bank for I-th row
C        NUMRW : The number of total rows
C          NDG : The maxmum digits of the bank numbers
C         NRDG : The maxmum digits of the banks's rows
C    Called by : DZLIBK
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      PARAMETER(NRLEN=1000)
      PARAMETER(NRWLN=20000)
      CHARACTER *4 TBNK
      DIMENSION NUBA(NRLEN),NUMID(2,NRWLN)
      EXTERNAL NAMIND,NLINK
      INTEGER  NAMIND,NLINK
      NUM=0
C     ................................... NWRDS is the total words of this bank
      NWRDS=0
      NUMRW=0
      MXNUM=0
C     ..................................... NROWS is the # of rows of this bank
      MXROW=0
      IBNK=IW(NAMI)
C     ................................................... Initialize index IBNK
      IBNK=NAMI+1
C     ....................... IBNK is index for the bank with number IW(IBNK-2)
  666 IBNK=IW(IBNK-1)
C     ............................................ Test termination of the loop
      IF(IBNK.NE.0)THEN
        NWRDS=NWRDS+IW(IBNK)
        NROWS=IW(IBNK+2)
        NR=IW(IBNK-2)
        MXNUM=MAX(MXNUM,NR)
        MXROW=MAX(MXROW,NROWS)
        NUM=NUM+1
        IF(NUM.GT.NRLEN)THEN
          NUM=NRLEN
          GO TO 1
        END IF
        NUBA(NUM)=NR
        DO I=1,NROWS
          NUMRW=NUMRW+1
          IF(NUMRW.GT.NRWLN)THEN
            NUMRW=NRWLN
            GO TO 1
          END IF
          NUMID(1,NUMRW)=IBNK
          NUMID(2,NUMRW)=I
        END DO
        GO TO 666
      END IF
C     ................................................. Get the digits of MXNUM
    1 CALL DZGTDG(MXNUM,NDG)
      NDG=MAX(NDG,2)
C     ................................................. Get the digits of MXROW
      CALL DZGTDG(MXROW,NRDG)
      NRDG=MAX(NRDG,2)
      RETURN
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZLISB
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZLISB
CH
      SUBROUTINE DZLISB(NTLEN,IBNK,TBNK)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     28-JUL-1988
C
C!: List bank with it's single number . e.g.  PFRF 0 (command:LB PFRF 0)
C    Inputs    :
C         NTLEN: The width of one line for listing bank
C         IBNK : The index of bank
C         TBNK : The bank name
C    Outputs   :
C    Called by : DZLOOK
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      PARAMETER(NFMTL=5000)
      CHARACTER *1 TFMD(NFMTL)
      COMMON/DZTLFM/TFMD,TITL
      CHARACTER *5 TALPH
      CHARACTER *4 TBNK,TBNKO
      CHARACTER *2 TITL(NFMTL)
      CHARACTER *140 TB
      TB=' '
C     .................................... NWRDS is the total words of the bank
      NWRDS=IW(IBNK)
C     .................................. NCOLS is the # of columns of this bank
      NCOLS=IW(IBNK+1)
C     ..................................... NROWS is the # of rows of this bank
      NROWS=IW(IBNK+2)
      IF(NWRDS.EQ.2+NCOLS*NROWS) THEN
        TALPH='ALEPH'
      ELSE
        TALPH='BOS  '
        NCOLS=NWRDS
        NROWS=1
      END IF
      NR=IW(IBNK-2)
C     ....................... Write the listing head into character variable TB
      CALL DZHEAD(TB,IETB,TALPH,TBNK,NR,NCOLS,NROWS,NWRDS)
C     .................................................... Write TB on terminal
      CALL DLWRBA(TB(1:IETB))
C     .............................................. Get the format of the bank
      IF(NCOLS.EQ.0.AND.TBNK.EQ.'RUNH')THEN
        NCOLS=NWRDS
        GO TO 1
      END IF
      IF(NCOLS.LT.1)THEN
        RETURN
      END IF
C     ......................... Get the column's titles and formats of the bank
    1 IF(TBNK.NE.TBNKO) THEN
        CALL DZFMTL(TBNK,NCOLS,TITL,TFMD,IERR)
        IF(IERR.NE.0)RETURN
      END IF
C     ....................................... List bank with it's single number
      IF(NROWS.GT.0)
     &CALL DZLBNK(NTLEN,TBNK,IBNK,NCOLS,TITL,TFMD,NWRDS,NROWS,TALPH)
      TBNKO=TBNK
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZLISA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZLISA
CH
      SUBROUTINE DZLISA(NTLEN,TBNK,NUMRW,NUMID,NUM,NDG,NRDG)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     28-JUL-1988
C
C!: List bank with it's total numbers . e.g.  PFRF 0,2  (command:LA PFRF)
C    Inputs    :
C         NTLEN: The width of one line for listing bank
C         TBNK : The bank name
C        NUMID : Array
C                NUMID(1,I)=INDEX of the bank for I-th row
C                NUMID(2,I)=NUMBER of the bank for I-th row
C        NUMRW : The number of total rows
C          NUM : The number of bank NR
C          NDG : The maxmum digits of the bank numbers
C         NRDG : The maxmum digits of the banks's rows
C    Outputs   :
C    Called by : DZLOOK
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      PARAMETER(NRWLN=20000)
      PARAMETER(NFMTL=5000)
      CHARACTER *1 TFMD(NFMTL)
      COMMON/DZTLFM/TFMD,TITL
      CHARACTER *5 TALPH
      CHARACTER *4 TBNK,TBNKO
      CHARACTER *2 TITL(NFMTL)
      CHARACTER *140 TB
      DIMENSION NUMID(2,NRWLN)
      EXTERNAL NAMIND,NLINK
      INTEGER  NAMIND,NLINK
      TB=' '
C     ...................................... NAMI is the name index of the bank
      NAMI=NAMIND(TBNK)
      IBNK=IW(NAMI)
C     .................................... NWRDS is the total words of the bank
      NWRDS=IW(IBNK)
C     .................................. NCOLS is the # of columns of this bank
      NCOLS=IW(IBNK+1)
C     ..................................... NROWS is the # of rows of this bank
      NROWS=IW(IBNK+2)
      IF(NWRDS.EQ.2+NCOLS*NROWS) THEN
        TALPH='ALEPH'
        NWRD=NCOLS*NUMRW+2*NUM
      ELSE
        TALPH='BOS  '
        NWRD=NCOLS*NUMRW
      END IF
C     ....................... Write the listing head into character variable TB
      CALL DZHEAD(TB,IETB,TALPH,TBNK,-9999999,NCOLS,NUMRW,NWRD)
C     .................................................... Write TB on terminal
      CALL DLWRBA(TB(1:IETB))
C     ................................... Get the format of the bank
      IF(NCOLS.EQ.0.AND.TBNK.EQ.'RUNH')THEN
        NCOLS=NWRDS
        GO TO 1
      END IF
      IF(NCOLS.LT.1)THEN
        RETURN
      END IF
C     ......................... Get the column's titles and formats of the bank
    1 IF(TBNK.NE.TBNKO) THEN
        CALL DZFMTL(TBNK,NCOLS,TITL,TFMD,IERR)
        IF(IERR.NE.0)RETURN
      END IF
C     .................. list a set of bank with same name and different number
      IF(NUMRW.GT.0)
     &CALL DZLBKA(NTLEN,NCOLS,TITL,TFMD,NUMRW,NUMID,NDG,NRDG,TBNK,
     &  NWRDS,TALPH)
      TBNKO=TBNK
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZLKBN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZLKBN
CH
      SUBROUTINE DZLKBN(NTLEN,IBNK,TBNK,N1,N2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     28-JUL-1988
C
C!: List bank with it's single number on above window.
C!: e.g.  PFRF 0 (command:LB PFRF 0)
C    Inputs    :
C         NTLEN: The width of one line for listing bank
C         IBNK : The index of bank
C         TBNK : The bank name
C           N1 : The number of the first line to be list
C           N2 : The number of the last line to be list
C    Outputs   :
C    Called by : DZLOKA
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      PARAMETER(NFMTL=5000)
      CHARACTER *1 TFMD(NFMTL)
      COMMON/DZTLFM/TFMD,TITL
      CHARACTER *5 TALPH
      CHARACTER *4 TBNK
      CHARACTER *2 TITL(NFMTL)
      CHARACTER *140 TB
      TB=' '
C     .................................... NWRDS is the total words of the bank
      NWRDS=IW(IBNK)
C     .................................. NCOLS is the # of columns of this bank
      NCOLS=IW(IBNK+1)
C     ..................................... NROWS is the # of rows of this bank
      NROWS=IW(IBNK+2)
      IF(NWRDS.EQ.2+NCOLS*NROWS) THEN
        TALPH='ALEPH'
      ELSE
        TALPH='BOS  '
        NCOLS=NWRDS
        NROWS=1
      END IF
      NR=IW(IBNK-2)
C     ....................... Write the listing head into character variable TB
      CALL DZHEAD(TB,IETB,TALPH,TBNK,NR,NCOLS,NROWS,NWRDS)
C     .................................................... Write TB on terminal
      CALL DLWRBA(TB(1:IETB))
C     .............................................. Get the format of the bank
      IF(NCOLS.EQ.0.AND.TBNK.EQ.'RUNH')THEN
        NCOLS=NWRDS
        GO TO 1
      END IF
      IF(NCOLS.LT.1)THEN
        RETURN
      END IF
C     ....................................... List bank with it's single number
    1 IF(NROWS.GT.0)CALL DZLSBN(NTLEN,TBNK,IBNK,NCOLS,TITL,TFMD,
     &  NWRDS,NROWS,TALPH,N1,N2)
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZLKBA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZLKBA
CH
      SUBROUTINE DZLKBA(NTLEN,TBNK,NUMRW,NUMID,NUM,NDG,NRDG,N1,N2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     28-JUL-1988
C
C!: List bank with it's total numbers on above window.
C!: e.g.  PFRF 0,2  (command:LA PFRF)
C    Inputs    :
C         NTLEN: The width of one line for listing bank
C         TBNK : The bank name
C        NUMID : Array
C                NUMID(1,I)=INDEX of the bank for I-th row
C                NUMID(2,I)=NUMBER of the bank for I-th row
C        NUMRW : The number of total rows
C          NUM : The number of bank NR
C          NDG : The maxmum digits of the bank numbers
C         NRDG : The maxmum digits of the banks's rows
C           N1 : The number of the first line to be list
C           N2 : The number of the last line to be list
C    Outputs   :
C    Called by : DZLOKA
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      PARAMETER(NRWLN=20000)
      PARAMETER(NFMTL=5000)
      CHARACTER *1 TFMD(NFMTL)
      COMMON/DZTLFM/TFMD,TITL
      CHARACTER *5 TALPH
      CHARACTER *4 TBNK
      CHARACTER *2 TITL(NFMTL)
      CHARACTER *140 TB
      DIMENSION NUMID(2,NRWLN)
      EXTERNAL NAMIND,NLINK
      INTEGER  NAMIND,NLINK
      TB=' '
C     ...................................... NAMI is the name index of the bank
      NAMI=NAMIND(TBNK)
      IBNK=IW(NAMI)
C     .................................... NWRDS is the total words of the bank
      NWRDS=IW(IBNK)
C     .................................. NCOLS is the # of columns of this bank
      NCOLS=IW(IBNK+1)
C     ..................................... NROWS is the # of rows of this bank
      NROWS=IW(IBNK+2)
      IF(NWRDS.EQ.2+NCOLS*NROWS) THEN
        TALPH='ALEPH'
        NWRD=NCOLS*NUMRW+2*NUM
      ELSE
        TALPH='BOS  '
        NWRD=NCOLS*NUMRW
      END IF
C     ....................... Write the listing head into character variable TB
      CALL DZHEAD(TB,IETB,TALPH,TBNK,-9999999,NCOLS,NUMRW,NWRD)
C     .................................................... Write TB on terminal
      CALL DLWRBA(TB(1:IETB))
C     ................................... Get the format of the bank
      IF(NCOLS.EQ.0.AND.TBNK.EQ.'RUNH')THEN
        NCOLS=NWRDS
        GO TO 1
      END IF
      IF(NCOLS.LT.1)THEN
        RETURN
      END IF
C     .................. list a set of bank with same name and different number
    1 IF(NUMRW.GT.0)
     &CALL DZLSBA(NTLEN,NCOLS,TITL,TFMD,NUMRW,NUMID,NDG,NRDG,TBNK,
     &  NWRDS,TALPH,N1,N2)
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZHEAD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZHEAD
CH
      SUBROUTINE DZHEAD(TB,IETB,TALPH,TBNK,NR,NCOLS,NROWS,NWRDS)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     28-JUL-1988
C
C!: Write the listing head into character variable TB
C    Inputs    :
C       TALPH : Is a flag
C                TALPH='ALEPH' means this bank is ALEPH bank
C                TALPH='BOS  ' means this bank is BOS  bank
C        NWRDS : The total words # of the bank
C        NCOLS : The total numbers of columns in the bank
C        NROWS : The total rows # of the bank
C           NR : The bank number
C         TBNK : The bank name
C    Outputs   :
C           TB : Character variable contain listing head
C         IETB : The digits of TB
C    Called by : DZLISB,DZLISA
C ---------------------------------------------------------------------
      CHARACTER *5 TALPH
      CHARACTER *4 TBNK
      CHARACTER *140 TB,TMP
C     ............................. Setting a character variable TB equal empty
      TB=' '
      TB(2:11)=TALPH//' '//TBNK
C     ..................................................... Write bank number
      ISTB=13
      IF(NR.EQ.-9999999)THEN
        IETB=17
        TB(ISTB:IETB)='(all)'
      ELSE
C       ........ Get # of digits of NR and write NR into character variable TMP
        CALL DZWRCV(TMP,NR,NVD)
        IETB=13+NVD-1
        TB(ISTB:IETB)=TMP(1:NVD)
      END IF
C     ..................................................... Write bank columns
      ISTB=IETB+1
      IETB=ISTB+11
      TB(ISTB:IETB)=' : columns '
C     .... Get # of digits of NCOLS and write NCOLS into character variable TMP
      CALL DZWRCV(TMP,NCOLS,NVD)
      ISTB=IETB+1
      IETB=ISTB+NVD-1
      TB(ISTB:IETB)=TMP(1:NVD)
C     ..................................................... Write bank rows
      ISTB=IETB+1
      IETB=ISTB+6
      TB(ISTB:IETB)=', rows '
C     .... Get # of digits of NROWS and write NROWS into character variable TMP
      CALL DZWRCV(TMP,NROWS,NVD)
      ISTB=IETB+1
      IETB=ISTB+NVD-1
      TB(ISTB:IETB)=TMP(1:NVD)
C     ........................................................ Write bank words
      ISTB=IETB+1
      IETB=ISTB+7
      TB(ISTB:IETB)=', words '
C     .... Get # of digits of NWRDS and write NWRDS into character variable TMP
      CALL DZWRCV(TMP,NWRDS,NVD)
      ISTB=IETB+1
      IETB=ISTB+NVD-1
      TB(ISTB:IETB)=TMP(1:NVD)
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZWRCV
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZWRCV
CH
      SUBROUTINE DZWRCV(T,NV,NVD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                    15-DEC-1991
C
C!:  Get the # of digits of NV and write NV into character variable T
C    Inputs    :
C            NV: Integer variable
C    Outputs   :
C           NVD: The # of digits of NV
C            T : character variable
C    Called by : DZHEAD
C ---------------------------------------------------------------------
      CHARACTER *(*)T
      CHARACTER *4 TF(9)
      DATA TF/
     &  '(I1)',
     &  '(I2)',
     &  '(I3)',
     &  '(I4)',
     &  '(I5)',
     &  '(I6)',
     &  '(I7)',
     &  '(I8)',
     &  '(I9)'/
C     .................................................... Get the digits of NV
      CALL DZGTDG(NV,NVD)
      WRITE(T(1:NVD),TF(NVD))NV
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZLBKA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZLBKA
CH
      SUBROUTINE DZLBKA(NTLEN,NCOLS,TITL,TFMD,NUMRW,NUMID,NDG,
     &NRDG,TBNK,NWRDS,TALPH)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  list a set of bank with same name and different number
C    Inputs    : The all input variables are translated by common block
C        NTLEN : The width of one line for listing bank
C        NUMID : Array
C                NUMID(1,I)=INDEX of the bank for I-th row
C                NUMID(2,I)=NUMBER of the bank for I-th row
C        NUMRW : The number of total rows
C         TBNK : The bank name
C        NWRDS : The total words # of the bank
C        NCOLS : The total numbers of columns in the bank
C       TALPH : Is a flag
C                TALPH='ALEPH' means this bank is ALEPH bank
C                TALPH='BOS  ' means this bank is BOS  bank
C          NDG : The maxmum digits of the bank numbers
C         NRDG : The maxmum digits of the banks's rows
C         TITL : The column's titles
C         TFMD : Column's format
C    Outputs   : The all output variables are translated by common block
C    Called by : DZLISA
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      PARAMETER(NFMTL=5000)
      COMMON/DZXULB/INDCDZ(NFMTL),MARKDZ(NFMTL),NCSTDZ
      COMMON/DZLIN/NDO,NREND
      CHARACTER *42 THELP(22)
      CHARACTER *49 T49
      CHARACTER *5 TALPH
      CHARACTER *1 TFMD(NCOLS),TCOM
      CHARACTER *4 TBNK
      CHARACTER *2 TITL(NCOLS)
      DIMENSION  NRMX(NFMTL),NRMN(NFMTL),NRVN(NFMTL),NDIG(NFMTL)
      DIMENSION NUMID(2,NUMRW)
      CHARACTER *1 TMP(-1:1)
      DATA TMP/'-','?','+'/
      DATA NDLEN/8/
      DATA  NCSTDZ/1/

      DATA NDIGT/6/
      DATA THELP/
     &    'Com.| Explanation                   | e.g.',
     &    '----+-------------------------------+-----',
     &    ' L# |List bank beginnig with line # | L32 ',
     &    '    |For single banks line #= row # |     ',
     &    ' C# |LIST bank beginnig with col.#  | C13 ',
     &    '<CR>|Continue listing               | <CR>',
     &    ' +# |Roll # rows down               | +9  ',
     &    ' -# |Roll # rows up                 | -2  ',
     &    ' N# |Roll # rows up/down            | N12 ',
     &    '  B | List backward                 | B   ',
     &    '  F | List forward                  | F   ',
     &    ' W# |# of digits of "reals" shown.  | W6  ',
     &    ' T# |Type title beginning with col.#| T6  ',
     &    ' D# |Angle in col.# set to degrees  | D5  ',
     &    ' D0 |All angles set to radians      | D0  ',
     &    ' M  |Get maximum & minimum          | M   ',
     &    ' M# |Max. & min. from col.# onwards | M3  ',
     &    ' V# |Get row with nearest value     | V92 ',
     &    ' ~# |Give a mark "~" to colomn #    | ~7  ',
     &    ' ~0 |Erase mark "~" to all colomn # | ~0  ',
     &    ' H  |Help                           | H   ',
     &    ' S  |STOP  =  GO BACK               | S   '/
      MTLEN=NTLEN-NDLEN
C     ................. Get the maxmum digits, the maxmum value and it's row #,
C     ................. the minimum value and it's row # of every column
      CALL DZGNDA(MTLEN,TALPH,NCOLS,NDIGT,NDIG,TFMD,NCFLG,
     &  NRMX,NRMN,NUMRW,NUMID,NDG,NRDG)
      NRSTA=1
C     ................................... NRL is the up limit # of printed rows
      NRL=15
C     ................................... If NCT=1 list the column #
      NCT=1
      NDO=1
      GO TO 936
C     ....... Read user commands: TCOM = 1 Letter command, NUM = Integer number
  666 CALL DLOPER(TCOM,GNUM)
C     ..................................... '~#' : Give a mark '~' to colomn #
C     .................................... '~0' : Erase mark '~' to all colomn
      NUM=GNUM
      IF(TCOM.EQ.'~')THEN
        IC=NUM
        IF(IC.EQ.0)THEN
          DO I=1,NCOLS
           MARKDZ(I)=0
          END DO
        ELSE IF(IC.LE.NCOLS.AND.IC.GE.1)THEN
          MARKDZ(IC)=1
        ELSE
          GO TO 935
        END IF
        NCT=1
        IF(IC.EQ.0)GO TO 935
        IF(IC.GE.NCSTDZ.AND.IC.LE.NCEND)GO TO 935
        NCSTDZ=IC
        GO TO 936
      END IF
C     ........................ 'D#': change from radian to degree for Col.# and
C     .........................  'D0' meams All columns (angle) reset to radian
      CALL CLTOU(TCOM)
      IF(TCOM.EQ.'D')THEN
        IC=NUM
        IF(IC.EQ.0)THEN
          DO I=1,NCOLS
           INDCDZ(I)=0
          END DO
        ELSE IF(IC.LE.NCOLS.AND.IC.GE.1.AND.TFMD(IC).EQ.'F')THEN
          INDCDZ(IC)=1
        ELSE
          GO TO 935
        END IF
C     ................. Get the maxmum digits, the maxmum value and it's row #,
C     ................. the minimum value and it's row # of every column
        CALL DZGNDA(MTLEN,TALPH,NCOLS,NDIGT,NDIG,TFMD,NCFLG,
     &    NRMX,NRMN,NUMRW,NUMID,NDG,NRDG)
        NCT=1
        IF(IC.EQ.0)GO TO 935
        IF(IC.GE.NCSTDZ.AND.IC.LE.NCEND)GO TO 935
        NCSTDZ=IC
        GO TO 936
      END IF
C     ................................... 'W#': set # of digits of real data
      IF(TCOM.EQ.'W')THEN
        NDIGT=NUM
        NDIGT=MAX(2,MIN(13,NDIGT))
        IF(NDIGT.GE.10)NDIGT=13
C       ............... Get the maxmum digits, the maxmum value and it's row #,
C       ............... the minimum value and it's row # of every column
        CALL DZGNDA(MTLEN,TALPH,NCOLS,NDIGT,NDIG,TFMD,NCFLG,
     &    NRMX,NRMN,NUMRW,NUMID,NDG,NRDG)
        NCT=1
        GO TO 936
      END IF
C     ..................................'L#': list bank beginnig with row #
      IF(TCOM.EQ.'L')THEN
        NRSTA=NUM
        NCT=0
        GO TO 938
      END IF
C     ................................... '+#': list # rows of bank forward
      IF(TCOM.EQ.'+'.OR.TCOM.EQ.'=')THEN
        NCT=0
        NRL=NUM
        NRSTA=NREND+1
        GO TO 938
      END IF
C     ................................... '-#': list # rows of bank backward
      IF(TCOM.EQ.'-'.OR.TCOM.EQ.'_')THEN
        NCT=0
        NRL=NUM
        NRL=-NRL
        NRSTA=NREND-1
        GO TO 938
      END IF
C     ................................... <CR>: continue listing
      IF(TCOM.EQ.' ') GO TO 935
C     ................................... 'F': list forward
      IF(TCOM.EQ.'F')THEN
        NRL=SIGN(NRL,1)
        GO TO 935
      END IF
C     ................................... 'B': list BACKward
      IF(TCOM.EQ.'B')THEN
        NRL=SIGN(NRL,-1)
        GO TO 935
      END IF
C     ................................... 'N#': list # rows of bank up or down
      IF(TCOM.EQ.'N')THEN
        NCT=0
        IF(NRL.GT.0)THEN
          NRL=NUM
          NRSTA=NREND+1
          GO TO 938
        ELSE
          NRL=-NUM
          NRSTA=NREND-1
          GO TO 938
        END IF
      END IF
C     .................................... 'H': list the help manual
      IF(TCOM.EQ.'H')THEN
        DO I=1,22
          CALL DWRT(THELP(I))
        END DO
        T49(1:49)='  H   | Help                                 | H'
     &  //TMP(NDO)
        CALL DWRT(T49)
        GO TO 666
      END IF
C     .................................... 'S': Stop listing of current bank
      IF(TCOM.EQ.'S')THEN
        RETURN
      END IF
C     ........................................... switch help/menue on or off.
      IF(TCOM.EQ.'<')THEN
        RETURN
      END IF
C     ............................................ 'M' : Get maximum & minimum
      IF(TCOM.EQ.'M')THEN
        IF(NUM.EQ.0)GO TO 670
        NCSTDZ=NUM
C       .................. Define the starting column # and the ending column #
  671   CALL DZCSEA(MTLEN,NCFLG,NCSTDZ,NCEND,NCOLS,NDIG,NDG,NRDG)
C       ..... list the maxmum and minimum value and their row # of every column
  670   CALL DZMXMA(TALPH,NUMRW,NUMID,MTLEN,NCSTDZ,NCEND,NCOLS,
     &    TITL,INDCDZ,MARKDZ,NDIG,TFMD,NRMX,NRMN,NDO,NDG,NRDG)
        GO TO 666
      END IF
C     ................ 'C#': list the bank with beginnig  column # , e.g. 'C28'
      IF(TCOM.EQ.'C')THEN
        NCT=1
        IF(NUM.EQ.0)THEN
C         ......................... list the being listed columns's # and title
          CALL DZBTCA(MTLEN,NCSTDZ,NCEND,NUMRW,NCOLS,TITL,
     &      INDCDZ,MARKDZ,NDIG,NDO,NDG,NRDG,IWID,0)
          GO TO 666
        ELSE
          NCSTDZ=NUM
          GO TO 936
        END IF
      END IF
C     ................. 'V': Get the nearest value of # and its row# ,e.g. V90
      IF(TCOM.EQ.'V')THEN
        VX=GNUM
C       ........... Get the nearest value of VX and it's row # for every column
        CALL DZFDVA(VX,NRVN,NCOLS,TFMD,TALPH,NUMRW,NUMID)
C       ............... list the nearest value of VX and it's row # on terminal
        CALL DZWRVA(TALPH,NUMRW,NUMID,MTLEN,NCSTDZ,NCEND,NCOLS,TITL,
     &    INDCDZ,MARKDZ,NDIG,TFMD,NRVN,NDO,NDG,NRDG)
        GO TO 666
      END IF
C     .................................... 'T': list the column title
      IF(TCOM.EQ.'T')THEN
        IF(NUM.GT.0)THEN
          NCSTDZ=NUM
C         ................ Define the starting column # and the ending column #
          CALL DZCSEA(MTLEN,NCFLG,NCSTDZ,NCEND,NCOLS,NDIG,NDG,NRDG)
        END IF
C       ........................... list the being listed columns's # and title
        CALL DZBTCA(MTLEN,NCSTDZ,NCEND,NUMRW,NCOLS,TITL,
     &    INDCDZ,MARKDZ,NDIG,NDO,NDG,NRDG,IWID,0)
        GO TO 666
      END IF
C     ................................................ Continue roll up or down
  935 NRSTA=NREND+NDO
      NCT=0
      GO TO 938
C     .................... Define the starting column # and the ending column #
  936 CALL DZCSEA(MTLEN,NCFLG,NCSTDZ,NCEND,NCOLS,NDIG,NDG,NRDG)
C     ..................... NCT=1 : list the # and title of being listed colums
      IF(NCT.EQ.1)CALL DZBTCA(MTLEN,NCSTDZ,NCEND,NUMRW,NCOLS,TITL,
     &  INDCDZ,MARKDZ,NDIG,NDO,NDG,NRDG,IWID,0)
C     ................... Adjust the starting row # and ending row # to be list
 938  IF(NRL.GT.0)THEN
C       ................................... NDO=1 Lisk the bank forward
        NDO=1
C       ...... NRMIN: minimum row # from which to last row can listd on terminal
        NRL=MAX(1,MIN(20,NRL))
        NRMIN=NUMRW-NRL+1
        NRSTA=MAX(1,MIN(NRMIN,NRSTA))
        NREND=MIN((NRL+NRSTA-1),NUMRW)
      ELSE
C       .................................... NDO=-1 list bank backward
        NDO=-1
C       ....... NRMIN: maxmum row # from which to 1st row can listd on terminal
        NRL=MIN(-1,MAX(-20,NRL))
        NRMIN=-NRL
        NRSTA=MIN(NUMRW,MAX(NRMIN,NRSTA))
        NREND=MAX((NRL+NRSTA+1),1)
      END IF
C     ................................... list bank with column # from NCSTDZ to
C     ..................................... NCEND and row # from NRSTA to NREND
      CALL DZLBTA(MTLEN,NRSTA,NREND,NCEND,NDO,TALPH,
     &  NUMRW,NUMID,NDG,NRDG,NCOLS,NDIG,TFMD)
      GO TO 666
      ENTRY DZLGLI(NOUT,NDIR)
      NOUT=NREND
      NDIR=NDO
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZLBNK
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZLBNK
CH
      SUBROUTINE DZLBNK(NTLEN,TBNK,IBNK,NCOLS,TITL,TFMD,NWRDS,
     &  NROWS,TALPH)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!: List bank with it's single number
C    Inputs    : The all input variables are translated by common block
C          TBNK: The bank name
C         IBNK : The index of bank
C         NWRDS: The total words # of the bank
C        NCOLS : The total numbers of columns in the bank
C        NROWS : The total rows # of the bank
C       TALPH : Is a flag
C                TALPH='ALEPH' means this bank is ALEPH bank
C                TALPH='BOS  ' means this bank is BOS  bank
C         NTLEN: The width of one line for listing bank
C         TITL : The column's titles
C         TFMD : Column's format
C    Outputs   : The all output variables are translated by common block
C    Called by : DZLISB
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      PARAMETER(NFMTL=5000)
      COMMON/DZXULB/INDCDZ(NFMTL),MARKDZ(NFMTL),NCSTDZ
      COMMON/DZLIN/NDO,NREND
      CHARACTER *42 THELP(22)
      CHARACTER *49 T49
      CHARACTER *5 TALPH
      CHARACTER *1 TFMD(NCOLS),TCOM
      CHARACTER *4 TBNK
      CHARACTER *2 TITL(NCOLS)
      DIMENSION  NRMX(NFMTL),NRMN(NFMTL),NRVN(NFMTL),NDIG(NFMTL)
      CHARACTER *1 TMP(-1:1)
      DATA TMP/'-','?','+'/
      DATA NDLEN/8/
      DATA NDIGT/6/
      DATA THELP/
     &    'Com.| Explanation                   | e.g.',
     &    '----+-------------------------------+-----',
     &    ' L# |List bank beginnig with line # | L32 ',
     &    '    |For single banks line #= row # |     ',
     &    ' C# |LIST bank beginnig with col.#  | C13 ',
     &    '<CR>|Continue listing               | <CR>',
     &    ' +# |Roll # rows down               | +9  ',
     &    ' -# |Roll # rows up                 | -2  ',
     &    ' N# |Roll # rows up/down            | N12 ',
     &    '  B | List backward                 | B   ',
     &    '  F | List forward                  | F   ',
     &    ' W# |# of digits of "reals" shown.  | W6  ',
     &    ' T# |Type title beginning with col.#| T6  ',
     &    ' D# |Angle in col.# set to degrees  | D5  ',
     &    ' D0 |All angles set to radians      | D0  ',
     &    ' M  |Get maximum & minimum          | M   ',
     &    ' M# |Max. & min. from col.# onwards | M3  ',
     &    ' V# |Get row with nearest value     | V92 ',
     &    ' ~# |Give a mark "~" to colomn #    | ~7  ',
     &    ' ~0 |Erase mark "~" to all colomn # | ~0  ',
     &    ' H  |Help                           | H   ',
     &    ' S  |STOP  =  GO BACK               | S   '/
      MTLEN=NTLEN-NDLEN
C     ................. Get the maxmum digits, the maxmum value and it's row #,
C     ................. the minimum value and it's row # of every column
      CALL DZGTND(MTLEN,IBNK,TALPH,NROWS,NCOLS,NDIGT,NDIG,TFMD,
     &  NCFLG,NRMX,NRMN)
      NRSTA=1
C     ................................... NRL is the up limit # of printed rows
      NRL=15
C     ................................... If NCT=1 list the column #
      NCT=1
      NDO=1
      GO TO 936
C     ....... Read user commands: TCOM = 1 Letter command, NUM = Integer number
  666 CALL DLOPER(TCOM,GNUM)
      NUM=GNUM
C     ..................................... '~#' : Give a mark '~' to colomn #
C     .................................... '~0' : Erase mark '~' to all colomn
      IF(TCOM.EQ.'~')THEN
        IC=NUM
        IF(IC.EQ.0)THEN
          DO I=1,NCOLS
           MARKDZ(I)=0
          END DO
        ELSE IF(IC.LE.NCOLS.AND.IC.GE.1)THEN
          MARKDZ(IC)=1
        ELSE
          GO TO 935
        END IF
        NCT=1
        IF(IC.EQ.0)GO TO 935
        IF(IC.GE.NCSTDZ.AND.IC.LE.NCEND)GO TO 935
        NCSTDZ=IC
        GO TO 936
      END IF
C     ........................ 'D#': change from radian to degree for Col.# and
C     .........................  'D0' meams All columns (angle) reset to radian
      CALL CLTOU(TCOM)
      IF(TCOM.EQ.'D')THEN
        IC=NUM
        IF(IC.EQ.0)THEN
          DO I=1,NCOLS
           INDCDZ(I)=0
          END DO
        ELSE IF(IC.LE.NCOLS.AND.IC.GE.1.AND.TFMD(IC).EQ.'F')THEN
          INDCDZ(IC)=1
        ELSE
          GO TO 935
        END IF
C       ............... Get the maxmum digits, the maxmum value and it's row #,
C       ............... the minimum value and it's row # of every column
        CALL DZGTND(MTLEN,IBNK,TALPH,NROWS,NCOLS,NDIGT,NDIG,TFMD,
     &    NCFLG,NRMX,NRMN)
        NCT=1
        IF(IC.EQ.0)GO TO 935
        IF(IC.GE.NCSTDZ.AND.IC.LE.NCEND)GO TO 935
        NCSTDZ=IC
        GO TO 936
      END IF
C     ...................................... 'W#': set # of digits of real data
      IF(TCOM.EQ.'W')THEN
        NDIGT=NUM
C       ........................... Get the maxmum # of digits for every column
        NDIGT=MAX(2,MIN(13,NDIGT))
        IF(NDIGT.GE.10)NDIGT=13
C       ............... Get the maxmum digits, the maxmum value and it's row #,
C       ............... the minimum value and it's row # of every column
        CALL DZGTND(MTLEN,IBNK,TALPH,NROWS,NCOLS,NDIGT,NDIG,TFMD,
     &    NCFLG,NRMX,NRMN)
        NCT=1
        GO TO 936
      END IF
C     ..................................'L#': list bank beginnig with row #
      IF(TCOM.EQ.'L')THEN
C        READ(TA(2:LTA),1003,ERR=201)NR
        NRSTA=NUM
        NCT=0
        GO TO 938
      END IF
C     ................................... '+#': list # rows of bank forward
      IF(TCOM.EQ.'+'.OR.TCOM.EQ.'=')THEN
        NCT=0
C        READ(TA(2:LTA),1003,ERR=203)NR
        NRL=NUM
        NRSTA=NREND+1
        GO TO 938
      END IF
C     ................................... '-#': list # rows of bank backward
      IF(TCOM.EQ.'-'.OR.TCOM.EQ.'_')THEN
        NCT=0
C        READ(TA(2:LTA),1003,ERR=204)NR
        NRL=NUM
        NRL=-NRL
        NRSTA=NREND-1
        GO TO 938
      END IF
C     ................................... <CR>: continue listing
      IF(TCOM.EQ.' ') GO TO 935
C     ................................... 'F': list forward
      IF(TCOM.EQ.'F')THEN
        NRL=SIGN(NRL,1)
        GO TO 935
      END IF
C     ................................... 'B': list BACKward
      IF(TCOM.EQ.'B')THEN
        NRL=SIGN(NRL,-1)
        GO TO 935
      END IF
      IF(TCOM.EQ.' '.AND.NUM.EQ.0)THEN
        GO TO 935
      END IF
C     ................................... 'N#': list # rows of bank up or down
      IF(TCOM.EQ.'N')THEN
        NCT=0
        IF(NRL.GT.0)THEN
          NRL=NUM
          NRSTA=NREND+1
          GO TO 938
        ELSE
          NRL=-NUM
          NRSTA=NREND-1
          GO TO 938
        END IF
      END IF
C     .................................... 'H': list the help manual
      IF(TCOM.EQ.'H')THEN
        DO I=1,22
          CALL DWRT(THELP(I))
        END DO
        T49(1:49)='  H   | Help                                 | H'
     &  //TMP(NDO)
        CALL DWRT(T49)
        GO TO 666
      END IF
C     .................................... 'S': Stop listing of current bank
      IF(TCOM.EQ.'S')THEN
        RETURN
      END IF
C     ........................................... switch help/menue on or off.
      IF(TCOM.EQ.'<')THEN
        RETURN
      END IF
C     ............................................ 'M' : Get maximum & minimum
      IF(TCOM.EQ.'M')THEN
        IF(NUM.EQ.0)GO TO 670
        NCSTDZ=NUM
C       .................. Define the starting column # and the ending column #
  671   CALL DZNCSE(MTLEN,NCFLG,NCSTDZ,NCEND,NCOLS,NDIG)
C       ..... list the maxmum and minimum value and their row # of every column
  670   CALL DZMXMN(TALPH,IBNK,MTLEN,NCSTDZ,NCEND,NROWS,NCOLS,
     &    TITL,INDCDZ,MARKDZ,NDIG,TFMD,NRMX,NRMN,NDO)
        GO TO 666
      END IF
C     ................ 'C#': list the bank with beginnig  column # , e.g. 'C28'
      IF(TCOM.EQ.'C')THEN
        NCT=1
        IF(NUM.EQ.0)THEN

C         ......................... list the being listed columns's # and title
          CALL DZBTNC(MTLEN,NROWS,NCOLS,TITL,INDCDZ,MARKDZ,NCSTDZ,
     &      NCEND,NDIG,NDO)
          GO TO 666
        ELSE
          NCSTDZ=NUM
          GO TO 936
        END IF
      END IF
C     ................. 'V': Get the nearest value of # and its row# ,e.g. V90.
      IF(TCOM.EQ.'V')THEN
        IF(NUM.EQ.0)GO TO 666
        VX=GNUM
C       ........... Get the nearest value of VX and it's row # for every column
        CALL DZFDVL(VX,NRVN,IBNK,NCOLS,TFMD,NROWS,TALPH)
C       ............... list the nearest value of VX and it's row # on terminal
        CALL DZWRVL(TALPH,IBNK,MTLEN,NCSTDZ,NCEND,NROWS,NCOLS,TITL,
     &    INDCDZ,MARKDZ,NDIG,TFMD,NRVN,NDO)
        GO TO 666
      END IF
C     .................................... 'T': list the column title
      IF(TCOM.EQ.'T')THEN
        IF(NUM.GT.0)THEN
          NCSTDZ=NUM
C         ................ Define the starting column # and the ending column #
          CALL DZNCSE(MTLEN,NCFLG,NCSTDZ,NCEND,NCOLS,NDIG)
        END IF
C       ........................... list the being listed columns's # and title
        CALL DZBTNC(MTLEN,NROWS,NCOLS,TITL,INDCDZ,MARKDZ,NCSTDZ,
     &    NCEND,NDIG,NDO)
        GO TO 666
      END IF
C     ................................................ Continue roll up or down
  935 NRSTA=NREND+NDO
      NCT=0
      GO TO 938
C     ...................  Define the starting column # and the ending column #
  936 CALL DZNCSE(MTLEN,NCFLG,NCSTDZ,NCEND,NCOLS,NDIG)
C     ..................... NCT=1 : list the # and title of being listed colums
      IF(NCT.EQ.1)CALL DZBTNC(MTLEN,NROWS,NCOLS,TITL,INDCDZ,MARKDZ,
     &  NCSTDZ,NCEND,NDIG,NDO)
C     ................... Adjust the starting row # and ending row # to be list
 938  IF(NRL.GT.0)THEN
C       ........................................... NDO=1 Lisk the bank forward
        NDO=1
C       ...... NRMIN: minimum row # from which to last row can listd on terminal
        NRL=MAX(1,MIN(20,NRL))
        NRMIN=NROWS-NRL+1
        NRSTA=MAX(1,MIN(NRMIN,NRSTA))
        NREND=MIN((NRL+NRSTA-1),NROWS)
      ELSE
C       ............................................. NDO=-1 list bank backward
        NDO=-1
C       ....... NRMIN: maxmum row # from which to 1st row can listd on terminal
        NRL=MIN(-1,MAX(-20,NRL))
        NRMIN=-NRL
        NRSTA=MIN(NROWS,MAX(NRMIN,NRSTA))
        NREND=MAX((NRL+NRSTA+1),1)
      END IF
C     ................................... list bank with column # from NCSTDZ to
C     ..................................... NCEND and row # from NRSTA to NREND
      CALL DZLBTP(MTLEN,NROWS,NRSTA,NREND,NCEND,
     &  NDO,IBNK,TALPH,NCOLS,NDIG,TFMD)
      GO TO 666
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZLSBA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZLSBA
CH
      SUBROUTINE DZLSBA(NTLEN,NCOLS,TITL,TFMD,NUMRW,NUMID,NDG,
     &NRDG,TBNK,NWRDS,TALPH,N1,N2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  list a set of bank with same name and different number on above window.
C    Inputs    : The all input variables are translated by common block
C        NTLEN : The width of one line for listing bank
C        NUMID : Array
C                NUMID(1,I)=INDEX of the bank for I-th row
C                NUMID(2,I)=NUMBER of the bank for I-th row
C        NUMRW : The number of total rows
C         TBNK : The bank name
C        NWRDS : The total words # of the bank
C        NCOLS : The total numbers of columns in the bank
C       TALPH : Is a flag
C                TALPH='ALEPH' means this bank is ALEPH bank
C                TALPH='BOS  ' means this bank is BOS  bank
C          NDG : The maxmum digits of the bank numbers
C         NRDG : The maxmum digits of the banks's rows
C         TITL : The column's titles
C         TFMD : Column's format
C           N1 : The number of the first line to be list
C           N2 : The number of the last line to be list
C    Outputs   : The all output variables are translated by common block
C    Called by : DZLKBA
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      PARAMETER(NFMTL=5000)
      COMMON/DZXULB/INDCDZ(NFMTL),MARKDZ(NFMTL),NCSTDZ
      COMMON/DZLIN/NDO,NREND
      CHARACTER *5 TALPH
      CHARACTER *1 TFMD(NCOLS)
      CHARACTER *4 TBNK
      CHARACTER *2 TITL(NCOLS)
      DIMENSION  NRMX(NFMTL),NRMN(NFMTL),NDIG(NFMTL)
      DIMENSION NUMID(2,NUMRW)
      DATA NDLEN/8/
      MTLEN=NTLEN-NDLEN
C     ................. Get the maxmum digits, the maxmum value and it's row #,
C     ................. the minimum value and it's row # of every column
      CALL DZGNDA(MTLEN,TALPH,NCOLS,NDIGT,NDIG,TFMD,NCFLG,
     &  NRMX,NRMN,NUMRW,NUMID,NDG,NRDG)
C     .................... Define the starting column # and the ending column #
      CALL DZCSEA(MTLEN,NCFLG,NCSTDZ,NCEND,NCOLS,NDIG,NDG,NRDG)
C     ..................... List the # and title of being listed colums
      CALL DZBTCA(MTLEN,NCSTDZ,NCEND,NUMRW,NCOLS,TITL,
     &  INDCDZ,MARKDZ,NDIG,NDO,NDG,NRDG,IWID,0)
C     ................... Adjust the starting row # and ending row # to be list
      IF(NDO.GT.0)THEN
C       ................................... NDO=1 Lisk the bank forward
        NRSTA=MAX(1,N1)
        NREND=MIN(N2,NUMRW)
      ELSE
C       .................................... NDO=-1 list bank backward
        NRSTA=MIN(NUMRW,N1)
        NREND=MAX(N2,1)
      END IF
C     ................................... list bank with column # from NCSTDZ to
C     ..................................... NCEND and row # from NRSTA to NREND
      CALL DZLBTA(MTLEN,NRSTA,NREND,NCEND,NDO,TALPH,
     &  NUMRW,NUMID,NDG,NRDG,NCOLS,NDIG,TFMD)
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZLSBN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZLSBN
CH
      SUBROUTINE DZLSBN(NTLEN,TBNK,IBNK,NCOLS,TITL,TFMD,NWRDS,
     &  NROWS,TALPH,N1,N2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!: List bank with it's single number on above window.
C    Inputs    : The all input variables are translated by common block
C          TBNK: The bank name
C         IBNK : The index of bank
C         NWRDS: The total words # of the bank
C        NCOLS : The total numbers of columns in the bank
C        NROWS : The total rows # of the bank
C       TALPH : Is a flag
C                TALPH='ALEPH' means this bank is ALEPH bank
C                TALPH='BOS  ' means this bank is BOS  bank
C         NTLEN: The width of one line for listing bank
C         TITL : The column's titles
C         TFMD : Column's format
C           N1 : The number of the first line to be list
C           N2 : The number of the last line to be list
C    Outputs   : The all output variables are translated by common block
C    Called by : DZLKBN
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      PARAMETER(NFMTL=5000)
      COMMON/DZXULB/INDCDZ(NFMTL),MARKDZ(NFMTL),NCSTDZ
      COMMON/DZLIN/NDO,NREND
      CHARACTER *5 TALPH
      CHARACTER *1 TFMD(NCOLS)
      CHARACTER *4 TBNK
      CHARACTER *2 TITL(NCOLS)
      DIMENSION  NRMX(NFMTL),NRMN(NFMTL),NDIG(NFMTL)
      DATA NDLEN/8/
      MTLEN=NTLEN-NDLEN
C     ................. Get the maxmum digits, the maxmum value and it's row #,
C     ................. the minimum value and it's row # of every column
      CALL DZGTND(MTLEN,IBNK,TALPH,NROWS,NCOLS,NDIGT,NDIG,TFMD,
     &  NCFLG,NRMX,NRMN)
C     ...................  Define the starting column # and the ending column #
      CALL DZNCSE(MTLEN,NCFLG,NCSTDZ,NCEND,NCOLS,NDIG)
C     ..................... List the # and title of being listed colums
      CALL DZBTNC(MTLEN,NROWS,NCOLS,TITL,INDCDZ,MARKDZ,
     &  NCSTDZ,NCEND,NDIG,NDO)
C     ................... Adjust the starting row # and ending row # to be list
      IF(NDO.GT.0)THEN
C       ................................... NDO=1 Lisk the bank forward
        NRSTA=MAX(1,N1)
        NREND=MIN(N2,NROWS)
      ELSE
C       .................................... NDO=-1 list bank backward
        NRSTA=MIN(NROWS,N1)
        NREND=MAX(N2,1)
      END IF
C     ................................... list bank with column # from NCSTDZ to
C     ..................................... NCEND and row # from NRSTA to NREND
      CALL DZLBTP(MTLEN,NROWS,NRSTA,NREND,NCEND,
     &  NDO,IBNK,TALPH,NCOLS,NDIG,TFMD)
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZLBTP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZLBTP
CH
      SUBROUTINE DZLBTP(MTLEN,NROWS,NRSTA,NREND,NCEND,
     &NDO,IBNK,TALPH,NCOLS,NDIG,TFMD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  list bank with column # from NCSTDZ to NCEND and row # from NRSTA to NREND
C    Inputs    :
C        MTLEN : The width of one line for listing bank
C        NCOLS : The total numbers of columns in the bank
C        NROWS : The total rows # of the bank
C        NRSTA : The starting row #
C        NREND : The ending row #
C        NDO   : Translated by common block
C                NDO=1 Typing the bank forward
C                NDO=-1 Typing the bank backward
C       NCSTDZ : The starting column #
C        NCEND : The ending column #
C        IBNK  : The index of bank translated by common block
C       TALPH : Is a flag
C                TALPH='ALEPH' means this bank is ALEPH bank
C                TALPH='BOS  ' means this bank is BOS  bank
C         NDIG : The # of digits for every column of the bank
C        TFMD  : Column's format translated by common block
C    Outputs   :
C    Called by : DZLBNK
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      PARAMETER(NFMTL=5000)
      COMMON/DZXULB/INDCDZ(NFMTL),MARKDZ(NFMTL),NCSTDZ
      CHARACTER *5 TALPH
      CHARACTER *1 TFMD(NCOLS)
      CHARACTER *20 TT,T1,T2
      CHARACTER *4 CHAINT,CTABL,CBTABL,T
      CHARACTER *140 TW,TWBNK
      DIMENSION NDIG(NCOLS)
      CHARACTER *1 TMP(-1:1)
      DATA TMP/'-',':','+'/
      CHARACTER *4 TF(9)
      DATA TF/
     &  '(I1)',
     &  '(I2)',
     &  '(I3)',
     &  '(I4)',
     &  '(I5)',
     &  '(I6)',
     &  '(I7)',
     &  '(I8)',
     &  '(I9)'/
      CHARACTER *7 TG(2:9)
      DATA TG/
     &  '(G13.1)',
     &  '(G13.2)',
     &  '(G13.3)',
     &  '(G13.4)',
     &  '(G13.5)',
     &  '(G13.6)',
     &  '(G13.7)',
     &  '(G13.8)'/
      INCLUDE 'A_BMACRO.INC'

CC - Lth character element of the NRth row of the bank with index ID
CC   (ALEPH bank)
C      CTABL(ID,NR,L) = CHAINT(IW(ID+LMHLEN+(NR-1)*IW(ID+1)+L))
CC - Lth character element of the NRth row of the bank with index ID (BOS bank)
C      CBTABL(ID,NR,L) = CHAINT(IW(ID+(NR-1)*IW(ID+1)+L))
CC - Lth integer element of the NRth row of the bank with index ID (BOS bank)
C      IBTABL(ID,NR,L) = IW(ID+(NR-1)*IW(ID+1)+L)
CC - Lth real element of the NRth row of the bank with index ID (BOS bank)
C      RBTABL(ID,NR,L) = RW(ID+(NR-1)*IW(ID+1)+L)

C - Lth character element of the NRth row of the bank with index ID (ALEPH bank)
      CTABL(JD,JR,JL) = CHAINT(IW(JD+LMHLEN+(JR-1)*IW(JD+1)+JL))
C - Lth character element of the NRth row of the bank with index ID (BOS bank)
      CBTABL(JD,JR,JL) = CHAINT(IW(JD+(JR-1)*IW(JD+1)+JL))
C - Lth integer element of the NRth row of the bank with index ID (BOS bank)
      IBTABL(JD,JR,JL) = IW(JD+(JR-1)*IW(JD+1)+JL)
C - Lth real element of the NRth row of the bank with index ID (BOS bank)
      RBTABL(JD,JR,JL) = RW(JD+(JR-1)*IW(JD+1)+JL)

C
C     ................................................. Get the digits of NROWS
C     ....................................... NDK: the maxmun digits of row #
      CALL DZGTDG(NROWS,NDK)
      NDK=MAX(4,NDK)
      DO K=NRSTA,NREND,NDO
        TW=' '
        ISTA=4+NDK
        DO I=NCSTDZ,NCEND
C         ................................ Put mark '~' in the assigned column
          IF(MARKDZ(I).EQ.1)THEN
            TW((ISTA-1):(ISTA-1))='~'
          END IF
C         ................................ Put mark '^' in the assigned column
C         ................................ in which angle to be set to degrees
          IF(INDCDZ(I).EQ.1)THEN
            TW((ISTA-1):(ISTA-1))='^'
          END IF
          IF(TFMD(I).EQ.'I') THEN
C           ................................ Get Ith integer element of the Kth
C           ................................... row of the bank with index IBNK
            IF(TALPH.EQ.'ALEPH')THEN
              NX=ITABL(IBNK,K,I)
            ELSE
              NX=IBTABL(IBNK,K,I)
            END IF
C           ....................................... Get the digits NTDIG of NX
            CALL DZGTDG(NX,NTDIG)
            IF(NTDIG.LE.9)THEN
C             ............................ Write NX into TT
              WRITE(TT(2:NTDIG+1),TF(NTDIG))NX
            ELSE
C             ............................ If the # of digits of integer >9
C             ............................ Unpack it into two integer
              NX1=IFIX(FLOAT(NX)*0.00000001)
              NX2=NX-NX1*10**8
C             ............................ Write NX1 into T1
              CALL DZFITT(T1,NTDIG1,NX1)
C             ............................ Write NX2 into T2
              CALL DZFITT(T2,NTDIG2,ABS(NX2))
              TT(2:NTDIG+1)=T1(2:NTDIG1+1)//T2(2:NTDIG2+1)
            END IF
C           .............................. Put TT in the suitable position of
C           .............................. TW to indent upto right for integer
            IEND=ISTA+NDIG(I)-1
            ISTA=IEND-NTDIG+1
            TW(ISTA:IEND+1)=TT(2:(NTDIG+1))//'|'
            IF(I.EQ.NCOLS)TW(IEND+1:IEND+1)=']'
            ISTA=IEND+3
          ELSE IF(TFMD(I).EQ.'F') THEN
C           .............................. Get Ith real element of the Kth
C           .............................. row of the bank with index IBNK
            IF(TALPH.EQ.'ALEPH')THEN
              X=RTABL(IBNK,K,I)
            ELSE
              X=RBTABL(IBNK,K,I)
            END IF
            IF(INDCDZ(I).EQ.1)X=X*180./3.14159
            IF(NDIG(I).LT.10)THEN
C             .................change floating point number X to dali format TT
              CALL DTN(X,NDIG(I),TT)
C             ........................... Put TT in the suitable position of TW
C             ..................... to indent upto left for floating point data
              DO J=1,NDIG(I)
                IF(TT(J:J).EQ.'.')THEN
                  NEND=3
                  IF(TT(1:1).EQ.'-')NEND=4
                  GO TO 555
                END IF
              END DO
              GO TO 444
  555         DO J=NDIG(I),NEND,-1
C               ..................... Delet the '0' in the end of floating data
                IF(TT(J:J).NE.'0')GO TO 444
                TT(J:J)=' '
              END DO
            ELSE
              WRITE(TT,TG(8))X
            END IF
 444        IEND=ISTA+NDIG(I)-1
            TW(ISTA:IEND)=TT(1:NDIG(I))
            IEND=IEND+1
            TW(IEND:IEND)='|'
            IF(I.EQ.NCOLS)TW(IEND:IEND)=']'
            ISTA=IEND+2
          ELSE
            T='    '
C           .............................. Get Ith character element of the
C           .............................. Kth row of the bank with index IBNK
            IF(TALPH.EQ.'ALEPH')THEN
              T=CTABL(IBNK,K,I)
            ELSE
              T=CBTABL(IBNK,K,I)
            END IF
            IEND=ISTA+NDIG(I)
            TW(ISTA:IEND)=T//'|'
            ISTA=IEND+2
            IF(I.EQ.NCOLS)TW(IEND:IEND)=']'
          END IF
        END DO
C       .................................. Write row # K into TT (character)
C       .................................. and get the # of digits NTDIG of K
        CALL DZFITT(TT,NTDIG,K)
C       ....................... Put the # of row in the suitable position of TW
        ISTA=2
        IEND=ISTA+NDK-1
        ISTA=IEND-NTDIG+1
        TW(1:ISTA-1)=' '
        TW(ISTA:IEND+1)=TT(2:NTDIG+1)//'|'
        IF(NCSTDZ.EQ.1)TW(IEND+1:IEND+1)='['
C       ........................ Setting a character variable TWBNK equal empty
        TWBNK=' '
        TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
C       ........................................... Write TWBNK on the terminal
        CALL DLWRBA(TWBNK(1:MTLEN+1))
      END DO
      IF(NREND.EQ.NROWS)THEN
      TW(1:1)=' '
        DO I=2,MTLEN
          TW(I:I)='-'
        END DO
C       ........................ Setting a character variable TWBNK equal empty
        TWBNK=' '
        TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
        CALL DLWRBA(TWBNK(1:MTLEN+1))
      END IF
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZLBTA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZLBTA
CH
      SUBROUTINE DZLBTA(MTLEN,NRSTA,NREND,NCEND,NDO,TALPH,
     &NUMRW,NUMID,NDG,NRDG,NCOLS,NDIG,TFMD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  list bank with total # and column # from NCSTDZ to NCEND and
C!:  row # from NRSTA to NREND
C    Inputs    :
C        MTLEN : The width of one line for listing bank
C        NCOLS : The total numbers of columns in the bank
C        NRSTA : The starting row #
C        NREND : The ending row #
C        NDO   : Translated by common block
C                NDO=1 Typing the bank forward
C                NDO=-1 Typing the bank backward
C        NCSTDZ : The starting column #
C        NCEND : The ending column #
C         NDIG : The # of digits for every column of the bank
C         TFMD : Column's format
C       TALPH : Is a flag
C                TALPH='ALEPH' means this bank is ALEPH bank
C                TALPH='BOS  ' means this bank is BOS  bank
C        NUMID : Array
C                NUMID(1,I)=INDEX of the bank for I-th row
C                NUMID(2,I)=NUMBER of the bank for I-th row
C        NUMRW : The number of total rows
C          NDG : The maxmum digits of the bank numbers
C         NRDG : The maxmum digits of the banks's rows
C    Outputs   :
C    Called by : DZLBKA
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      PARAMETER(NFMTL=5000)
      COMMON/DZXULB/INDCDZ(NFMTL),MARKDZ(NFMTL),NCSTDZ
      CHARACTER *5 TALPH
      CHARACTER *1 TFMD(NCOLS)
      CHARACTER *20 TT,T1,T2
      CHARACTER *4 CHAINT,CTABL,CBTABL,T
      CHARACTER *140 TW,TWBNK
      CHARACTER *1 TMP(-1:1)
      DATA TMP/'-',':','+'/
      DIMENSION NUMID(2,NUMRW),NDIG(NCOLS)
      CHARACTER *4 TF(9)
      DATA TF/
     &  '(I1)',
     &  '(I2)',
     &  '(I3)',
     &  '(I4)',
     &  '(I5)',
     &  '(I6)',
     &  '(I7)',
     &  '(I8)',
     &  '(I9)'/
      CHARACTER *7 TG(2:9)
      DATA TG/
     &  '(G13.1)',
     &  '(G13.2)',
     &  '(G13.3)',
     &  '(G13.4)',
     &  '(G13.5)',
     &  '(G13.6)',
     &  '(G13.7)',
     &  '(G13.8)'/
      INCLUDE 'A_BMACRO.INC'

C - Lth character element of the NRth row of the bank with index ID (ALEPH bank)
      CTABL(JD,JR,JL) = CHAINT(IW(JD+LMHLEN+(JR-1)*IW(JD+1)+JL))
C - Lth character element of the NRth row of the bank with index ID (BOS bank)
      CBTABL(JD,JR,JL) = CHAINT(IW(JD+(JR-1)*IW(JD+1)+JL))
C - Lth integer element of the NRth row of the bank with index ID (BOS bank)
      IBTABL(JD,JR,JL) = IW(JD+(JR-1)*IW(JD+1)+JL)
C - Lth real element of the NRth row of the bank with index ID (BOS bank)
      RBTABL(JD,JR,JL) = RW(JD+(JR-1)*IW(JD+1)+JL)
C
C     ................................................ Get the digits of NUMRW
C     ............................................ NDK: maxmun digits of row #
      CALL DZGTDG(NUMRW,NDK)
      NDK=MAX(4,NDK)
C      NDK=4
      DO K=NRSTA,NREND,NDO
        TW=' '
        ISTA=4+NDK+(NDG+NRDG+2)
        IBNK=NUMID(1,K)
        KR=NUMID(2,K)
        DO I=NCSTDZ,NCEND
C         ................................ Put mark '~' in the assigned column
          IF(MARKDZ(I).EQ.1)THEN
            TW((ISTA-1):(ISTA-1))='~'
          END IF
C         ................................ Put mark '^' in the assigned column
C         ................................ in which angle to be set to degrees
          IF(INDCDZ(I).EQ.1)THEN
            TW((ISTA-1):(ISTA-1))='^'
          END IF
          IF(TFMD(I).EQ.'I') THEN
C           ................................ Get Ith integer element of the Kth
C           ................................... row of the bank with index IBNK
            IF(TALPH.EQ.'ALEPH')THEN
              NX=ITABL(IBNK,KR,I)
            ELSE
              NX=IBTABL(IBNK,KR,I)
            END IF
C           .............................. Get # of digits NTDIG of NX
            CALL DZGTDG(NX,NTDIG)
            IF(NTDIG.LE.9)THEN
C             ............................ Write NX into TT
              WRITE(TT(2:NTDIG+1),TF(NTDIG))NX
            ELSE
C             ............................ If the # of digits of integer >9
C             ............................ Unpack it into two integer
              NX1=IFIX(FLOAT(NX)*0.00000001)
              NX2=NX-NX1*10**8
C             ............................ Write NX1 into T1
              CALL DZFITT(T1,NTDIG1,NX1)
C             ............................ Write NX2 into T2
              CALL DZFITT(T2,NTDIG2,ABS(NX2))
              TT(2:NTDIG+1)=T1(2:NTDIG1+1)//T2(2:NTDIG2+1)
            END IF
C           .............................. Put TT in the suitable position of
C           .............................. TW to indent upto right for integer
            IEND=ISTA+NDIG(I)-1
            ISTA=IEND-NTDIG+1
            TW(ISTA:IEND+1)=TT(2:(NTDIG+1))//'|'
            IF(I.EQ.NCOLS)TW(IEND+1:IEND+1)=']'
            ISTA=IEND+3
          ELSE IF(TFMD(I).EQ.'F') THEN
C           .............................. Get Ith real element of the Kth
C           .............................. row of the bank with index IBNK
            IF(TALPH.EQ.'ALEPH')THEN
              X=RTABL(IBNK,KR,I)
            ELSE
              X=RBTABL(IBNK,KR,I)
            END IF
            IF(INDCDZ(I).EQ.1)X=X*180./3.14159
            IF(NDIG(I).LT.10)THEN
C             .................change floating point number X to dali format TT
              CALL DTN(X,NDIG(I),TT)
C             ........................... Put TT in the suitable position of TW
C             ..................... to indent upto left for floating point data
              DO J=1,NDIG(I)
                IF(TT(J:J).EQ.'.')THEN
                  NEND=3
                  IF(TT(1:1).EQ.'-')NEND=4
                  GO TO 555
                END IF
              END DO
              GO TO 444
  555         DO J=NDIG(I),NEND,-1
C               ..................... Delet the '0' in the end of floating data
                IF(TT(J:J).NE.'0')GO TO 444
                TT(J:J)=' '
              END DO
            ELSE
              WRITE(TT,TG(8))X
            END IF
 444        IEND=ISTA+NDIG(I)-1
            TW(ISTA:IEND)=TT(1:NDIG(I))
            IEND=IEND+1
            TW(IEND:IEND)='|'
            IF(I.EQ.NCOLS)TW(IEND:IEND)=']'
            ISTA=IEND+2
          ELSE
            T='    '
C           .............................. Get Ith character element of the
C           .............................. Kth row of the bank with index IBNK
            IF(TALPH.EQ.'ALEPH')THEN
              T=CTABL(IBNK,KR,I)
            ELSE
              T=CBTABL(IBNK,KR,I)
            END IF
            IEND=ISTA+NDIG(I)
            TW(ISTA:IEND)=T//'|'
            ISTA=IEND+2
            IF(I.EQ.NCOLS)TW(IEND:IEND)=']'
          END IF
        END DO
C       .................................. Write row # K into TT (character)
C       .................................. and get the # of digits NTDIG of K
        CALL DZFITT(TT,NTDIG,K)
C       ....................... Put the # of row in the suitable position of TW
        ISTA=2
        IEND=ISTA+NDK-1
        ISTA=IEND-NTDIG+1
        TW(1:ISTA-1)=' '
        TW(ISTA:IEND+1)=TT(2:NTDIG+1)//'|'
C        IF(NCSTDZ.EQ.1)TW(IEND+1:IEND+1)='['
C       ................................. Write bank # NUM into TT (character)
C       .................................. and get the # of digits NTDIG of K
        NUM=IW(IBNK-2)
        CALL DZFITT(TT,NTDIG,NUM)
C       .................... Put the bank number in the suitable position of TW
        ISTA=IEND+2
        IEND=ISTA+NDG-1
        ISTA=IEND-NTDIG+1
        TW(ISTA:IEND+1)=TT(2:NTDIG+1)//'/'
C        IF(NCSTDZ.EQ.1)TW(IEND+1:IEND+1)='['
C       ................................ Write real row # K into TT (character)
C       .................................. and get the # of digits NTDIG of K
        CALL DZFITT(TT,NTDIG,KR)
C       ............... Put the real bank row # in the suitable position of TW
        ISTA=IEND+2
        IEND=ISTA+NRDG-1
        ISTA=IEND-NTDIG+1
        TW(ISTA:IEND+1)=TT(2:NTDIG+1)//'|'
        IF(NCSTDZ.EQ.1)TW(IEND+1:IEND+1)='['
C       ........................ Setting a character variable TWBNK equal empty
        TWBNK=' '
        TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
C       ........................................... Write TWBNK on the terminal
        CALL DLWRBA(TWBNK(1:MTLEN+1))
      END DO
      IF(NREND.EQ.NUMRW)THEN
      TW(1:1)=' '
        DO I=2,MTLEN
          TW(I:I)='-'
        END DO
        TWBNK=' '
        TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
        CALL DLWRBA(TWBNK(1:MTLEN+1))
      END IF
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZBTNC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZBTNC
CH
      SUBROUTINE DZBTNC(MTLEN,NROWS,NCOLS,TITL,INDCDZ,MARKDZ,NCSTDZ,
     &NCEND,NDIG,NDO)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  list the being listed columns's # and title of a bank with single number
C    Inputs    :
C        MTLEN: The width of one line for listing bank
C        NROWS : The total rows # of the bank
C        NCOLS : The total numbers of columns in the bank
C        NCSTDZ : The starting column #
C        NCEND : The ending column #
C         NDIG : The # of digits for every column of the bank
C         TITL : The column's titles
C          NDO : The flag translated by common block
C                NDO=1 Typing the bank forward
C                NDO=-1 Typing the bank backward
C    Outputs   :
C
C    Called by : DZLBNK
C ---------------------------------------------------------------------
      CHARACTER *2 TITL(NCOLS)
      CHARACTER *20 TT
C      CHARACTER *132 TW,TL,TWBNK
      CHARACTER *140 TW,TL,TWBNK
      DIMENSION INDCDZ(NCOLS),MARKDZ(NCOLS)
      DIMENSION NDIG(NCOLS)
      CHARACTER *1 TMP(-1:1)
      DATA TMP/'-',':','+'/
      CHARACTER *4 TF(9)
      DATA TF/
     &  '(I1)',
     &  '(I2)',
     &  '(I3)',
     &  '(I4)',
     &  '(I5)',
     &  '(I6)',
     &  '(I7)',
     &  '(I8)',
     &  '(I9)'/
      TW(1:1)=' '
      DO I=2,MTLEN
        TW(I:I)='-'
      END DO
      TWBNK=' '
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
      CALL DZGTDG(NROWS,NDK)
      NDK=MAX(4,NDK)
      ISTW=4+NDK
      ISTL=4+NDK
      TW=' '
      TL=' '
      TW(2+NDK:2+NDK)='|'
      TL(2+NDK:2+NDK)='|'
      IF(NCSTDZ.EQ.1)THEN
        TW(2+NDK:2+NDK)='['
        TL(2+NDK:2+NDK)='['
      END IF
      DO K=NCSTDZ,NCEND
C       ................................ Put mark '~' in the assigned column
        IF(MARKDZ(K).EQ.1)THEN
          TW((ISTW-1):(ISTW-1))='~'
          TL((ISTW-1):(ISTW-1))='~'
        END IF
C       ................................ Put mark '^' in the assigned column
C       ................................ in which angle to be set to degrees
        IF(INDCDZ(K).EQ.1)THEN
          TW((ISTW-1):(ISTW-1))='^'
          TL((ISTW-1):(ISTW-1))='^'
        END IF
C       ............................................  typing the column #
C       .................................. Write column # into TT and get
C       ..................................... the # of digits NTDIG of TT
        CALL DZGTDG(K,NTDIG)
        WRITE(TT(2:NTDIG+1),TF(NTDIG))K
C       .................................. Put the # or titles of columns in
C       .................................. the suitable position of TW
        IEND=ISTW+NDIG(K)
        ISTW=ISTW+NDIG(K)/2-1
        GO TO(1,1,2),NTDIG
    2   ISTW=ISTW-1
    1   N2=ISTW+NTDIG-1
        TW(ISTW:N2)=TT(2:NTDIG+1)
        TW(IEND:IEND)='|'
        ISTW=IEND+2
C       ............................................  typing the column title
        NTDIG=2
        TT(2:NTDIG+1)=TITL(K)
C       .................................. Put the # or titles of columns in
C       .................................. the suitable position of TL
        IETL=ISTL+NDIG(K)
        ISTL=ISTL+NDIG(K)/2-1
        GO TO(11,11,12),NTDIG
   12   ISTL=ISTL-1
   11   N2=ISTL+NTDIG-1
        TL(ISTL:N2)=TT(2:NTDIG+1)
        TL(IETL:IETL)='|'
        ISTL=IETL+2
      END DO
      IF(NCEND.EQ.NCOLS)THEN
        TW(IEND:IEND)=']'
        TL(IETL:IETL)=']'
      END IF
C     ........................... Typing # or titles of columns on the terminal
      TWBNK=' '
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
      TWBNK(1:MTLEN+1)=TL(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZBTCA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZBTCA
CH
      SUBROUTINE DZBTCA(MTLEN,NCSTDZ,NCEND,NUMRW,NCOLS,TITL,
     &INDCDZ,MARKDZ,NDIG,NDO,NDG,NRDG,IWID,MXF)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  list the being listed columns's # and title of a bank with total number
C    Inputs    :
C        MTLEN : The width of one line for listing bank
C        NUMRW : The number of total rows
C        NCOLS : The total numbers of columns in the bank
C        NCSTDZ : The starting column #
C        NCEND : The ending column #
C         NDIG : The # of digits for every column of the bank
C         TITL : The column's titles
C          NDO : The flag translated by common block
C                NDO=1 Typing the bank forward
C                NDO=-1 Typing the bank backward
C          NDG : The maxmum digits of the bank numbers
C         NRDG : The maxmum digits of the banks's rows
C          MXF :  MXF=0 If DZLBKA call DZCSEA, MXF=1 If DZMXMA call DZCSEA
C                 MXF=1 If DZMXMA and DZWRVA call DZCSEA
C    Outputs   :
C         IWID : =IEND-(NDG+NRDG+2)
C    Called by : DZLBKA, DZMXMA, DZWRVA
C ---------------------------------------------------------------------
      CHARACTER *2 TITL(NCOLS)
      CHARACTER *20 TT
      CHARACTER *140 TW,TL,TWBNK
      DIMENSION INDCDZ(NCOLS),MARKDZ(NCOLS)
      DIMENSION NDIG(NCOLS)
      CHARACTER *1 TMP(-1:1)
      DATA TMP/'-',':','+'/
      CHARACTER *4 TF(9)
      DATA TF/
     &  '(I1)',
     &  '(I2)',
     &  '(I3)',
     &  '(I4)',
     &  '(I5)',
     &  '(I6)',
     &  '(I7)',
     &  '(I8)',
     &  '(I9)'/
      TW(1:1)=' '
      DO I=2,MTLEN
        TW(I:I)='-'
      END DO
      TWBNK=' '
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
      CALL DZGTDG(NUMRW,NDK)
      NDK=MAX(4,NDK)
      ISTA=2+NDK+(NDG+NRDG+2)
      ISTL=2+NDK+(NDG+NRDG+2)
      TW=' '
      TL=' '
      IF(MXF.EQ.0)THEN
        TW(2+NDK:2+NDK)='|'
        ISTW=6+(NDG+2)/2-1
        TW(ISTW:ISTW+1)='NR'
        ISTW=6+(NDG+1)
        TW(ISTW:ISTW)='/'
        IETW=ISTW+(NRDG+2)/2-1
        TW(IETW:IETW+1)='R#'
        IETW=ISTW+(NRDG+1)
      END IF
      TW(ISTA:ISTA)='|'
      TL(ISTL:ISTL)='|'
      IF(NCSTDZ.EQ.1)THEN
        TW(ISTA:ISTA)='['
        TL(ISTL:ISTL)='['
      END IF
      ISTA=ISTA+2
      ISTL=ISTL+2
      DO K=NCSTDZ,NCEND
C       ................................ Put mark '~' in the assigned column
        IF(MARKDZ(K).EQ.1)THEN
          TW((ISTA-1):(ISTA-1))='~'
          TL((ISTL-1):(ISTL-1))='~'
        END IF
C       ................................ Put mark '^' in the assigned column
C       ................................ in which angle to be set to degrees
        IF(INDCDZ(K).EQ.1)THEN
          TW((ISTA-1):(ISTA-1))='^'
          TL((ISTL-1):(ISTL-1))='^'
        END IF
C       ................................................. typing the column #
C       ...................................... Write column # into TT and get
C       ......................................... the # of digits NTDIG of TT
        CALL DZGTDG(K,NTDIG)
        WRITE(TT(2:NTDIG+1),TF(NTDIG))K
C       .................................. Put the # or titles of columns in
C       .................................. the suitable position of TW
        IEND=ISTA+NDIG(K)
        ISTA=ISTA+NDIG(K)/2-1
        GO TO(1,1,2),NTDIG
    2   ISTA=ISTA-1
    1   N2=ISTA+NTDIG-1
        TW(ISTA:N2)=TT(2:NTDIG+1)
        TW(IEND:IEND)='|'
        ISTA=IEND+2
C       .........................................  typing the column title
        NTDIG=2
        TT(2:NTDIG+1)=TITL(K)
C       .................................. Put the # or titles of columns in
C       ........................................ the suitable position of TW
        IETL=ISTL+NDIG(K)
        ISTL=ISTL+NDIG(K)/2-1
        GO TO(11,11,12),NTDIG
   12   ISTL=ISTL-1
   11   N2=ISTL+NTDIG-1
        TL(ISTL:N2)=TT(2:NTDIG+1)
        TL(IETL:IETL)='|'
        ISTL=IETL+2
      END DO
      IF(NCEND.EQ.NCOLS)THEN
        TW(IEND:IEND)=']'
        TL(IETL:IETL)=']'
      END IF
C     ........................... Typing # or titles of columns on the terminal
      TWBNK=' '
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
C      TWBNK=' '
      TWBNK(1:MTLEN+1)=TL(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
      IWID=IEND-(NDG+NRDG+2)
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZFITT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZFITT
CH
      SUBROUTINE DZFITT(T,NTDIG,NX)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  Write NX into T (character) and get the # of digits NTDIG of T
C    Inputs    :
C            NX: Integer variable
C    Outputs   :
C             T: Character variable
C         NTDIG: The # of digits of T
C    Called by : DZLBTP,DZLBTA
C ---------------------------------------------------------------------
      CHARACTER *17 T
      CHARACTER *4 TF(9)
      DATA TF/
     &  '(I1)',
     &  '(I2)',
     &  '(I3)',
     &  '(I4)',
     &  '(I5)',
     &  '(I6)',
     &  '(I7)',
     &  '(I8)',
     &  '(I9)'/

      T=' '
C     .................................................... Get the digits of NX
      CALL DZGTDG(NX,NTDIG)
C     .......................................... Write NX into T (character)
      WRITE(T(2:NTDIG+1),TF(NTDIG))NX
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZGTDG
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZGTDG
CH
      SUBROUTINE DZGTDG(NV,N)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.DREVERMANN                   15-DEC-1991
C
C!:  Get the # of digits of NV
C    Inputs    :
C            NV: Integer variable
C    Outputs   :
C             N: The # of digits of NV
C    Called by : DZLBTP,DZLBTA,DZBTNC,DZBTCA,DZFITT,DZGTNM,DZWRCV
C                DZGTND,DZGNDA,DZWRNR
C ---------------------------------------------------------------------
      IF(NV.EQ.0)THEN
        N=1
        RETURN
      END IF
      IV=NV
      N=0
      IF(NV.LT.0)N=1
    1 IF(IV.EQ.0) RETURN
      IV=IV/10
      N=N+1
      GO TO 1
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZGTND
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZGTND
CH
      SUBROUTINE DZGTND(MTLEN,IBNK,TALPH,NROWS,NCOLS,NDIGT,NDIG,TFMD,
     &NCFLG,NRMX,NRMN)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  Get the maxmum digits, the maxmum value and it's row #,
C!:  the minimum value and it's row #
C!:  of every column for a bank with single number
C    Inputs    :
C        MTLEN : The width of one line for listing bank
C         IBNK : The index of bank
C        TALPH: The flag translated by common block
C                TALPH='ALEPH' means ALEPH bank
C                TALPH='BOS' means BOS  bank
C       NDIGT : The minimum # of digits for typing real data
C        NCOLS : The total numbers of columns in the bank
C        NROWS : The total rows # of the bank
C         TFMD : Column's format
C       INDCDZ : Indicate which columns will be changed from radian to degree
C                translated by common block
C            INDCDZ(I)=1 column I (angle) will be changed from radian to degree
C            INDCDZ(I)=0 column I (angle) will be reset to radian
C    Outputs   :
C         NDIG : The # of digits for every column translated by common block
C         NCFLG: The flag
C                NCFLG=0 means all columns can be listd in one line
C                otherwise NCFLG=1
C         NRMX :The row # of maxmun value of each column
C         NRMN :The row # of minimun value of each column
C    Called by : DZLBNK
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      PARAMETER(NFMTL=5000)
      COMMON/DZXULB/INDCDZ(NFMTL),MARKDZ(NFMTL),NCSTDZ
      CHARACTER *5 TALPH
      CHARACTER *1 TFMD(NCOLS)
CCC      CHARACTER *4 CHAINT,CTABL,CBTABL
      DIMENSION  NRMX(NCOLS),NRMN(NCOLS),NDIG(NCOLS),
     &GMX(NFMTL),GMN(NFMTL)
      INCLUDE 'A_BMACRO.INC'

CC - Lth character element of the NRth row of the bank with index ID
CC   (ALEPH bank)
C      CTABL(ID,NR,L) = CHAINT(IW(ID+LMHLEN+(NR-1)*IW(ID+1)+L))
CC - Lth character element of the NRth row of the bank with index ID (BOS bank)
C      CBTABL(ID,NR,L) = CHAINT(IW(ID+(NR-1)*IW(ID+1)+L))
CC - Lth integer element of the NRth row of the bank with index ID (BOS bank)
C      IBTABL(ID,NR,L) = IW(ID+(NR-1)*IW(ID+1)+L)
CC - Lth real element of the NRth row of the bank with index ID (BOS bank)
C      RBTABL(ID,NR,L) = RW(ID+(NR-1)*IW(ID+1)+L)

C - Lth character element of the NRth row of the bank with index ID (ALEPH bank)
CCC      CTABL(JD,JR,JL) = CHAINT(IW(JD+LMHLEN+(JR-1)*IW(JD+1)+JL))
C - Lth character element of the NRth row of the bank with index ID (BOS bank)
CCC      CBTABL(JD,JR,JL) = CHAINT(IW(JD+(JR-1)*IW(JD+1)+JL))
C - Lth integer element of the NRth row of the bank with index ID (BOS bank)
      IBTABL(JD,JR,JL) = IW(JD+(JR-1)*IW(JD+1)+JL)
C - Lth real element of the NRth row of the bank with index ID (BOS bank)
      RBTABL(JD,JR,JL) = RW(JD+(JR-1)*IW(JD+1)+JL)
      CALL DZGTDG(NROWS,NDK)

      DO I=1,NCOLS
        GMX(I)=0.
        GMN(I)=0.
      END DO
      DO I=1,NCOLS
C       ....................... The # of digits for all character variable is 4
        IF(TFMD(I).EQ.'A') THEN
          NDIG(I)=4
        ELSE
C         ................... Get maximum and minimum for intergers of all rows
          IF(TFMD(I).EQ.'I') THEN
            DO K=1,NROWS
              IF(TALPH.EQ.'ALEPH')THEN
                NX=ITABL(IBNK,K,I)
              ELSE
                NX=IBTABL(IBNK,K,I)
              END IF
              IF(K.EQ.1)THEN
                NMAX=NX
                NRMX(I)=K
                NMIN=NX
                NRMN(I)=K
              END IF
              IF(NX.GT.NMAX)THEN
                NMAX=NX
                NRMX(I)=K
              END IF
              IF(NX.LT.NMIN)THEN
                NMIN=NX
                NRMN(I)=K
              END IF
            END DO
            GMX(I)=FLOAT(NMAX)
            GMN(I)=FLOAT(NMIN)
C         ................... Get maximum and minimum for floatings of all rows
          ELSE
            DO K=1,NROWS
              IF(TALPH.EQ.'ALEPH')THEN
                X=RTABL(IBNK,K,I)
              ELSE
                X=RBTABL(IBNK,K,I)
              END IF
              IF(INDCDZ(I).EQ.1)X=X*180./3.14159
              IF(K.EQ.1)THEN
                GMAX=X
                NRMX(I)=K
                GMIN=X
                NRMN(I)=K
              END IF
              IF(X.GT.GMAX)THEN
                GMAX=X
                NRMX(I)=K
              END IF
              IF(X.LT.GMIN)THEN
                GMIN=X
                NRMN(I)=K
              END IF
            END DO
            GMX(I)=GMAX
            GMN(I)=GMIN
            IF(GMAX.LT.1.0E+9.AND.-GMIN.LT.1.0E+9)THEN
              NMAX=NINT(GMAX)
              NMIN=NINT(GMIN)
            ELSE
              NDIG(I)=13
              GO TO 100
            END IF
          END IF
          IF(NMIN.LT.0)NMIN=-NMIN
          IF(NMAX.GT.NMIN) THEN
C           ....................................... Get the digits of NMAX
            CALL DZGTDG(NMAX,NDIG(I))
C           ....................................... Get the digits of NMIN
            CALL DZGTDG(NMIN,NDIG0)
            IF(NDIG0.EQ.NDIG(I))NDIG(I)=NDIG(I)+1
C         .................................. If the above NMIN is negertive
          ELSE
C           ....................................... Get the digits of NMIN
            CALL DZGTDG(NMIN,NDIG(I))
            NDIG(I)=NDIG(I)+1
          END IF
          IF(TFMD(I).EQ.'F')THEN
            IF(GMIN.LT.0..AND.NDIG(I).LT.3)NDIG(I)=3
            NDIG(I)=MAX(NDIG(I),NDIGT)
          END IF
          NDIG(I)=MAX(NDIG(I),NDK)
        END IF
 100  END DO
      DO I=1,NCOLS
        IF(TFMD(I).NE.'A'.AND.GMN(I).EQ.0..AND.GMX(I).EQ.0.)THEN
          CALL DZGTDG(I,NC)
          NDIG(I)=MAX(2,NC)
        END IF
      END DO
C     .................................... IF all columns can be listd in one
C     .................................... line NCFLG=0 otherwise NCFLG=1
      NCFLG=0
      NSUM=7
C     ......... Calculate the total # of digits needed for printing one row
      DO I=1,NCOLS
        NSUM=NSUM+NDIG(I)+2
      END DO
      IF(NSUM.GT.MTLEN)NCFLG=1
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZGNDA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZGNDA
CH
      SUBROUTINE DZGNDA(MTLEN,TALPH,NCOLS,NDIGT,NDIG,TFMD,NCFLG,
     &NRMX,NRMN,NUMRW,NUMID,NDG,NRDG)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  Get the maxmum digits, the maxmum value and it's row #,
C!:  the minimum value and it's row #
C!:  of every column for a bank with total numbers
C    Inputs    :
C        MTLEN : The width of one line for listing bank
C       TALPH : Is a flag
C                TALPH='ALEPH' means this bank is ALEPH bank
C                TALPH='BOS  ' means this bank is BOS  bank
C         NDIGT : The minimum # of digits for typing real data
C        NCOLS : The total numbers of columns in the bank
C         TFMD : Column's format
C       INDCDZ : Indicate which columns will be changed from radian to degree
C                translated by common block
C            INDCDZ(I)=1 column I (angle) will be changed from radian to degree
C            INDCDZ(I)=0 column I (angle) will be reset to radian
C        NUMID : Array
C                NUMID(1,I)=INDEX of the bank for I-th row
C                NUMID(2,I)=NUMBER of the bank for I-th row
C        NUMRW : The number of total rows
C          NDG : The maxmum digits of the bank numbers
C         NRDG : The maxmum digits of the banks's rows
C    Outputs   :
C         NDIG : The # of digits for every column translated by common block
C         NCFLG: The flag
C                NCFLG=0 means all columns can be listd in one line
C                otherwise NCFLG=1
C         NRMX :The row # of maxmun value of each column
C         NRMN :The row # of minimun value of each column
C    Called by : DZLBKA
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      PARAMETER(NFMTL=5000)
      COMMON/DZXULB/INDCDZ(NFMTL),MARKDZ(NFMTL),NCSTDZ
      CHARACTER *5 TALPH
      CHARACTER *1 TFMD(NCOLS)
CCC      CHARACTER *4 CHAINT,CTABL,CBTABL
      DIMENSION  NRMX(NCOLS),NRMN(NCOLS),NDIG(NCOLS),
     &GMX(NFMTL),GMN(NFMTL)
      DIMENSION NUMID(2,NUMRW)
      DATA NDK/0/
      INCLUDE 'A_BMACRO.INC'

CC - Lth character element of the NRth row of the bank with index ID
CC   (ALEPH bank)
C      CTABL(ID,NR,L) = CHAINT(IW(ID+LMHLEN+(NR-1)*IW(ID+1)+L))
CC - Lth character element of the NRth row of the bank with index ID (BOS bank)
C      CBTABL(ID,NR,L) = CHAINT(IW(ID+(NR-1)*IW(ID+1)+L))
CC - Lth integer element of the NRth row of the bank with index ID (BOS bank)
C      IBTABL(ID,NR,L) = IW(ID+(NR-1)*IW(ID+1)+L)
CC - Lth real element of the NRth row of the bank with index ID (BOS bank)
C      RBTABL(ID,NR,L) = RW(ID+(NR-1)*IW(ID+1)+L)

C - Lth character element of the NRth row of the bank with index ID (ALEPH bank)
CCC      CTABL(JD,JR,JL) = CHAINT(IW(JD+LMHLEN+(JR-1)*IW(JD+1)+JL))
C - Lth character element of the NRth row of the bank with index ID (BOS bank)
CCC      CBTABL(JD,JR,JL) = CHAINT(IW(JD+(JR-1)*IW(JD+1)+JL))
C - Lth integer element of the NRth row of the bank with index ID (BOS bank)
      IBTABL(JD,JR,JL) = IW(JD+(JR-1)*IW(JD+1)+JL)
C - Lth real element of the NRth row of the bank with index ID (BOS bank)
      RBTABL(JD,JR,JL) = RW(JD+(JR-1)*IW(JD+1)+JL)

      DO I=1,NCOLS
        GMX(I)=0.
        GMN(I)=0.
      END DO
      DO I=1,NCOLS
C       ....................... The # of digits for all character variable is 4
        IF(TFMD(I).EQ.'A') THEN
          NDIG(I)=4
        ELSE
C         ................... Get maximum and minimum for intergers of all rows
          IF(TFMD(I).EQ.'I') THEN
            DO K=1,NUMRW
              IBNK=NUMID(1,K)
              KR=NUMID(2,K)
              IF(TALPH.EQ.'ALEPH')THEN
                NX=ITABL(IBNK,KR,I)
              ELSE
                NX=IBTABL(IBNK,KR,I)
              END IF
              IF(K.EQ.1)THEN
                NMAX=NX
                NRMX(I)=K
                NMIN=NX
                NRMN(I)=K
              END IF
              IF(NX.GT.NMAX)THEN
                NMAX=NX
                NRMX(I)=K
              END IF
              IF(NX.LT.NMIN)THEN
                NMIN=NX
                NRMN(I)=K
              END IF
            END DO
            GMX(I)=FLOAT(NMAX)
            GMN(I)=FLOAT(NMIN)
C         ................... Get maximum and minimum for floatings of all rows
          ELSE
            DO K=1,NUMRW
              IBNK=NUMID(1,K)
              KR=NUMID(2,K)
              IF(TALPH.EQ.'ALEPH')THEN
                X=RTABL(IBNK,KR,I)
              ELSE
                X=RBTABL(IBNK,KR,I)
              END IF
              IF(INDCDZ(I).EQ.1)X=X*180./3.14159
              IF(K.EQ.1)THEN
                GMAX=X
                NRMX(I)=K
                GMIN=X
                NRMN(I)=K
              END IF
              IF(X.GT.GMAX)THEN
                GMAX=X
                NRMX(I)=K
              END IF
              IF(X.LT.GMIN)THEN
                GMIN=X
                NRMN(I)=K
              END IF
            END DO
            GMX(I)=GMAX
            GMN(I)=GMIN
            IF(GMAX.LT.1.0E+9.AND.-GMIN.LT.1.0E+9)THEN
              NMAX=NINT(GMAX)
              NMIN=NINT(GMIN)
            ELSE
              NDIG(I)=13
              GO TO 100
            END IF
          END IF
          IF(NMIN.LT.0)NMIN=-NMIN
          IF(NMAX.GT.NMIN) THEN
C           ....................................... Get the digits of NMAX
            CALL DZGTDG(NMAX,NDIG(I))
            CALL DZGTDG(NMIN,NDIG0)
            IF(NDIG0.EQ.NDIG(I))NDIG(I)=NDIG(I)+1
C         .................................. If the above NMIN is negertive
          ELSE
C           ....................................... Get the digits of NMIN
            CALL DZGTDG(NMIN,NDIG(I))
            NDIG(I)=NDIG(I)+1
          END IF
          IF(TFMD(I).EQ.'F')THEN
            IF(GMIN.LT.0..AND.NDIG(I).LT.3)NDIG(I)=3
            NDIG(I)=MAX(NDIG(I),NDIGT)
          END IF
          NDIG(I)=MAX(NDIG(I),NDK)

        END IF
 100  END DO
      DO I=1,NCOLS
        IF(TFMD(I).NE.'A'.AND.GMN(I).EQ.0..AND.GMX(I).EQ.0.)THEN
C         ......................................... Get the digits of NC
          CALL DZGTDG(I,NC)
          NDIG(I)=MAX(2,NC)
        END IF
      END DO
C     .................................... IF all columns can be listd in one
C     .................................... line NCFLG=0 otherwise NCFLG=1
      NCFLG=0
      NSUM=7+(NDG+NRDG+2)
C     ......... Calculate the total # of digits needed for printing one row
      DO I=1,NCOLS
        NSUM=NSUM+NDIG(I)+2
      END DO
      IF(NSUM.GT.MTLEN)NCFLG=1
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZMXMN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZMXMN
CH
      SUBROUTINE DZMXMN(TALPH,IBNK,MTLEN,NCSTDZ,NCEND,NROWS,NCOLS,
     &TITL,INDCDZ,MARKDZ,NDIG,TFMD,NRMX,NRMN,NDO)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  list the maxmum and minimum value and their row # of every column
C!:  for a bank with single number
C    Inputs    :
C        MTLEN : The width of one line for listing bank
C        NCSTDZ : The starting column #
C        NCEND : The ending column #
C        NROWS : The total rows # of the bank
C        NCOLS : The total numbers of columns in the bank
C         NDIG : The # of digits for every column of the bank
C         TITL : The column's titles
C         TFMD : Column's format
C         NRMX :The row # of maxmun value of each column
C         NRMN :The row # of minimun value of each column
C          NDO : The flag translated by common block
C                NDO=1 Typing the bank forward
C                NDO=-1 Typing the bank backward
C    Outputs   :
C    Called by : DZLBNK
C ---------------------------------------------------------------------
      CHARACTER *5 TALPH
      CHARACTER *1 TFMD(NCOLS)
      CHARACTER *2 TITL(NCOLS)
      CHARACTER *140 TW,TWBNK
      DIMENSION  NRMX(NCOLS),NRMN(NCOLS),NDIG(NCOLS)
      DIMENSION  INDCDZ(NCOLS),MARKDZ(NCOLS)
      CHARACTER *1 TMP(-1:1)
      DATA TMP/'-',':','+'/
C     ............................ list the # or title of being listed columns
      CALL DZBTNC(MTLEN,NROWS,NCOLS,TITL,INDCDZ,MARKDZ,
     &  NCSTDZ,NCEND,NDIG,NDO)
C     .... Write row # of minimum value of each column to character variable TW
      CALL DZWRNR(NCSTDZ,NCEND,NROWS,NCOLS,NDIG,TFMD,NRMN,TW)
      TW(2:5)='ROW#'
      TWBNK=' '
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
C     ...........  Write  minimum value of each column to character variable TW
      CALL DZWRMX(TALPH,IBNK,NCEND,NROWS,NCOLS,NDIG,TFMD,NRMN,TW)
      TW(2:4)='MIN'
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
C     ............  Write  maxmum value of each column to character variable TW
      CALL DZWRMX(TALPH,IBNK,NCEND,NROWS,NCOLS,NDIG,TFMD,NRMX,TW)
      TW(2:4)='MAX'
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
C     ..... Write row # of maxmum value of each column to character variable TW
      CALL DZWRNR(NCSTDZ,NCEND,NROWS,NCOLS,NDIG,TFMD,NRMX,TW)
      TW(2:5)='ROW#'
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
      TW(1:1)=' '
      DO I=2,MTLEN
        TW(I:I)='-'
      END DO
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZMXMA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZMXMA
CH
      SUBROUTINE DZMXMA(TALPH,NUMRW,NUMID,MTLEN,NCSTDZ,NCEND,NCOLS,
     &TITL,INDCDZ,MARKDZ,NDIG,TFMD,NRMX,NRMN,NDO,NDG,NRDG)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  list the maxmum and minimum value and their row # of every column
C!:  for a bank with total numbers
C    Inputs    :
C       TALPH : Is a flag
C                TALPH='ALEPH' means this bank is ALEPH bank
C                TALPH='BOS  ' means this bank is BOS  bank
C        NUMID : Array
C                NUMID(1,I)=INDEX of the bank for I-th row
C                NUMID(2,I)=NUMBER of the bank for I-th row
C        NUMRW : The number of total rows
C        MTLEN : The width of one line for listing bank
C        NCSTDZ : The starting column #
C        NCEND : The ending column #
C        NCOLS : The total numbers of columns in the bank
C         NDIG : The # of digits for every column of the bank
C         TITL : The column's titles
C         TFMD : Column's format
C         NRMX :The row # of maxmun value of each column
C         NRMN :The row # of minimun value of each column
C         NRMX :The row # of maxmun value of each column
C         NRMN :The row # of minimun value of each column
C          NDO : The flag
C                NDO=1 Typing the bank forward
C                NDO=-1 Typing the bank backward
C          NDG : The maxmum digits of the bank numbers
C         NRDG : The maxmum digits of the banks's rows
C    Outputs   :
C    Called by : DZLBKA
C ---------------------------------------------------------------------
      CHARACTER *5 TALPH
      CHARACTER *1 TFMD(NCOLS)
      CHARACTER *2 TITL(NCOLS)
      CHARACTER *140 TW,TWBNK
      DIMENSION  NRMX(NCOLS),NRMN(NCOLS),NDIG(NCOLS),NUMID(2,NUMRW)
      DIMENSION  INDCDZ(NCOLS),MARKDZ(NCOLS)
      CHARACTER *1 TMP(-1:1)
      DATA TMP/'-',':','+'/
      NB=8+NDG+NRDG
      NW=NDG+NRDG+1
C     ............................ list the # or title of being listed columns
      CALL DZBTCA(MTLEN,NCSTDZ,NCEND,NUMRW,NCOLS,TITL,
     &  INDCDZ,MARKDZ,NDIG,NDO,NDG,NRDG,IWID,1)
C     .... Write row # of minimum value of each column to character variable TW
      CALL DZWRNR(NCSTDZ,NCEND,NUMRW,NCOLS,NDIG,TFMD,NRMN,TW)
      CALL DZSHFT(TW,NW,IWID)
      NH=(8+(NDG+NRDG+1))/2
      TW(NH-2:NH+2)='Row #'
      TW(NB:NB)='|'
      IF(NCSTDZ.EQ.1)TW(NB:NB)='['
      TWBNK=' '
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
C     ...........  Write  minimum value of each column to character variable TW
      CALL DZWRMA(TALPH,NUMRW,NUMID,NCEND,NCOLS,NDIG,TFMD,NRMN,TW)
      CALL DZSHFT(TW,NW,IWID)
      TW(NH-3:NH+3)='Minimum'
      TW(NB:NB)='|'
      IF(NCSTDZ.EQ.1)TW(NB:NB)='['
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
C     ............  Write  maxmum value of each column to character variable TW
      CALL DZWRMA(TALPH,NUMRW,NUMID,NCEND,NCOLS,NDIG,TFMD,NRMX,TW)
      CALL DZSHFT(TW,NW,IWID)
      TW(NH-3:NH+3)='Maximum'
      TW(NB:NB)='|'
      IF(NCSTDZ.EQ.1)TW(NB:NB)='['
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
C     ..... Write row # of maxmum value of each column to character variable TW
      CALL DZWRNR(NCSTDZ,NCEND,NUMRW,NCOLS,NDIG,TFMD,NRMX,TW)
      CALL DZSHFT(TW,NW,IWID)
      TW(NH-2:NH+2)='Row #'
      TW(NB:NB)='|'
      IF(NCSTDZ.EQ.1)TW(NB:NB)='['
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
      TW(1:1)=' '
      DO I=2,MTLEN
        TW(I:I)='-'
      END DO
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZSHFT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZSHFT
CH
      SUBROUTINE DZSHFT(TW,NW,IWID)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  Shift the characters right with NW digits in TW
C    Inputs    :
C           TW : Character variable will be shift
C           NW : The shift digits
C         IWID : =IEND-(NDG+NRDG+2)
C    Outputs   :
C    Called by : DZMXMA,DZWRVA
C ---------------------------------------------------------------------
      CHARACTER *132 TW,TS
      DO I=1,132
        TS(I:I)=TW(I:I)
      END DO
      DO I=7,7+NW
        TW(I:I)=' '
      END DO
      I0=7+NW
      DO I=7,IWID
        I0=I0+1
        TW(I0:I0)=TS(I:I)
      END DO
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZWRMX
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZWRMX
CH
      SUBROUTINE DZWRMX(TALPH,IBNK,NCEND,NROWS,NCOLS,NDIG,TFMD,NR,TW)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  Write maxmum or minimum value or Vnear of each column
C!:  to character variable TW for the bank with single number
C    Inputs    :
C       TALPH : Is a flag
C                TALPH='ALEPH' means this bank is ALEPH bank
C                TALPH='BOS  ' means this bank is BOS  bank
C         IBNK : The index of bank
C        NROWS : The total rows # of the bank
C        NCOLS : The total numbers of columns in the bank
C        NCSTDZ : The starting column #
C        NCEND : The ending column #
C           NR :digital arry contain row # of maxmum or minimum or Vnear
C         NDIG : The # of digits for every column of the bank
C         TFMD : Column's format
C    Outputs   :
C           TW :character arry
C    Called by : DZMXMN,DZWRVL
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      PARAMETER(NFMTL=5000)
      COMMON/DZXULB/INDCDZ(NFMTL),MARKDZ(NFMTL),NCSTDZ
      CHARACTER *5 TALPH
      CHARACTER *1 TFMD(NCOLS)
      DIMENSION NR(NCOLS),NDIG(NCOLS)
      CHARACTER *20 TT,T1,T2
      CHARACTER *132 TW
      CHARACTER *4 TF(9)
      DATA TF/
     &  '(I1)',
     &  '(I2)',
     &  '(I3)',
     &  '(I4)',
     &  '(I5)',
     &  '(I6)',
     &  '(I7)',
     &  '(I8)',
     &  '(I9)'/
      CHARACTER *7 TG(2:9)
      DATA TG/
     &  '(G13.1)',
     &  '(G13.2)',
     &  '(G13.3)',
     &  '(G13.4)',
     &  '(G13.5)',
     &  '(G13.6)',
     &  '(G13.7)',
     &  '(G13.8)'/
      INCLUDE 'A_BMACRO.INC'

CC - Lth character element of the NRth row of the bank with index ID
CC   (ALEPH bank)
C      CTABL(ID,NR,L) = CHAINT(IW(ID+LMHLEN+(NR-1)*IW(ID+1)+L))
CC - Lth character element of the NRth row of the bank with index ID (BOS bank)
C      CBTABL(ID,NR,L) = CHAINT(IW(ID+(NR-1)*IW(ID+1)+L))
CC - Lth integer element of the NRth row of the bank with index ID (BOS bank)
C      IBTABL(ID,NR,L) = IW(ID+(NR-1)*IW(ID+1)+L)
CC - Lth real element of the NRth row of the bank with index ID (BOS bank)
C      RBTABL(ID,NR,L) = RW(ID+(NR-1)*IW(ID+1)+L)

C - Lth character element of the NRth row of the bank with index ID (ALEPH bank)
CCC      CTABL(JD,JR,JL) = CHAINT(IW(JD+LMHLEN+(JR-1)*IW(JD+1)+JL))
C - Lth character element of the NRth row of the bank with index ID (BOS bank)
CCC      CBTABL(JD,JR,JL) = CHAINT(IW(JD+(JR-1)*IW(JD+1)+JL))
C - Lth integer element of the NRth row of the bank with index ID (BOS bank)
      IBTABL(JD,JR,JL) = IW(JD+(JR-1)*IW(JD+1)+JL)
C - Lth real element of the NRth row of the bank with index ID (BOS bank)
      RBTABL(JD,JR,JL) = RW(JD+(JR-1)*IW(JD+1)+JL)

C     .............................................. Get the digits of NROWS
      CALL DZGTDG(NROWS,NDK)
      NDK=MAX(4,NDK)
      ISTA=4+NDK
C      ISTA=8
      TW=' '
      TW(2+NDK:2+NDK)='|'
      IF(NCSTDZ.EQ.1)TW(2+NDK:2+NDK)='['
      DO I=NCSTDZ,NCEND
C        ................................ Put mark '~' in the assigned column
         IF(MARKDZ(I).EQ.1)THEN
          TW((ISTA-1):(ISTA-1))='~'
        END IF
C       ................................ Put mark '^' in the assigned column
C       ................................ in which angle to be set to degrees
        IF(INDCDZ(I).EQ.1)THEN
          TW((ISTA-1):(ISTA-1))='^'
        END IF
        IF(TFMD(I).EQ.'I') THEN
C         ................................ Get Ith integer element of the Kth
C         ................................... row of the bank with index IBNK
          IF(TALPH.EQ.'ALEPH')THEN
            NX=ITABL(IBNK,NR(I),I)
          ELSE
            NX=IBTABL(IBNK,NR(I),I)
          END IF
C           .............................. Get # of digits NTDIG of NX
          CALL DZGTDG(NX,NTDIG)
          IF(NTDIG.LE.9)THEN
C           ......................................... Write NX into TT
            WRITE(TT(2:NTDIG+1),TF(NTDIG))NX
          ELSE
C           ............................ If the # of digits of integer >9
C           ............................ Unpack it into two integer
            NX1=IFIX(FLOAT(NX)*0.00000001)
            NX2=NX-NX1*10**8
C           ........................................ Write NX1 into T1
            CALL DZFITT(T1,NTDIG1,NX1)
C           .......................................... Write NX2 into T2
            CALL DZFITT(T2,NTDIG2,ABS(NX2))
            TT(2:NTDIG+1)=T1(2:NTDIG1+1)//T2(2:NTDIG2+1)
          END IF
C         ................................. Put TT in the suitable position of
C         ................................ TW to indent upto right for integer
          NS=(NDIG(I)-NTDIG)/2
          IEND=ISTA+NDIG(I)-1-NS
          IEND0=ISTA+NDIG(I)
          ISTA=IEND-NTDIG+1
          TW(ISTA:IEND)=TT(2:(NTDIG+1))
          IF(I.EQ.NCOLS)THEN
            TW(IEND0:IEND0)=']'
          ELSE
            TW(IEND0:IEND0)='|'
          END IF
          ISTA=IEND0+2
        ELSE IF(TFMD(I).EQ.'F') THEN
C         ..................................... Get Ith real element of the Kth
C         ..................................... row of the bank with index IBNK
          IF(TALPH.EQ.'ALEPH')THEN
            X=RTABL(IBNK,NR(I),I)
          ELSE
            X=RBTABL(IBNK,NR(I),I)
          END IF
          IF(INDCDZ(I).EQ.1)X=X*180./3.14159
          IF(NDIG(I).LT.10)THEN
C           .................change floating point number X to dali format TT
            CALL DTN(X,NDIG(I),TT)
C           ........................... Put TT in the suitable position of TW
C           ..................... to indent upto left for floating point data
            DO J=1,NDIG(I)
              IF(TT(J:J).EQ.'.')THEN
                NEND=3
                IF(TT(1:1).EQ.'-')NEND=4
                GO TO 555
              END IF
            END DO
            GO TO 444
  555       DO J=NDIG(I),NEND,-1
C             ..................... Delet the '0' in the end of floating data
              IF(TT(J:J).NE.'0')GO TO 444
              TT(J:J)=' '
            END DO
          ELSE
            WRITE(TT,TG(8))X
          END IF
  444     IEND=ISTA+NDIG(I)-1
          TW(ISTA:IEND)=TT(1:NDIG(I))
          IEND=IEND+1
          TW(IEND:IEND)='|'
          IF(I.EQ.NCOLS)TW(IEND:IEND)=']'
          ISTA=IEND+2
        ELSE
C         ......................... If this column is Character just give blank
          IEND=ISTA+NDIG(I)-1
          ISTA=IEND-3
          TW(ISTA:IEND+1)='    '//'|'
          IF(I.EQ.NCOLS)TW(IEND+1:IEND+1)=']'
          ISTA=IEND+3
        END IF
      END DO
      IWID=IEND+1
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZWRMA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZWRMA
CH
      SUBROUTINE DZWRMA(TALPH,NUMRW,NUMID,NCEND,NCOLS,NDIG,TFMD,NR,TW)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  Write maxmum or minimum value or Vnear of each column
C!:  to character variable TW for the bank with total number
C    Inputs    :
C       TALPH : Is a flag
C                TALPH='ALEPH' means this bank is ALEPH bank
C                TALPH='BOS  ' means this bank is BOS  bank
C        NUMID : Array
C                NUMID(1,I)=INDEX of the bank for I-th row
C                NUMID(2,I)=NUMBER of the bank for I-th row
C        NUMRW : The number of total rows
C        NCOLS : The total numbers of columns in the bank
C        NCSTDZ : The starting column #
C        NCEND : The ending column #
C           NR :digital arry contain row # of maxmum or minimum or Vnear
C         NDIG : The # of digits for every column of the bank
C         TFMD : Column's format
C    Outputs   :
C           TW :character arry
C    Called by : DZMXMA,DZWRVA
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      PARAMETER(NFMTL=5000)
      COMMON/DZXULB/INDCDZ(NFMTL),MARKDZ(NFMTL),NCSTDZ
      CHARACTER *5 TALPH
      CHARACTER *1 TFMD(NCOLS)
      DIMENSION NR(NCOLS),NDIG(NCOLS),NUMID(2,NUMRW)
      CHARACTER *20 TT,T1,T2
      CHARACTER *132 TW
      CHARACTER *4 TF(9)
      DATA TF/
     &  '(I1)',
     &  '(I2)',
     &  '(I3)',
     &  '(I4)',
     &  '(I5)',
     &  '(I6)',
     &  '(I7)',
     &  '(I8)',
     &  '(I9)'/
      CHARACTER *7 TG(2:9)
      DATA TG/
     &  '(G13.1)',
     &  '(G13.2)',
     &  '(G13.3)',
     &  '(G13.4)',
     &  '(G13.5)',
     &  '(G13.6)',
     &  '(G13.7)',
     &  '(G13.8)'/
      INCLUDE 'A_BMACRO.INC'

CC - Lth character element of the NRth row of the bank with index ID
CC   (ALEPH bank)
C      CTABL(ID,NR,L) = CHAINT(IW(ID+LMHLEN+(NR-1)*IW(ID+1)+L))
CC - Lth character element of the NRth row of the bank with index ID (BOS bank)
C      CBTABL(ID,NR,L) = CHAINT(IW(ID+(NR-1)*IW(ID+1)+L))
CC - Lth integer element of the NRth row of the bank with index ID (BOS bank)
C      IBTABL(ID,NR,L) = IW(ID+(NR-1)*IW(ID+1)+L)
CC - Lth real element of the NRth row of the bank with index ID (BOS bank)
C      RBTABL(ID,NR,L) = RW(ID+(NR-1)*IW(ID+1)+L)

C - Lth character element of the NRth row of the bank with index ID (ALEPH bank)
CCC      CTABL(JD,JR,JL) = CHAINT(IW(JD+LMHLEN+(JR-1)*IW(JD+1)+JL))
C - Lth character element of the NRth row of the bank with index ID (BOS bank)
CCC      CBTABL(JD,JR,JL) = CHAINT(IW(JD+(JR-1)*IW(JD+1)+JL))
C - Lth integer element of the NRth row of the bank with index ID (BOS bank)
      IBTABL(JD,JR,JL) = IW(JD+(JR-1)*IW(JD+1)+JL)
C - Lth real element of the NRth row of the bank with index ID (BOS bank)
      RBTABL(JD,JR,JL) = RW(JD+(JR-1)*IW(JD+1)+JL)

C     .............................................. Get the digits of NUMRW
      CALL DZGTDG(NUMRW,NDK)
      NDK=MAX(4,NDK)
      ISTA=4+NDK
      TW=' '
      TW(2+NDK:2+NDK)='|'
      IF(NCSTDZ.EQ.1)TW(2+NDK:2+NDK)='['
       DO I=NCSTDZ,NCEND
C        ................................ Put mark '~' in the assigned column
         IF(MARKDZ(I).EQ.1)THEN
          TW((ISTA-1):(ISTA-1))='~'
        END IF
C       ................................ Put mark '^' in the assigned column
C       ................................ in which angle to be set to degrees
        IF(INDCDZ(I).EQ.1)THEN
          TW((ISTA-1):(ISTA-1))='^'
        END IF
        IBNK=NUMID(1,NR(I))
        KR=NUMID(2,NR(I))
        IF(TFMD(I).EQ.'I') THEN
C         ................................ Get Ith integer element of the Kth
C         ................................... row of the bank with index IBNK
          IF(TALPH.EQ.'ALEPH')THEN
            NX=ITABL(IBNK,KR,I)
          ELSE
            NX=IBTABL(IBNK,KR,I)
          END IF
C         ....................................... Get # of digits NTDIG of NX
          CALL DZGTDG(NX,NTDIG)
          IF(NTDIG.LE.9)THEN
C           ................................................ Write NX into TT
            WRITE(TT(2:NTDIG+1),TF(NTDIG))NX
          ELSE
C           ................................. If the # of digits of integer >9
C           ....................................... Unpack it into two integer
            NX1=IFIX(FLOAT(NX)*0.00000001)
            NX2=NX-NX1*10**8
C           ................................................ Write NX1 into T1
            CALL DZFITT(T1,NTDIG1,NX1)
C           ................................................ Write NX2 into T2
            CALL DZFITT(T2,NTDIG2,ABS(NX2))
            TT(2:NTDIG+1)=T1(2:NTDIG1+1)//T2(2:NTDIG2+1)
          END IF
C         ................................. Put TT in the suitable position of
C         ................................ TW to indent upto right for integer
          NS=(NDIG(I)-NTDIG)/2
          IEND=ISTA+NDIG(I)-1-NS
          IEND0=ISTA+NDIG(I)
          ISTA=IEND-NTDIG+1
          TW(ISTA:IEND)=TT(2:(NTDIG+1))
          IF(I.EQ.NCOLS)THEN
            TW(IEND0:IEND0)=']'
          ELSE
            TW(IEND0:IEND0)='|'
          END IF
          ISTA=IEND0+2
        ELSE IF(TFMD(I).EQ.'F') THEN
C         ..................................... Get Ith real element of the Kth
C         ..................................... row of the bank with index IBNK
          IF(TALPH.EQ.'ALEPH')THEN
            X=RTABL(IBNK,KR,I)
          ELSE
            X=RBTABL(IBNK,KR,I)
          END IF
          IF(INDCDZ(I).EQ.1)X=X*180./3.14159
          IF(NDIG(I).LT.10)THEN
C           ...................change floating point number X to dali format TT
            CALL DTN(X,NDIG(I),TT)
C           ............................. Put TT in the suitable position of TW
C           ....................... to indent upto left for floating point data
            DO J=1,NDIG(I)
              IF(TT(J:J).EQ.'.')THEN
                NEND=3
                IF(TT(1:1).EQ.'-')NEND=4
                GO TO 555
              END IF
            END DO
            GO TO 444
  555       DO J=NDIG(I),NEND,-1
C             ....................... Delet the '0' in the end of floating data
              IF(TT(J:J).NE.'0')GO TO 444
              TT(J:J)=' '
            END DO
          ELSE
            WRITE(TT,TG(8))X
          END IF
  444     IEND=ISTA+NDIG(I)-1
          TW(ISTA:IEND)=TT(1:NDIG(I))
          IEND=IEND+1
          TW(IEND:IEND)='|'
          IF(I.EQ.NCOLS)TW(IEND:IEND)=']'
          ISTA=IEND+2
        ELSE
C         ......................... If this column is Character just give blank
          IEND=ISTA+NDIG(I)-1
          ISTA=IEND-3
          TW(ISTA:IEND+1)='    '//'|'
          IF(I.EQ.NCOLS)TW(IEND+1:IEND+1)=']'
          ISTA=IEND+3
        END IF
      END DO
      IWID=IEND+1
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZWRNR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZWRNR
CH
      SUBROUTINE DZWRNR(NCSTDZ,NCEND,NRWS,NCOLS,NDIG,TFMD,NR,TW)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  Write the row # of maxmum or minimum value of each column
C!:  to character arryay TW
C    Inputs    :
C        NCOLS : The total numbers of columns in the bank
C        NCSTDZ :The starting column #
C        NCEND :The ending column #
C           NR : row # arry of maxmum value of each column
C         NDIG : The # of digits for every column of the bank
C         TFMD : Column's format
C         NRWS : The number of total rows
C    Outputs   :
C           TW :character arry
C    Called by : DZMXMN,DZMXMA,DZWRVA,DZWRVL
C ---------------------------------------------------------------------
      CHARACTER *1 TFMD(NCOLS)
      DIMENSION NR(NCOLS),NDIG(NCOLS)
      CHARACTER *20 TT
      CHARACTER *132 TW
      CHARACTER *4 TF(9)
      DATA TF/
     &  '(I1)',
     &  '(I2)',
     &  '(I3)',
     &  '(I4)',
     &  '(I5)',
     &  '(I6)',
     &  '(I7)',
     &  '(I8)',
     &  '(I9)'/
      CALL DZGTDG(NRWS,NDK)
      NDK=MAX(4,NDK)
      ISTA=4+NDK
      TW=' '
      TW(2+NDK:2+NDK)='|'
      IF(NCSTDZ.EQ.1)TW(2+NDK:2+NDK)='['
      DO I=NCSTDZ,NCEND
        IF(TFMD(I).NE.'A')THEN
C         .................................. Write row # into TT and get
C         .................................. the # of digits NTDIG of TT
          CALL DZGTDG(NR(I),NTDIG)
          WRITE(TT(2:NTDIG+1),TF(NTDIG))NR(I)
C         .................................. Put the # or titles of columns in
C         .................................. the suitable position of TW
          IEND=ISTA+NDIG(I)
          ISTA=ISTA+(NDIG(I)-NTDIG)/2
          N2=ISTA+NTDIG-1
          TW(ISTA:N2)=TT(2:NTDIG+1)
          TW(IEND:IEND)='|'
          ISTA=IEND+2
        ELSE
          IEND=ISTA+NDIG(I)-1
          ISTA=IEND-3
          TW(ISTA:IEND+1)='    '//'|'
          ISTA=IEND+3
        END IF
      END DO
      IF(NCEND.EQ.NCOLS)TW(IEND:IEND)=']'
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZCSEA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZCSEA
CH
      SUBROUTINE DZCSEA(MTLEN,NCFLG,NCSTDZ,NCEND,NCOLS,
     &NDIG,NDG,NRDG)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  Define the starting column # and the ending column # for a bank
C!:  with total numbers
C    Inputs    :
C        MTLEN : The width of one line for listing bank
C        NCOLS : The total numbers of columns in the bank
C        NCFLG : The flag
C                NCFLG=0 means all columns can be listd in one line
C                otherwise NCFLG=1
C         NDIG : The # of digits for every column of the bank
C          NDG : The maxmum digits of the bank numbers
C         NRDG : The maxmum digits of the banks's rows
C    Outputs   :
C        NCSTDZ : The output starting column #
C        NCEND : The ending column #
C    Called by : DZLBKA
C ---------------------------------------------------------------------
      DIMENSION NDIG(NCOLS)
C     .................................... IF NCFLG=0 then all columns can be
C     .................................... listd in one line otherwise NCFLG=1
 936  IF(NCFLG.EQ.0)THEN
        NCSTDZ=1
        NCEND=NCOLS
C     ............................... IF NCFLG.NE.0 calculate starting column #
      ELSE
        NSUM=7+(NDG+NRDG+2)
        DO I=NCOLS,1,-1
          NSUM=NSUM+NDIG(I)+2
          IF(NSUM.GT.MTLEN)GO TO 22
        END DO
C       ............................... NCMIN: minimum column # the all columns
C       ............................... from which to end can listd in one line
 22     NCMIN=I+1
        NCSTDZ=MAX(1,MIN(NCMIN,NCSTDZ))
C       .................................... calculate the ending column #
        IF(NCSTDZ.GE.NCMIN)THEN
          NCEND=NCOLS
          RETURN
        ELSE
          NSUM=7+(NDG+NRDG+2)
          DO I=NCSTDZ,NCOLS
            NSUM=NSUM+NDIG(I)+2
            IF(NSUM.GT.MTLEN)THEN
              NCEND=I-1
              RETURN
            END IF
          END DO
        END IF
      END IF
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZNCSE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZNCSE
CH
      SUBROUTINE DZNCSE(MTLEN,NCFLG,NCSTDZ,NCEND,NCOLS,NDIG)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  Define the starting column # and the ending column # for a bank
C!:  with a single number
C    Inputs    :
C        NCFLG : The flag
C                NCFLG=0 means all columns can be listd in one line
C                otherwise NCFLG=1
C        NCSTDZ : The input starting column #
C         NDIG : The # of digits for every column of the bank
C        MTLEN : The width of one line for listing bank
C    Outputs   :
C        NCSTDZ : The output starting column #
C        NCEND : The ending column #
C    Called by : DZLBNK
C ---------------------------------------------------------------------
      DIMENSION NDIG(NCOLS)
C     .................................... IF NCFLG=0 then all columns can be
C     .................................... listd in one line otherwise NCFLG=1
 936  IF(NCFLG.EQ.0)THEN
        NCSTDZ=1
        NCEND=NCOLS
C     ............................... IF NCFLG.NE.0 calculate starting column #
      ELSE
        NSUM=7
        DO I=NCOLS,1,-1
          NSUM=NSUM+NDIG(I)+2
          IF(NSUM.GT.MTLEN)GO TO 22
        END DO
C       ............................... NCMIN: minimum column # the all columns
C       ............................... from which to end can listd in one line
 22     NCMIN=I+1
        NCSTDZ=MAX(1,MIN(NCMIN,NCSTDZ))
C       ......................................... calculate the ending column #
        IF(NCSTDZ.GE.NCMIN)THEN
          NCEND=NCOLS
          RETURN
        ELSE
          NSUM=7
          DO I=NCSTDZ,NCOLS
            NSUM=NSUM+NDIG(I)+2
            IF(NSUM.GT.MTLEN)THEN
              NCEND=I-1
              RETURN
            END IF
          END DO
        END IF
      END IF
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZFDVL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZFDVL
CH
      SUBROUTINE DZFDVL(VX,NRVN,IBNK,NCOLS,TFMD,NROWS,TALPH)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  Get the nearest value of VX and it's row # for every column
C!:  for a bank with a single number
C    Inputs    :
C        NCOLS : The total numbers of columns in the bank
C        NROWS : The total rows # of the bank
C           VX : The setting value
C         TFMD : Column's format
C        INDCDZ: Indicate which columns will be changed from radian to degree
C                translated by common block
C            INDCDZ(I)=1 column I (angle) will be changed from radian to degree
C            INDCDZ(I)=0 column I (angle) will be reset to radian
C       TALPH : Is a flag
C                TALPH='ALEPH' means this bank is ALEPH bank
C                TALPH='BOS  ' means this bank is BOS  bank
C         IBNK : The index of bank
C    Outputs   :
C         NRVN : The row # of the nearest value
C    Called by : DZLBNK
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      PARAMETER(NFMTL=5000)
      COMMON/DZXULB/INDCDZ(NFMTL),MARKDZ(NFMTL),NCSTDZ
      CHARACTER *5 TALPH
      CHARACTER *1 TFMD(NCOLS)
      DIMENSION NRVN(NCOLS)
      INCLUDE 'A_BMACRO.INC'
C - Lth integer element of the NRth row of the bank with index ID (BOS bank)
      IBTABL(ID,NR,L) = IW(ID+(NR-1)*IW(ID+1)+L)
C - Lth real element of the NRth row of the bank with index ID (BOS bank)
      RBTABL(ID,NR,L) = RW(ID+(NR-1)*IW(ID+1)+L)
      DO I=1,NCOLS
        NRVN(I)=0.
      END DO
      DO I=1,NCOLS
        IF(TFMD(I).EQ.'A')GO TO 100
        IF(TFMD(I).EQ.'I') THEN
          DO K=1,NROWS
            IF(TALPH.EQ.'ALEPH')THEN
              NX=ITABL(IBNK,K,I)
            ELSE
              NX=IBTABL(IBNK,K,I)
            END IF
            NDLT=ABS(IFIX(VX)-NX)
            IF(K.EQ.1)THEN
              NMIN=NDLT
              NRVN(I)=K
            END IF
            IF(NMIN.GT.NDLT)THEN
              NMIN=NDLT
              NRVN(I)=K
            END IF
          END DO
        ELSE
          DO K=1,NROWS
            IF(TALPH.EQ.'ALEPH')THEN
              X=RTABL(IBNK,K,I)
            ELSE
              X=RBTABL(IBNK,K,I)
            END IF
            IF(INDCDZ(I).EQ.1)X=X*180./3.14159
            DLT=ABS(VX-X)
            IF(K.EQ.1)THEN
              AMIN=DLT
              NRVN(I)=K
            END IF
            IF(AMIN.GT.DLT)THEN
              AMIN=DLT
              NRVN(I)=K
            END IF
          END DO
        END IF
 100  END DO
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZFDVA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZFDVA
CH
      SUBROUTINE DZFDVA(VX,NRVN,NCOLS,TFMD,TALPH,NUMRW,NUMID)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  Get the nearest value of VX and it's row # for every column
C!:  for a bank with total numbers
C    Inputs    :
C        NCOLS : The total numbers of columns in the bank
C           VX :the setting value
C         TFMD : Column's format
C       INDCDZ : Indicate which columns will be changed from radian to degree
C                translated by common block
C            INDCDZ(I)=1 column I (angle) will be changed from radian to degree
C            INDCDZ(I)=0 column I (angle) will be reset to radian
C       TALPH : Is a flag
C                TALPH='ALEPH' means this bank is ALEPH bank
C                TALPH='BOS  ' means this bank is BOS  bank
C        NUMID : Array
C                NUMID(1,I)=INDEX of the bank for I-th row
C                NUMID(2,I)=NUMBER of the bank for I-th row
C        NUMRW : The number of total rows
C    Outputs   :
C         NRVN :the row # of the nearest value
C    Called by : DZLBKA
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      PARAMETER(NFMTL=5000)
      COMMON/DZXULB/INDCDZ(NFMTL),MARKDZ(NFMTL),NCSTDZ
      CHARACTER *5 TALPH
      CHARACTER *1 TFMD(NCOLS)
      DIMENSION NRVN(NCOLS)
      DIMENSION NUMID(2,NUMRW)
      INCLUDE 'A_BMACRO.INC'
C - Lth integer element of the NRth row of the bank with index ID (BOS bank)
      IBTABL(ID,NR,L) = IW(ID+(NR-1)*IW(ID+1)+L)
C - Lth real element of the NRth row of the bank with index ID (BOS bank)
      RBTABL(ID,NR,L) = RW(ID+(NR-1)*IW(ID+1)+L)
      DO I=1,NCOLS
        IF(TFMD(I).EQ.'A')GO TO 100
        IF(TFMD(I).EQ.'I') THEN
          DO K=1,NUMRW
            IBNK=NUMID(1,K)
            KR=NUMID(2,K)
            IF(TALPH.EQ.'ALEPH')THEN
              NX=ITABL(IBNK,KR,I)
            ELSE
              NX=IBTABL(IBNK,KR,I)
            END IF
            NDLT=ABS(IFIX(VX)-NX)
            IF(K.EQ.1)THEN
              NMIN=NDLT
              NRVN(I)=K
            END IF
            IF(NMIN.GT.NDLT)THEN
              NMIN=NDLT
              NRVN(I)=K
            END IF
          END DO
        ELSE
          DO K=1,NUMRW
            IBNK=NUMID(1,K)
            KR=NUMID(2,K)
            IF(TALPH.EQ.'ALEPH')THEN
              X=RTABL(IBNK,KR,I)
            ELSE
              X=RBTABL(IBNK,KR,I)
            END IF
            IF(INDCDZ(I).EQ.1)X=X*180./3.14159
            DLT=ABS(VX-X)
            IF(K.EQ.1)THEN
              AMIN=DLT
              NRVN(I)=K
            END IF
            IF(AMIN.GT.DLT)THEN
              AMIN=DLT
              NRVN(I)=K
            END IF
          END DO
        END IF
 100  END DO
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZWRVA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZWRVA
CH
      SUBROUTINE DZWRVA(TALPH,NUMRW,NUMID,MTLEN,NCSTDZ,NCEND,
     &NCOLS,TITL,INDCDZ,MARKDZ,NDIG,TFMD,NRVN,NDO,NDG,NRDG)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  list the nearest value of VX (see *DK DZFDVL)and it's row # on terminal
C!:  for a bank with total numbers
C    Inputs    :
C       TALPH : Is a flag
C                TALPH='ALEPH' means this bank is ALEPH bank
C                TALPH='BOS  ' means this bank is BOS  bank
C        NUMID : Array
C                NUMID(1,I)=INDEX of the bank for I-th row
C                NUMID(2,I)=NUMBER of the bank for I-th row
C        NUMRW : The number of total rows
C        MTLEN : The width of one line for listing bank
C        NCSTDZ : The starting column #
C        NCEND : The ending column #
C        NCOLS : The total numbers of columns in the bank
C           VN :the nearest value of VX for every column
C         NRVN :the row # of the nearest value
C          NDO : The flag translated by common block
C                NDO=1 Typing the bank forward
C                NDO=-1 Typing the bank backward
C          NDG : The maxmum digits of the bank numbers
C         NRDG : The maxmum digits of the banks's rows
C         NDIG : The # of digits for every column of the bank
C         TITL : The column's titles
C         TFMD : Column's format
C    Outputs   :
C    Called by : DZLBKA
C ---------------------------------------------------------------------
      CHARACTER *5 TALPH
      CHARACTER *1 TFMD(NCOLS)
      CHARACTER *2 TITL(NCOLS)
      CHARACTER *140 TW,TWBNK
      DIMENSION INDCDZ(NCOLS),MARKDZ(NCOLS)
      DIMENSION NRVN(NCOLS),NDIG(NCOLS)
      CHARACTER *1 TMP(-1:1)
      DATA TMP/'-',':','+'/
      NB=8+NDG+NRDG
      NW=NDG+NRDG+1
C     ............................ list the being listed columns's # and title
      CALL DZBTCA(MTLEN,NCSTDZ,NCEND,NUMRW,NCOLS,TITL,
     &  INDCDZ,MARKDZ,NDIG,NDO,NDG,NRDG,IWID,1)

C     ... Write the nearest value of VX of each column to character variable TW
      CALL DZWRMA(TALPH,NUMRW,NUMID,NCEND,NCOLS,NDIG,TFMD,NRVN,TW)
C     ......................... shift the characters right with NW digits in TW
      CALL DZSHFT(TW,NW,IWID)
      NH=(8+(NDG+NRDG+1))/2
      TW(NH-2:NH+3)='V near'
      TW(NB:NB)='|'
      IF(NCSTDZ.EQ.1)TW(NB:NB)='['
      TWBNK=' '
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
C     ..............  Write the row # of the nearest value of VX of each column
      CALL DZWRNR(NCSTDZ,NCEND,NUMRW,NCOLS,NDIG,TFMD,NRVN,TW)
C     ......................... shift the characters right with NW digits in TW
      CALL DZSHFT(TW,NW,IWID)
      TW(NH-2:NH+2)='Row #'
      TW(NB:NB)='|'
      IF(NCSTDZ.EQ.1)TW(NB:NB)='['
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
      TW(1:1)=' '
      DO I=2,MTLEN
        TW(I:I)='-'
      END DO
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZWRVL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZWRVL
CH
      SUBROUTINE DZWRVL(TALPH,IBNK,MTLEN,NCSTDZ,NCEND,NROWS,NCOLS,
     &TITL,INDCDZ,MARKDZ,NDIG,TFMD,NRVN,NDO)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  list the nearest value of VX (see *DK DZFDVL)and it's row # on terminal
C!:  for a bank with a single number
C    Inputs    :
C        MTLEN : The width of one line for listing bank
C        NCSTDZ : The starting column #
C        NCEND : The ending column #
C        NROWS : The total rows # of the bank
C        NCOLS : The total numbers of columns in the bank
C           VN :the nearest value of VX for every column
C         NRVN :the row # of the nearest value
C          NDO : The flag translated by common block
C                NDO=1 Typing the bank forward
C                NDO=-1 Typing the bank backward
C         NDIG : The # of digits for every column of the bank
C         TITL : The column's titles
C         TFMD : Column's format
C    Outputs   :
C    Called by : DZLBNK
C ---------------------------------------------------------------------
      CHARACTER *5 TALPH
      CHARACTER *1 TFMD(NCOLS)
      CHARACTER *2 TITL(NCOLS)
      CHARACTER *140 TW,TWBNK
      DIMENSION INDCDZ(NCOLS),MARKDZ(NCOLS)
      DIMENSION NRVN(NCOLS),NDIG(NCOLS)
      CHARACTER *1 TMP(-1:1)
      DATA TMP/'-',':','+'/
C     ............................ list the being listed columns's # and title
      CALL DZBTNC(MTLEN,NROWS,NCOLS,TITL,INDCDZ,MARKDZ,
     &  NCSTDZ,NCEND,NDIG,NDO)
C     ... Write the nearest value of VX of each column to character variable TW
      CALL DZWRMX(TALPH,IBNK,NCEND,NROWS,NCOLS,NDIG,TFMD,NRVN,TW)
      TW(2:5)='Vnea'
      TWBNK=' '
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
C     ..............  Write the row # of the nearest value of VX of each column
      CALL DZWRNR(NCSTDZ,NCEND,NROWS,NCOLS,NDIG,TFMD,NRVN,TW)
      TW(2:5)='ROW#'
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
      TW(1:1)=' '
      DO I=2,MTLEN
        TW(I:I)='-'
      END DO
      TWBNK(1:MTLEN+1)=TW(1:MTLEN)//TMP(NDO)
      CALL DLWRBA(TWBNK(1:MTLEN+1))
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZFMTL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZFMTL
CH
      SUBROUTINE DZFMTL(TBNK,NCOLS,TITL,TFMD,IERR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  Get the column's titles and formats of the bank
C    Inputs    :
C        NCOLS : The total numbers of columns in the bank
C         TBNK : The bank name
C    Outputs   :
C         TITL : The column's titles
C         TFMD : Column's format
C         IERR :
C     IERR=0 O.K. we've got the titles and formats
C     IERR=1 Read bank columns title file 'AL2$USER2:[XUR.PB]TITFM.DAT' error
C     IERR=2 The bank title was not existed
C     IERR=3 The number of format and title of columns for the bank
C            are not equal to the number of columns
C    Called by : DZLISB,DZLISA
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      CHARACTER *49 T49
      CHARACTER *4 TBNK
      CHARACTER *2 TITL(NCOLS)
      CHARACTER *1 TFMD(NCOLS)
      CHARACTER *80 TR,TB
      CHARACTER *15 TFIL
C     'DALI_C5.BNK_FMT'
      IERR=0
      TB=' '
      CALL DZEMPT(NCOLS,2,TITL)
      CALL DZEMPT(NCOLS,1,TFMD)
C     ............ Because the number of formats for banks 'HSCI','SGMN','T2FC'
C     .......................... in SBANK.LBF of official version in wrong now,
C     ................................. We user both of following format file :
C     ........................... (1) 'al2$user2:[xur.keep]ttlfm_hst.bank'
C     ................................ to get formats of 'HSCI','SGMN','T2FC'
C     ........................... (2) 'al2$user2:[xur.keep]ttlfm.bank'
C     ................................ to get formats of other bank
C     ................. If SBANK.LBF of official version is modified you should
C     ...................... @AL2$USER2:[XUR.PB]DZTLFM to get new 'ttlfm.bank'.
C      IF(TBNK.EQ.'HSCI'.OR.TBNK.EQ.'SGMN'.OR.TBNK.EQ.'T2FC')THEN
C        LTF=LTF2
C        TFIL(1:LTF)=TFIL2
C      ELSE
C        LTF=LTF1
C        TFIL(1:LTF)=TFIL1
C      END IF
      TFIL=TFILDC//'BNK_FMT'
      CALL DGOPEN(NUNIDU,TFIL,-2,*999,IER)
    1 READ(NUNIDU,1000,END=998)TR
 1000 FORMAT(A)
      IF(TR.EQ.TB)GO TO 1
      IF(TR(3:6).NE.TBNK)GO TO 1
      NC=0
      READ(TR(8:9),1001,ERR=999)NX
 1001 FORMAT(BN,I2)
      NUM=NX
      IS=10
      DO I=1,NUM
        NC=NC+1
        IF(NC.LE.NCOLS)THEN
          TITL(NC)=TR(IS+1:IS+2)
          TFMD(NC)=TR(IS+4:IS+4)
          IS=IS+5
        END IF
      END DO
      IF(NUM.LT.14)GO TO 997
    2 READ(NUNIDU,1000)TR
      IF(TR(2:2).NE.'&')THEN
        GO TO 997
      ELSE
        READ(TR(8:9),1001,ERR=999)NX
        NUM=NX
        IS=10
        DO I=1,NUM
          NC=NC+1
          IF(NC.LE.NCOLS)THEN
            TITL(NC)=TR(IS+1:IS+2)
            TFMD(NC)=TR(IS+4:IS+4)
            IS=IS+5
          END IF
        END DO
        IF(NUM.LT.14)THEN
          GO TO 997
        ELSE
          GO TO 2
        END IF
      END IF
  997 CLOSE(UNIT=NUNIDU)
      IF(NC.EQ.NCOLS)RETURN
      IF(NC.LT.NCOLS)THEN
        NCOLD=NCOLS
C       ..........If the # of formats less then # of columns repeat the formats
        CALL DZFRMT(TBNK,NCOLS,NC,TFMD,TITL)
C       .......................... Setting a character variable T49 equal empty
C        T49=' '
C        T49(1:46)='The # of columns format of '//TBNK//' is not right.'
C        CALL DWRT(T49)
      ELSE IF(TBNK.EQ.'X1TI')THEN
C        T49(1:49)='Formats of '//TBNK//
C     &  ' is wrong, we use the real formats'
C        CALL DWRT(T49)
        DO I=1,11
          IF(I.EQ.1.OR.I.GT.9)THEN
            TFMD(I)='I'
          ELSE
            TFMD(I)='A'
          END IF
        END DO
      ELSE
        T49(1:49)=' # of Format of '//TBNK//
     &  ' is more than # of columns.  '
        CALL DWRT(T49)
      END IF
      RETURN
  999 CLOSE(UNIT=NUNIDU)
C     ............................ Setting a character variable T49 equal empty
      T49=TFIL//' read error!'
      CALL DWRT(T49)
      IERR=1
      RETURN
  998 CLOSE(UNIT=NUNIDU)
      IF(TBNK.EQ.'RTLS')THEN
C        T49(1:49)='The format of '//TBNK//
C     &  ' not found, we set them to "I".'
C        CALL DWRT(T49)
        DO I=1,NCOLS
          TFMD(I)='I'
        END DO
       IERR=0
      RETURN
      END IF
C     ............................ Setting a character variable T49 equal empty
      T49='The formats of '//TBNK//' were not found.'
      CALL DWRT(T49)
      IERR=2
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZFRMT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZFRMT
CH
      SUBROUTINE DZFRMT(TBNK,NCOLS,NC,TFMD,TITL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by Rongfen XU                     15-DEC-1991
C
C!:  If the # of formats less then the # of columns repeat the formats
C    Inputs    :
C        NCOLS : The total numbers of columns in the bank
C         TFMD : The column's formats
C         TITL : The column's titles
C           NC : the # of formats
C    Outputs   :
C         TFMD : The column's formats
C         TITL : The column's titles
C    Called by : DZGTFM
C ---------------------------------------------------------------------
      PARAMETER(NFMTL=5000)
      CHARACTER *4 TBNK
      CHARACTER *2 TITL(NCOLS),TL(NFMTL)
      CHARACTER *1 TFMD(NCOLS),FM(NFMTL)
C     ................................... Default I is used for TFMD
C      IERR1=0
      MD=MOD(NCOLS,NC)
C     .............. IF(MD.EQ.2)NCOLS=NCOLS-2 for some special bank
      IF(MD.EQ.2)NCOLS=NCOLS-2
      NK=NCOLS/NC
      DO I=1,NC
        FM(I)=TFMD(I)
        TL(I)=TITL(I)
      END DO
      IC=NC
      DO N=1,NK
        DO I=1,NC
          IC=IC+1
          IF(IC.LE.NCOLS)THEN
            TITL(IC)=TL(I)
            TFMD(IC)=FM(I)
          END IF
        END DO
      END DO
      RETURN
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZERBK
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DZERBK
CH
      SUBROUTINE DZERBK(LTR,TR,IS,IE,IEMPT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    created by Rongfen XU                     15-FEB-1992
C
C!:  Take the blanks away from the beginnig and the endding of a string
C!:  and give the empty flag if the string is empty
C    Inputs    :
C           TR : A character string
C          LTR : The digits of the character string TR
C    Outputs   :
C           IS : The position of the first character in TR
C           IE : The position of the last character in TR
C        IEMPT : IEMPT=1 if the character string is empty
C    Called by : DZLIBK
C ---------------------------------------------------------------------
      CHARACTER *(*) TR
      IEMPT=0
      DO I=1,LTR
        IF(TR(I:I).NE.' ')GO TO 10
      END DO
      IEMPT=1
      RETURN
   10 IS=I
   20 DO I=LTR,1,-1
        IF(TR(I:I).NE.' ')GO TO 30
      END DO
   30 IE=I
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DZUTOL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DZUTOL
CH
      SUBROUTINE DZUTOL(TR,LTR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    created by Rongfen XU                     15-FEB-1992
C
C!:  Change a character string from upper case to lower case
C    Inputs    :
C           TR : A character string
C          LTR : The digits of TR
C    Outputs   :
C           TR : A character string with upper case
C    Called by : DPDBAS,DPSSTR
C ---------------------------------------------------------------------
      CHARACTER *(*) TR
      TOLOW=32
      DO I=1,LTR
        IASCTR=ICHAR(TR(I:I))
        IF(IASCTR.GE.65 .AND. IASCTR.LE.90)THEN
          IASCTR=IASCTR+TOLOW
          TR(I:I)=CHAR(IASCTR)
        END IF
      END DO
      END
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DPEMPT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPEMPT
CH
      SUBROUTINE DZEMPT(LN,IWD,T)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    created by Rongfen XU                     15-FEB-1992
C
C!:  Setting a character variable T equal empty
C    Inputs    :
C           LN : The dimension # of the character variable T
C          IWD : The digits of the character variable T
C            T : The character variable
C    Outputs   :
C    Called by : DZFMTL,DZBTNC,DZBTCA
C ---------------------------------------------------------------------
      CHARACTER *(*) T(LN)
      DO K=1,IWD
        T(1)(K:K)=' '
      END DO
      IF(LN.EQ.1)RETURN
      DO I=2,LN
        T(I)=T(1)
      END DO
      END
