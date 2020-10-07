      INTEGER FUNCTION ALTABL (NAME,NCOL,NROW,TABL,FMT,LIST)
C ------------------------------------------------------------
C - J.Boucrot - F.Ranjard - 870516
C
C! Book tabular bank 'NAME',NR=0
C  then Fill it with the array given in a tabular format,
C       Give a format and Put on a list
C
C - structure: INTEGER FUNCTION subprogram
C              User Entry Name: ALTABL
C              External References: AUBOS(ALEPHLIB), BKFMT/BLIST(BOS77)
C                                   UCOPY(CERNLIB)
C              Comdecks referenced: BCS
C
C - Usage   : JTABL   = ALTABL (NAME,NCOL,NROW,TABL,FMT,LIST)
C - Input   : NAME    = bank name (up to 4 characters)
C             NCOL    = # of columns
C             NROW    = # of rows
C             TABL    = array containing the table (REAL or INTEGER)
C             FMT     = bank format (character string)
C             LIST    = list on wich the bank must be put onto
C - Output  : ALTABL  = bank index
C   --------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      DIMENSION TABL(*)
      CHARACTER*(*) FMT,LIST,NAME
      CHARACTER LP*2
C -----------------------------------------------------------------
      IF (NCOL*NROW .EQ.0) THEN
         ALTABL = -1
         GOTO 999
      ELSE
         CALL AUBOS (NAME,0,NCOL*NROW+LMHLEN,JTABL,IGARB)
         IF (JTABL.GT.0) THEN
            IW(JTABL+LMHCOL) = NCOL
            IW(JTABL+LMHROW) = NROW
            CALL UCOPY (TABL(1),RW(JTABL+LMHLEN+1),NCOL*NROW)
            CALL BKFMT (NAME,FMT)
            LP = LIST//'+'
            CALL BLIST (IW,LP,NAME)
         ENDIF
         ALTABL = JTABL
      ENDIF
C
 999  CONTINUE
      END
