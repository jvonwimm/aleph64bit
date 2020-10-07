      INTEGER FUNCTION ALRUNH ( NRUN,NEXPE,IRTYP)
C --------------------------------------------------------------------
C - F.Ranjard - 861003
C!  Build the RUNH bank
C - Input :  NRUN   = run #
C            NEXPE  = experiment #
C            IRTYP  = run type
C - Output : ALRUNH = RUNH bank index
C                     0 means not enough space to book the bank
C  --------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C - LRUNH  = 'RUNH' bank length
C   NCENT  = century #
      PARAMETER (LRUNH = 5, NCENT = 19)
C --------------------------------------------------------------------
C - get date and time
      CALL DATIME (JDAT,JTIM)
C
C - Book 'RUNH', NR=0 bank
       CALL AUBOS ('RUNH',0,LRUNH,JRUNH,IGARB)
       IF (JRUNH.EQ.0) GOTO 999
       CALL BKFMT ('RUNH','(I)')
       IW(JRUNH+1)  = NEXPE
       IW(JRUNH+2)  = NRUN
       IW(JRUNH+3)  = IRTYP
       IW(JRUNH+4)  = JDAT
       IW(JRUNH+5)  = JTIM
C
  999  CONTINUE
       ALRUNH = JRUNH
       RETURN
       END
