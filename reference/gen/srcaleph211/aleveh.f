      INTEGER FUNCTION ALEVEH ( NEVT,NEXPE,NRUN,BEAM,IRTYP,ISTAT)
C --------------------------------------------------------------------
C - F.Ranjard - 861003
C!  Build the EVEH bank
C - Input  : NEVT   = event #
C            NEXPE  = experiment #
C            NRUN   = run #
C            BEAM   = beam energy
C            IRTYP  = run type
C            ISTAT  = status word
C - Output : ALEVEH = EVEH bank index
C                     0 means not enough space to book the bank
C
c -------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C   LEVEH  = 'EVEH' bank length
      PARAMETER (LEVEH = 13)
C ---------------------------------------------------------------------
C - Book the bank 'EVEH',NR=0 with length LEVEH
      CALL AUBOS ('EVEH',0,LEVEH,JEVEH,IGARB)
      IF (JEVEH.EQ.0) GOTO 999
      CALL BKFMT ('EVEH','(I)')
C
      IW (JEVEH+1)  = NEXPE
      IW (JEVEH+2)  = NRUN
      IW (JEVEH+3)  = IRTYP
      IW (JEVEH+4)  = 0
      IW (JEVEH+5)  = 0
      IW (JEVEH+6)  = NEVT
      IW (JEVEH+11) = 1
      IW (JEVEH+12) = ISTAT
      IW (JEVEH+13) = BEAM * 10.**6
C
 999  CONTINUE
      ALEVEH = JEVEH
      RETURN
      END
