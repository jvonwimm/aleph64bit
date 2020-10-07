      INTEGER FUNCTION ALRUNR (NEXPE,NRUN)
C -----------------------------------------------------------
C - F.Ranjard - 880222
C!  Build the RUNR bank
C - Input : NEXPE  = experiment #
C           NRUN   = run #
C - Output: ALRUNR = RUNR bank index
C                    0 means not enough space to book the bank
C   -----------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C - LRUNR = 'RUNR' bank length
      PARAMETER (LRUNR = 2)
C ------------------------------------------------------------
C - Book 'RUNR',NR=0 bank
      CALL AUBOS ('RUNR',0,LRUNR,JRUNR,IGARB)
      IF (JRUNR .EQ. 0) GOTO 999
         CALL BKFMT ('RUNR','(I)')
         IW(JRUNR+1) = NEXPE
         IW(JRUNR+2) = NRUN
C
 999  CONTINUE
      ALRUNR = JRUNR
      END
