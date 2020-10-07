      SUBROUTINE ECPRTR
C-------------------------------------------------------------------
C      O.CALLOT   29-JAN-86
C
C! Print trigger banks ETTR and EWTR
C
C-------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      DIMENSION IPVAL(12),LIMTH(12)
      CHARACTER*10 NAMOD(3)
C
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
      DATA LIMTH / 8,24,46,50,88,114,140,178,182,204,220,228/
      DATA NAMOD / 'End Cap A' , 'Barrel' , 'End Cap B' /
C ----------------------------------------------------------
      LOUT = IW(6)
C
      JETTR = NLINK ('ETTR',0)
      IF( JETTR .EQ. 0 ) GOTO 500
      KETTR = JETTR + LMHLEN
      WRITE(LOUT,1000) LIMTH
      DO 100 IPH = 1,24
        IND = KETTR + 1
        DO 110 J=1,12
          IPVAL(J) = NINT( .001 * (IW(IND) + IW(IND+1) + IW(IND+2)) )
          IND = IND + 72
  110   CONTINUE
        WRITE(LOUT,1010) IPH,IPVAL
        KETTR = KETTR + 3
  100 CONTINUE
C
  500 CONTINUE
      JEWTR = NLINK ('EWTR',0)
      IF( JEWTR .EQ. 0 ) GOTO 900
      KEWTR = JEWTR + LMHLEN
      WRITE(LOUT,1100)
      DO 510 JM=1,3
        DO 520 IM = 1,12
          IPVAL(IM) = NINT( .001 * (IW(KEWTR+1) + IW(KEWTR+2)) )
          KEWTR = KEWTR + 2
  520   CONTINUE
        WRITE(LOUT,1110) NAMOD(JM),IPVAL
  510 CONTINUE
  900 CONTINUE
      RETURN
 1000 FORMAT (/1X,'+++ECPRTR+++ ETTR tower trigger print out in MEV'//
     + ' Theta limits   0',12(4X,I3)/)
 1010 FORMAT(' Phi bin nb ',I2,1X,12I7)
 1100 FORMAT (/1X,'+++ECPRTR+++ EWTR wire trigger print out in MEV'//)
 1110 FORMAT(2X,A10,2X,12I7)
      END
