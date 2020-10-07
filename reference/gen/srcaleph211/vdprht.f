      SUBROUTINE VDPRHT
C
C... VDPRHT  1.00   860611                       F. Forti
C
C! Print out  'VDHT' bank in readable format.
C
C-----------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
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
C -------------------------------------------------------------------
C
      LOUT = IW(6)
C
      NR = 0
      IND = NLINK ('VDHT',0)
      IF (IND.GT.0) THEN
         LW = LCOLS (IND)
         LHIT = LROWS (IND)
         WRITE (LOUT,1000) LHIT,LW
         KND = IND + LMHLEN
         DO 100 IH=1,LHIT
         WRITE (LOUT,1010) (IW(KND+I),I=1,3),(RW(KND+J),J=4,LW)
 100     KND = KND + LW
      ENDIF
 1000 FORMAT(//' +++VDPRHT+++ VDHT bank printout'/
     &        '    Number of hits :',I4,
     &        '   Number of words per hit :',I4//
     &        6X,'Track  Layer  Phi        XIN      YIN      ZIN',
     &        6X,' XOUT     YOUT     ZOUT      ENERGY')
 1010 FORMAT(7X,I4,6X ,I1,  3X,I2 ,2(2X,3(2X,F7.3)),4X,E8.2,(G8.3))
  999 CONTINUE
      RETURN
      END
