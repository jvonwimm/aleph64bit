      SUBROUTINE EC4TRA (JTRA,EC4TR1,EC4TR2,EC4TR3)
C----------------------------------------------------------------
C  Marc Verderi                                        2-10-94
C! Reads the energies in the four central storeys associated to
C  the track JTRA
C---------------------------------------------------------------
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
      EC4TR1 = -1.
      EC4TR2 = -1.
      EC4TR3 = -1.
      JEIDT = IW (NAMIND('EIDT'))
      IF (JEIDT.LE.0) GOTO 999
      NEIDT = LROWS(JEIDT)
      DO I = 1,NEIDT
         IF (ITABL(JEIDT,I,14).EQ.JTRA) THEN
            EC4TR1 = RTABL(JEIDT,I,11)
            EC4TR2 = RTABL(JEIDT,I,12)
            EC4TR3 = RTABL(JEIDT,I,13)
         ENDIF
      ENDDO
999   CONTINUE

      END
