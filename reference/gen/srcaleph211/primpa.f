      SUBROUTINE PRIMPA
C -----------------------------------------------------------------
C - O.Callot and F.Ranjard - 860929
C! Print IMPAct Bank in Readable Format
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      CHARACTER*4 CHAINT
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
C --------------------------------------------------------------
C
      LOUT = IW(6)
C
      NAMI = NAMIND('IMPA')
      IF (NAMI .EQ. 0) THEN
         WRITE (LOUT,'(/1X,''+++PRIMPA+++ NO IMPA bank - RETURN'')')
         RETURN
      ENDIF
C
      JIMPA = NAMI + 1
 50   JIMPA = IW(JIMPA-1)
      IF(JIMPA.NE.0) THEN
         WRITE(LOUT,1000) IW(JIMPA-2),LROWS(JIMPA)
         KIMPA = JIMPA +LMHLEN
         DO 60 J=1,LROWS(JIMPA)
            WRITE(LOUT,1010) CHAINT(IW(KIMPA+1))
     &                      ,(RW(KIMPA+K),K=2,LCOLS(JIMPA))
   60    KIMPA = KIMPA + LCOLS(JIMPA)
         GOTO 50
      ENDIF
C
      RETURN
 1000 FORMAT(/' +++PRIMPA+++ track# ',I4,3X,I2,' Impacts')
 1010 FORMAT(10X,A4,2X,3F10.3,2X,3F8.4,2X,F10.4)
      END
