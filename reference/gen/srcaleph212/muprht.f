      SUBROUTINE MUPRHT
C
C-----------------------------------------------------------------
C
C T.Wang
C
C! print out 'MUHT'
C
      SAVE
C
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
C --------------------------------------------------------------
C
      LOUT = IW(6)
C
      KMUHT = NLINK ('MUHT',0)
      IF( KMUHT .NE. 0 ) THEN
         WRITE(LOUT,101)(IW(KMUHT+I),I=0,LMHROW)
         NSGNL = LROWS (KMUHT)
         IF( NSGNL .EQ. 0 )GOTO 900
         LWRDS = LCOLS (KMUHT)
         WRITE(LOUT,102)
         DO 10 J=1,NSGNL
         KPNT = KROW (KMUHT,J)
         WRITE(LOUT,103)(IW(KPNT+I),I=1,LWRDS)
   10    CONTINUE
  101    FORMAT(2X,'+++MUPRHT+++  ******** Bank MUHT data : ********'//
     *       2X,'# of words        -- ',I8/
     *       2X,'# of words/signal -- ',I8/
     *       2X,'# of signals      -- ',I8//)
  102    FORMAT(2X,'  Track #   electronics     strip plane  strip #'/
     *       2X,'            module  #           #               '/)
  103    FORMAT(3X,I5,8X,I5,8X,I5,8X,I5)
      ENDIF
C
  900 RETURN
      END
