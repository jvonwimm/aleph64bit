      SUBROUTINE MUPRDI
C
C-----------------------------------------------------------------
C T.Wang
C
C! print out 'MUDI'
C
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
C ------------------------------------------------------------------
C
      LOUT = IW(6)
C
      KMUDI = NLINK ('MUDI',0)
      IF( KMUDI .NE. 0 ) THEN
         WRITE(LOUT,101)(IW(KMUDI+I),I=0,LMHROW)
         NDGTZ = LROWS (KMUDI)
         IF( NDGTZ .EQ. 0 )GOTO 900
         WRITE(LOUT,102)
         DO 10 J=1,NDGTZ
            KPNT = KROW (KMUDI,J)
            WRITE(LOUT,103)(IW(KPNT+I),I=1,4)
   10    CONTINUE
  101    FORMAT(//2X,'+++MUPRDI+++   ******* Bank MUDI data : *******'//
     *       2X,'# of words        -- ',I8/
     *       2X,'# of words/digit  -- ',I8/
     *       2X,'# of digits       -- ',I8//)
  102    FORMAT(2X,
     *'    electronics    strip       cluster          1st address'/
     *       2X,
     *'    module  #      layer#      length(0-7)      of cluster '/)
  103    FORMAT(5X,I5,8X,I5,10X,I5,10X,I5)
      ENDIF
C
      KMDT1 = NLINK ('MUDT',1)
      IF (KMDT1 .NE. 0) THEN
         KMDT2 = IW(KMDT1-1)
         KMDT3 = IW(KMDT2-1)
         NW = LCOLS (KMDT1)
         ND = LROWS (KMDT1)
         WRITE(LOUT,104)NW,ND
         WRITE(LOUT,105)(IW(KMDT1+LMHLEN+I),I=1,ND)
  104    FORMAT(//2X,'*** Bank MUDT-1 -- # of tracks per digit ***',/
     *         3X,'# of words/digit ',I5,3X,'# of digits ',I5)
  105    FORMAT(10(I8))
         NW = LCOLS (KMDT2)
         ND = LROWS (KMDT2)
         WRITE(LOUT,106)NW,ND
         WRITE(LOUT,105)(IW(KMDT2+LMHLEN+I),I=1,ND)
  106    FORMAT(//2X,'*** Bank MUDT-2 -- Address in track list ***',/
     *         3X,'# of words/digit ',I5,3X,'# of digits ',I5)
         NW = LCOLS (KMDT3)
         ND = LROWS (KMDT3)
         WRITE(LOUT,108)NW,ND
         WRITE(LOUT,105)(IW(KMDT3+LMHLEN+I),I=1,ND)
  108    FORMAT(//2X,'*** Bank MUDT-3 -- Track list            ***',/
     *         3X,'# of words/digit ',I5,3X,'# of digits ',I5)
      ENDIF
C
      KMTD1 = NLINK ('MUTD',1)
      IF (KMTD1 .NE. 0) THEN
         KMTD2 = IW(KMTD1-1)
         KMTD3 = IW(KMTD2-1)
         KMTD4 = IW(KMTD3-1)
         NW = LCOLS (KMTD1)
         ND = LROWS (KMTD1)
         WRITE(LOUT,110)NW,ND
         WRITE(LOUT,105)(IW(KMTD1+LMHLEN+I),I=1,ND)
  110    FORMAT(//2X,'*** Bank MUTD-1 -- list of Galeph tracks ***',/
     *         3X,'# of words/track ',I5,3X,'# of tracks ',I5)
         NW = LCOLS (KMTD2)
         ND = LROWS (KMTD2)
         WRITE(LOUT,112)NW,ND
         WRITE(LOUT,105)(IW(KMTD2+LMHLEN+I),I=1,ND)
  112    FORMAT(//2X,'*** Bank MUTD-2 -- # of digits per track ***',/
     *         3X,'# of words/track ',I5,3X,'# of tracks ',I5)
         NW = LCOLS (KMTD3)
         ND = LROWS (KMTD3)
         WRITE(LOUT,114)NW,ND
         WRITE(LOUT,105)(IW(KMTD3+LMHLEN+I),I=1,ND)
  114    FORMAT(//2X,'*** Bank MUTD-3 -- Address in digit list  ***',/
     *         3X,'# of words/track ',I5,3X,'# of tracks ',I5)
         NW = LCOLS (KMTD4)
         ND = LROWS (KMTD4)
         WRITE(LOUT,116)NW,ND
         WRITE(LOUT,105)(IW(KMTD4+LMHLEN+I),I=1,ND)
  116    FORMAT(//2X,'*** Bank MUTD-4 -- Digit list             ***',/
     *         3X,'# of words/track ',I5,3X,'# of tracks ',I5)
      ENDIF
  900 RETURN
      END
