      SUBROUTINE YTOPRT(IPRL)
C
C----------------------------------------------------------*
C!    Printout of reconstructed vertices
C!    To be called once per event after YTOPOL
CKEY YTOP PRINT
C!    Author :     M. Bosman   jan 30 1989
C!    Modified:    S. Wasserbaech 9 jan 1990
C!
C!
C!    Description
C!    ===========
C!    This routine prints the reconstructed primary vertex
C!    and lists the tracks that are used in the vertex
C!    IPRLV = 1 : prints only the position of the vertex
C!            2 : lists the tracks used in the vertex
C!                and the CHI2
C!
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JPYETY=1,JPYEVX=2,JPYEVY=3,JPYEVZ=4,JPYEVM=5,JPYEC2=11,
     +          JPYEDF=12,LPYERA=12)
      PARAMETER(JPYFTN=1,JPYFVN=2,LPYFRA=2)
C!---------------------------------------------------------*
      EXTERNAL NAMIND
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
C!---------------------------------------------------------*
      LOUT = IW(6)
      KPYER = IW(NAMIND('PYER'))
      IF(KPYER.NE.0) THEN
        NTR = (ITABL(KPYER,1,JPYEDF)+3)/2
        IF (ITABL(KPYER,1,JPYETY) .EQ. 3) NTR = NTR - 1
        WRITE(LOUT,1000)
     +    (RTABL(KPYER,1,JPYEVX+I-1),I=1,3), NTR
      ENDIF
      IF(IPRL.GE.2.AND.KPYER.NE.0) THEN
        WRITE(LOUT,1001)
     +    (RTABL(KPYER,1,JPYEVM+I-1),I=1,6)
        WRITE(LOUT,1002)
     +    RTABL(KPYER,1,JPYEC2),ITABL(KPYER,1,JPYEDF)
        WRITE (LOUT,1003) (ITABL(KPYER,1,JPYETY) .EQ. 3)
        KPYFR = IW(NAMIND('PYFR'))
        IF(KPYFR.NE.0) THEN
          WRITE(LOUT,1004)
     +      (ITABL(KPYFR,I,JPYFTN),I=1,LROWS(KPYFR))
          WRITE(LOUT,1005)
        ENDIF
      ENDIF
      RETURN
 1000 FORMAT(/,' ------YTOPOL : vertex reconstruction ------'/,
     +       ' Primary Vertex (X,Y,Z) :',3F10.5,' from ',I2,' tracks',
     +       /,1X,43('-'))
 1001 FORMAT(' Variances in sequence X:XY:Y:XZ:YZ:Z '/,6E10.3)
 1002 FORMAT(' Chi2 ',F10.3,'  for ',I2,' degrees of freedom')
 1003 FORMAT(' Beam xy position used: ',L1)
 1004 FORMAT(' List of tracks that form the primary vertex '/,5(20I3/))
 1005 FORMAT(1X,43('-'))
      END
