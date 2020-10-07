      SUBROUTINE MINVER
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill vertex bank DVER for Mini-DST.
C
C     Author: Stephen Haywood      22-Jan-90
C
C     Input  : PYER bank
C     Output : DVER bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
*     PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
      PARAMETER(JPYETY=1,JPYEVX=2,JPYEVY=3,JPYEVZ=4,JPYEVM=5,JPYEC2=11,
     +          JPYEDF=12,LPYERA=12)
      PARAMETER(JDVEX0=1,JDVEY0=2,JDVEZ0=3,JDVEFP=4,JDVEMV=5,JDVEDT=6,
     +          LDVERA=6)
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
C
C++   Identify the number of vertices.
C++   If no vertices, return without creating DVER bank.
C
      KPYER = NLINK('PYER',0)
      IF(KPYER.GT.0) THEN
         NPYER = LROWS(KPYER)
      ELSE
         NPYER = 0
      ENDIF
      IF(NPYER.LE.0) RETURN
C
C++   Create the DVER bank.
C
      NDVER = NPYER
      LEN = LMHLEN + LDVERA * NDVER
      CALL AUBOS('DVER',0,LEN, KDVER,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINVER: Cannot create DVER bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KPYER = NLINK('PYER',0)
      ENDIF
      IW(KDVER+LMHCOL) = LDVERA
      IW(KDVER+LMHROW) = NDVER
C
C++   Loop over the PYER bank and fill the DVER bank.
C
      DO 100 I=1,NPYER
C
C++      Vertex position.
C
         IW(KROW(KDVER,I)+JDVEX0) = NINT(DFACTM * RTABL(KPYER,I,JPYEVX))
         IW(KROW(KDVER,I)+JDVEY0) = NINT(DFACTM * RTABL(KPYER,I,JPYEVY))
         IW(KROW(KDVER,I)+JDVEZ0) = NINT(DFACTM * RTABL(KPYER,I,JPYEVZ))
C
C++      Vertex fit probability.
C
         CHISQ = RTABL(KPYER,I,JPYEC2)
         NDEG  = ITABL(KPYER,I,JPYEDF)
         PRCHI = PROB(CHISQ,NDEG)
         IW(KROW(KDVER,I)+JDVEFP) = NINT(1000.*PRCHI)
C
C++      Set main vertex bit.
C
         IW(KROW(KDVER,I)+JDVEMV) = 1
C
C++      Link to parent track - zero for main vertices.
C
         IW(KROW(KDVER,I)+JDVEDT) = 0
C
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DVER')
C
      RETURN
      END
