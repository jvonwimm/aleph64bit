      SUBROUTINE MINPYE
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill PYER bank from DVER.
C
C     Author: Stephen Haywood      03-Apr-90
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
      PARAMETER(JDVEX0=1,JDVEY0=2,JDVEZ0=3,JDVEFP=4,JDVEMV=5,JDVEDT=6,
     +          LDVERA=6)
      PARAMETER(JPYETY=1,JPYEVX=2,JPYEVY=3,JPYEVZ=4,JPYEVM=5,JPYEC2=11,
     +          JPYEDF=12,LPYERA=12)
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
*     PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
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
      KDVER = NLINK('DVER',100)
      IF (KDVER.LE.0) THEN
         CALL MINUPD('DVER')
         KDVER = NLINK('DVER',100)
         IF (KDVER.LE.0) RETURN
      ENDIF
C
C++   Determine how many main vertices should be created.
C
      NPYER = 0
      DO I=1,LROWS(KDVER)
         IF(ITABL(KDVER,I,JDVEMV).GT.0) NPYER = NPYER + 1
      ENDDO
      IF(NPYER.LE.0) RETURN
C
C++   Create PYER bank.
C
      LEN = LMHLEN + LPYERA * NPYER
      CALL AUBOS('PYER',0,LEN, KPYER,IGARB)
      CALL BLIST(IW,'S+','PYER')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDVER = NLINK('DVER',100)
      ENDIF
      IW(KPYER+LMHCOL) = LPYERA
      IW(KPYER+LMHROW) = NPYER
C
C++   Fill PYER bank from DVER.
C
      DO 100 I=1,NPYER
         IW(KROW(KPYER,I)+JPYETY) = 1
         RW(KROW(KPYER,I)+JPYEVX) = FLOAT(ITABL(KDVER,I,JDVEX0))/DFACTM
         RW(KROW(KPYER,I)+JPYEVY) = FLOAT(ITABL(KDVER,I,JDVEY0))/DFACTM
         RW(KROW(KPYER,I)+JPYEVZ) = FLOAT(ITABL(KDVER,I,JDVEZ0))/DFACTM
  100 CONTINUE
C
      RETURN
      END
