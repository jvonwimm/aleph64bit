      SUBROUTINE MINPYF
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill PYFR bank from DTRA.
C
C     Author: Stephen Haywood      03-Apr-90
C
C     DVER bank also used.
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
      PARAMETER(JDTRCH=1,JDTRP0=2,JDTRTH=3,JDTRPH=4,JDTRD0=5,JDTRZ0=6,
     +          JDTRER=7,JDTRTF=12,JDTRHO=13,JDTRHM=14,JDTRVB=15,
     +          JDTRQF=16,JDTREA=17,JDTRVI=27,LDTRAA=27)
      PARAMETER(JDVEX0=1,JDVEY0=2,JDVEZ0=3,JDVEFP=4,JDVEMV=5,JDVEDT=6,
     +          LDVERA=6)
      PARAMETER(JPYFTN=1,JPYFVN=2,LPYFRA=2)
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
      KDTRA = NLINK('DTRA',100)
      IF (KDTRA.LE.0) THEN
         CALL MINUPD('DTRA')
         KDTRA = NLINK('DTRA',100)
         IF (KDTRA.LE.0) RETURN
      ENDIF
      NFRFT = LROWS(KDTRA)
      IF (NFRFT.LE.0) RETURN
C
      KDVER = NLINK('DVER',100)
      IF (KDVER.LE.0) THEN
         CALL MINUPD('DVER')
         KDVER = NLINK('DVER',100)
         IF (KDVER.LE.0) RETURN
      ENDIF
      KDVER = NLINK('DVER',0)
      IF (KDVER.LE.0) RETURN
C
C++   Determine how many main vertices will be created.
C
      NPYER = 0
      DO I=1,LROWS(KDVER)
         IF(ITABL(KDVER,I,JDVEMV).GT.0) NPYER = NPYER + 1
      ENDDO
      IF(NPYER.LE.0) RETURN
C
C++   Create PYFR bank.
C++   Must estimate the maximum number of rows.
C
      NPYFR = NFRFT * NPYER
      LEN = LMHLEN + LPYFRA * NPYFR
      CALL AUBOS('PYFR',0,LEN, KPYFR,IGARB)
      CALL BLIST(IW,'S+','PYFR')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDTRA = NLINK('DTRA',100)
      ENDIF
      IW(KPYFR+LMHCOL) = LPYFRA
      IW(KPYFR+LMHROW) = NPYFR
C
C++   Fill PYFR bank.
C
      NPYFR = 0
      DO 100 I=1,NFRFT
         MAPVB = ITABL(KDTRA,I,JDTRVB)
         DO 150 IDVER=1,NPYER
            MATCH = JBIT(MAPVB,IDVER)
            IF(MATCH.NE.1) GOTO 150
            NPYFR = NPYFR + 1
            IW(KROW(KPYFR,NPYFR)+JPYFTN) = I
            IW(KROW(KPYFR,NPYFR)+JPYFVN) = IDVER
  150    CONTINUE
  100 CONTINUE
C
C++   Compress bank to required size.
C
      LEN = LMHLEN + LPYFRA * NPYFR
      CALL AUBOS('PYFR',0,LEN, KPYFR,IGARB)
      IW(KPYFR+LMHROW) = NPYFR
C
      RETURN
      END
