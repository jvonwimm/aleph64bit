      SUBROUTINE MINFRI
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill FRID bank from DTRA.
C
C     Author: Stephen Haywood      14-Jan-91
C
C     Track quality is filled from DTRA if it is contained there.
C     If not, UFITQL is called, and this requires FRFT, FRTL and YV0V.
C     These banks can be obtained by calling MINFRF, MINFRT and MINYV0.
C     These banks will not be present if NOCH or NOV0 are used.
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
      PARAMETER(JFRIBP=1,JFRIDZ=2,JFRIBC=3,JFRIDC=4,JFRIPE=5,JFRIPM=6,
     +          JFRIPI=7,JFRIPK=8,JFRIPP=9,JFRINK=10,JFRIQF=11,
     +          LFRIDA=11)
C
      INTEGER UFITQL
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
C
      KDTRA = NLINK('DTRA',100)
      IF (KDTRA.LE.0) THEN
         CALL MINUPD('DTRA')
         KDTRA = NLINK('DTRA',100)
         IF (KDTRA.LE.0) RETURN
      ENDIF
C
C++   Get version number: used to decide whether to call UFITQL.
C
      MVER = MINGTV(DUM)
C
C++   Create FRID bank.
C
      NFRID = LROWS(KDTRA)
      IF(NFRID.LE.0) RETURN
      LEN = LMHLEN + LFRIDA * NFRID
      CALL AUBOS('FRID',0,LEN, KFRID,IGARB)
      CALL BLIST(IW,'S+','FRID')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDTRA = NLINK('DTRA',100)
      ENDIF
      IW(KFRID+LMHCOL) = LFRIDA
      IW(KFRID+LMHROW) = NFRID
C
C++   Fill FRID bank.
C
      DO 100 I=1,NFRID
         IW(KROW(KFRID,I)+JFRIBP) = ITABL(KDTRA,I,JDTRHO)
         IW(KROW(KFRID,I)+JFRIDZ) = ITABL(KDTRA,I,JDTRHM)
         IF (MVER.GE.53) THEN
            IW(KROW(KFRID,I)+JFRIQF) = ITABL(KDTRA,I,JDTRQF)
         ELSE
            IW(KROW(KFRID,I)+JFRIQF) = UFITQL(I)
         ENDIF
  100 CONTINUE
C
      RETURN
      END
