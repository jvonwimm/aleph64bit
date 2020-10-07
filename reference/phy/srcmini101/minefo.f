      SUBROUTINE MINEFO
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill EFOL bank from DENF.
C
C     Author: Stephen Haywood      05-Feb-91
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
      PARAMETER(JDENPX=1,JDENPY=2,JDENPZ=3,JDENE0=4,JDENWE=5,JDENTY=6,
     +          JDENPC=7,JDENDT=8,JDENDJ=9,LDENFA=9)
      PARAMETER(JEFOPX=1,JEFOPY=2,JEFOPZ=3,JEFOEW=4,JEFOWE=5,JEFOTY=6,
     +          JEFOLE=7,JEFOLT=8,JEFOLH=9,JEFOLC=10,JEFOLJ=11,
     +          LEFOLA=11)
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
C++   Need Mini version number later.
C
      MVER = MINGTV(DUM)
C
C++   Loop over different energy flow algorithms.
C
      DO 1000 IBNK=0,3
C
      KDENF = NLINK('DENF',IBNK)
      IF(KDENF.LE.0) GOTO 1000
C
C++   Create EFOL bank.
C
      NEFOL = LROWS(KDENF)
      IF(NEFOL.LE.0) GOTO 1000
      LEN = LMHLEN + LEFOLA * NEFOL
      CALL AUBOS('EFOL',IBNK,LEN, KEFOL,IGARB)
      CALL BLIST(IW,'S+','EFOL')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDENF = NLINK('DENF',IBNK)
      ENDIF
      IW(KEFOL+LMHCOL) = LEFOLA
      IW(KEFOL+LMHROW) = NEFOL
C
C++   Fill EFOL bank.
C++   The change arising from the storing of mass rather than energy
C++   is handled here rather than in MINUPD due to various bank numbers.
C
      DO 100 I=1,NEFOL
         RW(KROW(KEFOL,I)+JEFOPX) = FLOAT(ITABL(KDENF,I,JDENPX))/EFACTM
         RW(KROW(KEFOL,I)+JEFOPY) = FLOAT(ITABL(KDENF,I,JDENPY))/EFACTM
         RW(KROW(KEFOL,I)+JEFOPZ) = FLOAT(ITABL(KDENF,I,JDENPZ))/EFACTM
         IF (MVER.GE.90) THEN
            AM = FLOAT(ITABL(KDENF,I,JDENE0))/EFACTM
            ESQ = AM**2 + VMOD(RW(KROW(KEFOL,I)+JEFOPX),3)**2
            RW(KROW(KEFOL,I)+JEFOEW) = SQRT(ESQ)
         ELSE
            RW(KROW(KEFOL,I)+JEFOEW)=FLOAT(ITABL(KDENF,I,JDENE0))/EFACTM
         ENDIF
         RW(KROW(KEFOL,I)+JEFOWE) = FLOAT(ITABL(KDENF,I,JDENWE))/1000.
         IW(KROW(KEFOL,I)+JEFOTY) = ITABL(KDENF,I,JDENTY)
         ICALO = ITABL(KDENF,I,JDENPC)
         IF(ICALO.GT.0) THEN
            IW(KROW(KEFOL,I)+JEFOLE) = + ICALO
         ELSE
            IW(KROW(KEFOL,I)+JEFOLH) = - ICALO
         ENDIF
         IW(KROW(KEFOL,I)+JEFOLT) = ITABL(KDENF,I,JDENDT)
         IW(KROW(KEFOL,I)+JEFOLJ) = ITABL(KDENF,I,JDENDJ)
  100 CONTINUE
C
 1000 CONTINUE
C
      RETURN
      END
