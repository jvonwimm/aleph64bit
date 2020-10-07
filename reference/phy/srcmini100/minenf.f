      SUBROUTINE MINENF
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill energy flow bank DENF for Mini-DST.
C
C     Author: Stephen Haywood      05-Feb-91
C
C     Input  : EFOL banks
C     Output : DENF bank
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
      PARAMETER(JEFOPX=1,JEFOPY=2,JEFOPZ=3,JEFOEW=4,JEFOWE=5,JEFOTY=6,
     +          JEFOLE=7,JEFOLT=8,JEFOLH=9,JEFOLC=10,JEFOLJ=11,
     +          LEFOLA=11)
      PARAMETER(JDENPX=1,JDENPY=2,JDENPZ=3,JDENE0=4,JDENWE=5,JDENTY=6,
     +          JDENPC=7,JDENDT=8,JDENDJ=9,LDENFA=9)
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
C++   Loop over different energy flow algorithms.
C
      DO 1000 IBNK=3,3
C
C++   Pick up EFOL bank.
C
      KEFOL = NLINK('EFOL',IBNK)
      IF(KEFOL.LE.0) GOTO 1000
      NEFOL = LROWS(KEFOL)
      IF(NEFOL.LE.0) GOTO 1000
C
C++   Create the DENF bank.
C
      NDENF = NEFOL
      LEN = LMHLEN + LDENFA * NDENF
      CALL AUBOS('DENF',IBNK,LEN, KDENF,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINENF: Cannot create DENF bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KEFOL = NLINK('EFOL',IBNK)
      ENDIF
      IW(KDENF+LMHCOL) = LDENFA
      IW(KDENF+LMHROW) = NDENF
C
C++   Loop over EFOL storing information in DENF.
C++   Mass is stored rather than energy, so that the mass hypothesis is
C++   not lost by rounding.
C
      DO 100 I=1,NEFOL
         IW(KROW(KDENF,I)+JDENPX) = NINT(EFACTM * RTABL(KEFOL,I,JEFOPX))
         IW(KROW(KDENF,I)+JDENPY) = NINT(EFACTM * RTABL(KEFOL,I,JEFOPY))
         IW(KROW(KDENF,I)+JDENPZ) = NINT(EFACTM * RTABL(KEFOL,I,JEFOPZ))
         AMSQ = RTABL(KEFOL,I,JEFOEW)**2
     &     - VMOD(RW(KROW(KEFOL,I)+JEFOPX),3)**2
         IW(KROW(KDENF,I)+JDENE0) = NINT(EFACTM * SQRT(MAX(AMSQ,0.)))
         IW(KROW(KDENF,I)+JDENWE) = NINT(1000.  * RTABL(KEFOL,I,JEFOWE))
         IW(KROW(KDENF,I)+JDENTY) = ITABL(KEFOL,I,JEFOTY)
         IPECO = ITABL(KEFOL,I,JEFOLE)
         IF(IPECO.GT.0) THEN
            IW(KROW(KDENF,I)+JDENPC) = IPECO
         ELSE
            IW(KROW(KDENF,I)+JDENPC) = - ITABL(KEFOL,I,JEFOLH)
         ENDIF
         IW(KROW(KDENF,I)+JDENDT) = ITABL(KEFOL,I,JEFOLT)
         IW(KROW(KDENF,I)+JDENDJ) = ITABL(KEFOL,I,JEFOLJ)
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DENF')
C
C++   Add supplmentary information for EFOL/3 (ENFLW).
C
      IF (IBNK.EQ.3) CALL MINLIS('EAUX')
C
 1000 CONTINUE
C
      RETURN
      END
