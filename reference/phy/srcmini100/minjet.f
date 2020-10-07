      SUBROUTINE MINJET
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill jet bank DJET for Mini-DST.
C
C     Author: Stephen Haywood      05-Feb-91
C
C     Input  : EJET banks
C     Output : DJET bank
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
      PARAMETER(JEJEPX=1,JEJEPY=2,JEJEPZ=3,JEJEPE=4,LEJETA=4)
      PARAMETER(JDJEPX=1,JDJEPY=2,JDJEPZ=3,JDJEE0=4,LDJETA=4)
C
      LOGICAL GOTIT
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
C++   Loop over different energy flow algorithms.
C++   Now just jets created by ENFLW.
C
      GOTIT = .FALSE.
      DO 1000 IBNK=3,3
C
C++   Pick up EJET bank.
C
      KEJET = NLINK('EJET',IBNK)
      IF(KEJET.LE.0) GOTO 1000
      NEJET = LROWS(KEJET)
      IF(NEJET.LE.0) GOTO 1000
C
C++   Create the DJET bank.
C
      NDJET = NEJET
      LEN = LMHLEN + LDJETA * NDJET
      CALL AUBOS('DJET',IBNK,LEN, KDJET,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINJET: Cannot create DJET bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KEJET = NLINK('EJET',IBNK)
      ENDIF
      IW(KDJET+LMHCOL) = LDJETA
      IW(KDJET+LMHROW) = NDJET
C
C++   Loop over EJET storing information in DJET.
C
      DO 100 I=1,NEJET
         IW(KROW(KDJET,I)+JDJEPX) = NINT(EFACTM * RTABL(KEJET,I,JEJEPX))
         IW(KROW(KDJET,I)+JDJEPY) = NINT(EFACTM * RTABL(KEJET,I,JEJEPY))
         IW(KROW(KDJET,I)+JDJEPZ) = NINT(EFACTM * RTABL(KEJET,I,JEJEPZ))
         IW(KROW(KDJET,I)+JDJEE0) = NINT(EFACTM * RTABL(KEJET,I,JEJEPE))
  100 CONTINUE
      GOTIT = .TRUE.
C
 1000 CONTINUE
C
C++   Add the banks to the Mini list.
C
      IF (GOTIT) CALL MINLIS('DJET')
C
      RETURN
      END
