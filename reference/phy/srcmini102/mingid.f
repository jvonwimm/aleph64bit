      SUBROUTINE MINGID
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill photon bank DGID for Mini-DST.
C
C     Author: Stephen Haywood      02-Mar-93
C
C     Input  : PGID bank
C     Output : DGID bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
C
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
      PARAMETER(JPGIIF=1,JPGIDE=2,JPGICM=3,JPGIM1=4,JPGIM2=5,JPGIM3=6,
     +          JPGICE=7,JPGITH=8,JPGIPH=9,JPGIPE=10,LPGIDA=10)
      PARAMETER(JDGIIF=1,JDGIDE=2,JDGICM=3,JDGIM1=4,JDGIM2=5,JDGIM3=6,
     +          JDGICE=7,JDGITH=8,JDGIPH=9,JDGIPE=10,LDGIDA=10)
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
C++   Pick up PGID bank.
C
      KPGID = NLINK('PGID',0)
      IF (KPGID.LE.0) RETURN
      NPGID = LROWS(KPGID)
      IF (NPGID.LE.0) RETURN
C
C++   Create the DGID bank.
C
      NDGID = NPGID
      LEN = LMHLEN + LDGIDA * NDGID
      CALL AUBOS('DGID',0,LEN, KDGID,IGARB)
      IF (IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINGID: Cannot create DGID bank'')')
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KPGID = NLINK('PGID',0)
      ENDIF
      IW(KDGID+LMHCOL) = LDGIDA
      IW(KDGID+LMHROW) = NDGID
C
C++   Loop over PGID storing information in DGID.
C++   Note: non-standard name for index for relation between DGIG and
C++   DECO in order to avoid clash with depth estimator.
C
      DO 100 I=1,NPGID
         IW(KROW(KDGID,I)+JDGIIF) = ITABL(KPGID,I,JPGIIF)
         IW(KROW(KDGID,I)+JDGIDE) = NINT(100. * RTABL(KPGID,I,JPGIDE))
         IW(KROW(KDGID,I)+JDGICM) = NINT(100. * RTABL(KPGID,I,JPGICM))
         IW(KROW(KDGID,I)+JDGIM1) = NINT(100. * RTABL(KPGID,I,JPGIM1))
         IW(KROW(KDGID,I)+JDGIM2) = NINT(100. * RTABL(KPGID,I,JPGIM2))
         IW(KROW(KDGID,I)+JDGIM3) = NINT(EFACTM * RTABL(KPGID,I,JPGIM3))
         IW(KROW(KDGID,I)+JDGICE) = NINT(EFACTM * RTABL(KPGID,I,JPGICE))
         IW(KROW(KDGID,I)+JDGITH) = NINT(AFACTM * RTABL(KPGID,I,JPGITH))
         IW(KROW(KDGID,I)+JDGIPH) = NINT(AFACTM * RTABL(KPGID,I,JPGIPH))
         IW(KROW(KDGID,I)+JDGIPE) = ITABL(KPGID,I,JPGIPE)
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DGID')
C
      RETURN
      END
