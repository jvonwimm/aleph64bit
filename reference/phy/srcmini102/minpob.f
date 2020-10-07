      SUBROUTINE MINPOB
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill Hcal digital pattern object DPOB for Mini-DST.
C
C     Author: Stephen Haywood      22-Jan-90
C
C     Input  : PPOB bank
C     Output : DPOB bank
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
      PARAMETER(JPPODI=1,JPPODE=2,JPPOC1=3,JPPOC2=4,JPPOIP=5,JPPOFP=6,
     +          JPPOLP=7,JPPOMD=8,JPPOPD=9,LPPOBA=9)
      PARAMETER(JDPOE0=1,LDPOBA=1)
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
C++   Pick up PPOB bank.
C
      KPPOB = NLINK('PPOB',0)
      IF(KPPOB.GT.0) THEN
         NPPOB = LROWS(KPPOB)
      ELSE
         NPPOB = 0
      ENDIF
      IF(NPPOB.LE.0) RETURN
C
C++   Create DPOB bank.
C
      NDPOB = NPPOB
      LEN = LMHLEN + LDPOBA * NDPOB
      CALL AUBOS('DPOB',0,LEN, KDPOB,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINPOB: Cannot create DPOB bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KPPOB = NLINK('PPOB',0)
      ENDIF
      IW(KDPOB+LMHCOL) = LDPOBA
      IW(KDPOB+LMHROW) = NDPOB
C
C++   Loop over PPOB storing information in DPOB.
C
      DO 100 I=1,NPPOB
         IW(KROW(KDPOB,I)+JDPOE0) = NINT(EFACTM * RTABL(KPPOB,I,JPPODE))
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DPOB')
C
      RETURN
      END
