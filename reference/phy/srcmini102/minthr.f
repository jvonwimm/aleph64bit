      SUBROUTINE MINTHR (THRU,CTHR)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill thrust bank DTHR for Mini-DST.
C
C     Author: Agnieszka Jacholkowska 1-Oct-94
C
C     Input  : QJTHRU output
C     Output : DTHR bank
C
C     Called by QUEVT - MINALGO
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
C
      PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
      PARAMETER(JDTHPR=1,JDTHPX=2,JDTHPY=3,JDTHPZ=4,JDTHE0=5,LDTHRA=5)
C
      LOGICAL GOTIT
C
      DIMENSION CTHR(4)
C
C!    set of intrinsic functions to handle BOS banks
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
C
      GOTIT = .FALSE.
C
C++   Create the DTHR bank.
C
      IBNK = 0
      NDTHR = 1
      LEN = LMHLEN + LDTHRA
      CALL AUBOS('DTHR',IBNK,LEN, KDTHR,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINTHR: Cannot create DTHR bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         WRITE(IW(6),'('' MINTHR: IGARB NE 0. '')')
      ENDIF
c     WRITE(IW(6),'('' MINTHR: DTHR bank lifted'')')
c     PRINT * ,THRU, CTHR(1), CTHR(2), CTHR(3), CTHR(4)
      IW(KDTHR+LMHCOL) = LDTHRA
      IW(KDTHR+LMHROW) = NDTHR
C
C++   Loop over THRUST storing information in DTHR.
C
      IW(KROW(KDTHR,1)+JDTHPR) = NINT(AFACTM * THRU)
      IW(KROW(KDTHR,1)+JDTHPX) = NINT(AFACTM * CTHR(1))
      IW(KROW(KDTHR,1)+JDTHPY) = NINT(AFACTM * CTHR(2))
      IW(KROW(KDTHR,1)+JDTHPZ) = NINT(AFACTM * CTHR(3))
      IW(KROW(KDTHR,1)+JDTHE0) = NINT(AFACTM * CTHR(4))

      GOTIT = .TRUE.
C
 1000 CONTINUE
C
C++   Add the banks to the Mini list.
C
      IF (GOTIT) CALL MINLIS('DTHR')
C
      RETURN
      END
