      INTEGER FUNCTION MINGTV(DUMMY)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Mini-DST version number used to produce current data.
C
C     This is obtained from the event bank DVRS or the run bank RHAH by
C     a call to ALVSN.
C     It can be overrode by the MVOK data card.
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
      PARAMETER(JDVRMV=1,JDVRV0=2,LDVRSA=2)
C
      DIMENSION IPV(10),IAV(10)
      DATA LRUN,LEVT,MVER / -999,-999,0 /
      SAVE LRUN,LEVT,MVER
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
C++   See if we have already found version for this event.
C
      CALL ABRUEV(IRUN,IEVT)
      IF (IRUN.EQ.LRUN .AND. IEVT.EQ.LEVT) GOTO 1000
      LRUN = IRUN
      LEVT = IEVT
C
C++   Priority given to overriding version number with MVOK card.
C
      KMVOK = NLINK('MVOK',0)
      IF (KMVOK.GT.0 .AND. IW(KMVOK).GE.1) THEN
         MVER = IW(KMVOK+1)
         GOTO 1000
      ENDIF
C
C++   Next look for DVRS bank - default method for versions from 9.0.
C
      KDVRS = NLINK('DVRS',0)
      IF (KDVRS.GT.0) THEN
         MVER = ITABL(KDVRS,1,JDVRMV)
         GOTO 1000
      ENDIF
C
C++   Lastly look in RHAH history.
C
      CALL ALVSN(ITYP,IPV,IAV,IYR)
      MVER = IPV(5)
C
 1000 MINGTV = MVER
C
      RETURN
      END
