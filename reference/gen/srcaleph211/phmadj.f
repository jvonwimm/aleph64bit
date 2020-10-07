      SUBROUTINE PHMADJ (LIST,IER)
C----------------------------------------------------------------------
C!   Change PHMA POT bank into HMAD JULIA bank
C
C   Author   :D. SCHLATTER              5-NOV-1988
C     Input :    LIST      BOS event list
C                          if LIST(2:2).eq.'-' drop POT banks
C
C     Output:    IER       = 0  successful
C                          = 1  input bank does not exist or is empty
C                          = 2  not enough space
C                          =-1  OK but garbage collection
C======================================================================
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JHMANF=1,JHMANE=2,JHMANL=3,JHMAMH=4,JHMAIG=5,JHMAED=6,
     +          JHMACS=7,JHMAND=8,JHMAIE=9,JHMAIT=10,JHMAIF=11,
     +          JHMATN=12,LHMADA=12)
      PARAMETER(JPHMMH=1,JPHMIG=2,JPHMED=3,JPHMCS=4,JPHMND=5,JPHMIE=6,
     +          JPHMIT=7,JPHMIF=8,JPHMTN=9,LPHMAA=9)
      CHARACTER*(*) LIST, PLIST*4, JLIST*4
      LOGICAL FIRST,BTEST
      DATA FIRST/.TRUE./
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
C - set name-indices and bank formats ==============================
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        NAPHMA=NAMIND('PHMA')
        NAHMAD=NAMIND('HMAD')
        CALL BKFMT('HMAD','2I,(5I,2F,5I)')
      ENDIF
C
C - check existence of input banks, return if not there or empty =====
C
      IER=1
      IF(IW(NAPHMA).LE.0) GOTO 999
      NMUON=LROWS(IW(NAPHMA))
      IF(NMUON.LE.0) GOTO 999
C
C - create output bank(s), return if not booked =======================
C
      LNHMA=LMHLEN+NMUON*LHMADA
      CALL AUBOS('HMAD',0,LNHMA,KHMAD,IER)
      IF (IER.EQ.2) GOTO 999
      JLIST = 'HMAD'
      IW(KHMAD+LMHCOL)=LHMADA
      IW(KHMAD+LMHROW)=NMUON
C
C - fill output bank(s) ===========================================
C
      KPHMA=IW(NAPHMA)
      IND2=KHMAD+LMHLEN+JHMAMH-1
      IND1=KPHMA+LMHLEN
      DO 11 J=1,NMUON
        DO 10 I=1,LPHMAA
   10   IW(IND2+I)=IW(IND1+I)
        IND2=IND2+LHMADA
        IND1=IND1+LPHMAA
   11 CONTINUE
C
C          get # of fired/expected planes
C
      DO 25 IMU=1,NMUON
        I1=0
        I2=0
        I3=0
        DO 20 I=1,23
          IF( BTEST(ITABL(KHMAD,IMU,JHMAIE),I) ) I1=I1+1
          IF( BTEST(ITABL(KHMAD,IMU,JHMAIT),I) ) I2=I2+1
          IF(I.GE.12.AND.
     1        BTEST(ITABL(KHMAD,IMU,JHMAIE),I) ) I3=I3+1
   20   CONTINUE
        IW(KROW(KHMAD,IMU)+JHMANF)=I1
        IW(KROW(KHMAD,IMU)+JHMANE)=I2
        IW(KROW(KHMAD,IMU)+JHMANL)=I3
   25 CONTINUE
C
  998 CONTINUE
C - get the drop flag if any, then drop POT banks if required, =======
C   add JUL banks to S-list
C   POT banks are on PLIST, JUL banks on JLIST
C
      PLIST = 'PHMA'
C! add JLIST to S-list, drop PLIST if required
      IF (LNBLNK(LIST).EQ.2) THEN
         IF (LIST(2:2).EQ.'-' .AND. LNBLNK(PLIST).GE.4) THEN
            CALL BDROP (IW,PLIST)
            CALL BLIST (IW,LIST,PLIST(1:LNBLNK(PLIST)))
         ENDIF
      ENDIF
      CALL BLIST (IW,'S+',JLIST(1:LNBLNK(JLIST)))
C
C
C - if garbage collection then set IER = -1  =======================
C
      IF (IER .EQ. 1) IER = -1
C
C - return  ======================================================
  999 RETURN
      END
