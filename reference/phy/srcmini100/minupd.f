      SUBROUTINE MINUPD(BANK)
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Update style of Mini-DST banks to ensure backward compatibility.
C
C     Author: Stephen Haywood      10-Jan-91
C     Modify: Stephen Haywood      17-Feb-93
C
C     Input  : BANK  = bank name to be updated
C                    = 'DROP' to drop all updated banks
C
C     This routine is a 'dirty' routine to modify old banks written
C     by old versions of MINDST to the new format.
C     It decides what to do on the basis of MVER - the version number.
C     New banks with number 100 are created.
C     These are dropped after the banks have been used by MINFIL.
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
      PARAMETER(JRALRN=1,JRALLR=2,JRALRS=3,JRALRT=4,JRALTT=6,JRALTM=8,
     +          JRALEP=9,JRALMP=13,JRALMC=14,JRALMA=15,JRALMB=16,
     +          JRALNA=17,JRALNB=18,JRALQA=19,JRALQB=20,JRALDU=21,
     +          JRALCO=22,JRALVD=24,JRALIT=26,JRALTP=27,JRALEC=29,
     +          JRALHC=32,JRALMU=35,JRALSA=41,JRALLC=43,JRALBC=45,
     +          LRALEA=46)
      PARAMETER(JDECE0=1,JDECTH=2,JDECPH=3,JDECEF=4,JDECCC=6,LDECOA=6)
      PARAMETER(JDVEX0=1,JDVEY0=2,JDVEZ0=3,JDVEFP=4,JDVEMV=5,JDVEDT=6,
     +          LDVERA=6)
      PARAMETER(JDHEFP=1,JDHENX=2,JDHENP=3,JDHENM=4,JDHENV=5,JDHENJ=6,
     +          JDHEEC=7,JDHEEL=8,JDHEPF=9,JDHETH=10,JDHEPH=11,
     +          JDHEEF=12,JDHEET=13,JDHET1=14,JDHEP1=15,JDHET2=16,
     +          JDHEP2=17,JDHEE1=18,JDHEE2=19,JDHEE3=20,JDHERS=21,
     +          JDHEWT=22,LDHEAA=22)
      PARAMETER(JDEIR2=1,JDEIR3=2,JDEIQF=3,JDEIDE=4,JDEIDT=5,LDEIDA=5)
      PARAMETER(JDTBT1=1,JDTBT2=2,JDTBL2=3,LDTBPA=3)
      PARAMETER(JDTRCH=1,JDTRP0=2,JDTRTH=3,JDTRPH=4,JDTRD0=5,JDTRZ0=6,
     +          JDTRER=7,JDTRTF=12,JDTRHO=13,JDTRHM=14,JDTRVB=15,
     +          JDTRQF=16,JDTREA=17,JDTRVI=27,LDTRAA=27)
      PARAMETER(JDGANA=1,JDGAE1=2,JDGAE2=3,JDGAE3=4,JDGAE5=5,JDGAE6=6,
     +          JDGAE8=7,JDGAE0=8,JDGATH=9,JDGAPH=10,JDGADE=11,
     +          LDGAMA=11)
C
      CHARACTER*4 BANK
      DATA LRUN,LEVT,MVER / -999,-999,0 /
      DATA CURNOM,CUR92C / 4963750.,17700. /
      SAVE LRUN,LEVT,MVER,CURNOM,CUR92C
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
C++   After banks have been used by MINFIL, drop them so that they
C++   cannot be written out.
C++   (If they were, then they in turn may have to be updated
C++   - precisely what we want to avoid.)
C
      IF (BANK.EQ.'DROP') THEN
         KDROP = NDROP('DTRA',100)
         KDROP = NDROP('DVER',100)
         KDROP = NDROP('DEID',100)
         KDROP = NDROP('DGAM',100)
         KDROP = NDROP('DTBP',100)
         KDROP = NDROP('DECO',100)
         RETURN
      ENDIF
C
C++   Do we have a new event ? If so find version number.
C
      CALL ABRUEV(IRUN,IEVT)
      IF (IRUN.NE.LRUN .OR. IEVT.NE.LEVT) THEN
         MVER = MINGTV(DUM)
         LRUN = IRUN
         LEVT = IEVT
C
         IF (MVER.LE.0) THEN
            WRITE(IW(6),'(//'' MINUPD in MINI lib is very confused !''/
     &      '' It seems that the version number of your Mini is 0.''/
     &      '' This will create mess, therefore STOP.''/
     &      '' If you are sure you want to continue,'',
     &      '' supply MVOK card with the appropriate version number.'',
     &      '' Whatever, PLEASE call Mini expert !'')')
            STOP
         ENDIF
C
         IF (MVER.LT.61 .AND. NLINK('MVOK',0).LE.0) THEN
            WRITE(IW(6),'(//'' STOP called from MINUPD in MINI lib.''/
     &      '' It seems that the version number of your Mini is '',I4/
     &      '' This is very old, probably not very useful and may'',
     &      '' cause problems.''/
     &      '' If you are sure you want to continue,'',
     &      '' supply MVOK card.''/
     &      '' Whatever, PLEASE call Mini expert !'')') MVER
            STOP
         ENDIF
      ENDIF
C
C++   Determine which bank is to be updated.
C
      IF (BANK.EQ.'DTRA') GOTO 100
      IF (BANK.EQ.'DVER') GOTO 200
      IF (BANK.EQ.'DEID') GOTO 300
      IF (BANK.EQ.'DGAM') GOTO 400
      IF (BANK.EQ.'DTBP') GOTO 500
      IF (BANK.EQ.'DECO') GOTO 600
C
      RETURN
C-----------------------------------------------------------------------
C                                 DTRA
C-----------------------------------------------------------------------
C
C++   Update DTRA - copy to DTRA/100.
C
  100 IF (NLINK('DTRA',100).GT.0) GOTO 199
      CALL MIN100('DTRA',LDTRAA, KOLD,KNEW,NDTRA)
      IF (NDTRA.LE.0) GOTO 199
C
C++   Direct copy for most recent banks.
C
      IF (MVER.GE.90) THEN
         CALL UCOPY(RW(KOLD+LMHLEN+1),RW(KNEW+LMHLEN+1),LDTRAA*NDTRA)
      ENDIF
C
C++   Pre-9.0: copy information, skipping old EM, dE/dx and DTRA link.
C
      IF (MVER.GE.90) GOTO 199
      DO 110 I=1,NDTRA
         CALL UCOPY(RW(KROW(KOLD,I)+ 1),RW(KROW(KNEW,I)+JDTRCH),11)
         CALL UCOPY(RW(KROW(KOLD,I)+16),RW(KROW(KNEW,I)+JDTRTF), 3)
         NW = LCOLS(KOLD) - 22
         CALL UCOPY(RW(KROW(KOLD,I)+22),RW(KROW(KNEW,I)+JDTRVB),NW)
  110 CONTINUE
C
C++   Correct momentum due to problem with coil current reading.
C++   This affects only 1992 real data, processed in 1992.
C++   Corresponding to Mini vsn 8.7, Alephlib 14.4 was released in
C++   which ALFIEL calculates B-field correctly.
C++   Here, the corrected current from RALE is used.
C++   This is a 0.36 % effect.
C
      IF (MVER.GE.87) GOTO 199
      IF (IRUN.GT.14000 .AND. IRUN.LT.17900) THEN
         KRALE = IW(NAMIND('RALE'))
         IF (KRALE.GT.0) THEN
            CURR = FLOAT(ITABL(KRALE,1,JRALMC))
         ELSE
            CURR = CURNOM
         ENDIF
         BCOR = 1. - CUR92C/CURR
C
         DO 120 I=1,NDTRA
            P = BCOR * FLOAT(ITABL(KOLD,I,JDTRP0))
            IW(KROW(KNEW,I)+JDTRP0) = NINT(P)
  120    CONTINUE
      ENDIF
C
C++   Pre-7.3: replace track fit probability by Chisq/DoF.
C
      IF (MVER.GE.73) GOTO 199
      NR = IW(KOLD-2)
      DO 130 I=1,NDTRA
         IH = ITABL(KOLD,I,JDTRHO)
         IF (NR.EQ.2) THEN
            NMVD = MINHIT(IH,1)
         ELSE
            NMVD = 0
         ENDIF
         NITC = MINHIT(IH,2)
         NTPC = MINHIT(IH,3)
         NCONS = 5
         IF (NMVD+NTPC.EQ.0) NCONS = 3
         NDEG = 2*NMVD + NITC + 2*NTPC - NCONS
         PROB = FLOAT(ITABL(KOLD,I,JDTRTF)) / 1000.
         IF (PROB.GT.0. .AND. NDEG.GT.0) THEN
            CHISQ = AMAX1(CHISIN(1.-PROB,NDEG),0.)
            CHIN = CHISQ / NDEG
         ELSE IF (PROB.LE.0. .AND. NDEG.GT.0) THEN
            CHIN = 999999.
         ELSE
            CHIN = 0.
         ENDIF
         IW(KROW(KNEW,I)+JDTRTF) = NINT(10.*CHIN)
  130 CONTINUE
C
C++   Pre-6.1: copy the old EM to the new position - it will be decoded
C++   in MINFRF.
C
      IF (MVER.GE.61) GOTO 199
      DO 140 I=1,NDTRA
         CALL UCOPY(RW(KROW(KOLD,I)+12),RW(KROW(KNEW,I)+JDTREA),4)
  140 CONTINUE
C
  199 RETURN
C-----------------------------------------------------------------------
C                                 DVER
C-----------------------------------------------------------------------
C
C++   Update DVER - copy to DVER/100.
C
  200 IF (NLINK('DVER',100).GT.0) GOTO 299
      CALL MIN100('DVER',LDVERA, KOLD,KNEW,NDVER)
      IF (NDVER.LE.0) GOTO 299
C
C++   Direct copy for most recent banks.
C
      IF (MVER.GE.50) THEN
         CALL UCOPY(RW(KOLD+LMHLEN+1),RW(KNEW+LMHLEN+1),LDVERA*NDVER)
      ENDIF
C
C++   Pre-5.0: include main-vertex bit.
C++   The number of main-vertices is determined from DHEA if it exists
C++   or is assumed to be one.
C
      IF (MVER.GE.50) GOTO 299
      KDHEA = IW(NAMIND('DHEA'))
      IF (KDHEA.GT.0) THEN
         NMAIN = ITABL(KDHEA,1,JDHENX) - ITABL(KDHEA,1,JDHENV)
      ELSE
         NMAIN = 1
      ENDIF
      DO 210 I=1,NDVER
         CALL UCOPY(RW(KROW(KOLD,I)+1),RW(KROW(KNEW,I)+JDVEX0),4)
         IF (I.LE.NMAIN) THEN
            IW(KROW(KOLD,I)+JDVEMV) = 1
         ELSE
            IW(KROW(KOLD,I)+JDVEMV) = 0
         ENDIF
         IW(KROW(KNEW,I)+JDVEDT) = IW(KROW(KOLD,I)+5)
  210 CONTINUE
C
  299 RETURN
C-----------------------------------------------------------------------
C                                 DEID
C-----------------------------------------------------------------------
C
C++   Update DEID - copy to DEID/100.
C
  300 IF (NLINK('DEID',100).GT.0) GOTO 399
      CALL MIN100('DEID',LDEIDA, KOLD,KNEW,NDEID)
      IF (NDEID.LE.0) GOTO 399
C
C++   Direct copy for most recent banks.
C
      IF (MVER.GE.90) THEN
         CALL UCOPY(RW(KOLD+LMHLEN+1),RW(KNEW+LMHLEN+1),LDEIDA*NDEID)
      ENDIF
C
C++   Pre-9.0: skip R6 and R7.
C
      IF (MVER.GE.90) GOTO 399
      IF (MVER.GE.50) THEN
         DO 310 I=1,NDEID
            IW(KROW(KNEW,I)+JDEIR2) = IW(KROW(KOLD,I)+1)
            IW(KROW(KNEW,I)+JDEIR3) = IW(KROW(KOLD,I)+2)
            IW(KROW(KNEW,I)+JDEIQF) = IW(KROW(KOLD,I)+5)
            IW(KROW(KNEW,I)+JDEIDE) = IW(KROW(KOLD,I)+6)
            IW(KROW(KNEW,I)+JDEIDT) = IW(KROW(KOLD,I)+7)
  310    CONTINUE
      ENDIF
C
C++   Pre-9.0: include DECO link.
C
      IF (MVER.GE.50) GOTO 399
      DO 320 I=1,NDEID
         IW(KROW(KNEW,I)+JDEIR2) = IW(KROW(KOLD,I)+1)
         IW(KROW(KNEW,I)+JDEIR3) = IW(KROW(KOLD,I)+2)
         IW(KROW(KNEW,I)+JDEIQF) = IW(KROW(KOLD,I)+3)
         IW(KROW(KNEW,I)+JDEIDT) = IW(KROW(KOLD,I)+4)
  320 CONTINUE
C
  399 RETURN
C-----------------------------------------------------------------------
C                                 DGAM
C-----------------------------------------------------------------------
C
C++   Update DGAM - copy to DGAM/100.
C
  400 IF (NLINK('DGAM',100).GT.0) GOTO 499
      CALL MIN100('DGAM',LDGAMA, KOLD,KNEW,NDGAM)
      IF (NDGAM.LE.0) GOTO 499
C
C++   Direct copy for most recent banks.
C
      IF (MVER.GE.87) THEN
         CALL UCOPY(RW(KOLD+LMHLEN+1),RW(KNEW+LMHLEN+1),LDGAMA*NDGAM)
      ENDIF
C
C++   Pre-8.7: correct precision for E3.
C
      IF (MVER.GE.87) GOTO 499
      CALL UCOPY(RW(KOLD+LMHLEN+1),RW(KNEW+LMHLEN+1),LDGAMA*NDGAM)
      DO 410 I=1,NDGAM
         CALL UCOPY(RW(KROW(KOLD,I)+1),RW(KROW(KNEW,I)+JDGANA),11)
         IW(KROW(KNEW,I)+JDGAE3) = 100 * IW(KROW(KOLD,I)+4)
  410 CONTINUE
C
  499 RETURN
C-----------------------------------------------------------------------
C                                 DTBP
C-----------------------------------------------------------------------
C
C++   Update DTBP - copy to DTBP/100.
C
  500 IF (NLINK('DTBP',100).GT.0) GOTO 599
      CALL MIN100('DTBP',LDTBPA, KOLD,KNEW,NDTBP)
      IF (NDTBP.LE.0) GOTO 599
C
C++   Direct copy for most recent banks.
C
      IF (MVER.GE.51) THEN
         CALL UCOPY(RW(KOLD+LMHLEN+1),RW(KNEW+LMHLEN+1),LDTBPA*NDTBP)
      ENDIF
C
C++   Pre-5.1: include 3rd trigger word.
C
      IF (MVER.GE.51) GOTO 599
      DO 510 I=1,NDTBP
         CALL UCOPY(RW(KROW(KOLD,I)+1),RW(KROW(KNEW,I)+JDTBT1),2)
  510 CONTINUE
C
  599 RETURN
C-----------------------------------------------------------------------
C                                 DECO
C-----------------------------------------------------------------------
C
C++   Update DECO - copy to DECO/100.
C
  600 IF (NLINK('DECO',100).GT.0) GOTO 699
      CALL MIN100('DECO',LDECOA, KOLD,KNEW,NDECO)
      IF (NDECO.LE.0) GOTO 699
C
C++   Direct copy for most recent banks.
C
      IF (MVER.GE.51) THEN
         CALL UCOPY(RW(KOLD+LMHLEN+1),RW(KNEW+LMHLEN+1),LDECOA*NDECO)
      ENDIF
C
C++   Pre-5.0: shrink bank and increase precision of energy fractions.
C
      IF (MVER.GE.50) GOTO 699
      DO 610 I=1,NDECO
         CALL UCOPY(RW(KROW(KOLD,I)+1),RW(KROW(KNEW,I)+JDECE0),3)
         IW(KROW(KNEW,I)+JDECEF+0) = 10 * IW(KROW(KOLD,I)+4)
         IW(KROW(KNEW,I)+JDECEF+1) = 10 * IW(KROW(KOLD,I)+5)
         IW(KROW(KNEW,I)+JDECCC) = IW(KROW(KOLD,I)+10)
  610 CONTINUE
C
  699 RETURN
C
      END
