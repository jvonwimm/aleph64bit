      SUBROUTINE ITQUAL(ITK,IFLG,IWIR,ICO,XHT,XDC,IER)
C-----------------------------------------------------------------------
CKEY ITC
C! Create ITC Track quality info. bank for one track
C!
C!    Author  :  W. B. Atwood  2-Oct-89
C!    Modified:  J.Sedgbeer 20/04/90 To go in ALEPHLIB.
C!    Modified:  J.Sedgbeer 21/10/91 Fill wire info. in IQXT even if no
C!                                   coordinate
C!   Input:
C!      ITK       /I : Track number - bank IQXT will be created with
C!                                               this number.
C!      IFLG(i)   /I : Quality flag at layer i
C!      IWIR(i)   /I : Wire number at layer i. IWIR=0 if no crossing.
C!      ICO(i)    /I : Coord. no at wire i. ICO=0 if no coord.
C!      XHT(3,i)  /R : Track crossing point at layer i.
C!      XDC(3,i)  /R : Direction cosines of track at XHT
C!      commons:   /BCS/ for BOS bank ITCO and IQXT
C!      params:    ALCONS
C!                 IQXTJJ
C!                 ITCOJJ
C!
C!  Output:
C!  -------
C!     IER      /I  : error flag:
C!                   = 0  all O.K.
C!                   = 1  ITCO missing or empty - IQXT not created.
C!                   = 2  no room to create banks - IQXT not created
C!                   =-1  O.K. but garbage collection done.
C!     IQXT bank number ITK
C!
C!   calls: UTSWCO - ALEPHLIB
C!          ITROTN - ALEPHLIB
C!          ICDRIF - ALEPHLIB
C!          IUDOCA - ALEPHLIB
C!          AUBOS  - ALEPHLIB
C!
C!   Libraries required: BOS
C!
C! Create IQXT bank number ITK - this bank contains info on the
C! track crossings, wire positions, coord. positions and residuals
C! for the track. Note that the coord. and residual info. is
C! only filled if there is a valid coord on the wire, i.e. ICO(i) > 0.
C! Otherwise the contents of this part of the bank is UNDEFINED.
C! (If the bank is newly created then it will contain zeroes, however,
C! if the bank already existed then it will contain whatever was there
C! before - you have been warned!).
C!
C? Set error flag IER depending on existence of ITCO bank.
C? If no ITCO bank then return (IER=1)
C? Create(or locate) IQXT bank number ITK
C? If no room for bank then set error flag (IER=2) and return.
C? If Garbage collection done then set IER=-1
C?
C? Loop over ITC layers
C?   Fill part of IQXT bank from input arrays.
C?   If no crossing at this layer (wire=0) then skip
C?   Wire & track related info:
C?     Use wire no. and Z of track crossing to get sense wire coord
C?                             (corrected for alignment).
C?     From wire position and track crossing get DOCA
C?     Calculate track position at DOCA
C?     Fill IQXT bank with wire info
C?   If no coord on this wire then skip
C?   Coord. related info:
C?     Get the drift distance and error (use drift-time etc from ITCO)
C?     Choose coord. ambiguity closest to DOCA.
C?     Get residuals
C?     Fill IQXT bank with coord info.
C? Endloop
C-----------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER(JITCWN=1,JITCRA=2,JITCP1=3,JITCP2=4,JITCZH=5,JITCSR=6,
     +          JITCSZ=7,JITCDT=8,LITCOA=8)
      PARAMETER(JIQXFL=1,JIQXWN=2,JIQXCO=3,JIQXPW=4,JIQXWX=5,JIQXHX=8,
     +          JIQXDC=11,JIQXDT=12,JIQXDD=13,JIQXER=14,JIQXRD=15,
     +          JIQXRF=16,JIQXRZ=17,JIQXEZ=18,LIQXTA=18)
C-----------------------------------------------------------------------
      EXTERNAL NAMIND
      INTEGER ITK,IFLG(*),IWIR(*),ICO(*),IER
      REAL XHT(3,*),XDC(3,*)
      REAL WRD(3),WRX(3),HTX(3)
      LOGICAL  FIRST,DEB
      DATA FIRST/.TRUE./,DEB/.FALSE./
C-----------------------------------------------------------------------
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
C-----------------------------------------------------------------------
C Initialise
      IF(FIRST) THEN
        LPR = IW(6)
        LITCO = NAMIND('ITCO')
        LIQXT = NAMIND('IQXT')
        CALL BKFMT('IQXT','2I,(3I,15F)')
        WRD(1) = 0.
        WRD(2) = 0.
        WRD(3) = -1.
        FIRST = .FALSE.
      ENDIF
C
C Check for ITCO bank
C
      IER = 1
      JITCO = IW(LITCO)
      IF(JITCO.LE.0) GOTO 999
      NCO = LROWS(JITCO)
      IF(NCO.LE.0) GOTO 999
      IER = 0
C
C Create bank IQXT number ITK
C
      NLAY = 8
      LEN = NLAY*LIQXTA + LMHLEN
      CALL AUBOS('IQXT',ITK,LEN,JIQXT,IGARB)
      IF(IGARB.EQ.2) THEN
        IER = 2
        GOTO 999
      ENDIF
      IF(IGARB.EQ.1) IER = -1
C
      IW(JIQXT+LMHCOL) = LIQXTA
      IW(JIQXT+LMHROW) = NLAY
C
      IF(DEB) WRITE(LPR,1000) ITK,IGARB
 1000 FORMAT(' ITQUAL: IQXT bank number',I4,' created. IGARB=',I2,
     +  /3X,'Layer',' Flag',' Wire',' Coord',' Wire-phi',
     +  6X,'x',6X,'y',6X,'z',' Hit x      y      z ',' Doca',
     +  ' Dtime - dist','  Sigma',' Res.D-F-Z')
C
C-----------------------------------------------------------------------
C Loop over the ITC Layers. Fill IQXT
C
      DO 100 LAY=1,NLAY
        IC = ICO(LAY)
        IWIRE = IWIR(LAY)
        JJ = KROW(JIQXT,LAY)
        IW(JJ+JIQXFL) = IFLG(LAY)
        IW(JJ+JIQXWN) = IWIRE
        IW(JJ+JIQXCO) = IC
C Skip those with no associated wire
        IF(IWIRE.LE.0) GOTO 100
C
C Get wire position
        ZSW   = XHT(3,LAY)
        CALL UTSWCO(LAY,IWIRE,ZSW,RSW,FSW)
        CALL ITROTN(RSW,FSW,ZSW)
        FSW = AMOD(FSW + TWOPI, TWOPI)
        WRX(1) = RSW*COS(FSW)
        WRX(2) = RSW*SIN(FSW)
        WRX(3) = ZSW
C
C From wire position and track crossing get DOCA (signed)
        CALL IUDOCA(WRX,WRD,XHT(1,LAY),XDC(1,LAY),
     +                   DCA,DIST1,DIST2)
C
C Calculate track position at DOCA
        DO 50 I=1,3
          HTX(I) = XHT(I,LAY) + DIST2*XDC(I,LAY)
   50   CONTINUE
C
C Fill IQXT with wire info.
        RW(JJ+JIQXPW) = FSW
        DO 60 I=1,3
          RW(JJ+JIQXWX-1+I) = WRX(I)
          RW(JJ+JIQXHX-1+I) = HTX(I)
   60   CONTINUE
        RW(JJ+JIQXDC) = DCA
C
        IF(DEB) THEN
          WRITE(LPR,1001) LAY,IWIRE,IC,ZSW,RSW,FSW
 1001     FORMAT(' ITQUAL: Lay-wire-coor',3I4,' Zsw-Rsw-Fsw',2F7.2,F7.3)
          WRITE(LPR,1002) (WRX(I),I=1,3),(XHT(I,LAY),I=1,3),
     +     (XDC(I,LAY),I=1,3),(HTX(I),I=1,3),DCA,DIST1,DIST2
 1002     FORMAT(5X,' WRX=',3F12.4,/5X,' XHT=',3F12.4,' XDC=',3F12.4,
     +        /5X,' HTX=',3F12.4,/10X,'DOCA',F8.3,'   Dist1-2',2F8.3)
        ENDIF
C
C Skip those with no associated coord.
        IF(IC.LE.0) GOTO 100
C
C Coord: Get the drift distance and error
        DTM  = RTABL(JITCO,IC,JITCDT)
        ZOLD = RTABL(JITCO,IC,JITCZH)
        ERZ  = SQRT(RTABL(JITCO,IC,JITCSZ))
        CALL ICDRIF(LAY,IWIRE,DTM,RSW,ZOLD,ZSW,DS1,DS2,DER)
C
C Choose ambiguity closest to DOCA
        IF(ABS(DS1-DCA) .LT. ABS(DS2-DCA)) THEN
          DDS = DS1
        ELSE
          DDS = DS2
        ENDIF
C
C Residuals
        RSD = ABS(DDS) - ABS(DCA)
        RSF = DDS - DCA
        RSZ = ZOLD - HTX(3)
C
C Fill IQXT with coord. info.
        RW(JJ+JIQXDT) = DTM
        RW(JJ+JIQXDD) = DDS
        RW(JJ+JIQXER) = DER
        RW(JJ+JIQXRD) = RSD
        RW(JJ+JIQXRF) = RSF
        RW(JJ+JIQXRZ) = RSZ
        RW(JJ+JIQXEZ) = ERZ
C
        IF(DEB) THEN
          WRITE(LPR,1003) DTM,ZOLD,ZSW,DS1,DS2,DER
 1003     FORMAT('  DTM-ZOLD-ZSW',3F7.2,'  DS1-2',2F7.3,'  ERR',F7.4)
          WRITE(LPR,1004) DCA,DDS,RSD,RSF
 1004     FORMAT('  DOCA-DDS',2F8.3,'   RSD-RSF',2F8.4)
          WRITE(LPR,1005) LAY,(IW(JJ+I),I=1,3),(RW(JJ+I),I=4,17)
 1005     FORMAT(3X,3I5,I6,F9.4,2F7.3,F7.1,2F7.3,F7.1,F7.3,F7.1,F7.4,
     +       F7.4,2F7.4,F7.1)
        ENDIF
  100 CONTINUE
C
  999 CONTINUE
      END
