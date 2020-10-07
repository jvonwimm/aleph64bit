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
#ifndef DOC
      SAVE
#include "bcs.h"
#include "alcons.h"
#include "itcojj.h"
#include "iqxtjj.h"
C-----------------------------------------------------------------------
      EXTERNAL NAMIND
      INTEGER ITK,IFLG(*),IWIR(*),ICO(*),IER
      REAL XHT(3,*),XDC(3,*)
      REAL WRD(3),WRX(3),HTX(3)
      LOGICAL  FIRST,DEB
      DATA FIRST/.TRUE./,DEB/.FALSE./
C-----------------------------------------------------------------------
#include "bmacro.h"
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
#endif
