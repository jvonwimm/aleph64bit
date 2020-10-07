      SUBROUTINE IMICCO(IER)
C-----------------------------------------------------------------------
CKEY ITC
C! Make corrected coordinate bank (ICCO) from track and coord banks.
C!
C!   Author:     J.Sedgbeer 20/04/90
C!   Modified:   J.Sedgbeer 24/10/91 Use IRESRP to set rphi res. in ICCO
C!
C!   Input:
C!     common  :  /BCS/ for BOS banks FRFT,FRTL,FICL,ITCO
C!     params. :  FRFTJJ
C!                FRTLJJ
C!                ITCOJJ
C!                ICCOJJ
C!   Output:
C!     IER    /I  : Error flag:
C!                    = 0 if O.K.
C!                    = 1 if missing (or empty) input banks
C!                    = 2 no room to create banks.
C!                    =-1 O.K. but garbage collection done.
C!     ICCO bank of corrected ITC coords.
C!
C!   calls     : ITXING, ITCFLG, ITQUAL, IUPCCO
C!
C!   Libraries required: BOS
C!
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "frftjj.h"
#include "frtljj.h"
#include "itcojj.h"
#include "iccojj.h"
C-----------------------------------------------------------------------
      EXTERNAL NAMIND,IRESRP,AUARCM
      REAL IRESRP
      INTEGER NBNK,IFLMX,IZTST
      INTEGER IER,NXNG,ICLW(8),IFLG(8),IWIR(8),ICO(8)
      REAL XHT(3,8),XDC(3,8),DRF(8)
      LOGICAL  FIRST,DEB
      DATA FIRST/.TRUE./,DEB/.FALSE./
      DATA NBNK/0/, IFLMX/2/, IZTST/1/
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C Initialise
      IF(FIRST) THEN
        LPR = IW(6)
        LFRFT = NAMIND('FRFT')
        LFRTL = NAMIND('FRTL')
        LFICL = NAMIND('FICL')
        LITCO = NAMIND('ITCO')
        FIRST = .FALSE.
      ENDIF
      IER = 1
      IGARB = 0
C
C Check existence of banks
C
      JITCO = IW(LITCO)
      JFRFT = IW(LFRFT)
      JFRTL = IW(LFRTL)
      JFICL = IW(LFICL)
      IF(JITCO.LE.0.OR.JFRFT.LE.0.OR.JFICL.LE.0.OR.JFRTL.LE.0) GOTO 999
      NCO  = LROWS(JITCO)
      NTRK = LROWS(JFRFT)
      NTK  = LROWS(JFRTL)
      NIC  = LROWS(JFICL)
      IF(NCO.LE.0.OR.NTRK.LE.0.OR.NTK.LE.0.OR.NIC.LE.0) GOTO 999
C
      IF(DEB) WRITE(LPR,1000) NCO,NIC,NTRK,NTK
 1000 FORMAT('0IMICCO: No ITC coords(on tracks)',2I4,' # tracks',2I4)
C
C-----------------------------------------------------------------------
C Create ICCO - initially fill with uncorrected coords. from ITCO
C
      LEN = NCO*LICCOA + LMHLEN
      CALL AUBOS('ICCO',0,LEN,JICCO,IGARB)
      IF(IGARB.EQ.2) THEN
        IER = 2
        GOTO 999
      ENDIF
      JITCO = IW(LITCO)
      JFICL = IW(LFICL)
C
      IW(JICCO+LMHCOL) = LICCOA
      IW(JICCO+LMHROW) = NCO
C
      IF(DEB) WRITE(LPR,1004)
 1004 FORMAT('0IMICCO: Initial ICCO bank:')
C
C Loop over ITC used coords (in FICL). Fill ICCO from ITCO
C
      DO 10 IU=1,NIC
        ICS = IW(JFICL+LMHLEN+IU)
        IC = IABS(ICS)
        JC = KROW(JICCO,IC)
        JT = KROW(JITCO,IC)
        LAY = IW(JT+JITCWN)/1000
        RW(JC+JICCRV) = RW(JT+JITCRA)
        RW(JC+JICCPH) = RW(JT+JITCP1)
        IF(ICS.LT.0) RW(JC+JICCPH) = RW(JT+JITCP2)
        RW(JC+JICCZV) = RW(JT+JITCZH)
        DDS = AUARCM(RW(JT+JITCP1)-RW(JT+JITCP2))*0.5*RW(JT+JITCRA)
        DDS = ABS(DDS)
        IF(ICS.LT.0) DDS = -DDS
        RW(JC+JICCSR) = ( IRESRP(LAY,DDS,1.) )**2
        RW(JC+JICCSZ) = RW(JT+JITCSZ)
        IF(DEB) WRITE(LPR,1003) IU,ICS,(RW(JC+I),I=1,5),
     +                                 (RW(JT+I),I=2,7)
 1003   FORMAT(3X,2I5,' ICCO',F8.3,F8.4,8X,F8.1,F8.5,F10.1,
     +           /13X,' ITCO',F8.3,2F8.4,F8.1,F8.5,F10.1)
   10 CONTINUE
C-----------------------------------------------------------------------
C Create IWCR and ILIV banks
C
      CALL ILATCH('S+',IER)
      IF(IER.GT.0) GOTO 999
      IF(IER.LT.0) IGARB = 1
      CALL ILIVEW('S+',IER)
      IF(IER.GT.0) GOTO 999
      IF(IER.LT.0) IGARB = 1
C-----------------------------------------------------------------------
C Loop over tracks in FRFT that have ITC coords in 1st arc.
C
      DO 100 ITK = 1,NTK
        JFRFT = IW(LFRFT)
        JFRTL = IW(LFRTL)
        JN = KROW(JFRTL,ITK)
        JF = KROW(JFRFT,ITK)
        NI  = IW(JN+JFRTNI)
        IOF = IW(JN+JFRTII)
C
        IF(DEB) WRITE(LPR,1001) ITK,NI,IOF
 1001   FORMAT(' TRACK',I4,'   No. ITC coords',I5,' Iof',I4)
C
        IF(NI.LE.0) GOTO 100
C
C Get the track crossings in the ITC
        CALL ITXING(RW(JF+1),IZTST,NXNG,ICLW,XHT,XDC,DRF)
        IF(NXNG.LE.0) GOTO 100
C
        IF(DEB) WRITE(LPR,1010) (RW(JF+I),I=1,5),NXNG,ICLW
 1010   FORMAT(' IMICCO: TRACK PARAMS',5F8.4,' No. Xings',I5,/
     +           10X,'ICLW',8I4)
C
C Set flag,wire and coord lists according to FICL list.
        JI = JFICL+LMHLEN+IOF
        CALL ITCFLG(NI,IW(JI+1),ICLW,IFLG,IWIR,ICO,IER)
C
        IF(DEB) WRITE(LPR,1011) NI,(IW(JI+I),I=1,NI)
 1011   FORMAT(' After ITCflg: NI',I4,' COORDS',8I5)
        IF(DEB) WRITE(LPR,1012) IFLG,IWIR,ICO
 1012   FORMAT(' IFLG',8I5,/' IWIR',8I5,/' ICO ',8I5)
        IF(IER.NE.0) THEN
          IF(DEB) WRITE(LPR,1002) IER
 1002     FORMAT(' After call to ITCFLG: IER=',I8)
          IF(IER.GT.0) GOTO 998
        ENDIF
C
C Get track info. (DOCAs,residuals etc.) in bank IQXT number Nbnk
        CALL ITQUAL(NBNK,IFLG,IWIR,ICO,XHT,XDC,IER)
        IF(DEB) WRITE(LPR,1013) IER
 1013   FORMAT(' After call itqual IER=',I4)
        IF(IER.GT.0) GOTO 998
        IF(IER.LT.0) IGARB=1
C
C Create/update ICCO for these coords.
        CALL IUPCCO(NBNK,IFLMX,IER)
        IF(IER.GT.0) GOTO 998
        IF(IER.LT.0) IGARB=1
  100 CONTINUE
C
      IF(IGARB.EQ.1) IER = -1
C
  998 CALL BDROP(IW,'IQXT')
  999 CONTINUE
      END
#endif
