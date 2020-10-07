      SUBROUTINE X2DFCD
C
C --------------------------------------------------------
C! Read constants for level 2 trigger emulation.
C
C   Author : T. Medcalf   10/9/87
C   Modified T.M.         21/10/88
C
C? Read X2RU run data and assign default values.
C
C    X2RU Card values are
C    1 : Z-Acceptance in mm.(def. 100.)
C    2 : Print level, 0 = most detail.
C    3 : Digital noise: -1 = none, 0 = def. (100 hits), else>0 = # of
C    4 : Drift velocity, assumed for calculation. ( mm / microsec )
C    5 : Drift velocity, physical. ( mm/mcsec )(def. 50. )
C    6,7,8 : Numbers of the padrows ignored by the trigger algorithm.(9,
C    9 : Number of theta-bins in the inner zone.( def. 29 )
C   10 : Number of theta-bins in the outer zone.( def. 58 )
C   11 : Clockrate in MegaHerz. ( def. 5. )
C   12 : Excess timebins allowed by Z-road algorithm. ( def. 5 )
C   13 : Z-road width in timebins. ( def. 1 )
C   14 : 8-digit ( decimal ) number, encoding the pairing-off of pads.
C        def. : 16372514 = highest priority pair is (1,6), then (3,7)
C   15 : 8-digit ( decimal ) number, encoding the pad thresholds.
C        def. : 44434556 = thresholds of 4 for innermost 3 rows, then
C ---------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "x2cons.h"
C
C   These are the default values, which can be changed by the driver ca
C   Assumed and true drift velocities...
      PARAMETER ( ADVEL = 50.0 , TDVEL = 50.0 )
C   Numbers of the ignored Padrows...
      PARAMETER ( IGN1 = 9, IGN2 = 16, IGN3 = 19 )
C   Max. number of theta-bins in each zone...
      PARAMETER ( NTB1 = 29 , NTB2 = 58 )
C   Clock-rate in MHz...
      PARAMETER ( CLCKR = 5. )
C   Excess z-acceptance and road-width...
      PARAMETER ( IZXSD = 5, IRWD = 1 )
C   Pad-pair choices encoded as decimal digits...
      PARAMETER ( NPPWD = 16372514 )
C   Threshold table encoded as decimal digits.
      PARAMETER ( ITHWD = 44434556 )
      EXTERNAL NLINK
      EXTERNAL NAMIND
      INTEGER IPW10(9)
      DATA IPW10 / 100000000 ,10000000 ,1000000 ,100000 ,10000 ,1000,
     +             100, 10, 1 /
C  Macros for decoding words on card.
      NPP( IM, IP ) = 2 * IP - 2 + IM
      NDECO( NW, IMP ) = MOD( NW, IPW10(IMP) ) / IPW10(IMP+1)
C
C   Set values to be altered by data card, if read.
C
C ... Z-acceptance in mm ...
      ZACPMM = 100.0
C ... Print level ...
      IX2PRL = 0
C ... Digital noise level ...
      IDIGNZ = 100
C ... Drift velocities ...
      ADVELO = ADVEL
      TDVELO = TDVEL
C ... Ignored pad rows ...
      IGNPAD(1) = IGN1
      IGNPAD(2) = IGN2
      IGNPAD(3) = IGN3
C ... Number of theta bins inner/outer ...
      NTBINS(1) = NTB1
      NTBINS(2) = NTB2
C ... Clock rate in MHz ...
      CLOCKR = CLCKR
C ... Excess acceptance for z-road ...
      IZXS = IZXSD
C ... Road width ...
      IRWDTH = IRWD
C ... Pad pair tables ...
      NPPW = NPPWD
C ... Threshold table.
      ITHW = ITHWD
C
      IX2RU = IW(NAMIND('X2RU'))
      NX2RU = IW(IX2RU)
      IF( IX2RU .NE. 0 ) THEN
C
        IF( NX2RU.GE.1.AND.RW(IX2RU+1).GT.0.) ZACPMM = RW(IX2RU+1)
C
        IF( NX2RU.GE.2.AND.IW(IX2RU+2).GT.0 ) IX2PRL = IW(IX2RU+2)
C
        IF( NX2RU.GE.3 ) THEN
          IF( IW(IX2RU+3).EQ.-1 ) THEN
            IDIGNZ = 0
          ELSE IF (IW(IX2RU+3).GT.0) THEN
            IDIGNZ = IW(IX2RU+3)
          END IF
        ENDIF
C
        IF( NX2RU.GE.4.AND.RW(IX2RU+4).NE.0.) ADVELO = RW(IX2RU+4)
C
        IF( NX2RU.GE.5.AND.RW(IX2RU+5).NE.0.) TDVELO = RW(IX2RU+5)
C
        IF( NX2RU.GE.6.AND.IW(IX2RU+6).NE.0) IGNPAD(1) = IW(IX2RU+6)
C
        IF( NX2RU.GE.7.AND.IW(IX2RU+7).NE.0) IGNPAD(2) = IW(IX2RU+7)
C
        IF( NX2RU.GE.8.AND.IW(IX2RU+8).NE.0) IGNPAD(3) = IW(IX2RU+8)
C
        IF( NX2RU.GE.9.AND.IW(IX2RU+9).NE.0) NTBINS(1) = IW(IX2RU+9)
C
        IF( NX2RU.GE.10.AND.IW(IX2RU+10).NE.0) NTBINS(2)=IW(IX2RU+10)
C
        IF( NX2RU.GE.11.AND.RW(IX2RU+11).NE.0.) CLOCKR = RW(IX2RU+11)
C
        IF( NX2RU.GE.12.AND.IW(IX2RU+12) .NE.0 ) IZXS = IW(IX2RU+12)
C
        IF( NX2RU.GE.13.AND.IW(IX2RU+13).NE.0) IRWDTH = IW(IX2RU+13)
C
        IF( NX2RU.GE.14.AND.IW(IX2RU+14).NE. 0 ) NPPW = IW(IX2RU+14)
C
        IF( NX2RU.GE.15.AND.IW(IX2RU+15).NE. 0 ) ITHW = IW(IX2RU+15)
      ENDIF
C
      DO 7000 IPAIR = 1,ICHX
        DO 7010 IO = 1,2
          IPADPR(IO,IPAIR) = NDECO( NPPW,NPP(IO,IPAIR) )
 7010   CONTINUE
 7000 CONTINUE
C
      DO 7020 IROW = 1, NPUSE
        ITHRSH(IROW) = NDECO( ITHW,IROW )
 7020 CONTINUE
C
C calculate trigger z-road acceptance from excess and
C bin acceptance
C
      IZBMAX = IZXS+INT(ZACPMM*CLOCKR/ADVELO)
C
      RETURN
      END
#endif
