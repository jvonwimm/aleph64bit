      SUBROUTINE TCRTRA(IEND,R,PHI,Z,RC,PHIC)
C
C-----------------------------------------------------------------------
C! Correct TPC Coordinates for Transverse Drift Velocity Components
C!
C!  Author    :   A. Jahn    89/06/23
C!  Modified  :   R. Johnson 90/06/06   To operate stand-alone in
C!                                      the ALEPHLIB.
C!
C!                F.Ranjard  92/02/24   To call TPDVEL
CKEY TPC,E-FIELD,DISTORTIONS
C!
C!  Input     :
C!                IEND /I  : side of the TPC (1=A, 2=B)
C!                R    /R  : radius of TPC coordinate  [cm]
C!                PHI  /R  : azimuthal angle of TPC coordinate [rad]
C!                Z    /R  : z of TPC coordinate [cm]  (not changed)
C!
C!  Output     :  RC   /R  : corrected radius
C!                PHIC /R  : corrected azimuthal angle
C!
C!  Description
C!  ===========
C!  TCRTRA takes the drift-velocity components ux, uy and uz as
C!  measured by the laser-calibration system and evaluates the
C!  true coordinates via:
C!            x_real = x_meas + zdrft*(ux/uz)
C!            y_real = y_meas + zdrft*(uy/uz)
C!  where zdrft is the drift length.
C!
C!  Note:  the TPC geometry must be initialized before calling this
C!         routine.
C!         TPDVEL must have been called with the right option either
C!         by JULIA or AUNPCK.
C!         It is always called by AUNPCK at beginning of run with
C!         option 'POT'.
C-----------------------------------------------------------------------
      SAVE
C
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
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,
     +           LMXPDR=150,LTTSRW=11)
      COMMON /TPGEOM/RTPCMN,RTPCMX,ZTPCMX,DRTPMN,DRTPMX,DZTPMX,
     &               TPFRDZ,TPFRDW,TPAVDZ,TPFOF1,TPFOF2,TPFOF3,
     &               TPPROW(LTPDRO),TPTROW(LTTROW),NTSECT,NTPROW,
     &               NTPCRN(LTSTYP),TPCORN(2,LTCORN,LTSTYP),
     &               TPPHI0(LTSECT),TPCPH0(LTSECT),TPSPH0(LTSECT),
     &               ITPTYP(LTSECT),ITPSEC(LTSECT),IENDTP(LTSECT)
C
C
      DIMENSION DVX(2),DVY(2),DVZ(2), DVA(3),DVB(3)
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
      XMEAS = R*COS(PHI)
      YMEAS = R*SIN(PHI)
C
      CALL TPDVEL ('POT',DVA,DVB,IER)
      IF (IER.NE.0) GOTO 998
      IF (IEND.EQ.1) THEN
         DVX(1) = DVA(1)
         DVY(1) = DVA(2)
         DVZ(1) = DVA(3)
         ZDRFT=ZTPCMX-Z
      ELSE
         DVX(2) = DVB(1)
         DVY(2) = DVB(2)
         DVZ(2) = DVB(3)
         ZDRFT=ZTPCMX+Z
      ENDIF
C
      DX = ZDRFT*DVX(IEND)/DVZ(IEND)
      DY = ZDRFT*DVY(IEND)/DVZ(IEND)
C
      XREAL = XMEAS - DX
      YREAL = YMEAS - DY
C
      RC= SQRT( XREAL**2 + YREAL**2)
      PHIC= ATAN2( YREAL,XREAL )
C
      IF (PHIC.LT.0.) THEN
        PHIC = PHIC + TWOPI
      ELSEIF (PHIC.GT.TWOPI) THEN
        PHIC = PHIC - TWOPI
      ENDIF
C
      RETURN
C - drift velocity not found, return without corrections
 998  RC = R
      PHIC = PHI
      END
