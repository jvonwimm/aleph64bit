      SUBROUTINE TZCRVD(IEND,R,Z,ZC)
C
C-----------------------------------------------------------------------
C! Correct TPC Coordinates for whatever causes Z residuals in the VDET
C!
C!  Author    :   F. Sefkow  91/09/04
C!
CKEY TPC Z-CORRECTION VDET
C!
C!  Input     :
C!                IEND /I  : side of the TPC (1=A, 2=B)
C!                R    /R  : radius of TPC coordinate  [cm]
C!                Z    /R  : z of TPC coordinate [cm]
C!
C!  Output     :  ZC   /R  : corrected Z coordinate
C!
C!  Description
C!  ===========
C!  Studied:  Z hits in 2 VDET layers (Del R, Del Z apart)
C!  Observed:  Res = alpha * Del Z + beta * Del R
C!             where Res = Z(hit) - Z(track) and
C!             Z(track) calculated from other hit
C!             and track angle lambda as measured by TPC
C!  Possible Interpretation:  alpha: fractional change in v_z(drift)
C!                            beta: bow angle of endplate
C!  Correction:  Z(drift) -> Z(drift) * (1 + alpha)
C!               |lambda| -> |lambda| + beta
C!
C!  Note:  The alpha correction induces a z0 shift
C!         by Del Z0 = - alpha * (+/- ZTPCMX) for tracks in side A/B
C!  Note:  the TPC geometry must be initialized before calling this
C!         routine.
C-----------------------------------------------------------------------
      SAVE
C
      PARAMETER(JEVEEN=1,JEVERN=2,JEVERT=3,JEVEDA=4,JEVETI=5,JEVEEV=6,
     +          JEVEM1=7,JEVEM2=8,JEVEM3=9,JEVEM4=10,JEVETY=11,
     +          JEVEES=12,JEVETE=13,LEVEHA=13)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JTZCID=1,JTZCVR=2,JTZCAA=4,JTZCBA=5,LTZCVA=5)
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
C++   Correction coefficients for sides A (1) and B (2)
C
      REAL ZCALFA(2), ZCBETA(2)
      LOGICAL FIRST
      INTEGER AGETDB
      DATA FIRST/.TRUE./
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
C
      ZC = Z
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTZCV=NAMIND('TZCV')
        NEVEH=NAMIND('EVEH')
        IRLST=0
      ENDIF
C
C++   Get the current run number
C
      KEVEH=IW(NEVEH)
      IF (KEVEH.EQ.0) THEN
        CALL ALTELL('TZCVDT: cannot find EVEH bank to get the'//
     &              ' run number.',0,'RETURN')
        IRUN=4001
      ELSE
        IRUN=IW(KEVEH+JEVERN)
      ENDIF
C
C++   Link to the TPC z correction bank
C
      IF (IRUN.LE.2000) THEN
        RETURN
      ELSE
        IF (IRLST.NE.IRUN) THEN
          IRET=AGETDB('TZCV',IRUN)
          IF (IRET.EQ.0) THEN
            CALL ALTELL(
     &       'Bank TZCV is missing from the database.',0,'RETURN')
          ENDIF
          KTZCV=IW(NTZCV)
          IF (KTZCV.NE.0) THEN
            ZCALFA(1) = RTABL (KTZCV,1,JTZCAA)
            ZCALFA(2) = RTABL (KTZCV,2,JTZCAA)
            ZCBETA(1) = RTABL (KTZCV,1,JTZCBA)
            ZCBETA(2) = RTABL (KTZCV,2,JTZCBA)
          ELSE
            ZCALFA(1) = 0.
            ZCALFA(2) = 0.
            ZCBETA(1) = 0.
            ZCBETA(2) = 0.
          ENDIF
          IRLST=IRUN
        ENDIF
      ENDIF
C
C++   Calculate drift lenghth ZDRFT ( > 0 always)
C
      IF (IEND.EQ.1) THEN
        ZDRFT=ZTPCMX-Z
      ELSE
        ZDRFT=ZTPCMX+Z
      ENDIF
C
C++   Correct for possibly bowed TPC endplate
C
      ZDCOR = ZDRFT - ZCBETA (IEND) * R
C
C++   Correct for drift velocity error
C
      ZDCOR = ZDCOR * (1. + ZCALFA (IEND))
C
      IF (IEND.EQ.1) THEN
        ZC = ZTPCMX-ZDCOR
      ELSE
        ZC = ZDCOR-ZTPCMX
      ENDIF
C
      RETURN
      END
