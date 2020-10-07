      SUBROUTINE TCTGLB(ISLOT,RS,RPHIS,ZS,R,RPHI,Z)
C
C-----------------------------------------------------------------------
C! Transform TPC coordinates to global frame,
C! including associated alignment corrections
C!
C!     Author:    R. Johnson 7-2-87
C!     Modified:  R. Johnson 3-8-87
C!
C!     Input:
C!         - ISLOT    /I     Sector number
C!         - RS       /R     Radius in sector frame
C!         - RPHIS    /R     R*Phi is sector frame
C!         - ZS       /R     Z in sector frame
C!     Output:
C!         - R        /R     Radius in global frame
C!         - RPHI     /R     R*Phi in global frame
C!         - Z        /R     Z in global frame
C!
C!     Called by TCOOR
C!
C!----------------------------------------------------------------------
      SAVE
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
      COMMON/TALIGN/ ASTOGL(3,3,LTSECT),DSTOGL(3,LTSECT),
     &               AGLTOS(3,3,LTSECT),DGLTOS(3,LTSECT),
     &               TAPRRD(LTPDRO,LTSTYP),TAPRPH(LTPDRO,LTSTYP),
     &               ASTOTP(3,3,LTSECT),DSTOTP(3,LTSECT),
     &               ATPTOS(3,3,LTSECT),DTPTOS(3,LTSECT)
C
C
      DIMENSION XS(3),XG(3)
C
C++   Convert to cartesian coordinates in sector frame
C
      PHIS=RPHIS/RS
      XS(1)=RS*COS(PHIS)
      XS(2)=RS*SIN(PHIS)
      XS(3)=ZS
C
C++   Make a rotation followed by a translation
C
      DO 200 I=1,3
        XG(I)=DSTOGL(I,ISLOT)
        DO 100 J=1,3
          XG(I)=XG(I) + ASTOGL(I,J,ISLOT)*XS(J)
  100   CONTINUE
  200 CONTINUE
C
C++   Convert back to cylindrical coordinates
C
      R=SQRT(XG(1)**2+XG(2)**2)
      PHI=ATAN2(XG(2),XG(1))
      Z=XG(3)
C
C++   Place the phi value between 0 and 2pi
C
      IF (PHI.LT.0.) THEN
        RPHI=R*(PHI+TWOPI)
      ELSE
        RPHI=R*PHI
      ENDIF
C
  999 CONTINUE
      RETURN
      END
