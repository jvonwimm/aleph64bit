      SUBROUTINE TCTSEC(ISLOT,RG,PHIG,ZG,RS,RPHIS,ZS)
C
C-----------------------------------------------------------------------
C! Transform coordinates to TPC sector frame,
C! including associated alignment corrections
C!
C!     Author:    R. Johnson 20-3-88
C!     Modified:  R. Johnson 15-4-88
C!
C!     Input:
C!         - ISLOT    /I     Sector number
C!         - RG       /R     Radius in global frame
C!         - PHIG     /R     Phi in global frame
C!         - ZG       /R     Z in global frame
C!     Output:
C!         - RS       /R     Radius in sector frame
C!         - RPHIS    /R     R*Phi in sector frame
C!         - ZS       /R     Z in sector frame
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
      DIMENSION X(3),XS(3)
C
C++   Convert to cartesion coordinates
C
      X(1)=RG*COS(PHIG)
      X(2)=RG*SIN(PHIG)
      X(3)=ZG
C
C++   Make a rotation followed by a translation
C
      DO 200 I=1,3
        XS(I)=DGLTOS(I,ISLOT)
        DO 100 J=1,3
          XS(I)=XS(I) + AGLTOS(I,J,ISLOT)*X(J)
  100   CONTINUE
  200 CONTINUE
C
C++   Convert back to cylindrical coordinates.  The angle phi
C++   should be between -pi and pi radians.
C
      RS=SQRT(XS(1)**2+XS(2)**2)
      RPHIS=RS*ATAN2(XS(2),XS(1))
      ZS=XS(3)
C
  999 CONTINUE
      RETURN
      END
