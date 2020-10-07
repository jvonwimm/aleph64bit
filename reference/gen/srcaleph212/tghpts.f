      SUBROUTINE TGHPTS(ISLOT,P,PS)
C
C----------------------------------------------------------------------
C! Transform helix to sector frame
C!
C!    Author:   R. Johnson   07-08-87
C!
C!    Input:
C!       - ISLOT      /I     TPC sector slot number (1-36)
C!       - P(5)       /R     Helix parameters in global frame
C!                           1/r,tanl,phi0,d0,z0
C!                           (d0>0 = positive ang. mom. about z axis)
C!                           (r>0  = counterclockwise rotation)
C!    Output:
C!       - PS(5)      /R     Helix parameters in the reference frame
C!                           of the sector ISLOT:  z=0 is at the
C!                           sense wire plane with the positive z
C!                           axis pointing toward detector center.
C!                           The origin is at the padrow centers, and
C!                           the x axis divides the sector into two
C!                           symmetric pieces.
C!
C!     Description:
C!     -----------
C!     The basic transformation involves just a rotation in phi
C!     and a translation in z and, for endplate 1, A, a reflection
C!     in z.  In principle alignment corrections should be made,
C!     however, that is not done for two reasons: first, after
C!     such corrections, the helix would no longer be aligned with
C!     the z axis;  second, that would waste a lot of time, as this
C!     routine is intended to be used primarily for matching tracks
C!     with wire pulses, for which great accuracy is not necessary
C!     (the cuts for picking up wires being the order of centimeters).
C!     To find an accurate extrapolation of a helix to sector coordinate
C!     one must find the desired point on the helix in the global system
C!     and transform that point, including alignment and field
C!     corrections, into the sector system.
C!
C!--------------------------------------------------------------------
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
C
      DIMENSION P(*),PS(*),X(3),V(3),XS(3),VS(3)
C
      IF (IENDTP(ISLOT).EQ.1) THEN
        PS(1)=-P(1)
        PS(2)=-P(2)
        PS(3)=TPPHI0(ISLOT)-P(3)
        PS(4)=-P(4)
        PS(5)=ZTPCMX-P(5)
      ELSE
        PS(1)=P(1)
        PS(2)=P(2)
        PS(3)=P(3)-TPPHI0(ISLOT)
        PS(4)=P(4)
        PS(5)=ZTPCMX+P(5)
      ENDIF
    1 CONTINUE
      IF (PS(3).GT.PI) THEN
        PS(3)=PS(3)-TWOPI
        GO TO 1
      ELSEIF (PS(3).LT.-PI) THEN
        PS(3)=PS(3)+TWOPI
        GO TO 1
      ENDIF
C
      RETURN
      END
