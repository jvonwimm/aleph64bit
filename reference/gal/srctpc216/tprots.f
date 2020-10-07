      SUBROUTINE TPROTS(ITYPE,MISECT,LCUT)
C-----------------------------------------------------------------------
C! Rotate to the sector coordinate frame.  Check phi-acceptance
C! for first check of whether track crosses this sector.
C
C  Called from:  TPBRTK
C  Calls:        ROT (CERN)
C
C  Inputs:   PASSED:      --ITYPE,  sector type
C                         --MISECT, sector number on endplate
C            /TRAKEL/     --primary and secondary track parameters in
C                           the global frame for the track to be rotated
C            /TPGEOM/     --sector positions and phi limits of extended
C            /SCTBND/       sectors
C
C  Outputs:  PASSED:      --LCUT, flag indicating whether track comes
C                                 near sector to be rotated into
C            /TRAKEL/     --primary and secondary track parameters in th
C                           sector frame
C  D. DeMille
C-----------------------------------------------------------------------
C
C  TRAKEL:  track parameters for dE/dX and carrying around broken
C  tracks
C
      COMMON/TRAKEL/NTRK,X(3),VECT(3),ABSMOM,SEGLEN,TOF,AMASS,CHARGE,
     *              RAD,CENT(2),DELPSI,PSI1,ALPH01,ALPH02
C - MXBRK = 2* MAX(NLINES(1..3)) + 2 , NLINES= 8,10,10 in /SCTBND/
      PARAMETER (MXBRK=22, MXBRTE=MXBRK/2)
      COMMON/BRKNTK/XB(3,6),VECTB(3,6),SEGLNB(6)
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,
     +           LMXPDR=150,LTTSRW=11)
C
      COMMON /TPGEOM/RTPCMN,RTPCMX,ZTPCMX,DRTPMN,DRTPMX,DZTPMX,
     &               TPFRDZ,TPFRDW,TPAVDZ,TPFOF1,TPFOF2,TPFOF3,
     &               TPPROW(LTPDRO),TPTROW(LTTROW),NTSECT,NTPROW,
     &               NTPCRN(LTSTYP),TPCORN(2,LTCORN,LTSTYP),
     &               TPPHI0(LTSECT),TPCPH0(LTSECT),TPSPH0(LTSECT),
     &               ITPTYP(LTSECT),ITPSEC(LTSECT),IENDTP(LTSECT)
C
      COMMON /SCTBND/ NLINES(3),SLOPES(10,3),YCEPTS(10,3),
     1                XUPLIM(10,3),XLWLIM(10,3),PHIMAX(3)
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER(CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
C  Additional constants for TPCSIM
C  Units -- Mev,Joules,deg Kelvin,Coulombs
C
      REAL ELMASS,CROOT2,CKBOLT,CROOMT,ECHARG
      PARAMETER (ELMASS = 0.511)
      PARAMETER (CROOT2 = 1.41421356)
      PARAMETER (CKBOLT = 1.380662E-23)
      PARAMETER (CROOMT = 300.)
      PARAMETER (ECHARG = 1.602189E-19)
C
      LOGICAL LCUT
C
      PHIROT = TPPHI0(MISECT)
C
C  A little trick geometry here.  alph01 and alph02 are defined as
C  angles measured counterclockwise from the global x-axis.  Now, we
C  transform them into the sector frame, where they are measured
C  clockwise from the y-axis in the range from -pi to +pi.
C  (This is done only to conform to pre-existing angle conventions;
C  now the alphas are measured in the same way as the other angles in
C  the sector frame).  Note that now alph02 < alph01.
C
      DALPHA = ALPH02 - ALPH01
      ALPH02 = PHIROT - ALPH02
      ALPH02 = AMOD(ALPH02,TWOPI)
      IF ( ABS(ALPH02) .GT. PI )
     *   ALPH02 = ALPH02 - SIGN(1.,ALPH02)*TWOPI
      ALPH01 = ALPH02 + DALPHA
C
C  Now do crude phi-cut; most tracks will be eliminated.
C
      PHILIM = PHIMAX(ITYPE)
C
      IF ( ALPH01 .LT. -PHILIM .OR. ALPH02 .GT. PHILIM ) THEN
         LCUT = .FALSE.
         RETURN
      ELSE
         LCUT = .TRUE.
      ENDIF
C
C  Passed crude phi-cut; now rotate into sector coords
C
      PHIROT = PIBY2 - PHIROT
C
      CALL ROT(X,PHIROT,X)
      CALL ROT(VECT,PHIROT,VECT)
      CALL ROT(CENT,PHIROT,CENT)
C
C  psi1 = angle ( measured counterclockwise from x-axis in a coord. sys.
C  with origin at the center of the circle defined by the x-y proj.
C  of the track ) at the beginning of the track.
C
      XDIFF = X(1) - CENT(1)
      YDIFF = X(2) - CENT(2)
      PSI1 = ATAN2( YDIFF, XDIFF )
      IF ( DELPSI .LT. 0. ) PSI1 = PSI1 + DELPSI
      PSI1 = AMOD(PSI1, TWOPI)
      IF ( PSI1 .LT. 0.0 ) PSI1 = PSI1 + TWOPI
C
      RETURN
      END
