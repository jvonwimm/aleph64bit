      SUBROUTINE TPGBR0(ITYPE,MISECT,NBRTE)
C-----------------------------------------------------------------------
C!  Routine to get a track element and to break it at the boundary of
C!  a sector (ZERO MAGNETIC FIELD CASE)
C
C  Called from:  TPBRTK
C
C  Inputs:  PASSED
C                      --ITYPE, the type of this sector
C                      --MISECT, the sector number within the endplate
C            /TPGEOM/  --Phi position of sector
C            /TRAKEL/  --secondary track parameters for the track
C                        element to be broken
C            /SCTBND/  --x-limits, slopes, and intercepts of the
C                        line segments forming the boundaries of
C                        the extended sectors
C
C  Outputs: PASSED     --NBRTE, the number of subelements ("broken
C                        track elements") of the full track element
C                        which lie within the boundary of the extended
C                        sector corresponding to sector ISECT
C           /TRAKEL/   --Common blocks containing primary and secondary
C                        parameters for track element ISEG and each of
C                        its subelements, for use in dE/dX routine
C  M. Mermikides 21/9/87
C-----------------------------------------------------------------------
C
C  TRAKEL:  track parameters for dE/dX and carrying around broken
C  tracks
C
      COMMON/TRAKEL/NTRK,X(3),VECT(3),ABSMOM,SEGLEN,TOF,AMASS,CHARGE,
     *              RAD,CENT(2),DELPSI,PSI1,ALPH01,ALPH02
C - MXBRK = 2* MAX(NLINES(1..3)) + 2 , NLINES= 8,10,10 in /SCTBND/
      PARAMETER (MXBRK=22, MXBRTE=MXBRK/2)
      COMMON/BRKNTK/XB(3,6),VECTB(3,6),SEGLNB(6),TOFB(6)
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
C
      LOGICAL LIN
      DIMENSION XINT(10),YINT(10),ZINT(10),SINT(10),INDS(10)
C
C  First rotate track to sector ref. frame
C
      PHIROT = TPPHI0(MISECT)
      PHIROT = PIBY2 - PHIROT
      CALL ROT(X,PHIROT,X)
      CALL ROT(VECT,PHIROT,VECT)
C
C  Find the points of intersection of the track in the x-y projection
C  with the line segments forming the sector.
C
      NBRK = 1
      XINT(NBRK) = X(1)
      YINT(NBRK) = X(2)
      ZINT(NBRK) = X(3)
      SINT(NBRK) = 0.
C Equation of track:    Y = AA*X + BB ;
      AA = VECT(2)/VECT(1)
      BB = X(2) - X(1)*AA
      XEND = X(1) + SEGLEN*VECT(1)
      YEND = X(2) + SEGLEN*VECT(2)
      ZEND = X(3) + SEGLEN*VECT(3)
      DXY2 = (XEND-X(1))**2 + (YEND-X(2))**2
C
      DO 1 I = 1, NLINES(ITYPE)
C
C Sector boundary line: Y = SLOPES(i,ITYPE)*X + YCEPTS(i,ITYPE)
C
         XX = -(BB - YCEPTS(I,ITYPE))/(AA-SLOPES(I,ITYPE))
C Skip of intersection is outside sector boundary
         IF (XX.LT.XLWLIM(I,ITYPE).OR.XX.GT.XUPLIM(I,ITYPE)) GO TO 1
         YY = AA*XX + BB
C
C  Keep those intersections which actually lie between the endpoints
C  of the track.
C
         DIST2 = (XX-X(1))**2 + (YY-X(2))**2
         IF (DIST2.GT.DXY2) GO TO 1
         DIST2 = (XX-XEND)**2 + (YY-YEND)**2
         IF (DIST2.GT.DXY2) GO TO 1
         NBRK = NBRK + 1
         XINT(NBRK) = XX
         YINT(NBRK) = YY
         IF (ABS( VECT(2) ) .GT.0.0001) THEN
            SINT(NBRK) = (YY - X(2))/VECT(2)
         ELSE
            SINT(NBRK) = (XX - X(1))/VECT(1)
         ENDIF
         ZINT(NBRK) = X(3) + SINT(NBRK)*VECT(3)
C
 1    CONTINUE
      NBRK = NBRK + 1
      XINT(NBRK) = XEND
      YINT(NBRK) = YEND
      ZINT(NBRK) = ZEND
      SINT(NBRK) = SEGLEN
C
C  Order breakpoints in increasing S (measured from track start point).
C
      CALL SORTZV(SINT,INDS,NBRK,1,NWAY,0)
C
C  See whether the first point of the track segment lies in the sector
C
      CALL TPFIDS(X,ITYPE,LIN)
C
C  Determine the number of broken track elements
C
      IF ( MOD(NBRK,2) .EQ. 1 ) THEN
C
         NBRTE = ( NBRK - 1 )/2
C
      ELSE
C
         IF ( LIN ) THEN
            NBRTE = ( NBRK / 2 )
         ELSE
            NBRTE = ( NBRK / 2 ) - 1
         ENDIF
C
      ENDIF
C
C  Determine the initial and final points of each broken track element
C
      IF ( LIN ) THEN
C
         DO 4 J = 1, NBRTE
            XB(1,J) = XINT( INDS(2*J-1) )
            XB(2,J) = YINT( INDS(2*J-1) )
            XB(3,J) = ZINT( INDS(2*J-1) )
            SEGLNB(J) = SINT( INDS(2*J) ) - SINT( INDS(2*J-1) )
4       CONTINUE
C
      ELSE
C
         DO 5 J = 1, NBRTE
            XB(1,J) = XINT( INDS(2*J) )
            XB(2,J) = YINT( INDS(2*J) )
            XB(3,J) = ZINT( INDS(2*J) )
            SEGLNB(J) = SINT( INDS(2*J+1) ) - SINT( INDS(2*J) )
 5       CONTINUE
C
      ENDIF
      DO 10 J=1,NBRTE
         VECTB(1,J) = VECT(1)
         VECTB(2,J) = VECT(2)
         VECTB(3,J) = VECT(3)
   10 CONTINUE
C
      RETURN
      END
