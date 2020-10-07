      SUBROUTINE TPBRTP(NTREL,IERR)
C-----------------------------------------------------------------------
C!  Calculate secondary track parameters needed for breaking track
C!  elements at sector boundaries.
C
C  Called from: TSTEER
C  Calls      : WBANK
C
C  Input:  BANKS   --named bank 'TPTE' containing primary track
C                    track parameters for each track element
C
C  Output: PASSED       --NTREL, the number of track elements in
C                                this event
C          /TPCBOS/     --NATPTE, name index of track element bank
C
C          BANKS        --work bank id INDBRT containing secondary track
C                         parameters concerning the x-y projection of
C                         each track, which is an arc of a circle
C
C                         IW(INDBRT + 1) = no of words/track element
C                         IW(INDBRT + 2) = no of track elements
C                         KBR = INDBRT + 2
C
C                         RW(KBR + 1) = radius of circle
C                         RW(KBR + 2) = x-coord of center of circle
C                         RW(KBR + 3) = y-coord of center
C                         RW(KBR + 4) = signed arclength
C                         RW(KBR + 5) = minimum phi value of arc
C                         RW(KBR + 6) = maximum phi value of arc
C
C                         The sequence corresponds to that of 'TPTE',
C                         including neutral track elements.  The work
C                         bank is dropped in TSFINI after the
C                         digitization is done
C  D. DeMille
C
C  Modifications:
C
C     1.  D.Cowen 4-April-88  --Prevent subroutine from crashing when
C                               it is given a track element whose length
C                               measured from the origin <= TRLMO by
C                               setting (x0,y0,z0) =
C                               (x0+dxds,y0+dyds,z0+dzds),
C                               i.e., moving the starting point 1cm in
C                               the direction of the track element.
C                               TPTE bank's (x0,y0,z0) are overwritten.
C                               Track curvature due to B-field can be
C                               ignored over such a short distance.
C     2.  D.Cowen 22feb89     --Add error return and error message for
C                               when WBANK call gives error return.
C     3.  P.Janot 03mar89     --Redefine local indices before and after
C                               a WBANK call (garbage collection)
C-----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
C  TPCBOS contains parameters for handling BOS banks used in the
C  generation of analog and digitized signals in the TPC
C  NCHAN = number of channel types for analog signals and digitizations
C  at present, NCHAN = 3; 1 = wires, 2 = pads, 3 = trigger pads.
      PARAMETER ( NCHAN = 3 )
C
C  Work bank id's.  INDREF(ich) = index for signal reference bank for
C  channel of type ich; INDSIG = index for signal bank for current
C  channel.  IDCLUS = index for cluster bank
C
      COMMON/WORKID/INDREF(NCHAN),INDSIG,IDCLUS,ITSHAP,ITDSHP,
     *              ITPNOI,ITSNOI,ITPULS,ITMADC,INDBRT,INDHL,INDDI
C
C  Parameters for analog signal work banks:  for each type of channel,
C  include max number of channels, default number of channels in
C  signal bank, and number of channels by which to extend signal bank
C  if it becomes full; also keep counter for number of blocks actually
C  filled in signal bank
C
      COMMON/ANLWRK/MAXNCH(NCHAN),NDEFCH,NEXTCH,NTSGHT
C
C  Parameters for digitises (TPP) output banks
C
      COMMON/DIGBNK/NDIDEF(3),NDIEXT(3)
C
C  Hit list and digitization bank parameters: for each type of channel
C  include name nam, default length ndd, and length of extension nde.
C
      COMMON/TPBNAM/DIGNAM(2*NCHAN)
      CHARACTER*4 DIGNAM
C  Name index for track element bank
      COMMON/TPNAMI/NATPTE
C
C
C  TPCOND  conditions under which this simulation
C  will be performed
C
      COMMON /DEBUGS/ NTPCDD,NCALDD,NTPCDT,NCALDT,NTPCDA,NCALDA,
     &                NTPCDC,NCALDC,NTPCDS,NCALDS,NTPCDE,NCALDE,
     &                NTPCDI,NCALDI,NTPCSA,NCALSA,NTPCDR,NCALDR,
     &                LTDEBU
      LOGICAL LTDEBU
      COMMON /SIMLEV/ ILEVEL
      CHARACTER*4 ILEVEL
      COMMON /GENRUN/ NUMRUN,MXEVNT,NFEVNT,INSEED(3),LEVPRO
      COMMON /RFILES/ TRKFIL,DIGFIL,HISFIL
      CHARACTER*64 TRKFIL,DIGFIL,HISFIL
      COMMON /TLFLAG/ LTWDIG,LTPDIG,LTTDIG,LWREDC,FTPC90,LPRGEO,
     &                LHISST,LTPCSA,LRDN32,REPIO,WEPIO,LDROP,LWRITE
      COMMON /TRANSP/ MXTRAN,CFIELD,BCFGEV,BCFMEV,
     &                        DRFVEL,SIGMA,SIGTR,ITRCON
      COMMON /TPCLOK/ TPANBN,TPDGBN,NLSHAP,NSHPOF
      COMMON /AVLNCH/ NPOLYA,AMPLIT,GRANNO(1000)
      COMMON /COUPCN/ CUTOFF,NCPAD,EFFCP,SIGW,SIGH,HAXCUT
      COMMON /TGCPCN/ TREFCP,SIGR,SIGARC,RAXCUT,TCSCUT
      COMMON /DEFAUL/ PEDDEF,SPEDEF,SGADEF,SDIDEF,WPSCAL,NWSMAX,THRZTW,
     &                LTHRSH,NPRESP,NPOSTS,MINLEN,
     &                LTHRS2,NPRES2,NPOST2,MINLE2
      COMMON /SHAOPT/ WIRNRM,PADNRM,TRGNRM
C
      LOGICAL LTWDIG,LTPDIG,LTTDIG,LPRGEO,
     &        LWREDC,LTPCSA,LHISST,FTPC90,LRND32,
     &        REPIO,WEPIO,LDROP,LWRITE
C
      LOGICAL LTDIGT(3)
      EQUIVALENCE (LTWDIG,LTDIGT(1))
C
      REAL FACNRM(3)
      EQUIVALENCE (WIRNRM,FACNRM(1))
C
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
C  Minumum distance from origin for start of track element
C  before modification 1 (see above) is enabled.
C
      PARAMETER (TRLMO=0.02)
      DATA NWBRT/6/,ICALL/0/
      IERR = 0
C
      NATPTE = NAMIND('TPTE')
      INDT = IW(NATPTE)
C
C  Get number of track elements
C
      NWPTE = IW(INDT+1)
      NTREL = IW(INDT+2)
C
C  Create work bank for parameters used in breaking track elements
C
      NWRDS = 2 + NTREL*NWBRT
      CALL WBANK(IW,INDBRT,NWRDS,*999)
C
      IW(INDBRT + 1) = NWBRT
      IW(INDBRT + 2) = NTREL
      KBR = INDBRT + 2
      NATPTE = NAMIND('TPTE')
      INDT   = IW(NATPTE)
C
      IF (ABS(CFIELD).LT.0.0001) RETURN
C
C  Loop over all track elements
C
      DO 1 ISEG = 1, NTREL
C
C  Get primary parameters for this track
C
         NBGNTE = INDT + 2 + (ISEG-1)*NWPTE
C
         X0 = RW(2+NBGNTE)
         Y0 = RW(3+NBGNTE)
         Z0 = RW(4+NBGNTE)
         DXDS = RW(5+NBGNTE)
         DYDS = RW(6+NBGNTE)
         DZDS = RW(7+NBGNTE)
         ABSMOM = RW(8+NBGNTE)
         SEGLEN = RW(9+NBGNTE)
         BQ = RW(12+NBGNTE)*SIGN(1.,CFIELD)
C
C  See if this track is neutral.  If it is, we don't want to process it
C
         IF ( ABS(BQ) .LT. 0.001 ) GOTO 1
C
C  See if the distance of the starting point from the origin is too
C  small; if it is, boost it by 1 cm by adding (dxds,dyds,dzds), and
C  change accordingly in the TPTE bank itself.
C
         IF ( SQRT(X0**2 + Y0**2 + Z0**2)  .LE. TRLMO ) THEN
            X0 = X0 + DXDS
            Y0 = Y0 + DYDS
            Z0 = Z0 + DZDS
            RW(2+NBGNTE) = X0
            RW(3+NBGNTE) = Y0
            RW(4+NBGNTE) = Z0
            IF (ICALL.EQ.0) THEN
               WRITE (6,100) X0,Y0,Z0
               ICALL = 1
            ENDIF
  100       FORMAT(//,'          +++TPBRTP+++',/,
     &                ' A track element started too near (0,0,0)!',/,
     &                ' Changed (x0 y0 z0) to (',3(F7.5,2X),')',/,
     &                ' (This is the last warning of this type.)',
     &             //)
         ENDIF
C
C  Compute the radius and center of the track in x-y projection
C
         TCOMP = SQRT( DXDS*DXDS + DYDS*DYDS )
         RAD = ABSMOM * TCOMP / (2.997925E-4 * ABS(CFIELD))
         XC = ( BQ * RAD * DYDS / TCOMP ) + X0
         YC = ( -BQ * RAD * DXDS / TCOMP ) + Y0
C
C  Parameterize the circle in the x-y projection by
C    x = rad * cos(psi) + xc
C    y = rad * sin(psi) + yc
C
         DELX = X0 - XC
         DELY = Y0 - YC
C
         RCROST = DYDS*DELX - DXDS*DELY
         ROTDIR = SIGN( 1., RCROST )
C
         PSI1 = ATAN2( DELY,DELX )
         DELPSI = ROTDIR * SEGLEN * TCOMP / RAD
C
C  We wish to describe the arc covered by the track element in the
C  x-y projection by its starting angle psi1 and ending angle psi2.
C  as a convention, we wish psi1 < psi2, 0 < psi1 < 2pi.  Note that
C  'starting' and 'ending' do not refer to the actual path direction,
C  but rather simply to the fact that psi1 < psi2.  The sign of delpsi,
C  however, indicates the direction of the track (- = clockwise,
C  + = counterclockwise)
C
         IF ( DELPSI .LT. 0 ) PSI1 = PSI1 + DELPSI
         PSI1 = AMOD(PSI1, TWOPI)
         IF ( PSI1 .LT. 0.0 ) PSI1 = PSI1 + TWOPI
         PSI2 = PSI1 + ABS(DELPSI)
C
C  We wish to compute the phi extent of the arc.  In what follows,
C  psit1 and psit2 are the points at which lines through the origin are
C  tangent to the circle, with psit1 < psit2.  psi01 and psi02 are the
C  points at which the radial lines from the origin corresponding to
C  the phi boundaries of the arc will intersect the circle.
C
         RHO = SQRT( XC*XC + YC*YC )
C
         IF ( RHO .LE. RAD ) THEN
C
            PSI01 = PSI1
            PSI02 = PSI2
C
         ELSE
C
            PSIRC = ATAN2( YC, XC )
            DPSIT = ACOS( RAD / RHO )
C
            IF ( XC .GT. 0. ) THEN
               MFAC = 1
               AFAC = 0.
            ELSE
               MFAC = -1
               AFAC = 0.
               IF ( YC .LT. 0. ) AFAC = TWOPI
            ENDIF
C
            PSIT1 = PSIRC + MFAC*(PI - DPSIT) + AFAC
            PSIT2 = PSIT1 + 2.*DPSIT
C
C
            IF ( PSI1 .LT. PSIT1 .AND. PSIT1 .LT. PSI2 ) THEN
               PSI01 = PSIT1
            ELSE
               PSI01 = PSI1
            ENDIF
C
            IF ( PSI1 .LT. PSIT2 .AND. PSIT2 .LT. PSI2 ) THEN
               PSI02 = PSIT2
            ELSE
               PSI02 = PSI2
            ENDIF
C
         ENDIF
C
C  Compute the phi boundaries of the arc (phi measured counterclockwise
C  from the x-axis)
C
         X01 = RAD * COS( PSI01 ) + XC
         Y01 = RAD * SIN( PSI01 ) + YC
         X02 = RAD * COS( PSI02 ) + XC
         Y02 = RAD * SIN (PSI02 ) + YC
C
         PHI01 = ATAN2( Y01, X01 )
         PHI02 = ATAN2( Y02, X02 )
C
         KFLAG = 0
         IF ( ABS(PHI01-PHI02) .GT. PI ) KFLAG = 1
C
         IF ( PHI02 .LT. PHI01 ) THEN
              PHITMP = PHI01
              PHI01 = PHI02
              PHI02 = PHITMP
         ENDIF
C
         IF ( KFLAG .EQ. 1 ) THEN
              PHITMP = PHI02
              PHI02 = PHI01 + TWOPI
              PHI01 = PHITMP
         ENDIF
C
C  Fill the work bank which holds these parameters
C
         KBR = INDBRT + (ISEG-1)*NWBRT + 2
         RW(KBR + 1) = RAD
         RW(KBR + 2) = XC
         RW(KBR + 3) = YC
         RW(KBR + 4) = DELPSI
         RW(KBR + 5) = PHI01
         RW(KBR + 6) = PHI02
C
C If not last track element, go to next track element
C
 1    CONTINUE
C
      RETURN
  999 WRITE(6,998)
  998 FORMAT(' +++TPBRTP+++ Insufficient BOS array space for',
     .       ' booking track element banks - EXIT '/)
      IERR = 1
      RETURN
      END
