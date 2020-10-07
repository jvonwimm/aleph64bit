      SUBROUTINE TSTRAN(X,Y,Z,NEL,ITYPE,IWIR,WIRRD,WIRPH,TDRFT)
C-----------------------------------------------------------------------
C!  Routine to transport an electron to the wire grid.  This routine
C!  accounts for longitudinal and transverse diffusion and ExB effect.
C
C  Called from:  TSTEER
C  Calls:        RANNOR, RNDM, TPEXBS
C
C  Inputs:   PASSED:      --X,Y,Z,  original position of the electron
C                         --ITYPE,  sector type
C                         --NEL,    no. of e-'s to transfer at once
C            /TPCOND/:    --DRFVEL, drift velocity, cm/ns
C                         --SIGTR,  rms transverse diffusion, cm**.5
C                         --SIGMA,  rms longitudinal diffusion, cm**.5
C                         --ITRCON, field configuration
C            /TPGEOM/:    --endplate position, wire geometry
C            /TPGEOW/:
C
C  Outputs:  PASSED:      --IWIR,   wire number hit; = 0 if none hit
C                         --WIRRD,  radius at point of wire hit
C                         --WIRPH,  phi at point of wire hit
C                         --TDRFT,  time for this electron to reach
C                                   wire plane
C  A. Caldwell, D. DeMille
C
C  Modifications:
C             1.  P. Janot, 7Mar88 -- Transport NEL electrons at once
C                                     to the wire grid.  Take NEL into
C                                     account in calulation of r-phi
C                                     and z diffusion.
C             2.  D. Casper, 12 Oct 92 -- add fluctuations in
C                                         adsorption during drift
C
C-----------------------------------------------------------------------
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
      COMMON /TPGEOW/ TWSTEP(LTSTYP),TWIRE1(LTSTYP),NTWIRE(LTSTYP),
     &                TWIRMN(LTWIRE,LTSTYP),TWIRMX(LTWIRE,LTSTYP),
     &                TWIRLE(LTWIRE,LTSTYP),ITLWIF(LTSTYP),
     &                ITLWIL(LTSTYP),NTREG1(4,LTSTYP),TFRATH
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
      COMMON / HISCOM / IHDEDX,IHTRAN,IHAVAL,IHCOUP,IHTRCP,IHBOS,IHTOT
C
      LOGICAL LIN,LDBT1,LDBT2,LDBT3
C
C  Debug levels
C
      ICALLS = ICALLS + 1
      LDBT1 = ( NTPCDT .GE. 1 .AND. ICALLS .LE. NCALDT )
      LDBT2 = ( NTPCDT .GE. 2 .AND. ICALLS .LE. NCALDT )
      LDBT3 = ( NTPCDT .GE. 3 .AND. ICALLS .LE. NCALDT )
C
      IF ( LDBT3 ) WRITE(6,101) X,Y,Z
C
C  Considering the effects of diffusion, find the drift time and arrival
C  position of this electron at the field-sense wire plane.  First find
C  the bare drift distance and time (if bare drift distance is negative,
C  make it zero --DC).
C
      ZDRFT = ZTPCMX - ABS(Z)
      IF (ZDRFT .LT. 0.) ZDRFT = 0.0
      TDRFT = ZDRFT/DRFVEL
C
C  Calculate fluctuations from adsorption (2.5%/m)
C
      XABSOR = 0.025 * ZDRFT/100.
      XATTEN = XABSOR * NEL
      CALL TPELSG(XATTEN,1.,NATTEN)
      NEL = NEL + (XATTEN - NATTEN + 0.5)
      IF(NEL.LE.0)THEN
          NEL = 0
          IWIR = 0
          RETURN
      ENDIF
C
C  Roll the dice for diffusion
C
      CALL RANNOR(RDMR,RDMZ)
      RDMPH = TWOPI*RNDM(A)
      SQRZD = SQRT(ZDRFT)
C
C  R-phi diffusion (include statistical effect of NEL electrons)
C
      SQNEL = AMAX1(1.,SQRT(FLOAT(NEL)))
      RDIFF = RDMR * SIGTR * SQRZD / SQNEL
      XG = X + RDIFF*COS(RDMPH)
      YG = Y + RDIFF*SIN(RDMPH)
C
C  Z-diffusion and time difference (include NEL as above)
C
      ZDIFF = RDMZ * SIGMA * SQRZD / SQNEL
      TDRFT = TDRFT + ZDIFF/DRFVEL
      IF ( TDRFT .LE. 0. ) TDRFT = 0.
C
      IF ( LDBT3 ) WRITE(6,102) XG,YG,TDRFT
      IF ( LDBT1 ) THEN
           DTD = TDRFT - TDRFO
           IF ( ABS(DTD) .GT. 1000. ) DTD = 0.
           CALL HF1(IHTRAN+1,DTD,1.)
           CALL HF1(IHTRAN+2,(XG-X),1.)
           CALL HF1(IHTRAN+3,(YG-Y),1.)
           TDRFO = TDRFT
      ENDIF
C
C  Determine which wire is hit and where; quit if none is hit
C  First get the position of the first wire.
C
      YWIR1 = TWIRE1(ITYPE)
      WSTEP = TWSTEP(ITYPE)
C
C  Get the wire number and height of the wire.  The field wires are
C  +-1/2 wire spacings from the sense wires.
C
      IWIR = INT( (YG - YWIR1 + WSTEP/2.) / WSTEP ) + 1
      YWIR = YWIR1 + FLOAT(IWIR-1)*WSTEP
C
C  See if we have overshot the sector; if so, set iwir = 0 as a flag and
C  quit.
C
      IF ( IWIR .LE. 0  .OR.  IWIR .GT. NTWIRE(ITYPE) ) THEN
         IWIR = 0
         RETURN
      ENDIF
      IF ( LDBT3 ) WRITE(6,104) IWIR,YWIR
C
C  Find the position along the wire, applying ExB.
C  KRET tells us if we succeeded in hitting the wire
C
      YDIFF = YWIR - YG
      CALL TPEXBS(ITRCON,YDIFF,XSHFT,KRET)
C
      IF ( LDBT1 ) CALL HF2(IHTRAN+4,XSHFT,-YDIFF,1.)
      IF ( LDBT2 ) THEN
         IF ( KRET .EQ. 1  ) WRITE(6,106)
         IF ( KRET .EQ. 10 ) WRITE(6,107)
         IF ( KRET .EQ. 11 ) WRITE(6,107)
      ENDIF
      IF ( LDBT3 ) WRITE(6,105) ITRCON,XSHFT
C
C  If we missed the wire, set the flag for the calling program and retur
C
      IF ( KRET .NE. 0 ) THEN
          IWIR = 0
          RETURN
      ENDIF
C
C  Check that we're still on the wire after the shift
C  W is the coordinate along the wire
C
      W = XG + XSHFT
      WMIN = TWIRMN(IWIR,ITYPE)
      WMAX = TWIRMX(IWIR,ITYPE)
C
      LIN = ( (W .GE. WMIN .AND. W .LE. WMAX)     .OR.
     *        (W .GE. -WMAX .AND. W .LE. -WMIN) )
C
      IF ( .NOT. LIN ) THEN
C
C  We've drifted off the wire
C
           IF ( LDBT3 ) WRITE(6,108)
           IWIR = 0
           RETURN
      ENDIF
C
C  Update the drift time because of extra drift from shift.  Assume the
C  electron travels straight to the sense wire plane, then takes a
C  90 degree turn along the wire, all at the same drift velocity
C
      TDRFT = TDRFT +  XSHFT/DRFVEL
C
C  Calculate r,phi position of hit (to be used later on for pads)
C
      WIRRD = SQRT(W*W + YWIR*YWIR)
      WIRPH = ATAN2(W,YWIR)
C
C  We've got it all.
C
      IF ( LDBT2 ) WRITE(6,109) ITYPE,IWIR,W,TDRFT
C_______________________________________________________________________
C
 101  FORMAT(//,10X,'START ELECTRON TRANSPORT',
     *       /,'  ELECTRON X COORD   :',F10.4,
     *       /,'           Y         :',F10.4,
     *       /,'           Z         :',F10.4)
 102  FORMAT(/,'  X AFTER DRIFT      :',F10.4,
     *       /,'  Y AFTER DRIFT      :',F10.4,
     *       /,'  DRIFT TIME -- ns   :',F8.2)
 104  FORMAT(/,'  WIRE NUMBER HIT    :',I3,
     *       /,'  WIRE HEIGHT        :',F10.4)
 105  FORMAT(/,'  GRID CONDITIONS    :',I2,
     *       /,'  ExB SHIFT          :',F7.4)
 106  FORMAT(/,10X,' ELECTRON CAUGHT IN GATING GRID  ')
 107  FORMAT(/,10X,' ExB ROUTINE CALLED WITH INCORRECT VALUE ')
 108  FORMAT(/,10X,' WE HAVE DRIFTED OFF THE WIRE ')
 109  FORMAT(//,10X,' FINAL VALUES FROM TRANSPORT ',
     *       /,'  SECTOR TYPE HIT         -- ',I1,
     *       /,'  WIRE NUMBER HIT         -- ',I3,
     *       /,'  WIRE POSITION OF HIT(cm)-- ',F10.4,
     *       /,'  DRIFT TIME TO WIRE(ns)  -- ',F8.2)
C
      RETURN
      END
