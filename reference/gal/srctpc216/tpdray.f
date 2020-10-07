      SUBROUTINE TPDRAY(POI,ZB,NCL,TOTDIS,NSEGT,ISECT,ITYPE,NEL,IRETS)
C
C! Fast sim : Produce and digitize delta-rays in a super-broken segment.
C
C  Subroutine to determine the energy of the NCL primaries. If it is
C  below the cutoff for consideration of a delta-ray, we set the
C  PRIMary Energy = 0 and calculate the cluster size directly in
C  SUBROUTINE TPSIZE; otherwise, we keep the calculated value of
C  PRIME and treat the primary in detail.
C
C  Called from: T2DEDX
C  Calls:       TPRIMB, TPDMOM, TPDPAR, RANNOR, RNDM, TSTRAN
C               TSAVAL, TSPCPL, TSTCPL, TPSGNL
C
C  Inputs :    PASSED:  --POI, average number collisions per cm
C                       --ZB, charge**2/beta**2 of primary track
C                       --NCL,   number of primaries
C                       --TOTDIS,path length
C                       --INDEX, index of segment-bank
C                       --ITYPE, sector type
C
C  Output :    PASSED:  --NEL,   number of no-delta primaries
C
C  P. Janot  20/12/87
C  Modifications:
C     1. D. Cowen    1 Nov 1988
C        Add Mer's correction for the determination of the time
C        of delta rays.
C     2. P. Janot    3 Mar 1989
C        Do not pass work bank index as an argument
C     3. P. Janot    5 Mar 1988
C        Set to zero NLPHIT  and NTPHIT (otherwise these
C        variables are not initialized and lead to an infinite
C        loop in TPSGNL)
C--------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
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
C  TRAKEL:  track parameters for dE/dX and carrying around broken
C  tracks
C
      COMMON/TRAKEL/NTRK,X(3),VECT(3),ABSMOM,SEGLEN,TOF,AMASS,CHARGE,
     *              RAD,CENT(2),DELPSI,PSI1,ALPH01,ALPH02
C - MXBRK = 2* MAX(NLINES(1..3)) + 2 , NLINES= 8,10,10 in /SCTBND/
      PARAMETER (MXBRK=22, MXBRTE=MXBRK/2)
      COMMON/BRKNTK/XB(3,6),VECTB(3,6),SEGLNB(6)
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
      COMMON /AVLNCH/ NPOLYA,AMPLIT
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
C
C  TPCONS contains physical constants for TPC simulation
C
      COMMON /DELTA/ CDELTA,DEMIN,DEMAX,DELCLU,RADFAC,CYLFAC
      PARAMETER (MXGAMV = 8)
      COMMON /TGAMM/ GAMVAL(MXGAMV),GAMLOG(MXGAMV),POIFAC(MXGAMV),
     &               POIMAX,POIMIN,CFERMI,CA,CB,POIRAT,POICON
      PARAMETER (MXBINC = 20)
      COMMON /CLUST/ EBINC(MXBINC),CONCLU,WRKFUN,MXCL,CKNMIN,CFANO,CRUTH
     &              ,POWERC
      COMMON /AVALA/ THETA,ETHETA
      COMMON /TPTIME/ NTMXSH,NTMXNO,NTMXAN,NTMXDI,NTSCAN,NTBNAS,NTBAPD
      COMMON /TPELEC/ TPRPAR,TPRSER,TPCFET,TCFEED,TMVPEL,TSIGMX,NTPBIT
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
      COMMON / HISCOM / IHDEDX,IHTRAN,IHAVAL,IHCOUP,IHTRCP,IHBOS,IHTOT
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
      LOGICAL LDBD1,LDBD2,LDBD3
      DIMENSION CC(3),PRMVC(3),XX(3),XY(3)
      DATA ICALL/0/,IPACK/20/
      DATA NLPHIT,NTPHIT/0,0/
C
C  Debug levels
C
      IRETS = 0
      ICALL = ICALL + 1
      LDBD1 = ( NTPCDD .GE. 1 .AND. ICALL .LE. NCALDD )
      LDBD2 = ( NTPCDD .GE. 2 .AND. ICALL .LE. NCALDD )
      LDBD3 = ( NTPCDD .GE. 3 .AND. ICALL .LE. NCALDD )

      PRIME = 0.
      CALL TPRIMB(POI,ZB,NCL,PRIME,NDEL)
      NEL = NCL-NDEL
      IF(PRIME.EQ.0.) THEN
         RETURN
      ELSE
C
C  Get the momentum of the delta from kinematics
C
         INDEX = IDCLUS + 2 + (NSEGT-1)*10
         XX(1) = RW(INDEX+1)
         XX(2) = RW(INDEX+2)
         XX(3) = RW(INDEX+3)
         CC(1) = RW(INDEX+5)
         CC(2) = RW(INDEX+6)
         CC(3) = RW(INDEX+7)
C
         IF(LDBD3) WRITE(6,3672) XX
 3672    FORMAT(1X,' Original segment coordinates : '/1X,3F10.5)
C
         CALL TPDMOM(ABSMOM,CC,AMASS,GAMMA,PRIME,PRIMP,PRMVC)
C
         IF ( LDBD3 ) WRITE(6,150)
         IF ( LDBD3 ) WRITE(6,103) PRIMP,PRIME,(PRMVC(J),J=1,3)
         IF ( LDBD1 ) CALL HF1(IHDEDX+2,ALOG10(PRIME),1.)
C
C  Our delta-ray model is to assume that the clusters formed by the
C  passage of the delta will be formed on a cylinder whose radius DLRAD
C  and length DLZLN depend on the delta ray energy and momentum (and
C  some ad hoc assumptions).
C  DLBET = average beta=(v/c) for this primary
C
         CALL TPDPAR(PRIMP,PRMVC,DLRAD,DLZLN,DLBET)
C
         IF ( LDBD1 ) THEN
            CALL HF1(IHDEDX+3,DLZLN*SIGN(1.,PRMVC(3)),1.)
            CALL HF1(IHDEDX+4,DLRAD,1.)
            IF ( LDBD3 ) WRITE(6,109) DLRAD,DLZLN,1./PRMVC(3),DLBET
         ENDIF
C
C  We wish to take steps of length DELCLU in Z, and deposit the correct
C  fraction of the delta-ray energy at each step.  If the deposited
C  energy for steps of length DELCLU is less than the ionization
C  potential CKNMIN, we adjust the step size up so that this delta will
C  actually generate some clusters.  We end up with a step length DLSTP
C  and energy dep/step (i.e., per cluster) DLCLE
C
C
C  If the delta's path length along z is smaller than DELCLU then make
C  the z path length equal to DELCLU so that all the energy is
C  deposited in a single cluster.
C
         IF (DLZLN.LT.DELCLU) DLZLN = DELCLU
         TRYEN = (PRIME/DLZLN)*DELCLU*FLOAT(IPACK)
C
         IF ( TRYEN .GE. CKNMIN ) THEN
            DLCLE = TRYEN
            DLSTP = DELCLU*FLOAT(IPACK)
         ELSE
            DLCLE = 2*CKNMIN
            DLSTP = DLCLE*DLZLN/PRIME
         ENDIF
C
         IF ( LDBD3 ) WRITE(6,110) DLCLE
C
C  Set num of clusters from delta and length of delta Z-path covered
C
         NDLCL = 0
         DLCLN = 0.
C
C  Now actually start getting clusters from this delta
C
 21      NDLCL = NDLCL + 1
         N1    = 0
         IF ( LDBD3 ) WRITE(6,105) NDLCL
C
C  Take a step along path of delta
C
         DLCLN = DLCLN + DLSTP
         IF ( LDBD3 ) WRITE(6,111) DLCLN
C
C  If the step will overshoot the cylinder length, rescale the cluster
C  size so that this last cluster will take up the remainder of the
C  energy of the delta.
C
         IF ( DLCLN .GT. DLZLN) THEN
            DLCLE = DLCLE*((DLZLN-DLCLN)/DLSTP+1.)
            DLCLN = DLZLN
         ENDIF
C
C  Get cluster size.  This is given by the total number
C  of electrons produced (computed from the work function)
C
         CLUSIZ = (DLCLE - CKNMIN)/WRKFUN
C
C  Find the number of electrons in the cluster.  If this primary has
C  lost so much energy that it can no longer form secondaries, consider
C  the primary as the last cluster.  Otherwise, compute the size of the
C  cluster from the Fano formula.
C
         IF ( CLUSIZ .LE. 0.) THEN
            N1 = NDEL
C
         ELSE
C
            CALL RANNOR(PN,DUMMY)
C
            PN = PN*SQRT(CFANO*CLUSIZ)
            N1 = NDEL * MAX( INT(CLUSIZ + PN + .5), 0 )
C
         ENDIF
C
           IF ( LDBD3 ) WRITE(6,106) N1
           IF ( LDBD1 ) CALL HF1(IHDEDX+6,FLOAT(N1),1.)
C
C  Get the cluster position.  In the X-Y plane, we generate randomly a
C  point on a circle of radius DLRAD and using this point as a
C  displacement from the position of the primary's formation.  In the
C  Z-direction, we take the appropriate step.
C
         PHIDC = TWOPI*RNDM(Q)
         XY(1) = XX(1)
     *                 + DLRAD*COS(PHIDC)
         XY(2) = XX(2)
     *                 + DLRAD*SIN(PHIDC)
C
         XY(3) = XX(3) + DLCLN*SIGN(1.,PRMVC(3))
C
         IF ( LDBD3 ) WRITE(6,113) XY
C
C  Check that we're still in the TPC
C
         IF ( ABS(XY(3)) .GT. ZTPCMX ) GOTO 22
C
C  Get the time of formation of the cluster
C
         DTOCL = TOTDIS + ABS( DLCLN/PRMVC(3) )
         CLTIM = TOF + TOTDIS/(CLGHT*DLBET) + ABS(DLCLN/PRMVC(3))
         IF ( LDBD3 ) WRITE(6,107) CLTIM
C
C  Transport the electrons from this cluster to the endplate.
C  IWIR = 0  means the electron did not reach a sense wire
C
            IF(N1.EQ.0) GOTO 60
            CALL TSTRAN(XY(1),XY(2),XY(3),
     *                  N1,ITYPE,IWIR,WIRRAD,WIRPHI,TDRFT)
            IF ( IWIR .EQ. 0 ) GOTO 60
C
C  Form the avalanche at the wire for each arriving electron.
C  IRETA = 1 means that the avalanche is in an illegal time bin
C
             AVTIM = CLTIM + TDRFT
             CALL TSAVAL(N1,AVTIM,IRETA)
             IF ( IRETA .EQ. 1 ) THEN
                GOTO 60
             ENDIF
C
C  If the long pads and/or the trigger pads are 'on', couple each
C  avalanche to the appropriate pads.
C
             IF ( LTPDIG )
     &               CALL TSPCPL(ISECT,ITYPE,WIRRAD,WIRPHI,NLPHIT)
             IF ( LTTDIG ) CALL TSTCPL(ITYPE,WIRRAD,WIRPHI,NTPHIT)
C
C  Add the signal to the ones that already exist
C
             CALL TPSGNL(IWIR,NLPHIT,NTPHIT,IRETS)
             IF(IRETS.EQ.1) RETURN
C
C  Get next cluster for this delta ray
C
  60       CONTINUE
           IF ( DLCLN .NE. DLZLN ) GOTO 21
C
C  If we get here, we've exceeded the cylinder length; i.e., we're done
C  with this delta.
C
  22       IF ( LDBD3 ) WRITE(6,114) DLCLN,NDLCL,DLZLN,
     *                   DLRAD,XY
C
      ENDIF
 103  FORMAT(/, ' PRIMARY P , E  (Mev)       : ',2(E11.5,2X),
     *       /, ' MOMENTUM VECTOR : ',3(2X,F8.4))
 105  FORMAT(/, ' CLUSTER NUMBER             : ',I6)
 106  FORMAT(/, ' CLUSTER SIZE               : ',I6/
     .       /, ' TYPE OF SEGMENT            : ',I6)
 107  FORMAT(/, ' SEGMENT CREATION TIME      : ',F8.5,
     .       /, ' SEGMENT LENGTH (CM)        : ',F8.3)
 108  FORMAT(/, ' COORDS OF PRIMARY  (cm)    : ',3(F10.4,2X),
     *       /, ' DIRECTION OF TRACK         : ',3(F10.4,2X))
 109  FORMAT(/, ' RADIUS FOR DELTA DIST (cm) : ',F7.4,
     *       /, ' LENGTH FOR DELTA DIST (cm) : ',F8.4,
     *       /, ' DL/DZ FOR DELTA            : ',F8.3,
     *       /, ' AVERAGE GAMMA OF DELTA     : ',F8.3)
 110  FORMAT(/, ' ENERGY LOST/CLUSTER  (Mev) : ',F8.6)
 111  FORMAT(/, ' Z LENGTH OF DELTA    (CM)  : ',F8.4)
 112  FORMAT(/, ' CLUSTER CIRCLE COORDS      : ',2(F9.4,2X))
 113  FORMAT(/, ' CLUSTER COORDINATES        : ',3(F10.4,2X))
 114  FORMAT(//,'            FINAL NUMBERS FOR DELTA-RAY',
     *       //,' TOTAL LENGTH   (cm)        : ',F8.3,
     *       /, ' NUMBER OF CLUSTERS         : ',I6,
     *       /, ' CYLINDER LENGTH (cm)       : ',F8.3,
     *       /, ' CYLINDER RADIUS (cm)       : ',F8.4,
     *       /, ' LAST CLUSTER COORDS        : ',3(F10.4,2X))
 150  FORMAT(//, '               START  DELTA-RAY',/)
      RETURN
      END
