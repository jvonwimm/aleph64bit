      SUBROUTINE T2DEDX(IRET)
C
C! Fast sim : Routine to return the ionization charge produced
C! along a super-broken segment and this super broken-segment
C! coordinates.
C
C  Called from: T2TEER
C  Calls:       WBANK, TPELSG, TPSEGT, TPDRAY, TPSIZE
C
C  Inputs:   TRAKEL.INC:  --Primary track parameters for this track elt.
C            TPCONS.INC:  --POIMAX, the cutoff for treating single
C                                   primaries
C                         --CKNMIN, Ar ionization energy
C                         --WRKFUN, Ar work function
C                         --CFANO,  Fano factor for finding cluster
C                                   size variation
C                         --DELCLU, step size for delta simulation
C            TPCGEO.INC:  --ZTPCMX, the position of the TPC endplate
C
C  Outputs:  PASSED:      --NSEGT,  number of segments formed
C                         --IRET,   return code indicating whether
C                                   track was processed
C            BANKS:       --id IDCLUS, segment bank
C                           IW(IDCLUS+1) = num words per segment
C                           IW(IDCLUS+2) = num of segments
C                         --KIDCL = IDCLUS + 2 + (KSEGT-1)*IW(IDCLUS+1)
C                           RW(KIDCL+1) = X of segment extremity
C                           RW(KIDCL+2) = Y of segment extremity
C                           RW(KIDCL+3) = Z of segment extremity
C                           RW(KIDCL+4) = time of segment extremity
C                           RW(KIDCL+5) = direction
C                           RW(KIDCL+6) =       cosines
C                           RW(KIDCL+7) =            of segment
C                           RW(KIDCL+8) = length of segment
C                           IW(KIDCL+9) = num of electrons in segment
C                           IW(KIDCL+10)= type of segment
C   P. Janot  15/10/87
C ----------------------------------------------------------------------
C
C Modifications :
C         1. P. Janot   23 Mar 1988
C            Also process charged particles with |charge| > 1.
C         2. P. Janot   06 Aug 1988
C            Avoid problems if a track element begins outside TPC
C         3. D. Cowen   06 Sep 1988
C            Use R. Johnson's dEdx function TBLOCH instead of TPAVIP.
C         4. D. Cowen    22Feb 1989
C            Add error return for WBANK calls.
C ----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
C  TRAKEL:  track parameters for dE/dX and carrying around broken
C  tracks
C
      COMMON/TRAKEL/NTRK,X(3),VECT(3),ABSMOM,SEGLEN,TOF,AMASS,CHARGE,
     *              RAD,CENT(2),DELPSI,PSI1,ALPH01,ALPH02
C - MXBRK = 2* MAX(NLINES(1..3)) + 2 , NLINES= 8,10,10 in /SCTBND/
      PARAMETER (MXBRK=22, MXBRTE=MXBRK/2)
      COMMON/BRKNTK/XB(3,6),VECTB(3,6),SEGLNB(6),TOFB(6)
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
      COMMON / HISCOM / IHDEDX,IHTRAN,IHAVAL,IHCOUP,IHTRCP,IHBOS,IHTOT
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
C  FASTER : variables used in fast simulation
C
      COMMON / LANDAU / NITLAN,NITIND,INTWRD
      COMMON / EXBEFF / XPROP,TTTT,XXXX(1001),XSHFT(50)
      COMMON / EVT / IEVNT,ISECT
      COMMON / T3TR / JSEGT,NSEGT,ITYPE,WIRRAD(4),WIRPHI(4)
     &               ,AVTIM(4),NELE(4),NCL,SIGT(4)
      COMMON / TPSG / CC(3),XX(3),TOTDIS,XLEN,ISTY,IE
      COMMON / XBIN / IBIN(4),NAVBIN(4),NB
      DIMENSION XEX(4,4)
      PARAMETER (SQ2=1.4142136,SQ3=1.7320508)
C
      LOGICAL LDBD1,LDBD2,LDBD3,LDBDF
C
      DATA IPAR,ICALL,IPRCL/1,0,0/
      DATA MXSGDF,MXSGEX,NWPRSG/500,100,10/
C
C
C  Set the return flag  IRET = 1 if there is not enough space to extend
C                               signal bank (given by TPDRAY)
C
      IRET = 0
C
C  Debug levels
C
      ICALL = ICALL + 1
      LDBD1 = ( NTPCDD .GE. 1 .AND. ICALL .LE. NCALDD )
      LDBD2 = ( NTPCDD .GE. 2 .AND. ICALL .LE. NCALDD )
      LDBD3 = ( NTPCDD .GE. 3 .AND. ICALL .LE. NCALDD )

      IF ( LDBD2 ) WRITE(6,100) (X(I),VECT(I),I=1,3),SEGLEN,TOF,
     *                            ABSMOM,AMASS,CHARGE,NTRK
C
C  Open the BOS work bank which will contain the segment information.
C  Number of words = 2 + no. words/segment * default no. of segments.
C
      MXSEGT = MXSGDF
      NDWRDS = 2 + NWPRSG*MXSEGT
      CALL WBANK(IW,IDCLUS,NDWRDS,*999)
C
C-----------------------------------------------------------------------
C
C  First get the parameters to do dedx for this track ( bubble 4.3.1.1 )
C
C-----------------------------------------------------------------------
C
C  Get gamma and beta of the track.
C
      GAMMA = SQRT(1. + ABSMOM*ABSMOM/(AMASS*AMASS))
      BETA  = SQRT( GAMMA*GAMMA - 1. ) / GAMMA
      ZBET = (CHARGE**2/BETA**2)
      IF(ZBET.GT.100.)ZBET = 100.
C
C  Get Poisson average for number of primary ionizing interactions per
C  unit length
C
      BG     = BETA * GAMMA
      POISAV = TBLOCH(BG) * CHARGE**2
C
      IF ( LDBD3 ) WRITE(6,101) POISAV
      IF ( LDBD1 ) CALL HF2(IHDEDX+1,ALOG10(GAMMA),POISAV,1.)
C
C-----------------------------------------------------------------------
C
C  Generate primary interactions ( bubble 4.3.1.2 )
C
C-----------------------------------------------------------------------
C
C  TOTDIS keeps track of how far we've gone from the start point.
C  NSEGT keeps track of the segment number.
C
      TOTDIS = 0.
      XLEN  = 0.
      NSEGT = 0
      LDBDF = LDBD1
C
C  If the track element begins outside the TPC (|z| > Zmax),
C  skip this part of track element since we reach an endplate
C
      IF ( ABS(X(3)) .GT. ZTPCMX) THEN
           IE = 3
           CALL TPSEGT
      ENDIF
C
  1   IPRCL=IPRCL+1
      LDBD1 = ( LDBD1 .AND. IPRCL .LE. NCALDD )
      LDBD3 = ( LDBD3 .AND. IPRCL .LE. NCALDD )
C
C  If we've exhausted the allowed length, fill in the cluster bank
C  header, close it off, and quit
C
  2   IF ( TOTDIS .GE. SEGLEN ) THEN
C
           IF ( LDBD2 ) WRITE(6,115) (TOTDIS-XLEN),XLEN,SEGLEN,NSEGT,
     *        (XX(K),K=1,3),(CC(K),K=1,3)
           IF ( LDBDF )
     *        CALL HF2(IHDEDX+5,ALOG10(ABSMOM),FLOAT(NCLUST)/SEGLEN,1.)

C

           IW(IDCLUS+1) = NWPRSG
           IW(IDCLUS+2) = NSEGT
C
           NDWRDS = 2 + NWPRSG*NSEGT
           CALL WBANK(IW,IDCLUS,NDWRDS,*999)
C
C
           RETURN
C
      ENDIF
C
C  Get the position XX of the segment extremity, the direction
C  cosines CC of the track element at that position, together
C  with the length since the last segment extremity.
C
           NSEGT = NSEGT + 1
           IF ( LDBD3 ) WRITE(6,105) NSEGT
           IE  = 0
           DISLOC = TOTDIS
           CALL TPSEGT
           IF ( LDBD3 ) WRITE(6,108) (XX(K),K=1,3),(CC(K),K=1,3)
C
C  Check to see that we're still in the TPC
C
           IF ( ABS(XX(3)) .GE. ZTPCMX ) THEN
              IE = 2
              CALL TPSEGT
              XLEN = TOTDIS - DISLOC
              GOTO 5
           ENDIF
C
C  Check to see if we need to extend BOS bank
C
           IF ( NSEGT .GT. MXSEGT ) THEN
              MXSEGT = MXSEGT + MXSGEX
              NDWRDS = 2 + NWPRSG*MXSEGT
              CALL WBANK(IW,IDCLUS,NDWRDS,*999)
           ENDIF
C
  5        CONTINUE
           INDEX = IDCLUS + 2 + (NSEGT-1)*10
           IF(IE.EQ.2) GOTO 555
C
C  Fill the bos bank :
C
           IF(TOTDIS.GT.SEGLEN) THEN
             XLEN   = XLEN -(TOTDIS-SEGLEN)
      TOTDIS = SEGLEN
      IF( ABS(XLEN) .LT. 1.E-5) GOTO 555
             IE  = 1
             CALL TPSEGT
           ENDIF
C
C  Get the segment position.
C
  555      CONTINUE
           RW(INDEX+1) = XX(1)
           RW(INDEX+2) = XX(2)
           RW(INDEX+3) = XX(3)
C
C  Get the segment direction cosines
C
           RW(INDEX+5) = CC(1)
           RW(INDEX+6) = CC(2)
           RW(INDEX+7) = CC(3)
C
C  Get the time and the length for this segment.
C
           RW(INDEX+4) = TOF + TOTDIS/(CLGHT*BETA)
           RW(INDEX+8) = XLEN
           IF ( LDBD3 ) WRITE(6,107) RW(INDEX+4),RW(INDEX+8)
C
C  Get number of primary clusters in segment
C
           CALL TPELSG(POISAV,XLEN,NCLUST)
C
C  Treatment of Delta-rays
C
           CALL TPDRAY(POISAV,ZBET,NCLUST,TOTDIS,NSEGT,ISECT,ITYPE,
     1                                                NEL,IRET)
           IF(IRET .EQ. 1) RETURN
           NCLUST = NEL
            INDEX = IDCLUS + 2 + (NSEGT-1)*10
C
C  Get number of electrons in segment
C
           IF (NEL.NE.0) CALL TPSIZE(NEL)
           IW(INDEX+9) = NEL
           IW(INDEX+10)= ISTY
C
           IF ( LDBD1 ) CALL HF1(IHDEDX+6,FLOAT(IW(INDEX+9)),1.)
           IF ( LDBD3 ) WRITE(6,106) IW(INDEX+9),IW(INDEX+10)
C
C  Go and generate the next segment.
C
      IF(IE .GE. 1) THEN
             TOTDIS = SEGLEN + .001
             GOTO 2
           ENDIF
C
C  Go and generate the next primary.
C
      GOTO 1
C
  999 IRETD = 2
      WRITE(6,'(/'' +++T2DEDX+++ Insufficient BOS array space!''/)')
      RETURN
C-----------------------------------------------------------------------
 100  FORMAT(//,'              START TRACK ELEMENT',
     *       //,' DEDX STARTS WITH   X(I) ,  COSX(I) ',3(/,18X,2(F9.3)),
     *       /, ' TRACK ELEMENT LENGTH (cm)  : ',F8.3,
     *       /, ' TIME OF FLIGHT   (nanosec) : ',E12.6,
     *       /, ' MOMENTUM     (Mev)         : ',F9.3,
     *       /, ' MASS (Mev), CHARGE , TRACK : ',F8.3,2X,F4.1,2X,I4)
 101  FORMAT(/, ' POISSON MEAN               : ',F8.3)
 102  FORMAT(/, ' DISTANCE TO PRIMARY (cm)   : ',F8.5,
     *       /, ' TOTAL DISTANCE  (cm)       : ',F8.4)
 103  FORMAT(/, ' PRIMARY P , E  (Mev)       : ',2(E11.5,2X),
     *       /, ' MOMENTUM VECTOR : ',3(2X,F8.4))
 105  FORMAT(/, ' CLUSTER NUMBER             : ',I6)
 106  FORMAT(/, ' CLUSTER SIZE               : ',I6/
     *       /, ' TYPE OF SEGMENT            : ',I6)
 107  FORMAT(/, ' SEGMENT CREATION TIME      : ',F8.5,
     *       /, ' SEGMENT LENGTH (CM)        : ',F8.3)
 108  FORMAT(/, ' COORDS OF PRIMARY  (cm)    : ',3(F10.4,2X),
     *       /, ' DIRECTION OF TRACK         : ',3(F10.4,2X))
 115  FORMAT(//,'            FINAL NUMBERS FOR TRACK ELEMENT',
     *       //,' TOTAL LENGTH (CM)          : ',F9.4,
     *       /, ' LAST SEGMENT LENGTH (CM)   : ',F9.4,
     *       /, ' ELEMENT LENGTH   (cm)      : ',F9.4,
     *       /, ' NUMBER OF SEGMENTS         : ',I6,
     *       /, ' LAST PRIMARY COORDS        : ',3(F10.4,2X),
     *       /, ' DIR COSINES AT END         : ',3(F10.4,2X))
C_______________________________________________________________________
C
      END
