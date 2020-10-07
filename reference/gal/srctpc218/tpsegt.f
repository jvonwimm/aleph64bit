      SUBROUTINE TPSEGT
C
C! Fast sim : Routine to cut a track element in super-broken segments
C  corresponding to a given sense wire. The routine determines also
C  the length and the direction cosines of the segment together with
C  the time of the segment extremities.
C
C  Units -- MeV,nanoseconds,centimeters,Tesla
C
C  Calls :   None
C
C  Input :  FASTER :  --NSEGT  num of segment extremity
C                     --TOTDIS,path length at (k-1)th segment
C                              (= 0. if k=1 !)
C                     --IE,    1 if TOTDIS greater than the
C                                total track element length
C                              2 if we reached an endplate
C                              3 if |X(3)| > ZTPCMX
C                              0 otherwise
C                     --ITYPE  sector type
C
C  Output:  FASTER :  --TOTDIS,path length at kth segment
C                     --XLEN,  segment length
C                     --XX,    coords of segment extremity
C                     --CC,    direction cosines of segment
C                     --ISTY,  0 if middle of segment
C                              1 if end    of segment
C
C  P. Janot.  11/15/87
C
C  Now parameterize according to arclength l
C
C     X(l) = X0 + R * [ sin(l/R*sin(Theta0) - Phi0) + sin(Phi0) ]
C     Y(l) = Y0 + R * [ cos(l/R*sin(Theta0) - Phi0) - cos(Phi0) ]
C     Z(l) = Z0 + l * [          cos(Theta0)                    ]
C
C  where X0,Y0,Z0 are the starting X,Y,Z, Phi0 and Theta0 are
C  the starting Phi and Theta for l = 0.
C
C  Note the radius is postive or negative depending on the charge.
C
C ----------------------------------------------------------------------
C  Modifications :
C         1. P. Janot   23 Mar 1988
C            Avoid troubles when R gets very large. Use series
C            expansion
C
C              R*COS(X) = R*( 1 - (X**2)/2 + (X**4)/24 ...)
C              R*SIN(X) = R*( X - (X**3)/6 +           ...)
C
C            so that if R*(X**3)/6 < eps, then we've accuracy eps.
C            Here, X = l/R*sin(Theta0) and if the condition is
C            satisfied, we take the first two terms for COS and
C            the first term for SIN.
C         2. P. Janot   09 Aug 1988
C            Initialize YNEW when IE > 0 to avoid crazy super
C            broken segments.
C         3. P. Janot   09 Aug 1988
C            Avoid problems when track element begins outside
C            TPC . Correct new path length determination .
C ----------------------------------------------------------------------
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
      COMMON/BRKNTK/XB(3,6),VECTB(3,6),SEGLNB(6),TOFB(6)
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
      DOUBLE PRECISION RAY,PHI0,CPHI0,SPHI0,SINTH,COSTH,ARG1,ARG2
     *                ,YPRE,YNEW,PSINEW,CPSINW,DISPRE,PSIPRE,WSTEP
     *                ,REPS
      DATA ITAG/0/,IREC/0/
      DISPRE = TOTDIS
C
C  If no field, take linear step
C
      IF (ABS(CFIELD).LE.0.0001) THEN
        IF(TOTDIS.EQ.0.) THEN
C
C   Initialization (first broken segment of THIS track)
C   Determine Y coordinate at 1st segment
C
          Y1    = TWIRE1(ITYPE)
          WSTEP = TWSTEP(ITYPE)
          YPRE  = X(2)
          NWIR  = (YPRE-Y1)*2./WSTEP
          IF(YPRE.LT.Y1) NWIR = NWIR-1
          YNEW  = Y1 + (NWIR+1)*WSTEP/2.
          ISTY = IABS(MOD(NWIR,2))
        ELSE
C
C   Determine new Y coordinate at kth segment
C
          YPRE  = X(2) + VECT(2)*TOTDIS
          YNEW  = YPRE + WSTEP/2.
          NADJUS= (YNEW-Y1+WSTEP/10.)*2./WSTEP
          IF(YNEW.LT.(Y1-WSTEP/4.)) NADJUS = NADJUS-1
          YNEW  = Y1 + NADJUS*WSTEP/2.
          ISTY = IABS(MOD(NADJUS,2) -1)
        ENDIF
C
C   Special treatment if we've  exhausted the allowed length
C   or if we'e reached an endplate.
C
      IF(IE.EQ.1) GOTO 400
      IF(IE.GE.2) GOTO 399
C
C   Determine new path length at kth segment as a function
C   of YNEW
C
        TOTDIS  = (YNEW-X(2))/VECT(2)
        GOTO 400
C
C   Determine new path length at kth segment if we reached
C   a TPC endplate
C
399   TOTDIS = ABS( (ZTPCMX-ABS(X(3))) / VECT(3) )
C
C   Then calculate the new coordinates after kth segment
C   and the length of this segment.
C
 400    XX(1) = X(1) + TOTDIS*VECT(1)
        XX(2) = X(2) + TOTDIS*VECT(2)
        XX(3) = X(3) + TOTDIS*VECT(3)
        CC(1) = VECT(1)
        CC(2) = VECT(2)
        CC(3) = VECT(3)
        XLEN    = TOTDIS-DISPRE
        RETURN
      ENDIF
C
C  Now magnetic field non zero.
C
      IF(DISPRE.EQ.0.) THEN
C
C   Initialization (first broken segment of THIS track)
C
        NEPS  = 0
        COSTH = VECT(3)
        ARG1  = VECT(1)
        ARG2  = VECT(2)
        SINTH = DSQRT(1.-COSTH*COSTH)
        PHI0  = ATAN2(ARG2,ARG1)
        CPHI0 = DCOS(PHI0)
        SPHI0 = DSIN(PHI0)
        RAY   = ABSMOM*SINTH/(CHARGE*BCFMEV)
        ISIGN = DSIGN(1.D0,SPHI0)
        Y1    = TWIRE1(ITYPE)
        WSTEP = TWSTEP(ITYPE)
        YPRE  = X(2)
        NWIR  =  (YPRE-Y1)*2./WSTEP
        IF(YPRE.LT.Y1) NWIR = NWIR-1
        IF(IE .EQ. 3) THEN
           ITAG = 1
           IREC = 1
        ENDIF
      ENDIF
C
C   Let's call PSI = (l/R*sin(theta0)-phi0). PSIPRE is the value
C   of PSI at the (k-1)th broken segment. The aim is to determine
C   the PSI value at the kth segment (called PSINEW), defined by
C   cos(PSINEW) = cos(PSIPRE) + deltaY/R, where deltaY is half
C   the gap between two wires.
C
      PSIPRE= DISPRE/RAY*SINTH-PHI0
      CPSIPR= DCOS(PSIPRE)
C
C   Special treatment if we've  exhausted the allowed length
C   or if we'e reached an endplate.
C
      IF(IE.EQ.1) GOTO 200
      IF(IE.GE.2) GOTO 199
      IF(DISPRE.EQ.0.OR.ITAG.EQ.1) THEN
C
C   Determine Y coordinate at 1st segment in general case,
C   or at 2nd segment in the case ITAG=1.
C
        YNEW  = Y1 + (DBLE(FLOAT(NWIR))+DBLE(FLOAT(ISIGN+1))*.5)
     &          *WSTEP*.5
        ITAG=0
      ELSE
C
C   Determine Y coordinate at kth segment
C
        YPRE  = XX(2)
        YNEW  = YPRE + ISIGN*WSTEP/2.
        IREC = 0
      ENDIF
      NADJUS = (YNEW-Y1+WSTEP/10.)*2./WSTEP
      IF(YNEW.LT.(Y1-WSTEP/4.)) NADJUS = NADJUS-1
      YNEW  = Y1 + DBLE(FLOAT(NADJUS))*WSTEP/2.
      ISTY = IABS(MOD(NADJUS,2) -1)
C
C   And determine cos(PSINEW).
C
      CPSINW= CPSIPR + (YNEW-YPRE)/RAY
C
C Check if we want to use an approximation for large radius.
C
      REPS  =  (YNEW-X(2))/SPHI0*
     .         ( 1. + (YNEW-X(2))*CPHI0/(2.*RAY*SPHI0) )
      IF ( DABS(RAY) .GT. 1.D3 ) THEN
         TEST  =   REPS/( (RAY*RAY*6.)**(1./3.) )
         IF ( ( TEST.LT.4.64D-2 ) .AND. ( NEPS .EQ. 0 ) ) GOTO 600
      ENDIF
C
C If not, go into all the details of the calculation.
C
      IF(DABS(CPSINW).GT.1.) THEN
        NEPS = 1
C
C   Check for wrong value of cos(PSINEW) and correct. (Wrong
C   value means absolute value of the cosine greater than 1.)
C   Then determine the corresponding value of PSINEW.
C
        IF(DISPRE.EQ.0 .OR. IREC.EQ.1) ITAG = 1
        ISIGN = -ISIGN
        YNEW  = YPRE
        NP = 1
        NR = 1
        NS = 1
        IF(RAY.GT.0.) NR = 0
        IF(CPSINW.GT.1.) NP = 0
        IF((PSIPRE-NP*PI).LT.0.) NS = 0
        NPIPRE = (PSIPRE-NP*PI)/(2.*PI)
        NPIPRE = NPIPRE-NR +NS
        PSINEW = 4.*NPIPRE*PI-PSIPRE +2.*NP*PI
        CPSINW= CPSIPR
      ELSE
C
C   Calculate PSINEW value in most cases
C
        PSINEW = DACOS(CPSINW)
        NPI    = PSIPRE/PI
        IF(PSIPRE.LT.0.) NPI = NPI-1
        IF(MOD(NPI,2).EQ.0) THEN
           PSINEW = PSINEW + NPI*PI
        ELSE
           PSINEW =-PSINEW + (NPI+1)*PI
        ENDIF
      ENDIF
      SPSINW= DSQRT(1.D0-CPSINW*CPSINW)*DSIGN(1.D0,DSIN(PSINEW))
C
C   Determine new path length at kth segment as a function
C   of PSINEW
C
      TOTDIS  = RAY/SINTH*(PSINEW+PHI0)
      GOTO 300
C
C   Determine new path length at kth segment if we reached
C   a TPC endplate
C
199   TOTDIS = DABS( (ZTPCMX-ABS(X(3))) / COSTH )
C
C   Determine PSINEW either if we reached an endplate (IE=2) or if
C   TOTDIS is greater then the total track element length (IE=1)
C
 200  REPS   = TOTDIS*SINTH
C
C Check if we want to use an approximation for large radius.
C
      IF ( DABS(RAY) .GT. 1.D3 ) THEN
         TEST  =   REPS/( (RAY*RAY*6.)**(1./3.) )
         IF ( TEST.LT.4.64D-2 ) THEN
            YNEW = X(2) + REPS * ( SPHI0 - REPS*CPHI0/(2.*RAY) )
            GOTO 600
         ENDIF
      ENDIF
C
CC      TEST   = REPS**3/(RAY*RAY*6.)
CC      IF(TEST.LT.1.D-4 .AND.
CC     .   DABS(RAY) .GT. 1.D3) THEN
CC         YNEW = X(2) + REPS * ( SPHI0 - REPS*CPHI0/(2.*RAY) )
CC         GOTO 600
CC      ENDIF
      PSINEW = REPS/RAY -PHI0
      SPSINW = DSIN(PSINEW)
      CPSINW = DCOS(PSINEW)
      YNEW   = YPRE + RAY*(CPSINW-CPSIPR)
C
C   Then calculate the new coordinates after kth segment
C   and the length of this segment.
C
 300  XX(1) = X(1) + RAY*(SPSINW+SPHI0)
      XX(2) = YNEW
      XX(3) = X(3) + TOTDIS*COSTH
      CC(1) = SINTH*CPSINW
      CC(2) = SINTH*SPSINW
      CC(3) = COSTH
      IF(IE.NE.1) XLEN  = TOTDIS - DISPRE
      RETURN
C
C   Now use approximation for large radius
C
600   CONTINUE
      TOTDIS = REPS/SINTH
      IF(IE.NE.1) XLEN  = TOTDIS - DISPRE
      XX(1)  = X(1) + REPS * ( CPHI0 + REPS*SPHI0/(2.*RAY) )
      XX(2)  = YNEW
      XX(3)  = X(3) + TOTDIS*COSTH
      PSINEW = REPS/RAY - PHI0
      CC(1)  = SINTH* ( CPHI0 + REPS/RAY*SPHI0)
      CC(2)  = SINTH* (-SPHI0 + REPS/RAY*CPHI0)
      CC(3)  = COSTH
      RETURN
      END
