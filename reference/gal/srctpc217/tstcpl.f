      SUBROUTINE TSTCPL(ITYPE,RAD,PHI,NTHIT)
C-----------------------------------------------------------------------
C!  Couple the current avalanche to the trigger pads.
C
C  Called from:  TSTEER
C  Calls:        TSTCPL
C
C  Inputs:   PASSED:      --ITYPE, sector type
C                         --RAD, radius at avalanche location
C                         --PHI, phi at avalanche location
C            /TPGEOT/     --padrow geometry
C
C  Outputs:  PASSED:      --NTHIT,  the number of trigger pads affected
C                                   by the current avalanche
C            /CHANNL/     --NAMT,   pad number for each pad hit
C                         --MTPUL,  the number of electrons induced on
C                                   the corresponding pad in NAMP
C  A. Caldwell
C-----------------------------------------------------------------------
C
C  CHANNL carries the analog signals on all channels due to one
C  wire avalanche
C
C  Wire avalanche information
C  NAVELE     -- The charge from an avalanche
C  IBIN1  -- The first bin to get charge
C
      COMMON / AVALAN / NAVELE,IBIN1
C
C  Long pad coupling information
C  MPPUL   -- The array containing the coupled avalanches
C  NAMP   -- Pad 'name' generating this pulse
C            = (pad row-1)*150 + pad number
C
      PARAMETER (MXPHIT=15)
      COMMON / PADPUL / MPPUL(MXPHIT),NAMP(MXPHIT)
C
C  Trigger pad coupling information
C  MTPUL   -- The array containing the coupled avalanches
C  NAMT   -- Pad 'name' generating this pulse
C            = (tpad row# - 1)*(max # tpads/row) + tpad number
C
      PARAMETER (MXTHIT=2)
      COMMON / TRGPUL / MTPUL(MXTHIT),NAMT(MXTHIT)
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,
     +           LMXPDR=150,LTTSRW=11)
C
      COMMON /TPGEOT/ NTPTRW(LTSTYP),NTPTPR(LTTSRW,LTSTYP),
     &                TPTRBG(LTSTYP),TPTRST(LTSTYP),TPTRHG(LTSTYP),
     &                TPTPHC(LTTPAD,LTTSRW,LTSTYP),
     &                TPTPHW(LTTPAD,LTTSRW,LTSTYP),
     &                ITPADG(LTTPAD,LTTSRW,LTSTYP)
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
      COMMON / HISCOM / IHDEDX,IHTRAN,IHAVAL,IHCOUP,IHTRCP,IHBOS,IHTOT
C
      LOGICAL LDBR1,LDBR2,LDBR3
C
C  Debug levels
C
      ICALLS = ICALLS + 1
      LDBR1 = ( NTPCDR .GE. 1 .AND. ICALLS .LE. NCALDR )
      LDBR3 = ( NTPCDR .GE. 2 .AND. ICALLS .LE. NCALDR )
C
C  Find the closest pad row and check that this corresponds to a real
C  t-pad row in this sector
C
      NROW = INT((RAD-TPTRBG(ITYPE)+.5*TPTRST(ITYPE))/TPTRST(ITYPE)) + 1
C
      IF ( ITYPE .EQ. 1 ) THEN
         IF ( NROW .LT. 1 .OR. NROW .GT. 8 ) RETURN
      ELSE
         IF ( NROW .LT. 1 .OR. NROW .GT. 11 ) RETURN
      ENDIF
C
C  T-pad number:  look for the smallest difference between the phi at
C  the wire hit at the phi at the t-pad centers
C
      NTPOS = NTPTPR(NROW,ITYPE)
C
      DO 1 J = 1, NTPOS
C
         DPHI = ABS( TPTPHC(J,NROW,ITYPE) - PHI )
C
         IF ( J .EQ. 1 .OR. DPHI .LT. DPHI0 ) THEN
            DPHI0 = DPHI
            PAD0 = J
         ENDIF
C
 1    CONTINUE
C
C  Look one trigger pad on either side; count the number of affected
C  pads as we go.
C
      NTHIT = 0
C
      DO 2 JTPAD = -1,1
C
         MTPAD = PAD0 + JTPAD
         IF ( MTPAD .LT. 1 .OR. MTPAD .GT. NTPOS ) GOTO 2
C
C  Get radius,phi-coord at center, and phi-width of this t-pad
C
         TRAD = TPTRBG(ITYPE) + (NROW-1)*TPTRST(ITYPE)
         TPHIC = TPTPHC(MTPAD,NROW,ITYPE)
         TPHIW = TPTPHW(MTPAD,NROW,ITYPE)
C
C  Get the coupling for this pad
C
          ST = TPTCST(TRAD,RAD,TPHIC,PHI,TPHIW,TPRDWD)
C
C  If the coupling is significant, update the number of t-pads hit
C  and the arrays containing the t-pad 'names' and pulse heights.
C  name = row # * 4 t-pads/row + pad #
C
          IF ( ST .GT. 0. ) THEN
             NTHIT = NTHIT + 1
             NAMT(NTHIT) = (NROW-1)*4 + MTPAD
             MTPUL(NTHIT) = INT(ST*FLOAT(NAVELE))
          ENDIF
C
 2     CONTINUE
C
C  Debug stuff
C
      IF ( LDBR1 ) CALL HF1(IHTRCP+1,FLOAT(NTHIT),1.)
      IF ( LDBR3 ) THEN
          WRITE(6,102) NTHIT
          WRITE (6,103) (J,NAMT(J),MTPUL(J), J=1,NTHIT)
      ENDIF
C_______________________________________________________________________
C
 102  FORMAT(//,10X,'  FINAL NUMBERS FROM T-PAD COUPLING ',
     *        /,' T-PADS HIT  :',I2,/)
 103  FORMAT( ' HIT ',I2,' BIN ',I4,' CHARGE : ',I12)
C
      RETURN
      END
