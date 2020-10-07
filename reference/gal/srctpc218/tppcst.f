      FUNCTION TPPCST(R1,R2,P1,P2)
C-----------------------------------------------------------------------
C
C!  Return the relative coupling strength to a pad at (R1,P1) from
C!  an avalanche at (R2,P2). The coupling is calculated via the formula:
C                     2      2            2       2
C      P(x) = A exp[-w /2 sig ] exp[-(h-c) / 2 sig ]    if h > c
C                            w                    h
C  else
C                     2      2
C      P(x) = A exp[-w /2 sig ]                         if h <= c
C                            w
C
C  where w is the coordinate along the pad width and h along the
C  pad height, A is the coupling for a wire hit directly above the
C  center of the pad, and sig is an experimentally determined
C  exponential dropoff width.
C
C  Called from:  TSPCPL
C  Calls:        None
C
C  Inputs:   PASSED:      --R1,     radius at the center of the pad
C                         --P1,     phi-coord at the center of the pad
C                         --R2,     radius at the avalanche
C                         --P2,     phi-coord at the avalanche
C           /TPCOND/      --HAXCUT, c above
C                         --EFFCP,  A above
C                         --SIGW, as above
C                         --SIGH, as above
C
C  Outputs:  TPCPST, the function value
C
C  A. Caldwell 25-10-84
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
C
      DATA ICALLS/0/
C
      IF ( ICALLS .EQ. 0 ) THEN
C
C  Calculate WCUT and HCUT, which serve as cutoffs for the
C  distance beyond which we will not consider the effect of an
C  avalanche on a pad
C
         WDEN = 2.*SIGW*SIGW
         HDEN = 2.*SIGH*SIGH
C
         IF ( CUTOFF .LT. EFFCP ) THEN
            WCUT = SQRT(-1.*ALOG(CUTOFF/EFFCP)*WDEN)
            HCUT = SQRT(-1.*ALOG(CUTOFF/EFFCP)*HDEN)
         ELSE
            WCUT = 0.
            HCUT = 0.
         ENDIF
C
         ICALLS = 1
C
      ENDIF
C
C  Start calculating the coupling
C
      TPPCST = 0.
C
      SIND = SIN(P2-P1)
      COSD = COS(P2-P1)
C
C  Close enough in w?
C
      WDIFF = R2*SIND
      IF ( WDIFF .GT. WCUT ) RETURN
C
C  Close enough in h?
C
      HDIFF = ABS(R1 - R2*COSD) - HAXCUT
      IF ( HDIFF .GT. HCUT ) RETURN
C
C  Apply the formula
C
      IF ( HDIFF .GT. 0. ) THEN
C
         TPPCST = EFFCP * EXP( -1.*WDIFF*WDIFF / WDEN ) *
     *                    EXP( -1.*HDIFF*HDIFF / HDEN )
C
C  Watch for insignificant coupling strengths
C
         IF ( TPPCST .LT. CUTOFF ) TPPCST = 0.
C
      ELSE
C
         TPPCST = EFFCP * EXP( -1.*WDIFF*WDIFF / WDEN )
C
      ENDIF
C
      RETURN
      END
