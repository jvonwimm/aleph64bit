      FUNCTION TPTCST(R1,R2,P1,P2,PWD,RWD)
C-----------------------------------------------------------------------
C!  Returns the relative coupling strength on a trigger pad centered at
C!  (R1,P1) due to an avalanche at (R2,P2)
C  The coupling is accomplished via the formula:
C
C                  2              2              2              2
C  P = A * exp( - fr(R2-R1)/(2*sigr) ) * exp( - fs(S2-S1)/(2*sigs) )
C        fr(R2-R1) and fs(S2-S1) can be defined as fx(DX):
C        fx(DX) = |  0,           |DX| <= Cx
C                 |  |DX| - Cx,   |DX| >  Cx
C        sigr and sigs can be defined as sigx :
C        sigx is an experimentally determined exponential dropoff width.
C
C  R corresponds to radius; S corresponds to arc length along a circle
C  at the pad radius.
C  A is the coupling for a wire hit directly above the center of
C  the pad.
C  Cx  =  half-length of pad in x direction - sigx :
C   pad edge                                                  pad center
C  *-------------------------------------------------------------------*
C  <--sigx--><-------------------------Cx------------------------------>
C
C  Called from:  TSTCPL
C  Calls:        None
C
C  Inputs:   PASSED:      --R1,     radius at the center of the t-pad
C                         --P1,     phi at the center of the t-pad
C                         --R2,     radius at the avlanche
C                         --P2,     phi at the avalanche
C                         --PWD,    phi-width of t-pad in question
C                         --RWD,    radial width of t-pad in question
C           /TPCOND/      --SIGR,   as above
C                         --SIGARC, as above
C                         --TREFCP, A above
C
C  Outputs:  TPTCST, function value
C
C  A. Caldwell 25-10-84; D. DeMille 6-11-85
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
      DATA ICALLS/0/
C
      IF ( ICALLS .EQ. 0 ) THEN
C
C  Calculate DRMX and DSMX, which will serve as cutoffs for the
C  distance beyond which we will not consider the effect of an
C  avalanche on a trigger-pad
C
         RDEN = 2.*SIGR*SIGR
         ADEN = 2.*SIGARC*SIGARC
C
         IF ( CUTOFF .LT. TREFCP ) THEN
            DRMX = SQRT(-1.*ALOG(TCSCUT/TREFCP)*RDEN)
            DSMX = SQRT(-1.*ALOG(TCSCUT/TREFCP)*ADEN)
         ELSE
            DRMX = 0.
            DSMX = 0.
         ENDIF
C
         ICALLS = 1
C
      ENDIF
C
C  Differences in radial and arc-length coords between center of t-pad
C  and avalanche location
C
      DR = ABS(R2-R1)
      DS = R1*ABS(P2-P1)
C
C  Calculate the c's as described above
C
      CR = RWD - SIGR
      CS = R1*PWD - SIGARC
C
C  Calculate the f's as described above
C
      IF ( DR .LE. CR ) THEN
         FR = 0
      ELSE
         FR = DR - CR
      ENDIF
C
      IF ( DS .LE. CS ) THEN
         FS = 0
      ELSE
         FS = DS - CS
      ENDIF
C
C  See if we're beyond the cutoff distance in either direction
C
      IF ( FR .GT. DRMX .OR. FS .GT. DSMX ) THEN
         TPTCST = 0.
      ELSE
         TPTCST = TREFCP * EXP( -1*FR*FR/RDEN ) * EXP( -1*FS*FS/ADEN )
         IF ( TPTCST .LT. TCSCUT ) TPTCST = 0.
      ENDIF
C
      RETURN
      END
