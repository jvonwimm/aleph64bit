      SUBROUTINE TPDPAR(PRIMP,PRMVC,DLRAD,DLZLN,DLBET)
C-----------------------------------------------------------------------
C!  Calculate parameters to be used in making clusters for delta-rays
C
C  Called from:  TSDEDX
C
C  Inputs:   PASSED:      --PRIMP,  the scalar momentum of the delta at
C                                   time of creation
C                         --PRMVC,  the normalized momentum vector of
C                                   the delta at time of creation
C            /TPCONS/     --RADFAC, a scaling factor to account for
C                                   multiple scattering by increasing
C                                   the radius of the circle which would
C                                   define the X-Y projection of the
C                                   delta's path if it didn't interact
C                         --CYLFAC, a similar factor to decrease the pat
C                                   length due to multiple scattering
C  A. Caldwell, D. DeMille
C  M. Mermikides                    In case of no magnet field we just
C                                   take a fixed large radius of
C                                   curvature for the delta electron
C  Modifications:
C     D. Cowen 1-Nov-88             Account for possibility that beta=0
C                                   due to precision problems.
C-----------------------------------------------------------------------
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
      DIMENSION PRMVC(3)
C
C  Get the nominal radius of curvature
C
      IF (ABS(CFIELD).LT.0.001) THEN
         DLRAD = 100.
      ELSE
         DLRAD = PRIMP*SQRT(PRMVC(1)**2 + PRMVC(2)**2)/ABS(BCFMEV)
      ENDIF
C
C  Multiply by the 'safety' factor from TPCOND.INC
C
      DLRAD = DLRAD*RADFAC
C
C  Determine the total path length from the momentum
C
      DLM3 = PRIMP**3
      DLTLN = 100.*DLM3
      GAMMA = 1. + .65*DLM3
      DLBET= SQRT( GAMMA*GAMMA - 1.) / GAMMA
      IF (DLBET .LE. 0.0) DLBET = 1.0E-6
C
C  Get the corresponding length in z
C
      DLZLN = ABS( DLTLN * PRMVC(3) )
C
C  Multiply by reduction factor to somehow account for multiple
C  scattering.  CYLFAC is in /TPCONS/
C
      DLZLN = DLZLN/CYLFAC
C
      RETURN
      END
