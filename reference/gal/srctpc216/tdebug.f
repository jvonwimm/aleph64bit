      SUBROUTINE TDEBUG (NEVT)
C----------------------------------------------------------------------
C
C    Created by Doug Cowen         9-AUG-1988
C
C    Purpose:
C!               Make output necessary for the TPCSIM stand-alone
C!               version to work on problematic events.  GALEPH does
C!               not reproduce random number sequence, but TPCSIM does
C!               so prior to each GALEPH call to TGTEER, we call this
C!               routine (if LDEBUG .EQ. .TRUE.) which outputs the
C!               starting random number seed, and the RUNH, EVEH and
C!               TPTE banks.  The banks are written out to a file
C!               which is overwritten for each event; the random number
C!               seed appears in the log file.
C!   Inputs    : LDEBUG flag, BOS banks RUNH, EVEH, TPTE.
C                NEVT / I  = event number
C!   Outputs   : BOS banks RUNH, EVEH, TPTE.  Random number seed at
C!               start of event.
C!
C!   Called by : TGTEER
C    Calls    : RDMOUT (CERNLIB), BWRITE, VMCMS (IBM only), ALEVEH
C               ALTELL (ALEPHLIB)
C
C    Modifications :
C     1.- P. Janot  06 sep 88 -- Allow IBM users to use TDEBUG !
C     2.- P. Janot  01 oct 88 -- Build event header if not there.
C     3.- D. Cowen  17 oct 88 -- Get NAEVEH from BOS call, not from
C                                GALEPH commons, since these commons
C                                do not exist for standalone TPCSIM.
C     4.- F.Ranjard 28 mar 89 -- suppress references to GALEPH *CD and
C                                add argument in the calling sequence.
C                                adapt to new random number generators.
C!
C----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
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
      CHARACTER*80 COM
      INTEGER ALEVEH,DBGUNI,RNDSED(3)
C
      DATA DBGUNI, ICALL / 75, 0/
C
      NAEVEH  = NAMIND('EVEH')
      KEVEH   = IW(NAEVEH)
C
C  Build the 'EVEH' if not there
C
      IF(KEVEH .EQ. 0) THEN
        ISTAT = 1
        IEXP  = 1001
        ECMS  = 90.
        IPRO  = 1001
        IRUN  = 1001
        KRUNH = IW(NAMIND('RUNH'))
        IF (KRUNH.GT.0) THEN
           IEXP = IW(KRUNH+1)
           IRUN = IW(KRUNH+2)
           IPRO = IW(KRUNH+3)
        ENDIF
        KEVEH = ALEVEH (NEVT,IEXP,IRUN,ECMS,IPRO,ISTAT)
      ENDIF
      IF (KEVEH.NE.0) THEN
         IEVENT = IW(KEVEH + 6)
C
         CALL BWRITE(IW,DBGUNI,'RUNHAFIDTSIM')
         CALL BWRITE(IW,DBGUNI,'EVEHTPTE')
         CALL BWRITE(IW,DBGUNI,'0')
C
         CALL RDMOUT(RNDSED)
         WRITE(6,100) IEVENT,RNDSED
C
      ELSE
         WRITE(6,101)
      ENDIF
C
      GO TO 999
C
  888 WRITE(6,102) DBGUNI
C
999   CONTINUE
      RETURN
C
  100 FORMAT(/' TPCSIM DEBUG: seed at start of EVEH event ',I6,
     &         ' is : ',3I20/)
  101 FORMAT(/' +++DEBUG+++ No EVEH bank found.'//)
  102 FORMAT(/' +++DEBUG+++ Problem opening unit ',I2,
     &        ' for debug write.'//)
C
      END
