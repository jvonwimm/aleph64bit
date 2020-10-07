      SUBROUTINE TSWRED(ISECT)
C--------------------------------------------------------------------
C! Reduction of TPC wire data
C  Author: M.Mermikides  4-Oct-1987
C  INPUT:
C       ISECT    I/  TPC sector number
C
C  Description:
C  ===========
C
C       The purpose of this routine is to reduce the wire data
C  to start time and integrated charge for each pulse.
C  The information is packed in each hitlist word (in situo) as follows:
C      Bit:  31           24|23          14|13|12            0
C            -------------------------------------------------
C            | Wire number  | Total charge | 1|  Time est.    |
C            -------------------------------------------------
C  Bit 13 = 1 indicates that the wire data are reduced.
C  The total charge of each pulse is obtained by summing the
C  pulse heights and the time estimator is computed from the pulse shape
C  using the digitizations in TWDI.
C  The summed pulse heights are scaled by a factor WPSCAL found from
C  studies of pulse area distributions from simulated events, and
C  the time is multiplied by 16 to make use of the 13-bit resolution.
C  Pulses of inconsistent length or of complex structure are flagged
C  by setting the time estimator to saturation value (8191). Pulses
C  with more than two saturated samples are given charge = 1023
C
C  Modifications:
C    1. D. Cowen 24-06-88 -- Pulses of inconsistent length or of
C                            complex structure are dropped.  Time
C                            is multiplied by 16 instead of 8.
C    2. R. Johnson 20-10-88 -- Simplified code.  No longer any
C                              check on number of peaks.
C    3. D. Cowen (M. Mermikides) 14-Mar-89 -- Bug fix: Before, did
C                              not restore the wire number when packing
C                              information back into hitlist word.
C
C
C--------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
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
      DATA MTHRSH/2/
C
      KTWIR = NLINK(DIGNAM(1),ISECT)
      KTWDI = NLINK(DIGNAM(2),ISECT)
      IF (KTWIR.EQ.0.OR.KTWDI.EQ.0) GO TO 999
      NHIT = IW(KTWIR)
      IF (NHIT.LE.0) GO TO 999
C
      JSAMP = 0
      IBADPL = 0
C
      DO 50 I = 1,NHIT
         ID    = IW (KTWIR + I)
         IWIRE = IBITS(ID, 24, 8)
         IT0  = IBITS(ID,  0, 9)
         NSAMP = IBITS(ID, 16, 8)
C
         LENP=0
         NSAT = 0
         IPSUM = 0.
         IWSUM = 0.
         DO 20 J = 1,NSAMP
            JSAMP = JSAMP + 1
            JJ = (JSAMP-1)/4 + 1
            JA= 8*(4*JJ-JSAMP)
            IPH = IBITS(IW(KTWDI + JJ), JA, 8)
C
            IF (IPH.GT.MTHRSH) THEN
               LENP=LENP+1
               IPSUM=IPSUM+IPH
               IWSUM=IWSUM + J*IPH
            ENDIF
            IF (IPH.GE.255) NSAT=NSAT+1
   20    CONTINUE
C
C Pulse of inconsistent length, or with more than one peak
C
         IF (LENP.LT.MINLEN .OR. LENP.GT.NWSMAX) THEN
            IBADPL = IBADPL + 1
            GO TO 50
         ENDIF
C
C Time 0 -> 511.  Multiply by 16 to exploit 13-bit resolution
C
         PSUM=FLOAT(IPSUM)
         TIME=FLOAT(IT0-1)+FLOAT(IWSUM)/PSUM
         ITIME = NINT(16.0*TIME)
C
C Encode charge.  Pulses with saturated samples are forced to be
C completely saturated
C
         IF (NSAT.GE.2) THEN
            ICHAR=1023
         ELSE
C
C Scale factor is for default time slice of 100 nsec.  We scale the
C area by an additional factor which is the ratio of the actual
C bucket size used to the nominal.
C
            PAREA = PSUM*(WPSCAL*TPDGBN/100.)
            ICHAR = INT(PAREA)
            ICHAR = MIN(ICHAR,1023)
         ENDIF
C
C++  Pack information back into hitlist word if pulse is good.
C
         CALL MVBITS(IWIRE, 0, 8 ,IW(KTWIR+I-IBADPL),  24)
         CALL MVBITS(ICHAR, 0,10 ,IW(KTWIR+I-IBADPL),  14)
         CALL MVBITS(ITIME, 0,13 ,IW(KTWIR+I-IBADPL),   0)
         IW(KTWIR+I-IBADPL) = IBSET(IW(KTWIR+I-IBADPL),13)
C
   50 CONTINUE
C
C++  Reduce the size of this sector's TWIR bank by subtracting off
C++  the space which would have been used by bad pulses.
C
      IF (IBADPL .GT. 0) THEN
         LENGTH = NHIT - IBADPL
         CALL AUBOS('TWIR',ISECT,LENGTH,INDEX,KGARB)
         IF(KGARB .EQ. 2) GOTO 990
      ENDIF
C
      GO TO 999
C
  990 WRITE (5,101)
  101 FORMAT (//,'+++TSWRED+++ AUBOS could not find space!',//)
C
  999 RETURN
      END
