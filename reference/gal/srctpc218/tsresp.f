      SUBROUTINE TSRESP(ISECT,ITYPE)
C--------------------------------------------------------------------
C!  Steering routine for TPC electronics chain:  does shaping and
C!  amplification, digitization, and TPP formatting
C
C  Called from:  TSTEER
C  Calls:        TPFLPL, TPISOL, TSSHAM, TSDIGT, TPZSPR
C                TPFMT, TWFMT, TTFMT
C
C  Inputs:   PASSED:      --ISECT, sector number
C                         --ITYPE, sector type
C            BANKS:       --id INDREF, signal reference bank
C                         --id INDSIG, analog signal bank
C                         --id ITPULS, single-pulse (analog) bank
C                         --id ITMADC, single-pulse (digit.) bank
C
C  Outputs:  BANKS:       --opens workbanks INDHL, INDDI for
C                           accumulating hitlists and digitisations.
C
C  D. DeMille
C  M. Mermikides 19/5/86 Don't call TxFMT(1,..) if there are
C                        no hits to avoid empty sectors.
C                20/5/86 Ignore pulses of length < 3
C                31/8/86 Store hitlists and dig. in workbanks to avoid
C                        BOS problems
C  R. Johnson    28/11/86  Call new routine TPZSPR to do zero-
C                          suppression according to the TPD scheme
C  M.Mermikides  7/5/87   Drop work banks INDHL,INDDI at the end
C                         of each channel type
C  M.Mermikides  8/11/87  Use different constants for zero suppression
C                         or WIRE and PAD channels
C  P.Janot      02/03/89 Correct number of arguments in WBANK calls
C  P.Janot      03/03/89 Do NOT use local indices for work banks !!!
C                        (especially after WBANK calls)
C  D.Casper     12/10/92 Add channel type to call of TSDIGT
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
      COMMON / HISCOM / IHDEDX,IHTRAN,IHAVAL,IHCOUP,IHTRCP,IHBOS,IHTOT
C
      PARAMETER( MXISL = 20 )
      DIMENSION IOFFA(MXISL),NLENA(MXISL),
     *          IOFFD(MXISL),NLEND(MXISL)
C
      LOGICAL LDBI1,LDBI2
C
      DATA ICALL/0/
C
      ICALL = ICALL + 1
      LDBI1 = ( NTPCDI .GE. 1 .AND. ICALL .LE. NCALDI )
      LDBI2 = ( NTPCDI .GE. 2 .AND. ICALL .LE. NCALDI )
C
C  KCHAN indicates channel type:  1 = wire, 2 = pad, 3 = trigger pad
C
      DO 4 KCHAN = 1,NCHAN
C
C  If we are not doing the electronics simulation for this channel
C  type, go to the next channel type
C
        IF ( .NOT. LTDIGT(KCHAN) ) GOTO 4
C
C  Initialize number of hits, number of buckets in digitization bank,
C  and number of channels hit for this channel type
C
        NHITS = 0
        NCHIT = 0
C
C  Open the hit list and digitization banks for this channel type
C
        CALL WBANK(IW,INDHL,NDIDEF(1),*998)
C
        CALL WBANK(IW,INDDI,NDIDEF(2),*997)
C
C  Loop over all channels of type kchan
C
        DO 3 JCHN = 1 , IW(INDREF(KCHAN)+1)
C
C  ICHREF = beginning index of info for the jhit-th wire hit
C         = idr + lhdr + (jhit-1)*nwordsperwire
C  See if this channel has been hit; if not, go to next channel
C
          ICHRF = INDREF(KCHAN) + 2 + (JCHN-1)*IW(INDREF(KCHAN)+2)
          IF ( IW(ICHRF+1) .EQ. 0 ) GOTO 3
C
C  This channel has been hit.  Get the first time bin of the signal
C  and its length, and fill the single-pulse bank for shaping and
C  amplification.  Also increment the number of channels of this type
C  hit
C
          NCHIT = NCHIT + 1
          CALL TPFLPL(ICHRF,IBIN1,NANLG)
C
C  It is possible that the stretch of time between the first and last
C  affected time bins may be filled with mostly zeros, which we do not
C  want to shape and digitize.  Thus, we now search through this time
C  stretch and count and label in time the isolated pulses.
C  ('Isolated' in this context means separated by more than the time
C   length of the electronic response function)
C
          CALL TPISOL(IW(ITPULS+IBIN1),NANLG,NLSHAP,
     *                                 IOFFA,NLENA,NISLA,MXISL)
C
C  Now loop over these isolated pulses
C
          DO 2 JISLA = 1, NISLA
C
            MABIN = IBIN1 + IOFFA(JISLA)
            NALEN = NLENA(JISLA)
C
C  Get the amplification and shaping.
C  Skip to next pulse on error in TSSHAM.  DFC 15-OCT-88.
C
            CALL TSSHAM(ITYPE,KCHAN,JCHN,MABIN,NALEN,IADC1,NLADC)
            IF ( NLADC .LE. 0 ) GO TO 2
C
C  Get the digitizations
C
            CALL TSDIGT(KCHAN,IADC1,NLADC)
C
C  Do a zero suppression algorithm as done in hardware in the TPDs.
C  The signal will be broken into NISLD pulses
C  (Use different constants for WIRES and PADS)
C
            IF (KCHAN.EQ.1) THEN
               CALL TPZSPR(IW(ITMADC+IADC1+1),NLADC,LTHRSH,NPRESP,
     &                     NPOSTS,MINLEN,IOFFD,NLEND,NISLD,MXISL)
            ELSEIF (KCHAN.EQ.2) THEN
               CALL TPZSPR(IW(ITMADC+IADC1+1),NLADC,LTHRS2,NPRES2,
     &                     NPOST2,MINLE2,IOFFD,NLEND,NISLD,MXISL)
            ENDIF
C
C  Now loop over the isolated pulses in the digitized signal
C
            DO 1 JISLD = 1, NISLD
C
C  Every time we get here, we have another hit.  Increment this number,
C  then put the hit into the hit list and the digitization into the
C  digitization bank.  NDI is the actual number of words
C  occupied in the digitization bank
C
              NDLEN = NLEND(JISLD)
              NHITS = NHITS + 1
              MDBIN = IADC1 + IOFFD(JISLD) + 1
C
              IF (KCHAN.EQ.1) THEN
                 CALL TWFMT(0,ISECT,JCHN,MDBIN,NDLEN,NHITS,IERR)
              ELSEIF (KCHAN.EQ.2) THEN
                 CALL TPFMT(0,ISECT,ITYPE,JCHN,MDBIN,NDLEN,NHITS,IERR)
              ELSEIF (KCHAN.EQ.3) THEN
                 CALL TTFMT(0,ISECT,JCHN,MDBIN,NDLEN,NHITS,IERR)
              ENDIF
C
              IF (IERR.NE.0) GO TO 35
C
C  End loop over isolated pulses in digitized signal
C
 1          CONTINUE
C
C  End loop over isolated pulses in analog signal
C
 2        CONTINUE
C
C  End loop over channels of type kchan
C
 3      CONTINUE
C
C  The hit list and digitization banks for this sector of this channel
C  are now filled.  Squeeze out any empty space left at the end and
C  transfer data to named hitlist and dig. banks
C
   35   IF(NHITS.GT.0) THEN
           IF(KCHAN.EQ.1)  CALL TWFMT(1,ISECT,0, 0, 0, NHITS,IERR)
           IF(KCHAN.EQ.2)  CALL TPFMT(1,ISECT,0, 0, 0, 0, NHITS,IERR)
           IF(KCHAN.EQ.3)  CALL TTFMT(1,ISECT,0, 0, 0, NHITS,IERR)
        ENDIF
C
C  Drop work banks
C
        GOTO 40
  998   WRITE(6,801) ISECT,KCHAN
  801   FORMAT(' +++TSRESP+++ Error booking hitlist workbank',/
     .         ' Dig will not be done for sector',I5,
     .         ' CHAN type ',I5)
        GOTO 40
  997   WRITE(6,802) ISECT,KCHAN
  802   FORMAT(' +++TSRESP+++ Error booking dig. workbank',/
     .         ' Dig will not be done for sector',I5,
     .         ' CHAN type ',I5)
   40   CALL WDROP(IW,INDHL)
        CALL WDROP(IW,INDDI)
C
        IF ( LDBI1 ) THEN
          CALL HF1(IHBOS+KCHAN,FLOAT(NCHIT),1.)
          IF ( NHITS .GT. 0 ) THEN
            CALL HF1(IHBOS+NCHAN+KCHAN,FLOAT(NHITS),1.)
            JDIGI = NLINK(DIGNAM(KCHAN*2),ISECT)
            NDI = 0
            IF (JDIGI.NE.0) NDI = IW(JDIGI)*4
            CALL HF1(IHBOS+2*NCHAN+KCHAN,FLOAT(NDI),1.)
            IF ( LDBI2 ) WRITE(6,102) DIGNAM(2*KCHAN-1),
     &               DIGNAM(2*KCHAN),ISECT,NHITS,NDI
          ENDIF
        ENDIF
C
C  End loop over channel types
C
 4    CONTINUE
C
 999  RETURN
C_______________________________________________________________________
C
 102  FORMAT(' +++TSRESP+++ Readout banks ',2A6,'  for SECTOR',I3,
     & '  NHITS',I6,'  NSAMP',I6)
C
      END
