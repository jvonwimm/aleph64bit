      SUBROUTINE TWFMT(KEND,ISECT,JCHN,IBIN1,NLEN,NHITS,IERR)
C--------------------------------------------------------------------
C!  Format MC WIRE data to emulate the TPP readout
C
C  Called from:  TSRESP
C  Calls:        BKFRW, BLIST
C
C  Inputs:   PASSED:      --KEND,   "compress banks" flag
C                         --ISECT,  sector number
C                         --JCHN,   channel number
C                         --IBIN1,  first bin of this signal
C                         --NLEN,   number of bins in this signal
C                         --NHITS,  hit number or number of hits
C            TPCBOS.INC:  --DIGNAM, names of hl and dig banks
C                         --NDIDEF, default lengths of hl and dig banks
C                         --NDIEXT, default extensions
C                         --ITMADC, id for single digitized pulse
C
C  Outputs:
C            BANKS:       --named bank TWIR  NR = sector number,
C                           list 'E'; hit list
C                           IW(IDHL)  = number of hits (bank length)
C                           IW(IDHL+N) = info for hit, bit-packed
C                                          in MTPKHL
C                         --named bank TWDI, NR = sector number,
C                           list 'E'; digitizations
C                           IW(IDDI+Nbytes) = ADC counts in bucket,
C                                             1 bucket per byte
C            IERR:        -- 0 for normal completion, 1 if insufficient
C                            space to increase work bank length
C  D. DeMille
C  M. Mermikides   20/5/86   Correct offset error in storing dig.
C  M. Mermikides   11/3/87   Pack digitisations in IBM byte counting
C                            convention
C  D. Cowen        24/6/88   Change sector number on TPDI, TPAD
C                            banks to accomodate ONLINE's stupid
C                            convention.
C  D. Cowen        22feb89   Allow WBANK compression to give
C                            error message.
C  P. Janot        03mar89   Correct number of arguments in
C                            EVERY WBANK calls !
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
      DATA NHLPT,NDIPT/1,2/
C
      DATA NDI,NDILW,IBYTE/0,1,0/
C
      IERR = 0
C
C  If KEND = 1, we only are compressing the banks.
C
      IF ( KEND .EQ. 1 ) GOTO 11
C
C  If we're starting a new channel type, open the hit list and
C  digitization banks
C
      IF ( NHITS .EQ. 1 ) THEN
C
         NDI = 0
         NDILW = 1
         IBYTE = 0
C
      ENDIF
C
C  If we need more space, extend the banks
C
      NHLNEW =  NHITS
C
      IF ( NHLNEW .GT. IW(INDHL) ) THEN
         NDWRDS = IW(INDHL) + NDIEXT(1)
         CALL WBANK(IW,INDHL,NDWRDS,*997)
      ENDIF
C
      NDINEW = (NDI+NLEN-1)/4 + 1
C
      IF ( NDINEW .GT. IW(INDDI) ) THEN
         NDWRDS = IW(INDDI) + NDIEXT(2)
         CALL WBANK(IW,INDDI,NDWRDS,*996)
      ENDIF
C
C  Now we're ready to put this hit into the hit list; we pack
C  the channel number, first bin, and length of the pulse according
C  to TPP format for non-reduced wire hits.
C
      ID = IBIN1
      CALL MVBITS(NLEN, 0,8, ID, 16)
      CALL MVBITS(JCHN, 0,8, ID, 24)
      IW(INDHL+NHITS) = ID
C
C  Now the digitizations. Since the digitizations range from 0 to 255,
C  we put them in 1-byte words.  Bytes are stored from left to right
C  according to IBM convention
C
      INDADC = ITMADC + IBIN1 - 1
C
      DO 1 JBUCK = 1, NLEN
C
         NDI = NDI + 1
         IBYTE = IBYTE + 1
C
         IF ( IBYTE .EQ. 5 ) THEN
            IBYTE = 1
            NDILW = NDILW + 1
         ENDIF
C
         NBITLW = 8*(4-IBYTE)
         CALL MVBITS(IW(INDADC+JBUCK),0,8,IW(INDDI+NDILW),NBITLW)
C
 1    CONTINUE
C
C  Normal successful completion
C
      RETURN
C
C  If here, we are taking care of the banks after we are done
C  entering signals
C
 11   CONTINUE
C
      NHLWD =  NHITS
      NDIWD = (NDI-1)/4 + 1
C
C  Compress the workbanks
C
      CALL WBANK(IW,INDHL,NHLWD,*998)
      CALL WBANK(IW,INDDI,NDIWD,*998)
C
C  Copy into named banks (NB declare dig bank format as B32, otherwise
C  BKFRW will try to pack)
C  But first, change sector numbers 1 <==> 19, 2 <==> 20, etc.
C
      IF (ISECT .LE. 18) THEN
         ISECT2 = ISECT + 18
      ELSE
         ISECT2 = ISECT - 18
      ENDIF
C
      CALL BKFRW(IW,DIGNAM(NHLPT),ISECT2, IW,INDHL, *998)
      CALL BKFRW(IW,DIGNAM(NDIPT),ISECT2, IW,INDDI, *998)
C
      CALL BLIST(IW,'E+',DIGNAM(NHLPT))
      CALL BLIST(IW,'E+',DIGNAM(NDIPT))
C
C  Normal successful completion
C
      RETURN
C
C  Problem lifting BOS banks
C
  997 WRITE(6,801) ISECT
  801 FORMAT(' +++TWFMT+++ Error extending WIRE hitlist workbank',
     .       ' for sector',I5)
      IERR = 1
      RETURN
  996 WRITE(6,802) ISECT
  802 FORMAT(' +++TWFMT+++ Error extending WIRE dig. workbank',
     .       ' for sector',I5)
      IERR = 1
      RETURN
 998  WRITE(6,999) ISECT2
 999  FORMAT(' +++TWFMT+++ Error lifting or compressing',
     *       '  TWDI or TWIR bank for sector',I3)
C
      RETURN
      END
