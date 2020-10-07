      SUBROUTINE TPOPSG(NBLKS,IERR)
C--------------------------------------------------------------------
C!  Manage size of the analog signal workbank
C
C  Called from:  TPSGNL, TSTEER, TORAWI
C  Calls:        WBANK
C
C  Inputs:   PASSED:      NBLKS,  the number of blocks wanted in
C                                 the analog signal bank; NBLKS = -1
C                                 means to use preloaded sizes for
C                                 opening and extension
C            TPCONS.INC:  NTBNAS, number of time bins in each division
C                                 of the analog signal collection time,
C                                 i.e., in each block of the analog
C                                 signal bank
C            TPCBOS.INC:  NDEFCH, default number of blocks to open
C                         NEXTCH, defaulst number of blocks by which
C                                 to extend the bank
C
C  Outputs:  BANKS:       work bank id INDSIG, containing analog
C                         signals as they are collected
C                         IW(INDSIG+1) = number of bins per block
C                         IW(INDSIG+2) = number of blocks open
C                           KHID = INDSIG + 2 + (JHIT-1)*IW(INDSIG+1)
C                           IW(KHID+I) = num of electrons in Ith bin
C                                        of JHITth analog signal
C                                        section hit (see TPSGNL)
C            IERR:        = 1 if no enough BOS space to open/extend bank
C  D. DeMille
C
C  Modifications:
C
C     22feb89 DFC   -- Change WBANK so it takes error return *998.
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
      LOGICAL LOPEN, LAUTO
C
C  See if the bank is already open
C
      LOPEN = ( INDSIG .NE. 0 )
C
C  See if this call uses pre-loaded or manual sizes
C
      LAUTO = ( NBLKS .EQ. -1 )
C
C  Decide how many blocks we will end up with
C
      IF ( LAUTO ) THEN
C
         IF ( .NOT. LOPEN ) THEN
            NFINI = NDEFCH
         ELSE
            NFINI = IW(INDSIG+2) + NEXTCH
         ENDIF
C
      ELSE
C
         NFINI = NBLKS
C
      ENDIF
C
C  Extend the bank
C
      IERR = 0
      NDW = 2 + NFINI*NTBNAS
      CALL WBANK(IW,INDSIG,NDW,*998)
C
C  Fill in the header
C
      IW(INDSIG+1) = NTBNAS
      IW(INDSIG+2) = NFINI
C
      RETURN
C
C  Problem opening work bank
C
 998  WRITE(6,999) NBLKS, NFINI
 999  FORMAT(2X,' ++TPOPSG++ PROBLEM OPENING SIGNAL WORK BANKS',
     *          ' PASSED PARAMETER: ',I4,
     *          ' ACTUAL NUMBER OF BLOCKS ATTEMPTED TO OPEN: ',I4)
      IERR = 1
      RETURN
C
      END
