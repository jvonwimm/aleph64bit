      COMMON /TFLAGS/ IEPIO,MAGNET,NOPADS,FNOWIR,NOTRKF,FTPC90,
     &                FNOTPP,FTSTAT,DUM1TF,FCLEAN,FWIRPU,FFCORR,
     &                FMCTKA,DUM2TF,FECORR,FOLDWR,FSRTDX,FFMODE
              LOGICAL MAGNET,NOPADS,FNOWIR,NOTRKF,FTPC90,FNOTPP,FTSTAT
              LOGICAL DUM1TF,FCLEAN,FWIRPU,FFCORR,FMCTKA,DUM2TF,FECORR
              LOGICAL FOLDWR,FSRTDX,FFMODE
#if defined(DOC)
C!     Flags for control of TPC reconstruction.  Filled in TINIJO
C
C     MAGNET = .T.  Use B-field event by event from Hall probe
C     NOPADS = .T.  don't use pad data for clusters
C     FNOWIR = .T.  use wire data
C     NOTRKF = .T.  don't do track finding
C     FTPC90 = .T.  signifies that the event is from TPC90 (prototype)
C     FNOTPP = .T.  signifies that no TPP was used in DAQ
C     FTSTAT = .T.  accumulate statistics over the events analyzed
C     FCLEAN = .T.  clean up immediately all unnecessary banks
C     FWIRPU = .T.  fill the TWPU bank for pointers into wire raw data
C     FFCORR = .T.  correct coordinates for drift field imperfections
C     FECORR = .T.  correct coordinates for transverse drift field
C     FMCTKA = .T.  do the track-MC association (TRKMCA)
C     FOLDWR = .T.  Use old wire raw data banks even if the new ones
C                   are available
C     FSRTDX = .T.  Sort dE/dx samples by sector for each track.
C     FFMODE = .T.  Use B-field displacement map for radial coordinate
C                   correction in data taken with comp coils powered.
C                   Otherwise, use just the laser corrections.
C-----------------------------------------------------------------------
#endif
