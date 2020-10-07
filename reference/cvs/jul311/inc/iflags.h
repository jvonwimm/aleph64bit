      LOGICAL         FTPCIF,FITCIF,FEXTIF,FMATIF,FMCAIF,FREFIF
      COMMON /IFLAGS/ FTPCIF,FITCIF,FEXTIF,FMATIF,FMCAIF,FREFIF
#if defined(DOC)
C!     Flags for control of ITC tracking options. Filled in IINIJO
C
C FTPCIF = .T.  do ITC-TPC tracking (i.e. extending TPC tracks into ITC)
C FITCIF = .T.  do ITC alone tracking
C FEXTIF = .T.  do extending of ITC alone tracks back into TPC (to pick
C                up extra TPC hits - especially for low theta tracks)
C FMATIF = .T.  do ITC-TPC track matching (subroutine IMATCH)
C FMCAIF = .T.  do the track-MC association
C FREFIF = .T.  do track refining/improving (sub. IREFIN)
#endif
