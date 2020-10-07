C! YTOP track parameters
      COMMON/YTRKTO/NGTRTO,IPTRTO(MAXTRK),
     &              PTRECT(MAXTRK),PRECTO(MAXTRK),
     &              KPIDF0(MAXTRK),KPORF0(MAXTRK)
C
#if defined(DOC)
C     NGTRTO   ..... nb of tracks selected for vertex search
C     IPTRTO   ..... indices of selected tracks
C     PTRECT  ..... transverse momentum of tracks
C     PRECTO   ..... momentum of tracks
C     KPIDF0  ...... PARICLE IDENTIFICATION FLAG. HAS BITS SET FOR
C                    POSSIBLE ASSIGMENTS.
C                    BIT ASSIGNEMENT DEFINED IN COMMON YPAFLJJ
C                    1 POSITRON, 2 ELECTRON,   3 MU+, 4 MU-,
C                    5 PI+,      6 PI-,        7 K+,  8 K-,
C                    9 PROTON,  10 ANTIPROTON,
C              USED ALSO FOR NEUTRAL TRACKS WITH OFFSET MAXHLX
C                   11 PHOTON,  12 PIZERO,    13 K0, 14 LAMBDA,
C                   15 LAMBDA_B,16            17     18
C                   19          20
C    KPORF0  ....... particle origin flag. Has bits set for possible
C                    ORIGINS: PHOTON,K0,LAMBDA,LAMDA_BAR
C                    BIT ASSIGNEMENT DEFINED IN COMMON YPAFLJJ
#endif
