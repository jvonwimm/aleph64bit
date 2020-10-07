      SUBROUTINE QJMMCL (NJETS,CNAM,ICLASS,YCUT,EVIS)
CKEY ALPHA JETS /USER
C----------------------------------------------------------------------
C   Author   : P. Perez       28-MAR-1989
C   Modified : C. Bowdery      5-OCT-1990
C   Modified : C. Bowdery     18-APR-1991 to use the E scheme explicitly
C   Modified : C. Bowdery     28-APR-1991 to use the NORMAL algorithm
C   Modified : C. Bowdery     18-OCT-1991 to call QGJMMC
C
C   Description
C   ===========
C!   Call QGJMMC, the improved, generalised JADE algorithm jet finder
C!   with the arguments set to 'E' scheme and normal JADE algorithm.
C
C  Input   : CNAM            the name to be used for the jet 'particles'
C            ICLASS          KRECO or KMONTE
C            YCUT            YCUT value ( (M/EVIS)**2 )
C            EVIS            visible energy for normalisation
C                            (if EVIS=0., it is computed from the
C                             input particle energies)
C
C  Output  : NJETS           number of jets found or error code if -ve
C======================================================================
#ifndef DOC
C
      INTEGER    NJETS, ICLASS
      REAL       YCUT, EVIS
C
      CHARACTER  SCHEME*2, VERSN*6, CNAM*(*)
C
C-----------------------------------------------------------------------
C
C                             Use the E combination scheme and the
C                             normal JADE algorithm
C
      SCHEME = 'E'
      VERSN  = 'NORMAL'
C
      CALL QGJMMC( NJETS, CNAM, ICLASS, YCUT, EVIS, SCHEME, VERSN )
C
      END
#endif
