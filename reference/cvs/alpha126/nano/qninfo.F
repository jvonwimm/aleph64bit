      SUBROUTINE QNINFO
CKEY NANO IN ALPHA /USER
C----------------------------------------------------------------------
C!  - Print NanoDST production parameters
C!
C!   Author   :- Yves A. Maumary       11-DEC-1991
C!   Modified :- Yves A. Maumary       29-JAN-1993  Exit if no NDST bank
C!                                                  read yet
C!   Modified :- Gerrit Graefe         13-APR-1994  implemented new banks
C!   Modified :- Gerrit Graefe         05-MAY-1994  put into ALPHA
C!   Modified :- Gerrit Graefe         27-FEB-1995  added new NDCL NDOB bank
C!
C!======================================================================
#ifndef DOC
#include "qcde.h"
#include "qncde.h"
#include "qmacro.h"
#include "qnmacr.h"
C-----------------------------------------------------------------------
      IF(NCPROD.EQ.'    ')THEN
        WRITE(KUPRNT,1000)
 1000   FORMAT(/,' >>>>>>>>>>>>>>>> WARNING <<<<<<<<<<<<<<<<',/,
     &    ' Bank NDST not found >>> no INFO available',/)
        RETURN
      ENDIF
C
      WRITE(KUPRNT,1001)
 1001 FORMAT(/,' >>> INFO  on current run / NanoDST input file <<<',/)
C
      CALL PRRHAH
C
      WRITE(KUPRNT,1201) NCLPOT,NCLRUN
      WRITE(KUPRNT,1202) INDEX(NCLNDS,'NDBM').NE.0,
     &                   INDEX(NCLNDS,'NDCL').NE.0,
     &                   INDEX(NCLNDS,'NDDE').NE.0,
     &                   INDEX(NCLNDS,'NDEJ').NE.0,
     &                   INDEX(NCLNDS,'NDGC').NE.0,
     &                   INDEX(NCLNDS,'NBIP').NE.0,
     &                   INDEX(NCLNDS,'NDJT').NE.0,
     &                   INDEX(NCLNDS,'NDMS').NE.0,
     &                   INDEX(NCLNDS,'NDNT').NE.0,
     &                   INDEX(NCLNDS,'NDOB').NE.0,
     &                   INDEX(NCLNDS,'NDPH').NE.0,
     &                   INDEX(NCLNDS,'NDTK').NE.0,
     &                   INDEX(NCLNDS,'NDLV').NE.0,
     &                   INDEX(NCLNDS,'NDV0').NE.0
 1201 FORMAT(' List of event banks: ',A,/,
     &       ' List of run banks: RUNHRUNRRHAH',A,/)
 1202 FORMAT(L2,' NDBM lepton candidates among trks (LEPTAG)',/,
     &       L2,' NDCL lepton candidates among trks (QSELEP)',/,
     &       L2,' NDDE dE/dx for charged tracks',/,
     &       L2,' NDEJ EFLOW jets',/,
     &       L2,' NDGC gamma conversion candidates',/,
     &       L2,' NBIP QIPBTAG track probabilities',/,
     &       L2,' NDJT jets',/,
     &       L2,' NDMS Monte-Carlo stable particles',/,
     &       L2,' NDNT Monte-Carlo unstable particles',/,
     &       L2,' NDOB EFLOW objects',/,
     &       L2,' NDPH photons',/,
     &       L2,' NDTK charged tracks',/,
     &       L2,' NDLV Monte-Carlo truth V0  vertices',/,
     &       L2,' NDV0 V0 candidates among charged trks',/)
      WRITE(KUPRNT,1301) ECHMIN,MINTRK,PCUTMI,PCUTMA,THCCUT,NITCUT,
     &                   NTPCUT,BD0CUT,BZ0CUT
      WRITE(KUPRNT,1302) PCUMIJ,PCUMAJ,THCCUJ,NITCUJ,NTPCUJ,
     &                   BD0CUJ,BZ0CUJ
      WRITE(KUPRNT,1303) BMYCUT,ENEVIS,RECOOP
C     WRITE(KUPRNT,1304) PHOCUT
      WRITE(KUPRNT,1305) V0CHSI,V0CHBO
 1301 FORMAT(' NanoDST parameters:',/,
     &  ' ECHMIN: MINimum CHarged Energy                              ',
     &  G15.5,/,
     &  ' For the charged tracks bank NDTK:',/,
     &  ' MINTRK: MINimum number of charged TRacKs                    ',
     &  I3,/,
     &  ' PCUTMI: lower momentum CUT for charged tracks               ',
     &  G15.5,/,
     &  ' PCUTMA: upper momentum CUT for charged tracks               ',
     &  G15.5,/,
     &  ' THCCUT: Cosine THeta CUT for charged tracks                 ',
     &  G15.5,/,
     &  ' NITCUT: minimum Number of ITC coordinates for charged Tracks',
     &  I3,/,
     &  ' NTPCUT: minimum Number of TPC coordinates for charged Tracks',
     &  I3,/,
     &  ' BD0CUT: upper D0 CUT for charged tracks                     ',
     &  G15.5,/,
     &  ' BZ0CUT: upper Z0 CUT for charged tracks                     ',
     &  G15.5,/)
 1302 FORMAT(' For the jets bank NDJT:',/,
     &  ' PCUMIJ: lower momentum CUT for charged tracks               ',
     &  G15.5,/,
     &  ' PCUMAJ: upper momentum CUT for charged tracks               ',
     &  G15.5,/,
     &  ' THCCUJ: Cosine THeta CUT for charged tracks                 ',
     &  G15.5,/,
     &  ' NITCUJ: minimum Number of ITC coordinates for charged Tracks',
     &  I3,/,
     &  ' NTPCUJ: minimum Number of TPC coordinates for charged Tracks',
     &  I3,/,
     &  ' BD0CUJ: upper D0 CUT for charged tracks                     ',
     &  G15.5,/,
     &  ' BZ0CUJ: upper Z0 CUT for charged tracks                     ',
     &  G15.5,/)
 1303 FORMAT(' For the jet algorithm:',/,
     &  ' BMYCUT: mass cut for jet algorithm QJMMCL                   ',
     &  G15.5,/,
     &  ' ENEVIS: VISible ENErgy for jet algorithm QJMMCL             ',
     &  G15.5,/,
     &  ' RECOOP: REConstructed Objects OPtion                        ',
     &  A2,/)
C1304 FORMAT(' For the photons bank NDPH:',/,
C    &  ' PHOCUT: lower energy CUT for PHOtons                        ',
C    &  G15.5,/)
C
 1305 FORMAT(' For the V0 bank NDV0:',/,
     &  ' V0CHSI: minimum CHi square increase of SIngle track from V0 ',
     &  /,
     &  '         fitting back to the main vertex.                    ',
     &  G15.5,/,
     &  ' V0CHBO: minimum CHi square increase of BOth tracks from V0  ',
     &  /,
     &  '         fitting back to the main vertex.                    ',
     &  G15.5,/)
      END
#endif
