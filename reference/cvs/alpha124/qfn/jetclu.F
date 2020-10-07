      SUBROUTINE JETCLU(CNAM,PCUT,NJET,JETS,TRKJ )
CKEY   QFNDIP  QIPBTAG / INTERNAL
C ------------------------------------------------------------------------------
C! Jet clustering for QFNDIP and QIPBTAG
C  New version of jet clustering routine;  Use standard (ENFLW) energy
C  flow, jet algorithm (QJMMCL, E=ELEP), ycut (.01).  This will
C  use DJET banks if they exist, otherwise raw ENFLW objects.  Note that
C  a cut is used to define the jets so that THE LIST OF JETS IS NOT
C  COMPLETE.  The jets returned are useful (high momentum) jets for
C  many applicateions.
C  Called from QFNDIP or QIPBTAG
C                                               Dave Brown, 29,1,93
C  Modified 15-9-93 Dave Brown, ycut=0.01, jet vector not normalized.
C
C     INPUT:
C        CNAM     :   Name of output ALPHA jet objects
C        PCUT     :   Minimal Jet momentum
C     OUTPUT:
C        NJET     :   Number of jets found
C                      (momentum ordered, P>cut)
C        JETS     :   3-Momentum of the jets   i=1,njet
C        TRKJ     :   ALPHA track # of jet with number i
C
C    CALLS :
C      CALL SORTZV
C      CALL QJOPTR
C      CALL QJMMCL
C ------------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
C
C  Global includes
C
#include "qdecl.h"
#include "qcde.h"
#include "maxtrk.h"
C
C  Output parameters
C
      INTEGER NJET,TRKJ(MAXJET)
      REAL JETS(3,MAXJET),JP(MAXJET)
C
C  Local variables
C
      INTEGER MJET,IJET,JTRK,KJET(MAXJET),ISORT(MAXJET)
      REAL PJET(MAXJET),JMOM
C
C  Jet clustering parameters; these are all default now!!!
C
      REAL YCUT,PCUT
      CHARACTER*8 CNAM
      DATA YCUT/0.01/
C
C  Inline functions
C
#include "qmacro.h"
C ------------------------------------------------------------------------------
C  Drop any existing 'tracks' of the requested name
C
      CALL QVDROP(CNAM,KRECO)
C
C  Try to use the DJET objects; if not present, use ENFL objects
C
      IF(KNJET.GT.0)THEN
        CALL QJOPTR('EJ',' ')
      ELSE
        CALL QJOPTR('EF',' ')
      END IF
C
C  Cluster
C
      CALL QJMMCL(MJET,CNAM,KRECO,YCUT,QELEP)
C
C  Get the ALPHA pointers to these new objects
C
      JTRK=KPDIR(CNAM,KRECO)
      DO IJET = 1 , MJET
        PJET(IJET)  = QP(JTRK)
        KJET(IJET) = JTRK
        JTRK = KFOLLO(JTRK)
      ENDDO
C
C  Sort the array of the jet momenta in descending order :
C
      IF(MJET.GT.0)CALL SORTZV ( PJET,ISORT,MJET,1,1,0 )
C
C  Fill array with 3-momenta of the jets : cut on the momentum
C
      NJET = 0
      DO IJET = 1 , MJET
        IF(PJET(ISORT(IJET)).GE.PCUT)THEN
          NJET = NJET + 1
          TRKJ(NJET) = KJET(ISORT(IJET))
          JTRK = TRKJ(IJET)
          JETS(1,NJET) = QX(JTRK)
          JETS(2,NJET) = QY(JTRK)
          JETS(3,NJET) = QZ(JTRK)
        ENDIF
      END DO
C
      RETURN
      END
#endif
