      SUBROUTINE EINIRU
C***********************************************************************
C! Initialise run for ecal                                             *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  871023                                    *
C!   Modified :- E. Lancon             17-MAR-1992
C!        add call to EGACST
C!   Modified :- M.Cattaneo            06-NOV-1997
C!        Remove 'Temporary' (since before 1993...) call to EFIX
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "rconds.h"
#include "ectcom.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "bcs.h"
      INTEGER ALGTDB, GTSTUP
      EXTERNAL ALGTDB, GTSTUP
      CHARACTER DET*2, LIST*8
      DATA IROLD / 0 /
C
      FILDEC=FIELRC
C
C?  initialize magnetic field
C
      FILDEC=FIELRC
C
C? Definition of constant for clustering
C
      CALL ECBLDA
C
C Read ECAL geometry bank from DAF
C and fill ECAL internal common blocks
C
      CALL ERDDAF(LRCONS,IRUNRC,IERR)
      IF (IERR.EQ.0) GO TO 998
C
C?  Expands ECAL geometry
C
      CALL ECDFRD
C
C?  Initialize neutral showers parameters
C
      CALL ECPASR
C
C?  Compute the stack width in radiation length units
C
      CALL ECLGEO
C
C? Gives the positions of sensitive plans
C
      CALL EFIECP
C
C     Get constant for small ECAL defect correction
C
      CALL ESCDEF
C
C?  Get dead storey information from DBASE or Run Header
C
      CALL EGETDS
C
C?  Read platine gains from data base if not on run header
C
      KAJOB = IW(NAMIND('AJOB'))
      KECMC = IW( NAMIND('ECMC') )
      IF ( KECMC.EQ.0 ) THEN
        IREADB = ALGTDB(LRCONS,'ECMC',IRUNRC)
        IF ( IREADB.EQ.0 .AND. KAJOB.EQ.0) THEN
          CALL RERROR('EINIRU',-4,'No ECMC bank found for this run')
        ELSE
          WRITE (LOUTRL,'(A)') '    ECMC taken from DataBase '
        ENDIF
      ELSE
        WRITE (LOUTRL,'(A)') '    ECMC taken from Run Header '
      ENDIF
C
C
C?  Get Correction factor for gas flow
C?   (Theta dependance of absolute calibration in barrel modules)
C
      CALL EGFCST
C
C?  Get E-flow constants from Dbase
C
      CALL EFLCST
C
C?   Get constants for photon estimators bank
C?   (EGTE EF4N EW1N EW2N EL1N EL2N)
C
      CALL EGACST(IER)
      IF ( IER.NE.0 ) THEN
        CALL RERROR('EINIRU',-5,
     &            'No photon estimators coeff. found for this run')
      ENDIF
C
C?  Get wire gain for dbase
C
      LWIR =ALGTDB(LRCONS,'EWGA',IRUNRC)
      CALL ERCUPI(IRUNRC)
C
C?  Get gain calibration constants :  ECCA bank
C?  Get Leakage correction banks
C
      DET ='EC'
      LIST ='ECILECCA'
#include "gtdbbk.h"
      KECCA = IW(NAMIND('ECCA'))
      KECIL = IW(NAMIND('ECIL'))
C
      IF (KECCA.EQ.0)
     &  CALL RERROR('EINIRU',-1,'No ECCA bank found for this run')
C
      IF (KECIL.EQ.0)
     &  CALL RERROR('EINIRU',2,'No ECIL bank found for this run')
C
      KECLK  = MDARD(IW,LRCONS,'ECLK',0)
      IF (KECLK.EQ.0)
     &  CALL RERROR('EINIRU',-2,'No ECLK bank found for this run')
C
C     Built ECHE banks = calibration banks used in reconstruction
C
      CALL ECHEDC
C
C   Get constants for t0 calculations EFIP and init wire analysis
C
      CALL EFPARD
C
      RETURN
C
  998 WRITE(LOUTRL,100)
  100 FORMAT(' EINIRU : Incorrect or missing ECAL bank on DAF - STOP')
      END
#endif
