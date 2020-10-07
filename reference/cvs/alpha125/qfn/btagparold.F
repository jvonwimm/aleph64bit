      SUBROUTINE BTAGPAR_OLD
CKEY   QIPBTAG / INTERNAL
C-----------------------------------------------------------------------
C! Define cuts for QIPBTAG
C  Called from BTAG_INIT
C
C  read the cuts used for the B-tagging and then store them
C  (old version of code).
C
C  Author  Dave Brown  29-1-93
C  MODIFIED:
C  ---------
C  03/05/95  Stephan Schael, L_SCUT CHANGED FROM 2.0/3.0 TO 1.3/1.3
C  07/11/95  Stephan Schael, data card to read in NUMJET
C  Modified Ian Tomalin 8-3-99: Some code moved to BTAG_INIT
C
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
#include "btpar.h"
C
C  Local variables
C
      INTEGER IND, IPAR, IFIT, ITYPE
      INTEGER NLINK,NAMIND
C
C  Duplicate local variables for putting track parameters into common
C
      INTEGER L_MINN(3),L_IDB,L_NUMJET,L_IMASK,L_NVIEW(MAXTYPE)
      REAL L_D0_CUT,L_Z0_CUT,L_MINMOM,L_MAXMOM
      REAL L_MAX_ERR,L_CHI_CUT
      REAL L_DOTCUT
      REAL L_MXDMIN(MAXTYPE),L_MXSDMIN(MAXTYPE)
      REAL L_LCUT(MAXTYPE),L_LSIGCUT(MAXTYPE),L_SCUT(MAXTYPE)
      REAL L_PCUT
C
      INTEGER L_NTYPE
      REAL L_FITP(NFIT,MAXTYPE)
C
      LOGICAL L_NEGPROB
C
C  Data statements; these are the default track cuts and calibrations
C
      DATA L_MINN/1,4,4/,L_NUMJET/3/,L_D0_CUT/0.5/,L_Z0_CUT/0.5/
      DATA L_MINMOM/0.4/,L_MAXMOM/25./,L_CHI_CUT/5.0/,L_MAX_ERR/0.2/
      DATA L_DOTCUT/0.7/
      DATA L_NEGPROB/.FALSE./
      DATA L_LCUT/10*0.075/,L_LSIGCUT/10*3.0/
      DATA L_PCUT/10./
C SSC DATA L_SCUT/5*2.0,5*3.0/
      DATA L_SCUT/5*1.3,5*1.3/
      DATA L_MXSDMIN/5*0.1,5*.15/
      DATA L_MXDMIN/10*0.25/
      DATA L_NVIEW/2,2,2,1,1,1,1,3*2/
      DATA L_FITP/
     &  0.297685,0.0448793,1.02773,0.949288,4.03181,
     &  0.290828,0.0123158,1.01028,0.802609,3.89172,
     &  0.492502,0.0473466,0.908849,0.783833,2.31709,
     &  0.0366302,0.792102,1.28123,8.31592,2.46887,
     &  0.123738,0.320235,1.03392,6.53681,2.33844,
     &  0.237125,0.228661,1.22802,6.45101,2.00113,
     &  0.259365,0.,1.02543,1.35904,1.,
     &  0.190106,0.,1.02786,2.17761,1.,
     &  0.190106,0.,1.02786,2.17761,1.,
     &  0.190106,0.,1.02786,2.17761,1./
      DATA L_NTYPE/7/
C
C  Inlines
C
#include "qmacro.h"
C-----------------------------------------------------------------------
C
C  Load default parameters into common
C
      DO IPAR=1,3
        MINN(IPAR) = L_MINN(IPAR)
      END DO
      DO ITYPE=1,MAXTYPE
        LCUT(ITYPE) = L_LCUT(ITYPE)
        LSIGCUT(ITYPE) = L_LSIGCUT(ITYPE)
        SCUT(ITYPE) = L_SCUT(ITYPE)
        MXSDMIN(ITYPE) = L_MXSDMIN(ITYPE)
        MXDMIN(ITYPE) = L_MXDMIN(ITYPE)
        USRNAME(ITYPE) = 'AAAAbbbb'
        NVIEW(ITYPE) = L_NVIEW(ITYPE)
      END DO
      DOTCUT = L_DOTCUT
      NUMJET = L_NUMJET
      D0_CUT = L_D0_CUT
      Z0_CUT = L_Z0_CUT
      MINMOM = L_MINMOM
      MAXMOM = L_MAXMOM
      CHI_CUT = L_CHI_CUT
      PCUT    = L_PCUT
      MAX_ERR = L_MAX_ERR
      NTYPE = L_NTYPE
      INTYPE = NTYPE
      NEGPROB = L_NEGPROB
      DO IFIT=1,NFIT
        DO ITYPE=1,MAXTYPE
          FITP(IFIT,ITYPE) = L_FITP(IFIT,ITYPE)
        END DO
      END DO
C
C  Get calibration from card file if available.
C
      IND = NLINK('FITP',1)
      IF(IND .GT. 0)THEN
        WRITE (IW(6),*)'QIPBTAG: reading calibration from card FITP'
        NTYPE = LROWS(IND)
        DO ITYPE=1,NTYPE
          DO IFIT=1,NFIT
            FITP(IFIT,ITYPE) = RTABL(IND,ITYPE,IFIT)
          END DO
        END DO
      END IF
      END
#endif

