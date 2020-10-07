      SUBROUTINE BTAGPAR_NEW1
CKEY   QIPBTAG / INTERNAL
C-----------------------------------------------------------------------
C! Define cuts for QIPBTAG
C  Called from BTAG_INIT
C
C  read the cuts used for the B-tagging and then store them
C  (new version of code, with track selection cuts for old VDET).
C
C  Author  Dave Brown  29-1-93
C  MODIFIED:
C  ---------
C  03/05/95  Stephan Schael, L_SCUT CHANGED FROM 2.0/3.0 TO 1.3/1.3
C  07/11/95  Stephan Schael, data card to read in NUMJET
C  Modified  Manoj Thulasidas  Dec. 8, 1997
C      Read in the calibration from QIPC - database bank
C  Modified Ian Tomalin 8-3-99: Some code moved to BTAG_INIT
C  Modified P.-F. Giraud 17-8-99: Added option to gtipbk
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
      INTEGER IND, IPAR, IFIT, ITYPE, JULVER
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
      DATA L_NEGPROB/.TRUE./
      DATA L_LCUT/10*0.075/,L_LSIGCUT/10*3.0/
      DATA L_PCUT/10./
      DATA L_SCUT/5*1.3,5*1.3/
      DATA L_MXSDMIN/4*0.1,2*.15,4*0.1/
      DATA L_MXDMIN/10*0.25/
      DATA L_NVIEW/4*2,6*1/

C     CHANGE CUTS AND CAL  FOR NEW ITYPE   ( JC 3 SEPT 1996)
C
C     NUMJET=4  (was 3)
C     MAXMOM=40 (was 25)
C     CHI_CUT=6.0 (was 5.0)
C     NEGPROB=.TRUE. (was .FALSE.) ie NEGPROB by default
C                    introduce NOBG to reverse
C     PCUT=1.0 (was 10.0)
C     ITYPE is changed in TRSEL so order of remaining cuts:
C     LCUT, LSIGCUT, SCUT, MXSDMIN, MXDMIN are changed as are values
C     NVIEW changed to match
C      DATA L_MINN/1,4,4/,L_NUMJET/4/,L_D0_CUT/0.5/,L_Z0_CUT/0.5/
C      DATA L_MINMOM/0.4/,L_MAXMOM/40./,L_CHI_CUT/6.0/,L_MAX_ERR/0.2/
C      DATA L_DOTCUT/0.7/
C      DATA L_NEGPROB/.TRUE./
C      DATA L_PCUT/1./
C      DATA L_LCUT   /3*0.100, 0.065,0.075,5*0.065/
C      DATA L_LSIGCUT/3*15.0,7*2.5/
C      DATA L_SCUT   /3*1.5,1.0,1.3,5*1.0/
C      DATA L_MXDMIN /3*0.50,2*0.25,0.0,0.10,3*0.0/
C      DATA L_MXSDMIN/10*0.2/
C      DATA L_NVIEW/4*2,6*1/
C    PRODUCE FROM CUTS46
      DATA L_FITP/
     &   0.374238 , 0.0173856  , 0.95959  , 0.87885  , 3.91877 ,
     &   0.363417 , 0.0564959  , 0.994481 , 0.866839 , 3.24227 ,
     &   0.42203  , 0.         , 0.774524 , 0.942774 , 1.      ,
     &   0.290745 , 0.0383768  , 0.993929 , 0.800101 , 3.13527 ,
     &   0.29424  , 742.576E-6 , 0.917707 , 0.971486 , 7.0156  ,
     &   0.529427 , 0.         , 0.940015 , 2.46945  , 1.      ,
     &   0.96987  , 0.         , 5.56486  , 1.03572  , 1.      ,
     &   0.410428 , 0.         , 1.02536  , 1.94641  , 1.      ,
     &   0.82021  , 0.         , 1.33027  , 2.06544  , 1.      ,
     &   0.       , 0.         , 9.       , 9.       , 9.      /
C
      DATA L_NTYPE/9/
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
C  Get calibration from database bank if available.
C  (Depends on whether data/MC processed before/after tracking upgrade)
C 
      CALL GETJUV(JULVER)
      IF (JULVER.GE.300) THEN
        CALL GTIPBK(NFIT,FITP,'QIPC','FITP', 0)
      ELSE
C  Noone ever bothered calibrating the old data with Carr options.
C       CALL GTIPBK(NFIT,FITP,'QIOC','FITP', 0)
        CALL GTIPBK(NFIT,FITP,'QIPC','FITP', 0)
      END IF
      END
#endif

