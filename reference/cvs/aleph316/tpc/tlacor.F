      SUBROUTINE TLACOR( R,PHI,Z , RP,PHIP,ZP , MODE)
C-----------------------------------------------------------------------
C! Corrections for B-field distortions
C ====================================
CKEY   TPC,LASER,B-FIELD,DISTORTIONS
C
C Author : Michael Schmelling   / Mainz   15-Jan-1990
C Mod.   : Werner Wiedenmann 10-Sep-91
C          use TLACOR as driver for
C          1) phi independent laser corrections (done if
C             bank TLCP is present)
C          2) phi dependent laser corrections (done if
C             bank TLFC is present)
C
C Input:    : real*4    R        : TPC coordinate - radius  [cm]
C             real*4    PHI      : TPC coordinate - azimuth [rad]
C             real*4    Z        : TPC coordinate - Z       [cm]
C             integer*4 MODE     : radial correction mode
C
C                       MODE.EQ.0 => corrections based exclusively
C                                    on laser data
C                       MODE.NE.0 => for full compensation use radial
C                                    corrections based on the field map
C             bank TLCP/TLFC from data base
C
C
C Output:     real*4  RP,PHIP,ZP : corrected coordinates
C
C Common blocks       : BCS
C External references : AGETDB,JUNIDB,AOPDBS,TFCINI,TCOREC /ALEPHLIB
C
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
C
#include "evehjj.h"
C
      LOGICAL FIRST
      INTEGER AGETDB
C
      DATA FIRST/.TRUE./
      DATA MXPRT/2/
C
#include "bmacro.h"
C
C-----------------------------------------------------------------------
C
C      initialization
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        NAEVEH= NAMIND('EVEH')
        NATLCP= NAMIND('TLCP')
        NATLFC= NAMIND('TLFC')
        NATNLO= NAMIND('TNLO')
      ENDIF
C
      RP   = R
      PHIP = PHI
      ZP   = Z
C
      IF(R.LE.0.) THEN
         CALL ALTELL(
     &     ' TLACOR: TPC coordinate with radius .le. 0  => RETURN',
     &     0,'RETURN')
         RETURN
      ENDIF
C
C      check that we have the right constants for the current run
C
      KEVEH=IW(NAEVEH)
      IF(KEVEH.EQ.0) THEN
         CALL ALTELL(' TLACOR: event header bank missing; no run #',
     &              0,'RETURN')
         RETURN
      ENDIF
      KRUN=IW(KEVEH+JEVERN)
C
C     Don't make any correction for Monte Carlo
C
      IF (KRUN.LT.2001) THEN
         RETURN
      ENDIF
C
      IF(KRUN.NE.LRUN) THEN
         NPRT=0
         LRUN=KRUN
C
C++      Check if TNLO bank is not present. 
C++      In this case do old Laser corrections, otherwise use only 
C++      Fieldmap
C
         IF (IW(NATNLO).EQ.0) THEN 
C
C.......... get Laser coefficients for this run
C
            IIII =  AGETDB('TLCP',KRUN)
            IIPP =  AGETDB('TLFC',KRUN)
            IF ((IIII.EQ.0).AND.(IIPP.EQ.0)) THEN
                CALL ALTELL(
     &          ' TLACOR: '//
     &          'Bank TLCP and TLFC missing, '// 
     &          'cannot correct TPC coordinates',
     &          0,'RETURN')
                RETURN
            ENDIF
         ELSE
            CALL TFCINI(IW(6),JUNIDB(0),KRUN,IER)
         Endif 
      ENDIF
C
C+++  if TNLO is present use only field map
C
      IF (IW(NATNLO).NE.0) THEN
         CALL TCOREC(RP,PHIP,ZP)
      ELSE
C
C....... if TLFC is present call phi dependent corrections
C
         KTLFC = IW(NATLFC)
         IF (KTLFC.NE.0) THEN
            CALL TLACOP( R,PHI,Z , RP,PHIP,ZP , MODE)
         ELSE
            CALL TLACOC( R,PHI,Z , RP,PHIP,ZP , MODE)
         ENDIF
      Endif
C
      RETURN
      END
#endif
