      SUBROUTINE SELBHA(IBHAEV)
C----------------------------------------------------------------------
CKEY EDIR ECAL BHABHA
C! Purpose   : select Bhabha events on ECAL only:
C!             - at least 2 non adjacent modules with
C!               E > smwcut (35 Gev) on the wires (each)
C!             - 2 Ecal clusters with E > scocut (35 Gev) and
C!               |cos(theta)|< qctcut (.95)
C-
C   Inputs    : None
C   Outputs   : IBHAEV  = Class 20 logical flag
C-
C   Called by   : SELEVT
C   Calls  : None
C   Input banks : PEWI,PECO
C-
C                            Author: Gilles de BOUARD       2-FEB-1990
C ---------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
C --
      DIMENSION EWMOD( 36), LSMOD( 36), IADJ( 6)
      LOGICAL IBHAEV,FIRST
      DATA FIRST / .TRUE. /
C --
#include "bmacro.h"
C --
C   Initialize cuts at first call
C --
      IF( FIRST) THEN
        FIRST = .FALSE.
C   Max. COS(THETA) of tracks/clusters
        QCTCUT = .95
C   Minimum energy for wires planes in KeV
        EWPMN  = 1500.
C   Cut on single Ecalobject energy
        SCOCUT = 35.
C   Cut on single module wire energy
        SMWCUT = 35.
        NAMPEW = NAMIND( 'PEWI')
        NAMPEC = NAMIND( 'PECO')
      ENDIF
C --
C   Reset output flag
C --
      IBHAEV  = .FALSE.
C --
C   First, look for 2 non adjacent fired modules (on the wires)
C --
C   Check if wire data bank is there
C --
      IND = IW( NAMPEW)
      IF ( IND.EQ.0) IND = IW(NAMIND('PWEI'))
      IF( IND .EQ. 0) GO TO 999
C --
C   Reset variables
C --
      NSMOD = 0
      DO 10 I = 1, 36
        EWMOD( I) = 0.
 10   CONTINUE
C --
      NWMOD = IW( IND + 1)
      NMOD  = IW( IND + 2)
C --
C   Check the number of words per module
C --
      IF( NWMOD .NE. 55 .AND. NWMOD .NE. 63) GO TO 999
C --
C   Check number of modules
C --
      IF( NMOD .LT. 1 .OR. NMOD .GT. 36) GO TO 999
C --
C   Loop over wire modules
C --
      DO 20 I = 0, NMOD - 1
        NUMOD = IW( IND + 3 + I*NWMOD)
C --
C   Check module number
C --
        IF( NUMOD .LT. 1 .OR. NUMOD .GT. 36) GO TO 999
C --
C   Loop over wire planes
C --
        DO 30 J = 2, 46
          IF( FLOAT(IW(IND+2+I*NWMOD+J)) .GT. EWPMN) THEN
            EWMOD( NUMOD) = EWMOD( NUMOD) +
     &            FLOAT( IW( IND + 2 + I*NWMOD + J))
          ENDIF
 30     CONTINUE
        EWMOD( NUMOD) = EWMOD( NUMOD)/ 1000000.
C --
C   Apply wire energy cut on single module
C --
        IF( EWMOD( NUMOD) .GT. SMWCUT) THEN
          NSMOD         = NSMOD + 1
          LSMOD( NSMOD) = NUMOD
        ENDIF
 20   CONTINUE
C --
C   Select 2 non adjacent modules
C --
      IF( NSMOD .LE. 1) GO TO 999
      DO 40 I = 1, NSMOD - 1
C --
C   List of modules adjacent to module I
C --
        IMOD = MOD( LSMOD( I) - 1, 12) + 1
        ISUB = (( LSMOD( I) - IMOD)/ 12) + 1
        IF( MOD( ISUB, 2) .EQ. 0) THEN
          NADJ = 6
        ELSE
          NADJ = 4
        ENDIF
        IADJ( 1) = MOD( IMOD, 12) + 1 + 12*( ISUB -1)
        IADJ( 2) = MOD( IMOD - 2, 12) + 1 + 12*( ISUB -1)
        IF( ISUB .EQ. 1) THEN
          IADJ( 3) = LSMOD( I) + 12
          IADJ( 4) = IADJ( 2)  + 12
        ELSEIF( ISUB .EQ. 3) THEN
          IADJ( 3) = LSMOD( I) - 12
          IADJ( 4) = IADJ( 2)  - 12
        ELSE
          IADJ( 3) = LSMOD( I) - 12
          IADJ( 4) = IADJ( 1)  - 12
          IADJ( 5) = LSMOD( I) + 12
          IADJ( 6) = IADJ( 1)  + 12
        ENDIF
C --
C   Loop on all other modules above threshold
C --
        DO 50 J = I + 1, NSMOD
C --
C   Search for adjacente : if true, go to next module
C --
          DO 60 K = 1, NADJ
            IF( LSMOD( J) .EQ. IADJ( K)) GO TO 50
   60     CONTINUE
C --
C   If not true, keep event
C --
          GO TO 70
   50   CONTINUE
   40 CONTINUE
      GO TO 999
C --
  70  CONTINUE
C --
C   Second, count Ecalobjects with energy > SCOCUT and COST < QCTCUT
C --
      NCOEN  = 0
C --
C   Check if Ecalobject bank is there
C --
      IND = IW( NAMPEC)
      IF( IND .EQ. 0) GO TO 999
C
      NWECO = IW( IND + 1)
      NECO  = IW( IND + 2)
      IND   = IND + 2
C --
C   Check the number of word/Ecalobject
C --
      IF( NWECO .NE. 10) GO TO 999
C
      DO 80 I = 1, NECO
        IF( RW( IND + 6) .GT. SCOCUT .AND.
     &    ABS( COS( RW( IND + 4))) .LT. QCTCUT) THEN
            NCOEN = NCOEN + 1
        ENDIF
        IND = IND + NWECO
   80 CONTINUE
C
      IF( NCOEN .NE. 2) GO TO 999
C
      IBHAEV = .TRUE.
C
  999 RETURN
      END
#endif
