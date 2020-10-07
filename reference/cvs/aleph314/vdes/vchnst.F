      INTEGER FUNCTION VCHNST(IFLBE,JMOD,IWAF,IVIEW,IROS,IDAT,IFLC)
C----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!     Returns data channel corresponding to strip for given wafer, module
C  -
C
C   Author   :- J. Rothberg                   12-OCT-1995
C
C   Inputs:
C   IFLBE   / I       0 = nominal;  1 = Bonding errors used
C   JMOD    / I       Global Module number
C   IWAF    / I       Local Wafer number
C   IVIEW   / I       View
C   IROS    / I       Strip number in given wafer (1,2,3...)
C
C   Outputs:
C   IDAT    / I       Data Channel (CERN convention: 0,1,...)
C   IFLC    / I       Fault code (same as VMBE convention)
C
C   Libraries required:
C
C   Description
C   ===========
C     Returns data channel corresponding to strip for given wafer, module
C           Bonding errors are taken into account.
C
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
C ----------------------------------------------------------------------
#include "vglobl.h"
#include "vgindx.h"
#include "velchn.h"
#include "vduefc.h"
C ---------------------------------------------------------------------
C Arguments
       INTEGER IFLBE, IVIEW, JMOD, IWAF, IROS, IDAT, IFLC
C Functions
       INTEGER VMMODJ, VNRWAF, VRSWMD, VROSTM
C Local Variables
       INTEGER I, IWFRS(3), ISTRS(3)
       INTEGER IRET, IV, MMOD
       INTEGER NROSTR, IFREQ
       REAL RPITCH
C
       INTEGER IDA, IFCH, ILCH
       INTEGER ISFND
C
       INTEGER JMODLAST/0/
       SAVE JMODLAST
C -----------------------------------------------------------------
       VCHNST = VDERR
       IF(JMOD .LE. 0 .OR. JMOD .GT. NMODUL) GOTO 999
       IF(IVIEW .LE. 0 .OR. IVIEW .GT. 2) GOTO 999
       IF(IWAF .LE. 0 .OR. IWAF .GT. VNRWAF() ) GOTO 999
       IRET =  VROSTM(IVIEW,NROSTR,RPITCH,IFREQ)
       IF(IROS .LE. 0 .OR. IROS .GT. NROSTR ) GOTO 999
C
C first find data channel for nominal wafers, strips
            MMOD = VMMODJ(JMOD)
            IDAT = -1
            IFLC = 0
            IRET = VRSWMD(IROS,IWAF,MMOD,IVIEW,IDAT)
C
C  IF VIGBM =1 IGNORE BOND MAPS
C for nominal wafers, strips
       IF(IFLBE .EQ. 0 .OR. VIGBM .EQ. 1) THEN
            VCHNST = IRET
C
C bonding errors required
       ELSEIF(IFLBE .EQ. 1 .AND. VIGBM .EQ. 0) THEN
C new module requested, initialize tables
         IF(JMOD .NE. JMODLAST) THEN
            CALL VINIST
            DO IV=1,2
               CALL VCORMP(IV,JMOD)
            ENDDO
            JMODLAST = JMOD
         ENDIF
C search table for strip number
         IF(IVIEW .EQ. vviewz) THEN
            IFCH = MAX(1,IDAT-4)
            ILCH = MIN(IDAT+4,1023)
            IDAT = -1
            IFLC = -1
            VCHNST = VDOK
            DO IDA = IFCH,ILCH
              ISFND = IELCHZ(IDA,IWAF)
              IFLC = IELCHZ(IDA,IWAF+3)
              IF (ISFND .EQ. IROS )THEN
                 IDAT = IDA
                 GOTO 999
              ENDIF
            ENDDO
C
         ELSEIF(IVIEW .EQ. vviewp) THEN
            IFCH = MAX(1,IDAT-4)
            ILCH = MIN(IDAT+4,1023)
            IDAT = -1
            IFLC = -1
            VCHNST = VDOK
            DO IDA = IFCH,ILCH
              ISFND = IELCHP(IDA,IWAF)
              IFLC = IELCHP(IDA,IWAF+3)
C correct for readout direction
              IF (ISFND .EQ. 1022-IROS )THEN
                 IDAT = IDA
                 GOTO 999
              ENDIF
            ENDDO
C
         ENDIF
C
       ENDIF
C -------------------------------------------------------------------
  999 RETURN
      END
#endif
