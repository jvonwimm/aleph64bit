      SUBROUTINE VGRDLS(LUN,IRUN,TEXP)
C----------------------------------------------------------------------
CKEY VDETDES ALIGN / INTERNAL
C!  Add to alignment expanded form TEXP the laser informations 
C - Giacomo Sguazzoni, 30 September 1998.
C
C   The output form is the same as the input but includes corrections
C   from laser data analisys
C
C - Input:
C   LUN      / I  Data base unit
C   IRUN     / I  Current run number
C
C - Input/Output:
C   TEXP(33) / R  Transformation expanded form
C
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vlgbjj.h"
#include "vgpaal.h"
#include "bcs.h"
C
      REAL    TEXP(*), TLAS(LVTEXP), TWOR(LVTEXP) 
      REAL    RTIME
      INTEGER I, IPAR, IROW, IOK, IFR, ILR, NPAR
      PARAMETER (NPAR=JVLGLC-JVLGIV)
      REAL    PLAS(NPAR)
      INTEGER LUN, IRUN, IRET, KVLGB, NVLGB, IFLAG
      DOUBLE PRECISION PSI, THETA, PHI, C1, C2, C3, S1, S2, S3
#include "bmacrod.h"
C
C     Parameters:
#include "vgvmcr.h"
#include "vgmacr.h"
#include "bmacro.h"
C
C ----------------------------------------------------------------------
C
C     Read LASER bank from the DAF:
C     
      KVLGB = MDARD(IW,LUN,'VLGB',0)
      IF (KVLGB.le.0) THEN
         GO TO 999
      ENDIF
      NVLGB = LROWS(KVLGB)
C
C     Look for the correct bank row
C
      IOK   = 0
      RTIME = 0
      DO IROW=1,NVLGB
         IFR = ITABL(KVLGB,IROW,JVLGFR)
         ILR = ITABL(KVLGB,IROW,JVLGLR)
         IF( IRUN.ge.IFR.and.IRUN.le.ILR ) THEN
            IOK = IROW
            RTIME = REAL(IRUN-IFR)
         ENDIF
      ENDDO
C
C     No good entry found
C
      IF ( IOK.eq.0 ) RETURN
C
C     Parameters actual values
C
      DO IPAR=1,NPAR
         PLAS(IPAR) = 
     &        RTABL(KVLGB,IOK,JVLGIV+IPAR-1) +
     &        RTABL(KVLGB,IOK,JVLGLC+IPAR-1)*RTIME +
     &        RTABL(KVLGB,IOK,JVLGQC+IPAR-1)*RTIME*RTIME
      ENDDO
C
C     Laser translation vector:
C
      DO I=0,2
         IPAR = I+1
         TLAS(JVTETR+I) = PLAS(IPAR)
      ENDDO
C
C     Laser global rotation:
C
      PSI   = PLAS(4)
      THETA = PLAS(5)
      PHI   = PLAS(6)
      C1 = COS(PSI)
      C2 = COS(THETA)
      C3 = COS(PHI+PSI)
      S1 = SIN(PSI)
      S2 = SIN(THETA)
      S3 = SIN(PHI+PSI)
C     
C     Lauber-Brownd convention.
C     
      TLAS( KVINDX(1,1)) =   C3*C2*C1 + S3*S1
      TLAS( KVINDX(1,2)) =   C3*S1*C2 - C1*S3
      TLAS( KVINDX(1,3)) = - S2*C3
      TLAS( KVINDX(2,1)) =   S3*C2*C1 - S1*C3
      TLAS( KVINDX(2,2)) =   S3*S1*C2 + C3*C1
      TLAS( KVINDX(2,3)) = - S3*S2
      TLAS( KVINDX(3,1)) =   S2*C1
      TLAS( KVINDX(3,2)) =   S1*S2
      TLAS( KVINDX(3,3)) =   C2
C     
C     Combine laser transformation and alignment transformation
C
      CALL VGCMTR (TEXP,TLAS,TWOR)
      CALL UCOPY  (TWOR,TEXP,LVTEXP)

 999  CONTINUE

      IFLAG = 0

      RETURN
      END
#endif





