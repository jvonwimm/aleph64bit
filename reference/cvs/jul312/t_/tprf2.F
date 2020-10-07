       REAL FUNCTION TPRF2(Z,ISECT,NSUB,PADCR,WIRCR)
C
C--------------------------------------------------------------------
C! Get square of pad response
C!
C!     Author:    D. Schlatter
C!     Modified:  R. Johnson    06-06-89
C!     Modified:  A. Roth       14-12-89
C!
C!     Input:    Z       /R      Length of drift in cm.
C!               ISECT   /I      sector number
C!               NSUB    /I      Number of subpulses (0 not to make
C!                               corrections based on track angles)
C!               PADCRO  /R      Pad crossing angle
C!               WIRCRO  /R      Wire crossing angle
C!
C!     Called by TCOALG
C!
C!     Description
C!     ===========
C!     The square of the PRF as function of z is obtained
C!     by interpolation of measurements made at several drift
C!     lengths.
C!
C!     At each start of run, TPRFIN must be called for initialization.
C!
C!-------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "tpgpar.h"
#include "tpccon.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tprzdp.h"
#include "alcons.h"
C
      LOGICAL LFRST,LMC
      DATA LFRST/.TRUE./
C
C++   Check whether we are reading a Monte Carlo tape
C
      IF (LFRST) THEN
        LFRST=.FALSE.
C
        NTSIM=NAMIND('TSIM')
        KTSIM=IW(NTSIM)
        LMC=KTSIM.NE.0
      ENDIF
C
C++   Calculate square of PRF at given Z
C
      IF (LMC) THEN
C
C++     Use parametrization for MC
C
        BFF=ABS(BFIELD)
        TPRF2=TPRWDT**2 + ((TSIGMD**2)*Z)/(4.0 + (TOMTAU*BFF)**2)
        TPRF2=TPRF2/TPDSEP(ITPTYP(ISECT))**2
      ELSE
C
C++     Use measurements for data
C
        DO 83 I=2,NZVLDP-1
          IF (Z.LT.ZZVLDP(I)) THEN
            TPRF2= Z*TPRFA1(I)+TPRFQ1(I)
            GO TO 84
          ENDIF
   83   CONTINUE
        TPRF2= Z*TPRFA1(NZVLDP)+TPRFQ1(NZVLDP)
   84   CONTINUE
      ENDIF
C
C--   In case that there is already a track (NSUB .ne. 0)
C--   get the correction for the pad response function
C
      IF (NSUB .NE. 0) THEN
        CALL TPRFX (NSUB, PADCR, WIRCR, PRFCOR)
        TPRF2  =  TPRF2*PRFCOR
      END IF
C
      RETURN
      END
#endif
