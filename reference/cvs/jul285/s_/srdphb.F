      SUBROUTINE SRDPHB
C----------------------------------------------------------------------
C! Phi correction due to magnetic field
C!
C!    Author:     H. Meinhard       05-Mar-1990
C     Modified:   J. Knobloch       03-Jul-1990
C!
C!    Input:      - FIELRC,ENERRC from COMMON /RCONDS/
C!
C!    Output:     - DPHBSR in COMMON /SRECPC/
C!
C!    Description
C!    ===========
C!    Calculate correction factor to azimuth angles due to the ALEPH
C!    magnetic field assuming beam energy for the electrons. Beam energy
C!    is taken from RCONDS
C!    In order to obtain the actual azimuth correction, DPHBSR must be
C!    multiplied with the actual z position.
C----------------------------------------------------------------------
#ifndef DOC
#include "rconds.h"
#include "srecpc.h"
C get the beam energy
      EBEAM = ENERRC/2
C calculate delta phi due to magnetic field
C note that FIELRC is in kGauss, not Tesla!
      DPHBSR = .0015 * (FIELRC/10.) / EBEAM
      END
#endif
