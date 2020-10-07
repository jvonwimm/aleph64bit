C ---------------------------------------------------------------------------
      subroutine TPSMR1(MAXHIT,ZVEC,RVEC)
C ---------------------------------------------------------------------------
C!  adds the smearing vector to the TPCO coordinates.
C!  takes into account the drift length dependence for z hits
C  Author  Manoj Thulasidas  Dec. 8, 1997
C
C  Input 
C            MAXHIT  = number of hits to be smeared  (21 here)
C            ZVEC    = smearing vector in Z
C            RVEC    = smearing vector in Z
C  Output
C            smeared TPCO bank
C
C  Modified:  Manoj Thulasidas  Feb. 15, 1998
C            Change the Z smearing proportional to the drift distance
C            to an average smearing.  (Fixes the cos(theta) dependence)
C ---------------------------------------------------------------------------
#ifndef DOC
      implicit none
C  globals
#include "bcs.h"
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  arguments
      integer MAXHIT
      real ZVEC(MAXHIT), RVEC(MAXHIT)
C  functions
      integer  NAMIND
      EXTERNAL NAMIND
C  locals
      real ZMAX
      parameter (ZMAX = 220.0)
      real RHIT, ZHIT, PHIT, RVAL
      integer NATPCO, KTPCO, NTPCO, ITPCO, IHIT
      logical FIRST
      data FIRST /.TRUE./
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  inline macros
C  declare the bmacro stuff
      INTEGER LCOLS, ID, LROWS, KNEXT, KROW, NRBOS, LFRWRD,
     $   LFRROW, ITABL, L
      REAL RTABL
#include "tpcojj.h"
#include "bmacro.h"
C============================================================================
      if (FIRST) then
        FIRST = .FALSE.
        NATPCO = NAMIND('TPCO')
      endif
      KTPCO = IW(NATPCO)
      NTPCO = LROWS(KTPCO)
      DO ITPCO = 1, NTPCO
        IHIT = ITABL(KTPCO, ITPCO, JTPCIN) / 100000
        RHIT = RTABL(KTPCO, ITPCO, JTPCRV)
        PHIT = RTABL(KTPCO, ITPCO, JTPCPH)
        ZHIT = RTABL(KTPCO, ITPCO, JTPCZV)
        RVAL = RHIT*PHIT
C  apply the given smearing
        RVAL = RVAL + RVEC(IHIT)
        PHIT = RVAL/RHIT
C  z smearing is zero when the drift distance is zero
C        ZHIT = ZHIT + (ZMAX-ABS(ZHIT))/ZMAX * ZVEC(IHIT)
        ZHIT = ZHIT + 0.5 * ZVEC(IHIT)
C  put them back in TPCO
        RW(KROW(KTPCO,ITPCO)+JTPCPH) = PHIT
        RW(KROW(KTPCO,ITPCO)+JTPCZV) = ZHIT
      ENDDO
      return
      end
#endif
