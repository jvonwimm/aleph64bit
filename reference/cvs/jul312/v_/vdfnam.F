      SUBROUTINE  VDFNAM
C-----------------------------------------------------
C! defines namindex common VDNAMC
C!
C!    by Hans-Guenther Moser
C!
C!    created:           24/03/88
C!
C!    last modification: 24/03/88
C!
C!    -----------------------------------------
C!
C!
C!
C!
C!
C     =========================================
#ifndef DOC
C
#include "bcs.h"
#include "vdnamc.h"
#include "bosext.h"
#include "bmacro.h"
C
      NAVPLH = NAMIND('VPLH')
      NAVHLS = NAMIND('VHLS')
      NAVGTL = NAMIND('VGTL')
      NAVCPL = NAMIND('VCPL')
      NAVDCO = NAMIND('VDCO')
      NAVDXY = NAMIND('VDXY')
      NAVDZT = NAMIND('VDZT')
      NAVGCL = NAMIND('VGCL')
      NAVHOT = NAMIND('VHOT')
      NAVRCN = NAMIND('VRCN')
      NAVMRE = NAMIND('VMRE')
      NAVMPC = NAMIND('VMPC')
      NAVMGN = NAMIND('VMGN')
      NAVFPH = NAMIND('VFPH')
      RETURN
      END
#endif
