      SUBROUTINE LCBOOK
C----------------------------------------------------------------------
C!   Book Lcal histograms
C!
C!   Author   : P. H. Hansen  880315
C!   Modified : P. H. Hansen  890102
C!   Modified : P. H. Hansen  960501
C!   Description :
C!   ===========
C!=====================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "bcs.h"
#include "bmacro.h"
C----------------------------------------------------------------------
C
      LHID = JULLC * 1000
C
C Histogram events with Lcal clusters for general monitoring
      CALL HBOOK1(LHID + 1,'LCAL- Pad energy$',100,0.,200.,0.)
      CALL HBOOK1(LHID + 2,'LCAL- Cluster energies$',100,0.,200.,0.)
      CALL HBOOK1(LHID + 3,'LCAL- Wire energy$',100,0.,200.,0.)
      CALL HBOOK1(LHID + 4,'LCAL- Storey1/Etot$',50,0.,1.,0.)
      CALL HBOOK1(LHID + 5,'LCAL- Storey3/Etot$',50,0.,1.,0.)
      CALL HBOOK1(LHID + 6,
     &           'LCAL- Phi singles A Side$',90,0.,360.,0.)
      CALL HBOOK1(LHID + 7,
     &           'LCAL- Phi singles B Side$',90,0.,360.,0.)
      CALL HBOOK1(LHID + 8,'LCAL- Energy singles$',50,0.,100.,0.)
      CALL HBOOK1(LHID + 9,
     &            'LCAL- Theta singles (mrad)$',50,35.,135.,0.)
C
C Histogram Bhabha selected events
      CALL HBOOK1(LHID + 11,'LCAL-e+e-  Pad energy m1$',
     *  100,0.,100.,0.)
      CALL HBOOK1(LHID + 12,'LCAL-e+e- Pad energy m2$',
     *  100,0.,100.,0.)
      CALL HBOOK1(LHID + 13,'LCAL-e+e- Pad energy m3$',
     *  100,0.,100.,0.)
      CALL HBOOK1(LHID + 14,'LCAL-e+e- Pad energy m4$',
     *  100,0.,100.,0.)
      CALL HBOOK1(LHID + 15,'LCAL-e+e- Energy pr 0.5 rl m1$',
     *   47,1.25,24.75,0.)
      CALL HBOOK1(LHID + 16,'LCAL-e+e- Energy pr 0.5 rl m2$',
     *   47,1.25,24.75,0.)
      CALL HBOOK1(LHID + 17,'LCAL-e+e- Energy pr 0.5 rl m3$',
     *   47,1.25,24.75,0.)
      CALL HBOOK1(LHID + 18,'LCAL-e+e- Energy pr 0.5 rl m4$',
     *   47,1.25,24.75,0.)
      CALL HBOOK1(LHID + 31,'LCAL average X$',50,-5.,5.,0.)
      CALL HBOOK1(LHID + 32,'LCAL average Y$',50,-5.,5.,0.)
      LH = LHID + 40
      CALL HBOOK1(LH+1,'LCAL-Bhabha E1$'
     *  ,100,0.,100.,0.)
      CALL HBOOK1(LH+2,'LCAL-Bhabha E2$'
     *  ,100,0.,100.,0.)
      CALL HBOOK1(LH+3,'LCAL-Bhabha Dphi$',
     *   90,0.,180.,0.)
      CALL HBOOK2(LH+4,'LCAL-Bhabha Y vs X$'
     *   ,25,0.,25.,25,0.,25.,0.)
  999 CONTINUE
      END
#endif
