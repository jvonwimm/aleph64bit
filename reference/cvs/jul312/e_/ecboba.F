      SUBROUTINE ECBOBA
C----------------------------------------------------------------------
C!  - Book Gammas ecal histograms
C!
C!   Called if histogramm flag = 4
C!   Histo. number 4400 to 4499
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
C
      NECAL = JULEC * 1000 + 400
           CALL HBOOK1(NECAL + 1 ,'ECAL : Hypothesis type$'
     +       ,4 , .5 , 256.5 , 0. )
           CALL HBOOK1(NECAL + 2 ,'ECAL : Hypothesis type$'
     +       , 64 , -.5 , 63.5 , 0. )
           CALL HBOOK1(NECAL + 11 ,'ECAL : TOTAL TOWERS SIGNAL$'
     +       ,28 ,1.,15.,0.)
           CALL HBOOK1(NECAL + 12 ,'ECAL : TOTAL TOWERS SIGNAL$'
     +       ,60 ,0.,12.,0.)
           CALL HBOOK2(NECAL + 13 ,'ECAL : STACK 1 | STACK3$'
     +       ,60,0., 6.,60,0.,6.,0.)
           CALL HBPRO(NECAL + 13 , 0.)
           CALL HBOOK1(NECAL + 14 ,'ECAL : STACK 2$'
     +       ,100,0.,10.,0.)
           CALL HBOOK1(NECAL + 15 ,'ECAL : Energy ratio hyp2/hyp1$'
     +       ,50,1.,2.,0.)
           CALL HBOOK1(NECAL+ 21 ,'ECAL : MATERIALISATION DEPTH PROBA.$'
     +       ,20, 0.,1.,0.)
           CALL HBOOK2(NECAL + 22 ,
     +       'ECAL : MAT. DEPTH PROBA. | LONGITUDINAL SHAPE PROBA.$'
     +       ,20, 0.,1.,20,0.,1.,0.)
           CALL HBPRO(NECAL + 22 , 0.)
           CALL HBOOK1(NECAL + 31 ,
     +       'ECAL : SIGNAL FRACTION INTO 4 CENTRAL TOWERS$'
     +       ,50,.0,1.,0.)
           CALL HBOOK1(NECAL + 32 ,
     +       'ECAL : Pi0 ident. from  #4 Tower$'
     +       ,60,-3.,3.,0.)
      RETURN
      END
#endif
