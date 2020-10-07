      SUBROUTINE CABOOK
C***********************************************************************
C! Book histograms for calorimeter process                             *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870831                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "crunp.h"
      NCALO=JULCA*1000
        IF(JCALPR.EQ.1.OR.JCALPR.EQ.2)THEN
           CALL HBOOK2(NCALO+101,'LES BONS X=CLUSTER ENERGY,Y=Pt$',
     +     30,0.,30.,20,0.,2.,0.)
           CALL HBOOK2(NCALO+102,'LES MAUVAIS X=CLUSTER ENERGY,Y=Pt$',
     +     30,0.,30.,20,0.,2.,0.)
           CALL HBOOK1(NCALO+103,'THE GOOD ONES X=DISTANCE/RMS$',
     +     100,0.,.5,0.)
           CALL HBOOK1(NCALO+120,'THDIS$',50,-0.05,0.05,0.)
           CALL HBOOK1(NCALO+121,'PHDIS$',50,-0.05,0.05,0.)
        ELSEIF (JCALPR.EQ.3) THEN
           CALL HTABLE(NCALO+103,
     +'HC CLUSTERS X=TRUE, Y=RECONS 1:NEUT 2:MIP 3:NOMIP (PER EVENT)$',
     + 3,0.5,3.5,9,0.5,9.5,0.)
        ENDIF
      RETURN
      END
#endif
