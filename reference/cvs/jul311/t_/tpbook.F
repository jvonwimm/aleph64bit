      SUBROUTINE TPBOOK
C
C----------------------------------------------------------------------
C! Book TPC histograms
C!
C!---------------------------------------------------------------------
#ifndef DOC
C
#include "rparac.h"
C
      PARAMETER (VX=0.0, JOFF=JULTP*1000)
C
      CALL HBOOK1(JOFF+1,'TPC number of MC hits per event$',
     &                40,0.,1000.,VX)
      CALL HBOOK1(JOFF+2,
     & 'TPC number of found coordinates per event$',40,0.,1000.,VX)
      CALL HBOOK1(JOFF+3,
     & 'TPC MC coordinate finding efficiency per event$',40,0.,1.0,VX)
      CALL HBOOK1(JOFF+4,
     & 'TPC r-phi coord. residuals from MC hits (cm)$',40,-.2,.2,VX)
      CALL HBOOK1(JOFF+8,
     & 'TPC r-phi coord. residual from MC hits (sigma)$',40,-5.,5.,VX)
      CALL HBOOK1(JOFF+5,
     & 'TPC z coord. residuals from MC hits (cm)$',40,-.4,.4,VX)
      CALL HBOOK1(JOFF+9,
     & 'TPC z coord. residual from MC hits (sigma)$',40,-5.,5.,VX)
      CALL HBOOK1(JOFF+6,
     & 'TPC fraction of unassociated coordinates$',40,0.,1.0,VX)
      CALL HBOOK1(JOFF+7,
     & 'TPC no. of available coordinates per mc track$',40,0.,40.,VX)
      CALL HBOOK1(JOFF+11,'TPC number of MC tracks in TPC per event$',
     &                  40,0.,120.,VX)
      CALL HBOOK1(JOFF+12,'TPC no of MC tracks per '//
     &   'event with more than 3 hits$',40,0.,120.,VX)
      CALL HBOOK1(JOFF+13,'TPC number of tracks found per event$',
     &     40,0.,80.,VX)
      CALL HBOOK1(JOFF+14,
     &  'TPC fraction of tracks found per event$',40,0.,1.2,VX)
      CALL HBOOK1(JOFF+15,'TPC chisq. probability for helix fit$',
     &     50,0.,1.,VX)
      CALL HBOOK1(JOFF+16,'TPC projected length of fitted tracks$',
     &    40,0.,200.,VX)
      CALL HBOOK1(JOFF+20,'TPC number of coordinates on 1st arc '//
     &    'per fitted track$',22,0.,22.,VX)
      CALL HBOOK1(JOFF+17,
     &  'TPC number of coordinates per fitted track$',40,0.,40.,VX)
      CALL HBOOK1(JOFF+18,
     &'TPC Number of MC tracks associated with fit track$',10,0.,10.,VX)
      CALL HBOOK1(JOFF+19,'TPC Chisqr of track minus MC (>5 hits)$',
     &                 30,0.,24.,VX)
      CALL HBOOK1(JOFF+10,'TPC Number of MC tracks per pad cluster$',
     &                 20,0.,20.,VX)
      CALL HBOOK1(JOFF+21,'Fraction of hits on fit track shared with'//
     &                    ' closest MC track',21,0.,1.1,VX)
      CALL HBOOK1(JOFF+22,'Fraction of hits on MC track shared with'//
     &                    ' closest fit track',21,0.,1.1,VX)
C
      DO 100 I=1,22
        CALL HIDOPT(JOFF+I,'ERRO')
        CALL HIDOPT(JOFF+I,'INTE')
        CALL HIDOPT(JOFF+I,'ROTA')
  100 CONTINUE
C
      RETURN
      END
#endif
