*DK tobkhs
      SUBROUTINE TOBKHS(IHMIN)
C--------------------------------------------------------------------
C!  Initialize histograms for TPC simulation
C
C  Called from:  TSINIT
C  Calls:        HBOOK-library routines
C
C  Inputs:   PASSED:  --IHMIN, beginning index for histogram id's
C
C  Outputs:  None
C
C  Modifications :
C    1. P. Janot   26 May 1988  --Use HBOOK4-library routines
C--------------------------------------------------------------------
#include "hiscom.h"
      COMMON /PAWC/ HMEMOR(75000)
C
      CALL HLIMIT(75000)
C
      IHDEDX = IHMIN
C
      CALL HBOOK2(IHDEDX+1  ,' LOG GAMMA VS. POISSON AVERAGE',
     1            40,0.,4.,50,25.,50.,0.)
      CALL HBOOK1(IHDEDX+2  ,'LOG PRIMARY ENERGY', 50,-4.,1.,0.)
      CALL HIDOPT(IHDEDX+2,'LOGY')
      CALL HBOOK1(IHDEDX+3  ,'Z-LENGTH OF DELTA--cm', 50,-25.,25.,0.)
      CALL HBOOK1(IHDEDX+4  ,'RADIUS OF DELTA--cm', 50,0.,.5,0.)
      CALL HBOOK2(IHDEDX+5  ,'LOG MOMENTUM  VS. CLUSTERS/CM',
     1            50,0.,5.,50,0.,100.,0.)
      CALL HBOOK1(IHDEDX+6  ,'CLUSTER SIZE DISTRIBUTION',100,0.,100.,0.)
C
      IHTRAN = IHDEDX+6
C
      CALL HBOOK1(IHTRAN+1  ,'DRIFT TIME DIFFERENCE FOR NEXT EL--NS',
     1            50,-250.,250.,0.)
      CALL HBOOK1(IHTRAN+2  ,'X COORD VARIATION IN DRIFT--CM',
     1            50,-.1,.1,0.)
      CALL HBOOK1(IHTRAN+3  ,'Y COORD VARIATION IN DRIFT--CM',
     1            50,-.1,.1,0.)
      CALL HBOOK2(IHTRAN+4  ,'ExB SHIFT--CM VS DISTANCE FROM WIRE',
     1            50,-.25,.25,40,-.2,.2,0.)
C
      IHAVAL = IHTRAN+4
C
      CALL HBOOK1(IHAVAL+1  ,'LOG CHARGE PRODUCED IN AVALANCHE',
     1            50,0.,6.,0.)
C
      IHCOUP = IHAVAL+1
C
      CALL HBINSZ('YES ')
C
      CALL HBOOK1(IHCOUP+1  ,'NUMBER OF PADS AFFECTED/AVALANCHE',
     1            10,0.,10.,0.)
      CALL HBIGBI(IHCOUP+1,5)
C
      IHTRCP = IHCOUP + 1
C
      CALL HBOOK1(IHTRCP+1  ,'NUMBER OF T-PADS AFFECTED/AVALANCHE',
     1            2,0.,2.,0.)
      CALL HBIGBI(IHTRCP+1,5)
C
      IHBOS = IHTRCP + 1
C
      CALL HBOOK1(IHBOS+1  ,'NUMBER OF WIRES HIT/SECTOR',20,0.,200.,0.)
      CALL HBOOK1(IHBOS+2  ,'NUMBER OF PADS HIT/SECTOR',20,0.,300.,0.)
      CALL HBOOK1(IHBOS+3  ,'NUMBER OF T-PADS HIT/SECTOR',18,0.,36.,0.)
      CALL HBOOK1(IHBOS+4  ,'NHITS IN WIRE DIGIT BANK',20,1.,0.,0.)
      CALL HBOOK1(IHBOS+5  ,'NHITS IN PAD DIGIT BANK',20,1.,0.,0.)
      CALL HBOOK1(IHBOS+6  ,'NHITS IN T-PAD DIGIT BANK',20,1.,0.,0.)
      CALL HBOOK1(IHBOS+7  ,'NDIGS IN WIRE BANK',20,1.,0.,0.)
      CALL HBOOK1(IHBOS+8  ,'NDIGS IN PAD BANK',20,1.,0.,0.)
      CALL HBOOK1(IHBOS+9  ,'NDIGS IN T-PAD BANK',20,1.,0.,0.)
C
      DO 1 J = 1,9
         CALL HBIGBI(IHBOS+J,3)
         CALL HIDOPT(IHBOS+J,'1EVL')
         IF ( J .GE. 4 ) CALL HMINIM(IHBOS+J,0.)
 1    CONTINUE
C
      IHTOT = IHBOS + 9
C
 999  RETURN
      END
