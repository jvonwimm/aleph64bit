      SUBROUTINE MUBOOK
C--------------------------------------------------------------
C! booking of muon detector histograms
C! G.Capon               20-oct-1987
C?
C!======================================================================
#ifndef DOC
C
#include "rparac.h"
#include "rflags.h"
      CALL HBOOK1(8001,'MUON- THETA OF MUON HITS   >>$',64,0.,3.2,0.)
      CALL HBOOK1(8002,'MUON- PHI  OF MUON HITS    >>$',64,0.,6.4,0.)
      CALL HBOOK1(8003,'MUON- SLOT NUMBER          >>$',100,0.,100.,0.)
      CALL HBOOK1(8004,'MUON- (MUON HITS)/EVENT    >>$',60,0.,60.,0.)
      CALL HBOOK1(8005,'MUON- NORM MULT SCATT DIST >>$',50,0., 5.,0.)
      CALL HBOOK1(8006,'MUON- NORM MULT SCATT ANGLE>>$',50,0., 5.,0.)
      CALL HBOOK1(8007,'MUON- IMPACT DIST INT LAYER>>$',50,0.,50.,0.)
      CALL HBOOK1(8008,'MUON- IMPACT DIST EXT LAYER>>$',50,0.,50.,0.)
      CALL HBOOK1(8009,'MUON-EXPECTED IMPACT INT LAYER>>$',50,0.,50.,0.)
      CALL HBOOK1(8010,'MUON-EXPECTED IMPACT EXT LAYER>>$',50,0.,50.,0.)
      CALL HBOOK1(8011,'MUON- P-EXIT/P-INITIAL     >>$',60,0.,1.2,0.)
      CALL HBOOK1(8012,'MUON-PROB CHI2 IMPACT INT LAYER>>$',40,0.,2.,0.)
      CALL HBOOK1(8013,'MUON-PROB CHI2 IMPACT EXT LAYER>>$',40,0.,2.,0.)
      CALL HBOOK1(8014,'MUON- PROB CHI2 EXIT ANGLE>>$',40,0.,2.,0.)
      CALL HBOOK1(8016,'MUON- EXIT ANGLE            >>$',50,0.,0.2,0.)
      CALL HBOOK1(8017,'MUON- EXPECTED EXIT ANGLE     >>$',50,0.,0.2,0.)
C?           histo for comparison with MonteCarlo
      IF (JHISRF(JULMU).GE.2) THEN
         CALL HBOOK1(8101,'MUON- DPHI >>          $',80,-0.08,0.08,0.)
         CALL HBOOK1(8102,'MUON- DTHET >>         $',80,-0.08,0.08,0.)
         CALL HBOOK1(8111,'MUON- DX ENDCAP >>     $',80,-10.,10.,0.)
         CALL HBOOK1(8112,'MUON- RDPHI MID. ANG.>>$',80,-10.,10.,0.)
         CALL HBOOK1(8113,'MUON- RDPHI BARREL >>  $',80,-10.,10.,0.)
         CALL HBOOK1(8121,'MUON- DY ENDCAP >>     $',80,-10.,10.,0.)
         CALL HBOOK1(8122,'MUON- DZ MID. ANG.>>   $',80,-10.,10.,0.)
         CALL HBOOK1(8123,'MUON- DZ BARREL >>     $',80,-10.,10.,0.)
         CALL HBOOK1(8199,'MUON- X CLUSTER SIZE >>$',20,0.,20.,0.)
         CALL HBOOK1(8200,'MUON- Y CLUSTER SIZE >>$',20,0.,20.,0.)
         CALL HBOOK2
     +(8201,'MUON- DX V EC-SLOT     $',80,-10.,10.,17,0.,17.,0.)
         CALL HBOOK2
     +(8211,'MUON- DY V EC-SLOT     $',80,-10.,10.,17,0.,17.,0.)
         CALL HBOOK2
     +(8202,'MUON- RDPHI V MA-SLOT  $',80,-10.,10.,39,0.,39.,0.)
         CALL HBOOK2
     +(8212,'MUON- DZ V MA-SLOT     $',80,-10.,10.,39,0.,39.,0.)
         CALL HBOOK2
     +(8203,'MUON- RDPHI V BA-SLOT  $',80,-10.,10.,35,0.,35.,0.)
         CALL HBOOK2
     +(8213,'MUON- DZ V BA-SLOT     $',80,-10.,10.,35,0.,35.,0.)
         CALL HIDOPT(0,'BLAC')
      ENDIF
C
      RETURN
      END
#endif
