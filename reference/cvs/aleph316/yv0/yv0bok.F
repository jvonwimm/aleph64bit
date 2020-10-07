      SUBROUTINE YV0BOK(JOFF)
C------------------------------------------------------------------------
C! Histogram for V0
CKEY YV0 HIST /INTERNAL
C AUTHOR:
C         M.A.Ciocci 20/2/90
C Modified:  J.Sedgbeer 91/04/09  Add histos for new cuts.
C
C        INPUT:
C               JOFF/INTEGER    histograms for V0 will start from joff+200
C        OUTPUT:
C                NONE
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
      CALL HBOOK1(JOFF+200,
     $ 'Chi square track-to-prim. vertex before O1 cut',100,0.,100.,0.)
      CALL HBOOK1(JOFF+201,
     $ 'Sum Chi square TRACK-1 TRACK-2 to primary vx. after O1 cut'
     $                ,100,0.,100.,0.)
      CALL HBOOK1(JOFF+202,
     $ 'Distance between helices at approx vx. after O2 cut'
     $                ,100,0.,100.,0.)
      CALL HBOOK1(JOFF+203,'PSII PSIJ angles after CT cut'
     $                ,100,-5.,5.,0.)
      CALL HBOOK1(JOFF+204,'Vertex radius after PS cut'
     $                ,100,0.,200.,0.)
      CALL HBOOK1(JOFF+205,'ABS(Z) vertex after RV cut'
     $                ,100,0.,200.,0.)
      CALL HBOOK1(JOFF+206,
     $ 'Difference between tangent of DIP after ZV cut',100,0.,1.,0.)
      CALL HBOOK1(JOFF+207,
     $ 'V0 vertex fit Chi square first hypothesis after ZV cut'
     $                ,100,0.,100.,0.)
      CALL HBOOK1(JOFF+208,
     $ 'V0 vertex fit Chi Square second hypothesis after ZV cut'
     $                ,100,0.,1000.,0.)
      CALL HBOOK1(JOFF+209,'No. of coords before vertex after CF cut'
     $                ,35,0.,35.,0.)
      CALL HBOOK1(JOFF+210,'Cosine vertex-momentum after NC cut'
     $                ,100,-1.,1.,0.)
      CALL HBOOK1(JOFF+211,'V0 d0 after CS cut',100,0.,50.,0.)
      CALL HBOOK1(JOFF+212,'V0 Z0 after DZ cut',100,0.,100.,0.)
      CALL HBOOK1(JOFF+213,'Distance prim-to-decay after ZZ cut'
     $                ,100,0.,20.,0.)
      CALL HBOOK1(JOFF+214,'Distance prim-to-decay(in xy) after ZZ cut'
     $                ,100,0.,20.,0.)
      CALL HBOOK1(JOFF+215,
     $ 'Chi square to primary vertex after MD cut',100,0.,100.,0.)
C
      CALL HBOOK1(JOFF+216,'Q value in K0 hypothesis after CO cut'
     $                ,100,-0.4,0.4,0.)
      CALL HBOOK1(JOFF+217,'Chi square in K0 hypothesis after CO cut'
     $                ,100,0.,100.,0.)
      CALL HBOOK1(JOFF+218,'Q value in Lam/AntiLam hypoth. after CO cut'
     $                ,50,-0.04,0.04,0.)
      CALL HBOOK1(JOFF+219,'Chisquare in Lam/AntiL hypoth. after CO cut'
     $                ,100,0.,50.,0.)
      CALL HBOOK1(JOFF+220,'Q value in Gamma hypoth. after CO cut'
     $                ,100,0.,0.01,0.)
      CALL HBOOK1(JOFF+221,'Chi square in Gamma hypoth. after CO cut'
     $                ,100,0.,100.,0.)
      CALL HBOOK1(JOFF+222,'Inv. mass K0',100,0.,1.,0.)
      CALL HBOOK1(JOFF+223,'Chi K0',100,0.,100.,0.)
      CALL HBOOK2(JOFF+224,'Chi K0 vs. inv mass K0'
     $                ,100,0.,1.,100,0.,100.,0.)
      CALL HBOOK1(JOFF+225,'Inv. mass Lambda and AntiLam.'
     $                ,100,1.,1.5,0.)
      CALL HBOOK1(JOFF+226,'Chi Lam. and AntiLam',100,0.,100.,0.)
      CALL HBOOK2(JOFF+227,'Chi Lam/AntiL vs. inv. mass Lam/AntiLam'
     $                ,100,1.,1.5,100,0.,100.,0.)
      CALL HBOOK1(JOFF+228,'Inv. mass Gamma',100,0.,1.,0.)
      CALL HBOOK1(JOFF+229,'Chi Gamma',100,0.,100.,0.)
      CALL HBOOK2(JOFF+230,'Chi Gamma vs. inv. mass Gamma'
     $                ,100,0.,1.,100,0.,100.,0.)
C
      CALL HBOOK2(JOFF+231,
     $ 'Chisq. trk-to-prim (min.) vs Chisq vx-to-prim after all cuts'
     $                ,40,0.,100.,40,0.,100.,0.)
      CALL HBOOK2(JOFF+232,
     $ 'Chisq. trk-to-prim (sum) vs Chisq vx-to-prim after all cuts'
     $                ,40,0.,100.,40,0.,100.,0.)
      CALL HBOOK1(JOFF+233,'Radial diatance of vertex after all cuts'
     $                ,100,0.,180.,0.)
      CALL HBOOK1(JOFF+234,'Z of vertex after all cuts'
     $                ,100,0.,220.,0.)
      CALL HBOOK2(JOFF+235,'Z vs R of vertex after all cuts'
     $                ,100,0.,180.,100,0.,220.,0.)
      CALL HBOOK1(JOFF+236,'ICODEV0 after all cuts',50,-25.,25.,0.)
      CALL HBOOK2(JOFF+237,'B vs R after all cuts'
     $                ,50,-1.,1.,50,0.,0.25,0.)
      END
#endif
