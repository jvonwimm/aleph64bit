C!    PJETEF content result of THRUST and Jet analysis
      COMMON / PJETEF / NFITJT (2) , PJETRF (5,10,2),THRTEF(4,3,2)
     &               , VRECNF (5,2),ETRACK
#if defined(DOC)
C-          2 fit are performed IFIT =1  using calorimetry alone
C-                                   =2  energy flow algorithm
C-          NFITJT(IFIT) = # of jets reconstructed
C-          PJETRF(5,10,IFIT) = PX,PY,PZ,E,PT of each jet (<=10)
C-          THRTEF(4,J,IFIT) = PX,PY,PZ,Eigen value for thrust (J=1)
C-                                          & for the 2 others axis(J=2,3
C-          VRECNF (5,IFIT) = PX,PY,PZ ,E ,ET of flow result
C-          ETRACK  =  Energy of selected charged tracks (no weight)
#endif
