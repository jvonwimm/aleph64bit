      COMMON/YV0CUT/YV0CIV,YV0CO1,YV0CO2,YV0CCT,YV0CPS,YV0CRV,YV0CZV,
     $              YV0DIP,YV0CC2,YV0CCS,YV0CC0,YV0CMA
#if defined(DOC)
C
C!     This common contains the cuts for V0 finding. It is filled
C     in YV0INI from the direct acces  bank 'YV0C'
C     YV0CIV  = Flag for take reconstructed vertex from yvxl bank
C                (YV0CIV=0.) or vertex in (0.,0.,0.) (YV0ICV=1.)
C
C     YV0CO1  = Min value of chi square increase constraining only
C                                     one track to the main vertex
C     YV0CO2  = Min value of chi square increase constraining both
C                                        tracks to the main vertex
C     YV0CCT  = Max value of distance between
C               starting points of fit
C     YV0CPS  = Minimum value for the psi angle
C     YV0CRV  = Max value of the V0 vertex radius
C     YV0CZV  = Max value of the V0 vertex abs(Z)
C     YV0DIP  = Max value of difference between tg of dip
C               (for test on parallelism between two tracks)
C     YV0CC2  = Max value of the V0 fit chi square
C     YV0CCS  = Minimum value of the cosinus of the angle between
C                                       V0 vertex and V0 momentum
C     YV0CC0  = Minimum value of the chi square increase constraining
C                                     V0 vertex to the primary vertex
C     YV0CMA  = Maximum value of the chi square of the mass constraint
C
C---------------------------------------------------------------------
#endif
