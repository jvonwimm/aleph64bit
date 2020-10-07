      COMMON / RPASS0 / NPEVMI,  NPEVMA, TMTLAS,
     &  DT0CUT, VIPAS0(3,2), NHOTBF
#if defined(DOC)
C
C!  JULIA Constants for PASS0
C
C   NPEVMI  = Minimum number of events needed for computing new TPC Vz
C   NPEVMA  = Maximum number of events needed for computing new TPC Vz
C   TMTLAS  = Maximum elapsed time (in mn) of last laser meas. for using
C   DT0CUT  = Maximum value of T0 for which the drift velocity is update
C   VIPAS0  = Drift velocity (x,y,z) side A and B as input to PASS0
C
#endif
