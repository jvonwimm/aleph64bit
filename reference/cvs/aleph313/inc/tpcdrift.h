        COMMON /TPCDRIFT/  TPC_UXY(2,LTSECT)
        PARAMETER (WT=8.9, WT2=WT**2, C2=1./(1.+WT2))

#if defined(DOC)
C!     TPC_UXY = transverse drift velocity for each sector
C      WT      = omega*tau
C      WT2     = (omega*tau)**2
C      C2      = parameter related to drift
#endif
