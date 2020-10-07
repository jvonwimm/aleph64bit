      PARAMETER (LTPDCH=64,LTPD=20,LTHIDI=600,LTPDNB=8,
     +           LMXPCL=20,LMXSCL=60,LMXPRT=5)
#if defined(DOC)
C-----------------------------------------------------------------------
C! Parameter definitions for TPC reconstruction.  Parameters used within
C more than a single routine or comdeck are defined here, otherwise
C they are defined within the routine or comdeck (see also TPGPAR).
C
C First, those parameters related to electronics readout:
C   LTPDCH  = max. # of channels per TPD
C   LTPD    = max. # of TPDs
C   LTPDNB  = number of bits for a TPD sample
C Second, limits on arrays internal to TPC reconstruction:
C   LMXPCL  = upper limit on number of pads in cluster allowed by TCLCOR
C   LMXSCL  = upper limit on length of a cluster in samples (for TCLCOR)
C   LTHIDI  = expected upper limit on # of hits per sector padrow.
C             It is OK if this limit is occassionally violated.
C
C   LMXPRT  = maximum number of times to print a given TPC error message
C-----------------------------------------------------------------------
#endif
