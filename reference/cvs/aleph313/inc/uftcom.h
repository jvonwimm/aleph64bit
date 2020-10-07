C!  Curvature/momentum conversion and scattering length for Kalman Filter
      REAL ROVERP, SCATTR
      COMMON/UFBFLD/ROVERP, SCATTR

C =======
      DOUBLE PRECISION CORE_FRACT, TAIL_FRACT, CORE_WIDTH, TAIL_WIDTH
      INTEGER DO_ELOSS
      REAL FILTER_PROB, CH2_1D, CH2_2D
      COMMON/UFG2/  CORE_FRACT, TAIL_FRACT, CORE_WIDTH, TAIL_WIDTH
     &             ,FILTER_PROB, DO_ELOSS
     &             ,CH2_1D, CH2_2D

#if defined(DOC)
C  The following parameters are read from the UFG2 CARD:
C
C            CORE_FRACT = fraction of scattering in core
C            CORE_WIDTH = width of core compared to nominal
C            TAIL_WIDTH = width of tail compared to nominal
C            FILTER_PROB= probability cut to remove coord
C            DO_ELOSS   = 0: No energy loss
C                         1: Energy loss by particle hypothesis
C                            mass in call, or pion by default.
#endif
C ========

C!  Contraint values for Kalman Filter
      REAL OME_FIX, TANL_FIX, P_FIX, RAD_CUT
      COMMON / UFCSTR / OME_FIX, TANL_FIX, P_FIX, RAD_CUT
C ========

C!  Logical flags for constrained fits by Kalman Filter
      LOGICAL FIXED_OME, FIXED_TANL, FIXED_P
      COMMON/UFLOGS/FIXED_OME, FIXED_TANL, FIXED_P
C ========

C!  Pi-related constants for Kalman filter
      DOUBLE PRECISION ONE_PI, TWO_PI, HALF_PI
      COMMON/UFPICO/ ONE_PI, TWO_PI, HALF_PI
