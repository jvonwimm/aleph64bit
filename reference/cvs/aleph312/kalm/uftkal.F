      INTEGER FUNCTION UFTKAL(FIELD,
     +                  N_COORD, R_IN, RPHI_IN, Z_IN,
     +                  RPHI_SIG, Z_SIG, RPHI_Z_COR,
     +                  TRK_IN,CHI2_IN,
     +                  TRK_OUT, COV_OUT, CHI2_OUT, N_DOF)
CKEY KALMAN
C!  New and improved Kalman filter
C!  with double Gaussian multiple scattering, energy loss,
C!  and other goodies.
C!            - Dave Casper
C!
C!  Modified: 29-oct-1997 D.Casper
C!    Disable removal of outliers if track has 4 or less remaining 3-D coord.
C!    Fail if numerical problem would reduce track to exactly 3 hits.
C!
C!  INPUT:  FIELD  = magnetic field strength in kG
C!          TRK_IN = 5 input track parameters from a preliminary fit
C!              1 : 1/R         [1/cm]   neg. if clockwise
C!              2 : TAN(LAMBDA) [dZ/dS]  tan(angle to x,y plane)
C!              3 : PHI0        [0,2pi]  angle to x-axis
C!              4 : D0*SIGN      [cm]    minimal dist. to z-axis,
C!                                       sign of angular mom. lz
C!              5 : Z0           [cm]    z pos at r=d0
C!          CHI2_IN = chi**2 of preliminary fit
C!
C!          N_COORD     = number of coordinates
C!
C!          R_IN       = array of r- coordinates
C!          RPHI_IN    = array of r-phi-coordinates
C!          Z_IN       = array of z-coordinates
C!          RPHI_SIG   = array of sigma(rphi)**2
C!          Z_SIG      = array of sigma(z)**2
C!          RPHI_Z_COR = array of rphi-z correlations
C!
C!  The following parameters are read from the UFG2 CARD:
C!
C!            CORE_FRACT = fraction of scattering in core
C!            CORE_WIDTH = width of core compared to nominal
C!            TAIL_WIDTH = width of tail compared to nominal
C!            FILTER_PROB= probability cut to remove coord
C!            DO_ELOSS   = 0: No energy loss
C!                         1: Energy loss by particle hypothesis
C!                            mass in call, or pion by default.
C!
C!  OUTPUT:   TRK_OUT = 6 final track parameters
C!              1 : 1/R          [1/cm]  neg. if clockwise
C!              2 : TAN(LAMBDA)  =dz/DS} tan(angle to x,y plane)
C!              3 : PHI0        [0,2pi]} angle to x-axis
C!              4 : D0*SIGN      [cm]    minimal dist. to z-axis,
C!                                       sign of angular mom. lz
C!              5 : Z0           [cm]    z pos at r=d0
C!              6 : ALPHA      [-pi,pi]: scattering angle
C!                                       at ITC wall (in x-y)
C!            COV_OUT = covariance matrix in lower triang. form
C!                     1
C!                     2  3
C!                     4  5  6
C!                     7  8  9 10
C!                    11 12 13 14 15
C!                    16 17 18 19 20 21
C!            CHI2_OUT = chi squared from last kalman filter step
C!            N_DOF= number of degrees of freedom
C!
C!  NOTE:     fit done for the 5 helix parameters only.
C!            the angle alpha is computed but elements
C!            16,...,21 of cov_out are dummy.
C!
C!  MASS HYPOTHESIS FOR FIT
C!  =======================
C!
C!      ENTRY UFMKAL(FIELD, N_COORD, R_IN, RPHI_IN, Z_IN,
C!     +                  RPHI_SIG, Z_SIG, RPHI_Z_COR,
C!     +                  TRK_IN,CHI2_IN, MASS_IN,
C!     +                  TRK_OUT, COV_OUT, CHI2_OUT, N_DOF)
C!
C!  Identical to calling UFTKAL, except MASS_IN is used as the
C!  particle mass to determine energy loss.
C!
C!
C!  SWIMMING TO TRACK ORIGIN:
C!  =========================
C!            CALL UFSWIM( 1 ) ===>  fit errors are given for track
C!                                   origin from now on
C!            CALL UFSWIM( 0 ) ===>  fit errors are given at innermost
C!                                   coordinates from now on
C!                                   (===> DEFAULT <===)
C!
C!  FILTERING OF BAD COORDINATES:
C!  ============================
C!            CALL UFTFIL( FILTER_PROB )
C!            FILTER_PROB =  probability to throw away a good (!)
C!                           coordinate when filtering for badies
C!                           in the first smoother step.
C!                           if =0, no coordinate filtering will
C!                           be done.
C|                           recommended for filtering: 5.e-4
C!
C!  CONSTRAINTS POSSIBLE:
C!  ====================
C!            OMEGA :       fixed to OME_FIX by calling UFTOME(1)
C!                          released by calling UFTOME(0)
C!
C!            TAN(LAMBDA) : fixed to TANL_FIX by calling UFTTAL(1)
C!                          released by calling UFTTAL(0)
C!                          if TAN(LAMBDA) is fixed there is no
C!                          multiple scattering in s-z plane
C!
C!            MOMENTUM :    fixed to P_FIX (>0) by calling
C!                          UFTMOM(1)
C!                          released by calling UFTMOM(0)
C!
C!            RADIUS :      energy loss and multiple scattering are
C!                          restricted to R > RAD_CUT.  This is
C!                          useful for V0s which decay outside the
C!                          beampipe, VDET, etc.  RAD_CUT is set
C!                          by CALL UFTRAD(RAD_MIN).  CALL UFTRAD(0.)
C!                          afterward!
C!
C!        The values for the constraints are steered via
C!        COMMON /UFCSTR/ OME_FIX, TANL_FIX, P_FIX, RAD_CUT
C!
C!  SPECIAL MODE FOR EVENT DISPLAY APPLICATIONS:
C!  ===========================================
C!        CALL   DALINF(1)  to fill  COMMON / INFDAL /
C!        CALL   DALINF(0)  if smoothed local track parameters
C!                          are not any longer needed
C!
C!  SPECIAL STEERING INFORMATION FOR VDET PATTERN RECOGNITION
C!  =========================================================
C!      ENTRY UFVDIN(NVDIN,ISAFVD,ISBEVD)
C!              INPUT:
C!              NVDIN  : number of vdet coordinates to be used in filter
C!              ISAFVD :  .NE. 0 <=> stop after vdet filtering (flag)
C!              ISBEVD :  .NE. 0 <=> start before vdet filtering (flag)
C!      ENTRY UFVDOU(NVDOUT,CHISVD)
C!              OUTPUT:
C!              NVDOUT : number of vdet measurements surviving filter
C!              CHI2VD : chi**2 of fit after vd filtering
C!
C!   KALMAN FILTER ALGORITHM INTERNALS
C!   ==================================
C!
C!   STATE VECTOR (1,K)  ===>  R*FI       AT R(K)
C!                (2,K)  ===>  Z          AT R(K)
C!                (3,K)  ===>  PHI        AT R(K)
C!                (4,K)  ===>  LAMBDA     AT R(K)
C!                (5,K)  ===>  OMEGA      AT R(K)
C!
C!   "TIME"              ===>  PAD ROW RADIUS R(K)
C!
C!   PREDICTED STATE VECTOR  ===>  X_PRED(5,K)
C!   FILTERED  STATE VECTOR  ===>  X_FILT(5,K)
C!   SMOOTHED  STATE VECTOR  ===>  X_SMOOTH(5,K)
C!
C!   MEASUREMENT VECTOR      ===>  R_MEAS(2,N)
C!             R_MEAS(1,K)  ===>  R*FI       AT R(K)
C!             R_MEAS(2,K)  ===>  Z          AT R(K)
C!
C!   COVARIANCE MATRICES (SYMMETRIC):
C!     C_PRED(5,5,K)   ===>  PREDICTION INTO ROW K
C!     C_FILT(5,5,K)   ===>  FILTER IN ROW K
C!     C_SMOOTH(5,5,K)   ===>  SMOOTHING IN ROW K
C!
C!   PROCESS NOISE MATRIX (WITH ALL CORRELATIONS):
C!     Q_PROC(5,5,K) ===>  COVARIANCE MATRIX FOR STATE VECTOR
C!                        FROM MULTIPLE SCATTERING ON THE WAY
C!                        TO ROW K
C!
C!   MEASUREMENT NOISE MATRIX (DIAGONAL):
C!     V_MEAS(2,2,K) ===>  COVARIANCE MATRIX FOR MEASUREMENT
C!                        VECTOR FROM ERRORS IN THE TPC
C!                        COORDINATES IN ROW K
C!
C!
C!  UTILITY ENTRIES FOR TRACKING STUDIES ETC
C!  ======= ======= === ======== ======= ===
C!
C!    ENTRY UF2RES(RES_TRIM, TRIM_ERR, FIT_TRIM, FIT_ERR)
C!
C!      Returns:
C!               RES_TRIM(2,MAX_PTS) (R*4)
C!                  Coordinate residuals (rphi,z) calculated for each point
C!                  by excluding that point from the fit.  This
C!                  removes the bias from measuring coordinate resolution
C!                  using tracks fitted with the coordinates (since the
C!                  fit is pulled towards the coordinates, the measured
C!                  resolution will be smaller than the true resolution
C!                  otherwise).
C!              TRIM_ERR(2,MAX_PTS) (R*4)
C!                  Calculated error on the fit (omitting the coordinate)
C!                  at the coordinate.  RES_TRIM is roughly the sum in
C!                  quadrature of the true resolution and the error in the
C!                  fitted track position.
C!              FIT_TRIM(2,MAX_PTS) (R*4)
C!                  The r-phi and z position of the track at each coordinate,
C!                  when the coordinate is excluded from the fit.
C!              FIT_ERR(2,MAX_PTS) (R*4)
C!                  The calculated error on the fit at the coordinate if
C!                  the coordinate is included.
C!
C!    INTEGER FUNCTION UF2PNT(FIELD, R_PNT, X_PNT, C_PNT)
C!
C!      Input:
C!              FIELD (R*4)
C!                  Magnetic field strength in standard Aleph units.
C!
C!              R_PNT (R*4)
C!                  Radius at which the state vector its covariance are
C!                  desired (user should check that the value of R_PNT is
C!                  sensible or face unpredictable consequences...)
C!
C!      Returns:
C!              X_PNT(5) (R*4)
C!                  Smoothed state vector at R_PNT.
C!
C!              C_PNT(21) (R*4)
C!                  Covariance of X_PNT.
C!
C!              UF2PNT (I*4)
C!                  0 if OK; Kalman error code otherwise.
C!
C!
C!    INTEGER FUNCTION UF2TRM(ICOORD, TRK_TRIM, COV_TRIM)
C!
C!      Input:
C!              ICOORD (I*4)
C!                  Coordinate at which the track fit and covariance
C!                  matrix, using all points EXCEPT ICOORD in the fit,
C!                  is desired.
C!
C!      Output:
C!              TRK_TRIM(6) (R*4)
C!                  Helix parameters at ICOORD, calculated by fitting
C!                  all other points, expressed in the standard helix
C!                  form.
C!
C!              COV_TRIM(21) (R*4)
C!                  Covariance matrix of TRK_TRIM, in usual triangular
C!                  form.
C!
C!              UF2TRM (I*4)
C!                  Returns zero if OK.
C!
C!    INTEGER FUNCTION UF2TPO(ICOORD, TRK_TRIM, COV_TRIM, POS_TRIM)
C!      
C!      Input:  Same as UF2TRM
C!
C!      Output: Same as UF2TRM, plus
C!              POS_TRIM(2) (R*4)
C!                  Returns the rphi and z value at the coordinate requested
C!
C!    INTEGER FUNCTION UF2XYZ(XPOS,EPOS)
C!
C!      Input:  None, but must be called immediately after UF2PNT
C!
C!      Output:
C!              XPOS(6) (R*4)
C!                  The (x,y,z) position and (dx,dy,dz) direction cosines
C!                  of the track at the last point requested by UF2PNT.
C!
C!              EPOS(2) (R*4)
C!                  The error on r-phi and z at the last point requested
C!                  in a call to UF2PNT.
C!                  
C!
C!    ENTRY UFTRAC(LTRACE)
C!  
C!      Input: LTRACE (Logical)
C!                  Call with value .TRUE. to enable printout of the
C!                  state vectors on the *next* track fit by UFTKAL.
C!                  Tracing is automatically disabled on return; there
C!                  is no need for the user to explicitly disable it.
C!                  
C!
C!  KALMAN FILTER RETURN CODES
C!  ====== ====== ====== =====
C!
C!      0       = Success
C!      1       = Invalid B field
C!      2       = Invalid input track
C!      3       = Invalid input coordinates
C!      4       = Unusable input coordinates
C!      5       = Invalid number of coordinates for VDET patrec
C!      6       = Too many bad coordinates
C!      7       = Core determinant is zero
C!      8       = Ratio of determinants negative
C!      9       = Transport error during VDET patrec
C!      10      = Transport error leaving too few coordinates
C!      11      = Transport error in UFTRAN
C!      12      = Error calculating Jacobian
C!      13      = Filtered chi^2 not positive
C!      14      = Error calculating process time
C!      15      = # DOF < 1
C!      16      = Smoothed covariance matrix invalid
C!      17      = Matrix inversion failed
C!      18      = Unable to calculate chi^2
C!      19      = Unable to calculate scattering angle
C!      20      = Failure to converge
C!      21      = No used coordinates for UF2PNT
C!      22      = Chi^2 was above the set limit (see UFCLIM)
C!      23      = Trimmed state vector not valid
C!      24      = Error calculating new state in UFSTAT
C!      25      = Covariance matrix with negative diagonal in UFTKAL
C!      26      = Covariance matrix with negative diagonal in UF2PNT
C!
#ifndef DOC
#include "bcs.h"
#include "vrldcom.h"
#include "uftcom.h"
      PARAMETER (MAX_LOOP = 15)
      DOUBLE PRECISION EPS, EPS1
      PARAMETER (EPS = 1.0D-15)
      PARAMETER (EPS1 = 1.0D-25)
      PARAMETER (MAX_ZVAR = 900.)
      PARAMETER (MAX_RPHIVAR = 900.)
      PARAMETER (MIN_COORD = 3)
C
      REAL R_IN(*), RPHI_IN(*), Z_IN(*), RPHI_SIG(*), Z_SIG(*)
      REAL RPHI_Z_COR(*), TRK_IN(*), TRK_OUT(*), COV_OUT(*)

      INTEGER UFMKAL

      PARAMETER (MAX_PTS = 40)
      COMMON / INFDAL / FIRST_COORD, LAST_COORD, X_COORD(5,MAX_PTS),
     &   R_MEAS_COORD(2,MAX_PTS), R_COORD(MAX_PTS), COORD_USED(MAX_PTS),
     &   V_MEAS_COORD(2,2,MAX_PTS)

      INTEGER FIRST_COORD, LAST_COORD
      DOUBLE PRECISION X_COORD, R_MEAS_COORD, R_COORD, V_MEAS_COORD
      INTEGER COORD_USED
#if defined(DOC)
C!        Description of COMMON / INFDAL /:
C!              FIRST_COORD    : index of first coordinate used
C!              LAST_COORD     : index of last coordinate used
C!              X_SMOOTH(5,40) : smoothed state vectors  (dble prec)
C!              R_MEAS(2,40)   : measurement vectors (double prec)
C!              R_COORD(40)    : radii of coordinates (double prec)
C!              COORD_USED(40)  : coordinate flags
C!                          -2 = coordinate excluded from fit by UF2CRD
C!                          -1 = coordinate has been removed twice and will neve
C!                           0 = not used in fit
C!                           1 = used in fit
C!                           2 = coordinate has been removed and re-added
C!              V_MEAS(2,2,40) : covariance matrix of coordinates
C!                                (double prec)
C!                  1,1   ===>  r-phi error squared
C!                  2,2   ===>  z error squared
C!                  1,2   ===>  correlation (normally 0.)
C!                  2,1   ===>  same as 1,2
#endif

      INTEGER SORTARR(MAX_PTS)

      INTEGER DAL_FIRST,DAL_LAST,DAL_USED(MAX_PTS)
      DOUBLE PRECISION DAL_STATE(5,MAX_PTS),DAL_RMEAS(2,MAX_PTS),
     &  DAL_RCOORD(MAX_PTS),DAL_VMEAS(2,2,MAX_PTS)

      DOUBLE PRECISION X_SMOOTH(5,MAX_PTS),R_MEAS(2,MAX_PTS),
     &  V_MEAS(2,2,MAX_PTS)
      DOUBLE PRECISION R_PT(MAX_PTS),PHI_PT(MAX_PTS),Z_PT(MAX_PTS),
     &  WT_RPHI_PT(MAX_PTS),WT_Z_PT(MAX_PTS),COR_RPHI_Z_PT(MAX_PTS)
      INTEGER USED_BEFORE(MAX_PTS), INNER, OUTER, OUTER_ALL, FIRST_INNER
      INTEGER I_COORD(MAX_PTS), INNER_PT, OUTER_PT, PT_USED(MAX_PTS)
      INTEGER I_PT(MAX_PTS), INNER_USE, OUTER_USE
      INTEGER UFGETT, UFTRAN, UFJACO, UFSTAT
      DOUBLE PRECISION  XY_ARC(MAX_PTS)
      DOUBLE PRECISION  PHI_COORD(MAX_PTS), Z_COORD(MAX_PTS),
     +                  WT_Z(MAX_PTS), WT_RPHI(MAX_PTS),
     +                  COR_RPHI_Z(MAX_PTS)
      DOUBLE PRECISION X_PRED(5,MAX_PTS), X_FILT(5,MAX_PTS),
     &                 X_TRIM(5,MAX_PTS), C_TRIM(5,5,MAX_PTS),
     2                 C_PRED(5,5,MAX_PTS), C_FILT(5,5,MAX_PTS),
     3                 C_SMOOTH(5,5,MAX_PTS), Q_PROC(5,5,MAX_PTS),
     3                 DE_DX(2,MAX_PTS),
     4                 DF_DX(5,5,MAX_PTS), RES_PRED(2,MAX_PTS),
     5                 RES_FILT(2,MAX_PTS), RES_SMOOTH(2,MAX_PTS),
     6                 RES_FILT_COV(2,2,MAX_PTS),
     6                 RES_TRIM(2,MAX_PTS), RES_TRIM_COV(2,2,MAX_PTS),
     +                 RES_SMOOTH_COV(2,2,MAX_PTS),
     7                 CHI2_FILT(MAX_PTS), Q_LOCAL(2,2), CHI2_LAST,
     7                 CHI2, CHI2_CUT,X_EXTR(6),E_EXTR(2)
      DOUBLE PRECISION W_CORE(5,5), W_TAIL(5,5), W_FILT(5,5)
      DOUBLE PRECISION GAIN_FILT(5,2), GAIN_SMOOTH(5,5)
      DOUBLE PRECISION GAIN_CORE(5,2), GAIN_TAIL(5,2), GAIN_TRIM(5,2)
      DOUBLE PRECISION X_FILT_CORE(5,MAX_PTS), X_FILT_TAIL(5,MAX_PTS)
      DOUBLE PRECISION C_FILT_CORE(5,5,MAX_PTS)
      DOUBLE PRECISION C_FILT_TAIL(5,5,MAX_PTS)
      DOUBLE PRECISION C_PRED_CORE(5,5,MAX_PTS)
      DOUBLE PRECISION C_PRED_TAIL(5,5,MAX_PTS)
      DOUBLE PRECISION B_CORE(MAX_PTS), B_TAIL(MAX_PTS)
      DOUBLE PRECISION DETW_FILT, DETW_CORE, DETW_TAIL, GAIN_DIFF(5,2)
      DOUBLE PRECISION DELTA_PHI, DELTA_R, DELTA_U, NUMER, DENOM,
     &  PHI_OLD, PHI_NEW, T_PROC
      DOUBLE PRECISION C_NEW(21), C_TEMP(5,5), C_WORK(5,5), WT_PRED(5),
     &  R_INNER,T_CHECK,X_INNER(5),TMP_C_PRED(5,5)
      DOUBLE PRECISION DEN, FACT, CS, SS, DET

C used to optimize matrix calculations
      DOUBLE PRECISION TMP

      DOUBLE PRECISION OMEGA_TRK,D0_TRK,PHI0_TRK,TANL_TRK,Z0_TRK
      DOUBLE PRECISION DBL_TRK(5)
      EQUIVALENCE (DBL_TRK(1), OMEGA_TRK), (DBL_TRK(2),TANL_TRK)
      EQUIVALENCE (DBL_TRK(3), PHI0_TRK), (DBL_TRK(4), D0_TRK)
      EQUIVALENCE (DBL_TRK(5), Z0_TRK)

      REAL CHI2LIM, NCHI2DEL
      PARAMETER (NCHI2DEL = 1.5)
      SAVE CHI2LIM

      REAL RES_TRIM_OUT(2,MAX_PTS), FIT_ERR_OUT(2,MAX_PTS),
     &  TRIM_ERR_OUT(2,MAX_PTS), TRK_TRIM_OUT(6), COV_TRIM_OUT(21),
     &  POS_TRIM_OUT(2),X_DUM(6),E_DUM(2), FIT_TRIM_OUT(2,MAX_PTS)

      REAL R_PNT, X_PNT(5), C_PNT(21)
      INTEGER UF2PNT, UF2TRM, UF2TPO
      INTEGER UFTOME,UFTTAL,UFTMOM,UFTRAD,DALINF,UFDALI,UFSWIM
      INTEGER UFCLIM,UFVDIN,UFVDOU,UF2RES,UF2XYZ,UFTRAC

      DOUBLE PRECISION R_USE, TMP_R(3), TMP_XY(3), TMP_X(5,3),
     &  TMP_Q_PROC(5,5,3), TMP_DE_DX(2,3), TMP_X_FILT(5),
     &  TMP_C_FILT(5,5), TMP_DF_DX(5,5,2), TMP_C_SMOOTH(5,5),
     &  TMP_X_SMOOTH(5)

      DOUBLE PRECISION MASS, M_PION
      PARAMETER (M_PION = 0.13956995D0)
      REAL MASS_IN

      INTEGER ISCODE(MAX_PTS)
      INTEGER LRUN,IRUN
      LOGICAL FIRST, AT_VTX, FILL_HELIX
      LOGICAL LAST_ITER, DO_SMTH_COV
      LOGICAL TRACING,LTRACE
      SAVE
      DATA LRUN/0/
      DATA FIRST/.TRUE./
      DATA FIXED_OME/.FALSE./, FIXED_TANL/.FALSE./, FIXED_P/.FALSE./
      DATA FILL_HELIX /.FALSE./, AT_VTX/.FALSE./
      DATA FILTER_PROB / 0. /, RAD_CUT/0./
      DATA ISAFVD / 0 /, ISBEVD / 0 /
      DATA TRACING/.FALSE./
      DATA CHI2LIM/ 1.0E32 /
C
#include "bmacro.h"

C - UFTKAL entry point
C   Default pion mass

      MASS = M_PION
      GOTO 101
C --------------------------------------------------------------
      ENTRY UFMKAL(FIELD, N_COORD, R_IN, RPHI_IN, Z_IN,
     +                  RPHI_SIG, Z_SIG, RPHI_Z_COR,
     +                  TRK_IN,CHI2_IN, MASS_IN,
     +                  TRK_OUT, COV_OUT, CHI2_OUT, N_DOF)

      MASS = DBLE(MASS_IN)
C -----------------------------------------------------------------
  101 CONTINUE

      CALL ABRUEV(IRUN,IEVT)

C - new run ----------

      IF(IRUN.NE.LRUN) THEN

C     get the VRLD bank and fill /VRLDCOM/ UK.... variables
      CALL VRLDGT(IER)
      IF (IER.LT.0)THEN
        CALL ALTELL('UFTKAL: unable to load VRLD bank from database',
     +          0, 'STOP')
        STOP
      ENDIF

C     At 1st entry : 

        IF (LRUN.EQ.0) THEN
C       read the UFG2 card/bank and
C       set up chi^2 cuts and filtering probabilities
           CALL UFREIN

C       Initialize numerical constants
           HALF_PI = DASIN(1.0D0)
           ONE_PI  = 2.D0 * HALF_PI
           TWO_PI  = 4.D0 * HALF_PI
        ENDIF
        LRUN = IRUN
      ENDIF

C - new entry --------------

C  Initialize magnetic field parameters

      UFTKAL = 1
      IF (FIELD.EQ.0.) GOTO 999
      ROVERP = 1./(0.29979*FIELD/10.)*100.
      SCATTR = .0136 * ROVERP
C  Initialize input track

      UFTKAL = 2
      CALL UF2TRK(TRK_IN, DBL_TRK, *999)

C  Initialize input coordinates and sort by decreasing radius

      UFTKAL = 3
      CALL UF2CRD(N_COORD, R_IN, RPHI_IN, Z_IN,
     +           RPHI_SIG, Z_SIG, RPHI_Z_COR,
     +           N_PT, R_PT, PHI_PT, Z_PT, WT_RPHI_PT,
     +           WT_Z_PT, COR_RPHI_Z_PT, I_COORD, *999)
      IC = 0
      DO IPT = 1, N_PT
        IF (I_COORD(IPT).NE.0)THEN
          IC = IC + 1
          R_COORD(IC) = R_PT(IPT)
          PHI_COORD(IC) = PHI_PT(IPT)
          Z_COORD(IC) = Z_PT(IPT)
          WT_RPHI(IC) = WT_RPHI_PT(IPT)
          WT_Z(IC) = WT_Z_PT(IPT)
          COR_RPHI_Z(IC) = COR_RPHI_Z_PT(IPT)
        ENDIF
      ENDDO

C  Find usable coordinates based on input track parameters

      UFTKAL = 4
      CALL UF2USE(N_PT, DBL_TRK, R_PT, I_COORD, N_COORD_USED, INNER_PT,
     &  OUTER_PT, *999)
      IF (.NOT. AT_VTX)THEN
        IPT = INNER_PT
        DO WHILE (I_COORD(IPT).EQ.0)
            IPT = IPT - 1
        ENDDO
        INNER_PT = IPT
      ENDIF
      N_PT_USED = INNER_PT - OUTER_PT + 1

C  Initialize flags for coordinates used

      DO II = 1, MAX_PTS
        PT_USED(II) = 1
        USED_BEFORE(II) = 1
      ENDDO

C Check special requirements for vdet pattern recognition
  100 CONTINUE

      OUTER_ALL = OUTER_PT
      IF ( ISBEVD .NE. 0 ) THEN
        DO WHILE (OUTER_PT.LT.INNER_PT+1 .AND.
     &    (OUTER_PT.EQ.OUTER_ALL .OR. N_COORD_INSIDE.GT.0))
          N_COORD_INSIDE = 0
          DO K = OUTER_PT+1,INNER_PT
            IF (I_COORD(K).NE.0 .AND. R_PT(K) .GT. UKRITC)
     &        N_COORD_INSIDE = N_COORD_INSIDE + 1
          ENDDO
          IF (N_COORD_INSIDE .GT. 0) OUTER_PT = OUTER_PT+1
        ENDDO
      ENDIF
      IF ( INNER_PT .LT. OUTER_PT ) THEN
        UFTKAL = 5
        GOTO 999
      ENDIF

C Find innermost point which is a coordinate

      INNER_COORD = INNER_PT
      DO WHILE(I_COORD(INNER_COORD).EQ.0)
        INNER_COORD = INNER_COORD - 1
      ENDDO

C Swim preliminary fit to coord radii, and calculate arclengths

      CALL UF2ARC(INNER_PT, OUTER_PT, DBL_TRK, R_PT, X_SMOOTH,
     &  XY_ARC)

C Initialize measurement vector and its covariance matrix

      DO  I = 1, N_PT
        IF (I_COORD(I).NE.0)THEN
          R_MEAS(1,I)   =  R_PT(I)*PHI_PT(I)
          R_MEAS(2,I)   =  Z_PT(I)
          V_MEAS(1,1,I) = 1.D0/WT_RPHI_PT(I)
          V_MEAS(2,2,I) = 1.D0/WT_Z_PT(I)
          V_MEAS(1,2,I) = COR_RPHI_Z_PT(I)
          V_MEAS(2,1,I) = COR_RPHI_Z_PT(I)
        ENDIF
      ENDDO

C  Get covariance matrix of process noise (and energy loss)

      CALL UFQPRC(FIELD, INNER_PT, OUTER_PT, DBL_TRK, R_PT, XY_ARC,
     +                  X_SMOOTH, AT_VTX, MASS,
     +                  Q_PROC, DO_ELOSS, DE_DX)

C  If the vdet is filtered alone, the starting vector at the innermost
C  itc/tpc coordinate is already defined

      IF ( ISBEVD .NE. 0 ) GOTO 207

C  Starting vector with infinite covariance matrix derived from preliminary
C  helix fit

      N_LOOP = 0
      CHI2_LAST  = DBLE(CHI2_IN)
      NBADCL = 0

C  Recursion over multiple passes (in/out/in/...) starts here

 1000 CONTINUE

C  Number of coordinates with no z, or r-phi information
      N_COORD_USED = 0
      DO IPT = OUTER_PT, INNER_PT
        IF(I_COORD(IPT).NE.0) N_COORD_USED = N_COORD_USED + 1
      ENDDO
      NCOORD_NOZ = 0
      NCOORD_NORPHI = 0
C  Number of bad coordinates
      NBADC  = 0

C  Start with the smoothed state vector at the outer most coordinate
C  Make it the predicted one.  Smoothed vector is either result of
C  previous iteration, or the input values initialized above.

      CALL DVMOVE(X_SMOOTH(1,OUTER_PT), X_PRED(1,OUTER_PT),5)
      RES_PRED(1,OUTER_PT) = PHI_PT(OUTER_PT)*R_PT(OUTER_PT)
     &  - X_PRED(1,OUTER_PT)
      DELTA_PHI = RES_PRED(1,OUTER_PT) / R_PT(OUTER_PT)
      CALL UBAPI(DELTA_PHI)
      RES_PRED(1,OUTER_PT) = DELTA_PHI * R_PT(OUTER_PT)
      RES_PRED(2,OUTER_PT) = Z_PT(OUTER_PT) - X_PRED(2,OUTER_PT)

C Zero out the covariance matrix

      CALL DVZERO(C_PRED(1,1,OUTER_PT),25)

C Make the diagonal terms of the covariance matrix big

      DELTA_PHI = PHI_PT(OUTER_PT) - X_PRED(1,OUTER_PT)/R_PT(OUTER_PT)
      CALL UBAPI(DELTA_PHI)

      C_PRED(1,1,OUTER_PT) = 100.D0/WT_RPHI(OUTER_PT)
     &  + (R_PT(OUTER_PT)*DELTA_PHI)**2
      C_PRED(2,2,OUTER_PT) = 100.D0/WT_Z(OUTER_PT)
     +                  + (Z_PT(OUTER_PT)-X_PRED(2,OUTER_PT))**2
      C_PRED(3,3,OUTER_PT) = 1.D0
      C_PRED(4,4,OUTER_PT) = 1.D0
      C_PRED(5,5,OUTER_PT) = 4.D0*C_PRED(1,1,OUTER_PT)
     &  /R_PT(OUTER_PT)**4 + X_PRED(5,OUTER_PT)**2

C Satisfy requested constraints

C Fixed omega
      IF ( FIXED_OME ) THEN
        X_PRED(5,OUTER_PT) = DBLE(OME_FIX)
        C_PRED(5,5,OUTER_PT) = 0.D0
      ENDIF
C Fixed lambda
      IF ( FIXED_TANL ) THEN
        X_PRED(4,OUTER_PT) = DATAN(DBLE(TANL_FIX))
        C_PRED(4,4,OUTER_PT) = 0.D0
      ENDIF
C Fixed momentum
      IF ( FIXED_P ) THEN
        X_PRED(5,OUTER_PT) = DSIGN(1.D0,OMEGA_TRK)/(DBLE(ROVERP*P_FIX)
     &    *DCOS(X_PRED(4,OUTER_PT)))
        C_PRED(5,5,OUTER_PT) = 0.D0
      ENDIF

C Create CORE and TAIL matrices

      CALL DVMOVE(C_PRED(1,1,OUTER_PT), C_PRED_CORE(1,1,OUTER_PT),25)
      CALL DVMOVE(C_PRED(1,1,OUTER_PT), C_PRED_TAIL(1,1,OUTER_PT),25)

C  Start loop over coordinates

      N_LOOP = N_LOOP + 1

C  Come here on VD patrec

  207 CONTINUE

C  Filtering stage:
C  Loop from outer point to inner coord

      DO K = OUTER_PT, INNER_PT
C  Is this a measured coordinate, or a material crossing?

        IF (I_COORD(K).NE.0)THEN

C  Check whether Z is really measured

          IF ( V_MEAS(2,2,K) .GT. MAX_ZVAR ) THEN
            IF ( PT_USED(K) .GT. 0 .AND. ISAFVD .EQ. 0 ) NCOORD_NOZ =
     &        NCOORD_NOZ + 1
            RES_PRED(2,K) = 0.D0
            R_MEAS(2,K)  = X_PRED(2,K)
          ENDIF

C Check whether r-phi is really measured

          IF ( V_MEAS(1,1,K) .GT. MAX_RPHIVAR ) THEN
            IF ( PT_USED(K) .GT. 0 .AND. ISAFVD .EQ. 0 )
     &        NCOORD_NORPHI = NCOORD_NORPHI + 1
            RES_PRED(1,K) = 0.D0
            R_MEAS(1,K)  = X_PRED(1,K)
          ENDIF

C Get kalman gain matrices
C Check whether the coordinate is to be used
C If not, the gain matrix is 0

          IF ( PT_USED(K) .GT. 0 )  THEN
            CALL UFGAIN(V_MEAS(1,1,K), C_PRED_CORE(1,1,K), DETW_CORE,
     &        W_CORE, GAIN_CORE)
            CALL UFGAIN(V_MEAS(1,1,K), C_PRED_TAIL(1,1,K), DETW_TAIL,
     &        W_TAIL, GAIN_TAIL)
          ELSE
            NBADC = NBADC + 1
            IF ( N_COORD_USED - NBADC .LT. MIN_COORD )THEN
              UFTKAL = 6
              GOTO 999
            ENDIF
            CALL DVZERO(GAIN_CORE,10)
            CALL DVZERO(GAIN_TAIL,10)
          ENDIF
 
C Calculate filtered state vector from gain matrix and residuals

          UFTKAL = UFSTAT(R_PT(K),X_PRED(1,K),RES_PRED(1,K),GAIN_CORE,
     &      X_FILT_CORE(1,K))
          IF (UFTKAL.NE.0) GO TO 999
          UFTKAL = UFSTAT(R_PT(K),X_PRED(1,K),RES_PRED(1,K),GAIN_TAIL,
     &      X_FILT_TAIL(1,K))
          IF (UFTKAL.NE.0) GO TO 999

C Filtered covariance matrices

          CALL UFCOVA(C_PRED_CORE(1,1,K),GAIN_CORE,C_FILT_CORE(1,1,K))
          CALL UFCOVA(C_PRED_TAIL(1,1,K),GAIN_TAIL,C_FILT_TAIL(1,1,K))

C Check whether fixed momentum requested

          IF ( FIXED_P )   THEN
            X_FILT_CORE(5,K) = DSIGN(1.D0,OMEGA_TRK)/(DBLE(ROVERP*P_FIX)
     &        *DCOS(X_FILT_CORE(4,K)))
            FACT = X_FILT_CORE(5,K) * DTAN(X_FILT_CORE(4,K))
            CALL UPCONS(FACT, C_FILT_CORE(1,1,K))

            X_FILT_TAIL(5,K) = DSIGN(1.D0,OMEGA_TRK)/(DBLE(ROVERP*P_FIX)
     &        *DCOS(X_FILT_TAIL(4,K)))
            FACT = X_FILT_TAIL(5,K) * DTAN(X_FILT_TAIL(4,K))
            CALL UPCONS(FACT, C_FILT_TAIL(1,1,K))
          ENDIF

C Determine weighting factors for both components

C Difference in gain matrices

          DO J = 1, 2
            DO I = 1, 5
              GAIN_DIFF(I,J) = GAIN_CORE(I,J) - GAIN_TAIL(I,J)
            ENDDO
          ENDDO

C DETW_x is only used to calculate the weighting between the scattering core
C and tail.  It should be possible to fit the track (using a zero gain matrix
C for this point) even if something went wrong in the determinant...
C         IF(DETW_CORE.EQ.0)THEN
C            UFTKAL = 7
C            GO TO 999
C          ENDIF
C          IF(DETW_TAIL/DETW_CORE.LT.0.D0)THEN
C            UFTKAL = 8
C            GO TO 999
C          ENDIF
          IF (PT_USED(K).GT.0 .AND. DETW_CORE.NE.0.
     &          .AND. DETW_TAIL*DETW_CORE.GT.0.) THEN
            FACT =  (W_CORE(1,1)-W_TAIL(1,1)) * RES_PRED(1,K)**2 +
     +        2*(W_CORE(1,2)-W_TAIL(1,2))*RES_PRED(1,K)*RES_PRED(2,K)
     +      + (W_CORE(2,2)-W_TAIL(2,2)) * RES_PRED(2,K)**2
            IF(FACT.GT.30.D0)THEN
              B_CORE(K) = 0.D0
            ELSE IF(FACT.LT.-30.D0)THEN
              B_CORE(K) = 1.D0
            ELSE
              B_CORE(K) = 1.D0/(1.D0 + (TAIL_FRACT/CORE_FRACT) *
     +                 DSQRT(DETW_TAIL/DETW_CORE) * DEXP( FACT ))
            ENDIF
            IF (B_CORE(K).GT.1.D0)THEN
              B_CORE(K) = 1.D0
            ELSE IF(B_CORE(K).LT.0.D0)THEN
              B_CORE(K) = 0.D0
            ENDIF
            B_TAIL(K) = 1.D0 - B_CORE(K)
          ELSE
            B_CORE(K) = CORE_FRACT
            B_TAIL(K) = TAIL_FRACT
          ENDIF

C  Form the combined filtered state vector

          DO I = 1, 5
            X_FILT(I,K) = B_CORE(K) * X_FILT_CORE(I,K) +
     +                       B_TAIL(K) * X_FILT_TAIL(I,K)
          ENDDO

C  Form the combined filtered covariance matrix

          DO J = 1, 5
            TMP = B_CORE(K) * B_TAIL(K) *
     $          (GAIN_DIFF(J,1) * RES_PRED(1,K) +
     +          GAIN_DIFF(J,2) * RES_PRED(2,K))
            DO I = 1, 5
              C_FILT(I,J,K) = B_CORE(K) * C_FILT_CORE(I,J,K) +
     +                          B_TAIL(K) * C_FILT_TAIL(I,J,K) +
     +                          (GAIN_DIFF(I,1) * RES_PRED(1,K) +
     +                          GAIN_DIFF(I,2) * RES_PRED(2,K)) *
     +                          TMP
            ENDDO
          ENDDO

C   Combined gain matrix for chi^2

          DO I = 1,2
            DO J = 1, 5
              GAIN_FILT(J,I) = B_CORE(K) * GAIN_CORE(J,I) +
     +                           B_TAIL(K) * GAIN_TAIL(J,I)
            ENDDO
          ENDDO

C  Filtered residuals

          DELTA_PHI = (RES_PRED(1,K) - GAIN_FILT(1,1)*RES_PRED(1,K)
     +               - GAIN_FILT(1,2)*RES_PRED(2,K))/ R_PT(K)
          CALL UBAPI(DELTA_PHI)
          RES_FILT(1,K) = DELTA_PHI*R_PT(K)
          RES_FILT(2,K) = RES_PRED(2,K) - GAIN_FILT(2,1)*RES_PRED(1,K)
     +                        - GAIN_FILT(2,2)*RES_PRED(2,K)

C  Covariance matrix of filtered residuals

          IF ( PT_USED(K) .GT. 0 ) THEN
C   ... for coordinates used in the fit
            RES_FILT_COV(1,1,K) = V_MEAS(1,1,K) - C_FILT(1,1,K)
            RES_FILT_COV(1,2,K) = V_MEAS(1,2,K) - C_FILT(1,2,K)
            RES_FILT_COV(2,1,K) = RES_FILT_COV(1,2,K)
            RES_FILT_COV(2,2,K) = V_MEAS(2,2,K) - C_FILT(2,2,K)
          ELSE
C   ... and for those presently not used
C   This seems to be a kludge for the case that the error on the fit is
C   larger than the error on the measurement.  Assuming the point is
C   included in the fit, and is not totally crazy, one would suppose the
C   fit would have smaller errors than the measurement.
            RES_FILT_COV(1,1,K) = V_MEAS(1,1,K) + C_FILT(1,1,K)
            RES_FILT_COV(1,2,K) = V_MEAS(1,2,K) + C_FILT(1,2,K)
            RES_FILT_COV(2,1,K) = RES_FILT_COV(1,2,K)
            RES_FILT_COV(2,2,K) = V_MEAS(2,2,K) + C_FILT(2,2,K)
          ENDIF

C  If the coordinate is filtered out, the rest is not necessary

          IF  ( PT_USED(K) .LE. 0 ) THEN
            IF ( K .GT. OUTER_PT ) THEN
              CHI2_FILT(K) = CHI2_FILT(K-1)
            ELSE
              CHI2_FILT(K) = 0.D0
            ENDIF
            GOTO 270
          ENDIF

C  Filtered chi-squared

          DET = RES_FILT_COV(1,1,K)*RES_FILT_COV(2,2,K)
     +          - RES_FILT_COV(1,2,K)**2
          IF ( ABS(DET) .LT. EPS1 ) DET = EPS1
C  Chi^2 contribution from this point...
          CHI2_FILT(K) = ( RES_FILT_COV(2,2,K)*RES_FILT(1,K)**2 +
     +      RES_FILT_COV(1,1,K)*RES_FILT(2,K)**2 -
     +      2.D0*RES_FILT_COV(1,2,K)*RES_FILT(1,K)*RES_FILT(2,K) ) / DET
C  ... and all previous points too
          IF ( K .GT. OUTER_PT ) CHI2_FILT(K) = CHI2_FILT(K)
     &      + CHI2_FILT(K-1)
C  Add to chi^2 of tpc fit during VDET patrec
          IF ( K.EQ.OUTER_PT .AND. ISBEVD.NE.0
     &      .AND. OUTER_PT.GT.OUTER_ALL )
     +                   CHI2_FILT(K) = CHI2_FILT(K) + CHI2_FILT(K-1)
  270     CONTINUE
        ELSE
C This is not a measurement - copy predicted to filtered
          CALL DVMOVE(X_PRED(1,K),X_FILT(1,K),5)
          CALL DVMOVE(C_PRED(1,1,K),C_FILT(1,1,K),25)
          CHI2_FILT(K) = CHI2_FILT(K-1)
        ENDIF

C Step 2 :  Prediction

        IF (K .LT. INNER_PT) THEN
C Discrete energy loss at this point

          CALL DVMOVE(X_FILT(1,K),TMP_X_FILT,5)
          CALL UFDISC(TMP_X_FILT,MASS,DE_DX(1,K))

C Nonlinear transport of helix parameters to next pad-row

          IERR = UFTRAN(R_PT(K), R_PT(K+1), TMP_X_FILT, T_PROC,
     &      X_PRED(1,K+1))
          IF(IERR.EQ.2)THEN

C  If this happens in vdet pattern recognition this is certainly
C  not a solution

            IF ( ISAFVD .NE. 0 ) THEN
              UFTKAL = 9
              GOTO 999
            ENDIF
            N_COORD_INNER = 0
            N_COORD_OUTER = 0
            DO I = OUTER_PT, INNER_COORD
              IF (I_COORD(I).NE.0)THEN
                IF (I.LT.K) THEN
                  N_COORD_OUTER = N_COORD_OUTER + 1
                ELSE
                  N_COORD_INNER = N_COORD_INNER + 1
                ENDIF
              ENDIF
            ENDDO
            IF ( N_COORD_OUTER .LE. N_COORD_INNER ) THEN
              IF (N_COORD_INNER.LT.MIN_COORD+1)THEN
                UFTKAL = 10
                GOTO 999
              ENDIF
              OUTER_PT = K + 1
              DO WHILE (I_COORD(OUTER_PT).EQ.0)
                OUTER_PT = OUTER_PT + 1
              ENDDO
            ELSE
              IF (N_COORD_OUTER.LT.MIN_COORD+1)THEN
                UFTKAL = 10
                GOTO 999
              ENDIF
              INNER_PT = K
            ENDIF
            N_LOOP = N_LOOP - 1
            GOTO 100
          ELSE IF (IERR.NE.0) THEN
            UFTKAL = 11
            GOTO 999
          ENDIF

C  Get Jacobian of transport mapping at smoothed point
C  (this is the approximation)

          IERR = UFJACO(R_PT(K),R_PT(K+1),X_SMOOTH(1,K),DF_DX(1,1,K))
          IF(IERR.NE.0) THEN
            UFTKAL = 12
            GO TO 999
          ENDIF

C  Apply effects of continuous energy loss to state vector

          CALL UFLOSS(X_PRED(1,K+1),DF_DX(1,1,K),MASS,DE_DX(2,K))

C  Apply requested constraints

          IF ( FIXED_OME ) THEN

C  Fixed curvature

            DO  I = 1, 5
              DF_DX(I,5,K) = 0.D0
              DF_DX(5,I,K) = 0.D0
            ENDDO
          ENDIF
          IF ( FIXED_TANL ) THEN
C  Fixed dip angle
            DO  I = 1, 5
              DF_DX(I,4,K) = 0.D0
              DF_DX(4,I,K) = 0.D0
            ENDDO
          ENDIF
          IF ( FIXED_P )  THEN
C   Fixed momentum
            DO  I = 1, 4
              DF_DX(I,4,K) = DF_DX(I,4,K) +
     +                    DF_DX(I,5,K)*DSIGN(1.D0,OMEGA_TRK)
     +                    *DTAN(X_SMOOTH(4,K))/DCOS(X_SMOOTH(4,K))/
     +                    (DBLE(ROVERP*P_FIX))
            ENDDO
            DO  I = 1, 5
              DF_DX(I,5,K) = 0.D0
              DF_DX(5,I,K) = 0.D0
            ENDDO
          ENDIF

C  Apply linearized error propagation
C  Predicted covariance matrix (no scattering)

          CALL UFMULT(DF_DX(1,1,K),C_FILT(1,1,K),DF_DX(1,1,K),
     $         C_PRED(1,1,K+1))

C  Add in process noise

          CALL UFMADD(C_PRED(1,1,K+1),CORE_FRACT*CORE_WIDTH, Q_PROC(1,1,
     &      K), C_PRED_CORE(1,1,K+1))
          CALL UFMADD(C_PRED(1,1,K+1),TAIL_FRACT*TAIL_WIDTH, Q_PROC(1,1,
     &      K), C_PRED_TAIL(1,1,K+1))
          CALL UFMADD(C_PRED(1,1,K+1),CORE_FRACT*CORE_WIDTH +
     &      TAIL_FRACT*TAIL_WIDTH, Q_PROC(1,1,K), C_PRED(1,1,K+1))

C  Get residuals

          IF (I_COORD(K+1).NE.0)THEN
            RES_PRED(1,K+1) = R_MEAS(1,K+1) - X_PRED(1,K+1)
            RES_PRED(2,K+1) = R_MEAS(2,K+1) - X_PRED(2,K+1)
            DELTA_PHI = RES_PRED(1,K+1) / R_PT(K+1)
            CALL UBAPI(DELTA_PHI)
            RES_PRED(1,K+1) = DELTA_PHI * R_PT(K+1)
          ENDIF

C  Loop over coordinates in filtering step ends here
        ENDIF
      ENDDO
  500 CONTINUE

      IF ( CHI2_FILT(INNER_PT) .LE. 0.D0 ) THEN
        UFTKAL = 13
        GOTO 999
      ENDIF

C  Check special requirements for vdet pattern recognition

      CHI2VD = SNGL(CHI2_FILT(INNER_PT))
      NVDOUT = 0
      IF ( ISBEVD .NE. 0 )   THEN
        DO 501  I = OUTER_PT+1, INNER_COORD
          IF (I_COORD(I).NE.0)THEN
            IF ( V_MEAS(1,1,I) .LT. MAX_RPHIVAR ) NVDOUT = NVDOUT+1
            IF ( V_MEAS(2,2,I) .LT. MAX_ZVAR ) NVDOUT = NVDOUT+1
          ENDIF
  501   CONTINUE
        IF ( ISAFVD .NE. 0 ) THEN
          ISAFVD = 0
          ISBEVD = 0
          UFTKAL = 0
          UFMKAL = 0
          TRACING = .FALSE.
          RETURN
        ENDIF
      ELSE IF ( ISAFVD .NE. 0 ) THEN
        JVD_OUTER = OUTER_PT
        DO WHILE (R_PT(JVD_OUTER).GE.UKRITC)
          JVD_OUTER = JVD_OUTER + 1
        ENDDO
        DO WHILE (I_COORD(JVD_OUTER).EQ.0)
          JVD_OUTER = JVD_OUTER + 1
        ENDDO
        DO I = J, INNER_COORD
          IF (I_COORD(I).NE.0)THEN
            IF ( V_MEAS(1,1,I) .LT. MAX_RPHIVAR )  NVDOUT = NVDOUT+1
            IF ( V_MEAS(2,2,I) .LT. MAX_ZVAR )  NVDOUT = NVDOUT+1
          ENDIF
        ENDDO
        CALL UFSWMX(R_PT(INNER_PT),X_FILT(1,INNER_PT),0.D0,
     &    DBL_TRK)
C  Beware: covariance matrix is not valid here!
        CALL ULOAD(DBL_TRK,C_NEW,TRK_OUT,COV_OUT)
        ISAFVD = 0
        ISBEVD = 0
        UFTKAL = 0
        UFMKAL = 0
        TRACING = .FALSE.
        RETURN
      ENDIF

C  VDET pattern recognition done, switch back to defaults

      ISAFVD = 0
      ISBEVD = 0
      OUTER_PT = OUTER_ALL

C  Set flag for normal smoothing

      LAST_ITER = .FALSE.

C  Check if fit configuration changed

      DO  I = OUTER_PT, INNER_PT
        IF ( PT_USED(I) .NE. USED_BEFORE(I) ) NBADCL = -1
      ENDDO

C  Check maximal number of iterations

      IF ( N_LOOP .LT. MAX_LOOP ) THEN

C If coordinate filtering is required, smoothing in the first step is
C     mandatory

        IF (N_LOOP .EQ. 1) GOTO 2000

C  If coordinates got filtered, smoothing has to be
C  repeated to recover some of them, if possible

        IF ( NBADC .NE. NBADCL ) GOTO 2000

C  Difference in phi0 and r-phi between smoothed and filtered states

        DELTA_PHI = X_FILT(3,INNER_COORD) - X_SMOOTH(3,INNER_COORD)
        CALL UBAPI(DELTA_PHI)
        DELTA_U = ( X_FILT(1,INNER_COORD) - X_SMOOTH(1,INNER_COORD) ) /
     &    R_PT(INNER_COORD)
        CALL UBAPI(DELTA_U)
        DELTA_U = DELTA_U * R_PT(INNER_COORD)

C For VDet pattern recognition, check if the chi2 is well above the
C     desired maximum. If so, abort now.

        IF (CHI2_FILT(INNER_COORD) -
     &       NCHI2DEL * ABS(CHI2_FILT(INNER_COORD) - CHI2_LAST)
     &       .GT. CHI2LIM) THEN
          UFTKAL = 22
          GOTO 999
        END IF

C  Check convergence.  If any test fails, go smooth and filter again.

        IF(ABS(CHI2_FILT(INNER_COORD)-CHI2_LAST)
     &     .GT.0.001D0*CHI2_LAST.OR.
     1     DELTA_U**2.GT.0.01D0*C_FILT(1,1,INNER_COORD) .OR.
     2     (X_FILT(2,INNER_COORD)-X_SMOOTH(2,INNER_COORD))**2
     2     .GT.0.01D0*C_FILT(2,2,INNER_COORD) .OR.
     3     DELTA_PHI**2.GT.0.01D0*C_FILT(3,3,INNER_COORD) .OR.
     4     (X_FILT(4,INNER_COORD)-X_SMOOTH(4,INNER_COORD))**2
     4     .GT.0.01D0*C_FILT(4,4,INNER_COORD) .OR.
     5     (X_FILT(5,INNER_COORD)-X_SMOOTH(5,INNER_COORD))**2
     5     .GT.0.01D0*C_FILT(5,5,INNER_COORD) ) GOTO 2000
      ENDIF

C  Filter finished: helix parameters at innermost coordinate

      UFTKAL = 20

      IF ( N_LOOP .GE. MAX_LOOP .AND.
     1   ( ABS(CHI2_FILT(INNER_COORD)-CHI2_LAST)
     1   .GT.0.001D0*CHI2_LAST.OR.
     2   DELTA_U**2.GT.0.01D0*C_FILT(1,1,INNER_COORD) .OR.
     3   (X_FILT(2,INNER_COORD)-X_SMOOTH(2,INNER_COORD))**2
     3   .GT.0.01D0*C_FILT(2,2,INNER_COORD) .OR.
     4   DELTA_PHI**2.GT.0.01D0*C_FILT(3,3,INNER_COORD) .OR.
     5   (X_FILT(4,INNER_COORD)-X_SMOOTH(4,INNER_COORD))**2
     5   .GT.0.01D0*C_FILT(4,4,INNER_COORD) .OR.
     6   (X_FILT(5,INNER_COORD)-X_SMOOTH(5,INNER_COORD))**2
     6   .GT.0.01D0*C_FILT(5,5,INNER_COORD) .OR.
     7   NBADC.NE.NBADCL ) ) GOTO 999

C  Clean up flags for coordinate usage

      DO  II = 1, OUTER_PT-1
        PT_USED(II) = -2
      ENDDO
      DO  II = INNER_PT+1, MAX_PTS
        PT_USED(II) = -2
      ENDDO
      IC = 0
      FIRST_COORD = 0
      DO IPT = 1, N_PT
        IF (I_COORD(IPT).NE.0)THEN
          IC = IC + 1
          I_COORD(IPT) = IC
          I_PT(IC) = IPT
          R_COORD(IC) = R_PT(IPT)
          COORD_USED(IC) = PT_USED(IPT)
          CALL DVMOVE(X_SMOOTH(1,IPT),X_COORD(1,IC),5)
          R_MEAS_COORD(1,IC) = R_MEAS(1,IPT)
          R_MEAS_COORD(2,IC) = R_MEAS(2,IPT)
          DO II = 1,2
            DO JJ = 1,2
              V_MEAS_COORD(II,JJ,IC) = V_MEAS(II,JJ,IPT)
            ENDDO
          ENDDO
          IF (IPT.GE.OUTER_PT .AND. FIRST_COORD.EQ.0) FIRST_COORD = IC
          IF (IPT.LE.INNER_PT) LAST_COORD = IC
        ENDIF
      ENDDO

C  Swim state vector from innermost point to origin
C  Get process time of inner-most coordinate

      IERR = UFGETT(R_PT(INNER_PT),X_FILT(1,INNER_PT),T_PROC)
      IF (IERR.NE.0) THEN
        UFTKAL = 14
        GOTO 999
      ENDIF
C  Do energy loss between last point and origin
      CALL DVMOVE(X_FILT(1,INNER_PT),TMP_X_FILT,5)
      CALL UFDISC(TMP_X_FILT,MASS,DE_DX(1,INNER_PT))
      CALL UFSWMX(R_PT(INNER_PT),TMP_X_FILT,
     &    T_PROC, DBL_TRK)

C  Covariance matrix for output parameters

      CALL UFSWMC(R_PT(INNER_PT),TMP_X_FILT,T_PROC,
     &  C_FILT(1,1,INNER_PT),MASS,C_NEW)

C  Add multiple scattering to origin if swimming is requested

      IF (AT_VTX) CALL UFSWMS(C_NEW, Q_PROC(1,1,INNER_PT), C_NEW)

C  Load output arrays

      CALL ULOAD(DBL_TRK, C_NEW, TRK_OUT, COV_OUT)
      CHI2_OUT = SNGL(CHI2_FILT(INNER_COORD))

C  Get number of degrees of freedom

      N_COORD_USED = N_COORD_USED - NBADC
      N_DOF = 2 * N_COORD_USED - 5

C  Subtract non-measured coordinates

      N_DOF = N_DOF - NCOORD_NOZ
      N_DOF = N_DOF - NCOORD_NORPHI

C  If no z-coordinate is measured, consider this as circle fit

      IF ( NCOORD_NOZ .EQ. N_COORD_USED ) N_DOF = N_DOF + 2

      IF ( N_DOF .LT. 1 ) THEN
        UFTKAL = 15
        GOTO 999
      ENDIF

C  Check whether user requested constraints

      IF ( FIXED_OME ) N_DOF = N_DOF + 1
      IF ( FIXED_TANL ) N_DOF = N_DOF + 1
      IF ( FIXED_P ) N_DOF = N_DOF + 1

      LAST_ITER = .TRUE.

C  Step 3 :  Smoothing
C  If LAST_ITER = .FALSE.   :  General smoothing for next iteration
C  If LAST_ITER = .TRUE.    :  Do full smoothing, including covariance

 2000 CONTINUE

      DO_SMTH_COV = LAST_ITER .OR.
     $     (FILTER_PROB .GT. 0. .AND. (N_LOOP .EQ. 1 .OR. NBADC .GT. 0))

      N_COORD_USED = 0
      DO I = OUTER_PT, INNER_PT
        IF (I_COORD(I).NE.0) N_COORD_USED = N_COORD_USED + 1
      ENDDO
      CHI2_LAST = CHI2_FILT(INNER_COORD)

C  Accounting of worst 1D and 2D hits failing cuts found on this pass

      WORST_2D = 0.
      WORST_1D = 0.
      IWORST_1D = 0
      IWORST_2D = 0

C  Accounting of best currently unused 1D and 2D hits passing cuts
C  found on this pass through.

      BEST_2D = 1.E30
      BEST_1D = 1.E30
      IBEST_1D = 0
      IBEST_2D = 0

C  Accounting of coordinate usage.

      NBADCL = NBADC
      DO  I = OUTER_PT, INNER_PT
        USED_BEFORE(I) = PT_USED(I)
      ENDDO

      CALL DVMOVE(X_FILT(1,INNER_PT),X_SMOOTH(1,INNER_PT),5)

      IF (DO_SMTH_COV) THEN
        CALL DVMOVE(C_FILT(1,1,INNER_PT), C_SMOOTH(1,1,INNER_PT),25)
        IF (I_COORD(INNER_PT).NE.0)THEN
          RES_SMOOTH(1,INNER_COORD) = RES_FILT(1,INNER_COORD)
          RES_SMOOTH(2,INNER_COORD) = RES_FILT(2,INNER_COORD)
          RES_SMOOTH_COV(1,1,INNER_COORD) =
     &      RES_FILT_COV(1,1,INNER_COORD)
          RES_SMOOTH_COV(1,2,INNER_COORD) =
     &      RES_FILT_COV(1,2,INNER_COORD)
          RES_SMOOTH_COV(2,1,INNER_COORD) =
     &      RES_FILT_COV(2,1,INNER_COORD)
          RES_SMOOTH_COV(2,2,INNER_COORD) =
     &      RES_FILT_COV(2,2,INNER_COORD)
        ENDIF
      ENDIF

      DELTA_PHI = (X_SMOOTH(1,INNER_PT)-X_PRED(1,INNER_PT))
     &  /R_PT(INNER_PT)
      IF ( DELTA_PHI .GT. ONE_PI )X_SMOOTH(1,INNER_PT) =
     &  X_SMOOTH(1,INNER_PT) - TWO_PI*R_PT(INNER_PT)
      IF ( DELTA_PHI .LT.-ONE_PI )X_SMOOTH(1,INNER_PT) =
     &  X_SMOOTH(1,INNER_PT) + TWO_PI*R_PT(INNER_PT)
      DELTA_PHI = X_SMOOTH(3,INNER_PT) - X_PRED(3,INNER_PT)
      IF ( DELTA_PHI .LT.-ONE_PI )X_SMOOTH(3,INNER_PT) =
     &  X_SMOOTH(3,INNER_PT) + TWO_PI
      IF ( DELTA_PHI .GT. ONE_PI )  X_SMOOTH(3,INNER_PT) =
     &  X_SMOOTH(3,INNER_PT) - TWO_PI
      CALL UBAPI2(X_SMOOTH(4,INNER_PT))

C  Find range of coordinates to smooth

      LAST_SMOOTH = OUTER_PT

C  Find coordinate near TPC wall
      IF (LAST_ITER) THEN
        IF (R_PT(INNER_PT) .GE. DBLE(UKRTPC)) THEN
          LAST_TPC = 0
        ELSE
          I = OUTER_PT
          DO WHILE(R_PT(I) .GE. DBLE(UKRTPC) .AND. I.LE.INNER_COORD)
            I = I + 1
          ENDDO
          LAST_TPC = I-1
          FIRST_INNER = I
          DO WHILE (I_COORD(LAST_TPC).EQ.0)
            LAST_TPC = LAST_TPC - 1
          ENDDO
          DO WHILE (I_COORD(FIRST_INNER).EQ.0)
            FIRST_INNER = FIRST_INNER + 1
          ENDDO
        ENDIF
      ENDIF

C  Smooth from innermost coordinate

      DO  690  K = INNER_PT, LAST_SMOOTH, -1
        IF ( K .EQ. INNER_PT )   GOTO 688

C  Smoother gain matrix

        DO  I = 1, 5
          IF ( C_PRED(I,I,K+1) .LT. 0.D0 ) THEN
            UFTKAL = 16
            GOTO 999
          ENDIF
          WT_PRED(I) = DSQRT(C_PRED(I,I,K+1))
        ENDDO

C  Check whether user requested constraint

        IF ( FIXED_OME ) WT_PRED(5) = 1.D0
        IF ( FIXED_TANL ) WT_PRED(4) = 1.D0
        IF ( FIXED_P ) WT_PRED(5) = 1.D0

C  Protect against divide by zero after checking constraint mode
        DO I = 1, 5
            IF (WT_PRED(I).EQ.0.D0) THEN
                UFTKAL = 25
                GOTO 999
            ENDIF
        ENDDO

C    c_temp(i,j) = c_pred(i,j) / (wt_pred(i) * wt_pred(j))
        CALL UFMSCL( C_TEMP, C_PRED(1,1,K+1), WT_PRED, 5 )

C  Handle constraints

        IF ( FIXED_OME ) C_TEMP(5,5) = 1.D0
        IF ( FIXED_TANL ) C_TEMP(4,4) = 1.D0
        IF ( FIXED_P ) THEN
          C_TEMP(5,5) = 1.D0
          C_TEMP(4,5) = 0.D0
          C_TEMP(5,4) = 0.D0
          C_TEMP(3,5) = 0.D0
          C_TEMP(5,3) = 0.D0
          C_TEMP(2,5) = 0.D0
          C_TEMP(5,2) = 0.D0
          C_TEMP(1,5) = 0.D0
          C_TEMP(5,1) = 0.D0
        ENDIF

C  Invert 5x5
C  Give up on this track if the matrix inversion fails because the
C  matrix is not positive definite

        CALL DSINV (5,C_TEMP,5,IFAIL)
        IF (IFAIL.NE.0) THEN
          UFTKAL = 17
          GOTO 999
        ENDIF

        MATDIM = 5
        IF ( FIXED_P ) MATDIM = 4

C    c_temp(i,j) = c_temp(i,j) / (wt_pred(i) * wt_pred(j))
        CALL UFMSCL( C_TEMP, C_TEMP, WT_PRED, MATDIM )

C   c_work = c_filt * df_dx^T
        CALL UFMMLT(C_WORK, C_FILT(1,1,K), DF_DX(1,1,K), MATDIM)

C   gain_smooth = c_work * c_temp
        CALL UFMMUL(GAIN_SMOOTH, C_WORK, C_TEMP, MATDIM)

C  Smoothed state vector
        CALL DVMOVE(X_FILT(1,K),X_SMOOTH(1,K),MATDIM)
        DO  J = 1, MATDIM
          TMP = X_SMOOTH(J,K+1) - X_PRED(J,K+1)
C  Protect against a 0->2*pi problem
          IF (J.EQ.1) THEN
            DELTA_PHI = TMP/R_PT(K+1)
            CALL UBAPI(DELTA_PHI)
            TMP = R_PT(K+1) * DELTA_PHI
          ELSE IF (J.EQ.3) THEN
            CALL UBAPI(TMP)
          ENDIF
          DO  I = 1, MATDIM
             X_SMOOTH(I,K) = X_SMOOTH(I,K) + GAIN_SMOOTH(I,J) * TMP
          ENDDO
        ENDDO

        PHI_NEW = X_SMOOTH(1,K) / R_PT(K)
        CALL UBA2PI(PHI_NEW)
        DELTA_PHI = PHI_NEW-X_PRED(1,K)/R_PT(K)
        IF ( DELTA_PHI .GT. ONE_PI )  PHI_NEW = PHI_NEW - TWO_PI
        IF ( DELTA_PHI .LT.-ONE_PI )  PHI_NEW = PHI_NEW + TWO_PI
        X_SMOOTH(1,K) = PHI_NEW * R_PT(K)
        CALL UBA2PI(X_SMOOTH(3,K))
        DELTA_PHI = X_SMOOTH(3,K) - X_PRED(3,K)
        IF ( DELTA_PHI .GT. ONE_PI )  X_SMOOTH(3,K) = X_SMOOTH(3,K) - TWO_PI
        IF ( DELTA_PHI .LT.-ONE_PI )  X_SMOOTH(3,K) = X_SMOOTH(3,K) + TWO_PI
        CALL UBAPI2(X_SMOOTH(4,K))

C  Check whether user requested fixed momentum

        IF ( FIXED_P )  THEN
          X_SMOOTH(5,K) = DSIGN(1.D0,OMEGA_TRK) / (DBLE(ROVERP*P_FIX)
     &      *DCOS(X_SMOOTH(4,K)))
        ENDIF

C  If coordinate filtering already done or not required ...
C  get out of here

  688   CONTINUE
        IF (.NOT. DO_SMTH_COV) GOTO 690
        IF ( K .EQ. INNER_PT ) GOTO 689

C  Smoothed covariance matrix

        CALL UFSCOV(C_SMOOTH(1,1,K+1), C_PRED(1,1,K+1), C_FILT(1,1,K),
     $       GAIN_SMOOTH, C_SMOOTH(1,1,K), MATDIM)

C  Check if user requested fixed momentum

        IF ( FIXED_P )  THEN
          FACT = X_SMOOTH(5,K) * DTAN(X_SMOOTH(4,K))
          C_SMOOTH(5,5,K) = FACT * FACT * C_SMOOTH(4,4,K)
          C_SMOOTH(1,5,K) = FACT * C_SMOOTH(1,4,K)
          C_SMOOTH(5,1,K) = C_SMOOTH(1,5,K)
          C_SMOOTH(2,5,K) = FACT * C_SMOOTH(1,4,K)
          C_SMOOTH(5,2,K) = C_SMOOTH(1,5,K)
          C_SMOOTH(3,5,K) = FACT * C_SMOOTH(1,4,K)
          C_SMOOTH(5,3,K) = C_SMOOTH(1,5,K)
          C_SMOOTH(4,5,K) = FACT * C_SMOOTH(1,4,K)
          C_SMOOTH(5,4,K) = C_SMOOTH(1,5,K)
        ENDIF

C  Smoothed residuals

        IF (I_COORD(K).NE.0) THEN
          DELTA_PHI = ( RES_FILT(1,K) - (X_SMOOTH(1,K)-X_FILT(1,K)) ) /
     +                  R_PT(K)
          CALL UBAPI(DELTA_PHI)
          RES_SMOOTH(1,K) = DELTA_PHI * R_PT(K)
          RES_SMOOTH(2,K) = RES_FILT(2,K) - (X_SMOOTH(2,K)-X_FILT(2,K))

C  Covariance matrix of smoothed residuals

          IF ( PT_USED(K) .GT. 0 )  THEN
C  For a coordinate which was used in the fit
            RES_SMOOTH_COV(1,1,K) = V_MEAS(1,1,K) - C_SMOOTH(1,1,K)
            RES_SMOOTH_COV(1,2,K) = V_MEAS(1,2,K) - C_SMOOTH(1,2,K)
            RES_SMOOTH_COV(2,1,K) = RES_SMOOTH_COV(1,2,K)
            RES_SMOOTH_COV(2,2,K) = V_MEAS(2,2,K) - C_SMOOTH(2,2,K)
          ELSE
C  ...and a coordinate which was not used in the fit
            RES_SMOOTH_COV(1,1,K) = V_MEAS(1,1,K) + C_SMOOTH(1,1,K)
            RES_SMOOTH_COV(1,2,K) = V_MEAS(1,2,K) + C_SMOOTH(1,2,K)
            RES_SMOOTH_COV(2,1,K) = RES_SMOOTH_COV(1,2,K)
            RES_SMOOTH_COV(2,2,K) = V_MEAS(2,2,K) + C_SMOOTH(2,2,K)
          ENDIF
        ENDIF
  689   CONTINUE

C  Calculate chi^2 contribution of present point

        IF (I_COORD(K).NE.0) THEN
          DET = RES_SMOOTH_COV(1,1,K)*RES_SMOOTH_COV(2,2,K) -
     +                            RES_SMOOTH_COV(1,2,K)**2

C  In case of numerical instability use the safe assumption
C  that the fit has small errors compared to the coordinate
C  VALID FOR VDET POINTS???

          IF ( DET .LE. 0.D0 )  THEN
            RES_SMOOTH_COV(1,1,K) = V_MEAS(1,1,K)
            RES_SMOOTH_COV(2,2,K) = V_MEAS(2,2,K)
            RES_SMOOTH_COV(1,2,K) = V_MEAS(1,2,K)
            RES_SMOOTH_COV(2,1,K) = V_MEAS(2,1,K)
            DET = RES_SMOOTH_COV(1,1,K)*RES_SMOOTH_COV(2,2,K) -
     +                                 RES_SMOOTH_COV(1,2,K)**2
            IF ( DET .LE. 0.D0 ) THEN
              UFTKAL = 18
              GOTO 999
            ENDIF
          ENDIF

C  Smoothed chi^2

          CHI2 =  (RES_SMOOTH_COV(2,2,K)*RES_SMOOTH(1,K)**2 +
     +      RES_SMOOTH_COV(1,1,K)*RES_SMOOTH(2,K)**2 -
     +      2.D0*RES_SMOOTH_COV(1,2,K)*RES_SMOOTH(1,K)*RES_SMOOTH(2,K))
     +      /DET

C  Calculate smoothed fit without this point.
C  This calculation doesn't make sense if the point is already not included...
C  First get trimming gain matrix.

          IF (N_COORD_USED .GT. MIN_COORD)THEN
            DO I = 1, 5
              GAIN_TRIM(I,1) =
     &          -(C_SMOOTH(I,1,K) * RES_SMOOTH_COV(2,2,K) -
     &          C_SMOOTH(I,2,K) * RES_SMOOTH_COV(1,2,K))/DET
              GAIN_TRIM(I,2) =
     &          -(- C_SMOOTH(I,1,K) * RES_SMOOTH_COV(1,2,K) +
     &          C_SMOOTH(I,2,K) * RES_SMOOTH_COV(1,1,K))/DET
            ENDDO

C  Calculate trimmed state vector, covariance matrix, and residuals

            UFTKAL = UFSTAT(R_PT(K), X_SMOOTH(1,K), RES_SMOOTH(1,K),
     &        GAIN_TRIM, X_TRIM(1,K))
            IF (UFTKAL.NE.0) GO TO 999
            CALL UFCOVA(C_SMOOTH(1,1,K), GAIN_TRIM, C_TRIM(1,1,K))

C  Trimmed residuals

            DELTA_PHI = ( RES_FILT(1,K) - (X_TRIM(1,K)-X_FILT(1,K)) ) /
     +                  R_PT(K)
            CALL UBAPI(DELTA_PHI)
            RES_TRIM(1,K) = DELTA_PHI * R_PT(K)
            RES_TRIM(2,K) = RES_FILT(2,K) - (X_TRIM(2,K)-X_FILT(2,K))

C  Covariance matrix of trimmed residuals

            IF ( PT_USED(K) .GT. 0 )  THEN
              RES_TRIM_COV(1,1,K) = V_MEAS(1,1,K) - C_TRIM(1,1,K)
              RES_TRIM_COV(1,2,K) = V_MEAS(1,2,K) - C_TRIM(1,2,K)
              RES_TRIM_COV(2,1,K) = RES_TRIM_COV(1,2,K)
              RES_TRIM_COV(2,2,K) = V_MEAS(2,2,K) - C_TRIM(2,2,K)
            ELSE
              RES_TRIM_COV(1,1,K) = V_MEAS(1,1,K) + C_TRIM(1,1,K)
              RES_TRIM_COV(1,2,K) = V_MEAS(1,2,K) + C_TRIM(1,2,K)
              RES_TRIM_COV(2,1,K) = RES_TRIM_COV(1,2,K)
              RES_TRIM_COV(2,2,K) = V_MEAS(2,2,K) + C_TRIM(2,2,K)
            ENDIF
          ELSE
            CALL DVZERO(X_TRIM(1,K),5)
          ENDIF

C  Check if coordinate has one or two degrees of freedom

          CHI2_CUT = DBLE(CH2_2D)
          NDEG_CUT = 2
          IF ( V_MEAS(2,2,K) .GT. MAX_ZVAR .OR.
     +       V_MEAS(1,1,K) .GT. MAX_RPHIVAR )  THEN
            CHI2_CUT = DBLE(CH2_1D)
            NDEG_CUT = 1
          ENDIF

C  Recover coordinates

          IF (PT_USED(K).EQ.0 .AND. CHI2.LE.CHI2_CUT .AND.
     +          .NOT. LAST_ITER ) THEN
            IF(NDEG_CUT .EQ. 1)THEN
              IF (CHI2 .LT. BEST_1D) THEN
                BEST_1D = CHI2
                IBEST_1D = K
              ENDIF
            ELSE IF(NDEG_CUT.EQ.2)THEN
              IF(CHI2.LT.BEST_2D)THEN
                BEST_2D = CHI2
                IBEST_2D = K
              ENDIF
            ENDIF
          ENDIF
C
C  Test for bad coordinates here
C
C
C  Be more careful about throwing away the last few coordinates of a track
C
          CHI2_CUT = CHI2_CUT *
     &        (1.+DEXP(-DBLE(N_COORD_USED)/MIN_COORD))
C
          IF( (.NOT. LAST_ITER) .AND.
     +      (CHI2 .GE. CHI2_CUT) .AND.
     +      (PT_USED(K) .GT. 0) .AND.
     +      ((N_COORD_USED-NCOORD_NOZ-NCOORD_NORPHI)
     +          .GT.MIN_COORD+1))THEN
C     +      N_COORD_USED.GT.MIN_COORD+1)THEN
            IF(NDEG_CUT.EQ.1)THEN
              IF (CHI2 .GT. WORST_1D)THEN
                WORST_1D = CHI2
                IWORST_1D = K
              ENDIF
            ELSE
              IF (CHI2 .GT. WORST_2D)THEN
                WORST_2D = CHI2
                IWORST_2D = K
              ENDIF
            ENDIF
          ENDIF
        ENDIF
  690 CONTINUE

C Dump the filtered, predicted, and smoothed state vector for each point,
C if requested.

      IF (TRACING) THEN
        WRITE(IW(6),*)'UFTKAL trace at N_LOOP=',N_LOOP
        DO K=OUTER_PT,INNER_PT
            WRITE(IW(6),691)'FILT:',K,(SNGL(X_FILT(KK,K)),KK=1,5)
            WRITE(IW(6),691)'PRED:',K,(SNGL(X_PRED(KK,K)),KK=1,5)
            WRITE(IW(6),691)'SMOO:',K,(SNGL(X_SMOOTH(KK,K)),KK=1,5)
 691        FORMAT(1X,A5,1X,I2,5(1X,1PE15.8))
        ENDDO
        WRITE(IW(6),692)CHI2_LAST,CHI2
 692    FORMAT(1X,'Filtered, smoothed chi^2:',2(1PE15.8))
      ENDIF

C  Now check whether it is necessary to remove hits

      IF (IWORST_1D .NE. 0 .OR. IWORST_2D .NE.0)THEN
        IF (IWORST_1D .NE. 0 .AND. IWORST_2D .EQ. 0)THEN
          IKILL = IWORST_1D
          WORST = WORST_1D
        ELSE IF(IWORST_2D .NE. 0 .AND. IWORST_1D .EQ. 0)THEN
          IKILL = IWORST_2D
          WORST = WORST_2D
        ELSE
          PROB_1D = UFCHI2(WORST_1D,1)
          PROB_2D = UFCHI2(WORST_2D,2)
          IF(PROB_1D.LE.PROB_2D)THEN
            IKILL = IWORST_1D
            WORST = WORST_1D
          ELSE
            IKILL = IWORST_2D
            WORST = WORST_2D
          ENDIF
        ENDIF
        IF (PT_USED(IKILL).EQ.1)THEN
          PT_USED(IKILL) = 0
        ELSE
C  Flag coordinate to *never* readd to track
          PT_USED(IKILL) = -1
        ENDIF
      ELSE IF(IBEST_1D .NE. 0 .OR. IBEST_2D .NE. 0)THEN
C  Can only add if nothing was removed
        IF (IBEST_1D .NE. 0 .AND. IBEST_2D .EQ. 0)THEN
          ISAVE = IBEST_1D
          BEST = BEST_1D
        ELSE IF(IBEST_2D .NE. 0 .AND. IBEST_1D .EQ. 0)THEN
          ISAVE = IBEST_2D
          BEST = BEST_2D
        ELSE
          PROB_1D = UFCHI2(BEST_1D,1)
          PROB_2D = UFCHI2(BEST_2D,2)
          IF (PROB_1D .GT. PROB_2D)THEN
            ISAVE = IBEST_1D
            BEST = BEST_1D
          ELSE
            ISAVE = IBEST_2D
            BEST = BEST_2D
          ENDIF
        ENDIF
        PT_USED(ISAVE) = 2
      ENDIF

C  Smoothing done .... ready to reiterate filter

      IF  ( .NOT. LAST_ITER ) GOTO 1000

C  Smoothing to tpc done  ... get mult.scat. angle

 4000 CONTINUE
      TRK_OUT(6)  = 0.
      C_NEW(21) = 1.D0
      IF  ( LAST_TPC .GE. OUTER_PT
     &      .AND. FIRST_INNER.LT. INNER_PT) THEN
        IERR = UFTRAN(R_PT(LAST_TPC),DBLE(UKRTPC),
     &      X_SMOOTH(1,LAST_TPC),T_PROC,TMP_X_SMOOTH)
        IF (IERR.NE.0) THEN
          UFTKAL = 19
          GOTO 999
        ENDIF
        PHI_NEW =  TMP_X_SMOOTH(3)
        IERR = UFTRAN(R_PT(FIRST_INNER),DBLE(UKRTPC),
     &      X_SMOOTH(1,FIRST_INNER),T_PROC,TMP_X_SMOOTH)
        IF (IERR.NE.0) THEN
          UFTKAL = 19
          GOTO 999
        ENDIF
        PHI_OLD =  TMP_X_SMOOTH(3)
        DELTA_PHI = PHI_OLD - PHI_NEW
        CALL UBAPI(DELTA_PHI)
        TRK_OUT(6) = SNGL(DELTA_PHI)
        COV_OUT(21) = TRK_OUT(6)**2
      ENDIF
      UFTKAL = 0
      UFMKAL = 0
      TRACING = .FALSE.

      RETURN

  999 CONTINUE

C  Jump here if something goes crazy

      NVDOUT = 0
      CHI2VD = 1.0E30
      CHI2_OUT = 1.0E30
      ISAFVD = 0
      ISBEVD = 0
      UFMKAL = UFTKAL
      IF (TRACING)
     1      WRITE(IW(6),*)'Kalman filter exiting with error:',UFTKAL,
     2                    ' at N_LOOP =',N_LOOP

      TRACING = .FALSE.

      RETURN
C
C -------------------------------------------------------------
C
      ENTRY UFTRAC(LTRACE)
      TRACING = LTRACE
      RETURN
      
C
C --------------------------------------------------------------
C
      ENTRY UFTOME(IFLAG)
C  Steering for curvature constraint
      FIXED_OME = .TRUE.
      IF ( IFLAG .EQ. 0 )  FIXED_OME = .FALSE.
      RETURN
C
C ---------------------------------------------------------------
C
      ENTRY UFTTAL(IFLAG)
C  Steering for tan(lambda) constraint
      FIXED_TANL = .TRUE.
      IF ( IFLAG .EQ. 0 )  FIXED_TANL = .FALSE.
      RETURN
C
C ----------------------------------------------------------------
C
      ENTRY UFTMOM(IFLAG)
C  Steering for momentum constraint
      FIXED_P = .TRUE.
      IF ( IFLAG .EQ. 0 )  FIXED_P = .FALSE.
      IF ( FIXED_P ) THEN
        FIXED_OME = .FALSE.
        FIXED_TANL = .FALSE.
      ENDIF
      RETURN
C
C ----------------------------------------------------------------
      ENTRY UFTRAD(R4_RAD)
C   Steering for radial cut on coordinates
      RAD_CUT = MAX(0.,R4_RAD)
      RETURN
C
C ------------------------------------------------------------------
C
      ENTRY DALINF(IFLAG)
C  Switching for output of local helix parameters
C  ** pointless **
C  (i.e. now done by default...entry retained for compatibility)

      FILL_HELIX = IFLAG .NE. 0
      RETURN
C
C --------------------------------------------------------------------
C
      ENTRY UFDALI(DAL_FIRST,DAL_LAST,DAL_STATE,DAL_RMEAS,
     &  DAL_RCOORD,DAL_USED,DAL_VMEAS)
C  Incredibly stupid entry point to pass /INFDAL/ the hard way...
      DAL_FIRST = FIRST_COORD
      DAL_LAST = LAST_COORD
      DO IC = 1, MAX_PTS
        DAL_USED(IC) = COORD_USED(IC)
        DAL_RCOORD(IC) = R_COORD(IC)
        DAL_RMEAS(1,IC) = R_MEAS_COORD(1,IC)
        DAL_RMEAS(2,IC) = R_MEAS_COORD(2,IC)
        DAL_VMEAS(1,1,IC) = V_MEAS_COORD(1,1,IC)
        DAL_VMEAS(2,1,IC) = V_MEAS_COORD(2,1,IC)
        DAL_VMEAS(1,2,IC) = V_MEAS_COORD(1,2,IC)
        DAL_VMEAS(2,2,IC) = V_MEAS_COORD(2,2,IC)
        DAL_STATE(1,IC) = X_COORD(1,IC)
        DAL_STATE(2,IC) = X_COORD(2,IC)
        DAL_STATE(3,IC) = X_COORD(3,IC)
        DAL_STATE(4,IC) = X_COORD(4,IC)
        DAL_STATE(5,IC) = X_COORD(5,IC)
      ENDDO
      RETURN
C
C -------------------------------------------------------------------
C
      ENTRY UFSWIM(IFLAG)
C  Switching for swimming to track origin
      AT_VTX = IFLAG .NE. 0
      RETURN
C
C ------------------------------------------------------------------
C
      ENTRY UFCLIM(PR)
C Set the upper limit for the chi^2. Useful in VGLOB VDet pattern rec.
      CHI2LIM = PR
      RETURN
C
C ---------------------------------------------------------------------
C
      ENTRY UFVDIN(MVDIN,JSAFVD,JSBEVD)
C  Input for vdet pattern recognition steering
      NVDIN  = MVDIN
      ISAFVD = JSAFVD
      ISBEVD = JSBEVD
      RETURN
C
C ----------------------------------------------------------------------
C
      ENTRY UFVDOU(MVDOUT,CHSVD)
C  Output from special vdet filter step
      MVDOUT = NVDOUT
      CHSVD  = CHI2VD
      RETURN
C
C -------------------------------------------------------------------------
C

      ENTRY UF2RES(RES_TRIM_OUT, TRIM_ERR_OUT, FIT_TRIM_OUT,
     &              FIT_ERR_OUT )

C  return the residual for each point, using a fit which excludes it

      IC = 0
      DO IPT = 1, N_PT
        IF(I_COORD(IPT).NE.0)THEN
          IC = IC + 1
          RES_TRIM_OUT(1,IC) = SNGL(RES_TRIM(1,IPT))
          RES_TRIM_OUT(2,IC) = SNGL(RES_TRIM(2,IPT))
          FIT_ERR_OUT(1,IC) = SNGL(C_SMOOTH(1,1,IPT))
          FIT_ERR_OUT(2,IC) = SNGL(C_SMOOTH(2,2,IPT))
          TRIM_ERR_OUT(1,IC) = SNGL(C_TRIM(1,1,IPT))
          TRIM_ERR_OUT(2,IC) = SNGL(C_TRIM(2,2,IPT))
          FIT_TRIM_OUT(1,IC) = SNGL(X_TRIM(1,IPT))
          FIT_TRIM_OUT(2,IC) = SNGL(X_TRIM(2,IPT))
        ENDIF
      ENDDO
      RETURN
C
C -----------------------------------------------------------------
C
      ENTRY UF2TRM(ICOORD, TRK_TRIM_OUT, COV_TRIM_OUT)
C  Get the trimmed helix and its error IN GLOBAL COORDINATES
C  at a given coordinate.
      ENTRY UF2TPO(ICOORD, TRK_TRIM_OUT, COV_TRIM_OUT, POS_TRIM_OUT)
C  Same as UF2TRM, but also return the fit position at that point

      IPT = I_PT(ICOORD)

      IF (X_TRIM(1,IPT).EQ.0.D0)THEN
        UF2TRM = 23
        UF2TPO = 23
        RETURN
      ENDIF

C  Get process time at coordinate

      IERR = UFGETT(R_COORD(ICOORD),X_TRIM(1,IPT),T_PROC)
      IF (IERR.NE.0) THEN
        UF2TRM = 14
        UF2TPO = 14
        RETURN
      ENDIF

C  Swim the state vector to the origin

      CALL UFSWMX(R_COORD(ICOORD),X_TRIM(1,IPT), T_PROC, DBL_TRK)

C  Covariance matrix for output parameters

      CALL UFSWMC(R_COORD(ICOORD),X_FILT(1,IPT),T_PROC,C_TRIM(1,1,
     &  IPT),MASS,C_NEW)

C  Load output arrays

      CALL ULOAD(DBL_TRK, C_NEW, TRK_TRIM_OUT, COV_TRIM_OUT)
      POS_TRIM_OUT(1) = X_TRIM(1,IPT)
      POS_TRIM_OUT(2) = X_TRIM(2,IPT)

      UF2TRM = 0
      UF2TPO = 0

      RETURN
C
C ---------------------------------------------------------------------
C
      ENTRY UF2PNT(FIELD, R_PNT, X_PNT, C_PNT)

C Make the best possible estimate of track parameters and their errors
C at a given radius.  Three cases are possible. The point can be:
C      1) inside the innermost coordinate,
C      2) between two coordinates, or
C      3) outside the outermost coordinate.
C
C Cases 1 and 3 are the easiest, as the track parameters are merely
C those at the nearest coordinate, adjusted for energy loss, and
C only the covariance matrix need be swum.  Case 2 is harder, because we
C want to interpolate between two points.
C
C The procedure is to run an abbreviated version of the
C predict+filter+smooth, procedure, using only the nearest one or two
C coordinates, and assuming the gain matrix at the input point is zero
C (this is equivalent to calling the fitter with a point with huge
C errors). Also, we can assume most of the needed quantities are still
C lying around from the last call.

C Begin by determining what case we are dealing with.

      R_USE = DBLE(R_PNT)
C First, check if the radius is equal to one of the points. Only make
C     the comparison within 0.01cm, since this is too small to be relevant
C     and it protects against rounding errors.

      DO I_IN = OUTER_PT, INNER_PT
        IF ( ABS( R_USE - R_PT(I_IN)) .LT. 0.01  ) THEN
C Asked for helix at a supplied point
          DO I = 1, 5
            TMP_X_SMOOTH(I) = X_SMOOTH(I,I_IN)
            DO J = 1, 5
              TMP_C_SMOOTH(J,I) = C_SMOOTH(J,I,I_IN)
            END DO
          END DO
          GOTO 4500
        END IF
      END DO

C We need to interpolate/extrapolate.
C Make sure we only work with valid coordinates which are actually on the
C track.

      OUTER_USE = OUTER_PT
      INNER_USE = INNER_PT
      DO WHILE (PT_USED(OUTER_USE).LE.0 .AND. OUTER_USE.LT.INNER_USE)
        OUTER_USE = OUTER_USE+1
      ENDDO
      IF (OUTER_USE .GE. INNER_USE) THEN
        UF2PNT=21
        RETURN
      ENDIF
      DO WHILE (PT_USED(INNER_USE).LE.0 .AND. INNER_USE.GT.OUTER_USE)
        INNER_USE = INNER_USE-1
      ENDDO
      IF (INNER_USE .LE. OUTER_USE) THEN
        UF2PNT = 21
        RETURN
      ENDIF

C Copy the appropriate starting matrices into TMP_X_FILT, TMP_C_FILT

      IF (R_USE .GT. R_PT(OUTER_USE)) THEN !Outside outermost
         TMP = (2.D0/DBL_TRK(1) - DBL_TRK(4))*DSIGN(1.D0,DBL_TRK(1))
         IF (R_USE .GT. TMP-0.000001) THEN
            UF2PNT = -1
            RETURN
         END IF

         TMP_R(1) = R_PT(OUTER_USE)
         TMP_R(2) = R_USE
         TMP_R(3) = TMP-0.000001
         I_OUT = OUTER_USE

C start from smoothed results
         DO I = 1, 5
            TMP_X_FILT(I) = X_SMOOTH(I,OUTER_USE)
            DO J = 1, 5
               TMP_C_FILT(J,I) = C_SMOOTH(J,I,OUTER_USE)
            END DO
         END DO

      ELSE IF (R_USE .LT. R_PT(INNER_USE)) THEN !Inside innermost
         IF (R_USE .LT. ABS(DBL_TRK(4))+0.000001) THEN
            UF2PNT = -1
            RETURN
         END IF

         TMP_R(1) = R_PT(INNER_USE)
         TMP_R(2) = R_USE
         TMP_R(3) = ABS(DBL_TRK(4))+0.000001   ! r must be > 0
         I_OUT = INNER_USE

C start from filtered results (same as smoothed at innermost point)
         DO I = 1, 5
            TMP_X_FILT(I) = X_FILT(I,INNER_USE)
            DO J = 1, 5
               TMP_C_FILT(J,I) = C_FILT(J,I,INNER_USE)
            END DO
         END DO
      ELSE
C Between two coordinates
C The radius must be between points.
C First find the points it is between irrespective of whether they are
C good coordinates or not.  I_IN and I_OUT cannot go out of range, otherwise
C we wouldn't be in this ELSE block.  Add bullet-proof protection just in
C case though...

        I_OUT = OUTER_USE
        I_IN = INNER_USE
        DO WHILE (R_USE .GT. R_PT(I_IN-1)
     &      .AND. I_IN.GT.OUTER_USE+1)
          I_IN = I_IN - 1
        ENDDO
        DO WHILE (R_USE .LT. R_PT(I_OUT+1)
     &      .AND. I_OUT.LT.INNER_USE-1)
          I_OUT = I_OUT + 1
        ENDDO
        IF (I_IN .LT. I_OUT .OR. I_IN.LE.OUTER_USE
     &      .OR. I_OUT.GE.INNER_USE) THEN
          UF2PNT = 21
          RETURN
        ENDIF

C Now make sure have good coordinates for the two points to extrapolate between

        DO WHILE(PT_USED(I_IN).LE.0 .AND. I_IN.LT.INNER_USE)
            I_IN = I_IN + 1
        ENDDO
        DO WHILE(PT_USED(I_OUT).LE.0 .AND. I_OUT.GT.OUTER_USE)
            I_OUT = I_OUT - 1
        ENDDO

C Fill temporary variables

        TMP_R(1) = R_PT(I_OUT)
        TMP_R(2) = R_USE
        TMP_R(3) = R_PT(I_IN)

        DO I = 1, 5
           TMP_X_FILT(I) = X_FILT(I,I_OUT)
           DO J = 1, 5
              TMP_C_FILT(J,I) = C_FILT(J,I,I_OUT)
           END DO
        END DO

      END IF

C Set up arc, initial guess at state vector, etc. for trial point

      CALL UF2ARC(3, 1, DBL_TRK, TMP_R, TMP_X, TMP_XY)

C  Get covariance matrix of process noise (and energy loss)

      CALL UFQPRC(FIELD, 3, 1, DBL_TRK, TMP_R, TMP_XY,
     +     TMP_X, AT_VTX, MASS,
     +     TMP_Q_PROC, DO_ELOSS, TMP_DE_DX)

C  Predicted state vector at trial point = filtered state vector
C  at trial point, since there is no measurement.

      CALL UFDISC(TMP_X_FILT,MASS,TMP_DE_DX(1,1))
      IERR = UFTRAN(R_PT(I_OUT),R_USE, TMP_X_FILT, T_PROC,
     &     TMP_X_FILT)
      IF (IERR .NE. 0) THEN
         UF2PNT = 11
         RETURN
      END IF

C  Apply linearized error propagation

      IERR = UFJACO(R_PT(I_OUT), R_USE, X_SMOOTH(1,I_OUT),
     &     TMP_DF_DX(1,1,1))

C  Apply effects of energy loss

      CALL UFLOSS(TMP_X_FILT,TMP_DF_DX(1,1,1),MASS,TMP_DE_DX(2,1))

C  Predicted covariance matrix (no scattering)
C  Predicted covariance variance at trial point = filtered covariance
C  matrix at trial point, for same reason as above.

      CALL UFMULT(TMP_DF_DX(1,1,1),TMP_C_FILT,
     &     TMP_DF_DX(1,1,1),TMP_C_FILT)

C  Add in process noise

      CALL UFMADD(TMP_C_FILT,CORE_FRACT*CORE_WIDTH +
     &     TAIL_FRACT*TAIL_WIDTH, TMP_Q_PROC(1,1,1),
     &     TMP_C_FILT)

C  Now we have filtered state and covariance matrices at the trial point
C  (although they are just the predicted matrices).

C If the operation is an extrapolation, we don't need to do any
C     smoothing. Copy filtered variables to smoothed variables and go to
C     the end.

      IF (R_USE.LT.R_PT(INNER_PT) .OR. R_USE.GT.R_PT(OUTER_PT)) THEN
         DO I = 1, 5
            TMP_X_SMOOTH(I) = TMP_X_FILT(I)
            DO J = 1, 5
               TMP_C_SMOOTH(J,I) = TMP_C_FILT(J,I)
            END DO
         END DO
         GOTO 4500
      END IF

C This is an interpolation. Therefore, we need to "smooth" the
C     result, ie. incorporate the information from r<r_use.

C We have filtered state and covariance matrices for all coordinates,
C     hence we can apply the usual smoothing equations just as if the
C     trial point was a coordinate.

C One (big) caveat. These computations are very sensitive numerically. So:
C   To compute the Jacobian from the trial point to I_IN, we must compute a
C   state vector starting from the smoothed vector at I_OUT.
C   Also, we must recompute C_FILT(I_IN) by going through the trial point.
C
C All of this is because the transport has been linearized. Therefore, going
C     directly from I_OUT to I_IN is not the same as going via the trial
C     point.

C  Jacobian, expanded at the trial point.
C Use TMP_X_SMOOTH temporarily.
      IERR = UFTRAN(R_PT(I_OUT),R_USE, X_SMOOTH(1,I_OUT), T_PROC,
     &     TMP_X_SMOOTH)
      IF (IERR .NE. 0) THEN
         UF2PNT = 11
         RETURN
      END IF

      IERR = UFJACO(R_USE, R_PT(I_IN), TMP_X_SMOOTH,
     &     TMP_DF_DX(1,1,2))
      IF (IERR .NE. 0 ) THEN
         UF2PNT = 12
         RETURN
      ENDIF

C now compute C_PRED at I_IN going through the trial point
C  Apply effects of energy loss

      CALL UFLOSS(TMP_X_SMOOTH,TMP_DF_DX(1,1,2),MASS,TMP_DE_DX(2,2))

C  Predicted covariance matrix (no scattering)
C  Predicted covariance variance at trial point = filtered covariance
C  matrix at trial point, for same reason as above.

      CALL UFMULT(TMP_DF_DX(1,1,2),TMP_C_FILT,
     &     TMP_DF_DX(1,1,2),TMP_C_PRED)

C  Add in process noise

      CALL UFMADD(TMP_C_PRED,CORE_FRACT*CORE_WIDTH +
     &     TAIL_FRACT*TAIL_WIDTH, TMP_Q_PROC(1,1,2),
     &     TMP_C_PRED)

C Do the smoothing from the newly calculated values at I_IN to the trial
C     point.

      DO I = 1, 5
        IF (TMP_C_PRED(I,I).LT.0)THEN
            UF2PNT = 26
            RETURN
        ENDIF
        WT_PRED(I) = DSQRT( TMP_C_PRED(I,I) )
      ENDDO

C  Check whether user requested constraint

      IF ( FIXED_OME ) WT_PRED(5) = 1.D0
      IF ( FIXED_TANL ) WT_PRED(4) = 1.D0
      IF ( FIXED_P ) WT_PRED(5) = 1.D0

C  Protect against zero weight after checking constraint mode

      DO I = 1, 5
        IF (WT_PRED(I).EQ.0.) THEN
            UF2PNT = 26
            RETURN
        ENDIF
      ENDDO

C    c_temp(i,j) = c_pred(i,j) / (wt_pred(i) * wt_pred(j))
      CALL UFMSCL( C_TEMP, TMP_C_PRED, WT_PRED, 5 )

C  Handle constraints

        IF ( FIXED_OME ) C_TEMP(5,5) = 1.D0
        IF ( FIXED_TANL ) C_TEMP(4,4) = 1.D0
        IF ( FIXED_P ) THEN
          C_TEMP(5,5) = 1.D0
          C_TEMP(4,5) = 0.D0
          C_TEMP(5,4) = 0.D0
          C_TEMP(3,5) = 0.D0
          C_TEMP(5,3) = 0.D0
          C_TEMP(2,5) = 0.D0
          C_TEMP(5,2) = 0.D0
          C_TEMP(1,5) = 0.D0
          C_TEMP(5,1) = 0.D0
        ENDIF

C  Invert 5x5
C  Give up on this track if the matrix inversion fails because the
C  matrix is not positive definite

      CALL DSINV (5,C_TEMP,5,IFAIL)
      IF (IFAIL.NE.0) THEN
         UF2PNT = 17
         RETURN
      ENDIF

      MATDIM = 5
      IF ( FIXED_P ) MATDIM = 4

C    c_temp(i,j) = c_pred(i,j) / (wt_pred(i) * wt_pred(j))
      CALL UFMSCL( C_TEMP, C_TEMP, WT_PRED, MATDIM )

C    c_work = tmp_c_filt * tmp_df_dx^T
      CALL UFMMLT(C_WORK, TMP_C_FILT, TMP_DF_DX(1,1,2), MATDIM )

C   gain_smooth = c_work * c_temp
      CALL UFMMUL(GAIN_SMOOTH, C_WORK, C_TEMP, MATDIM )

C  Smoothed state vector

      DO I = 1, MATDIM
         TMP_X_SMOOTH(I) = TMP_X_FILT(I)
      END DO
      DO J = 1, MATDIM
         TMP = X_SMOOTH(J,I_IN)-X_PRED(J,I_IN)
C  Protect against a 0->2*pi problem
         IF (J.EQ.1) THEN
            DELTA_PHI = TMP/R_USE
            CALL UBAPI(DELTA_PHI)
            TMP = R_USE * DELTA_PHI
         ELSE IF (J.EQ.3) THEN
            CALL UBAPI(TMP)
         ENDIF
         DO  I = 1, MATDIM
            TMP_X_SMOOTH(I) = TMP_X_SMOOTH(I) +
     $           GAIN_SMOOTH(I,J) * TMP
         ENDDO
      ENDDO
      PHI_NEW = TMP_X_SMOOTH(1) / R_USE
      CALL UBA2PI(PHI_NEW)
      DELTA_PHI = PHI_NEW-TMP_X_FILT(1)/R_USE
      IF ( DELTA_PHI .GT. ONE_PI )  PHI_NEW = PHI_NEW - TWO_PI
      IF ( DELTA_PHI .LT.-ONE_PI )  PHI_NEW = PHI_NEW + TWO_PI
      TMP_X_SMOOTH(1) = PHI_NEW * R_USE
      CALL UBA2PI(TMP_X_SMOOTH(3))
      DELTA_PHI = TMP_X_SMOOTH(3) - TMP_X_FILT(3)
      IF ( DELTA_PHI .GT. ONE_PI )  TMP_X_SMOOTH(3) = TMP_X_SMOOTH(3)
     $     -TWO_PI
      IF ( DELTA_PHI .LT.-ONE_PI )  TMP_X_SMOOTH(3) = TMP_X_SMOOTH(3)
     $     +TWO_PI
      CALL UBAPI2(TMP_X_SMOOTH(4))

C  Check whether user requested fixed momentum

      IF ( FIXED_P )  THEN
        TMP_X_SMOOTH(5) = DSIGN(1.D0,OMEGA_TRK) / (DBLE(ROVERP*P_FIX)
     &      *DCOS(TMP_X_SMOOTH(4)))
      ENDIF

C  Smoothed covariance matrix

      CALL UFSCOV(C_SMOOTH(1,1,I_IN), TMP_C_PRED, TMP_C_FILT,
     $     GAIN_SMOOTH, TMP_C_SMOOTH, MATDIM)

C  Check if user requested fixed momentum

        IF ( FIXED_P )  THEN
          FACT = TMP_X_SMOOTH(5) * DTAN(TMP_X_SMOOTH(4))
          TMP_C_SMOOTH(5,5) = FACT * FACT * TMP_C_SMOOTH(4,4)
          TMP_C_SMOOTH(1,5) = FACT * TMP_C_SMOOTH(1,4)
          TMP_C_SMOOTH(5,1) = TMP_C_SMOOTH(1,5)
          TMP_C_SMOOTH(2,5) = FACT * TMP_C_SMOOTH(1,4)
          TMP_C_SMOOTH(5,2) = TMP_C_SMOOTH(1,5)
          TMP_C_SMOOTH(3,5) = FACT * TMP_C_SMOOTH(1,4)
          TMP_C_SMOOTH(5,3) = TMP_C_SMOOTH(1,5)
          TMP_C_SMOOTH(4,5) = FACT * TMP_C_SMOOTH(1,4)
          TMP_C_SMOOTH(5,4) = TMP_C_SMOOTH(1,5)
        ENDIF

C  Now we're done, except for converting to standard coordinates.
 4500 CONTINUE
      IERR = UFGETT(R_USE,TMP_X_SMOOTH,T_PROC)
      IF (IERR.NE.0) THEN
        UF2PNT = 14
        RETURN
      ENDIF

      CALL UFSWMX(R_USE, TMP_X_SMOOTH, T_PROC, DBL_TRK)
      CALL UFSWMC(R_USE, TMP_X_SMOOTH, T_PROC, TMP_C_SMOOTH,
     &  MASS, C_NEW)

      CALL ULOAD(DBL_TRK, C_NEW, X_PNT, C_PNT)

      UF2PNT = 0

      RETURN
C
C ---------------------------------------------------------------
C
      ENTRY UF2XYZ(X_DUM,E_DUM)

      RPHI_EXTR = SNGL(TMP_X_SMOOTH(1))
      Z_EXTR = SNGL(TMP_X_SMOOTH(2))
      PHI_EXTR = RPHI_EXTR/SNGL(R_USE)
      X_EXTR(1) = SNGL(R_USE) * COS(PHI_EXTR)
      X_EXTR(2) = SNGL(R_USE) * SIN(PHI_EXTR)
      X_EXTR(3) = Z_EXTR
      X_EXTR(4) = DCOS(TMP_X_SMOOTH(3))*DCOS(TMP_X_SMOOTH(4))
      X_EXTR(5) = DSIN(TMP_X_SMOOTH(3))*DCOS(TMP_X_SMOOTH(4))
      X_EXTR(6) = DSIN(TMP_X_SMOOTH(4))
      
      E_EXTR(1) = DSQRT(max(TMP_C_SMOOTH(1,1),0.D0))
      E_EXTR(2) = DSQRT(max(TMP_C_SMOOTH(2,2),0.D0))

      X_DUM(1) = X_EXTR(1)
      X_DUM(2) = X_EXTR(2)
      X_DUM(3) = X_EXTR(3)
      X_DUM(4) = X_EXTR(4)
      X_DUM(5) = X_EXTR(5)
      X_DUM(6) = X_EXTR(6)
      E_DUM(1) = E_EXTR(1)
      E_DUM(2) = E_EXTR(2)

      RETURN

      END
#endif
