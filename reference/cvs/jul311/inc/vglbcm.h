#ifdef DOC
C
C  Common block for the parameters needed in the vdet global pat. rec.
C
C  NSIGRD   = # of sigma road to use in the initial hit search
C  MAXNCHI  = Maximum number of VMUC,VMWC rows
C  MAXCHIS  = Maximum simple chisquared for a single track
C  MAXCHIK  = Maximum Kalman chisquared for a single track
C  CHINUL   = chisquared penalty for a null hit
C  BIGERR   = Large error to assign to non-existant hits
C  PHTHRN   = Single/double hit threshold in normalized pulse height
C  PHNOMI   = Most likely pulse height (taken as nominal)
C  MINERR   = Minimum error
C  MAXBRTSZ = Maximum size of component to use "brute force"
C  FINALPRB = the final (delta) log(prob) cut
C  USEITC   = whether to keep ITC hits on track (temporary)
C  ONEDPEN  = penalty for a 1D hit (ie. only found hit in one view)
C  PHDIFF2V = minimum difference in normalized pulse height between a double
C              hit and its (single) partner.
C  MOMCUT   = cut used in VGMCUT
C  PckSln   = if .TRUE., use VPKSLN algorithm to choose from ambiguous solns.
C  PckTrk   = if .TRUE., use V0 search and impact parameter cuts to
C             reject tracks from VDet pattern recognition.
C
#endif
      integer NLAYER
      parameter (NLAYER = 2)
      integer NVIEW
      parameter (NVIEW = 2)
      integer NGRK
      parameter (NGRK = 2)
C
      integer MAXSOLN
      parameter (MAXSOLN = 20)
C
      INTEGER MAXNCHI
      PARAMETER (MAXNCHI = 200)
      REAL MAXCHIS
      PARAMETER (MAXCHIS = 100.0)
      REAL BIGERR
      PARAMETER (BIGERR = 100000.0)
C
      REAL NSIGRD, MAXCHIK, CHINUL
      REAL PHTHRN, PHNOMI, MINERR
      INTEGER MAXBRTSZ
      REAL FINALPRB
      LOGICAL USEITC, PckSln, PckTrk
      REAL ONEDPEN, PHDIFF2V
      REAL MOMCUT, PENITC, ICHICUT
      INTEGER NITCCUT, IBADCUT
C
      COMMON/VGLBCM/NSIGRD, MAXCHIK, CHINUL,
     &     PHTHRN, PHNOMI, MINERR,
     &     MAXBRTSZ, FINALPRB,
     $     USEITC, ONEDPEN, PHDIFF2V, MOMCUT, PckSln,
     $     PckTrk
