      COMMON /YSVWRK/ INDYSVW, INDYSEW, INDYSTW, INDYS0W, INDYSCW, 
     &      INDYSTL, INDYSTM, INDHXOU, INDVHOU, 
     &      INDYS0L, INDYS0M, INDHXNU, INDVHNU, 
     &      INDYSCL, INDYSCM, INDCXOU, INDVCOU,
     &      INDYSKL
#ifdef DOC
C---------------------------------------------------------
C
C  Work bank indices for secondary vertex finder
C  Note: the order of these variables should not be changed, e.g. by adding
C  another variable somewhere in the middle.  This common block is initialized
C  to zero in YSVBLD, and the number of workbanks is significant in the
C  call to WDROP at the end of that routine.
C
C   INDYSVW =   work bank for vertices
C   INDYSEW =   work bank for error matrices
C   INDYSTW =   work bank for vertex charged track mask
C   INDYS0W =   work bank for vertex V0 mask
C   INDYSCW =   work bank for vertex circle mask
C   INDYSTL =   work bank for track indices
C   INDYSTM =   work bank for track mask
C   INDYS0L =   work bank for V0 indices
C   INDYS0M =   work bank for V0 mask
C   INDYSCL =   work bank for circle (ITC track) indices
C   INDYSCM =   work bank for circle (ITC track) mask
C   INDHXOU =   work bank for ouput helix parameters from vertex fit
C   INDVHOU =   work bank for output helix covariance matrix
C   INDHXNU =   work bank for output neutral helices
C   INDVHNU =   work bank for output neutral covariance matrix
C   INDCXOU =   work bank for ouput circle parameters from vertex fit
C   INDVCOU =   work bank for output circle covariance matrix
C   INDYSKL =   work bank for forbidden kink pairs
#endif
