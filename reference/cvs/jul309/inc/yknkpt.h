      integer MAXPTS
      parameter (MAXPTS =  40)
      integer nptsIn
      real rIn(MAXPTS), uIn(MAXPTS), zIn(MAXPTS)
      real sigUIn(MAXPTS), sigZIn(MAXPTS)
      integer nptsOut
      real rOut(MAXPTS), uOut(MAXPTS), zOut(MAXPTS)
      real sigUOut(MAXPTS), sigZOut(MAXPTS)
      integer nvdet
      integer wafers(4)
      real uVDet(4), wVDet(4)

      common / knkpts / nptsIn, rIn, uIn, zIn, sigUIn, sigZIn,
     $     nptsOut, rOut, uOut, zOut, sigUOut, sigZOut,
     $     nvdet, wafers, uVDet, wVDet
#if defined(DOC)
C-------------------------------------------------------------------------
C!      Common block to hold the points along the two kink candidates
C
C   MAXPTS  = Maximum number of points allowed on a track
C
C   nptsIn  = Number of points on inner track
C   rIn     = Radius of points on inner track
C   uIn     = R*phi of points on inner track
C   zIn     = Z of points on inner track
C   sigUIn  = Rphi coordinate errors for inner track
C   sigZIn  = Z coordinate errors for inner track
C   nptsOut = Number of points on inner track
C   rOut    = Radius of points on outer track
C   uOut    = R*phi of points on outer track
C   zOut    = Z of points on outer track
C   sigUOut = Rphi coordinate errors for outer track
C   sigZOut = Z coordinate errors for outer track
C   nvdet   = Number of VDET hits on inner track
C   wafers  = VDET wafers crossed by inner track
C   uVDet   = U-coordinate of VDET hits on inner track
C   wVDet   = W-coordinate of VDET hits on inner track
#endif
