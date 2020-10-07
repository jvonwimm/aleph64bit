      real fidOuterRad,fidInnerRad,fidMaxZ,vdetChiDiff,vdetChiCut,
     $  minDistXYCut,minDistZCut,maxBadHits,firstChgCut,nMCutPi,
     $  nMCutX,Chi2VtxCut,Chi2TrkCut

      common / knkcut / fidOuterRad, fidInnerRad, fidMaxZ,
     $     minDistXYCut, minDistZCut, maxBadHits, firstChgCut,
     $     vdetChiDiff, vdetChiCut, Chi2VtxCut, Chi2TrkCut
#if defined(DOC)
C-------------------------------------------------------------------------
C
C! common block for Kink Search constants, filled from YKCI
C
C   fidOuterRad  =   Outer radius of kink fiducial region
C   fidInnerRad  =   Inner radius of kink fiducial region
C   fidMaxZ      =   Limit of kink fiducial region in Z
C   minDistXYCut =   Cut on miss distance in r-phi plane
C   minDistZCut  =   Cut on miss distance in Z
C   maxBadHits   =   Cut on number of bad hits
C   firstChgCut  =   Early cut on tracks having the same charge
C   vdetChiDiff  =
C   vdetChiCut   =
C   Chi2VtxCut   =   Chi^2 cut on the vertex
C   Chi2TrkCut   =   Chi^2 cut on the track (per DOF)
#endif
      integer YKMXSPEC
      parameter ( YKMXSPEC = 10 )
      integer ykNspec
      real YKParM(YKMXSPEC), YKDauM(YKMXSPEC), YKneutM(YKMXSPEC)
      real YKMCut(YKMXSPEC), YKTagC(YKMXSPEC)
      common / knkspec / ykNspec, YKParM, YKDauM, YKNeutM,
     $     YKMCut, YKTagC
#if defined(DOC)
C------------------------------------------------------------------------
C! common block of kink species information; filled from YKSP
C
C       YKMXSPEC        =       number of kink species recognized
C
C       ykNspec         =
C       YKParM          =       Parent mass
C       YKDauM          =       Charged daughter mass
C       YKNeutM         =       Neutral daughter mass
C       YKMCut          =       Mass cut for YKNK physics output bank
C       YKTagC          =       Mass cut for FXTR VDET tagging
#endif
