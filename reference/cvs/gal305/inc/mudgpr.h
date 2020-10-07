*CD mudgpr
      PARAMETER (MXPHT = 200, MXDHT =  40, MXEMD = 94)
      PARAMETER (MXTRK = 100, MXTDI = 100)
C
#if defined(DOC)
      MXPHT -- maximum # of hits one plane can have ( the term 'plane'
               means the plane in electronics modules, for the time
               being, it is identical to the strip plane)
      MXDHT -- maximum # of hits that contribute to a digit
      MXEMD -- maximum # of electronics modules
      MXTRK -- maximum # of tracks in one event that contribute to
               all muon detector digit
      MXTDI -- maximum # of muon detector digit a track can produces
#endif
