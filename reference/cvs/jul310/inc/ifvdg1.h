      KVD  = IW(NAVDG1)
      IF (KVD.NE.0) THEN
      MVD  = IW(KVD) - IW(KVD+1)
      LV1  = IW(KVD+2)
      LV2  = IW(KVD+3)
      KV1  = KVD + IW(KVD+1)
      KV3  = KV1 + LV1*LV2
C
#if defined(DOC)
C! Geant3 BOS bank for VDET :
                               'VDG1' , index = KVD , # of data = MVD
                               2 buffers, 3 dimensions : LV1, LV2 , LV3
   RW (KVD +         bank length = LV1*LV2+LV3 + LHDR  =  4*3+5 + 4 = 2
              1      LHDR : header length
              2      LV1  : # of words/element
              3      LV2  : # of elements
              4      LV3  : # of words to define an element

    RW (KV1 + 1      inner radius of layer # 1
              2      thickness
              3      z minimum
              4      total z length
      + LV1 + 1      same as above for layer # 2
             LV1      ---
      +2*LV1+ 1      same as above for support of vdet
             LV1      ---

   RW (KV3 +  1      width  of elementary element
              2      total length of "     "
              3      thickness of     "      "
              4      # of elements in first layer
              5      # of elements in second layer
#endif
