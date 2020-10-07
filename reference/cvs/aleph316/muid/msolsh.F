      SUBROUTINE MSOLSH(I1,IDF1,RAPP1,SUDN1,AN1,I2,IDF2,RAPP2,SUDN2,AN2
     &                  ,IGT)
C-----------------------------------------------------------------------
C
CKEY MUONID SHAD / INTERNAL
C
C! solve ambiguity for 2 mu-candidates tracks in shadow
C!
C!  author : G. Capon                 11-jun-1990
C!         : G. Taylor                12-APR-1992
C!              change to use dist (c/w dist/dcut)
C!              use info from layer 2 of mu chambers
C!
C!  input : I1 , I2     : track numbers (in JULIA)
C!          IDF1, IDF2  : identif flags (=1,2,3 for Hmad,mcad,hmad+mcad)
C!          RAPP1,RAPP2 : dist for tracks (see mcad bank)
C!          SUDN1,SUDN2 : sum of residuals for tracks (see HGSUDN)
C!          AN1,AN2     : ANG/ACUT for tracks (see mcad bank)
C!  output :IGT         : index of accepted track (that is I1 or I2)
C=======================================================================
#ifndef DOC
      IGT = 0
      ISOLV = 0
C choose muon with best id flag
      IF(IDF1.GT.IDF2) THEN
        IGT = I1
      ELSE IF(IDF1.LT.IDF2) THEN
        IGT = I2
C both muon have the same id flag
      ELSE IF(IDF1.GT.3) THEN
C both have two muon chamber hits choose best dist match
        IF(RAPP1.LT.RAPP2) THEN
          IGT = I1
        ELSE
          IGT = I2
        ENDIF
      ELSE IF(IDF1.GT.1) THEN
C both have only one muon chamber hits choose best dist match
        IF(RAPP1.LT.RAPP2) THEN
          IGT = I1
        ELSE
          IGT = I2
        ENDIF
      ELSE
C both have only hcal hits choose best hcal distance match
        IF(SUDN1.LT.SUDN2) THEN
          IGT = I1
        ELSE
          IGT = I2
        ENDIF
      ENDIF
C
      RETURN
      END
#endif
