C>>> HM890307 TEMPORARY MOD FOR BACKWARD COMPATIBILITY
      COMMON /SFLAGC/LOLDSF,LOLGSF
      LOGICAL LOLDSF,LOLGSF
#if defined(DOC)
C
C!         FLAGS FOR THE SATR RECONSTRUCTION
C
C LOLDSF:   Flag set if input used database version 112 or earlier
C LOLGSF:   Flag set if input is from GALEPH 230 or earlier
C>>> END HM890307
#endif
