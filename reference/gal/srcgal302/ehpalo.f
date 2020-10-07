      FUNCTION EHPALO( X )
C.----------------------------------------------------------------
C       J.Badier - 11/12/85
C! Longitud. parametrisation
C       Forme reduite de la parametrisation longitudinale
C       EMALM1 = ALPHA - 1.
C       X = [BETA * (Z cm /Lrad cm)] / EMALM1
C    -Called by EHLOTI
C.------------------------------------------------------------------
      SAVE
      PARAMETER(LTYPEL=48, LTYPME=49, LTYPBA=50, LTYPAN=51, LTYPGA=52)
      COMMON / CAPANO / NATGER,METHCO,TOTNRJ,TINOX0,TINOL0,EMGNRJ,
     + EMALFA,EMBETA,EMALM1,EMAEXP,EMFACT,EMUDP0,EMUDP2, HADNRJ,HALPHA,
     + HABETA,HADRAY,HAPUIS,HGAMMA,SMAXLR,SMAXLA
       COMMON / CANAME / EMNAME
       CHARACTER*4 EMNAME
C
C
      EHPALO = X * EXP(1.-X)
      IF(EHPALO.LE.0.) EHPALO = 0.
      EHPALO = EHPALO ** EMALM1
C
      RETURN
      END
