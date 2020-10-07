      SUBROUTINE EHADRN( ICOND , XGERB )
C.--------------------------------------------------------------
C        J.Badier - 17/03/87
C! Tirage d'une gerbe hadronique.
C   INPUT   ICOND = 0    Initialisation
C                   1    Tirage d'un point.
C   OUTPUT  XGERB        Coordonnees du point tire dans le systeme
C                        de la gerbe. XGERB(1) en cm selon l'axe.
C        Call RANNOR , RNDM.
C   Appele par EHGERBE
C. --------------------------------------------------------------
      SAVE
      DIMENSION XGERB(3)
      PARAMETER(LTYPEL=48, LTYPME=49, LTYPBA=50, LTYPAN=51, LTYPGA=52)
      COMMON / CAPANO / NATGER,METHCO,TOTNRJ,TINOX0,TINOL0,EMGNRJ,
     + EMALFA,EMBETA,EMALM1,EMAEXP,EMFACT,EMUDP0,EMUDP2, HADNRJ,HALPHA,
     + HABETA,HADRAY,HAPUIS,HGAMMA,SMAXLR,SMAXLA
       COMMON / CANAME / EMNAME
       CHARACTER*4 EMNAME
C
      IF( ICOND .EQ. 0 ) THEN
         BABA = 0.
      ELSE
         XGERB(1) = 100. * RNDM(BABA)
         CALL RANNOR( ALEA2 , ALEA3 )
         XGERB(2) = XGERB(1) * HADRAY * ALEA2
         XGERB(3) = XGERB(1) * HADRAY * ALEA3
      ENDIF
      RETURN
      END
