      SUBROUTINE EHTRTI( ZLON , RADIS )
C.-----------------------------------------------------------------
C       J.Badier - 11/12/85
C       J.Badier - 16/03/87 : modifie
C       J.Badier - 11/08/87 : modifie
C!   Tirage transversal
C    Input : ZLON    Distance a l'origine de la gerbe en cm.
C    Output: RADIS   Distance a l'axe de la gerbe
C  - Called by EHGERB
C
C.-----------------------------------------------------------------
      SAVE
      PARAMETER(LTYPEL=48, LTYPME=49, LTYPBA=50, LTYPAN=51, LTYPGA=52)
      COMMON / CAPANO / NATGER,METHCO,TOTNRJ,TINOX0,TINOL0,EMGNRJ,
     + EMALFA,EMBETA,EMALM1,EMAEXP,EMFACT,EMUDP0,EMUDP2, HADNRJ,HALPHA,
     + HABETA,HADRAY,HAPUIS,HGAMMA,SMAXLR,SMAXLA
       COMMON / CANAME / EMNAME
       CHARACTER*4 EMNAME
C
C
      EXTERNAL RNDM
C
      DATA KMAX/ 20 / , RMAX/ 10. /
        IF ( METHCO .NE. 2 ) GO TO 40
C ----- Tirage radial
           KPASS = 0
C ----- Tirage de X1 , selon X1/(1+X1)**PUIS
C ----- La variable U1 permet un gain de temps.
   20      KPASS=KPASS+1
         IF( KPASS .GT. KMAX ) GO TO 40
C ----- Tirage de U1 selon U1**(PUIS-1),EMAEXP = 1. / ( PUIS - 2 )
      U1 = RNDM(U1)
      IF( U1 .LT. .001 ) U1 = .0005
      U1 = U1 ** EMAEXP
C ----- Tirage de U1 selon (1-U1)*U1**(PUIS-1)
           IF( RNDM(U1) .GT. 1. - U1 ) GO TO 20
           X1 = 1. / U1 - 1.
C ----- Radis = r0 * x1
           RADIS = X1 * EMFACT * (  EMUDP0 + EMUDP2 * ZLON**2 )
      IF( RADIS .GT. RMAX ) GO TO 20
   98 CONTINUE
      RETURN
C ----- Pas de distribution radiale
   40 RADIS = 0.
      GO TO 98
         END
