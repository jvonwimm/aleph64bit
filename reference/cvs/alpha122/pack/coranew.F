      FUNCTION CORANEW(E,TH,PH,NRUN)
C ---------------------------------------------------------------------
C!  New corrections to photon energy
C - Author: M.N Minard          940121
C   Called from FIXGAEN
C - INPUT : E       : Energy of cluster
C-          TH      : Cluster theta angle (rad)
C-          PH      : Cluster PHI angle (rad)
C-          NRUN    : Run number
C - Output: CORANEW : correction factor
C ---------------------------------------------------------------------
#ifndef DOC
C
      DIMENSION PADR3(4),PADR4(4),PADR5(4)
      DIMENSION PAMC3(4),PAMC4(4),PADMC5(4)
      DATA PADR3/.11,.16,.16,.13/
      DATA PADR4/1.022,.92,1.031,1.015/
      DATA PAMC3/.056,0.17,.12,.11/
      DATA PAMC4/1.036,1.013,1.019,1.015/
      DATA PADR5/.956,.956,.956,.956/
      DATA PADMC5/.956,.956,.956,.956/
C ---------------------------------------------------------------------
C
      IMC = 0
      IF (NRUN.LT.2000) IMC =1
      CORANEW = 1
C ----
      CT=ABS(COS(TH))
      IBCOR=1
      IF(CT .GT. 0.72 .AND. CT .LT. 0.8) THEN
         IBCOR=2
      ENDIF
      IF(CT .GT. 0.8 .AND. CT .LT.0.9) THEN
         IBCOR=3
      ENDIF
      IF(CT .GT. 0.9 .AND. CT .LT. 1.) THEN
         IBCOR=4
      ENDIF
CC--
      IF(IMC .LE. 0) THEN
         COREN=PADR4(IBCOR)
     X              *(1.-PADR3(IBCOR)/(E)**PADR5(IBCOR))
      ELSE
         COREN=PAMC4(IBCOR)*(1.-PAMC3(IBCOR)/(E)**PADMC5(IBCOR))
      ENDIF
C
       CORANEW = 1./COREN
       END
#endif
