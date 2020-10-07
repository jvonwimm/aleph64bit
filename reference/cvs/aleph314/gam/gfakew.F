      REAL FUNCTION GFAKEW(NFAKE,NGOOD)
C---------------------------------------------------------------------
C  M.Verderi.   Creation 29/09/94
C!  Computes likelihood that photon "2" is an electromagnetic fake photon
C!   induced by the photon "1"
C
C  Input : reference distribution numbers
C
C  Rq    : Fills the words distang,angle12,Keclflag of the common
C          emfake
C  Called by GFAKE
C  Calls GXEM
C---------------------------------------------------------------------
#ifndef DOC
      REAL E1,F4_1,ERAW1,THEBARY1,PHIBARY1,STAGERM1,EGERS1_1,EGERS2_1,
     &     PRFAK1_1,PRFAK2_1,ECLVS1_1,ECLVS2_1,ECLVS3_1,FENDCAP1,
     &     EFRF4I1,EC4CRA1
      REAL E2,F4_2,ERAW2,THEBARY2,PHIBARY2,STAGERM2,EGERS1_2,EGERS2_2,
     &     PRFAK1_2,PRFAK2_2,ECLVS1_2,ECLVS2_2,ECLVS3_2,FENDCAP2,
     &     EFRF4I2,EC4CRA2
      COMMON / EMFAKE /
     &     E1,F4_1,ERAW1,THEBARY1,PHIBARY1,STAGERM1,EGERS1_1,EGERS2_1,
     &     PRFAK1_1,PRFAK2_1,ECLVS1_1,ECLVS2_1,ECLVS3_1,FENDCAP1,
     &     EFRF4I1,EC4CRA1,
     &     E2,F4_2,ERAW2,THEBARY2,PHIBARY2,STAGERM2,EGERS1_2,EGERS2_2,
     &     PRFAK1_2,PRFAK2_2,ECLVS1_2,ECLVS2_2,ECLVS3_2,FENDCAP2,
     &     EFRF4I2,EC4CRA2,
     &     DISTANG,ANGLE12,KECLFLAG

      DATA DEUXPI /6.283185307/
      NREG = 1
      IF (NFAKE.GE.700) NREG = 2


Calcul de angle12, angle relatif, et distang, distance angulaire:
C BARREL :

      IF (NREG.EQ.1) THEN
         DELTET = ABS(THEBARY1-THEBARY2)/SIN(THEBARY1)
         DELPHI = ABS(PHIBARY1-PHIBARY2)
         DEL2PI = ABS(DELPHI-DEUXPI)
         IF (DELPHI.GT.DEL2PI) DELPHI = DEL2PI
         HYPOTE = SQRT ( DELTET**2+DELPHI**2 )
         DISTANG= HYPOTE*1000.
         IF (DELTET.GT.DELPHI) THEN
            DIREC12 = ASIN(DELTET/HYPOTE)
         ELSE
            DIREC12 = ACOS(DELPHI/HYPOTE)
         ENDIF
         ANGLE12 = DIREC12/3.14159265
         DINF = 23.
      ELSE
C END CAP:
         DELTET = THEBARY1-THEBARY2
         DELTET = DELTET/ABS(COS(THEBARY1))
         DELPHI = PHIBARY1-PHIBARY2
         PHIABS = ABS(DELPHI)
         PHI2PI = ABS(PHIABS-DEUXPI)
         IF (PHI2PI.LT.PHIABS) DELPHI = PHI2PI*SIGN(1.,DELPHI)
         DELPHI = DELPHI*ABS(TAN(THEBARY1))
         IF (ABS(COS(THEBARY1)).LT.0.90) THEN
            DELPHI = DELPHI/( .041 - .032*ABS(COS(THEBARY1)) )
         ELSE
            DELPHI = DELPHI/( .071 - .062*ABS(COS(THEBARY1)) )
         ENDIF
C 1.48 OU 1.45 OU 1.39 OU 1.58 .. :
         DELPHI = DELPHI*9.E-3*1.58
         DELTET = ABS(DELTET)
         DELPHI = ABS(DELPHI)
         HYPOTE = SQRT ( DELTET**2+DELPHI**2 )
         IF (DELTET.GT.DELPHI) THEN
            DIREC12 = ASIN(DELTET/HYPOTE)
         ELSE
            DIREC12 = ACOS(DELPHI/HYPOTE)
         ENDIF
         ANGLE12 = DIREC12/3.14159265
         DISTANG = HYPOTE*1000.*250.5/184.7
         DINF = 25.
      ENDIF

Calcul Keclflag:
      KECLFLAG=0
      I1 = 0
      I2 = 0
      IF (ECLVS1_2.EQ.ERAW1.OR.ECLVS2_2.EQ.ERAW1.OR.
     &    ECLVS3_2.EQ.ERAW1) I2 = 1
      IF (ECLVS1_1.EQ.ERAW2.OR.ECLVS2_1.EQ.ERAW2.OR.
     &    ECLVS3_1.EQ.ERAW2) I1 = 1
      KECLFLAG = 10*I1 + I2

Calcul de l'energie des 4 tours centrales partages:
      F4_SHARE = (EFRF4I1+EFRF4I2)/(ERAW1+ERAW2)
      IF (NREG.EQ.1) F4_SHMAX = .78
      IF (NREG.EQ.2) F4_SHMAX = .75


Calcul des probas:
       PFAKE = 1.0
       PGOOD = 1.0

C ANGLES ENTRE LES DEUX BARYCENTRES:
      IF (F4_SHARE.GT.F4_SHMAX) THEN
         IH = 01
         VAR = DISTANG
      ELSE
         IF (DISTANG.LT.32.) THEN
            IH = 02
            VAR = ANGLE12
         ELSE
            IH = 03
            VAR = ANGLE12
         ENDIF
      ENDIF
      PF = GXEM(NFAKE+IH,VAR)
      PG = GXEM(NGOOD+IH,VAR)
      IF (PF+PG.NE.0.) THEN
         PFAKE = PFAKE*PF/(PF+PG)
         PGOOD = PGOOD*PG/(PF+PG)
      ENDIF

C Petites et grandes distances:
C grand f4_shared(~ petites distances):
      IF (F4_SHARE.GT.F4_SHMAX) THEN
         XFRF41 = EFRF4I1/(F4_1*ERAW1)
         XFRF42 = EFRF4I2/(F4_2*ERAW2)
         XFRF4 = MIN(XFRF41,XFRF42)
         VAR = XFRF4
         IH = 04
         PF = GXEM(NFAKE+IH,VAR)
         PG = GXEM(NGOOD+IH,VAR)
         IF (PF+PG.NE.0.) THEN
            PFAKE = PFAKE*PF/(PF+PG)
            PGOOD = PGOOD*PG/(PF+PG)
         ENDIF
C   Fin des cas f4_shared grands
         GOTO 100
      ENDIF

C Grandes distances:
      IF (DISTANG.GT.32.) THEN
         VAR = ERAW2
         IF (STAGERM2.EQ.1.) THEN
            IH = 05
         ELSE
            IH = 06
         ENDIF
         PF = GXEM(NFAKE+IH,VAR)
         PG = GXEM(NGOOD+IH,VAR)
         IF (PF+PG.NE.0.) THEN
            PFAKE = PFAKE*PF/(PF+PG)
            PGOOD = PGOOD*PG/(PF+PG)
         ENDIF
      ENDIF

C Les prfakes :
       IF (FENDCAP1+FENDCAP2.EQ.0.) THEN
          IF (STAGERM2.EQ.1.) THEN
             VAR = PRFAK1_2
             IF (DISTANG.LT.32.) THEN
                IH = 7
             ELSE
C    les deux sont separes car nombres d'underflow est different:
                IH = 8
             ENDIF
          ELSE
             IH = 9
             VAR = PRFAK2_2
          ENDIF
       ELSE
          IF (STAGERM2.EQ.1.) THEN
             IH = 10
             VAR = PRFAK1_2
          ELSE
             IH = 11
             VAR = PRFAK2_2
          ENDIF
       ENDIF
       PF = GXEM(NFAKE+IH,VAR)
       PG = GXEM(NGOOD+IH,VAR)
       IF (PF+PG.NE.0.) THEN
          PFAKE = PFAKE*PF/(PF+PG)
          PGOOD = PGOOD*PG/(PF+PG)
       ENDIF

       IF (STAGERM2.EQ.1.) THEN
          VAR = PRFAK2_2
          IF (PRFAK2_2.LT.-1000.) VAR = -PRFAK2_2
          IF (DISTANG.GT.32.) THEN
             IH = 12
          ELSE
             IF (DISTANG.LT.DINF) THEN
                IH = 13
             ELSE
                IH = 14
             ENDIF
          ENDIF
          PF = GXEM(NFAKE+IH,VAR)
          PG = GXEM(NGOOD+IH,VAR)
          IF (PF+PG.NE.0.) THEN
             PFAKE = PFAKE*PF/(PF+PG)
             PGOOD = PGOOD*PG/(PF+PG)
          ENDIF
       ENDIF

C Energies de germination:
       IH = 0
       IF (FENDCAP1+FENDCAP2.EQ.0.) THEN
          IF (STAGERM2.EQ.1.) THEN
             VAR = ABS(EGERS1_2)
             XFRF4I2 = EFRF4I2/(F4_2*ERAW2)
             IF (XFRF4I2.EQ.0..OR.PRFAK1_2.LT.-100.) THEN
                IH = 15
             ELSE
                IF (DISTANG.GT.DINF) THEN
                   IH = 16
                ELSE
                   IH = 17
                ENDIF
             ENDIF
          ELSE
             VAR = ABS(EGERS2_2)
             IF (DISTANG.GT.DINF) THEN
                IH = 18
             ENDIF
          ENDIF
       ELSE
          IF (STAGERM2.EQ.1.) THEN
             VAR = ABS(EGERS1_2)
             IF (DISTANG.GT.DINF) THEN
                IH = 19
             ENDIF
          ELSE
             VAR = ABS(EGERS2_2)
             IF (DISTANG.GT.32.) THEN
                IH = 20
             ENDIF
          ENDIF
       ENDIF
       IF (IH.NE.0) THEN
          PF = GXEM(NFAKE+IH,VAR)
          PG = GXEM(NGOOD+IH,VAR)
          IF (PF+PG.NE.0.) THEN
             PFAKE = PFAKE*PF/(PF+PG)
             PGOOD = PGOOD*PG/(PF+PG)
          ENDIF
      ENDIF

100   CONTINUE

       GFAKEW = PFAKE/(PFAKE+PGOOD)
       RETURN
       END
#endif
