      SUBROUTINE GDRAY
C.
C.    ******************************************************************
C.    *                                                                *
C.    *  Generates Delta rays                                          *
C.    *                                                                *
C.    *    ==>Called by : GTELEC,GTHADR,GTMUON                         *
C.    *       Authors    D.Ward, L.Urban  ********                     *
C.    *                  G. Taylor 1/8/95                              *
C.    *                  This GEANT routine has been modified so that  *
C.    *                  the effective threshold energy for delta ray  *
C.    *                  production in the VDET area has been modified *
C.    *                  by a FACTOR. This results in an effective     *
C.    *                  increase in the cross-section by the same     *
C.    *                  FACTOR.                                       *
C.    *                                                                *
C.    *                                                                *
C.    *                                                                *
C.    *                                                                *
C.    ******************************************************************
C.
      COMMON/GCPHYS/IPAIR,SPAIR,SLPAIR,ZINTPA,STEPPA
     +             ,ICOMP,SCOMP,SLCOMP,ZINTCO,STEPCO
     +             ,IPHOT,SPHOT,SLPHOT,ZINTPH,STEPPH
     +             ,IPFIS,SPFIS,SLPFIS,ZINTPF,STEPPF
     +             ,IDRAY,SDRAY,SLDRAY,ZINTDR,STEPDR
     +             ,IANNI,SANNI,SLANNI,ZINTAN,STEPAN
     +             ,IBREM,SBREM,SLBREM,ZINTBR,STEPBR
     +             ,IHADR,SHADR,SLHADR,ZINTHA,STEPHA
     +             ,IMUNU,SMUNU,SLMUNU,ZINTMU,STEPMU
     +             ,IDCAY,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +             ,ILOSS,SLOSS,SOLOSS,STLOSS,DPHYS2
     +             ,IMULS,SMULS,SOMULS,STMULS,DPHYS3
     +             ,IRAYL,SRAYL,SLRAYL,ZINTRA,STEPRA
      COMMON/GCPHLT/ILABS,SLABS,SLLABS,ZINTLA,STEPLA
     +             ,ISYNC
     +             ,ISTRA
*
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
      DOUBLE PRECISION PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      DOUBLE PRECISION EMMU,PMASS,AVO
*
      PARAMETER (PI=3.14159265358979324D0)
      PARAMETER (TWOPI=6.28318530717958648D0)
      PARAMETER (PIBY2=1.57079632679489662D0)
      PARAMETER (DEGRAD=0.0174532925199432958D0)
      PARAMETER (RADDEG=57.2957795130823209D0)
      PARAMETER (CLIGHT=29979245800.D0)
      PARAMETER (BIG=10000000000.D0)
      PARAMETER (EMASS=0.0005109990615D0)
      PARAMETER (EMMU=0.105658387D0)
      PARAMETER (PMASS=0.9382723128D0)
      PARAMETER (AVO=0.60221367D0)
*
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
      INTEGER MXGKIN
      PARAMETER (MXGKIN=100)
      COMMON/GCKING/KCASE,NGKINE,GKIN(5,MXGKIN),
     +                           TOFD(MXGKIN),IFLGK(MXGKIN)
      INTEGER       KCASE,NGKINE ,IFLGK,MXPHOT,NGPHOT
      REAL          GKIN,TOFD,XPHOT
C
      PARAMETER (MXPHOT=800)
      COMMON/GCKIN2/NGPHOT,XPHOT(11,MXPHOT)
C
      COMMON/GCKIN3/GPOS(3,MXGKIN)
      REAL          GPOS
C
      COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5)
C
      DIMENSION PELS(3)
      DIMENSION RNDM(2)
      LOGICAL ROTATE
C.
C.    ------------------------------------------------------------------
C.
C==============================================================
C change by Gary Taylor 16/7/95
C in the region of the vertex detector change the
C value of DCUTM,DCUTE by FACTOR. This will effectively change
C the cross section for delta ray production in this region by
C the same FACTOR.
C
      FACTOR=1.40
      RAD=SQRT(VECT(1)**2+VECT(2)**2)
      Z=VECT(3)
      DCUTME=DCUTM
      DCUTEE=DCUTE
      IF(RAD.GT.5..AND.RAD.LT.12..AND.ABS(Z).LT.30.) THEN
       DCUTME=DCUTM*FACTOR
       DCUTEE=DCUTE*FACTOR
      ENDIF
C==============================================================
      P=VECT(7)
      XE=GETOT
      TE=GEKIN
      GAM=XE/AMASS
      GAM2=GAM*GAM
      T=GAM-1.
C==============================================================
C change by Gary Taylor 16/7/95 dcute->dcutee
      X=DCUTEE/(T*EMASS)
C==============================================================
C
      KCASE = NAMEC(10)
      IF(IPART.EQ.3)   THEN
C
C======>       Moller scattering
C
        IF(X.GE.0.5) GO TO 90
        CC=1.-2.*X
C
  10    CALL GRNDM(RNDM,2)
        E=X/(1.-CC*RNDM(1))
C
        B1=4./(9.*GAM2-10.*GAM+5.)
        B2=T*T*B1
        B3=(2.*GAM2+2.*GAM-1.)*B1
        E1=1.-E
C
        SCREJ=B2*E*E-B3*E/E1+B1*GAM2/(E1*E1)
C
        IF(RNDM(2).GT.SCREJ) GOTO 10
C
      ELSEIF(IPART.EQ.2)THEN
C
C======>       Bhabha scattering
C
        IF(X.GE.1.) GO TO 90
        X1=1.-X
  20    CALL GRNDM(RNDM,2)
        E=X/(1.-X1*RNDM(1))
C
        Y=1./(GAM+1.)
        Y2=Y*Y
        C=1.-2.*Y
        B1=2.-Y2
        B2=C*(3.+Y2)
        C2=C*C
        B4=C2*C
        B3=C2+B4
        B0=GAM2/(GAM2-1.)
C
        SCREJ=(((B4*E-B3)*E+B2)*E-B1)*E+B0
        SCREJ=SCREJ/((((B4*X-B3)*X+B2)*X-B1)*X+B0)
        IF(RNDM(2).GT.SCREJ) GOTO 20
C
      ELSE
C
C======>     Heavy particle.
C
        TMAX=2.*EMASS*(GAM2-1.)/
     +  (1.+2.*GAM*EMASS/AMASS+(EMASS/AMASS)**2)
C==============================================================
C change by Gary Taylor 16/7/95 dcutm->dcutme
        IF(TMAX.LE.DCUTME)  GOTO 90
  40    CALL GRNDM(RNDM,2)
        E=1./DCUTME+RNDM(1)*(1./TMAX-1./DCUTME)
C==============================================================
        E=1./E
        BET2=1.-1./GAM2
        SCREJ=1.-BET2*(E/TMAX)
C ---         extra term for spin 1/2 parent.
        IF(AMASS.GT.0.9 .OR. AMASS.LT.0.12)
     +  SCREJ=SCREJ+0.5*(E/GETOT)**2
        IF(RNDM(2).GT.SCREJ) GO TO 40
        E=E/(T*EMASS)
C
      ENDIF
C
      EEL=(T*E+1.)*EMASS
      TEL=EEL-EMASS
      PEL=SQRT(ABS((EEL+EMASS)*TEL))
      COSTH=(XE*EEL+EMASS*(TEL-XE))/(P*PEL)
      IF(COSTH.GE.1.) THEN
         COSTH=1.
         SINTH=0.
      ELSEIF(COSTH.LE.-1.) THEN
         COSTH=-1.
         SINTH=0.
      ELSE
         SINTH=SQRT((1.+COSTH)*(1.-COSTH))
      ENDIF
      CALL GRNDM(RNDM,1)
      PHI    = TWOPI*RNDM(1)
      COSPHI = COS(PHI)
      SINPHI = SIN(PHI)
C
C             Polar co-ordinates to momentum components.
C
      NGKINE = 1
      GKIN(1,1)=PEL*SINTH*COSPHI
      GKIN(2,1)=PEL*SINTH*SINPHI
      GKIN(3,1)=PEL*COSTH
      GKIN(4,1)=EEL
      GKIN(5,1)=3
      TOFD(NGKINE)=0.
      GPOS(1,NGKINE) = VECT(1)
      GPOS(2,NGKINE) = VECT(2)
      GPOS(3,NGKINE) = VECT(3)
C
      PELS(1)=-GKIN(1,1)
      PELS(2)=-GKIN(2,1)
      PELS(3)=P-GKIN(3,1)
C
      CALL GFANG(VECT(4),COSTH,SINTH,COSPH,SINPH,ROTATE)
      IF(ROTATE) THEN
         CALL GDROT(PELS(1),COSTH,SINTH,COSPH,SINPH)
         CALL GDROT(GKIN,COSTH,SINTH,COSPH,SINPH)
      ENDIF
C
C             Correct track vector for lost energy and scattered angles
C
      TELS=TE-TEL
      EELS=TELS+AMASS
      PEELS=SQRT(ABS((EELS+AMASS)*TELS))
      IF(PEELS.GT.0.)THEN
         DO 55 I=1,3
            VECT(I+3) = PELS(I)/PEELS
  55     CONTINUE
      ENDIF
      VECT(7) = PEELS
      GEKIN=TELS
      GETOT=EELS
      CALL GEKBIN
      IF((IDRAY.NE.1).OR.(TEL.LE.CUTELE)) THEN
          NGKINE = 0
          DESTEP = DESTEP + TEL
      ENDIF
C
C             Update probabilities
C
  90  CALL GRNDM(RNDM,1)
      ZINTDR=-LOG(RNDM(1))
      SLDRAY=SLENG
      STEPDR=BIG
C
      END
