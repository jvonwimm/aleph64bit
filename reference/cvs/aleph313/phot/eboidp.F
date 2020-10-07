      SUBROUTINE EBOIDP(NUST , INDX , ESTO , ICRE , MODU , ECLS )
C ----------------------------------------------------
C   AUTHOR   : J. Badier     25/09/89
C!  Stacks and modules contents.
CKEY PHOTONS STACKS MODULE EXTENSION / INTERNAL
C
C   Input     : NUST    Number of storeys of the cluster ICLN.
C               INDX(1,IST) Theta index of the storey IST.
C               INDX(2,IST) Phi index of the storey IST.
C               INDX(3,IST) Stack number of the storey IST.
C               ESTO(IST)   Raw content of the storey IST.
C                           IST = 1 , NUST
C
C   Output    : ICRE(4)    Describe the cluster extension over
C                          several modules.
C               MODU(M,1)  Module number of the module M.
C               MODU(M,2)  Subcomponent number of the module M.
C               ECLS(K,M)  Content of the stack K in the module M.
C                       Up to 3 modules ordered as decreasing energies.
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C
C-----------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION INDX(3,*) , ESTO(*) , ICRE(*) , MODU(3,*) , ECLS(3,*)
      DIMENSION LMOL(3) , ENRG(4) , LREG(4)
C   Initialisations.
      NBMO = 0
      DO 1 I = 1 , 3
        LMOL(I) = 0
        MODU(I,1) = 0
        MODU(I,2) = 0
        DO 2 J = 1 , 3
          ECLS(I,J) = 0.
    2   CONTINUE
        LREG(I) = 0
        ENRG(I) = 0.
    1 CONTINUE
      LREG(4) = 0
      ENRG(4) = 0.
C   Loop over the storeys
      DO 3 ISTO = 1 , NUST
C   Search subcomponent , module and region.
        CALL EMDTOW( INDX(1,ISTO),INDX(2,ISTO),ISCI,IMOD,NREG )
        IF( IMOD .LE. 0 .OR. IMOD .GT. 12  .OR.
     +      NREG .LE. 0 .OR. NREG .GT.  4  .OR.
     +      ISCI .LE. 0 .OR. ISCI .GT. 3 ) GO TO 101
        NMOD = IMOD + 12 * ( ISCI - 1 )
C   Dispatch over modules.
        IF( NBMO .NE. 0 ) THEN
          DO 5 JMOD = 1 , NBMO
            IF( LMOL( JMOD ) .EQ. NMOD ) GO TO 4
    5     CONTINUE
        ENDIF
        IF ( NBMO .GE. 3 ) GO TO 101
        NBMO = NBMO + 1
        LMOL(NBMO) = NMOD
        MODU(NBMO,1) = IMOD
        MODU(NBMO,2) = ISCI
        JMOD = NBMO
    4   CONTINUE
        KFIN = INDX(3,ISTO)
        ECLS(KFIN,JMOD) = ECLS(KFIN,JMOD) + ESTO(ISTO)
C   Dispatch over regions.
        LREG(NREG) = 1
        ENRG(NREG) = ENRG(NREG) + ESTO(ISTO)
    3 CONTINUE
C   Reorder modules according to decreasing energy.
      IF (NBMO.GT.1) THEN
        I1 = 0
    7   I1 = I1 + 1
        ESUP = 0.
        IMAX = 0
        DO 6 IM = I1 , NBMO
          EMOD = ECLS(1,IM) + ECLS(2,IM) + ECLS(3,IM)
          IF (EMOD.GT.ESUP) THEN
            IMAX = IM
            ESUP = EMOD
          ENDIF
    6   CONTINUE
        IF (IMAX.NE.I1) THEN
C
          ISAV         = LMOL(I1)
          LMOL(I1)    = LMOL(IMAX)
          LMOL(IMAX) = ISAV
C
          ISAV         = MODU(I1,1)
          MODU(I1,1)  = MODU(IMAX,1)
          MODU(IMAX,1) = ISAV
C
          ISAV         = MODU(I1,2)
          MODU(I1,2)    = MODU(IMAX,2)
          MODU(IMAX,2) = ISAV
C
          DO 8 I = 1 , 3
            XSAV           = ECLS(I,I1)
            ECLS(I,I1)    = ECLS(I,IMAX)
            ECLS(I,IMAX) = XSAV
    8     CONTINUE
C
        ENDIF
        IF( I1 .LT. NBMO - 1 ) GO TO 7
      ENDIF
C   Define region code. 1 = Region , 2 = Secondary crack in overlap ,
C   3 = Crack , 4 = Overlap.
      ICRE(2) = 0
      IF( NBMO .EQ. 1 ) THEN
C   Out of cracks and overlap.
        ICRE(4) = 0
        ICRE(3) = 0
      ELSE
        IF( NBMO .EQ. 2 .AND. MODU(2,2) .EQ. MODU(1,2) ) THEN
C   It is a crack.
          ICRE(4) = 0
          IF( MODU(1,1) .GT. MODU(2,1)   .OR.
     +        MODU(1,1) .EQ. 1         ) THEN
            ICRE(3) = 1
          ELSE
            ICRE(3) = 2
          ENDIF
        ELSE
C   Main detector . ICRE(4) = 1 : Barrel.
C                   ICRE(4) = 2 : Endcap.
          ICRE(4) = MODU(1,2) - 1
          IF( ICRE(4) .EQ. 0 ) ICRE(4) = 2
          IF( NBMO .NE. 3 ) THEN
C   Overlap
            ICRE(3) = 0
          ELSE
C   Overlap and crack.
            IF( MODU(2,2) .EQ. MODU(3,2) ) THEN
C               ICRE(4)   ICRE(3)              ICRE(2)
C   Ba Ec Ec     1      1 : #Ec2>#Ec3     1 : Ba>Ec2+Ec3
C                       2 : #Ec2<#Ec3     0 : Ba<Ec2+Ec3
C   Ec Ba Ba     2      1 : #Ba2>#Ba3     1 : Ec>Ba2+Ba3
C                       2 : #Ba2<#Ba3     0 : Ec<Ba2+Ba3
              SOM1 = ECLS(1,1) + ECLS(2,1) + ECLS(3,1)
              SOM2 = ECLS(1,2) + ECLS(2,2) + ECLS(3,2)
              SOM3 = ECLS(1,3) + ECLS(2,3) + ECLS(3,3)
              IF( SOM1 .GT. SOM2 + SOM3 ) ICRE(2) = 1
              IF( MODU(2,1) .GT. MODU(3,1) .OR.
     +            MODU(2,1) .EQ. 1 )     THEN
                ICRE(3) = 1
              ELSE
                ICRE(3) = 2
              ENDIF
            ELSE
              IF( MODU(1,2) .EQ. MODU(2,2) ) THEN
C   Ba Ba Ec     if      #Ba1>#Ba2       ICRE(3) = 1
C   Ec Ec Ba     if      #Ec1>#Ec2       ICRE(3) = 1
                IF( LMOL(1) .GT. LMOL(2)        .OR.
     +              LMOL(2) - LMOL(1) .EQ. 11 ) THEN
                  ICRE(3) = 1
                ELSE
                  ICRE(3) = 2
                ENDIF
              ELSE
C   Ba Ec Ba     if      #Ba1>#Ba3       ICRE(3) = 1
C   Ec Ba Ec     if      #Ec1>#Ec3       ICRE(3) = 1
                IF( LMOL(1) .GT. LMOL(3)        .OR.
     +              LMOL(3) - LMOL(1) .EQ. 11 ) THEN
                  ICRE(3) = 1
                ELSE
                  ICRE(3) = 2
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C   Limits of regions.
      IRG = 0
      DO 15 I = 1 , 4
        IF( LREG(I) .EQ. 1 ) GO TO 11
   15 CONTINUE
      GO TO 101
   11 IF( I .GE. 4 ) THEN
        ICRE(1) = 0
      ELSE
        IF( LREG(I+1) .EQ. 0 ) THEN
          ICRE(1) = 0
        ELSE
          ICRE(1) = 1
          IF( ENRG(I) .GT. ENRG(I+1) ) ICRE(1) = 2
        ENDIF
      ENDIF
C
   98 CONTINUE
      RETURN
C
C   More than 3 modules or illegal region.
  101 NUST = 0
      GO TO 98
      END
#endif
