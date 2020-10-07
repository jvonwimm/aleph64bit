      SUBROUTINE AGSI92
C ----------------------------------------------------------
C - B.Bloch-Devaux                  910930
C! build SCAL geometry for crystals and ceramics
CKEY SCAL GEOM
C - called by AGSCAL
C - calls     GSROTM,GSPOS                      from GEANTLib
C - calls     ALTELL                            from ALEPHLib
C ---------------------------------------------------------
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
      PARAMETER  (D360=TWOPI*RADEG   ,D180=PI*RADEG   ,D90=0.5*D180)
      PARAMETER  (D45=0.5*D90   ,D15=D45/3.   ,D210=7.*D90/3.)
      PARAMETER  (D225=D180+D45   ,D270=D180+D90  ,D7P5=0.5*D15)
      PARAMETER(LSENV=30)
      PARAMETER (LIMVOL=17)
C
      COMMON/AGCONS/ IAGROT,IAGMAT,IAGMED,IAGFHB,IAGSLV,IAGSEN(LSENV,2)
     2      , NAGIMP,LAGIMP(3,LIMVOL)
C
       COMMON /WRKSPC/ WSPACE(88320)
      PARAMETER (LPTAB=50)
      DIMENSION PTAB(LPTAB),JTAB(LPTAB)
      EQUIVALENCE (PTAB(1),JTAB(1),WSPACE(1))
C
      COMMON/SIGACO/NMODSI,NRBNSI,NPBNSI,NZBNSI,Z0SNSI(2),
     $              ZWIDSI,ZWFRSI,ZWFLSI,ZWLASI,ZWRFSI,ZWRLSI,OVLPSI
      DIMENSION IROT(3)
C=======================================================================
      DELTOT = ZWFRSI+(NZBNSI-1)*ZWIDSI+ZWLASI
      DSIL0= 2.*D360/NPBNSI
      IROT(1) = IAGROT-1
      IROT(2) = 0
      IROT(3) = IAGROT
      Z = 0.5*(DELTOT-ZWIDSI-2.*ZWLASI)
      X = 0.
      Y = 0.
      DO 10 I = NZBNSI-1,1,-1
C.......... modules are rotated by DELPHI,0,-DELPHI,DELPHI,0,... etc
        IR = MOD(( I-1),3)+1
        CALL GSPOS  ('SMOD',I,'SACT',X,Y,Z,IROT(IR),'ONLY')
        Z = Z - ZWIDSI
   10 CONTINUE
      OO = OVLPSI/SIN(0.5*DSIL0*DEGRA)
      ALSIL = 0.5*DSIL0
      IR = 0
      DO 20 I=1,8
         J = 2*(I-1)+1
         K = 2*I
         XSIL = - OO*COS(ALSIL*DEGRA)
         YSIL = - OO*SIN(ALSIL*DEGRA)
         CALL GSPOS('SKAP',J,'SSN1',XSIL,YSIL,0.,IR,'ONLY')
         CALL GSPOS('SCRM',J,'SSN2',0.  ,0.  ,0.,IR,'ONLY')
         IAGROT = IAGROT+1
         DSIL = DSIL0*(2*I-1)
         CALL GSROTM(IAGROT,D90,DSIL,D90,D90+DSIL,0.,0.)
         IR = IAGROT
         ALSIL = ALSIL+DSIL0
         XSIL = - OO*COS(ALSIL*DEGRA)
         YSIL = - OO*SIN(ALSIL*DEGRA)
         CALL GSPOS('SKAP',K,'SSN2',XSIL,YSIL,0.,IR,'ONLY')
         CALL GSPOS('SCRM',K,'SSN3',0.  ,0.  ,0.,IR,'ONLY')
         IF ( I.EQ.8) GO TO 20
         IAGROT = IAGROT+1
         DSIL = 2*I*DSIL0
         CALL GSROTM(IAGROT,D90,DSIL,D90,D90+DSIL,0.,0.)
         IR = IAGROT
         ALSIL = ALSIL+DSIL0
 20   CONTINUE
      RETURN
      END
