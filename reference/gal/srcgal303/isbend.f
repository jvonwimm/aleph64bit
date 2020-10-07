      SUBROUTINE ISBEND(Z, ZM)
C.
C...ISBEND  2.00  920213  18:59                        R.Beuselinck
C.
C!  Calculate apparent measured Z with non-linear S-bend parameters.
C.
C.  Arguments:
C.  Z   [S,I,REAL] : Z coordinate on ITC wire (cm).
C.  ZM  [S,O,REAL] : "Measured Z" from inverting S-bend relation.
C.
C.  The measured Z is computed according to an interpolation
C.  table generated from the parametrisation of Z = F(ZM) of the form
C.           Z = S1*ZM + S2*SIN(2*Pi/S3 * ZM)
C.  where S1,S2,S3 are the "S-bend" coefficients from the IZNL bank.
C.  This routine must be initialised by first calling ISBGIN.
C.
C-----------------------------------------------------------------------
      SAVE
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
      REAL TDIF(101), Z, ZMIN, ZBIN, ZMAX, DZ, ZM, S1, S2, S3
      REAL FLZ, PRED, AA, BB, CC, DD, EE, T1, T2, TT, Z1, Z2, ZZ, TOLZ
      REAL SA, SB, SC, ZWANT
      INTEGER IZB, I
C
C--  Statement functions for parametrization and interpolation.
C--
      FLZ(AA,SA,SB,SC) = SA*AA + SB*SIN(TWOPI*AA/SC)
      PRED(AA,BB,CC,DD,EE) = EE + (DD-EE)*(CC-BB)/(AA-BB)
C
      IF (Z .EQ. ZMIN) THEN
         ZM = TDIF(1)
      ELSEIF (Z.EQ.-ZMIN) THEN
         ZM = TDIF(101)
      ELSE
         IZB = (Z - ZMIN)/ZBIN + 1
         DZ = Z - ZMIN - (IZB - 1)*ZBIN
         ZM = (TDIF(IZB+1) - TDIF(IZB))*DZ/ZBIN + TDIF(IZB)
      ENDIF
      RETURN
      ENTRY ISBGIN(ZMAX, S1, S2, S3)
C.
C...ISBGIN  2.00  920213  19:13                        R.Beuselinck
C.
C!  Setup lookup table for inverse S-bend parametrisation.
C.
C.  Arguments:
C.  ZMAX  [S,I,REAL] : Maximum Z coordinate to interpolate.
C.  S1    [S,I,REAL] : S-bend parameter 1.
C.  S2    [S,I,REAL] : S-bend parameter 2.
C.  S3    [S,I,REAL] : S-bend parameter 3.
C.
C.  The input S-bend parameters describe the non-linearity of time
C.  difference measured as a function of Z along the ITC wires.
C.  This routine computes the true Z for a set of equally spaced
C.  measured Z positions along the wires which are then used for a fast
C.  interpolation of the inverse function, i.e. Zmease as a function
C.  of Ztrue.
C.  This routine is the initialisation entry point for ISBEND.
C.
C.  NB: This routine used to work in terms of time difference rather
C.  than measured Z, so the variables T1, T2, TT, TDIF should now
C.  be understood as referring to measured Z values.
C.
C-----------------------------------------------------------------------
      ZMIN = -ZMAX
      ZBIN = (ZMAX - ZMIN)/100.
      TOLZ = ZBIN/100.
C
C--  Loop over values of Zm required for interpolation table and seek
C--  a Zt value using a search technique.
C--
      DO 100 I=1,101
        ZWANT = ZMIN + (I-1)*ZBIN
        T1 = 1.5*ZMAX
        T2 = -T1
        Z1 = FLZ(T1,S1,S2,S3)
        Z2 = FLZ(T2,S1,S2,S3)
   10   TT = PRED(Z1,Z2,ZWANT,T1,T2)
        ZZ = FLZ(TT,S1,S2,S3)
        IF (ABS(ZZ-ZWANT).LE.TOLZ) THEN
          TDIF(I) = TT
        ELSE IF (ZZ.GT.ZWANT) THEN
          Z1 = ZZ
          T1 = TT
          GO TO 10
        ELSE
          Z2 = ZZ
          T2 = TT
          GO TO 10
        ENDIF
  100 CONTINUE
      END
