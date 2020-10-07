      SUBROUTINE EBCLCF(CLCCO)
C ------------------------------------------------------------------
C!  - Coefficients of ECAL clustering correction
CKEY PHOTONS ECAL CLUSTER CORRECTION
C!  Author: R.Edgecock - 900115
C!   Modified :- E. Lancon              2-MAY-1991
C!                      EPCC taken from setup scheme for MC
C!               F.Ranjard              6-Oct-1992
C!                      use GTDBBK to get EPCC
C!
C! This routine calculates the coefficients that should be used for the
C! ECAL clustering correction for given ROC Zero suppression thresholds
C! for the endcaps. It assumes the clustering correction is of the form:
C!
C!        D(E) = ALPHA*SQRT(E) + BETA*E, where D(E) is the correction to
C!                                       energy E and ALPHA and BETA are
C!                                       returned by this routine.
C!
C!   WARNINGS!!
C!   ==========
C!             (1) This routine returns the default values of ALPHA and
C!                 BETA for the barrel as nobody has studied this yet;
C!             (2) It takes no account of the clustering thresholds
C!                 (currently 30 MeV);
C!             (3) It assumes that the stack 1 zero suppression threshol
C!                 is less than the clustering threshold;
C!             (4) It works upto a stack 2 threshold of 45 MeV and stack
C!                 threshold of 65 MeV.
C!
C!   Input:     None
C!   Output:    CLCCO(Coeff,ECAL Component) is the array giving the
C!              coefficients:
C!                           Coeff = 1 => Alpha
C!                                 = 2 => Beta
C!                       ECAL comp = 1 => Endcap A
C!                                   2 => Barrel
C!                                   3 => Endcap B
C!
C!   Banks Used: EPCC (Stored on DataBase)
C!               EZTH (Comes with run header)
C!
C! Should be called once at the start of each run
C?
C!======================================================================
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JRUNEN=1,JRUNRN=2,JRUNRT=3,JRUNSD=4,JRUNST=5,LRUNHA=5)
      PARAMETER(JEPCID=1,JEPCVR=2,JEPCBD=4,JEPCED=6,JEPCS2=8,JEPCS3=12,
     +          JEPCPP=15,LEPCCA=26)
      PARAMETER(JEVEEN=1,JEVERN=2,JEVERT=3,JEVEDA=4,JEVETI=5,JEVEEV=6,
     +          JEVEM1=7,JEVEM2=8,JEVEM3=9,JEVEM4=10,JEVETY=11,
     +          JEVEES=12,JEVETE=13,LEVEHA=13)
      PARAMETER (NECCO=2,MODTY=3)
      REAL CLCCO(NECCO,MODTY),POSP2(4),POSP3(3),ESTHR(3,3)
      INTEGER ALGTDB, GTSTUP
      EXTERNAL ALGTDB, GTSTUP
      CHARACTER DET*2 , LIST*4
      DATA DET/'EC'/,LIST/'EPCC'/
      DATA NRUNH/0/
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C
      IF (NRUNH.EQ.0) THEN
         NRUNH=NAMIND('RUNH')
         NEVEH=NAMIND('EVEH')
         NEPCC=NAMIND('EPCC')
         NEZTH=NAMIND('EZTH')
      ENDIF
C
C Initialise...fill CLCCO with the values used before this routine exist
C
      DO 10 I = 1,3
         CLCCO(1,I) = 0.085
         CLCCO(2,I) = 0.00
   10 CONTINUE
C
C Check whether the EPCC bank exists and quit if it doesn't.
C
      LDUM = 0
      LUNDB = JUNIDB(LDUM)
C
C Find the run number
C
C
C?   Look at EPCC Bank, This bank is year dependant (it has changed in
C?   91), furthermore the coefficients of this bank are strongly correla
C?   the normalisation coefficients from ECCA bank.
C

C! Get banks from DB depending on run and setup code
C
      CALL ABRUEV (IRUN,IEVT)
      IRET = 0
      IF (IRUN.NE.IROLD) THEN
        IROLD = IRUN
        IF (IRUN.LE.2000) THEN
           ITP = GTSTUP (DET,IRUN)
        ELSE
           ITP = IRUN
        ENDIF
        IRET= ALGTDB(JUNIDB(0),LIST,-ITP)
      ENDIF
C
      KEPCC = IW(NEPCC)
      IF (KEPCC.EQ.0) GOTO 999
C
C If it does exist, update the default CLCCO values.
C
      DO 20 I = 1,2
         CLCCO(I,2) = RTABL(KEPCC,1,JEPCBD+(I-1))
         CLCCO(I,1) = RTABL(KEPCC,1,JEPCED+(I-1))
         CLCCO(I,3) = CLCCO(I,1)
   20 CONTINUE
C
C    Now just for backward compatibility, quit for date > 1991
C
      IF (GTSTUP('EC',IRUN) .GT. 2) GOTO 999
C
C and pick up the threshold points
C
      DO 21 I = 1,4
         POSP2(I) = RTABL(KEPCC,1,JEPCS2+(I-1))
         IF (I.LE.3) POSP3(I) = RTABL(KEPCC,1,JEPCS3+(I-1))
   21 CONTINUE
C
C Now check for the EZTH bank, which has the thresholds used, and again
C quit if it doesn't exist.
C
      CALL EBTZTH(ESTHR,IRC)
      IF (IRC.NE.0) GO TO 999
C
C Ok man, we have the thresholds, so find out where they are and
C calculate the corresponding coefficients
C
      DO 110 MODR = 1,3,2
         DIFM2 = 99999.
         NPOS2 = 4
         THR2 = ESTHR(2,MODR)
         DO 100 I=1,4
            IF (ABS(THR2 - POSP2(I)).LT.DIFM2) THEN
               DIFM2 = ABS(THR2 - POSP2(I))
               NPOS2 = I
            ENDIF
  100    CONTINUE
C
         DIFM3 = 99999.
         NPOS3 = 3
         THR3 = ESTHR(3,MODR)
         DO 101 I=1,3
            IF (ABS(THR3 - POSP3(I)).LT.DIFM3) THEN
               DIFM3 = ABS(THR3 - POSP3(I))
               NPOS3 = I
            ENDIF
  101    CONTINUE
C
C As the errors on the determined coefficients are large, it is
C sufficient just to take the nearest point, NPOS2, NPOS3
C
         CLCCO(1,MODR) = RTABL(KEPCC,1,JEPCPP - 1 + (NPOS3-1)*4 + NPOS2)
C
  110 CONTINUE
C
  999 CONTINUE
      RETURN
      END
