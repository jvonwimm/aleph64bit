      SUBROUTINE VDLAND(STEP, AMASS, P, E, ITRTYP, CHARGE, DE)
C -------------------------------------------------------------
C! Compute Landau fluctuations in silicon
CKEY VDET DIGITIZE
C!
C-- This subroutine is the same as GLANDZ in GEANT 3.16
C-- EXCEPT:
C--  1    It has been modified to have the medium as Silicon, the
C--       corresponding calling arguments have been removed.
C--  2    The variables passed through GCKINE (mass, charge and ITRTYP)
C--       have become calling arguments.
C--  3    All the GEANT common blocks have been eliminated.
C.
C.    ******************************************************************
C.    *                                                                *
C.    *  Energy straggling using a Monte Carlo model.                  *
C.    *  It can be used with or without delta ray generation.          *
C.    *                                                                *
C.    *   It is a NEW VERSION of the model , which reproduces          *
C.    *   the experimental data rather well.                           *
C.    *                                                                *
C.    *  Input : STEP  = current step-length (cm)                      *
C.    *          AMASS, P, E, ITRTYP, CHARGEC : part. characteristics  *
C.    *  Output: DE    = actual energy loss (Gev)                      *
C.    *                 ( NOT the fluctuation DE/DX-<DE/DX> !)         *
C.    *                                                                *
C.    *     ==> Called by : GTELEC,GTHADR,GTMUON                       *
C.    *                                                                *
C.    *  Author      : L.Urban                                         *
C.    *  Date        : 28.04.1988       Last update :  1.02.90         *
C.    *                                                                *
C.    ******************************************************************
C.
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      REAL EMMU,PMASS,AVO
C-- *
      PARAMETER (PI=3.14159265358979324)
      PARAMETER (TWOPI=6.28318530717958648)
      PARAMETER (PIBY2=1.57079632679489662)
      PARAMETER (DEGRAD=0.0174532925199432958)
      PARAMETER (RADDEG=57.2957795130823209)
      PARAMETER (CLIGHT=29979245800.)
      PARAMETER (BIG=10000000000.)
      PARAMETER (EMASS=0.00051099906)
      PARAMETER (EMMU=0.105658389)
      PARAMETER (PMASS=0.93827231)
      PARAMETER (AVO=0.60221367)

C--  Arguments
      REAL STEP, P, E, DE, CHARGE
      INTEGER ITRTYP

C--  silicon parameters
      REAL Z, POTI, POTIL, DEDX

C-- Some cuts
      REAL DCUTM, DCUTE
      PARAMETER (
     $   DCUTM = 0.001,
     $   DCUTE = 0.001
     $   )

C--  Local parameters
      PARAMETER (MAXRND=100)
      DIMENSION RNDM(MAXRND),APOIS(3),FPOIS(3), NPOIS(3)
      PARAMETER (HMXINT=2**30)
      PARAMETER (ONE=1.,HALF=ONE/2,ZERO=0.)
      PARAMETER (RCD=0.40, RCD1=1.-RCD, PROBLM=0.01)
      PARAMETER( C1=4., C2=16.)

C-- local variables
C-- *
C-- *     --------------------------------------------------------------
C-- *
C-- *     POTI=1.6E-8*Z**0.9
C-- *     POTIL=LOG(POTI)
C-- *
C--       IF(Z.GT.2.) THEN
C--          F2=2./Z
C--       ELSE
C--          F2=0.
C--       ENDIF

      Z = 14.0
      POTI=1.6E-8*Z**0.9
      POTIL=LOG(POTI)

C-- DEDX has to be tuned so that we get the same pulsht distribution
C-- from data and monte carlo.  program ud:[manoj.tem.v95.stal]pulsh.for
C-- written by alan.
C-- multistep process.
C-- 1  put in a guess for DEDX, compare the pulsht obtained from data an
C--    MC.
C-- 2  reduce the mean by a FACTOR and scale the whole array (in the cal
C--    routine by the same FACTOR to get the width right.
C--      DEDX = 6.7E-3
      DEDX = 2.2E-3


      F2 = 2.0/Z
      F1=1.-F2
C-- *
      E2 = 1.E-8*Z*Z
      E2L= LOG(E2)
      E1L= (POTIL-F2*E2L)/F1
      E1 = EXP(E1L)
C-- *
C-- *
      P2=P*P
      B2=P2/(E*E)
      BG2=P2/(AMASS*AMASS)
      IF(ITRTYP.EQ.2) THEN
         TM=P2/(E+EMASS)
         IF(CHARGE.LT.0.) TM=TM/2.
         TM=TM-POTI
         IF(TM.GT.DCUTE) TM=DCUTE
      ELSE
         TM=EMASS*P2/(0.5*AMASS*AMASS+EMASS*E)
         TM=TM-POTI
         IF(TM.GT.DCUTM) TM=DCUTM
      ENDIF
C-- *
C-- *
C-- * *** Protection against negative TM    ---------------------
C-- *     TM can be negative only for heavy particles with  a very
C-- *     low kinetic energy (e.g. for proton with T  100-300 kev)
      TM=MAX(TM,ZERO)
C-- *
      W  = TM+POTI
      WW = W/POTI
      WWW= 2.*EMASS*BG2
      WL = LOG(WWW)
      CSB=STEP*RCD1*DEDX/(WL-POTIL-B2)
      APOIS(1)=CSB*F1*(WL-E1L-B2)/E1
      APOIS(2)=CSB*F2*(WL-E2L-B2)/E2
C-- *
      IF(TM.GT.0.) THEN
         APOIS(3)=RCD*DEDX*STEP*TM/(POTI*W*LOG(WW))
      ELSE
         APOIS(1)=APOIS(1)/RCD1
         APOIS(2)=APOIS(2)/RCD1
         APOIS(3)=0.
      ENDIF
C-- *
C-- *    calculate the probability of the zero energy loss
C-- *
      APSUM=APOIS(1)+APOIS(2)+APOIS(3)
      IF(APSUM.LT.50.) THEN
         PROB=EXP(-APSUM)
      ELSE
         PROB=0.
      ENDIF
C-- *
C-- *
C-- *      do it differently if prob > problm  <====================
      IF(PROB.GT.PROBLM) THEN
         E0=1.E-8
         EMEAN=DEDX*STEP
         IF(TM.LE.0.) THEN
C-- *      excitation only ....
            APOIS(1)=EMEAN/E0
C-- *
            CALL GPOISS(APOIS,NPOIS,1)
            FPOIS(1)=NPOIS(1)
            DE=FPOIS(1)*E0
C-- *
         ELSE
C-- *         ionization only ....
            EM=TM+E0
            APOIS(1)=EMEAN*(EM-E0)/(EM*E0*LOG(EM/E0))
            CALL GPOISS(APOIS,NPOIS,1)
            NN=NPOIS(1)
            DE=0.
C-- *
            IF(NN.GT.0) THEN
               RCORR=1.
               IF(NN.GT.MAXRND) THEN
                  RCORR=FLOAT(NN)/MAXRND
                  NN=MAXRND
C-- *
               ENDIF
               W=(EM-E0)/EM
               CALL GRNDM(RNDM,NN)
               DO 10 I=1,NN
                  DE=DE+E0/(1.-W*RNDM(I))
   10          CONTINUE
               DE=RCORR*DE
C-- *
            ENDIF
         ENDIF
         GOTO 999
      ENDIF
C-- *
      IF(MAX(APOIS(1),APOIS(2),APOIS(3)).LT.HMXINT) THEN
         CALL GPOISS(APOIS,NPOIS,3)
         FPOIS(1)=NPOIS(1)
         FPOIS(2)=NPOIS(2)
         FPOIS(3)=NPOIS(3)
      ELSE
         DO 20 JPOIS=1, 3
            IF(APOIS(JPOIS).LT.HMXINT) THEN
               CALL GPOISS(APOIS(JPOIS),NPOIS(JPOIS),1)
               FPOIS(JPOIS)=NPOIS(JPOIS)
            ELSE
               CALL GRNDM(RNDM,2)
               FPOIS(JPOIS)=ABS(SQRT(-2.*LOG(RNDM(1)*ONE)
     +         *APOIS(JPOIS))*SIN(TWOPI*RNDM(2)*ONE)+APOIS(JPOIS))
            ENDIF
   20    CONTINUE
      ENDIF
C--
C-- *
C-- *          Now we have all three numbers in REAL/DOUBLE
C-- *          variables. REALK is actually an INTEGER that now may
C-- *          exceed the machine representation limit for integers.
C-- *
      DE=FPOIS(1)*E1+FPOIS(2)*E2
C-- *
C-- *     smear to avoid peaks in the energy loss (note: E1<<E2)
C-- *
      IF(DE.GT.0.) THEN
         CALL GRNDM(RNDM,1)
         DE=DE+E1*(1.-2.*RNDM(1))
      ENDIF
C-- *
      ALFA=1.
      REALK=0
      GDEC=0.
C-- *
      ANC=FPOIS(3)
      IF(ANC.GE.C2) THEN
         R=ANC/(C2+ANC)
         AN=ANC*R
         SN=C1*R
         CALL GRNDM(RNDM,2)
         RR=SQRT(-2.*LOG(RNDM(1)))
         PHI=TWOPI*RNDM(2)
         X=RR*COS(PHI)
         AK=AN+SN*X
         ALFA=WW*(C2+ANC)/(C2*WW+ANC)
         EA=AK*POTI*ALFA*LOG(ALFA)/(ALFA-1.)
         SA=SQRT(ABS(AK*ALFA*POTI*POTI-EA*EA/AK))
         AKL=(EA-C1*SA)/POTI
         IF(AK.LE.AKL) THEN
            X=RR*SIN(PHI)
            GDEC=EA+SA*X
            REALK=AK+HALF-MOD(AK+HALF,ONE)
         ELSE
            ALFA=1.
         ENDIF
      ENDIF
      NN=NINT(FPOIS(3)-REALK)
      IF(NN.GT.MAXRND) THEN
         W=1.-ALFA/WW
         WW=POTI*ALFA
C-- *
C-- *     Here we take a gaussian distribution to avoid loosing
C-- *     too much time in computing
C-- *
         AVERAG=-LOG(1.-W)/W
         SIGMA =SQRT(NN*(1./(1.-W)-AVERAG**2))
         CALL GRNDM(RNDM,2)
         GDEC  = GDEC+WW*(NN*AVERAG+SIGMA*SQRT(-2.*LOG(RNDM(1)))*
     +           SIN(TWOPI*RNDM(2)))
         DE=DE+GDEC
      ELSEIF(NN.GT.0) THEN
         W=1.-ALFA/WW
         WW=POTI*ALFA
         CALL GRNDM(RNDM,NN)
         DO 30 I=1,NN
            GDEC=GDEC+WW/(1.-W*RNDM(I))
   30    CONTINUE
         DE=DE+GDEC
      ENDIF
C-- *
  999 CONTINUE
      END
