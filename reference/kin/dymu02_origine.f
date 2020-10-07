C-----------------------------------------------------------------------
C  A  L  E  P  H   I  N  S  T  A  L  L  A  T  I  O  N    N  O  T  E  S |
C                                                                      |
C    original code : DYMU3 from J.E Campagne and R.Zitoun              |
C    trasmitted by : J.E. Campagne, SEPTEMBER 1989                     |
C    modifications to the code ( description,author,date)              |
C    1. Change random number generator RNDM2  to RNDM so we can use    |
C       B. Bloch November 89                            RANMAR         |
C       That is : replace all RNDM2 by RNDM                            |
C                             RD2OUT (I1,I2) by RDMOUT(ISEED)          |
C                             with ISEED(3)                            |
C    2. After discussion with J.E. Campagne, modify  DYMUS to          |
C       compute W1MAX more precisely
C    3. Introduce an entry in DYMUS to recompute W1MAX in case coupling|
C       constants have been changed (used when generating mixtures)    |
C    4. Add dummy LUTAUD routine in case modified LUND is not loaded   |
C       standard scheme will be used.                                  |
C    5. Use HEAVY FLAVOR version of PVECT in any case ,upgraded in     |
C       collaboration with the author and protected against |cos|>1    |
C    6. Declare function RABSP and DGAUSS DOUBLE PRECISION in EXSPIN to|
C.      avoid pbs......           B. Bloch September 1995              |
C    7. use always real arg for RNDM in dfunc  B.Bloch October 2000    |
************************************************************************
************************************************************************
*                                                                      *
*                      DYMU3 PROGRAM VERSION 2.0                       *
*                            19TH NOV  89                              *
*                                                                      *
*         (E+) + (E-) ------> (F+) +(F-) + (UP TO 3 PHOTONS)           *
*                             F:MU, or Quark                           *
*             there is also an generation of tau with a tau decaying   *
*             into pion, but it was not tested against KORALZ.         *
*                                                                      *
*     AUTHORS : J.E.CAMPAGNE     DIVISION EP AT CERN                   *
*               ELECTRONIC MAIL : CAMPAGNE@CERNVM                      *
*                                 CAMPAGNE@FRCPN11                     *
*               AND R.ZITOUN                                           *
*               LPNHE . UNIVERSITES DE PARIS VI-VII                    *
*               4,PLACE JUSSIEU -TR 33 - RC                            *
*               75 252  PARIS    CEDEX 05   FRANCE                     *
*                                                                      *
*     REFERENCES:                                                      *
*         LPNHEP 88-06                                                 *
*         J.E.CAMPAGNE Ph.D Thesis LPNHEP 89-02                        *
*     +   Z. PHYS. C PART. AND FIELDS 43,469-475(1989)                 *
*     +   Z PHYSICS AT LEP Vol3: 2.2.5, 3.1.3, 3.1.5, 3.2.5            *
*     +   THE PROCEEDINGS OF THE BRIGHTON WORKSHOP (9/07/89)           *
*                                                                      *
*     THE SUBROUTINES THE USER MAY MODIFY ARE :                        *
*                                                                      *
*        * RADCOR : MAIN PROGRAM WITH PARAMETERS :                     *
*                                                                      *
*           NEVMAX:NUMBER OF EVENTS TO BE GENERATED PER RUN            *
*                                                                      *
*           TIMAX :TIME LIMIT FOR THE OVERALL JOB                      *
*                                                                      *
*        * INIRUN(IRUN): INITIALISATION OF PARAMETERS FOR EACH RUN     *
*                                                                      *
*               # WEAK PARAMETERS : OUTGOING FERMION NATURE            *
*                                   Z0 MASS     : AMZ                  *
*                                   Z0 WIDTH    : GAMM                 *
*                                   SIN2(TETAW) : SW2                  *
*                                                                      *
*               # BEAM ENERGY      :EBEAM                              *
*                                                                      *
*               # INITIAL RANDOM NUMBER GENERATOR SEEDS    :I1,I2      *
*                                                                      *
*        * USER(NEVT) : CALLED BY RADCOR FOR EACH NEW EVENT            *
*            COMMON VECLAB CONTAINS 4-MOMENTA OF FINAL STATE           *
*            PARTICLES WITH OBVIOUS NAMES                              *
*                                                                      *
*            IN ADDITION THIS ROUTINE IS CALLED AT THE END OF          *
*            EACH RUN WITH ARGUMENT NEVT=0 .                           *
*            IN COMMON /RESULT/ WILL BE FOUND:                         *
*                                                                      *
*               # SIGTOT +/- ERRSIG = TOTAL CROSS SECTION + STAT ERR   *
*               # ASYTOT +/- ERRASY = TOTAL ASYMMETRY + STAT ERR       *
*                                                                      *
*                                                                      *
************************************************************************
      PROGRAM RADCOR
      COMMON /ZDUMP/ IDUMP,NEVT
      COMMON /PAWC/ HMEMOR(20000)
      CALL HLIMIT(20000)
*
C     CALL TIMON
      TIMAX = 2.
      SIMUL = 0.
      NEVMAX = 10000
*
      DO 100 IRUN = 1,1
*
        CALL INIRUN(IRUN)
        IF(SIMUL.NE.0)CALL HEADER
        NEVT = 0
1       NEVT=NEVT+1
        IDUMP = 0
        IF(NEVT.LT.0)IDUMP = 1
        IF(IDUMP.GE.1)
     &  WRITE(6,*)'-----------------  EVENT NO',NEVT,'-----------------'
*
        CALL DYMUS(WEIGHT)
        CALL USER(NEVT,WEIGHT)
        IF(SIMUL.NE.0)CALL OUTEV
        CALL TIMEL(REBOUR)
        IF(NEVT.EQ.NEVMAX.OR.REBOUR.LT.TIMAX)GOTO2
        GOTO1
2       CALL FINISH(IRUN,NEVT)
        IF(SIMUL.NE.0)CALL TRAILER
        CALL USER(0,0.)
        IF(REBOUR.LT.TIMAX)GOTO101
*
100   CONTINUE
101   CONTINUE
*
      END
************************************************************************
      SUBROUTINE INIRUN(IRUN)
***********************************************************************
*
*     THIS SUBROUTINE ALLOWS THE USER TO DEFINE HIS OWN VALUES FOR
*         - THE Z0 PARAMETERS ( AMZ, GAMM, SW2 )
*         - THE FINAL STATE FLAVOR ( COL, QI, T3 )
*         - THE BEAM ENERGY ( EBEAM )
*         - THE INITIAL SEEDS OF RNDM
*
***********************************************************************
      COMMON /CONST/ ALFA,PI,ALFA1
      REAL*4          SOLD,FINEXP,POIDS,XK0
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI
      COMMON /BEAM/ S0,EBEAM
      COMMON /EVTS/ NEVT1,NEVT2,NFWD,NBKW
      REAL*8 SIG,SIG2,SECFWD,SECBKW,SCFWD2,SCBKW2
      COMMON /COUNTS/ SIG,SIG2,SECFWD,SECBKW,SCFWD2,SCBKW2
      DIMENSION ISEED(3)
*----
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        FIRST = .FALSE.
        WRITE(6,*)'*****************************************'
        WRITE(6,*)'*         WELCOME    TO    DYMU3        *'
        WRITE(6,*)'*                                       *'
        WRITE(6,*)'*         AUTHORS: J.E.CAMPAGNE         *'
        WRITE(6,*)'*                  R.ZITOUN             *'
        WRITE(6,*)'*                                       *'
        WRITE(6,*)'*         19 NOV  89 RELEASE            *'
        WRITE(6,*)'*                                       *'
        WRITE(6,*)'*****************************************'
        WRITE(6,*)' '
      ENDIF
*
*---- BEAM ENERGY
*
      EMIN = 91.0
      STEP =  1.
      ECMS = EMIN+STEP*(IRUN-1)
*
      EBEAM = ECMS/2
      S0 = 4.*EBEAM**2
*
*----  Z0 MASS IN GEV    Z0 WIDTH IN GEV    SINUS**2(THETA W)
*
      AMZ  = 91.0000
      GAMM =  2.4670
      SW2  =  0.2317
*
*----  COUPLING CONSTANTS
*
      COL = 1.
      T3 = -1./2.
      QI = -1.
      CA    = -1./4./SQRT(SW2*(1.-SW2))
      CV    = (1.-4.*SW2)*CA
      CVPRI = (-2*T3/QI+4.*SW2)/4./SQRT(SW2*(1.-SW2))
      CAPRI = -T3/QI/2./SQRT(SW2*(1.-SW2))
      CV2 = CVPRI*CV
      CA2 = CAPRI*CA
      CA2CV2 = ( CV**2+CA**2 )*( CVPRI**2+CAPRI**2 )
*
*----  LEPTON MASSES
*
      AEL   =    .511E-3
      AMU   = 105.659E-3
*
*---- CONST
*
      ALFA  = 1./137.036
      PI    = 3.14159265
      ALFA1 = ALFA/PI
*
      WRITE(6,*)'*************************************************'
      WRITE(6,*)'*     RUN PARAMETERS FOR RUN',IRUN
      WRITE(6,*)'******                                 **********'
      WRITE(6,1000) AMZ,GAMM,SW2
      WRITE(6,1003) ECMS,EBEAM
 1000   FORMAT('     Z MASS =',F8.3,' GEV ,      Z WIDTH =',F6.3,
     &         ' GEV ,  SIN2 TETA =',F7.4)
 1003   FORMAT(' CMS ENERGY =',F8.3,' GEV ,  BEAM ENERGY =',F8.3)
*
      IF(POIDS.EQ.1)THEN
        WRITE(6,*)'UNWEIGHTED EVENTS'
      ELSE
        WRITE(6,*)'WEIGHTED EVENTS'
      ENDIF
      WRITE(6,*)'INITIAL STATE EXPONENTIATION'
      IF(FINEXP.EQ.1)THEN
        WRITE(6,*)'FINAL STATE EXPONENTIATION'
      ELSE IF(FINEXP.EQ.-1) THEN
        WRITE(6,*)'NO FINAL STATE PHOTON'
      ENDIF
      IF(ID2.EQ.1)THEN
        WRITE(6,*)'DII IN D(X)'
      ELSE
        WRITE(6,*)'DII NOT IN D(X)'
      ENDIF
      IF(ID3.EQ.1)THEN
        WRITE(6,*)'DIII IN D(X)'
      ELSE
        WRITE(6,*)'DIII NOT IN D(X)'
      ENDIF
      IF(INTERF.EQ.0)THEN
        WRITE(6,*)'NO INTERFERENCE'
      ELSE
        WRITE(6,*)'INTERFERENCE WITH K0 =',XK0
      ENDIF
*
C     I1=-132028223
C     I2=-1412319451
C     CALL RD2IN(I1,I2)
      CALL RDMOUT(ISEED)
      WRITE(6,*)'INITIAL SEEDS ARE',I1,I2
      WRITE(6,*)'*************************************************'
*
*---- SET TO ZERO
*
      SIG    = 0.
      SIG2   = 0.
      SECFWD = 0.
      SECBKW = 0.
      SCFWD2 = 0.
      SCBKW2 = 0.
      NEVT1  = 0
      NEVT2  = 0
      NFWD   = 0
      NBKW   = 0
*----
      RETURN
      END
*|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      SUBROUTINE USER(NEVT,WEIGHT)
*
************************************************************************
*                                                                      *
*  USER ROUTINE CALLED WITH    NEVT =1  FOR INITIALISATION + 1ST EVENT *
*                                    0  AT END OF RUN                  *
*                                                                      *
*   IN NORMAL RUN, WEIGHT IS EQUAL TO 1.                               *
*                                                                      *
************************************************************************
      PARAMETER (RADDEG = 180./3.141592653)
      PARAMETER (NEMIN=3,NZET=11)
      DIMENSION ECUT(NEMIN),ZETCUT(NZET)
      DATA ECUT /0., 25., 40./
      DATA ZETCUT /2.,3.,6.,8.,10.,12.,15.,20.,30.,40.,50./
      LOGICAL EOK(NEMIN),ZETOK(NZET)
      DIMENSION FORWD(NEMIN,NZET), BACKWD(NEMIN,NZET)
      COMMON / VECLAB / PFPOS(4),PFNEG(4),GAMPOS(4),GAMNEG(4),GAMFIN(4)
      COMMON /BEAM/ S0,EBEAM
      COMMON/TAU/TAU,CPTAU,HEL,PITAU(4)
      COMMON /RESULT/ SIGBOR,SIGTOT,ERRSIG,ASYTOT,ERRASY
      REAL*4          SOLD,FINEXP,POIDS,XK0
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0
      IF(NEVT.EQ.0)THEN
        CALL HOPERA(20,'/',10,30,1.,1.)
        CALL HPRINT(0)
        CALL HSTORE(0,10)
         CALL HDELET(0)
        RETURN
      ENDIF
*
* --- START OF RUN
*
      IF(NEVT.EQ.1)THEN
        CALL HBOOK1(10,'Energie Gam qcq',50.,0.,50.,0.)
        CALL HBOOK1(20,'Energie Gam fin',50.,0.,50.,0.)
         CALL HIDOPT(0,'BLAC')
         CALL HMINIM(1,0.)
        SUM = 0.
      ENDIF
*
* --- EVENT
*
      SUM = SUM + 1.
      IF(GAMPOS(4).gt.0.)CALL HFILL(10,GAMPOS(4),0.,1.)
      IF(GAMNEG(4).gt.0.)CALL HFILL(10,GAMNEG(4),0.,1.)
      IF(GAMFIN(4).gt.0.)CALL HFILL(10,GAMFIN(4),0.,1.)
      IF(GAMFIN(4).gt.0.)CALL HFILL(20,GAMFIN(4),0.,1.)
      RETURN
      END
*|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      SUBROUTINE FINISH(IRUN,NEVT)
      COMMON /BEAM/ S0,EBEAM
      REAL*8 SIG,SIG2,SECFWD,SECBKW,SCFWD2,SCBKW2
      COMMON /COUNTS/ SIG,SIG2,SECFWD,SECBKW,SCFWD2,SCBKW2
      COMMON /EVTS/ NEVT1,NEVT2,NFWD,NBKW
      REAL*4          SOLD,FINEXP,POIDS,XK0
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI
      COMMON /RESULT/ SIGBOR,SIGTOT,ERRSIG,ASYTOT,ERRASY
      COMMON/NORMAL/DXSXIN
      DIMENSION ISEED(3)
*
      DATA HBARC2 / 3.8938771E+5 /
C     DATA COEFF / 2.23058749E-4 /
      WRITE(6,*)'------------THE END OF RUN',IRUN,'-------------------'
      WRITE(6,10)EBEAM,2*EBEAM
      WRITE(6,11) NEVT,NEVT1,NEVT2
10    FORMAT(' BEAM ENERGY',F10.3,' SQRT(S)',F10.3)
11    FORMAT(I8,' GENERATED EVENTS.',I8,' WERE NECESSARY.',
     &       ' CROSS SECTION COMPUTED FROM',I8,' EVENTS')
*
      COEFF = 8./3./3.141593*(1.16637E-5*AMZ**2*SW2*(1-SW2))**2
      COEFF = COEFF * HBARC2
      SIGBOR = COEFF * COL * QI**2 * W1(S0)/S0
      ASYBOR = 3./8. * W2(S0)/W1(S0)
      WRITE(6,13)
13    FORMAT(/,15X,'   CROSS SECTION     ASYMMETRY')
      WRITE(6,12) 'BORN',SIGBOR,ASYBOR
12    FORMAT(3X,A10,F10.4,' NANOBARN', 2X,F10.4)
*
      COMP   = SECFWD + SECBKW
      COMP2  = SCFWD2 + SCBKW2
*
      SIGTOT = COEFF * DXSXIN**2 *SIG/NEVT1/S0 * COMP/NEVT2
      P1     = COMP2/COMP/COMP - 1./NEVT2
      P2     = SIG2/SIG/SIG - 1./NEVT1
      ERRSIG = SIGTOT*SQRT(P1+P2)
*
      ASYTOT = (SECFWD-SECBKW)/(SECFWD+SECBKW)
      ERRASY = SQRT( (1.-ASYTOT**2)/FLOAT(NFWD+NBKW) )
      WRITE(6,12) 'DYMU3',SIGTOT,ASYTOT
      WRITE(6,12) 'ERRORS',ERRSIG,ERRASY
*
      WRITE(6,*)
      CALL RDMOUT(ISEED)
      WRITE(6,*)'THE FINAL SEEDS ARE',ISEED
      WRITE(6,*)'***************************************************'
      RETURN
      END
*|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      BLOCK DATA
***********************************************************************
*     Here are defined DYMU3 control cards                            *
*                                                                     *
*     ID2 = 1/0 WITH(OUT) FII  TERM  IN D(X)                          *
*     ID3 = 1/0 WITH(OUT) FIII TERM IN D(X)                           *
*     OLD IEXPO OF DYMU2 IS SET TO 1 NOW (EXPONENTIATED FORM OF D(X)) *
*     FINEXP = 1./-1. FINAL PHOTON EXPONENTIATED / NONE               *
*     POIDS = 1./0  UN/WEIGHTED EVENT PRODUCED                        *
*     TAU = 1./0 POLARISED TAU DECAY INTO PION NEUTRINO IS ON/OFF,    *
*                IN THE FINAL STATE ONLY THE 4-MOMENTA OF THE PION.   *
***********************************************************************
      REAL*4          SOLD,FINEXP,POIDS,XK0
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0
      COMMON/TAU/TAU,CPTAU,HEL,PITAU(4)
      DATA ID2/0/,ID3/0/,FINEXP/1./,POIDS/1./
      DATA TAU/0./
      DATA XK0 /.003 /,INTERF/0/,SOLD/-1./
      END
***************************************************************
      SUBROUTINE DYMUS(WEIGHT)
*
*     STEERING ROUTINE TO PRODUCE AN EVENT
*
      REAL*8 SIG,SIG2,SECFWD,SECBKW,SCFWD2,SCBKW2
      COMMON /COUNTS/ SIG,SIG2,SECFWD,SECBKW,SCFWD2,SCBKW2
      COMMON /EVTS/ NEVT1,NEVT2,NFWD,NBKW
      COMMON / VECLAB / PFPOS(4),PFNEG(4),GAMPOS(4),GAMNEG(4),GAMFIN(4)
      COMMON /ZDUMP/ IDUMP,NEVT
      REAL*4          SOLD,FINEXP,POIDS,XK0
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI
      COMMON/TAU/TAU,CPTAU,HEL,PITAU(4)
      COMMON /BEAM/ S0,EBEAM
      COMMON /CONST/ ALFA,PI,ALFA1
      DATA S0OLD/-1./, AMZOLD/-1./,GAMOLD/-1./,SW2OLD/-1./
      IF( S0.NE.S0OLD   .OR.
     +   AMZ.NE.AMZOLD  .OR.
     +  GAMM.NE.GAMOLD  .OR.
     +   SW2.NE.SW2OLD      )THEN
         S0OLD  = S0
         AMZOLD = AMZ
         GAMOLD = GAMM
         SW2OLD = SW2
        IF(2*EBEAM.LT.AMZ) THEN
          W1MAX = W1(S0)
        ELSE
          ZZ1 = CA2CV2/CV2
          ZZ0 = SQRT(ZZ1**2+4.*(GAMM/AMZ)**2)
          ZZP = .5*(-ZZ1+ZZ0)
          SSP = AMZ**2/(1.-ZZP)
          W1MAX = W1(SSP)
        ENDIF
        IF(FINEXP.EQ.1) W1MAX = W1MAX*1.005
        W1MAX = W1MAX * COL * QI**2
        XMUMU = 4*AMU**2
      ENDIF
*
* --- FIRST DETERMINE INITIAL STATE PHOTONS
*
 100  CALL DFUNC(X1,C1)
      CALL DFUNC(X2,C2)
      SXX = X1*X2*S0
      IF(SXX.LT.XMUMU)THEN
        STOTAP = 0.
      ELSE
        STOTAP = W1(SXX)
        IF(FINEXP.EQ.1)THEN
          AMU2 = XMUMU/SXX
          XMAX = 1.- AMU2
          ALFA1F = ALFA1*QI**2
          BETF = -2*ALFA1F*(LOG(AMU2/4)+1.)
          XI1 = -XMAX+XMAX*XMAX/4.
          XI2 = -2.*DILOG(XMAX) + AMU2*(3.-XMAX)/2.*LOG(AMU2)
     @          - AMU2*(5.-XMAX)/4 + 5./4
          STOTAP = STOTAP* ( (1.+ALFA1F*2.789868+.75*BETF)*XMAX**BETF
     &                       + BETF*XI1 + ALFA1F*XI2 )
        ENDIF
      ENDIF
      STOTAP = STOTAP * COL * QI**2
*---- COUNTER FOR X-SECTION
      NEVT1 = NEVT1 + 1
      SIG   = SIG   + STOTAP
      SIG2  = SIG2  + STOTAP**2
*
*----- PROCEDURE DE REJECTION
*
      IF(STOTAP.GT.W1MAX)THEN
        WRITE(6,*)'WARNING: W1MAX TOO SMALL'
        WRITE(6,*)'PB! SXX,STOTAP,W1MAX',SXX,STOTAP,W1MAX
      ELSE
        IF(W1MAX*RNDM(SIG).GT.STOTAP)THEN
           IF(IDUMP.GE.1) WRITE(6,*)'EVENT REJECTED AFTER HIT AND MISS'
           GOTO 100
        ENDIF
      ENDIF
      IF(IDUMP.GE.1) WRITE(6,*)'EVENT KEPT AFTER HIT AND MISS'
*----
      CALL GETGAM(X1,X2,C1,C2,SPRIME)
      IF(SPRIME.LT.XMUMU)  GOTO 100
      IF(IDUMP.GE.1) WRITE(6,*)' INITIAL PHOTON K1,COST1 = ',1-X1,C1
      IF(IDUMP.GE.1) WRITE(6,*)' INITIAL PHOTON K2,COST2 = ',1-X2,C2
      IF(IDUMP.GE.1) WRITE(6,*)' SPRIME',SPRIME
*
* --- GENERATE EVENT IN THE MOVING E+E- CMS
*
      CALL INISPR(SPRIME)
      CALL KJB(SPRIME)
*
* --- COMPUTE APPROXIMATE CROSS-SECTION
*
      IF(TAU.NE.0.) THEN
        CALL TAUPOL(IBAD)
        IF(IBAD.EQ.1) GOTO100
      ENDIF
      NEVT2 = NEVT2 + 1
*
* --- GO BACK TO OVERALL CMS
*
      CALL LABO(SPRIME)
*
* --- INTERFERENCE TERMS
*
      W = 1.
      IOK = 1
      IF(INTERF .EQ. 1) THEN
        ISOF = 0
        CALL WGHT(W,IOK,ISOF)
        IF(IDUMP.GE.1)
     &     WRITE(6,*)'W,IOK,ISOF= ',W,IOK,ISOF
      ENDIF
      IF(TAU.NE.0.)CALL TAUDEC
* --- COUNTS FOR ASYMMETRY
      IF(PFPOS(3).GT.0.)THEN
        SECFWD  = SECFWD  + W
        SCFWD2  = SCFWD2  + W**2
        NFWD    = NFWD    + 1
      ELSE
        SECBKW  = SECBKW  + W
        SCBKW2  = SCBKW2  + W**2
        NBKW    = NBKW    + 1
      ENDIF
*
* --- KEEP (OR NOT) EVENTS WITH WEIGHT 1
*
      IF(POIDS.EQ.1)THEN
        IF(IOK.NE.1)              GOTO100
        WEIGHT = 1.
      ELSE
        WEIGHT = W
      ENDIF
      IF(IDUMP.GE.1) THEN
        WRITE(6,*)'WEIGHT OF THIS EVENT=',WEIGHT
        WRITE(*,'(1X,A10,4F10.4)') 'F  +',PFPOS
        WRITE(*,'(1X,A10,4F10.4)') 'FB -',PFNEG
        WRITE(*,'(1X,A10,4F10.4)') ' G +',GAMPOS
        WRITE(*,'(1X,A10,4F10.4)') ' G -',GAMNEG
        WRITE(*,'(1X,A10,4F10.4)') ' G F',GAMFIN
      ENDIF
      RETURN
      ENTRY DYMUSI
        IF(2*EBEAM.LT.AMZ) THEN
          W1MAX = W1(S0)
        ELSE
          ZZ1 = CA2CV2/CV2
          ZZ0 = SQRT(ZZ1**2+4.*(GAMM/AMZ)**2)
          ZZP = .5*(-ZZ1+ZZ0)
          SSP = AMZ**2/(1.-ZZP)
          W1MAX = W1(SSP)
        ENDIF
        IF(FINEXP.EQ.1) W1MAX = W1MAX*1.005
        W1MAX = W1MAX * COL * QI**2
        XMUMU = 4*AMU**2
      RETURN
      END
*|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      SUBROUTINE GETGAM(X1,X2,C1,C2,XM)
      COMMON /BEAM/ S0,EBEAM
      COMMON /VECLAB/ QMPL(4),QMML(4),QKEPL(4),QKEML(4),QKL(4)
      DIMENSION PCDM(4)
      PARAMETER ( PI = 3.14159265, EMIN = .010)
*
* --- PHOTON EMITED IN LAB-FRAME ON E+ LINE
*
      CALL VZERO(QKEPL,4)
      IF(1-X1.GT.EMIN/EBEAM)THEN
        QKEPL(4) = EBEAM * (1-X1)
        IF(ABS(C1).LT.0.99999) THEN
          PTKEP = QKEPL(4) * SQRT(1-C1**2)
        ELSE
          PTKEP = 0
        ENDIF
        PHIP     = 2.*PI*RNDM(PHIP)
        QKEPL(1) = PTKEP*COS(PHIP)
        QKEPL(2) = PTKEP*SIN(PHIP)
        QKEPL(3) = QKEPL(4)*C1
      ENDIF
*
* --- PHOTON EMITED IN LAB-FRAME ON E- LINE
*
      CALL VZERO(QKEML,4)
      IF(1.-X2.GT.EMIN/EBEAM)THEN
        QKEML(4) = EBEAM * (1-X2)
        IF(ABS(C2).LT.0.99999) THEN
          PTKEM = QKEML(4) * SQRT(1-C2**2)
        ELSE
          PTKEM = 0
        ENDIF
        PHIM     = 2.*PI*RNDM(PHIM)
        QKEML(1) = PTKEM*COS(PHIM)
        QKEML(2) = PTKEM*SIN(PHIM)
        QKEML(3) = - QKEML(4)*C2
      ENDIF
*
* --- PCDM
*
      PCDM(1) = -(QKEPL(1)+QKEML(1))
      PCDM(2) = -(QKEPL(2)+QKEML(2))
      PCDM(3) = -(QKEPL(3)+QKEML(3))
      PCDM(4) = -(QKEPL(4)+QKEML(4)) + 2.*EBEAM
*
* --- INVARIANTE MASS OF E+E- PAIR
*
      XM      = PCDM(4)**2-PCDM(1)**2-PCDM(2)**2-PCDM(3)**2
      RETURN
      END
*|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      SUBROUTINE INISPR(S)
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI
      COMMON /WEAK3/ ZETA,GM,B0,AMU2,ALGMU,BETF,ALFA1F,W1,W2
      COMMON /CONST/ ALFA,PI,ALFA1
      REAL*4          SOLD,FINEXP,POIDS,XK0
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0
*
*---  INITIALISATION OF THE COMMON WEAK3
*
      ZETA  = 1. - (AMZ**2)/S
C     GM    = AMZ*GAMM/S
      GM    = GAMM/AMZ
      B0    = ZETA**2 + GM**2
      AMU2  = 4.*AMU**2/S
      ALGMU = LOG(S/AMU**2)
      ALFA1F= ALFA1*QI**2
      BETF  = 2.*ALFA1F*(ALGMU-1.)
      W1    = 1. + (2.*ZETA*CV2+CA2CV2)/B0
      W2    = 4.*(ZETA*CA2+2.*CA2*CV2)/B0
      RETURN
      END
*|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      SUBROUTINE KJB(S)
      REAL*4          SOLD,FINEXP,POIDS,XK0
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0
      COMMON /ZDUMP/ IDUMP,NEVT
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI
      COMMON /WEAK3/ ZETA,GM,B0,AMU2,ALGMU,BETF,ALFA1F,W1,W2
      COMMON /CONST/ ALFA,PI,ALFA1
      COMMON /BEAM/ S0,EBEAM
      COMMON/TAU/TAU,CPTAU,HEL,PITAU(4)
*
* --- GENERATE FINAL PHOTON ENERGY
*
      XK = 0.
      IF(FINEXP.EQ.1) XK = GFIN(S)
      IF( XK.LT..01/EBEAM) GO TO 700
*
* --- IF EGAMMA > 10 MEV  GENERATE GAMMA
*
      IF(IDUMP.GE.1)WRITE(6,*)' FINAL HARD PHOTON. XK=',XK
      S1 = S*(1.-XK)
      IT = 1
      XD = AMU2/2./(1.-XK)
  11  CALL CHOIX(XD,DPOS,DNEG)
      DELT  = 1. - AMU2/2./(1.+(1.-XK)**2)*(DPOS/DNEG+DNEG/DPOS)
      IF(RNDM(DELT).GT.DELT) GOTO 11
      XKIP = (1.-XK/2.*DPOS)**2
      XKI  = XKIP + (1.-XK/2.*DNEG)**2
      XR = 2.*RNDM(XKI)
      IF(XKI.LT.XR) GOTO 11
      IOHM = 0
      IF(XKIP.LT.XR) IOHM = 1
      CG = (1.-DPOS)*(1.-AMU2/2.)
      SG = SQRT((DPOS-XD)*(DNEG-XD))
      GOTO 701
*
* --- IF EGAMMA < 10 MEV DON'T GENERATE IT
*
 700  IT = 2
      IF(IDUMP.GE.1)WRITE(6,*)'FINAL SOFT PHOTON. XK=',XK
 701  CALL OMEGA(CP,PHIP)
      IF(TAU.NE.0.) CPTAU = CP
      CALL PVECT(IT,IOHM,XK,CG,SG,CP,PHIP)
      RETURN
      END
*|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      SUBROUTINE CHOIX(XD,DP,DM)
      R  = RNDM(R)
      IF(XD.GT.2.)XD = 1.9999
      DP = (2.-XD)*(XD/(2.-XD))**R
      DM = 2. - DP
      XR = RNDM(XR)
      IF(XR.GT..5) THEN
        DMM=DP
        DP=DM
        DM=DMM
      ENDIF
      RETURN
      END
*|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      SUBROUTINE OMEGA(CP,PHIP)
      COMMON /WEAK3/ ZETA,GM,B0,AMU2,ALGMU,BETF,ALFA1F,W1,W2
      COMMON /CONST/ ALFA,PI,ALFA1
      PPLUS = (1.+W2/2./W1)/2.
      XR    = RNDM(XR)
      IF(XR.LT.PPLUS) THEN
        CP = 2.*(RNDM(CP)**(1./3.)) - 1.
      ELSE
        CP = 1. - 2.*(RNDM(CP)**(1./3.))
      ENDIF
      PHIP = 2.*PI*RNDM(PHIP)
      RETURN
      END
      SUBROUTINE PVECT(IT,IOHM,XK,CG,SG,CP,PHIP)
      COMMON /WEAK3/ ZETA,GM,B0,AMU2,ALGMU,BETF,ALFA1F,W1,W2
      REAL *8 QK,QMP,QMM
      COMMON /VECT/ QK(4),QMP(4),QMM(4)
* --- IN UNIT OF SQRT(S)/2 AND IN THE CDM FRAME
      SP = SQRT(1.0-CP*CP)
      IF(IT.NE.2)THEN
*
* --- FINAL STATE HARD
*
        IF(IOHM.EQ.1) THEN
          CG = -CG
          CP = -CP
          SP = -SP
        ENDIF
*
* --- MU+
*
        QMP(4) = (2.-XK-XK*CG)/2.
        QMP4   = QMP(4)
        HMASS  = SQRT(1.0 - AMU2/QMP4**2)
        QMP(1) = 0.
        QMP(2) = SP*QMP4*HMASS
        QMP(3) = CP*QMP4*HMASS
*
* --- PHOTON
*
C       CGP = ((2.-XK)*CG-XK)/(2.-XK-XK*CG)/HMASS
        CGP = ((2./(1D0-AMU2/2D0)-XK)*CG-XK)/(2.-XK-XK*CG)/HMASS
        IF (CGP**2.GT.1.) THEN
C          WRITE(6,'('' ++XK,CG,HMASS,AMU2 :'',4F13.5)')XK,CG,HMASS,AMU2
           CGP1= SIGN(1.,CGP)
           WRITE(6,'('' +++ PVECT WARNING:CGP WAS '',E13.6,'' SET TO ''
     $  ,E13.6)') CGP,CGP1
           CGP = CGP1
        ENDIF
        SGP = SQRT(1.0-CGP**2)
        QK(4) = XK
        QK(1) = XK*SGP*SIN(PHIP)
        QK(2) = XK*(CGP*SP+SGP*COS(PHIP)*CP)
        QK(3) = XK*(CGP*CP-SGP*COS(PHIP)*SP)
*
* --- MU-
*
  150   QMM(4) = 2.-XK-QMP(4)
        DO 200 I=1,3
  200     QMM(I) = -(QMP(I)+QK(I))
*
* --- CP REFLEXION
*
        IF(IOHM.NE.0) THEN
          DO 225 I=1,4
            QII    = QMM(I)
            QMM(I) = QMP(I)
  225       QMP(I) = QII
        ENDIF
      ELSE
*
* --- SOFT EVENT
*
        QMP(4) = 1.
        HMASS  = SQRT(1D0-AMU2)
        QMP(1) = SP*COS(PHIP)*HMASS
        QMP(2) = SP*SIN(PHIP)*HMASS
        QMP(3) = CP*HMASS
* --- MU-/PHOTON
        QMM(4) = 1.
        QK(4)  = 0.
*
        DO 410 I=1,3
          QMM(I) = -QMP(I)
 410      QK(I)  = 0.
      ENDIF
*---
      RETURN
      END
*|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      SUBROUTINE LABO(S)
      COMMON /ZDUMP/ IDUMP,NEVT
      COMMON /BEAM/ S0,EBEAM
      COMMON /VECLAB/ QMPL(4),QMML(4),QKEPL(4),QKEML(4),QKL(4)
      REAL*8 DQK,DQMP,DQMM,DPCDM(4)
      COMMON /VECT/ DQK(4),DQMP(4),DQMM(4)
      REAL EPL(4),EPK(4),PCDM(4)
      REAL*8 DEPK(4),EE,CTETA,STETA,CPHI,SPHI,CFI,SFI
      PARAMETER ( PI = 3.14159265 )
*---------------------------------------------------------------*
*     THIS ROUTINE GIVES THE 4-MOMENTA IN THE LAB-FRAME IN THE  *
*     COMMON VECLAB.                                            *
*---------------------------------------------------------------*
*
* --- CHANGE SCALE
      EBK = SQRT(S)/2.
      DO 10 I=1,4
      DQMP(I)  = EBK*DQMP(I)
      DQMM(I)  = EBK*DQMM(I)
      DQK(I)   = EBK*DQK(I)
      PCDM(I) = -(QKEPL(I)+QKEML(I))
 10   DPCDM(I) = PCDM(I)
      PCDM(4) = PCDM(4) + 2*EBEAM
      DPCDM(4) = PCDM(4)
*
* --- ROTATION OF QK,QMP,QMM  AROUND Z SYMMETRY AXIS IN THE CDM
*
      FI  = 2.*PI*RNDM(FI)
      CFI = COS(FI)
      SFI = SIN(FI)
      CALL ROTZ(DQMP,CFI,SFI)
      CALL ROTZ(DQMM,CFI,SFI)
      CALL ROTZ(DQK,CFI,SFI)
*
* --- CONTROL THE USEFULLNESS OF THE TRANSFORMATION
*
      AMASS = DPCDM(4)**2-DPCDM(1)**2-DPCDM(2)**2-DPCDM(3)**2
      PNORM = DPCDM(1)**2+DPCDM(2)**2+DPCDM(3)**2
      IF((AMASS.LE.0.).OR.(PNORM.EQ.0.))THEN
        DO 500 I=1,4
          QMPL(I) = DQMP(I)
          QMML(I) = DQMM(I)
 500      QKL(I)  = DQK(I)
        RETURN
      ENDIF
*
* --- FIND THE LAB Z AXIS IN THE CMS
*
      EPL(1) =    -QKEPL(1)
      EPL(2) =    -QKEPL(2)
      EPL(3) = EBEAM-QKEPL(3)
      EPL(4) = EBEAM-QKEPL(4)
      CALL LOREN4(PCDM,EPL,EPK)
*
* --- ROTATION OF 3 MOMENTA BEFORE BOOST
* --- DEFINITION OF THE ANGLES
*
      DO 20 I=1,4
  20  DEPK(I) = EPK(I)
      EE    = SQRT(DEPK(1)**2+DEPK(2)**2+DEPK(3)**2)
      CTETA = DEPK(3)/EE
      IF ( ABS(CTETA).GT.1. ) THEN
        CTETA = 1.
        STETA = 0.
      ELSE
        STETA = SQRT(1.-CTETA**2)
      ENDIF
*
* ----CONTROL THE USEFULLNESS OF THE ROTATION
*
      IF(STETA.LT.0.000001)GOTO 40
*
      CPHI = DEPK(1)/EE/STETA
      SPHI = DEPK(2)/EE/STETA
*
* --- ROTATION
*
      CALL DROT(DQK,CTETA,STETA,CPHI,SPHI)
      CALL DROT(DQMP,CTETA,STETA,CPHI,SPHI)
      CALL DROT(DQMM,CTETA,STETA,CPHI,SPHI)
*
* --- BOOST
*
  40  CONTINUE
      IF(IDUMP.NE.0) THEN
        PRINT*,'PHOTON BEFORE BOOST',DQK
        PRINT*,'FERM   BEFORE BOOST',DQMP
        PRINT*,'ANTIF  BEFORE BOOST',DQMM
        PRINT*,'BOOST',DPCDM
      ENDIF
      CALL DLAURE(DPCDM,DQMP,QMPL)
      CALL DLAURE(DPCDM,DQMM,QMML)
      CALL DLAURE(DPCDM,DQK ,QKL )
*---
      RETURN
      END
**********************************************************************
      SUBROUTINE DROT(X,CT,ST,CP,SP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL *8 X(3)
      COEFF1 = -X(1)*SP + X(2)*CP
      COEFF2 = (X(1)*CP + X(2)*SP)*CT + X(3)*ST
      COEFF3 =  X(1)*CP + X(2)*SP
      X(1)   = -SP*COEFF1 + CP*COEFF2
      X(2)   =  CP*COEFF1 + SP*COEFF2
      X(3)   = -COEFF3*ST + X(3)*CT
      RETURN
      END
*---------------------------------------------------------------
      SUBROUTINE ROTZ(X,CF,SF)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL*8 X(2)
      Y1   = CF*X(1) - SF*X(2)
      X(2) = SF*X(1) + CF*X(2)
      X(1) = Y1
      RETURN
      END
*-------------------------------------------------------------------
      SUBROUTINE DLAURE(P0,P1,P2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL *4  P2(4)
      DIMENSION P0(4),P1(4)
      XM  = SQRT(P0(4)**2-P0(1)**2-P0(2)**2-P0(3)**2)
      P24 = P0(1)*P1(1)+P0(2)*P1(2)+P0(3)*P1(3)+P0(4)*P1(4)
      P24 = P24/XM
      T   = (P1(4)+P24)/(P0(4)+XM)
      DO 1 I=1,3
1     P2(I) = P1(I) + T*P0(I)
      P2(4) = P24
      RETURN
      END
**********************************************************************
      SUBROUTINE TAUPOL(IBAD)
      COMMON /ZDUMP/ IDUMP,NEVT
      COMMON/TAU/TAU,CPTAU,HEL,PITAU(4)
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI
      COMMON /WEAK3/ ZETA,GM,B0,AMU2,ALGMU,BETF,ALFA1F,W1,W2
*
* --- COMPUTE F4
      F4 = 2*SQRT(CA2*CV2)*(ZETA+CA2CV2)/B0
      IF(SW2.GT.0.25)F4 = -F4
* --- CHOOSE HELICITY
      HEL = 1.
      IF(F4/W1.LT. (1.-2*RNDM(F4)) ) HEL = -1.
      IF(IDUMP.NE.0)PRINT*,'F4,W1,HEL',F4,W1,HEL
* --- correct angular distributions
      AMAX = 1. - 4*HEL*F4/(2*W1+W2)
      IF(AMAX.LT.1.)AMAX = 1.
      W = 1. - HEL*F4 * (1+CPTAU)**2 / ( W1*(1+CPTAU**2)+W2*CPTAU )
      IF(AMAX.LT.W)PRINT*,'WEIGHT GT AMAX'
      IF(IDUMP.GE.1)PRINT*,'W,AMAX',W,AMAX
      IF( RNDM(W) .GT. W/AMAX) THEN
        IF(IDUMP.GE.1)PRINT*,'TAU REJECTED'
        IBAD = 1
      ELSE
        IBAD = 0
      ENDIF
      RETURN
      END
**********************************************************************
      SUBROUTINE TAUDEC
      COMMON /ZDUMP/ IDUMP,NEVT
      COMMON / VECLAB / PFPOS(4),PFNEG(4),GAMPOS(4),GAMNEG(4),GAMFIN(4)
      COMMON/TAU/TAU,CPTAU,HEL,PITAU(4)
      COMMON /CONST/ ALFA,PI,ALFA1
      PARAMETER ( TAUMAS = 1.7841 , PIMOM = .887 , PIMAS = 0.1396 )
*
* --- chose pion angles
      CT = ( 1. - 2*SQRT(RNDM(CT)) ) * HEL
      PHI = 2*PI*RNDM(PHI)
* --- GENERATE PION IN TAU REST FRAME
      PITAU(4) = SQRT(PIMAS**2+PIMOM**2)
      PITAU(3) = PIMOM * CT
      PITAU(2) = PIMOM * SQRT(1. - CT**2)
      PITAU(1) = PITAU(2) * COS(PHI)
      PITAU(2) = PITAU(2) * SIN(PHI)
      IF(IDUMP.NE.0)PRINT*,'PITAU      ',PITAU
* --- ROTATE PITAU TO SYSTEM PARALLEL TO LAB SYSTEM
      PTAU = SQRT(PFPOS(1)**2+PFPOS(2)**2+PFPOS(3)**2)
      CTAU = PFPOS(3)/PTAU
      PHITAU = ATAN2(PFPOS(2),PFPOS(1))
      CALL ROT(PITAU,CTAU,PHITAU)
      IF(IDUMP.NE.0)PRINT*,'PITAU ROT  ',PITAU
* --- BOOST TO LAB SYSTEM. BUT BEFORE MAKE E**2-P**2 = M**2 FOR THE TAU
      PFOLD = PFPOS(4)
      PFPOS(4) = SQRT(PTAU**2+TAUMAS**2)
      CALL LAURE(PFPOS,PITAU,PITAU)
      PFPOS(4) = PFOLD
      IF(IDUMP.NE.0)PRINT*,'PITAU BOOST',PITAU
      RETURN
      END
*|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      SUBROUTINE ROT(X,CT,PHI)
      REAL X(3)
      ST = SQRT(1-CT**2)
      CP = COS(PHI)
      SP = SIN(PHI)
      COEFF1 = -X(1)*SP + X(2)*CP
      COEFF2 = (X(1)*CP + X(2)*SP)*CT + X(3)*ST
      COEFF3 =  X(1)*CP + X(2)*SP
      X(1)   = -SP*COEFF1 + CP*COEFF2
      X(2)   =  CP*COEFF1 + SP*COEFF2
      X(3)   = -COEFF3*ST + X(3)*CT
      RETURN
      END
*-------------------------------------------------------------------
      SUBROUTINE LAURE(P0,P1,P2)
      REAL P0(4),P1(4),P2(4)
      XM  = SQRT(P0(4)**2-P0(1)**2-P0(2)**2-P0(3)**2)
      P24 = P0(1)*P1(1)+P0(2)*P1(2)+P0(3)*P1(3)+P0(4)*P1(4)
      P24 = P24/XM
      T   = (P1(4)+P24)/(P0(4)+XM)
      DO 1 I=1,3
1     P2(I) = P1(I) + T*P0(I)
      P2(4) = P24
      RETURN
      END
**********************************************************************
      SUBROUTINE WGHT(W,IOK,ISOF)
      RETURN
      END
*|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      FUNCTION W1(S)
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI
      DATA SOLD/-1./
      IF(SOLD.NE.S)THEN
        SOLD=S
        ZETA = 1. - (AMZ**2)/S
C       GM   = AMZ*GAMM/S
        GM   = GAMM/AMZ
        B0   = ZETA**2 + GM**2
      ENDIF
      W1 = 1.+(2.*ZETA*CV2+CA2CV2)/B0
      RETURN
      END
*----------------------------------------------------------------
      FUNCTION W2(S)
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI
      DATA SOLD/-1./
      IF(SOLD.NE.S)THEN
        SOLD=S
        ZETA = 1. - (AMZ**2)/S
C       GM   = AMZ*GAMM/S
        GM   = GAMM/AMZ
        B0   = ZETA**2 + GM**2
      ENDIF
      W2 = 4.*(ZETA*CA2+2.*CA2*CV2)/B0
      RETURN
      END
***********************************************************************
      FUNCTION GFIN(SPRIME)
      COMMON /CONST/ ALFA,PI,ALFA1
      COMMON /WEAK3/ ZETA,GM,B0,AMU2,ALGMU,BETF,ALFA1F,W1,W2
      COMMON /ZDUMP/ IDUMP,NEVT
1     ALEA = RNDM(GFIN)
      U = 1./BETF*LOG(ALEA)
      IF(ABS(U).LT.50.)THEN
        GFIN = EXP(U)
      ELSE
        GFIN = 0.
      ENDIF
      Y = 1-GFIN
      IF(Y.LT.AMU2)GOTO1
      IF(IDUMP.GE.1)WRITE(6,*)'FINAL GAMMA ENERGY',GFIN,Y
      IF(GFIN.LT.1E-5)THEN
        WEIGHT = 1.
      ELSE
        WEIGHT = 1. - (1+Y)*GFIN/ALEA*(BETF/2.+ALFA1F*LOG(Y))/
     &           BETF/(1+ALFA1F*2.789868+.75*BETF)
      ENDIF
      IF(IDUMP.GE.1)WRITE(6,*)'WEIGHT',WEIGHT
      IF(RNDM(WEIGHT).GT.WEIGHT)THEN
        IF(IDUMP.GE.1)WRITE(6,*)'THROWN OUT'
        GOTO1
      ENDIF
2     CONTINUE
      RETURN
      END
***********************************************************************
      SUBROUTINE DFUNC(X,COST)
*
* --- ROUTINE TO GENERATE ENERGY AND ANGLE OF AN INITIAL STATE PHOTON
C BBL oct 2000 change rndm(ibin) to rndm(yibin) argtype real for linux
*
      REAL*4        S,EBEAM
      COMMON /BEAM/ S,EBEAM
      PARAMETER ( NBIN = 100, X0 = .99999 )
      COMMON /BINX/ TOT,XBIN(0:NBIN)
      REAL*4          BET,BET2,ALFBET,GRIBLI,A0,A,ZETA2
      COMMON /PARAME/ BET,BET2,ALFBET,GRIBLI,A0,A,ZETA2
      REAL*4          SOLD,FINEXP,POIDS,XK0
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI
      COMMON/NORMAL/DXSXIN
      PARAMETER (EPSILO = 1.E-10)
*
* --- INITIALISE BINNING FOR RANDOM GENERATION
*
      IF(SOLD.NE.S)THEN
        ZMIN = 4*AMU**2/S
        CALL INIBIN(ZMIN,DXSXIN)
        XME = (.511E-3)**2/S
      ENDIF
*
* --- GENERATE X ACCORDING TO D(X)
*
10    X = X0
      IF(DXSXIN*RNDM(X).LT.TOT)THEN
        IBIN = NBIN * RNDM(YIBIN)   !   here bbl oct 2000
        X = XBIN(IBIN) + RNDM(XIBIN) * ( XBIN(IBIN+1)-XBIN(IBIN) )
      ENDIF
*
* --- GENERATE THETA ACCORDING TO 1ST ORDER FORMULA
*
11    XGAM = 1 - X
1     COEF = XME**(2.*RNDM(XEL)-1.)
      COST = (COEF -1.)/(COEF + 1.)
      IF(COST.GT.1.)COST=1.
      IF(COST.LT.-1.)COST=-1.
      DPLUS  = 1 + COST + 2*XME*COST
      DMINUS = 1 - COST - 2*XME*COST
      IF ( ABS(DPLUS ).LT.EPSILO ) DPLUS  = EPSILO
      IF ( ABS(DMINUS).LT.EPSILO ) DMINUS = EPSILO
*---
*     CALCULATION OF 1ST RESIDUAL TERM
*---
      U = (1 - XGAM*DMINUS)**2
      IF(RNDM(COEF).GT.U)GOTO 1
*---
*     CALCULATION OF 2ND RESIDUAL TERM
*---
      WEIGHT = 1 - 2*XME*X/(1+X**2)*(DPLUS/DMINUS+DMINUS/DPLUS)
      IF(WEIGHT.LT.RNDM(WEIGHT))GOTO 1
*---
      RETURN
      END
***********************************************************************
      SUBROUTINE INIPAR
*
*     DEFINE PHYSICAL CONSTANTS FOR INITIAL STATE PHOTON GENERATION
*
      REAL*4          BET,BET2,ALFBET,GRIBLI,A0,A,ZETA2
      COMMON /PARAME/ BET,BET2,ALFBET,GRIBLI,A0,A,ZETA2
      REAL*4          SOLD,FINEXP,POIDS,XK0
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0
      REAL*4        S,EBEAM
      COMMON /BEAM/ S,EBEAM
      REAL   MELEC
      PARAMETER ( MELEC  =  .511E-3 )
      PARAMETER (PI = 3.14159265 , ALPH = 1./137.036 , EULER = 0.577216)
      PARAMETER ( ZETA3 = 1.202056 )
      SOLD = S
      ZETA2=PI**2/6
      A0 = 2*ZETA2-.5
      A = 9./8-2*ZETA2
      B = -45./16. + 11./2*ZETA2 + 3.*ZETA3
      BET=2*ALPH/PI*(LOG(S/MELEC**2)-1)
* D2
      IF(ID2.EQ.1)BET = -6*LOG(1-BET/6)
*
      BET2=BET**2
      ALFBET = ALPH * BET /4/PI
      GRIBLI = EXP(BET*(3-EULER*4)/8) / GAMMA(1+BET/2)
     &         * (1 + ALPH/2/PI*A0 + ALFBET*(2*A+B-1.5*A0 -ID2/24.) )
      RETURN
      END
***********************************************************************
      SUBROUTINE INIBIN(ZMIN,XNORM)
*
* --- SPLIT X INTERVAL INTO NBIN OF EQUAL WEIGHT FOR GENERATION
* --- THE LAST BIN BETWEEN X0 AND 1 HAS A WEIGHT TOT
*
      PARAMETER ( NBIN = 100, X0 = .99999 )
      COMMON /BINX/ TOT,XBIN(0:NBIN)
      PARAMETER (TOL = 1E-6)
      EXTERNAL EXSPIN
*------
      XNORM = EXSPIN(ZMIN,1.)
      TOT   = EXSPIN(ZMIN,X0)
*-----
      AA = TOT/NBIN
      XBIN(0) = ZMIN
      DO 10 I=1, NBIN-1
        A = AA * I
        XMIN = XBIN(I-1)
        IF(XMIN.GT..9999)THEN
          XMAX = X0
        ELSE
          XMAX = XMIN
          STEP = AA/EXSP(XMIN)
3         XMAX = MIN(X0,XMAX+STEP)
          STEP = STEP/2
          IF(STEP.LE.1E-10)STEP = 1E-5
          IF(EXSPIN(ZMIN,XMAX).LT.A)THEN
            XMIN = XMAX
            GOTO3
          ENDIF
        ENDIF
1       X = (XMAX+XMIN)/2
        XINT = EXSPIN(ZMIN,X)
        IF(XINT.GT.A)THEN
          XMAX = X
        ELSE
          XMIN = X
        ENDIF
        XBINI = (XMIN + XMAX)/2
        IF(ABS(X-XBINI).LT.TOL)GOTO2
        GOTO1
 2      XBIN(I) = XBINI
 10   CONTINUE
      XBIN(NBIN) = X0
*------
      RETURN
      END
***********************************************************************
      FUNCTION EXSP(X)
      REAL*4          SOLD,FINEXP,POIDS,XK0
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0
      REAL*4        S,EBEAM
      COMMON /BEAM/ S,EBEAM
      IF(SOLD.NE.S) CALL INIPAR
*---------------------------------------------------
      EXSP = EXS(X)/X
      RETURN
      END
***********************************************************************
      FUNCTION EXSPIN(ZMIN,X)
      REAL*4          SOLD,FINEXP,POIDS,XK0
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0
      REAL*4          BET,BET2,ALFBET,GRIBLI,A0,A,ZETA2
      COMMON /PARAME/ BET,BET2,ALFBET,GRIBLI,A0,A,ZETA2
      REAL*4        S,EBEAM
      COMMON /BEAM/ S,EBEAM
      REAL*8 XX,ZZMIN,EPSI
      REAL LOGX,LI2X,LI21MX,LOGZM,LOG1MZ,LI2ZM,LI21MZ
      REAL L1MXPE,LOG2X,LOG2ZM,LOG3X,LOG3ZM
      PARAMETER (EPSI = 1E-4)
      REAL*8 RABSP ,DGAUSS
      EXTERNAL RABSP ,DGAUSS
      IF(SOLD.NE.S) CALL INIPAR
      ZZMIN = ZMIN
      BET05 = BET/2
      ONEMX=1.-X
      IF(X.EQ.1.) THEN
        OMXBET = 0.
        OMXLOG = 0.
        X2 = 1.
        LOGX = 0.
        LOG2X=0.
        LOG3X=0.
        LI2X = ZETA2
        LI21MX = 0.
        ZMMXPE = ZMIN + .5
        L1MXPE = 0.
        PEE2A  = 2*A
      ELSE
        OMXBET = ONEMX**BET05
        OMXLOG = ONEMX*LOG(ONEMX)
        X2=X**2
        LOGX=LOG(X)
        LOG2X=LOGX**2
        LOG3X=LOGX*LOG2X
        LI2X = DILOG(X)
        LI21MX=DILOG(ONEMX)
        ZMMXPE = ZMIN - X
        L1MXPE = LOG(ONEMX)
        PEE2A  = 0.
      ENDIF
      ZM2=ZMIN**2
      ONEMZ=1.-ZMIN
      LOGZM=LOG(ZMIN)
      LOG2ZM = LOGZM**2
      LOG3ZM = LOGZM*LOG2ZM
      LOG1MZ=LOG(ONEMZ)
      LI2ZM=DILOG(ZMIN)
      LI21MZ=DILOG(ONEMZ)
*---------------------------------------------------
* INT_ZMIN^X (1+Z)/Z DZ = O1INT
      O1INT =  LOG(X/ZMIN) + X - ZMIN
* INT_ZMIN^X 2A(Z)/Z DZ = O2INT
      O2INT =  4*(LI2X-LI2ZM) + 4*(OMXLOG - ONEMZ*LOG1MZ)
     & -(LOG2X - LOG2ZM)/2 + X*(3.*LOGX+1.)-ZMIN*(3.*LOGZM+1.)
     & - 5.*LOG(X/ZMIN) - (X-ZMIN) + 4*(LI21MZ-LI21MX)
* INT_ZMIN^X PEE(Z)/Z = O3INT
      O3INT = ZMMXPE + LOGX - LOGZM + 2*(LOG1MZ - L1MXPE)
* INT_ZMIN^X PEEPEE(Z)/Z = O4INT
      O4INT = O2INT + PEE2A + 8*(LI2ZM - LI2X)
     & + 4*(LOG1MZ**2 - L1MXPE**2)
     & + 6.*(LOGX - LOGZM + LOG1MZ - L1MXPE)
*---------------------------------------------------
      XX = X
      EXSPIN =
*--To remove the pole of (1-x)**(bet/2-1)/x at x = 1 for DGAUSS
     & ( ONEMZ**BET05 - OMXBET )*GRIBLI
     & - BET/4 * O1INT
     & + BET2/32 * O2INT
     & + ALFBET * (A0*O1INT + O2INT)
*--To remove the pole of (1-x)**(bet/2-1)/x at x = 0 for DGAUSS
     & + GRIBLI * BET05 * (LOGX-LOGZM)
*--To remove the pole of B(x) at x = 0  for DGAUSS
     & + ALFBET * ( (7-ZETA2)*(LOGX-LOGZM) + 1.25*(LOG2X - LOG2ZM)
     & - (LOG3X - LOG3ZM)/12. )
     & + ID2 * ALFBET * ( -13/6.*O3INT + O4INT/2
     &                    -LOGX + LOGZM - ZMIN + X
     &                    -(LOG2X - LOG2ZM)/4 - X*(LOGX - 1.)/2
     &                    + ZMIN*(LOGZM - 1.)/2                 )/3.
     & + ID3 * BET2/32. * ( -4/3./X + 4/3./ZMIN + LOG2X - LOG2ZM
     &                      + 2*(X*(LOGX-1.) - ZMIN*(LOGZM-1.))
     &                      + LOGX - LOGZM - X + ZMIN - 2/3.*(X2-ZM2) )
     & + DGAUSS(RABSP,ZZMIN,XX,EPSI)
      RETURN
      END
***********************************************************************
      DOUBLE PRECISION FUNCTION RABSP(X)
      REAL*4          BET,BET2,ALFBET,GRIBLI,A0,A,ZETA2
      COMMON /PARAME/ BET,BET2,ALFBET,GRIBLI,A0,A,ZETA2
      REAL*4          SOLD,FINEXP,POIDS,XK0
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0
      REAL*4        S,EBEAM
      COMMON /BEAM/ S,EBEAM
      REAL*8 X
      REAL LOGX,LOG2X
*------
      IF(SOLD.NE.S) CALL INIPAR
      XX = X
      LOGX = LOG(X)
      LOG2X = LOGX**2
*---------------------------------------------------
      RABSP =
     &   GRIBLI * BET/2 * ( (1.-X)**(BET/2) -1.)
     & + RAB(XX)
     & + ALFBET * (-7 + 2*ZETA2 -2.5*LOGX + LOG2X/4)
      RABSP = RABSP/X
      RETURN
      END
***********************************************************************
      FUNCTION EXS(X)
      REAL*4          BET,BET2,ALFBET,GRIBLI,A0,A,ZETA2
      COMMON /PARAME/ BET,BET2,ALFBET,GRIBLI,A0,A,ZETA2
      REAL*4          SOLD,FINEXP,POIDS,XK0
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0
      REAL*4        S,EBEAM
      COMMON /BEAM/ S,EBEAM
      REAL LOGX, LOG1MX
*-----
      IF(SOLD.NE.S) CALL INIPAR
      LOGX=LOG(X)
      LOG1MX=LOG(1.-X)
      X2 = X*X
      X3 = X2*X
      AX     = -2*LOGX/(1.-X)+(1+X)*(1.5*LOGX-2*LOG1MX)-2.5-X/2
      PEE    = (1.+X2)/(1.-X)
      PEEPEE = (8*LOG1MX + 6)/(1.-X) + 2*AX
*---------------------------------------------------
      EXS = BET/2 * (1-X)**(BET/2-1) * GRIBLI
     &   - BET/4*(1+X) + BET2/16*AX
     &   + ALFBET * (A0*(1+X) + 2*AX)
     &   + ID2 * ALFBET * ( -13/6.*PEE + PEEPEE/2
     &                      - (1.-X) - (1.+X)*LOGX/2 )/3.
     &   + ID3 * BET2/32. * ((1-X3)/.75/X +1-X + 2*(1+X)*LOGX)
     &   + RAB(X)
*---------------------------------------------------
      RETURN
      END
********************************************************************
      FUNCTION RAB(X)
      REAL*4          BET,BET2,ALFBET,GRIBLI,A0,A,ZETA2
      COMMON /PARAME/ BET,BET2,ALFBET,GRIBLI,A0,A,ZETA2
      REAL*4          SOLD,FINEXP,POIDS,XK0
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0
      REAL*4        S,EBEAM
      COMMON /BEAM/ S,EBEAM
      REAL LOGX, LOG2X,LOG1MX,LI21MX
*----
      IF(SOLD.NE.S) CALL INIPAR
      X2=X**2
      X3=X*X2
      LOGX=LOG(X)
      LOG2X=LOGX**2
      LOG1MX=LOG(1.-X)
      LI21MX = DILOG(1.-X)
      BX = (1+X2)/(1-X) * (LI21MX + LOGX*LOG1MX + 3.5*LOGX-LOG2X/2)
     &   + (1+X) * (LOG2X/4 + 4*LOG1MX - 2*ZETA2) - LOGX + 7 + X/2
*---------------------------------------------------
      RAB =  ALFBET * BX
* D3
     & + ID3 * ALFBET  * ( (1+X)*(2*LOGX*LOG1MX+LOGX-LOG2X+2*LI21MX)
     &           + (1-X)*(LOG1MX-13./6) + (1-X3)/.75/X*(LOG1MX-1./6)
     &           - LOGX*(2./3/X+1-X/2-X2/2.25)                       )
      RETURN
      END
      SUBROUTINE LUTAUD(IFL)
C----------------------------------------------------------------------
C B.Bloch-Devaux December 88
C! Dummy version of the subroutine in LUNMOD code
C  If used, standard decays of taus will be done
C
C-----------------------------------------------------------------------
      IFL = 0
      WRITE(6,101)
 101  FORMAT(//,10X,'+++ WARNING - LUTAUD DUMMY VERSION USED !!',
     & ' standard LUND scheme for decay modes and branching ratios',//)
      RETURN
      END
