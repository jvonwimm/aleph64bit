C-----------------------------------------------------------------------
C           A L E P H   I N S T A L L A T I O N    N O T E S           |
C                                                                      |
C    Aleph    name  : ESTA01.                                          |
C    original code  : ESTAR from Mana, Martinez and Miquel.            |
C    transmitted by : R. Miquel April 1988.                            |
C                                                                      |
C       (see KINLIB DOC for a description of this version)             |
C                                                                      |
C ----------------------------------------------------------------------
C Modifications to the original code :                                 |
C                                                                      |
C 1. The calls to the RNDM2 function have been replaced by calls to    |
C    RNDM such that the random number generator RANMAR will be used in |
C    relation with KINRUN.                                             |
C                                                                      |
C 2. The variables MZ (Z0 mass), SW2 (sin(theta-w)**2) and NF (number  |
C    of fermion families) are initialised in ASKUSI (DATA CARDS also)  |
C    instead in subroutine PARAMS. Masses, like the mass of the Higgs  |
C    MH or the mass of the Top MT, have not been initialised in ASKUSI |
C    because these parameters are not used in this version of the cal- |
C    culations.                                                        |
C                                 G. Bonneaud, June 6th, 1989.         |
C                                                                      |
C 3. Misusage of DATAN2 function has been fixed in subroutine MCQ,MCE  |
C    call it with (sin,cos) instead of (cos,sin) thanks to T.Medcalf   |
C ----------------------------------------------------------------------
      Program demo
C ======================================================================
C THIS PROGRAM COMPUTES THE CROSS-SECTION FOR E-E-GAMMA FINAL STATES
C INCLUDING DIAGRAMS WITH E* PROPAGATORS
C
C          M. MARTINEZ AND R. MIQUEL (BARCELONA)
C                      AND
C                   C. MANA (CERN)
C
C THE INPUT PARAMETRES ARE MARKED WITH THE LINE :
C INPUT
C
C THIS PROGRAM IS OPTIMISED TO LOOK FOR E-GAMMA FINAL STATES,
C MISSING THE OTHER ELECTRON
C IT ALLOWS THE ELECTRON AND THE POSITRON TO BE LONGITUDINALLY POLARISED
C ======================================================================
        IMPLICIT REAL*8(A-H,M,O-Z)
C GB    REAL*4 RNDM2
        EXTERNAL RNDM
        REAL*4 RNDM, DUMMY
        COMMON / INPOUT / LWRITE
        COMMON // HMEMOR(2000)
        COMMON /CMS/EB,CME,S,BE,BE2
     .         /LEPT1/ME,ME2
        COMMON /DETCUT/XDL,CD,CV,XDE,CE,CTMIN,CTMAX,SSMIN,SSMAX
        COMMON /SAP/SAPE,SAPQ
        COMMON /WAP/WAPE,WAPQ
        COMMON /CONST/PI,DR,SR2,PB
        COMMON /FAST/NWEAK,NFAST,IFULL
        COMMON /PARA  / FCTR,M,M2,GES,MGES,MM(5),CL,CR,RATIO,XLAM
        COMMON /FORHIS/ CT3,E3,CT4,E4,CT5,E5,C35,C45,RSDOT
        COMMON /POLARI/ PLM,PLP
        DIMENSION X(5)
        DIMENSION NPG(2),WSG(2),WS2G(2),WMAXG(2),
     .           SIGG(2),ERRG(2),EFFG(2)
C
C ==========================================================
C ==  INITIALIZATION
C ==========================================================
C
        DATA X/5*0.D0/
        DATA NPG/0,0/
        DATA WS,WS2,WMAX,SIG,ERR,EFF/6*0.D0/
        DATA WSG,WS2G,WMAXG,SIGG,ERRG,EFFG/12*0.D0/
        WRITE(6,998)
998     FORMAT('1    ',60('*')/
     . '          E+ E-  ---->  E+ E- GAMMA (INCLUDING VIRTUAL E_STAR)'/
     . '     ',60('*')////)
C
C.. A) PHYSIC PARAMETERS:
C
C INPUT : CME  = CENTER OF MASS ENERGY (GEV)
C         NFAST= SEE HEADER OF ROUTINE ELEMAT
C         NWEAK=   ""
C         IFULL=   ""
C         PLP  = POSITRON LONGITUDINAL POLARIZATION
C         PLM  = ELECTRON LONGITUDINAL POLARIZATION
C
        CME = 92.D0
        NFAST = 2
        NWEAK = 0
        IFULL = 1
        PLP   = 0
        PLM   = 0
C
C INPUT : XLAM = LAMBDA CUT-OFF IN THE LAGRANGIAN OF THE E*-E-GAMMA AND
C                E*-E-Z0 INTERACTIONS (GEV)
C         CL,CR= LEFT AND RIGHT HANDED PARTS OF THE E*-E-GAMMA COUPLING.
C                (G-2) EXPERIMENTS INDICATE THAT CL*CR HAS TO BE 0
C         M    = E* MASS (GEV)
C
        XLAM = 1.D+3
        CL   =-0.5D0
        CR   = 0.D0
        M    = 50.D0
        CALL PARAMS
C.. B) CUTS
C.. DETECTED PARTICLES
C
C INPUT : EDL = MINIMUM DETECTABLE PHOTON (ELECTRON) ENERGY (GEV)
C         ACD = MINIMUM (SYMMETRIC) DETECTABLE PHOTON (ELECTRON)
C               ANGLE (DEGREES)
C
        EDL = 1.D0
          XDL = EDL/EB
          XDE = XDL
        ACD = 15.D0
          CD = DCOS(ACD*DR)
CCC          CD = 0.966D0
          CE = CD
C
C.. MISSING PARTICLES
C
C INPUT : ACV = VETO ANGLE (DEGREES)
C
        ACV = 2.5D0
          CV = DCOS(ACV*DR)
CCC          CV  = 0.999D0
        CTMIN = 1.D0
        CTMAX = CV
C SSMIN (SSMAX) = MINIMUM (MAXIMUM) E-GAMMA INVARIANT MASS
C
        SSMIN = ME2 + 2.D0*S*(0.5-CD/(CV+CD))
        SSMAX = (CME-ME)**2
        RSMIN = DSQRT(SSMIN)
        RSMAX = DSQRT(SSMAX)
C
C.. C) INTEGRATION PARAMETERS
C
C INPUT : NP     = TOTAL NUMBER OF INTEGRATION POINTS
C         WAPE   = WEIGHT FACTOR FOR SUB-GENERATOR "E" (E*)
C         WAPQ   = WEIGHT FACTOR FOR SUB-GENERATOR "Q" (QED)
C         IRAN   = INITIAL RANDOM NUMBER.
C
        NP   = 100
        WAPE = 1.D0
        WAPQ = 1.D0
        IRAN = 1
        CALL RD2OUT(IS,JS)
        CALL RD2IN(IS*IRAN,JS*IRAN)
C
C
        WRITE(6,999) CME,CD,XDL,SSMIN,SSMAX,CV
999     FORMAT('  =====>  INPUT PARAMETERS:'//
     . ' ==  A) GENERAL:'/
     . '  CME =',G10.4,'GEV '//
     . ' ==  B) DETECTION CUTS:'/
     . '  DETECTED PARTICLES: ',
     .      ' ABS(CT) < ',G11.6,
     .      '  X > ',G10.4,
     .      ' SS IN (',G10.4,' /',G10.4,')'/
     . '   MISSING PARTICLES: ',
     .      ' ABS(CT) > ',G11.6/)
        WRITE(6,1000) NP,WAPE,WAPQ
1000    FORMAT(' ==  C) INTEGRATION:'/
     . '  # INTEGRATION POINTS =',I8/
     . '    IMPORTANCE FACTORS :'/
     . '         GENERATOR_E =',G12.5,'  GENERATOR_Q =',G12.5///)
C...BOOKING SOME HISTOGRAMS
       CALL HBOOK1(1000,' TOTAL WEIGHT        DISTRIBUTION$', 50, 0.,0.)
       CALL HBOOK1(1001,' GEN_E WEIGHT        DISTRIBUTION$', 50, 0.,0.)
       CALL HBOOK1(1002,' GEN_Q WEIGHT        DISTRIBUTION$', 50, 0.,0.)
       CALL HBOOK1(1011,' CT(ELECTRON)        DISTRIBUTION$', 50,-1.,1.)
       CALL HBOOK1(1021,' CT(POSITRON)        DISTRIBUTION$', 50,-1.,1.)
       CALL HBOOK1(1031,' CT(PHOTON)          DISTRIBUTION$', 50,-1.,1.)
       CALL HBOOK1(1012,' E (ELECTRON)        DISTRIBUTION$', 50, 0.,EB)
       CALL HBOOK1(1022,' E (POSITRON)        DISTRIBUTION$', 50, 0.,EB)
       CALL HBOOK1(1032,' E (PHOTON)          DISTRIBUTION$', 50, 0.,EB)
       CALL HBOOK1(1100,' C (ELECTRON-PHOTON) DISTRIBUTION$', 50,-1.,1.)
       CALL HBOOK1(1101,' M (ELECTRON-PHOTON) DISTRIBUTION$',100,
     .                                                      RSMIN,RSMAX)
C
       CALL HBSTAT(0)
       CALL HINTEG(0,'YES')
C
C =================================================================
C ==  X-SECTION ESTIMATION
C =================================================================
C
        CALL MCE(X,IOUT)
        CALL MCQ(X,IOUT)
        SAPT = SAPE + SAPQ
        PE = SAPE/SAPT
        PQ = 1.D0 - PE
        WRITE(6,1001) SAPE,SAPQ,PE,PQ,SAPT
1001    FORMAT(
     .'  =====>  INITIAL ESTIMATIONS:....SAP_E.........SAP_Q...'
     . //23X,'X-SEC. = ',G12.5,'  ',G12.5,' PB'/
     . 14X,'  INITIAL PROB. = ',G12.5,'  ',G12.5//
     .'  =====>  ESTIMATED TOTAL X-SECTION =',G12.5,' PB'///)
C
C =================================================================
C ==  INTEGRATION LOOP
C =================================================================
C
        DO 10 K=1,NP
C....
         DO 20 I=1,5
C GB      X(I) = RNDM2(FLOAT(I))
          X(I) = DBLE(RNDM(DUMMY))
20       CONTINUE
C....
C GB    ETA=RNDM2(6.)
        ETA=DBLE(RNDM(DUMMY))
         IF(ETA.LT.PE)THEN
            CALL MCE(X,IOUT)
            IG = 1
               ELSE
            CALL MCQ(X,IOUT)
            IG = 2
         ENDIF
         IF(IOUT.GT.0.)THEN
            W = 0.D0
               ELSE
            W = F(IDUMMY)
         ENDIF
C....
        NPG(IG) = NPG(IG) + 1
        WS = WS + W
        WSG(IG) = WSG(IG) + W
        WS2 = WS2 + W*W
        WS2G(IG) = WS2G(IG) + W*W
        IF(W.GT.WMAX) WMAX = W
        IF(W.GT.WMAXG(IG)) WMAXG(IG) = W
        CALL HFILL(1000,W)
        CALL HFILL(1000+IG,W)
C
C FILLING HISTOGRAMS WITH THE WEIGHTED EVENTS
        CALL HFILL(1011,CT3,W)
        CALL HFILL(1021,CT4,W)
        CALL HFILL(1031,CT5,W)
        CALL HFILL(1012, E3,W)
        CALL HFILL(1022, E4,W)
        CALL HFILL(1032, E5,W)
        CALL HFILL(1100,C35,W)
        CALL HFILL(1101,RSDOT,W)
10    CONTINUE
C
C ==========================================================
C ==  STATISTICS
C ==========================================================
C
        WRITE(6,1010)
1010    FORMAT('1 ',52('*')/
     . '  ',18('*'),'>    RESULTS   <',18('*')/'  ',52('*')///)
        DO 44 I=1,2
        IF(NPG(I).EQ.0)GO TO 44
        SIGG(I) = WSG(I)/NPG(I)
        ERRG(I) = DSQRT(WS2G(I)-NPG(I)*SIGG(I)**2) / NPG(I)
        IF(WMAXG(I).NE.0.)EFFG(I) = 100.*SIGG(I)/WMAXG(I)
44      CONTINUE
        SIG = WS/NP
        ERR = DSQRT(WS2-NP*SIG**2) / NP
        IF(WMAX.NE.0.)EFF = 100.*SIG/WMAX
C
        WRITE(6,1002)
1002    FORMAT('  ',18('-'),'>   SUBGEN_E     <',18('-')/)
        WRITE(6,1012) NPG(1),SIGG(1),ERRG(1),EFFG(1)
        WRITE(6,1003)
1012    FORMAT('   # POINTS =',I8/
     .         '    AVERAGE =',G12.5,' +/- ',G12.5/
     .         ' WEIGHT EFF.=',G10.4,' %'///)
1003    FORMAT('  ',18('-'),'>   SUBGEN_Q     <',18('-')/)
        WRITE(6,1012) NPG(2),SIGG(2),ERRG(2),EFFG(2)
        WRITE(6,1004)
1004    FORMAT('  ',55('=')/18('='),'>     TOTAL      <',18('=')/
     .   55('=')/)
        WRITE(6,1014) NP,SIG,ERR,EFF
1014    FORMAT('   # POINTS =',I8/
     .         '  X-SECTION =',G12.5,' +/- ',G12.5,' PB'/
     .         ' WEIGHT EFF.=',G10.4,' %'///)
C
C        CALL HSTORE(0,20)
        CALL HISTDO
        END
        SUBROUTINE PARAMS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   INITIALIZATION OF THE MAIN CONSTANTS
C             AND PARAMETERS
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        IMPLICIT REAL*8(A-H,M,O-Z)
        COMPLEX*16 Z0PRO
        COMMON /CONST/PI,DR,SR2,PB
        COMMON /CMS/EB,CME,S,BE,BE2
     .       /NFAM/NF
     .       /HARSOF/DC
        COMMON /BOSON1/MZ,GZ,MW,GW,MH
     .       /BOSON2/MZ2,MW2,MH2
     .       /LEPT1/ME,ME2
     .       /LEPT2/MMU,MTAU,MHEAVY
     .       /HAD/MU,MD,MS,MC,MB,MT
        COMMON /WEAK/SW2,CW2,A2,V2,VU2,VD2
     .       /QED/ALF,ALF2,ALDPI
        COMMON /PARA  / FCTR,M,M2,GES,MGES,MM(5),CL,CR,RATIO,XLAM
        COMMON /COUP/ AL,AR,Z0PRO
C
C..CONST
        PI = DACOS(-1.D0)
        DR = PI/180.D0
        SR2 = DSQRT(2.D0)
        PB = 389386.D3
C
C..QED
        ALF = 1.D0/137.036D0
        ALF2 = ALF*ALF
        ALDPI = ALF/PI
C
C..WEAK
C GB    SW2 = .229D0
        CW2 = 1.D0 - SW2
        A2 = 1.D0/(16.D0*SW2*CW2)
        V2 = A2 * (1.D0-4.D0*SW2)**2
        VU2 = A2 * (8.D0/3.D0*SW2-1.D0)**2
        VD2 = A2 * (1.D0-4.D0/3.D0*SW2)**2
C
C..BOSON1 - 2
C GB    MZ = 92.D0
C GB    NF = 3
        GZ = 2.4D0 + (NF-3.D0)*.17D0
        MW = DSQRT(CW2) * MZ
        GW = 2.6D0
        MH = 100.D0
        MZ2 = MZ*MZ
        MW2 = MW*MW
        MH2 = MH*MH
        SINW=DSQRT(SW2)
        COSW=DSQRT(CW2)
        AL = (SW2-.5)/(SINW*COSW)
        AR = SW2/(SINW*COSW)
        Z0PRO  = DCMPLX(MZ**2,-MZ*GZ)
C
C..LEPT1 - 2
        ME = .000511D0
        ME2 = ME*ME
        MMU = .10565943D0
        MTAU = 1.7842D0
        MHEAVY = 100.D0
C
C..HAD
        MU = .032D0
        MD = .032D0
        MS = .15D0
        MC = 1.5D0
        MB = 4.5D0
        MT = 50.D0
C
C..CMS
        EB = CME/2.D0
        S = CME*CME
        BE2 = 1.D0-(ME/EB)**2
        BE = DSQRT(BE2)
C
        FCTR = 4.D0/XLAM**2
        M2   = M*M
C E* WIDTH (ONLY THE DECAY IN E-GAMMA HAS BEEN CONSIDERED)
        GES  = ALF/XLAM**2*M**3*(CL**2+CR**2)
        MGES = M*GES
C
C RATIO BETWEEN THE E*-E-Z0 AND THE E*-E-GAMMA COUPLINGS
        RATIO= (DSQRT(CW2/SW2)-DSQRT(SW2/CW2))/2.D0
C
        MM(1)= M-ME
        MM(2)= -MM(1)
        MM(3)= M
        MM(4)= MM(1)
        MM(5)= MM(2)
C
        END
      FUNCTION F(IOUT)
C
C CALCULATION OF THE EVENT WEIGHT
C
      IMPLICIT REAL*8(A-H,M,O-Z)
      COMMON /CMS   / EB,CME,S,BE,BE2
     .       /LEPT1 / ME,ME2
     .       /QED   / ALF,ALF2,ALDPI
      COMMON /CONST / PI,DR,SR2,PB
      COMMON /ZETAS / WZ,WZH
      COMMON /MOMENZ/ Q1(5),Q2(5),Q5(5),Q3(5),Q4(5)
      COMMON /STU   / SS,T,U,SP,TP,UP,X1,X2,Y1,Y2
      COMMON /DETCUT/ XDL,CD,CV,XDE,CE,CTMIN,CTMAX,SSMIN,SSMAX
      COMMON /PARA  / FCTR,M,M2,GES,MGES,MM(5),CL,CR,RATIO,XLAM
      COMMON /FORHIS/ CT3,E3,CT4,E4,CT5,E5,C35,C45,RSDOT
      COMMON /GENEE / UU,UUE,SDOT,EPS,X4P,BE4P
      COMMON /GENEQ / EPSS,BE4,Z
      COMMON /WAP   / WAPE,WAPQ
      COMMON /SAP   / SAPE,SAPQ
      DATA FACT/3.97638D+4/
C
C E* APPROXIMATION
      WTAPE  = -(2.D0*PI)**5 * 2.D0*S * 64.D0 * ALDPI**3 * UU/UUE *
     .          100.D0 / XLAM**2 / (1.D0-(SDOT+ME2)/S) /
     .          DSQRT(1.D0-4.D0*ME2*SDOT/(S-SDOT-ME2)**2) /
     .          (EPS-X4P*(1.D0+BE*BE4P*CT4)) / 10.D0 /
     .          ((SDOT-M2)**2+MGES**2) * WAPE
C
C QED APPROXIMATION
      WTAPQ  =  (2.D0*PI)**5 * 2.D0*S * 8.D0 * ALF**3 / (2.D0*PI*PI) /
     .          (2.D0*EB) / (1.D0-CT5+EPSS) / (EB-E4)**2 /
     .          (1.D0+CT4+EPSS/Z**2) * (CME-E4*(1.D0-BE4*C45)) / 10.D0 /
     .          (BE4**2*E4*E5) * WAPQ
C
      CALL ELEMAT(WT)
C
      F      =  WT / (WTAPE+WTAPQ) * (SAPE + SAPQ)
      END
      SUBROUTINE MCE(X,IOUT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   3-BODY PHASE SPACE GENERATOR (E*)
C
C            Q3 - ELECTRON
C            Q4 - POSITRON
C            Q5 - PHOTON
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      IMPLICIT REAL*8(A-H,M,O-Z)
C GB  REAL*4 RNDM2
      EXTERNAL RNDM
      REAL*4 RNDM, DUMMY
      COMMON /CMS   / EB,CME,S,BE,BE2
     .       /LEPT1 / ME,ME2
     .       /QED   / ALF,ALF2,ALDPI
      COMMON /CONST / PI,DR,SR2,PB
      COMMON /ZETAS / WZ,WZH
      COMMON /MOMENZ/ Q1(5),Q2(5),Q5(5),Q3(5),Q4(5)
      COMMON /STU   / SS,T,U,SP,TP,UP,X1,X2,Y1,Y2
      COMMON /DETCUT/ XDL,CD,CV,XDE,CE,CTMIN,CTMAX,SSMIN,SSMAX
      COMMON /PARA  / FCTR,M,M2,GES,MGES,MM(5),CL,CR,RATIO,XLAM
      COMMON /FORHIS/ CT3,E3,CT4,E4,CT5,E5,C35,C45,RSDOT
      COMMON /GENEE / UU,UUE,SDOT,EPS,X4P,BE4P
      COMMON /GENEQ / EPSS,BE4,Z
      COMMON /WAP   / WAPE,WAPQ
      COMMON /SAP   / SAPE,SAPQ
      DIMENSION X(5),QES(4),Q5EG(4)
C
C ===========================================
C    INITIALIZATION
C ===========================================
      DATA INIT/0/,DEL/1.D-5/
      IF(INIT.NE.0)GO TO 330
      INIT=1
C
      Q1(1)=0.D0
      Q1(2)=0.D0
      Q1(3)=EB*BE
      Q1(4)=EB
      Q1(5)=ME
      Q2(1)=0.D0
      Q2(2)=0.D0
      Q2(3)=-EB*BE
      Q2(4)=EB
      Q2(5)=-ME
      Q3(5)=ME
      Q4(5)=-ME
      Q5(5)=0.D0
C
      TPI    = 2.D0*PI
      PI4    = PI/4.D0
      EPS    = 4.D0*ME2/S
      X4P    = 1.D0 - (M2-ME2)/S
      BE4P   = DSQRT(1.D0-ME2/(X4P*EB)**2)
      UCDL   = EPS-X4P*(1.D0-BE*BE4P*CTMIN)
      UCDH   = EPS-X4P*(1.D0-BE*BE4P*CTMAX)
      ATL    = DATAN((SSMIN-M2)/MGES)
      ATH    = DATAN((SSMAX-M2)/MGES)
      UU     = 1.D0 - M2/S
      UUE    = 1.D0 - ME2/S
C
C APPROXIMATION SIZE (LAST FACTOR 2.D0 IS FOR SYMM.)
      FAC    = PB  * 8.D0 * ALF**3 /PI /XLAM**2 * 100.D0 * 2.D0 * WAPE
      SAPE   = FAC * UU/X4P/BE4P/BE*DLOG(UCDH/UCDL)*(ATH-ATL)/MGES/10.D0
C      WRITE(6,111) SAPE
C111   FORMAT(//'   SIGMA_APPROX (E*) = ',G14.6//)
      RETURN
330   CONTINUE
      IOUT   = 1
C
C ===========================================
C    GENERATION
C ===========================================
C
C GENERATE SDOT (INVARIANT (E-G) MASS)
      SDOT   = M2 + MGES * DTAN(ATL+(ATH-ATL)*X(1))
      RSDOT  = DSQRT(SDOT)
C
C COMPUTING E4 (POSITRON)
      E4     = EB*(1.D0-(SDOT-ME2)/S)
      P4     = DSQRT(E4**2-ME2)
      BE4    = P4/E4
      Z      = E4/(EB-E4)
C
C GENERATE COS(THETA_4) & PHI_4 (POSITRON)
      CT4    = (-1.D0 + (EPS - (UCDL*(UCDH/UCDL)**X(2)))/X4P)/BE/BE4P
      ST4    = DSQRT(1. - CT4**2)
C
      PH4    = TPI*X(3)
      CP4    = DCOS(PH4)
      SP4    = DSIN(PH4)
C
C COMPUTING THE FOUR-VECTORS
      Q4(1)  = P4*ST4*CP4
      Q4(2)  = P4*ST4*SP4
      Q4(3)  = P4*CT4
      Q4(4)  = E4
      DO 10 I=1,3
      QES(I) = -Q4(I)
 10   CONTINUE
      QES(4) = CME - Q4(4)
C
C GENERATE COS(THETA_5) & PHI_5 (PHOTON) IN THE (E-G) C.O.M. FRAME
      CT5EG  = 2.D0*X(4) -1.D0
      ST5EG  = DSQRT(1. - CT5EG**2)
C
      PH5EG  = TPI*X(5)
      CP5EG  = DCOS(PH5EG)
      SP5EG  = DSIN(PH5EG)
      E5EG   = 0.5D0*RSDOT*(1.D0-ME2/SDOT)
      Q5EG(1)= E5EG*ST5EG*CP5EG
      Q5EG(2)= E5EG*ST5EG*SP5EG
      Q5EG(3)= E5EG*CT5EG
      Q5EG(4)= E5EG
C
C BOOST BACK TO THE LAB FRAME
      Q5(4)  = (QES(4)*Q5EG(4) + QES(1)*Q5EG(1) + QES(2)*Q5EG(2) +
     .          QES(3)*Q5EG(3))/ RSDOT
      E5     = Q5(4)
C
C REQUIRING MINIMUM ENERGY FOR THE PHOTON
      IF(Q5(4).LT.EB*XDL)RETURN
C
      FAC    = (Q5(4)+Q5EG(4))/(QES(4)+RSDOT)
      Q5(1)  = Q5EG(1) + QES(1)*FAC
      Q5(2)  = Q5EG(2) + QES(2)*FAC
      Q5(3)  = Q5EG(3) + QES(3)*FAC
C
C REQUIRING THE PHOTON TO BE SEEN
      CT5    = Q5(3)/Q5(4)
      IF(DABS(CT5).GT.CE)RETURN
C
      DO 12 I=1,4
      Q3(I)  = QES(I) - Q5(I)
 12   CONTINUE
      E3     = Q3(4)
C
C REQUIRING MINIMUM ENERGY FOR THE ELECTRON
      IF(Q3(4).LT.EB*XDE)RETURN
C
C REQUIRING THE ELECTRON TO BE SEEN
      P32    = Q3(1)**2 + Q3(2)**2 + Q3(3)**2
      CT3    = Q3(3)/DSQRT(P32)
      IF(DABS(CT3).GT.CE)RETURN
C
CBBL    Fix DATAN2 arguments
CBBL  PH5    = DATAN2(Q5(1),Q5(2))
CBBL  PH3    = DATAN2(Q3(1),Q3(2))
      PH5    = DATAN2(Q5(2),Q5(1))
      PH3    = DATAN2(Q3(2),Q3(1))
C
C HERE YOU CAN INCLUDE A CUT IN PHI BETWEEN ELECTRON AND PHOTON
C      PHD    = DABS(PH5-PH3)
C      IF(PHD.GT.PI) PHD = TPI - PHD
C      IF(PHD.LT.PI4)RETURN
C
      ST3    = DSQRT(1.D0-CT3**2)
      ST5    = DSQRT(1.D0-CT5**2)
      CP3    = DCOS(PH3)
      CP5    = DCOS(PH5)
      SP3    = DSIN(PH3)
      SP5    = DSIN(PH5)
      C35    = CT3*CT5 + ST3*ST5*(CP3*CP5+SP3*SP5)
      C45    = CT4*CT5 + ST4*ST5*(CP4*CP5+SP4*SP5)
C
C CHECKING THE KINEMATICS
      ZME2   = (Q3(4)**2 - P32)/ME2
      IF(ZME2.LT.(1.D0-DEL).OR.ZME2.GT.(1.D0+DEL))RETURN
C
      IOUT   = -1
      END
      SUBROUTINE MCQ(X,IOUT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   3-BODY PHASE SPACE GENERATOR (QED)
C
C            Q3 - ELECTRON
C            Q4 - POSITRON
C            Q5 - PHOTON
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      IMPLICIT REAL*8(A-H,M,O-Z)
C GB  REAL*4 RNDM2
      EXTERNAL RNDM
      REAL*4 RNDM, DUMMY
      COMMON /CMS   / EB,CME,S,BE,BE2
     .       /LEPT1 / ME,ME2
     .       /QED   / ALF,ALF2,ALDPI
      COMMON /CONST / PI,DR,SR2,PB
      COMMON /ZETAS / WZ,WZH
      COMMON /MOMENZ/ Q1(5),Q2(5),Q5(5),Q3(5),Q4(5)
      COMMON /STU   / SS,T,U,SP,TP,UP,X1,X2,Y1,Y2
      COMMON /DETCUT/ XDL,CD,CV,XDE,CE,CTMIN,CTMAX,SSMIN,SSMAX
      COMMON /FORHIS/ CT3,E3,CT4,E4,CT5,E5,C35,C45,RSDOT
      COMMON /GENEE / UU,UUE,SDOT,EEPS,X4P,BE4P
      COMMON /GENEQ / EPS,BE4,Z
      COMMON /WAP   / WAPE,WAPQ
      COMMON /SAP   / SAPE,SAPQ
      DIMENSION X(5)
C
C ===========================================
C    INITIALIZATION
C ===========================================
      DATA INIT/0/,DEL/1.D-5/
      IF(INIT.NE.0)GO TO 330
      INIT=1
C
      Q1(1)=0.D0
      Q1(2)=0.D0
      Q1(3)=EB*BE
      Q1(4)=EB
      Q1(5)=ME
      Q2(1)=0.D0
      Q2(2)=0.D0
      Q2(3)=-EB*BE
      Q2(4)=EB
      Q2(5)=-ME
      Q3(5)=ME
      Q4(5)=-ME
      Q5(5)=0.D0
C
      TPI = 2.D0*PI
      PI4 = PI/4.D0
      EPS = 2.D0*ME2/S
CCC      CT45L = -DCOS(DACOS(CD)-DACOS(CV))
CCC      EH = 2.D0*EB * (1.D0-XDL)/(2.D0 + XDL*(CT35L-1.D0))
      EH = CME*CD/(CD+CV)
      ZH = EH/(EB-EH)
      ZL = ME/(EB-ME)
      DZ = ZH-ZL
      UCV = 1.D0-CV
      WZH = 2.D0/(BE*S) * DLOG(1.D0 + UCV*ZH*ZH/EPS)
      UCDL = EPS-CD+1.D0
      UCDH = EPS+CD+1.D0
CCC      UCDH = EPS+1.D0
C
C APPROXIMATION SIZE
      FAC = PB * ALF**3/(2.D0*PI**2)
      SAPQ = FAC * DZ*WZH * TPI * DLOG(UCDH/UCDL) * TPI *2.D0*WAPQ/10.D0
C      WRITE(6,111) SAPQ
C111   FORMAT(//'   SIGMA_APPROX (QED) = ',G14.6//)
      RETURN
330   CONTINUE
      IOUT=1
C
C ===========================================
C    GENERATION
C ===========================================
C
C GENERATE E4  (POSITRON)
      Z = ZL + X(1)*DZ
      E4 = EB*Z/(1.D0+Z)
      P4 = DSQRT(E4*E4-ME2)
      BE4= P4/E4
      RF = 1.D0 + UCV*Z*Z/EPS
      WZ = 2.D0*E4/(S*P4) * DLOG(RF)
C GB  ZRAN = RNDM2(0.)
      ZRAN = DBLE(RNDM(DUMMY))
      IF(ZRAN*WZH.GT.WZ)RETURN
C
C GENERATE COS(THETA_4) & PHI_4  (POSITRON)
      CT4 = - EPS/(Z*Z) * (1.D0 - RF**X(2)) - 1.D0
      ST4 = DSQRT(1. - CT4**2)
C
      PH4 = TPI*X(3)
      CP4 = DCOS(PH4)
      SP4 = DSIN(PH4)
C
C GENERATE COS(THETA_5) & PHI_5  (PHOTON)
      CT5 = - UCDL*(UCDH/UCDL)**X(4) + EPS + 1.D0
      ST5 = DSQRT(1. - CT5**2)
C
      PH5 = TPI*X(5)
      CP5 = DCOS(PH5)
      SP5 = DSIN(PH5)
C
      C45=ST4*ST5*(SP4*SP5+CP4*CP5)+CT4*CT5
C
C COMPUTING E5
      E5 = CME*(EB-E4)/(P4*C45-E4+CME)
C REQUIRING MINIMUM ENERGY FOR THE PHOTON
      IF(E5.LT.EB*XDL)RETURN
C
C COMPUTING THE FOUR-VECTORS
      Q4(1) = P4*ST4*CP4
      Q4(2) = P4*ST4*SP4
      Q4(3) = P4*CT4
      Q4(4) = E4
C
      Q5(1) = E5*ST5*CP5
      Q5(2) = E5*ST5*SP5
      Q5(3) = E5*CT5
      Q5(4) = E5
C
      DO 121 I=1,3
      Q3(I) = - Q4(I) - Q5(I)
121   CONTINUE
      Q3(4) = CME - E4 - E5
      E3    = Q3(4)
C
C REQUIRING MINIMUM ENERGY FOR THE ELECTRON
      IF(Q3(4).LT.EB*XDE)RETURN
C
C REQUIRING THE ELECTRON TO BE SEEN
      P32 = Q3(1)**2 + Q3(2)**2 + Q3(3)**2
      CT3 = Q3(3)/DSQRT(P32)
      IF(DABS(CT3).GT.CE)RETURN
C
CBBL    Fix DATAN2 arguments
CBBL  PH5    = DATAN2(Q5(1),Q5(2))
CBBL  PH3    = DATAN2(Q3(1),Q3(2))
      PH5    = DATAN2(Q5(2),Q5(1))
      PH3    = DATAN2(Q3(2),Q3(1))
C
C HERE YOU CAN INCLUDE A CUT IN PHI BETWEEN ELECTRON AND PHOTON
C      PHD = DABS(PH5-PH3)
C      IF(PHD.GT.PI) PHD = TPI - PHD
C      IF(PHD.LT.PI4)RETURN
C
C CHECKING THE KINEMATICS
      ZME2 = (Q3(4)**2 - P32)/ME2
      IF(ZME2.LT.(1.D0-DEL).OR.ZME2.GT.(1.D0+DEL))RETURN
C
      SDOT = S + ME2 - 2.D0*CME*E4
      RSDOT= DSQRT(SDOT)
      IF (SDOT.LT.SSMIN.OR.SDOT.GT.SSMAX) RETURN
      ST3=DSQRT(1.D0-CT3**2)
      CP3=DCOS(PH3)
      SP3=DSIN(PH3)
      C35=ST3*ST5*(SP3*SP5+CP3*CP5)+CT3*CT5
      IOUT = -1
      END
      SUBROUTINE ELEMAT(WT)
C ................................................................
C ... THIS ROUTINE CALCULATES THE COMPLETE MATRIX ELEMENT
C ... SQUARED ( VIRTUAL PHOTON + VIRTUAL Z0 ) FOR THE
C ... PROCESS
C ...
C ... E-(Q1) + E+(Q2) ----> G(Q3) + E-(Q4) + E+(Q5)
C ...
C ... INCLUDING DIAGRAMS WITH AN E* PROPAGATOR
C ...
C ...
C ... FOR THE STANDARD MODEL DIAGRAMS THE OPTIONS ARE
C ...
C ...    NWEAK=0 ===> QED
C ...         =1 ===> ELECTROWEAK
C ...
C ...    NFAST=0 ===> COMPLETE MATRIX ELEMENT SQUARED
C ...         =1 ===> Z0 T-CHANNEL DIAGRAMS NEGLECTED
C ...         =2 ===> ONLY PHOTON T-CHANNEL DIAGRAMS INCLUDED
C ...         =3 ===> NO STANDARD MODEL DIAGRAMS TAKEN INTO ACCOUNT
C ...
C ...
C ... FOR THE E* DIAGRAMS THE OPTIONS ARE
C ...
C ...    IFULL=0 ===> NO E* DIAGRAMS TAKEN INTO ACCOUNT
C ...         =1 ===> ONLY PHOTON T-CHANNEL DIAGRAMS INCLUDED
C ...         =2 ===> PHOTON AND Z0 S-CHANNEL DIAGRAMS ALSO INCLUDED
C ...         =3 ===> COMPLETE MATRIX ELEMENT SQUARED
C ...
C ...
C ...            C. MANA, M. MARTINEZ & R. MIQUEL (BARCELONA 87)
C ................................................................
      IMPLICIT REAL*8(A-H,M,O-Z)
      COMMON /POLARI/ PLM,PLP
      DIMENSION XX(32),NCONF(5,32),POL(4)
      DATA NCONF/
     . 1, 1, 1,-1, 1,  1, 1, 1,-1,-1,  1, 1,-1, 1,-1,  1, 1,-1,-1, 1,
     . 1, 1,-1, 1, 1,  1, 1, 1, 1, 1,  1, 1,-1,-1,-1,  1, 1, 1, 1,-1,
     . 1,-1,-1, 1,-1,  1,-1, 1, 1,-1,  1,-1, 1,-1, 1,  1,-1, 1,-1,-1,
     . 1,-1,-1, 1, 1,  1,-1,-1,-1, 1,  1,-1,-1,-1,-1,  1,-1, 1, 1, 1,
     .-1, 1,-1, 1,-1, -1, 1,-1,-1,-1, -1, 1, 1, 1, 1, -1, 1, 1, 1,-1,
     .-1, 1, 1,-1, 1, -1, 1,-1,-1, 1, -1, 1, 1,-1,-1, -1, 1,-1, 1, 1,
     .-1,-1, 1, 1, 1, -1,-1, 1, 1,-1, -1,-1, 1,-1,-1, -1,-1,-1,-1,-1,
     .-1,-1, 1,-1, 1, -1,-1,-1, 1, 1, -1,-1,-1, 1,-1, -1,-1,-1,-1, 1/
      DATA FACT/7.69437D-4/
C        FACT = (4.*PI*ALF)**3
      CALL SPININ(INF)
      DO 101 I=1,32
      XX(I) = AMTOT(NCONF(1,I))
 101  CONTINUE
C
      ALP    = (1.+PLP)/2.
      ALM    = (1.+PLM)/2.
      ARP    = 1.-ALP
      ARM    = 1.-ALM
      POL(1) = ALM*ALP
      POL(2) = ALM*ARP
      POL(3) = ARM*ALP
      POL(4) = ARM*ARP
C
      WT = 0.D0
      DO 100 I=1,32
      WT  = WT + XX(I)*POL((I+7)/8)
 100  CONTINUE
      WT = WT*FACT
      RETURN
      END
      FUNCTION AMTOT(L)
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ................................................................
C ... THIS FUNCTION ADDS THE CONTRIBUTION OF ALL THE DIAGRAMS
C ... FOR A GIVEN POLARISATION CONFIGURATION
C ...
C ...                         R. MIQUEL (BARCELONA 87)
C ................................................................
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION L(5)
      COMPLEX*16 AMT,AMPLI,AMTS,AMPLIS
      INTEGER P
      COMMON /FAST/ NWEAK,NFAST,IFULL
C      COMMON /CHECK / P
      COMMON /PARA/ FCTR
C        FCTR = 4./LAMBA**2
      DATA INF /0/
      DATA P   /1/
C
      AMT = (0.D0,0.D0)
      AMTS= (0.D0,0.D0)
      IF (NFAST.EQ.3) GOTO 1000
C
C ========== STANDARD MODEL DIAGRAMS ================================
C
C ............................. T-CHANNEL DIAGRAMS
      NWE  = 1
      IF(NFAST.NE.0.OR.NWEAK.EQ.0)NWE=0
      AMT =       AMPLI(1,L(1),2,L(2),3, L(3),4,L(4),5,L(5),0,NWE,INF)
      AMT = AMT - AMPLI(4,L(4),5,L(5),3,-L(3),1,L(1),2,L(2),1,NWE,INF)
      AMT = AMT - AMPLI(2,L(2),1,L(1),3,-L(3),5,L(5),4,L(4),1,NWE,INF)
      AMT = AMT + AMPLI(5,L(5),4,L(4),3, L(3),2,L(2),1,L(1),0,NWE,INF)
C ............................. S-CHANNEL DIAGRAMS
      IF(NFAST.EQ.2) GO TO 1000
      NWE = 1
      IF(NWEAK.EQ.0)NWE=0
      AMT = AMT - AMPLI(1,L(1),4,L(4),3, L(3),2,L(2),5,L(5),0,NWE,INF)
      AMT = AMT + AMPLI(4,L(4),1,L(1),3,-L(3),5,L(5),2,L(2),1,NWE,INF)
      AMT = AMT + AMPLI(2,L(2),5,L(5),3,-L(3),1,L(1),4,L(4),1,NWE,INF)
      AMT = AMT - AMPLI(5,L(5),2,L(2),3, L(3),4,L(4),1,L(1),0,NWE,INF)
1000  CONTINUE
      IF (IFULL.EQ.0) GOTO 999
C
C ========== E* DIAGRAMS =============================================
C
C ............................. T-CHANNEL DIAGRAMS
      IZ = 0
      IF (IFULL.EQ.3) IZ = 1
         AMTS=      AMPLIS(1,L(1),2,L(2),3, L(3),4,L(4),5,L(5),IZ,0,INF)
         AMTS=AMTS- AMPLIS(4,L(4),5,L(5),3,-L(3),1,L(1),2,L(2),IZ,1,INF)
         AMTS=AMTS- AMPLIS(2,L(2),1,L(1),3,-L(3),5,L(5),4,L(4),IZ,1,INF)
         AMTS=AMTS+ AMPLIS(5,L(5),4,L(4),3, L(3),2,L(2),1,L(1),IZ,0,INF)
C ............................. S-CHANNEL DIAGRAMS
      IZ = 1
      IF (IFULL.GT.1) THEN
         AMTS=AMTS- AMPLIS(1,L(1),4,L(4),3, L(3),2,L(2),5,L(5),IZ,0,INF)
         AMTS=AMTS+ AMPLIS(4,L(4),1,L(1),3,-L(3),5,L(5),2,L(2),IZ,1,INF)
         AMTS=AMTS+ AMPLIS(2,L(2),5,L(5),3,-L(3),1,L(1),4,L(4),IZ,1,INF)
         AMTS=AMTS- AMPLIS(5,L(5),2,L(2),3, L(3),4,L(4),1,L(1),IZ,0,INF)
      ENDIF
C
      AMT   = AMT + FCTR*AMTS
999   AMTOT = AMT * DCONJG(AMT)
      END
         FUNCTION AMPLI(P1,L1,P2,L2,P3,L3,P4,L4,P5,L5,ICONJG,NWE,INF)
C ................................................................
C ... THIS FUNCTION CALCULATES THE COMPLETE AMPLITUDE OF A
C ... COUPLE ( VIRTUAL PHOTON + VIRTUAL Z0 ) OF DIAGRAMS
C ... GIVEN ANY CONFIGURATION OF MOMENTA AND SPINS.
C ... (STANDARD MODEL DIAGRAMS)
C ...
C ... E-(Q1) + E+(Q2) ----> G(Q3) + E-(Q4) + E+(Q5)
C ...
C ...                  C.MANA & M.MARTINEZ   DESY 86
C ................................................................
         IMPLICIT REAL*8(A-H,O-Z)
         COMMON / INPOUT / LWRITE
         COMMON / PRODUX / SP,SM,U,E,D
         DIMENSION SP(5,5),SM(5,5),D(5,5),E(5),U(5),B(5)
         COMPLEX*16 Z,SP,SM
         COMPLEX*16 AMPLI,AMPLIZ,PHOT1,PHOT2,Z0PRO,PROPZ
         INTEGER P,P1,P2,P3,P4,P5
C         COMMON /CHECK / P
         COMMON /COUP/ AL,AR,Z0PRO
         DATA P   /1/
         DATA SQR2 /1.41421356D0/, O /1.D0/
         DATA B/1.D0, 1.D0,-1.D0,-1.D0,-1.D0/
C C----
         AMPLI = (0.D0,0.D0)
         AMPLIZ = (0.D0,0.D0)
C0 ++++++++++++++++++++++++++  LOOP OVER REPEATED INDEX "IL"
         ZN=1./(SQR2 * CDABS(SP(P,P3)) )
         DO 1 I=1,2
         IL  = 2*I - 3
         PHOT1 = ZN * B(P1) *  Z(P1,IL,P1,L1,P,L3,P3,L3,O,O,O,O,INF)
         PHOT2 = ZN * B(P3) *  Z(P3,IL,P1,L1,P,L3,P3,L3,O,O,O,O,INF)
C ------------------------  PHOTON DIAGRAM.
         AMPLI = AMPLI
     . + Z(P4,L4,P1,IL,P2,L2,P5,L5,O,O,O,O,INF)*PHOT1
     . + Z(P4,L4,P3,IL,P2,L2,P5,L5,O,O,O,O,INF)*PHOT2
C ------------------------  Z0 DIAGRAM.
         IF(NWE.EQ.0)GO TO 1
         AMPLIZ = AMPLIZ
     . + Z(P4,L4,P1,IL,P2,L2,P5,L5,AL,AR,AL,AR,INF)*PHOT1
     . + Z(P4,L4,P3,IL,P2,L2,P5,L5,AL,AR,AL,AR,INF)*PHOT2
C ------------------------
C GB     IF (INF.GE.4) PRINT 2,IL,P1,L1,P2,L2,P3,L3,P4,L4,P5,L5,
         IF (INF.GE.4) WRITE(LWRITE,2)IL,P1,L1,P2,L2,P3,L3,P4,L4,P5,L5,
     .                                AMPLI,AMPLIZ
    2    FORMAT(' AMPLI: IL,P(I),L(I),AMPLI,AMPLIZ =',
     .                        I3,' ',5(2I3,' ')/4G15.6)
    1    CONTINUE
C0 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         PROPE = B(P1)*B(P3)*D(P1,P3)
         PROPG = 0.5D0*D(P2,P2)+0.5D0*D(P5,P5)+B(P2)*B(P5)*D(P2,P5)
         IF (ICONJG.EQ.1) AMPLI = DCONJG(AMPLI)
         AMPLI = AMPLI/(PROPE*PROPG)
         IF(NWE.EQ.0) RETURN
C
         IF (ICONJG.EQ.1) AMPLIZ= DCONJG(AMPLIZ)
         PROPZ = PROPG - Z0PRO
         AMPLIZ= AMPLIZ/(PROPE*PROPZ)
         AMPLI = AMPLI+AMPLIZ
         END
      FUNCTION AMPLIS(P1,L1,P2,L2,P3,L3,P4,L4,P5,L5,IZ,ICONJG,INF)
C ................................................................
C ... THIS FUNCTION CALCULATES THE COMPLETE AMPLITUDE
C ... OF A COUPLE (GAMMA + Z0) OF DIAGRAMS
C ... GIVEN ANY CONFIGURATION OF MOMENTA AND SPINS.
C ... (E* DIAGRAMS)
C ...
C ... E-(Q1) + E+(Q2) ----> G(Q3) + E-(Q4) + E+(Q5)
C ...
C ...                            R. MIQUEL (BARCELONA 87)
C ................................................................
      IMPLICIT REAL*8(A-H,M,O-X)
      IMPLICIT COMPLEX*16(Y,Z)
      COMPLEX*16 SP,SM,AMPLIS,AMPLIZ,PROPES,PROPZ
      REAL*8 ZN
      INTEGER P,P1,P2,P3,P4,P5
      COMMON /PRODUX/ SP,SM,U,E,D
      COMMON /PARA  / FCTR,M,M2,GES,MGES,MM(5),CL,CR,RATIO
      COMMON /LEPT1/ ME,ME2
      COMMON /COUP/ AL,AR,Z0PRO
C      COMMON /CHECK / P,IFULL
      DATA P   /1/
      DIMENSION SP(5,5),SM(5,5),D(5,5),E(5),U(5),B(5)
      DATA SQR2 /1.41421356D0/, O /1.D0/
      DATA B/1.D0, 1.D0,-1.D0,-1.D0,-1.D0/
C
      AMPLIS= (0.D0,0.D0)
      AMPLIZ= (0.D0,0.D0)
      ZN    = 1./(SQR2 * CDABS(SP(P,P3)) )
C +++++++++++++++++++++  LOOP OVER REPEATED INDICES "IL,ILP,ILPP"
      DO 1 I=1,2
      IL  = 2*I - 3
C
      Z5  = Z(P2,L2,P5,L5,P4,L4,P5,IL,O,O,O,O,INF) * B(P5)
      Z2  = Z(P2,L2,P5,L5,P4,L4,P2,IL,O,O,O,O,INF) * B(P2)
CZ
      IF (IZ.EQ.1) THEN
         Z5Z = Z(P2,L2,P5,L5,P4,L4,P5,IL,AL,AR,O,O,INF) * B(P5)
         Z2Z = Z(P2,L2,P5,L5,P4,L4,P2,IL,AL,AR,O,O,INF) * B(P2)
      ENDIF
CCCC
CCCC
      DO 1 J=1,2
      ILPP= 2*J - 3
      ZY53= (0.D0,0.D0)
      ZY23= (0.D0,0.D0)
C
      Y3  = Y(P3,ILPP,P1,L1,CL,CR)
      Z53 = Z(P5,IL,P3,ILPP,P,L3,P3,L3,CL,CR,O,O,INF) * B(P1)*MM(P1)
      Z23 = Z(P2,IL,P3,ILPP,P,L3,P3,L3,CL,CR,O,O,INF) * B(P1)*MM(P1)
CCCC
CCCC
      DO 2 K=1,2
      ILP = 2*K - 3
C
      Z13 = Z(P1,ILP,P3,ILPP,P,L3,P3,L3,O,O,O,O,INF) * B(P1)
      Z33 = Z(P3,ILP,P3,ILPP,P,L3,P3,L3,O,O,O,O,INF) * B(P3)
      YY51= Y(P5,IL,P1,ILP,CR,CL)
      YY53= Y(P5,IL,P3,ILP,CR,CL)
      YY21= Y(P2,IL,P1,ILP,CR,CL)
      YY23= Y(P2,IL,P3,ILP,CR,CL)
      ZY53= ZY53 + YY51*Z13 + YY53*Z33
      ZY23= ZY23 + YY21*Z13 + YY23*Z33
 2    CONTINUE
CCCC
CCCC
C
      AMPLIS= AMPLIS + Y3 * (Z5 *(ZY53+Z53) + Z2 *(ZY23+Z23))
CZ
      IF (IZ.EQ.1) AMPLIZ=AMPLIZ+ Y3 * (Z5Z*(ZY53+Z53) + Z2Z*(ZY23+Z23))
 1    CONTINUE
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      PROPES= DCMPLX (B(P1)*B(P3)*D(P1,P3) + ME2 - M2 , MGES)
      PROPG = B(P2)*B(P5)*D(P2,P5) + 2.D0*ME2
      IF (ICONJG.EQ.1) THEN
         AMPLIS= DCONJG(AMPLIS)
         AMPLIZ= DCONJG(AMPLIZ)
      ENDIF
      AMPLIS = AMPLIS *ZN/PROPES/PROPG
CZ
      IF (IZ.EQ.1) THEN
         PROPZ = PROPG - Z0PRO
         AMPLIZ= AMPLIZ*ZN/PROPES/PROPZ*(-RATIO)
         AMPLIS= AMPLIS + AMPLIZ
      ENDIF
      END
      SUBROUTINE SPININ(INF)
C ....................................................................
C ... COMPUTATION OF THE BASIC QUANTITIES
C ... NEEDED FOR THE HELICITY AMPLISTUDES EVALUATION
C ...
C ...   INPUT   ==> VECTORS P1,P2,P3,P4,P5       ( COMMON /MOMENZ/ )
C ...       FORMAT: (PX,PY,PZ,E,M)
C ...   OUTPUT  ==> BASIC QUANTITIES SP,SM,U,E,D ( COMMON /PRODUX/ )
C ...               ( SP --> S+  / SM --> S- )
C ...
C ...                  C.MANA & M.MARTINEZ   DESY 86
C ....................................................................
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 SP,SM
      COMMON / INPOUT / LWRITE
      COMMON / MOMENZ / P1,P2,P3,P4,P5
      COMMON / PRODUX / SP,SM,U,E,D
      DIMENSION P1(5),P2(5),P3(5),P4(5),P5(5)
      DIMENSION Q(5,5),SP(5,5),SM(5,5),D(5,5)
      DIMENSION E(5),U(5)
      EQUIVALENCE ( P1(1) , Q(1,1) )
C ----------
      DO 1 I=1,5
      DIFF  = Q(4,I) - Q(1,I)
      IF (DIFF.LE.0.) WRITE (LWRITE,*) 'WARNING : DIFF(',I,') = ',
     .                                            DIFF
      U(I) = DSQRT( 2.*( Q(4,I) - Q(1,I) )  )
      E(I) = Q(5,I)/U(I)
    1 CONTINUE
      DO 2 I=1,5
      DO 2 J=I,5
      SP(I,J) = DCMPLX( Q(2,I) , Q(3,I) ) * U(J)/U(I)
     .        - DCMPLX( Q(2,J) , Q(3,J) ) * U(I)/U(J)
      SP(J,I) =-SP(I,J)
      SM(I,J) =-DCONJG( SP(I,J) )
      SM(J,I) =-SM(I,J)
      D(I,J)  = SP(I,J)*SM(J,I) + (E(I)*U(J))**2 + (E(J)*U(I))**2
      D(J,I)  = D(I,J)
    2 CONTINUE
C ----------
      IF(INF.LT.1) RETURN
      WRITE(LWRITE,100)
  100 FORMAT(' ',40(1H-),' SPININ INF  ',40(1H-))
      WRITE(LWRITE,101) (P1(I),P2(I),P3(I),P4(I),P5(I),I=1,5)
  101 FORMAT('0INPUT (PX ,PY ,PZ ,E ,M ) ',/,(5G15.6))
      WRITE(LWRITE,102) (U(I),E(I),I=1,5)
  102 FORMAT('0VECTORS U(I) AND E(I)',/,(2G15.6))
      WRITE(LWRITE,104) ((SP(I,J),J=1,5),I=1,5)
  104 FORMAT('0MATRIX SP(I,J)',/,(5('  ',2G10.3)))
      WRITE(LWRITE,105) ((SM(I,J),J=1,5),I=1,5)
  105 FORMAT('0MATRIX SM(I,J)',/,(5('  ',2G10.3)))
      WRITE(LWRITE,107) ((D(I,J),J=1,5),I=1,5)
  107 FORMAT('0MATRIX D(I,J)',/,(5('  ',G15.6)))
      RETURN
      END
      FUNCTION Z(P1,L1,P2,L2,P3,L3,P4,L4,CL1,CR1,CL2,CR2,INF)
C ....................................................................
C ... CALCULATION OF ALL THE Z FUNCTIONS FOR GAMMA OR Z0 EXCHANGE.
C ... ( FOR GAMMA, SET CL1=1,CR1=1,CL2=1,CR2=1 )
C ...
C ...                  C.MANA & M.MARTINEZ   DESY 86
C ....................................................................
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 Z,SP,SM
      INTEGER P1,P2,P3,P4
      DIMENSION SP(5,5),SM(5,5),D(5,5),E(5),U(5)
      COMMON / INPOUT / LWRITE
      COMMON / PRODUX / SP,SM,U,E,D
      LZ=9-4*L1-2*L2-L3-(L4+1)/2
      GOTO(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),LZ
    1 Z= -2.*( CR1*CR2*SP(P1,P3)*SM(P2,P4)
     .       - CR1*CL2*U(P1)*U(P2)*E(P3)*E(P4)
     .       - CL1*CR2*U(P3)*U(P4)*E(P1)*E(P2) )
      GOTO 17
    2 Z= -2.*U(P2)*(    CR1*CR2*SP(P1,P3)*E(P4)
     .                - CR1*CL2*SP(P1,P4)*E(P3) )
      GOTO 17
    3 Z= -2.*U(P1)*(    CR1*CL2*SM(P2,P3)*E(P4)
     .                - CR1*CR2*SM(P2,P4)*E(P3) )
      GOTO 17
    4 Z= -2.*( CR1*CL2*SP(P1,P4)*SM(P2,P3)
     .       - CR1*CR2*U(P1)*U(P2)*E(P3)*E(P4)
     .       - CL1*CL2*U(P3)*U(P4)*E(P1)*E(P2) )
      GOTO 17
    5 Z= -2.*U(P4)*(    CR1*CR2*SP(P3,P1)*E(P2)
     .                - CL1*CR2*SP(P3,P2)*E(P1) )
      GOTO 17
    6 Z=(0.D0,0.D0)
      GOTO 17
    7 Z=  2.*( CR1*E(P2)*U(P1) - CL1*E(P1)*U(P2) )
     .      *( CL2*E(P4)*U(P3) - CR2*E(P3)*U(P4) )
      GOTO 17
    8 Z=  2.*U(P3)*(    CR1*CL2*SP(P1,P4)*E(P2)
     .                - CL1*CL2*SP(P2,P4)*E(P1) )
      GOTO 17
    9 Z=  2.*U(P3)*(    CL1*CR2*SM(P1,P4)*E(P2)
     .                - CR1*CR2*SM(P2,P4)*E(P1) )
      GOTO 17
   10 Z=  2.*( CL1*E(P2)*U(P1) - CR1*E(P1)*U(P2) )
     .      *( CR2*E(P4)*U(P3) - CL2*E(P3)*U(P4) )
      GOTO 17
   11 Z=(0.D0,0.D0)
      GOTO 17
   12 Z=  2.*U(P4)*(    CL1*CL2*SM(P1,P3)*E(P2)
     .                - CR1*CL2*SM(P2,P3)*E(P1) )
      GOTO 17
   13 Z= -2.*( CL1*CR2*SP(P2,P3)*SM(P1,P4)
     .       - CL1*CL2*U(P1)*U(P2)*E(P3)*E(P4)
     .       - CR1*CR2*U(P3)*U(P4)*E(P1)*E(P2) )
      GOTO 17
   14 Z= -2.*U(P1)*(    CL1*CR2*SP(P2,P3)*E(P4)
     .                - CL1*CL2*SP(P2,P4)*E(P3) )
      GOTO 17
   15 Z= -2.*U(P2)*(    CL1*CL2*SM(P1,P3)*E(P4)
     .                - CL1*CR2*SM(P1,P4)*E(P3) )
      GOTO 17
   16 Z= -2.*( CL1*CL2*SP(P2,P4)*SM(P1,P3)
     .       - CL1*CR2*U(P1)*U(P2)*E(P3)*E(P4)
     .       - CR1*CL2*U(P3)*U(P4)*E(P1)*E(P2) )
   17 IF(INF.LT.5) RETURN
C GB  PRINT 18,L1,L2,L3,L4,LZ,Z
      WRITE(LWRITE,18)L1,L2,L3,L4,LZ,Z
   18 FORMAT(' Z:   L1,L2,L3,L4,  LZ,  Z =',4I3,I6,G20.6,G15.6)
      RETURN
      END
      FUNCTION Y(P1,L1,P2,L2,CL,CR)
C CALCULATION OF MASS TERMS
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 Y,SP,SM
      INTEGER P1,P2
      DIMENSION SP(5,5),SM(5,5),D(5,5),E(5),U(5)
      COMMON / PRODUX / SP,SM,U,E,D
C
      LZ=3-L1-(L2+1)/2
      GOTO (1,2,3,4),LZ
    1 Y= CR*E(P1)*U(P2)+CL*E(P2)*U(P1)
      GOTO 5
    2 Y= CL*SP(P1,P2)
      GOTO 5
    3 Y= CR*SM(P1,P2)
      GOTO 5
    4 Y= CL*E(P1)*U(P2)+CR*E(P2)*U(P1)
    5 RETURN
      END
