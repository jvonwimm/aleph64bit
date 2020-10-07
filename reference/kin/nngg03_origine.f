C-----------------------------------------------------------------------
C           A L E P H   I N S T A L L A T I O N    N O T E S           |
C                                                                      |
C    Aleph    name  : NNGG03.                                          |
C    original code  : NUNUBGG from M. Martinez, R. Miquel et al.       |
C    transmitted by : P. Gay May 1989.
C                                                                      |
C       (see KINLIB DOC for a description of this version)             |
C                                                                      |
C ----------------------------------------------------------------------
C Modifications to the original code (all the corrections included in
C the precedent version have been also included in the present one, but
C remnants of corrections of the previous versions were discarded) :
C
C    For original code structure see NNGG02 ORIGINE
C    The main modifications are concerning the abality to set a
C    more important number of parametres trought the CARDS.
C
C 1. See NNGG02 and NNGG01 for modifications .
C
C 2. The variable NHPR ( = PRINT HISTOGRAMS ) wasn't initialized and has
C    been removed from COMMON / OUTPUT /. In any case to save histograms
C    see KINRUN EXEC.
C
C 3. Again ..TONS...of blanck lines have been removed from the version
C    which has been transmitted!!!
C
C                                 G. Bonneaud, May 30th, 1989.
C
C ----------------------------------------------------------------------
C ==========================================================
C ==                                                      ==
C ==         E+ E- ----> NU NUB GAMMA (GAMMA)             ==
C ==             ( M.C. EVENT GENERATOR )                 ==
C ==                                                      ==
C ==========================================================
C ==                                                      ==
C == BASED ON                                             ==
C ==    "RADIATIVE CORRECTIONS TO THE PROCESS             ==
C ==        E+ E- ---> NU NUB GAMMA" // BERENDS,BURGERS,  ==
C ==        MANA,MARTINEZ & VAN NERVEEN // PREPRINT       ==
C ==        CERN-TH/87-4865 (1987) (SUBMITTED TO          ==
C ==        NUCL.PHYS.)                                   ==
C == ADDITIONNAL REFERENCES ARE                           ==
C ==     "QED RADIATIVE CORRECTIONS TO THE REACTION       ==
C ==        E+E- --> Z GAMMA" // BERENDS,BURGERS,VAN      ==
C ==        NEERVEN // PHYS.LETT. B177 (1986)             ==
C ==     "HARD PHOTON CORRECTIONS TO THE PROCESS          ==
C ==        E+E- -->NU NUBAR GAMMA" // MANA, MARTINEZ,    ==
C ==        CORNET  // DESY 86-114  PREPRINT SEPT 1986    ==
C == ORIGINAL MONTE CARLO GENERATOR WRITTEN BY            ==
C ==        C. MANA AND M. MARTINEZ                       ==
C ==                                                      ==
C ==                                                      ==
C == LONGITUDINAL BEAM POLARIZATION IMPLEMENTED BY        ==
C ==                                                      ==
C ==                  M. MARTINEZ & R. MIQUEL (JUL-88)    ==
C ==                                                      ==
C ==                                                      ==
C ==                                                      ==
C == 'STAR' TREATMENT FOR WEAK CORRECTIONS INCLUDED BY    ==
C ==                                                      ==
C ==                                R. MIQUEL (OCT-88)    ==
C ==                                                      ==
C ==  REF. : D. KENNEDY & B. LYNN, SLAC-PUB-4039 (1988)   ==
C ==                                                      ==
C ==                                                      ==
C ==                                                      ==
C == MODIFICATIONS TO THE 'STAR SCHEME' TO INCLUDE ALSO   ==
C == THE NON-ABELIAN VERTEX CORRECTIONS BY                ==
C ==                                                      ==
C ==                                R. MIQUEL (NOV-88)    ==
C ==                                                      ==
C ==                                                      ==
C ==                                                      ==
C == EXPONENTIATION OF QED CORRECTIONS INCLUDED BY        ==
C ==                                                      ==
C ==         C. MANA, M. MARTINEZ & R. MIQUEL (DEC-88)    ==
C ==                                                      ==
C ==========================================================
        SUBROUTINE GENTOT(CODE,NGAM,GAM1,GAM2)
C---------------------------------------------------------------------
C  P. Gay    Clermont-Fd
C            21 March 89
C            29 May   89 (modification to take in account new
C                         input parameters)
C
C based on the main programm structure with modifications
C
C
C   now, 3 part, controlled by the CODE variable which is
C      -  'INI'    "initia"
C      -  'STO'    "to generate an event"
C      -  'END'    "to close the generation and calculate X_section"
C
C  The event is given by
C       NGAM : # number of photon in the final state
C       GAM1(1,2,3,4) and GAM2(1,2,3,4) : the 4_vector of each photon
C               in the order P_x, P_y, P_z, E.
C----------------------------------------------------------------------
        IMPLICIT REAL*8(A-H,M,O-Z)
        REAL*4 RNDM,DUMY
        EXTERNAL RNDM
        COMMON /CMS/EB,CME,S,BE,BE2
     .       /NFAM/NF
     .       /HARSOF/DC
        COMMON /DETCUT/XDL,XDH,CD,XMH,CM,CR
        COMMON /SEEN/ NG,NSEEN
        COMMON /SOFT/SIG0,SIGQ,SIGZ
        COMMON /CSE/ ZS0,ZS,ZH,ZT
        COMMON /CONST/PI,DR,SR2,PB
        COMMON /OUTPUT/ NHSE,NHWE,NHEV,NHST,NREC
        COMMON /POL/ DPP,DPM
C...PG
        COMMON/INPOUT/LWRITE
        COMMON/PGVAL/PGMZ,PGMT,PGMH,PGHEVY
C  Some variables in simple precision
        REAL*4   GAM1(4),GAM2(4)
        REAL*4   CMEIN,EDCIN,DPPIN,DPMIN
        REAL*4   PGMZIN,PGMTIN,PGMHIN,PGHEIN
        REAL*4   XDHIN,EDLIN,ACDIN
        REAL*4   XMHIN,ACMIN
        REAL*4   WCUTIN,FIIN
        REAL*4   AQCDIN
        DIMENSION FIIN(2)
        CHARACTER*3 CODE
        INTEGER NTIN,NEVIN,NPIN,NFIN
C
        COMMON/DATARE/ CMEIN,EDCIN,DPPIN,DPMIN,
     .                 PGMZIN,PGMTIN,PGMHIN,PGHEIN,AQCDIN,
     .                 XDHIN,EDLIN,ACDIN,
     .                 XMHIN,ACMIN,WCUTIN,FIIN
        COMMON/DATAIN/ NTIN,NEVIN,NPIN,NFIN,NHWEIN,NHEVIN
        COMMON/DALESS/ IRANIN,JRANIN,NHPRIN
C... PG
C     To pass the kinematics
        COMMON /PLOTS3/ PH0
        COMMON /PLOTS1/ X0,CT0
        COMMON /PLOTS2/ X1,CT1,ST1,PH1,X2,CT2,ST2,PH2,CT12
C... To pass the alpha_strong value
        REAL*8 AQCD
        COMMON/ALFSTR/AQCD
C
        COMMON/VERSION/ TYPVER
        CHARACTER*7 TYPVER
        DIMENSION T(4),R(4)
        DIMENSION X(8),FI(2),NST(4)
C
C ==========================================================
C ==  INITIALIZATION
C ==========================================================
C
        DATA WS,WS2,WSMAX,EFFS/4*0.D0/
        DATA WH,WH2,WHMAX,EFFH/4*0.D0/
        DATA WS0,WS02,WSQ,WSZ/4*0.D0/
        DATA NS,NH,NT,NSA,NHA,NPAS,NPAH/7*0/
        DATA NST/4*0/
C...PG
C     init for output arguments of the subroutine
        DO I=1,4
           GAM1(I)=0.
           GAM2(I)=0.
        ENDDO
        NGAM=0
C... Choice the work to do
        IF(.NOT.((CODE.EQ.'INI').OR.(CODE.EQ.'STO').OR.
     .          (CODE.EQ.'END') ))
     .  THEN
        WRITE(LWRITE,501)
 501    FORMAT(2X,'---> P.Gay modif CODE value false in GENTOT ')
        STOP
        ENDIF
C... PG
C
        IF(CODE.EQ.'INI')THEN
        WRITE(LWRITE,998)
998     FORMAT('1    ',60('*')/
     . '          E+ E-  ---->  NU NUB GAMMA (GAMMA)  M.C. GENERATOR'/
     . '          BY C.MANA, M.MARTINEZ & R. MIQUEL (BARCELONA 1988)'/
     . '                       (VERSION DEC.88) '/
     . '     ',60('*')/)
C
C...PG
       IF(TYPVER.EQ.'NUCOUNT')WRITE(LWRITE,997)
 997   FORMAT(5X,'   WITH MODIFICATIONS TO SUIT IT       ',/,
     .        5X,'   TO ALEPH NEUTRINO COUNTING GROUP    ',/,
     .        5X,'                         P. GAY        ',/,
     .        5X,60('*'),/)
C
C.. A) PHYSICAL PARAMETERS:
C..CME = CENTER OF MASS ENERGY
C..NF  = # LIGHT NEUTRINO FAMILIES
C..NG  = # HARD PHOTONS DETECTED
C..EDC = HARD-SOFT ENERGY LIMIT
C..THE RELEVANT MASSES AND COUPLING CONSTANTS NEEDED
C.(CORRESPONDING TO THE "ON MASS SHELL" RENORMALIZATION SCHEME)
C..CAN BE MODIFIED IN 'SUBROUTINE PARAMS'
C
C.. DEGREE OF LONGITUDINAL POLARIZATION
C..       OF THE INITIAL STATE
C..         -1. < DP < 1.
C..  DPP (E+) = 1. IF (E+) IS LEFT HANDED
C..      (SPIN IN OPPOSITE DIRECTION TO REAL MOTION-TRIMOMENTUM-)
C..  DPP (E-) = 1. IF (E-) IS RIGHT HANDED
C..      (SPIN IN SAME DIRECTION TO REAL MOTION-TRIMOMENTUM-)
C..
C... PG
C     Equivalence between the DATA (simple precision)and the
C                             DATA (double precision)
C
                     CME   =    DBLE(CMEIN)
                     EDC   =    DBLE(EDCIN)
                     DPP   =    DBLE(DPPIN)
                     DPM   =    DBLE(DPMIN)
                     PGMZ  =    DBLE(PGMZIN)
                     PGMT  =    DBLE(PGMTIN)
                     PGMH  =    DBLE(PGMHIN)
                     PGHEVY=    DBLE(PGHEIN)
                     XDH   =    DBLE(XDHIN)
                     EDL   =    DBLE(EDLIN)
                     ACD   =    DBLE(ACDIN)
                     XMH   =    DBLE(XMHIN)
                     ACM   =    DBLE(ACMIN)
                     WCUT  =    DBLE(WCUTIN)
                     FI(1) =    DBLE(FIIN(1))
                     FI(2) =    DBLE(FIIN(2))
                     AQCD  =    DBLE(AQCDIN)
                     NF  = NFIN
                     NT  = NTIN
                     NEV = NEVIN
                     NP  = NPIN
C
C... End of the equivalence
C... Set the suitable values for the Neutrino Counting Group.
          IF(TYPVER.EQ.'NUCOUNT') THEN
            NG=2
            ACR=0.D0
          ENDIF
C...
C... Set some parameter, at least, to their minimal value
          IF(NP.LT.100)     NP=100
          IF(WCUT.LT.1.4D0) WCUT=1.4D0
          IF(XDH.GT.1.D0)  XDH=1.D0
          IF(XMH.GT.1.D0)  XMH=1.D0
C...
          CALL PARAMS
          DC = EDC/EB
C
C.. B) DETECTION CUTS
C.. DETECTED PHOTON
C..EDL = MINIMUM DETECTABLE PHOTON ENERGY
C..ACD = MINIMUM (SYMMETRIC) DETECTABLE PHOTON ANGLE (DEGREES)
C
            XDL = EDL/EB
            CD = DCOS(ACD*DR)
C.. MISSING PHOTON
C..ACM = MAXIMUM (SYMMETRIC) NON TAGGED PHOTON ANGLE (DEGREES)
C..ACR = TWO-PHOTON SEPARATION ANGLE (DEGREES)
C  XMH=EMH/EB
            CM = DCOS(ACM*DR)
C... PG
        IF(TYPVER.EQ.'ORIGINE') CR = DCOS(ACR*DR)
        IF(TYPVER.EQ.'NUCOUNT') CR =1.D0
C
        IF(TYPVER.EQ.'ORIGINE') THEN
        WRITE(LWRITE,999) CME,NG,EDC,CD,XDL,XDH,CM,XMH,CR
999     FORMAT('  =====>  INPUT PARAMETERS:'//
     . ' ==  A) GENERAL:'/
     . '  CME =',G10.4,' GEV  '/
     . '  # MAX.HARD GAMMA VIS.=',I2,
     . '  SOFT-HARD ENERGY LIMIT =',G10.4,' GEV'//
     . ' ==  B) DETECTION CUTS:'/'  DETECTED PHOTON: ',
     .      'ABS(CT) < ',G11.6,' AND  X = (',G10.4,' /',G10.4,')'/
     . '   MISSING PHOTON: ABS(CT) > ',G11.6,'  OR  X <  ',G10.4/
     . '     2 GAMMA RES.: ABS(CT) < ',G11.6/)
      ENDIF
        IF(TYPVER.EQ.'NUCOUNT')THEN
        EMH=XMH*EB
        EDH=XDH*EB
        ACDCOM=180.D0-ACD
        WRITE(LWRITE,996) CME,NG,EDC,EDL,EDH,XDL,XDH,ACDCOM,ACD,CD,
     .                    EMH,XMH,CM
996     FORMAT('  =====>  INPUT PARAMETERS:'//
     . ' ==  A) general:'/
     . '  CME =',G10.4,' GeV  '/
     . '  # max.hard gamma vis.=',I2,/,
     . '  soft-hard energy limit =',G10.4,' GeV'//
     . ' ==  B) detection cuts ',/,
     . '        (according ALEPH neutrino counting group request)',/,
     . '   at least a photon like : ',/,
     . '         ',G11.4,' < E_gamma < ',G11.4,' GeV ',/,
     . '          ( ',G11.6,' < X < ',G11.6,')',/,
     . '       ',G11.4,' < Theta_gamma < ',G11.4,' Degrees',/,
     . '      ( ABS(Theta_gamma) < ',G11.6,' )',/,
     . '   and a possible second photon ',/,
     . '                 E_gamma < ',G11.4,' GEV ',/,
     . '                     ( X < ',G11.6,')',/,
     . '        ABS(Theta_gam_2) < ',G11.6 )
       ENDIF
C
C.. C) GENERATION PARAMETERS
C..NT     = # TOTAL ALLOWED TRIALS
C..WCUT   = WEIGHT FACTOR TO BE SAFE IN THE REJECTION PROCEDURE
C..NEV    = # REQUESTED EVENTS
C..NP     = # POINTS IN THE INITIAL ESTIMATION OF THE RELATIVE IMPORTAN
C             OF THE TWO PROCESSES.
C..FI(I)  = IMPORTANCE FACTOR OF GENERATOR "I" (IF IS ZERO, THEN NO EVE
C             OF THAT KIND ARE PRODUCED).
        IF(DC.EQ.0.)FI(2) = 0.D0
C
        IF(TYPVER.EQ.'ORIGINE') THEN
        WRITE(LWRITE,1000) NP,NT,NEV,WCUT,FI(1),FI(2)
1000    FORMAT(' ==  C) GENERATION:'/
     . '   # ESTIMATION POINTS =',I8/
     . ' # MAX. ALLOWED POINTS =',I8/
     . '    # REQUESTED EVENTS =',I8/
     . '                 W_CUT =',G12.5/
     . '    IMPORTANCE FACTORS :'/
     . '         GENERATOR (1) =',G12.5,'  GENERATOR (2) =',G10.5///)
      ENDIF
C
C.. D) OUTPUT CONTROL PARAMETERS
C..NHWE   = BOOK WEIGHT HISTOGRAMS ? (=0 NO)
C..NHEV   = BOOK EVENT HISTOGRAMS ? (=0 NO)
        NHWE = NHWEIN
        NHEV = NHEVIN
C... PG
C     The storage of the histograms will be done in KINGAL
C                  Thus, NHST is always egal to 0
C     The storage of the events will be done in KINGAL
C                  Thus, NREC is always egal to 0
C     The Histograms self-energy are suppressed
        NHSE = 0
        NHST = 0
        NREC = 0
C =================================================================
C ==  X-SECTION ESTIMATION (USING "NP" POINTS FOR EACH SUBPROCESS)
C =================================================================
C
        DO 10 K=1,NP
        DO 20 I=1,8
C       X(I) = RNDM2(FLOAT(I))
        X(I) = RNDM (DUMY)
        IF(FI(2).EQ.0.D0.AND.I.GE.5)GO TO 21
20      CONTINUE
21      CONTINUE
        IF(FI(1).EQ.0.D0)GO TO 22
        W = VVG(X)
        WS = WS + W
        WS2 = WS2 + W*W
C
        WS0 = WS0 + SIG0
        WS02 = WS02 + SIG0*SIG0
        WSQ = WSQ + SIGQ
        WSZ = WSZ + SIGZ
C
        IF(W.GT.WSMAX) WSMAX = W
22      CONTINUE
        IF(FI(2).EQ.0.D0)GO TO 10
        W = VVGG(X)
        WH = WH + W
        WH2 = WH2 + W*W
        IF(W.GT.WHMAX) WHMAX = W
10      CONTINUE
C
        SL = FI(1)*WSMAX / (FI(1)*WSMAX + FI(2)*WHMAX)
        HL = 1.D0 - SL
C
        IF(SL.NE.0.) WTSMAX = WSMAX/SL
        IF(HL.NE.0.) WTHMAX = WHMAX/HL
        WTMAX = DMAX1(WTSMAX,WTHMAX)
        WTCUT = WTMAX * WCUT
C
        AS = WS / NP
        AH = WH / NP
        IF(WSMAX.NE.0.) EFFS = 100.D0 * AS/WSMAX
        IF(WHMAX.NE.0.) EFFH = 100.D0 * AH/WHMAX
C
        IF(TYPVER.EQ.'ORIGINE') THEN
        WRITE(LWRITE,1001)
     .  AS,AH,WSMAX,WHMAX,EFFS,EFFH,FI(1),FI(2),SL,HL
1001    FORMAT(
     .'  =====>  INITIAL ESTIMATIONS:......VVG..........VVGG...'
     . //23X,'X-SEC. = ',G12.5,'  ',G12.5,' PB'/
     . 23X,'W_MAX. = ',G12.5,'  ',G12.5/
     . 23X,'  EFF. = ',G12.4,'  ',G12.4,' %'/
     . 14X,'IMPORTANCE FAC. = ',G12.3,'  ',G12.3/
     . 14X,'  INITIAL PROB. = ',G12.5,'  ',G12.5///)
      ENDIF
           IF(NHWE.NE.0) THEN
        IF(SL.NE.0.)
     . CALL HBOOK1(1,' "VVG" WEIGHT DISTRIBUTION$',50,0.,0.,0.)
        IF(HL.NE.0.)
     . CALL HBOOK1(2,' "VVGG" WEIGHT DISTRIBUTION$',50,0.,0.,0.)
        IF(SL*HL.NE.0.)
     . CALL HBOOK1(3,' TOTAL WEIGHT DISTRIBUTION$',50,0.,0.,0.)
           ENDIF
C
C... PG
C    Reset KT
      KT=0
C... ENDIF corresponding to IF(CODE.EQ.'INI')
      ENDIF
      IF(CODE.EQ.'STO')THEN
C ==========================================================
C ==  EVENT GENERATION
C ==========================================================
C
C       DO 30 KT=1,NT
C... PG
 600    IFLAG=0
        KT=KT+1
C       IF(NEV.NE.0) WNOW = RNDM2(FLOAT(KT)) * WTCUT
        IF(NEV.NE.0) WNOW = RNDM (DUMY ) * WTCUT
        DO 32 I=1,5
C       X(I) = RNDM2(FLOAT(I))
        X(I) = RNDM (DUMY)
32      CONTINUE
C
C.. A) CHOOSING "VVG" OR "VVGG"
C       RL = RNDM2(0.)
        RL = RNDM (DUMY)
        IF(RL.GT.SL)GO TO 100
C
C.. B) GENERATING A "VVG" EVENT
        W = VVG(X)
        NS = NS + 1
        WS = WS + W
        WS2 = WS2 + W*W
        IF(W.GT.WSMAX) WSMAX = W
        IF(NHWE.NE.0.AND.W.NE.0.)CALL HFILL(1,REAL(W),0.,1.)
C
        WS0 = WS0 + SIG0
        WS02 = WS02 + SIG0*SIG0
        WSQ = WSQ + SIGQ
        WSZ = WSZ + SIGZ
C
        WW = 0.D0
        IF(SL.NE.0.D0) WW = W / SL
        IF(NEV.EQ.0.OR.WW.LT.WNOW) GO TO 200
        CALL ACCEPT(1)
C...PG
C            the two 4_vectors in double precision
        NGAM=1
        IFLAG=1
             DO 605,I=1,4
 605         R(I)=0.D0
             ST0=DSQRT(1.D0-CT0**2)
             T(1)=X0*ST0*DCOS(PH0)
             T(2)=X0*ST0*DSIN(PH0)
             T(3)=X0*CT0
             T(4)=X0
        DO 602,I=1,4
  602   T(I)=T(I)*EB
C... PG
        NSA = NSA + 1
        IF(WW.GT.WTCUT) NPAS = NPAS + 1
        GO TO 200
C
C.. C) GENERATING A "VVGG" EVENT
100     CONTINUE
        DO 34 I=6,8
C       X(I) = RNDM2(FLOAT(I))
        X(I) = RNDM (DUMY)
34      CONTINUE
        W = VVGG(X)
        NH = NH + 1
        WH = WH + W
        WH2 = WH2 + W*W
        IF(W.GT.WHMAX) WHMAX = W
        IF(NHWE.NE.0.AND.W.NE.0.D0)CALL HFILL(2,real(W),0.,1.)
C
        WW = 0.
        IF(HL.NE.0.D0) WW = W / HL
        IF(NEV.EQ.0.OR.WW.LT.WNOW) GO TO 200
        CALL ACCEPT(2)

C...PG
C            the two 4_vectors in double precision
        NGAM=2
        IFLAG=1
             T(1)=X1*ST1*DCOS(PH1)
             T(2)=X1*ST1*DSIN(PH1)
             T(3)=X1*CT1
             T(4)=X1
             R(1)=X2*ST2*DCOS(PH2)
             R(2)=X2*ST2*DSIN(PH2)
             R(3)=X2*CT2
             R(4)=X2
C
        DO 603,I=1,4
        R(I)=R(I)*EB
  603   T(I)=T(I)*EB
C
C... PG
        NHA = NHA + 1
        NST(NSEEN) = NST(NSEEN) + 1
        IF(WW.GT.WTCUT) NPAH = NPAH + 1
C
200     CONTINUE
        IF(WW.GT.WTMAX) WTMAX = WW
        IF(NHWE.NE.0.AND.WW.NE.0.D0.AND.SL*HL.NE.0.)
     .     CALL HFILL(3,real(WW),0.,1.)
        NTA = NSA + NHA
C...PG
        IF(IFLAG.EQ.0)GOTO 600
C            the two 4_vectors in simple precision
        DO 5,I=1,4
        GAM1(I)=SNGL(T(I))
 5      GAM2(I)=SNGL(R(I))
C... PG
C     ENDIF corresponding to IF(CODE.EQ.'STO')
        ENDIF
        IF(CODE.EQ.'END')THEN
C ==========================================================
C ==  STATISTICS
C ==========================================================
300     CONTINUE
        WRITE(LWRITE,1010)
1010    FORMAT('1 ',52('*')/
     . '  ',18('*'),'>    RESULTS   <',18('*')/'  ',52('*')///)
        NT = KT
C.. A) "VVG" CONTRIBUTION
        IF(FI(1).EQ.0.) GO TO 50
        NSP = NS + NP
        AS = WS / NSP
        ZS = AS / NSA
        SIGS = DSQRT((WS2/NSP - AS*AS)/NSP)
        EFFA = 0.D0
        IF(NS.NE.0) EFFA = 100.D0 * NSA/NS
        EFF = 0.D0
        IF(WSMAX.NE.0.D0) EFF = 100.D0 * AS/WSMAX
C
        AS0 = WS0 / NSP
        ZS0 = AS0 / NSA
        SIGS0 = DSQRT((WS02/NSP - AS0*AS0)/NSP)
        DSQ = 100.D0 * WSQ/WS0
        DSZ = 100.D0 * (WSZ/WS0-1.D0)
        IF(WSZ.EQ.0.D0)DSZ = 0.D0
        WRITE(LWRITE,1002)
1002    FORMAT('  ',18('-'),'>      VVG     <',18('-')/)
C... PG
C       WRITE(LWRITE,1012) NSP,AS0,SIGS0,DSQ,DSZ,
        WRITE(LWRITE,1012) NSP,AS0,SIGS0,
     .  AS,SIGS,NS,WSMAX,EFF,NSA,EFFA
1012    FORMAT(' =====> INTEGRATION:          # POINTS =',I8/
     . '  LOW.ORDER X-SEC. =',G12.5,' +/- ',G12.5,' PB'//
C... RM & PG
C    . '  SOFT+VIRT.Q.E.D CORR. ==> DELTA =',G11.3,' %'/
C    . '  Z0 SELF ENERGY CORR.  ==> DELTA =',G11.3,' %'//
     . '            X-SEC. =',G12.5,' +/- ',G12.5,' PB'//
     .       ' =====> GENERATION :          # TRIALS =',I8/
     .'     W_MAX =',G12.5,'MAX.THEO.EFF. =',G10.4,' %'/
     . '  # EVENTS =',I8,'        REAL EFF. =',G10.4,' %'///)
C
C.. B) "VVGG" CONTRIBUTION
50      CONTINUE
        IF(FI(2).EQ.0.D0) GO TO 51
        NHP = NH + NP
        AH = WH / NHP
        ZH = AH / NHA
        SIGH = DSQRT((WH2/NHP - AH*AH)/NHP)
        EFFA = 0.D0
        IF(NH.NE.0) EFFA = 100.D0 * NHA/NH
        EFF = 0.D0
        IF(WHMAX.NE.0.) EFF = 100.D0 * AH/WHMAX
        NST(2) = NST(2) + NST(1)
        WRITE(LWRITE,1003)
1003    FORMAT('  ',18('-'),'>     VVGG     <',18('-')/)
        IF(TYPVER.EQ.'ORIGINE') THEN
        WRITE(LWRITE,1013)
     .  NHP,AH,SIGH,NH,WHMAX,EFF,NHA,EFFA,(NST(J),J=2,4)
1013    FORMAT(' =====> INTEGRATION:          # POINTS =',I8//
     . '            X-SEC. =',G12.5,' +/- ',G12.5,' PB'//
     .       ' =====> GENERATION :          # TRIALS =',I8/
     . '     W_MAX =',G12.5,'MAX.THEO.EFF. =',G10.4,' %'/
     . '  # EVENTS =',I8,'        REAL EFF. =',G10.4,' %'/
     . '  # EVENTS (1 G SEEN) =',I8/'  # EVENTS (2 G COLL.)=',I8/
     . '  # EVENTS (2 G SEEN) =',I8///)
        ENDIF
       IF(TYPVER.EQ.'NUCOUNT')THEN
        WRITE(LWRITE,5013)
     .     NHP,AH,SIGH,NH,WHMAX,EFF,NHA,EFFA,NST(2)
5013    FORMAT(' =====> INTEGRATION:          # POINTS =',I8//
     . '            X-SEC. =',G12.5,' +/- ',G12.5,' PB'//
     .       ' =====> GENERATION :          # TRIALS =',I8/
     . '     W_MAX =',G12.5,'MAX.THEO.EFF. =',G10.4,' %'/
     . '  # EVENTS =',I8,'        REAL EFF. =',G10.4,' %'/
     . '  # EVENTS (1 G SEEN at least) =',I8////)
       ENDIF
C
C.. C) TOTAL
51      CONTINUE
        AT = AS + AH
        ZT = AT / NTA
        SIG = DSQRT(SIGS**2+SIGH**2)
        NTP = NSP + NHP
        EFFA = 0.D0
        IF(NT.NE.0) EFFA = 100.D0 * NTA/NT
        EFF = 0.D0
        IF(WTMAX.NE.0.D0) EFF = 100.D0 * AT/WTMAX
        WRITE(LWRITE,1004)
1004    FORMAT('  ',18('='),'>    TOTAL     <',18('=')/)
        WRITE(LWRITE,1014)
     .  NTP,AT,SIG,NT,WTMAX,EFF,WTCUT,NPAS,NPAH,NTA,EFFA
1014    FORMAT(
     .' =====> INTEGRATION:          # POINTS =',I8//12X,40('-')/
     .'            X-SEC. =',G12.5,' +/- ',G12.5,' PB'/12X,40('-')//
     .' =====> GENERATION :          # TRIALS =',I8/
     . '     W_MAX =',G12.5,'MAX.THEO.EFF. =',G10.4,' %'/
     . '     W_CUT =',G12.5/
     . '  # OVERFLOW: SOFT =',I6,'  HARD =',I6/
     . '  # EVENTS =',I8,'        REAL EFF. =',G10.4,' %'///)
C
        CALL ACCEPT(0)
C... PG
C      ENDIF corresponding to IF(CODE.EQ.'END')
        ENDIF
        RETURN
        END
        SUBROUTINE PARAMS
C............................................
C   INITIALIZATION OF THE MAIN CONSTANTS
C             AND PARAMETERS
C............................................
        IMPLICIT REAL*8(A-H,M,O-Z)
        COMMON /CONST/PI,DR,SR2,PB
        COMMON /CMS/EB,CME,S,BE,BE2
     .       /NFAM/NF
     .       /HARSOF/DC
        COMMON /BOS1/MZ,GZ,MW,GW,MH
     .       /BOS2/MZ2,MW2,MH2
     .       /LEPT1/ME,ME2
     .       /LEPT2/MMU,MTAU,MHEAVY
     .       /HAD/MU,MD,MS,MC,MB,MT
        COMMON /WEAK/SW2,CW2,A2,V2,VU2,VD2
     .       /QED/ALF,ALF2,ALDPI
     .       /FERMI/GF
      COMMON/FERMAS/NLPT,NQRK,XLPT,XQRK
      REAL*8 XLPT(40,2),XQRK(40,2)
      COMMON/PRMGSW/ALFA,GMU,MMW,MMZ,MMH
C...PG
        COMMON/INPOUT/LWRITE
        COMMON/PGVAL/PGMZ,PGMT,PGMH,PGHEVY
C
C---CONST---------------------------------------
        PI = DACOS(-1.D0)
        DR = PI/180.D0
        SR2 = DSQRT(2.D0)
        PB = 389386.D3
C
C---QED-----------------------------------------
C QED COUPLING CONSTANT IN THE THOMSON LIMIT
        ALF = 1.D0/137.036D0
        ALF2 = ALF*ALF
        ALDPI = ALF/PI
C
C---FERMI---------------------------------------
C GF IS THE FERMI COUPLING CONSTANT
        GF = 1.16637D-5
C
C---LEPT1 LEPT2----------------------------------
C       READ (15,105) MZ,MT,MH,MHEAVY

C... PG
C     MZ MT MH MHEAVY are passed through a COMMON and the values
C                         are given in main program part
              MZ=PGMZ
              MT=PGMT
              MH=PGMH
              MHEAVY=PGHEVY
C
        ME = .000511D0
        ME2 = ME*ME
        MMU = .10565943D0
        MTAU = 1.7842D0
C NEW HEAVY CHARGED LEPTONS MASS
C (CORRESPONDING TO THE NEW NEUTRINO
C  FAMILIES):
CC        MHEAVY = 100.D0
C
C---HAD------------------------------------------
C THE FOLLOWING MASSES SHOULDN'T BE MODIFIED
C BECAUSE HAVE BEEN OBTAINED FROM THE BEST FIT
C OF HADRONIC DATA :
        MU = .032D0
        MD = .0321D0
        MS = .15D0
        MC = 1.5D0
        MB = 4.5D0
C TOP MASS (MAKE YOUR BET !):
CC        MT = 40.D0
C
C---CMS-------------------------------------------
        EB = CME/2.D0
        S = CME*CME
        BE2 = 1.D0-(ME/EB)**2
        BE = DSQRT(BE2)
C
C---BOS1 BOS2-----------------------------------
C Z0 MASS
CC        MZ = 93.D0
        MMZ= MZ
        MZ2 = MZ*MZ
C HIGGS MASS (MAKE YOUR BET !):
CC        MH = 100.D0
        MMH= MH
        MH2 = MH*MH
C W MASS
CC-RM        MW = WMASS(DUMMY)
        ALFA=1.D0/137.03602D0
        GMU=1.16632D-5
        NLPT=NF*2
        NQRK=6
        QUP=2.D0/3.D0
        QDOWN=-1.D0/3.D0
C  ELECTRON NEUTRINO MASS AND CHARGE
        XLPT(1,1) = 0.D0
        XLPT(1,2) = 0.D0
C  ELECTRON MASS AND CHARGE
        XLPT(2,1) = ME
        XLPT(2,2) = -1.D0
C  MUON NEUTRINO MASS AND CHARGE
        XLPT(3,1) = 0.D0
        XLPT(3,2) = 0.D0
C  MUON MASS AND CHARGE
        XLPT(4,1) = MMU
        XLPT(4,2) = -1.D0
C  TAU NEUTRINO MASS AND CHARGE
        XLPT(5,1) = 0.D0
        XLPT(5,2) = 0.D0
C  TAU MASS AND CHARGE
        XLPT(6,1) = MTAU
        XLPT(6,2) = -1.D0
C  UP QUARK MASS AND CHARGE
        XQRK(1,1) = MU
        XQRK(1,2) = QUP
C  DOWN QUARK MASS AND CHARGE
        XQRK(2,1) = MD
        XQRK(2,2) = QDOWN
C  CHARMED QUARK MASS AND CHARGE
        XQRK(3,1) = MC
        XQRK(3,2) = QUP
C  STRANGE QUARK MASS AND CHARGE
        XQRK(4,1) = MS
        XQRK(4,2) = QDOWN
C  TOP QUARK MASS AND CHARGE
        XQRK(5,1) = MT
        XQRK(5,2) = QUP
C  BOTTOM QUARK MASS AND CHARGE
        XQRK(6,1) = MD
        XQRK(6,2) = QDOWN
C  NEW FAMILIES
        IF (NF.GT.40) THEN
            WRITE (LWRITE,*) 'TOO MANY FAMILIES!! : NF = ', NF
            STOP
        ENDIF
        IF (NF.GT.3) THEN
            DO 12 I=1,NF-3
                  XLPT(5+2*I,1) = 0.D0
                  XLPT(5+2*I,2) = 0.D0
                  XLPT(6+2*I,1) = MHEAVY
                  XLPT(6+2*I,2) = -1.D0
 12         CONTINUE
        ENDIF
        CALL WMASET
        MW=MMW
        MW2 = MW*MW
C
C---WEAK----------------------------------------
C SIN(THETA_W)**2 DEFINED AS A BOOKKEPING
C PARAMETER: SW2 = 1 - (MW/MZ)**2
        SW2 = 1.D0 - (MW/MZ)**2
        CW2 = 1.D0 - SW2
        A2 = 1.D0/(16.D0*SW2*CW2)
        V2 = A2 * (1.D0-4.D0*SW2)**2
        VU2 = A2 * (1.D0-8.D0/3.D0*SW2)**2
        VD2 = A2 * (1.D0-4.D0/3.D0*SW2)**2
C
C VECTOR BOSON WIDTH
C... TREE LEVEL WIDTHS
C...(USED ONLY TO BUILD THE APPROXIMANTS
C... SINCE THE PROPER VALUES ARE COMPUTED
C... IN THE CALCULATION)
        CALL ZWDTH0(GAZ)
        GZ = GAZ
        GW = 2.6D0
C
        WRITE(LWRITE,100)NF,MZ,MH,MT,MHEAVY
100     FORMAT('  =====>  INPUT RELEVANT PHYSICAL CONSTANTS:'//
     . '   LEPTON FAMILIES     = ',I5/
     . '   M_Z0                = ',G12.5,' GEV'/
     . '   M_HIGGS             = ',G12.5,' GEV'/
     . '   M_TOP               = ',G12.5,' GEV'/
     . '   M_HEAVY LEPTON      = ',G12.5,' GEV'//)
        WRITE(LWRITE,101)MW,SW2,GZ
101     FORMAT('  =====>  COMPUTED PHYSICAL CONSTANTS:'//
     . '                  M_W                 = ',G12.5,' GEV'/
     . '   SIN(THETA_W)**2 = 1 - (M_w/M_z)**2 = ',G12.5/
     . '                  TREE LEVEL Z0_WITDH = ',G12.5,' GEV'///)
        RETURN
        END
        SUBROUTINE ACCEPT(N)
C....................................
C   PLOTTING AND STORING ROUTINE
C....................................
        IMPLICIT REAL*8(A-H,M,O-Z)
        REAL*4 RNDM,DUMY
        EXTERNAL RNDM
        COMMON /SEEN/ NG,NSEEN
        COMMON /SOFT/SIG0,SIGQ,SIGZ
        COMMON /CSE/ ZS0,ZS,ZH,ZT
        COMMON /CONST/ PI,DR,SR2,PB
        COMMON /PLOTS3/ PH0
        COMMON /PLOTS1/ X0,CT0
        COMMON /PLOTS2/ X1,CT1,ST1,PH1,X2,CT2,ST2,PH2,CT12
        COMMON /OUTPUT/ NHSE,NHWE,NHEV,NHST,NREC
C...PG
        COMMON/INPOUT/LWRITE
        COMMON/VERSION/ TYPVER
        CHARACTER*7 TYPVER
C--------------------------------------
C   BOOKING THE HISTOGRAMS
C--------------------------------------
C
        DATA INIT/0/
        DATA XMI,XMA,NBX/0.D0,1.D0,50/
        DATA CTMI,CTMA,NBCT/-1.D0,1.D0,40/
        DATA PTMI,PTMA,NBPT/0.D0,1.D0,50/
        IF(INIT.NE.0)GO TO 101
        INIT=1
          IF(NHEV.NE.0)THEN
        DX = (XMA-XMI)/NBX
        DCT = (CTMA-CTMI)/NBCT
        DPT = (PTMA-PTMI)/NBPT
        CALL HBOOK1(1001,' D(SIGMA_0)/ D(ENERGY/EBEAM)$',50,0.,1.,0.)
        CALL HBOOK1(1002,' D(SIGMA_0)/ D(COS(THE))$',40,-1.,1.,0.)
        CALL HBOOK1(1003,' D(SIGMA_0)/ D(PT)$',50,0.,1.,0.)
CBB add missing argument
        CALL HBOOK1(1011,' D(SIGMA_VVG)/ D(ENERGY/EBEAM)$',50,0.,1.,0.)
        CALL HBOOK1(1012,' D(SIGMA_VVG)/ D(COS(THE))$',40,-1.,1.,0.)
        CALL HBOOK1(1013,' D(SIGMA_VVG)/ D(PT)$',50,0.,1.,0.)
C...PG
       IF(TYPVER.EQ.'NUCOUNT')THEN
        CALL HBOOK1(1051,' D(SIGMA_VVGG)/ D(ENERGY_2/EBEAM)$'
     .                   ,50,0.,1.,0.)
        CALL HBOOK1(1052,' D(SIGMA_VVGG)/ D(COS(THE_2))$',40,-1.,1.,0.)
        CALL HBOOK1(1053,' D(SIGMA_VVGG)/ D(PT_2)$',50,0.,1.,0.)
       ENDIF
        CALL HBOOK1(1021,' D(SIGMA_VVGG)/ D(ENERGY/EBEAM)$'
     .   ,50,0.,1.,0.)
        CALL HBOOK1(1022,' D(SIGMA_VVGG)/ D(COS(THE))$',40,-1.,1.,0.)
        CALL HBOOK1(1023,' D(SIGMA_VVGG)/ D(PT)$',50,0.,1.,0.)
        CALL HBOOK1(1031,' D(SIGMA_TOT)/ D(ENERGY/EBEAM)$',50,0.,1.,0.)
        CALL HBOOK1(1032,' D(SIGMA_TOT)/ D(COS(THE))$',40,-1.,1.,0.)
        CALL HBOOK1(1033,' D(SIGMA_TOT)/ D(PT)$',50,0.,1.,0.)
        CALL HBOOK1(1040,' COS. BETWEEN PHOTONS -VVGG- $',50,-1.,1.,0.)
        CALL HIDOPT(0,'STAT')
            ENDIF
101     CONTINUE
C--------------------------------------
C   FILLING HISTOGRAMS
C   AND STORING EVENTS ON UNIT #21
C--------------------------------------
C
        IF(N.NE.1)GO TO 10
C
C VVG EVENT
         IF((SIGQ+SIGZ).EQ.0.D0)THEN
             W0 = 1
           ELSE
             W0 = SIG0/(SIGQ+SIGZ)
         ENDIF
C       PH0 = 2.*PI*RNDM2(0.)
        PH0 = 2.*PI*RNDM (DUMY)
       IF(NREC.NE.0) WRITE(21) N,X0,CT0,PH0
        XD = X0
        CTD = CT0
        PTD= XD * DSQRT(1.D0 - CTD*CTD)
          IF(NHEV.NE.0)THEN
        CALL HFILL(1001,REAL(XD),0.,real(W0))
        CALL HFILL(1002,REAL(CTD),0.,real(W0))
        CALL HFILL(1003,REAL(PTD),0.,real(W0))
        CALL HFILL(1011,REAL(XD),0.,1.)
        CALL HFILL(1012,REAL(CTD),0.,1.)
        CALL HFILL(1013,REAL(PTD),0.,1.)
          ENDIF
10      IF(N.NE.2)GO TO 20
C
C VVGG EVENT
       IF(NREC.NE.0) WRITE(21) N,X1,CT1,PH1,X2,CT2,PH2
      IF(TYPVER.EQ.'ORIGINE') THEN
        IF(NSEEN.NE.1)GO TO 21
        XD = X1
        CTD = CT1
21      IF(NSEEN.NE.2)GO TO 22
        XD = X2
        CTD = CT2
22      IF(NSEEN.NE.3)GO TO 23
        XD = X1 + X2
        CTD = CT1
23      IF(NSEEN.NE.4)GO TO 24
        XD = X1 + X2
        CTD = (CT1*X1+CT2*X2)/(X1+X2)
24      CONTINUE
        PTD= XD * DSQRT(1.D0 - CTD*CTD)
           IF(NHEV.NE.0)THEN
        CALL HFILL(1021,REAL(XD),0.,1.)
        CALL HFILL(1022,REAL(CTD),0.,1.)
        CALL HFILL(1023,REAL(PTD),0.,1.)
           ENDIF
C...PG
      ENDIF
      IF(TYPVER.EQ.'NUCOUNT') THEN
        IF(NSEEN.EQ.1) THEN
        XD = X1
        CTD = CT1
        XD2= X2
        CTD2= CT2
        ENDIF
        IF(NSEEN.EQ.2) THEN
        XD = X2
        CTD = CT2
        XD2= X1
        CTD2= CT1
        ENDIF
        PTD= XD * DSQRT(1.D0 - CTD*CTD)
        PTD2= XD2 * DSQRT(1.D0 - CTD2*CTD2)
           IF(NHEV.NE.0)THEN
        CALL HFILL(1021,REAL(XD),0.,1.)
        CALL HFILL(1022,REAL(CTD),0.,1.)
        CALL HFILL(1023,REAL(PTD),0.,1.)
        CALL HFILL(1051,REAL(XD2),0.,1.)
        CALL HFILL(1052,REAL(CTD2),0.,1.)
        CALL HFILL(1053,REAL(PTD2),0.,1.)
           ENDIF
      ENDIF
C...PG end
20      IF(N.EQ.0)GO TO 30
           IF(NHEV.NE.0)THEN
        CALL HFILL(1031,REAL(XD),0.,1.)
        CALL HFILL(1032,REAL(CTD),0.,1.)
        CALL HFILL(1033,REAL(PTD),0.,1.)
         IF(N.EQ.2) CALL HFILL(1040,real(CT12),0.,1.)
           ENDIF
        RETURN
30      CONTINUE
           IF(NHEV.NE.0)THEN
        CALL HOPERA(1001,'+   ',1001,1001,REAL(ZS/DX),0.)
        CALL HOPERA(1002,'+   ',1002,1002,REAL(ZS/DCT),0.)
        CALL HOPERA(1003,'+   ',1003,1003,REAL(ZS/DPT),0.)
        CALL HOPERA(1011,'+   ',1011,1011,REAL(ZS/DX),0.)
        CALL HOPERA(1012,'+   ',1012,1012,REAL(ZS/DCT),0.)
        CALL HOPERA(1013,'+   ',1013,1013,REAL(ZS/DPT),0.)
        CALL HOPERA(1021,'+   ',1021,1021,REAL(ZH/DX),0.)
        CALL HOPERA(1022,'+   ',1022,1022,REAL(ZH/DCT),0.)
        CALL HOPERA(1023,'+   ',1023,1023,REAL(ZH/DPT),0.)
C... PG
       IF(TYPVER.EQ.'NUCOUNT')THEN
        CALL HOPERA(1051,'+   ',1051,1051,real(ZH/DX),0.)
        CALL HOPERA(1052,'+   ',1052,1052,real(ZH/DCT),0.)
        CALL HOPERA(1053,'+   ',1053,1053,real(ZH/DPT),0.)
       ENDIF
        CALL HOPERA(1031,'+   ',1031,1031,real(ZT/DX),0.)
        CALL HOPERA(1032,'+   ',1032,1032,real(ZT/DCT),0.)
        CALL HOPERA(1033,'+   ',1033,1033,real(ZT/DPT),0.)
           ENDIF
        END
      DOUBLE PRECISION FUNCTION VVG(X)
C......................................................
C     CROSS SECTION FOR E+ E- ---> NU NUB GAMMA
C   (INCLUDING THE MAIN VIRTUAL+SOFT CORRECTIONS)
C......................................................
      IMPLICIT REAL*8(A-H,M,O-Z)
        REAL*4 RNDM,DUMY
        EXTERNAL RNDM
      REAL*8 KP,KM,KPP,KMP
      COMPLEX*16 W,WP,Z,II,GVST
      COMMON /CMS/EB,CME,S,BE,BE2
     .       /NFAM/NF
     .       /HARSOF/DC
      COMMON /BOS1/MZ,GZ,MW,GW,MH
     .       /BOS2/MZ2,MW2,MH2
     .       /LEPT1/ME,ME2
      COMMON /WEAK/SW2,CW2,A2,V2,VU2,VD2
     .       /QED/ALF,ALF2,ALDPI
      COMMON /DETCUT/ZDL,ZDH,YD,XMH,CM,CR
      COMMON /SOFT/SIG0,SIGQ,SIGZ
      COMMON /CONST/PI,DR,SR2,PB
      COMMON /PLOTS1/ ZZ,Y
      COMMON /POL/ DPP,DPM
C...PG
        COMMON/INPOUT/LWRITE
      DIMENSION X(5),ZJ(3),IU(2)
      DATA INIT/0/
      IF(INIT.NE.0) GO TO 100
C--------------------------------------------------
C         INITIALIZATION
C--------------------------------------------------
C
C  DEFINE BASIC CONSTANTS
      INIT = 1
CC-RM
      ROOT2 = DSQRT(2.D0)
      II    = DCMPLX(0.D0,1.D0)
      DUMMY = E2TAB(DUM)
      DUMMY = GMUTAB(DUM)
      DUMMY = RHOTAB(DUM)
      DUMMY =-MPAATB(DUM)
      DUMMY = S2TAB(DUM)
      DUMMY =-MPZATB(DUM)
      DUMMY = GAMTAB(DUM)
CC-RM
      GA = -.5D0
      GV = -.5D0 + 2.D0*SW2
      FAMS  = ALF**3/(8.D0*SW2**2)
C
      AM = 1.D0-MZ2/S
      BM = MZ*GZ/S
      AM2 = AM**2
      BM2 = BM**2
      ZF=1./(2.*BM*(AM2+BM2))
      ZDLMA=(ZDH-AM)**2+BM2
      ZDLMI=(ZDL-AM)**2+BM2
      ATANMA=DATAN((AM-ZDH)/BM)
      ATANMI=DATAN((AM-ZDL)/BM)
C
C   JACOBIANS:
C   COS(THETA_GAMMA)
      YJC=DLOG((BE*YD+1.D0)/(1.D0-BE*YD))/BE
C   X_GAMMA
      ZJ(1)=ZF* 2.D0*(AM2+BM2-AM)*(ATANMA-ATANMI)
      ZJ(2)=ZF* 2.D0*BM*DLOG(ZDH/ZDL)
      ZJ(3)=-ZF* BM*DLOG( ((ZDH-AM)**2+BM2)/((ZDL-AM)**2+BM2) )
      ZJC=0.D0
      DO 1 I=1,2
      IU(I)=1
      IF(ZJ(I).LE.0.)IU(I)=0
      ZJC=ZJC+ZJ(I)
1     CONTINUE
      XJ1=ZJ(1)/ZJC
      ZJC=ZJC+ZJ(3)
100   CONTINUE
C--------------------------------------------------
C         PHOTON MAPPING
C--------------------------------------------------
C
C--- 1/(1-BE**2*COS**2) MAPPING
        ZLA=((BE*YD+1.D0)/(1.D0-BE*YD))**(2.D0*X(1)-1.D0)
        Y=(ZLA-1.D0)/(BE*(ZLA+1.D0))
        CT = Y
        ST = DSQRT(1.D0-CT**2)
        YWE=YJC* (1.D0-BE2*Y*Y)
C
C--- PHOTON SPECTRUM MAPPING
303     CONTINUE
C       ETA = RNDM2(0.)
C       ETAR = RNDM2(1.)
        ETA = RNDM (DUMY)
        ETAR = RNDM (DUMY)
        IF(ETA.LE.XJ1)
     .      ZZ = AM - BM * DTAN( ATANMI + X(2)*(ATANMA-ATANMI) )
        IF(ETA.GT.XJ1) ZZ=ZDL*(ZDH/ZDL)**X(2)
        ZWE= (AM2+BM2)/(IU(1)*(AM-AM2-BM2)/
     .     ((ZZ-AM)**2+BM2)+IU(2)/ZZ)
        ZWEC= ZZ*((ZZ-AM)**2+BM2)/(1.D0-ZZ)
        DRAT=ZWE/ZWEC
        DATA SFA/1.5D0/
        IF(DRAT.GT.SFA)WRITE(LWRITE,*)'  WARNING ! : DRAT =',DRAT
          IF(DRAT.LT.ETAR*SFA)THEN
C       X(2) = RNDM2(2.)
        X(2) = RNDM (DUMY)
        GO TO 303
          ENDIF
      ZWE=ZJC*ZWEC
C
      PH   = 2*PI*X(3)
      CP  = DCOS(PH)
      SP  = DSIN(PH)
C
C   GENERATING THE NEUTRINO VARIABLES
C
      CTN  = -1.D0+2.D0*X(4)
      STN  = DSQRT(1.D0-CTN**2)
      PHN  = 2*PI*X(5)
      CPN  = DCOS(PHN)
      SPN  = DSIN(PHN)
CC=============================
CC  COMPUTING THE REST OF RELEVANT
CC  VARIABLES
CC=============================
      CTGN = ST*STN*CP+CT*CTN
      XN   = 2.D0*(1.D0-ZZ)/(2.D0+ZZ*(CTGN-1.D0))
      XP   = 2.-ZZ-XN
CC=============================
CC  CHECKING THE KINEMATICS
CC=============================
      PGX  = ZZ*ST*CP
      PGY  = ZZ*ST*SP
      PGZ  = ZZ*CT
      PNX  = XN*STN
      PNZ  = XN*CTN
      PP   = DSQRT( (PGX+PNX)**2 + PGY**2 + (PGZ+PNZ)**2 )
      DIFF = 100.D0*DABS(XP-PP)/DMAX1(XP,PP)
      IF(DIFF.GT.1.D-3) WRITE(LWRITE,235)XP,PP,DIFF
 235  FORMAT(' WARNING !!! IN "VVG" BAD KINEMATICS :'/
     . 6X,' XP= ',G12.5,' PP= ',G12.5,'  DIFF %= ',G12.5)
CC....
      CTP=0.
      IF(XP.NE.0.)CTP=(-PGZ-PNZ)/XP
      IF(CTP.LT.-1..OR.CTP.GT.1.) WRITE(LWRITE,231)
 231  FORMAT(' WARNING !!! IN "VVG" CTP OUT OF RANGE')
      IF(CTP.GT.1.)CTP=1.
      IF(CTP.LT.-1.)CTP=-1.
      STP=DSQRT(1.-CTP**2)
CC=============================
CC  COMPUTING THE SCALAR PRODUCTS
CC=============================
      EB2 = EB**2
      P12 = (S-2.D0*ME2)/2.D0
      P13 = EB2*ZZ*(1.-BE*CT)
      P23 = EB2*ZZ*(1.+BE*CT)
      P14 = EB2*XN*(1.-BE*CTN)
      P24 = EB2*XN*(1.+BE*CTN)
      P34 = EB2*ZZ*XN*(1.-CTGN)
      P15 = ME2+P12-P13-P14
      P25 = P12+ME2-P23-P24
      P35 = P13+P23-P34
      P45 = P15+P25-P35
      EPS = -2.*EB**4*BE*PNX*PGY
CC=============================
CC  COMPUTING SOME USEFULL INVARIANTS
CC=============================
      SP = 2.D0*P45
      T  = ME2-2.*P14
      U  = ME2-2.*P15
      TP = ME2-2.*P25
      UP = ME2-2.*P24
      KP = 2.D0*P13
      KM = 2.D0*P23
      KPP= 2.D0*P34
      KMP= 2.D0*P35
C
C  NON-CORRECTED PROPAGATORS
C
C W'S
      W  = DCMPLX(T-MW2,0.D0)
      W2 = W*DCONJG(W)
      WP = DCMPLX(TP-MW2,0.D0)
      WP2= WP*DCONJG(WP)
C Z'S
      Z  = DCMPLX(SP-MZ2,MZ*GZ)
      Z2 = Z*DCONJG(Z)
C-------------------------------------------------------------------
C   LOWEST ORDER CROSS SECTION FOR NU-NUBAR-GAMMA
C-------------------------------------------------------------------
C
      FBMS  = ZZ*XN**2/(1.D0-ZZ) * SP/(KP*KM)
C  Z0
      SZMP  = (GV-GA)**2*  (T**2+TP**2)/(2.D0*CW2**2*Z2)
      SZPM  = (GV+GA)**2*  (U**2+UP**2)/(2.D0*CW2**2*Z2)
      SGZT0 = ( SZMP*(1.D0+DPP)*(1.D0+DPM) +
     .          SZPM*(1.D0-DPP)*(1.D0-DPM) )/4.D0
C  W
      SWPM  = 2.D0/(W2*WP2) * ( U *
     .(U*WP2-TP*KPP*KM-(SP*KM-TP*KPP+UP*KMP)*DREAL(WP)-
     .  4.D0*EPS*DIMAG(WP))   + UP *
     .(UP*W2-T*KMP*KP-(SP*KP-T*KMP+U*KPP)*DREAL(W)+
     .  4.D0*EPS*DIMAG(W) ) )
C-BB      SWPM  = 2.D0/(W2*WP2) * ( U**2 *
C-BB     .( WP2-TP*KPP*KM-(SP*KM-TP*KPP+UP*KMP)*DREAL(WP)-
C-BB     .  4.D0*EPS*DIMAG(WP))   + UP**2 *
C-BB     .( W2-T*KMP*KP-(SP*KP-T*KMP+U*KPP)*DREAL(W)+
C-BB     .  4.D0*EPS*DIMAG(W) ) )
      SGWT0 = SWPM*(1.D0-DPP)*(1.D0-DPM) /4.D0
C  W-Z0
      SWZPM = (GV+GA)/CW2* ( DREAL( U/(DCONJG(Z)*W) *
     .        (2.D0*U - DCMPLX(SP*KM-TP*KPP+UP*KMP,-4.D0*EPS)/WP) )
     .                     + DREAL( UP/(DCONJG(Z)*WP) *
     .        (2.D0*UP - DCMPLX(SP*KP-T*KMP+U*KP,4.D0*EPS)/W) ) )
C-BB      SWZPM = (GV+GA)/CW2* ( U**2 *
C-BB     .( DREAL( 1.D0/(DCONJG(Z)*W) *
C-BB     .        (2.D0 - DCMPLX(SP*KM-TP*KPP+UP*KMP,-4.D0*EPS)/WP) ) )
C-BB     .                     + UP**2 *
C-BB     .( DREAL( 1.D0/(DCONJG(Z)*WP) *
C-BB     .        (2.D0 - DCMPLX(SP*KP-T*KMP+U*KP,4.D0*EPS)/W) ) ) )
      SGWZT0= SWZPM*(1.D0-DPP)*(1.D0-DPM) /4.D0
C  TOTAL
      SIG0 = PB * YWE * ZWE * FAMS * FBMS *
     .     (SGWT0 + SGWZT0 +  NF * SGZT0)
      VVG = SIG0
      IF(DC.EQ.0.D0)RETURN
C---------------------------------
C   NON-QED RADIATIVE CORRECTIONS
C---------------------------------
CC-RM
      ESQ = E2TAB(SP)
      GMURUN = GMUTAB(SP)
      RHORUN = RHOTAB(SP)
      SWSQ = S2TAB(SP)
      GAMMAZ = GAMTAB(SP)
      PIZAP=-MPZATB(SP)
      PIAAP=-MPAATB(SP)
      CWSQ=1.D0-SWSQ
      GSU2SQ=ESQ/SWSQ
      SW=DSQRT(SWSQ)
      CW=DSQRT(CWSQ)
      GVST=-0.5D0+2.D0*(SWSQ-II*SW*CW*PIZAP)
      REG = DREAL(GVST)
      IMG = DIMAG(GVST)
      ALFST=ESQ/(4.D0*PI)
      FAMSZZ=ALFST**2*ALF/(8.D0*SWSQ**2)
      FAMSWZ=ALFST*ALF**2/(8.D0*SWSQ*SW2)
CC-RM      CALL ZSELF(SP,DRMZ,RMGZ)
C Z'S CORRECTED PROPAGATORS
CC-RM      Z  = DCMPLX(SP-MZ2+DRMZ,RMGZ)
      FN = PIZAP**2+(SWSQ-CWSQ)/(SW*CW)*PIZAP*PIAAP
      Z  = SP*(1.D0+FN)
     .             -(GSU2SQ/CWSQ)/(4.D0*ROOT2*GMURUN*RHORUN)
     .             +II*DSQRT(DABS(SP))*GAMMAZ
      Z2 = Z*DCONJG(Z)
C  Z0
CC-RM      SZMP  = (GV-GA)**2*  (T**2+TP**2)/(2*CW2**2*Z2)
CC-RM      SZPM  = (GV+GA)**2*  (U**2+UP**2)/(2*CW2**2*Z2)
      SZMP  = ((REG-GA)**2+IMG**2)*  (T**2+TP**2)/(2*CWSQ**2*Z2)
      SZPM  = ((REG+GA)**2+IMG**2)*  (U**2+UP**2)/(2*CWSQ**2*Z2)
      SGZT  = ( SZMP*(1.D0+DPP)*(1.D0+DPM) +
     .          SZPM*(1.D0-DPP)*(1.D0-DPM) )/4.D0
C  W-Z0
CC-RM      SWZPM = (GV+GA)/CW2* ( DREAL( U/(DCONJG(Z)*W) *
      SWZPM = DREAL( (DCONJG(GVST)+GA)/CWSQ* (
     .                               U/(DCONJG(Z)*W) *
     .        (2.D0*U - DCMPLX(SP*KM-TP*KPP+UP*KMP,-4.D0*EPS)/WP)
     .                     +         UP/(DCONJG(Z)*WP) *
     .        (2.D0*UP - DCMPLX(SP*KP-T*KMP+U*KP,4.D0*EPS)/W) ) )
      SGWZT = SWZPM*(1.D0-DPP)*(1.D0-DPM) /4.D0
CCC
CC-RM      SIGZ = PB * YWE * ZWE * FAMS * FBMS *
CC-RM     . (  SGWZT + SGWT0 +  NF*SGZT )
      SIGZ = PB * YWE * ZWE * FBMS *
     . (  FAMSWZ*SGWZT + FAMS*SGWT0 + FAMSZZ*NF*SGZT )
CC
C-----------------------------------------------------------------------
C   QED RADIATIVE CORRECTIONS (SOFT + VIRTUAL) EXPONENTIATED
C-----------------------------------------------------------------------
C
      SIGQ = 0.D0
      BETA = 2.D0*ALDPI*(DLOG(S/ME2)-1.D0)
CC-RM      VVG  = PB * YWE * ZWE * FAMS * FBMS *
CC-RM     . (  SGWZT + SGWT0 + NF*SGZT*(1.D0+DELT1(ZZ,Y))*DC**BETA  )
      VVG  = PB * YWE * ZWE * FBMS *
     .      (FAMSWZ*SGWZT + FAMS*SGWT0 +
     .       FAMSZZ*NF*SGZT*(1.D0+DELT1(ZZ,Y))*DC**BETA)
      IF (VVG.LT.0.D0) THEN
          WRITE(LWRITE,*) 'WARNING !! : VVG = ', VVG
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION D2(RT,RK,S,Z,RM2)
      IMPLICIT REAL*8(A-H,M,O-Z)
      COMMON /CMS/EB,CME,SS,BE,BE2
     1       /LEPT1/ME,ME2
     2       /QED/ALF,ALF2,ALDPI
      COMMON /CONST/PI,DR,SR2,PB
C
      DLG1= -PI*PI/6.D0 + .5D0*DLOG(1.D0+RK/RM2)*
     .  DLOG((1.D0+RK/RM2)/(RK/RM2)**2) +
     .     DL(1.D0/(1.D0+RK/RM2))
CC-RM
           IF (RK.LT.ME2) THEN
      DLG2=DL(1.D0-RK/ME2)
           ELSE
      DLG2=.5D0*DLOG(RK/ME2)*DLOG(RK/ME2/(RK/ME2-1.D0)**2) -
     .      PI*PI/6.D0 + DL(ME2/RK)
           ENDIF
CC-RM
      DLG3= -.5D0*(DLOG(1.D0-Z))**2 - DL(Z)
C
      FC10= (S*S + (S-RT)**2)/(2*RT*RK)
      FC11= -DLG3 + DLOG(1.D0-Z)*DLOG(RK/RM2) + DLG1 +
     .      DLOG(RK/RM2)*DLOG(1.D0+RK/RM2)
      FC1 = FC10*FC11
C
      FC20= 1.D0 - .5D0*(RK-2.D0*RT)/(RM2+RK) -
     .     .25D0*(RK*RT)/(RM2+RK)**2
      FC2 = DLOG(RK/RM2)*FC20
C
      FC3 = (S/(S-RM2) - .5D0*S*S/(S-RM2)**2)* DLOG(RM2/S)
C
      FC4 = .25D0*S/(RM2+RK) + .25D0*S/RK -
     .        .5D0*S/(S-RM2) - .25D0*RT/RK
C
      FC51= ME2*RM2/RK/RK
      FC52= DLOG(RM2/S)*(DLOG(S/ME2)+.5D0*DLOG(RM2/S)) -
     .      2.D0*DLOG(RK/ME2)*DLOG(RM2/S) -
     .        2.D0*DL(Z)+ PI*PI/12.D0 -
     .     .5D0*DLG2 + .5D0*(RK/(ME2-RK))*
     .      (-DLOG(RK/ME2)+.5D0+.5D0*(RK/(ME2-RK))*DLOG(RK/ME2))
      FC5 = FC51*FC52
C
      FC61= ME2*(S-RM2)/RK/RK
      FC62= -.5D0 + (.5D0+.25D0*(RK/(ME2-RK)))*DLOG(RK/ME2) -
     .      (ME2/2.D0/RK)*(DLG2-PI*PI/6.D0)
      FC6 = FC61*FC62
C
      D2 = FC1 + FC2 + FC3 + FC4 + FC5 + FC6
      RETURN
      END
      DOUBLE PRECISION FUNCTION VVGG(X)
C.............................................................
C    CROSS SECTION FOR
C      E-(P1) E+(P2) --->  NU(P3) NUB(P4) GAMMA(P5) GAMMA(P6)
C.............................................................
      IMPLICIT REAL*8(A-H,M,O-Z)
C
      COMMON /CMS/EB,CME,S,BE,BE2
     .       /NFAM/NF
     .       /HARSOF/DC
     .       /LEPT1/ME,ME2
     2       /QED/ALF,ALF2,ALDPI
      COMMON /CONST/PI,DR,SR2,PB
C
      COMMON/MOMENZ/Q1(5),Q2(5),Q3(5),Q4(5),Q5(5),Q6(5)
C...PG
        COMMON/INPOUT/LWRITE
      DIMENSION X(8)
C--------------------------------------------------
C  INITIALIZATION
C--------------------------------------------------
C
      DATA INIT/0/
      VVGG = 0.D0
      WPH = 0.D0
      WT  = 0.D0
      IF(INIT.NE.0) GO TO 1010
      INIT = 1
C
C  FIVE-VECTORS
      Q1(1)=0.D0
      Q1(2)=0.D0
      Q1(3)=EB*BE
      Q1(4)=EB
      Q1(5)=ME
C
      Q2(1)=0.D0
      Q2(2)=0.D0
      Q2(3)=-Q1(3)
      Q2(4)=EB
      Q2(5)=-ME
C
      Q3(5)=0.D0
      Q4(5)=0.D0
      Q5(5)=0.D0
      Q6(5)=0.D0
C
C  CONSTANTS AND FLUX FACTOR
      FFT=PB/(2.D0*PI)**8 /(2.D0*S)
 1010 CONTINUE
C
C-------------------------------------------------------
C   GENERATION OF A POINT IN THE ALLOWED FOUR BODY PHASE SPACE
C-------------------------------------------------------
      CALL PHASE(X,WPH)
      IF(WPH.LT.0.)WRITE(LWRITE,400)WPH
 400  FORMAT(' !!!!!!  WARNING: PHASE SPACE WEIGHT VVGG = ',G12.4)
      IF(WPH.EQ.0.) RETURN
C
C----------------------------------------
C   CALLING THE MATRIX ELEMENT SQUARED
C----------------------------------------
      CALL ELEMAT(WT)
      IF(WT.LT.0.)WRITE(LWRITE,402)WT
 402  FORMAT(' !!!!!!  WARNING: MATRIX ELEMENT VVGG = ',G12.6)
CC-RM
CC-RM THE FACTOR DUE TO EXPONENTIATION IS INCLUDED IN WPH!!
CC-RM
      VVGG = WT * NF/3.D0 * WPH * FFT
      IF (VVGG.LT.0.D0) THEN
          WRITE(LWRITE,*) ' WARNING !! : VVGG = ', VVGG
      ENDIF
      RETURN
      END
      SUBROUTINE PHASE(X,WPH)
C......................................................
C   FOUR BODY PHASE SPACE GENERATOR
C   ( MAPPED FOR E+E- ---> VVGG)
C       FOLLOWING THE SCHEME:
C
C    -----------------     X1=PHOTON        Q5
C           /---------     X2=PHOTON        Q6
C           /    /----     X3=NEUTRINO      Q3
C    ------------/----     X4=ANTINEUTRINO  Q4
C            Z*
C......................................................
      IMPLICIT REAL*8(A-H,M,O-Z)
        REAL*4 RNDM,DUMY
        EXTERNAL RNDM
      COMMON /CMS/EB,CME,S,BE,BE2
     .       /HARSOF/DC
     .       /BOS1/MZ,GZ,MW,GW,MH
     .       /BOS2/MZ2,MW2,MH2
     .       /LEPT1/ME,ME2
     .       /QED/ALF,ALF2,ALDPI
      COMMON /CONST/PI,DR,SR2,PB
      COMMON /DETCUT/XDL,XDH,CD,XMH,CM,CR
      COMMON /PLOTS2/X1,CT1,ST1,PH1,X2,CT2,ST2,PH2,CT12
      COMMON /MOMENZ/Q1(5),Q2(5),Q3(5),Q4(5),Q5(5),Q6(5)
      COMMON /SEEN/ NG,NSEEN
C...PG
        COMMON/INPOUT/LWRITE
        COMMON/VERSION/TYPVER
        CHARACTER*7 TYPVER
C...PG
C   NSYM = 1 the photons could be observed
C             (the same meaning that NSEEN=4 in the original version)
        COMMON/PGSYM /NSYM
      DIMENSION X(8),QZ(4),Q3SZ(4)
C
C ===========================================
C    INITIALIZATION
C ===========================================
      DATA INIT/0/
      WPH = 0.D0
      NV2 = 0
      NCOL = 0
      NSEEN = 0
C... PG
      NSYM=0
      IF(INIT.NE.0)GO TO 330
      INIT = 1
C
C  LIMITS FOR THE NEUTRINO PAIR INVARIANT MASS SQUARE
      ZMX2 = 4.D0*MZ2/S
      GZX2 = 4.D0*MZ*GZ/S
      ATAL = DATAN(-ZMX2/GZX2)
      ATAH0 = DATAN( (4.D0-4.D0*XDL-ZMX2) / GZX2)
      ATAH1 = DATAN( (4.D0-2.D0*XDL-ZMX2) / GZX2)
C
C  SOME JACOBIANS
      FCTO = (1.D0 + BE*CD)/(1.D0 - BE*CD)
      DCTO = DLOG(FCTO)/BE
      FCPO = 2.D0*PI
      FCTM = (1.D0 + BE)/(1.D0 - BE)
      DCTM = DLOG(FCTM)/BE
CC-RM
      BETA= 2.D0*ALDPI*(DLOG(S/ME2)-1.D0)
330   CONTINUE
C
C ========================================================
C  GENERATING THREE BODY (PHOTON, PHOTON, Z* ) PHASE SPACE
C ========================================================
C
C --- PHOTON (1)
C
C COS(THETA): MAPPING 1/(1-COS(THETA)**2)
      ZKO = FCTO**(2.D0*X(1)-1.D0)
      CT1 = (ZKO - 1.D0)/(1.D0 + ZKO)/BE
      ST1 = DSQRT(1.D0 - CT1**2)
C
C PHI : LINEAL MAPPING
      PH1 = FCPO*X(2)
      CP1 = DCOS(PH1)
      SP1 = DSQRT(1.D0 - CP1**2)
C
C --- PHOTON (2)
C
C COS(THETA): MAPPING 1/(1-COS(THETA)**2)
      ZKM = FCTM**(2.D0*X(3)-1.D0)
      CT2 = (ZKM - 1.D0)/(1.D0 + ZKM)/BE
      ST2 = DSQRT(1.D0 - CT2**2)
      IF(DABS(CT2).LT.CD)NV2=1
C
C PHI : LINEAL MAPPING
      PH2 = FCPO*X(4)
      CP2 = DCOS(PH2)
      SP2 = DSQRT(1.D0 - CP2**2)
C
C  COMPUTING THE ANGLE BETWEEN BOTH PHOTONS CT12
      CT12 = ST1*ST2*(SP1*SP2+CP1*CP2)+CT1*CT2
      IF(NV2.EQ.1.AND.CT12.GE.CR)NCOL=1
C
C ======================================
C  GENERATE Z* INVARIANT MASS SQUARE
C ======================================
C
C XMZ2: BREIT-WIGNER MAPPING
      ATAH = ATAH0
      IF(NCOL.EQ.1)ATAH = ATAH1
      XMZ2 = ZMX2 + GZX2 * DTAN( ATAL + X(5)*(ATAH-ATAL) )
      XMZ = DSQRT(XMZ2)
C
C ENERGY LIMITS FOR PHOTON (1)
      XMAX = (4.D0*(1.D0-DC)-XMZ2) / (4.D0+2.D0*DC*(CT12-1.D0))
      XMIN = XDL
      IF(NCOL.EQ.1)XMIN = XDL/2.D0
      IF(XMAX.LE.XMIN)RETURN
C
C ENERGY: MAPPING 1/(X*(AA-X))
      AA = 1.D0-XMZ2/4.D0
      XWW = XMAX*(AA-XMIN)/(XMIN*(AA-XMAX))
      FCXO = DLOG(XWW) / AA
      XLAM = XMIN/(AA-XMIN) * XWW**X(6)
      X1 = AA * XLAM / (1.D0+XLAM)
      E1 = EB*X1
C
C COMPUTING X2
      X2 = (4.D0*(1.D0-X1)-XMZ2) / (4.D0+2.D0*X1*(CT12-1.D0))
      IF(X2.LT.0..OR.X2.GT.1.)WRITE(LWRITE,229)X2
229   FORMAT(' WARNING !!! IN "SUBROUTINE PHASE" X2 = ',G12.5)
      IF(NCOL.EQ.1.AND.X1.LT.X2)RETURN
      E2 = EB*X2
C
C----------------------------------------------------------------
C   ADDITIONNAL INTEGRATION CUTS ( NCUT=1:ACCEPTED/ =0:REJECTED)
C----------------------------------------------------------------
      CALL CUTS(NCUT)
      IF(NCUT.EQ.0)RETURN
C
C  COMPUTING THE Z* VARIABLES
      XZ = 2.D0-X1-X2
      EZ = EB*XZ
      BEZ = DSQRT(1.D0-XMZ2/(XZ*XZ))
      XPZ = DSQRT(XZ*XZ-XMZ2)
CC....
      CTZ=0.
      IF(BEZ.NE.0.D0)CTZ=(-X1*CT1-X2*CT2)/XPZ
      IF(CTZ.LT.-1.D0.OR.CTZ.GT.1.D0)WRITE(LWRITE,231)
231   FORMAT(' WARNING !!! IN "SUBROUTINE PHASE" CTZ OUT OF RANGE')
      IF(CTZ.GT.1.D0)CTZ=1.D0
      IF(CTZ.LT.-1.D0)CTZ=-1.D0
      STZ=DSQRT(1.D0-CTZ**2)
C
C  EVALUATE PHASE-SPACE DENSITY FACTOR
      ZJAC = ((ZMX2-XMZ2)**2+GZX2**2)* (ATAH-ATAL)/GZX2 * FCXO
     .  * (1.D0-BE2*CT1**2)*DCTO * FCPO
     .  * (1.D0-BE2*CT2**2)*DCTM * FCPO
      FX = CME**4/512.D0 * (X1*X2)**2/4.D0 * 4.D0*PI
C
C  THE ADDITIONAL FACTOR SYM IS DUE TO THE SYMMETRY
C  OF THE INTEGRATION REGION USED
C  (WHICH WE WILL INCLUDE EXPLICITELLY BY USING A RANDOM ELECTION
C  OF THE PHOTON LABEL)
      SYM = 2.D0
C
C  NSEEN=4 MEANS THAT BOTH PHOTONS ARE OBSERVED
C... PG
      IF(TYPVER.EQ.'ORIGINE') THEN
                  IF(NSEEN.EQ.4)SYM = 1.D0
      ENDIF
      IF(TYPVER.EQ.'NUCOUNT') THEN
                  IF(NSYM .EQ.1)SYM = 1.D0
      ENDIF
CC-RM      WPH = SYM * FX * ZJAC
CC-RM FACTOR DUE TO EXPONENTIATION
CC-RM
      DD1 = DELT1(X1,CT1)
      WPH = SYM * FX * ZJAC * (1.D0+DD1)*X2**BETA
C
C ======================================
C CALCULATE THE COMPLETE KINEMATICS
C ======================================
C 1)....PHOTON SYMMETRIZATION
C
C     IF(RNDM2(0.).GT..5)GO TO 33
      IF(RNDM (DUMY).GT..5)GO TO 33
      Q5(1) = E1*ST1*CP1
      Q5(2) = E1*ST1*SP1
      Q5(3) = E1*CT1
      Q5(4) = E1
      Q6(1) = E2*ST2*CP2
      Q6(2) = E2*ST2*SP2
      Q6(3) = E2*CT2
      Q6(4) = E2
      GO TO 44
33    CONTINUE
      Q6(1) = E1*ST1*CP1
      Q6(2) = E1*ST1*SP1
      Q6(3) = E1*CT1
      Q6(4) = E1
      Q5(1) = E2*ST2*CP2
      Q5(2) = E2*ST2*SP2
      Q5(3) = E2*CT2
      Q5(4) = E2
44    CONTINUE
C
C 2)....Z* DECAY
C
C  COMPUTE Z* FOUR MOMENTUM
      DO 121 I=1,3
      QZ(I) = - Q5(I) - Q6(I)
121   CONTINUE
      QZ(4) = EZ
C
C  GENERATE NEUTRINO FOUR MOMENTUM (Z* -REST FRAME)
      E3 = EB*XMZ/2.
      CTHN = -1.D0+2.D0*X(7)
      STHN = DSQRT(1.D0-CTHN*CTHN)
      PHIN = FCPO*X(8)
      Q3SZ(1) = E3 * STHN * DCOS(PHIN)
      Q3SZ(2) = E3 * STHN * DSIN(PHIN)
      Q3SZ(3) = E3 * CTHN
      Q3SZ(4) = E3
C
C   BOOST BACK TO THE LAB FRAME
      CALL BOOST(QZ,Q3SZ,Q3)
C
C 3)....KINEMATIC CLAUSURE
C
      DO 101 I=1,3
      Q4(I) = -Q5(I)-Q6(I)-Q3(I)
101   CONTINUE
      Q4(4) = CME-Q5(4)-Q6(4)-Q3(4)
C
C CHECK OF KINEMATICS
      Q4T=DSQRT( Q4(1)**2 + Q4(2)**2 + Q4(3)**2 )
      DIFX4=100.D0 * DABS(Q4(4)-Q4T)/DMAX1(Q4(4),Q4T)
      IF(Q4(4).LT.0.D0.OR.DIFX4.GT.1.D-4)
     .    WRITE(LWRITE,236)Q4(4),Q4T,DIFX4
 236  FORMAT(' WARNING !!! IN "SUBROUTINE PHASE" BAD KINEMATICS :'/
     . 6X,' Q4(4)= ',G12.5,' Q4T= ',G12.5,'  DIFX4= ',G12.5,' %')
      RETURN
      END
      SUBROUTINE BOOST(QM,QI,QO)
C................................................
C  GENERAL DOUBLE PRECISION BOOSTING ROUTINE
C
C  QM = TOTAL 4-VECTOR OF SYSTEM "SP"
C                      IN SYSTEM "S"
C  QI = INPUT : 4-VECTOR IN SYSTEM SP
C  QO = OUTPUT: 4-VECTOR IN SYSTEM S
C................................................
      IMPLICIT REAL*8 (A-H,M,O-Z)
      DIMENSION QM(4),QI(4),QO(4)
C...PG
        COMMON/INPOUT/LWRITE
C..
      EM = QM(4)
      PM2 = QM(1)**2+QM(2)**2+QM(3)**2
      IF(PM2.NE.0.D0) GO TO 15
        DO 7 I=1,4
7       QO(I) = QI(I)
        RETURN
15    PM = DSQRT(PM2)
      M = DSQRT(EM*EM-PM2)
      IF(M.NE.0.D0) GO TO 16
        WRITE(LWRITE,100)
100     FORMAT('  BOOST AT BETA=1 REQUIRED !. STOP AT SUBROUTINE BOOST')
        STOP
C..
16    EI = QI(4)
      PIP = 0.D0
      DO 10 I=1,3
10    PIP = PIP + QI(I)*QM(I)/PM
C..
      EO  = ( EM*EI + PM*PIP )/M
      POP = ( PM*EI + EM*PIP )/M
C..
      QO(4) = EO
      C = (POP-PIP)/PM
      DO 20 I=1,3
20    QO(I) = C*QM(I) + QI(I)
      RETURN
      END
      SUBROUTINE CUTS(NCUT)
C..........................................
C             DETECTION CUTS
C        ( NCUT = 1 --> ACCEPTED
C               = 0 --> REJECTED )
C..........................................
      IMPLICIT REAL*8(A-G,O-Z)
      COMMON/DETCUT/XDL,XDH,CD,XMH,CM,CR
      COMMON/PLOTS2/X1,CT1,ST1,PH1,X2,CT2,ST2,PH2,CT12
      COMMON/SEEN/ NG,NSEEN
C...PG
        COMMON/INPOUT/LWRITE
        COMMON/PGSYM/NSYM
        COMMON/VERSION/TYPVER
        CHARACTER*7 TYPVER
      NCUT=0

C... PG
         IF(TYPVER.EQ.'NUCOUNT') GOTO 60
C
C   ACCEPT ONE PHOTON SEEN AND OTHER UNSEEN(LOW ANGLE & ANY ENERGY)
C   (SINGLE PHOTON SIGNATURE)
      NSEEN = 1
      IF(X1.GT.XDL.AND.DABS(CT1).LT.CD.
     .        AND.(DABS(CT2).GE.CM.OR.X2.LE.XMH))GOTO 20
      NSEEN = 2
      IF(X2.GT.XDL.AND.DABS(CT2).LT.CD.
     .        AND.(DABS(CT1).GE.CM.OR.X1.LE.XMH))GOTO 20
C
C   ACCEPT BOTH IF ARE IN THE DETECTOR WITH SMALL ANGLE BETWEEN THEM
C   AND COMBINED ENERGY BIGGER THAN MINIMUM DETECTION ENERGY
C   (SINGLE PHOTON SIGNATURE)
      NSEEN = 3
      IF(X1+X2.LT.XDL)RETURN
      IF(DABS(CT1).LT.CD.AND.DABS(CT2).LT.CD.
     .        AND.CT12.GE.CR)GOTO 20
C
C   ACCEPT BOTH PHOTONS SEEN
C   (DOUBLE PHOTON SIGNATURE)
      IF(NG.LT.2)RETURN
      NSEEN = 4
      IF(X1.GT.XDL.AND.DABS(CT1).LT.CD.
     . AND.X2.GT.XDL.AND.DABS(CT2).LT.CD.AND.CT12.LT.CR)GOTO 20
C
      RETURN
C
  20  NCUT = 1
      RETURN
C...PG
  60  CONTINUE
      IF(X1.GT.XDL.AND.DABS(CT1).LT.CD.
     . AND.X2.GT.XDL.AND.DABS(CT2).LT.CD) NSYM=1
C... PG
C NSYM used to set symmetry factor when the two photons
C                                  are in the acceptance
C NSEEN used to fill difrently the histograms
C
      NSEEN = 1
      IF(X1.GT.XDL.AND.DABS(CT1).LT.CD
     .    .AND.DABS(CT2).LT.CM) GOTO 70
      NSEEN = 2
      IF(X2.GT.XDL.AND.DABS(CT2).LT.CD
     .    .AND.DABS(CT1).LT.CM) GOTO 70
      RETURN
  70  NCUT=1
      END
      SUBROUTINE ELEMAT(WT)
C.............................................................
C    THIS ROUTINE CALCULATES THE MATRIX ELEMENT
C    SQUARED WITH Z0 IN S-CHANNEL FOR THE PROCESS
C    E-(P1) + E+(P2) ----> NU(P3) + NUB(P4)+ G(P5) + G(P6)
C    THE OUTPUT CONTAINS ALL FACTORS REATED WITH MAT. ELEMENT
C    BUT NOT CONVERSION TO NB.
C.............................................................
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /POL/ DPP,DPM
C...PG
        COMMON/INPOUT/LWRITE
      DIMENSION NCONF(6,16)
      DATA NCONF/
     . 1, 1,-1,-1, 1, 1,   1, 1,-1,-1, 1,-1,
     . 1, 1,-1,-1,-1,-1,   1, 1,-1,-1,-1, 1,
     .-1,-1,-1,-1, 1, 1,  -1,-1,-1,-1, 1,-1,
     .-1,-1,-1,-1,-1,-1,  -1,-1,-1,-1,-1, 1,
     . 1,-1,-1,-1, 1, 1,   1,-1,-1,-1, 1,-1,
     . 1,-1,-1,-1,-1,-1,   1,-1,-1,-1,-1, 1,
     .-1, 1,-1,-1, 1, 1,  -1, 1,-1,-1, 1,-1,
     .-1, 1,-1,-1,-1,-1,  -1, 1,-1,-1,-1, 1/
C
C  THE MAIN CONTRIBUTION COMES FROM THE 8 FIRST CONFIGURATIONS AND
C  USUALLY THE REST CONTRIBUTE LESS THAN ONE PER MIL SO THAT THEIR
C  CALCULATION CAN BE AVOIDED TO SAVE CPU TIME.
C     NSPIN = 16
      NSPIN = 8
      CALL SPININ(0)
C
      WT = 0.D0
      DO 100 I=1,NSPIN
      PCONF = (1.D0+NCONF(1,I)*DPM)*(1.D0+NCONF(2,I)*DPP)*
     .                      AMTOT(NCONF(1,I))
      WT  = WT + PCONF
 100  CONTINUE
C
C   FACTOR 3 STANDS FOR NUMBER OF FAMILIES
C   FACTOR 8 STANDS FOR AVERAGE OVER INITIAL POLS AND PHOT SYMM FACTOR
      WT = WT*3.D0/8.D0
      RETURN
      END
      FUNCTION AMTOT(L)
C.............................................................
C THIS FUNCTION ADDS THE CONTRIBUTION OF ALL THE POLARIZATION
C CONFIGURATIONS BY THE ADEQUATE PERMUTATIONS OF THE ONE
C CALCULATED IN AMPLI
C.............................................................
      IMPLICIT REAL*8(A-H,M,O-Z)
      COMMON / PRODUX / SP,SM,U,E,D
      COMMON /CONST/ PI,DR,SR2,PB
      COMMON /BOS1/MZ,GZ,MW,GW,MH
     .       /BOS2/MZ2,MW2,MH2
      COMMON /WEAK/SW2,CW2,A2,V2,VU2,VD2
     .       /QED/ALF,ALF2,ALDPI
CC-RM
      COMMON /STAR/SWSQ,SW,CW,PIZAP
C...PG
        COMMON/INPOUT/LWRITE
CC-RM
      DIMENSION SP(6,6),SM(6,6),D(6,6),E(6),U(6),B(6)
      DIMENSION L(6)
      COMPLEX*16 AMT,AMPLI,SP,SM,II,Z
      DATA INIT/0/
      IF(INIT.NE.0)GO TO 100
      INIT=1
      ROOT2=DSQRT(2.D0)
      II=DCMPLX(0.D0,1.D0)
100   CONTINUE
C
C  OVERALL FACTOR |M|**2
C  AMFAC =  4.* E**8/(4*SINW*COSW)**4
      SPP=D(3,4)
      ESQ = E2TAB(SPP)
      GMURUN = GMUTAB(SPP)
      RHORUN = RHOTAB(SPP)
      SWSQ = S2TAB(SPP)
      GAMMAZ = GAMTAB(SPP)
      PIZAP=-MPZATB(SPP)
      PIAAP=-MPAATB(SPP)
      CWSQ=1.D0-SWSQ
      GSU2SQ=ESQ/SWSQ
      SW=DSQRT(SWSQ)
      CW=DSQRT(CWSQ)


      ALFST=ESQ/(4.D0*PI)
CC-RM      AMFAC = 4.D0*(PI*ALF)**4 / (SW2*CW2)**2
      AMFAC = 4.D0*(PI*ALFST)**2*(PI*ALF)**2 / (SWSQ*CWSQ)**2
C
C  OVERALL FACTOR : Z0 PROPAGATOR
C  Z0D1  =   D(3,4) - MZ2 + RDBZ
C  Z0PRO = 1./(Z0D1**2 + (MZ*GZ)**2)
CC-RM      CALL ZSELF(D(3,4),RDBZ,G2BZ)
CC-RM      Z0D1  =   D(3,4) - MZ2 + RDBZ
CC-RM      Z0PRO = 1./(Z0D1**2 + G2BZ**2)
      FN = PIZAP**2+(SWSQ-CWSQ)/(SW*CW)*PIZAP*PIAAP
      Z  = SPP*(1.D0+FN)
     .             -(GSU2SQ/CWSQ)/(4.D0*ROOT2*GMURUN*RHORUN)
     .             +II*DSQRT(DABS(SPP))*GAMMAZ
      Z0PRO = 1.D0/(Z*DCONJG(Z))
C
      AMT =    AMPLI (1,L(1),2,L(2),3,L(3),4,L(4),5, L(5),6, L(6),1,0)
     .    +    AMPLI (1,L(1),2,L(2),3,L(3),4,L(4),6, L(6),5, L(5),1,0)
     .+ DCONJG(AMPLI (2,L(2),1,L(1),4,L(4),3,L(3),5,-L(5),6,-L(6),1,1)
     .    +    AMPLI (2,L(2),1,L(1),4,L(4),3,L(3),6,-L(6),5,-L(5),1,1))
     .    -    AMPLI (1,L(1),2,L(2),3,L(3),4,L(4),5, L(5),6, L(6),2,0)
     .- DCONJG(AMPLI (2,L(2),1,L(1),4,L(4),3,L(3),5,-L(5),6,-L(6),2,1))
C
      AMTOT =  AMT*DCONJG(AMT)*Z0PRO*AMFAC
      RETURN
      END
      FUNCTION AMPLI(P1,L1,P2,L2,P3,L3,P4,L4,P5,L5,P6,L6,IND,ICONJG)
C...........................................................
C    THIS FUNCTION CALCULATES THE AMPLITUDE FOR THE
C    Z0 IN S-CHANNEL FOR THE PROCESS
C    E-(P1) + E+(P2) ----> NU(P3) + NUB(P4)+ G(P5) + G(P6)
C    WITH BREMSS. FROM E+ OR E-  WITH IND=1 AND WITH
C    WITH BREMSS. FROM E+ &  E-  WITH IND=2
C...........................................................
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / PRODUX / SP,SM,U,E,D
      COMMON / WEAK / SW2,CW2,A2,V2,VU2,VD2
      COMMON / CONST / PI,DR,SR2,PB
      DIMENSION SP(6,6),SM(6,6),D(6,6),E(6),U(6),B(6)
      COMPLEX*16 Z,SP,SM
      COMPLEX*16 AMPLI,ZREP1,ZREP2,II,CR,CL,CG,CZ
CC-RM
      COMMON /STAR/SWSQ,SW,CW,PIZAP
CC-RM
      INTEGER P,P1,P2,P3,P4,P5,P6
C
C  INITIALIZATION
      DATA B/1.D0, 1.D0,-1.D0,-1.D0,-1.D0,-1.D0/
      DATA CG /(1.D0,0.D0)/, CZ /(0.D0,0.D0)/
      DATA P/3/, INIT/0/
      IF(INIT.NE.0)GO TO 111
      INIT = 1
CC-RM      CR   = 4.D0*SW2
CC-RM      CL   = CR-2.D0
      II=DCMPLX(0.D0,1.D0)
111   CONTINUE
      CR   = 4.D0*(SWSQ-II*SW*CW*PIZAP)
      CL   = CR-2.D0
      IF (ICONJG.EQ.1) THEN
          CR=DCONJG(CR)
          CL=DCONJG(CL)
      ENDIF
      AMPLI = (0.D0,0.D0)
C
C  NORMALIZATION FACTORS FOR THE PHOTON POLARIZATION VECTOR
      ZN1=1.D0/(SR2 * CDABS(SP(P,P5)) )
      ZN2=1.D0/(SR2 * CDABS(SP(P,P6)) )
C
C   LOOP OVER REPEATED INDEX "IL" AND  "ILP"
C   AND EVALUATE THE AMPLITUDE FOR THE Z0 DIAGRAM
      DO 100 I1=1,2
      IL  = 2*I1 - 3
      ZREP1 = Z(P5,IL,P1,L1,P,L5,P5,L5,CG,CG,CG,CG)
      ZREP2 = Z(P1,IL,P1,L1,P,L5,P5,L5,CG,CG,CG,CG)
      DO 100 I2=1,2
      ILP = 2*I2 - 3
      IF(IND.EQ.2) GO TO 150
      AMPLI = AMPLI
     . + Z(P2,L2,P1,ILP,P3,L3,P4,L4,CL,CR,CG,CZ)   * B(P1) *
     .     (Z(P1,ILP,P5,IL,P,L6,P6,L6,CG,CG,CG,CG) * ZREP1 * B(P5)
     .     +Z(P1,ILP,P1,IL,P,L6,P6,L6,CG,CG,CG,CG) * ZREP2 * B(P1))
     . + Z(P2,L2,P5,ILP,P3,L3,P4,L4,CL,CR,CG,CZ)   * B(P5) *
     .     (Z(P5,ILP,P5,IL,P,L6,P6,L6,CG,CG,CG,CG) * ZREP1 * B(P5)
     .     +Z(P5,ILP,P1,IL,P,L6,P6,L6,CG,CG,CG,CG) * ZREP2 * B(P1))
     . + Z(P2,L2,P6,ILP,P3,L3,P4,L4,CL,CR,CG,CZ)   * B(P6) *
     .     (Z(P6,ILP,P5,IL,P,L6,P6,L6,CG,CG,CG,CG) * ZREP1 * B(P5)
     .     +Z(P6,ILP,P1,IL,P,L6,P6,L6,CG,CG,CG,CG) * ZREP2 * B(P1))
      GO TO 100
C
  150 AMPLI  = AMPLI   +
     .  Z(P2,L2,P6,ILP,P,L6,P6,L6,CG,CG,CG,CG) * B(P6) *
     .    ( Z(P6,ILP,P1,IL,P3,L3,P4,L4,CL,CR,CG,CZ) * ZREP2 * B(P1)
     .    + Z(P6,ILP,P5,IL,P3,L3,P4,L4,CL,CR,CG,CZ) * ZREP1 * B(P5))
     .+ Z(P2,L2,P2,ILP,P,L6,P6,L6,CG,CG,CG ,CG) *B(P2) *
     .    ( Z(P2,ILP,P1,IL,P3,L3,P4,L4,CL,CR,CG,CZ) * ZREP2 * B(P1)
     .    + Z(P2,ILP,P5,IL,P3,L3,P4,L4,CL,CR,CG,CZ) * ZREP1 * B(P5))
  100 CONTINUE
C
      PROP1 = B(P1)*B(P5)*D(P1,P5)
      PROP2 = B(P5)*B(P6)*D(P5,P6) + B(P1)*B(P5)*D(P1,P5)
     .      + B(P1)*B(P6)*D(P1,P6)
      PROP3 = B(P2)*B(P6)*D(P2,P6)
      PROPG = PROP2
      IF(IND.EQ.2) PROPG = PROP3
      AMPLI = ZN1*ZN2*AMPLI/PROPG/PROP1
      RETURN
      END
      SUBROUTINE SPININ(INF)
C..................................................................
C    COMPUTATION OF THE BASIC QUANTITIES
C    NEEDED FOR THE HELICITY AMPLITUDES EVALUATION
C      INPUT   ==> VECTORS P1,P2,P3,P4,P5,P6    ( COMMON /MOMENZ/ )
C          FORMAT: (PX,PY,PZ,E,M)
C      OUTPUT  ==> BASIC QUANTITIES SP,SM,U,E,D ( COMMON /PRODUX/ )
C                  ( SP --> S+  / SM --> S- )
C..................................................................
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 SP,SM
      COMMON / MOMENZ / P1,P2,P3,P4,P5,P6
      COMMON / PRODUX / SP,SM,U,E,D
C...PG
        COMMON/INPOUT/LWRITE
      DIMENSION P1(5),P2(5),P3(5),P4(5),P5(5),P6(5)
      DIMENSION Q(5,6),SP(6,6),SM(6,6),D(6,6)
      DIMENSION E(6),U(6)
      EQUIVALENCE ( P1(1) , Q(1,1) )
C
      DO 1 I=1,6
      U(I) = DSQRT( 2.*( Q(4,I) - Q(1,I) )  )
      E(I) = Q(5,I)/U(I)
    1 CONTINUE
      DO 2 I=1,6
      DO 2 J=I,6
      SP(I,J)= DCMPLX( Q(2,I) , Q(3,I) ) * U(J)/U(I)
     .        -DCMPLX( Q(2,J) , Q(3,J) ) * U(I)/U(J)
      SP(J,I)=-SP(I,J)
      SM(I,J)=-DCONJG( SP(I,J) )
      SM(J,I)=-SM(I,J)
      D(I,J) = SP(I,J)*SM(J,I) + (E(I)*U(J))**2 + (E(J)*U(I))**2
      D(J,I) = D(I,J)
    2 CONTINUE
C
      IF(INF.LT.1) RETURN
      WRITE(LWRITE,100)
  100 FORMAT(' ',40(1H-),' SPININ INF  ',40(1H-))
      WRITE(LWRITE,101) (P1(I),P2(I),P3(I),P4(I),P5(I),P6(I),I=1,5)
  101 FORMAT('0INPUT (PX ,PY ,PZ ,E ,M ) ',/,(6G15.6))
      WRITE(LWRITE,102) (U(I),E(I),I=1,6)
  102 FORMAT('0VECTORS U(I) AND E(I)',/,(2G15.6))
      WRITE(LWRITE,104) ((SP(I,J),J=1,6),I=1,6)
  104 FORMAT('0MATRIX SP(I,J)',/,(6('  ',2G10.3)))
      WRITE(LWRITE,105) ((SM(I,J),J=1,6),I=1,6)
  105 FORMAT('0MATRIX SM(I,J)',/,(6('  ',2G10.3)))
      WRITE(LWRITE,107) ((D(I,J),J=1,6),I=1,6)
  107 FORMAT('0MATRIX D(I,J)',/,(6('  ',G15.6)))
      RETURN
      END
      FUNCTION Z(P1,L1,P2,L2,P3,L3,P4,L4,CL1,CR1,CL2,CR2)
C................................................................
C    CALCULATION OF ALL THE Z FUNCTIONS FOR GAMMA OR Z0 EXCHANGE.
C    ( FOR GAMMA, SET CL1=1,CR1=1,CL2=1,CR2=1 )
C................................................................
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 Z,SP,SM,CL1,CL2,CR1,CR2
      INTEGER P1,P2,P3,P4,P5,P6
      DIMENSION SP(6,6),SM(6,6),D(6,6),E(6),U(6)
      COMMON / PRODUX / SP,SM,U,E,D
      LZ=9-4*L1-2*L2-L3-(L4+1)/2
      GOTO(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),LZ
    1 Z= -2.D0*( CR1*CR2*SP(P1,P3)*SM(P2,P4)
     .        - CR1*CL2*U(P1)*U(P2)*E(P3)*E(P4)
     .        - CL1*CR2*U(P3)*U(P4)*E(P1)*E(P2) )
      GOTO 17
    2 Z= -2.D0*U(P2)*(    CR1*CR2*SP(P1,P3)*E(P4)
     .                 - CR1*CL2*SP(P1,P4)*E(P3) )
      GOTO 17
    3 Z= -2.D0*U(P1)*(    CR1*CL2*SM(P2,P3)*E(P4)
     .                 - CR1*CR2*SM(P2,P4)*E(P3) )
      GOTO 17
    4 Z= -2.D0*( CR1*CL2*SP(P1,P4)*SM(P2,P3)
     .        - CR1*CR2*U(P1)*U(P2)*E(P3)*E(P4)
     .        - CL1*CL2*U(P3)*U(P4)*E(P1)*E(P2) )
      GOTO 17
    5 Z= -2.D0*U(P4)*(    CR1*CR2*SP(P3,P1)*E(P2)
     .                 - CL1*CR2*SP(P3,P2)*E(P1) )
      GOTO 17
    6 Z=(0.D0,0.D0)
      GOTO 17
    7 Z=  2.D0*( CR1*E(P2)*U(P1) - CL1*E(P1)*U(P2) )
     .       *( CL2*E(P4)*U(P3) - CR2*E(P3)*U(P4) )
      GOTO 17
    8 Z=  2.D0*U(P3)*(    CR1*CL2*SP(P1,P4)*E(P2)
     .                 - CL1*CL2*SP(P2,P4)*E(P1) )
      GOTO 17
    9 Z=  2.D0*U(P3)*(    CL1*CR2*SM(P1,P4)*E(P2)
     .                 - CR1*CR2*SM(P2,P4)*E(P1) )
      GOTO 17
   10 Z=  2.D0*( CL1*E(P2)*U(P1) - CR1*E(P1)*U(P2) )
     .       *( CR2*E(P4)*U(P3) - CL2*E(P3)*U(P4) )
      GOTO 17
   11 Z=(0.D0,0.D0)
      GOTO 17
   12 Z=  2.D0*U(P4)*(    CL1*CL2*SM(P1,P3)*E(P2)
     .                 - CR1*CL2*SM(P2,P3)*E(P1) )
      GOTO 17
   13 Z= -2.D0*( CL1*CR2*SP(P2,P3)*SM(P1,P4)
     .        - CL1*CL2*U(P1)*U(P2)*E(P3)*E(P4)
     .        - CR1*CR2*U(P3)*U(P4)*E(P1)*E(P2) )
      GOTO 17
   14 Z= -2.D0*U(P1)*(    CL1*CR2*SP(P2,P3)*E(P4)
     .                 - CL1*CL2*SP(P2,P4)*E(P3) )
      GOTO 17
   15 Z= -2.D0*U(P2)*(    CL1*CL2*SM(P1,P3)*E(P4)
     .                 - CL1*CR2*SM(P1,P4)*E(P3) )
      GOTO 17
   16 Z= -2.D0*( CL1*CL2*SP(P2,P4)*SM(P1,P3)
     .        - CL1*CR2*U(P1)*U(P2)*E(P3)*E(P4)
     .        - CR1*CL2*U(P3)*U(P4)*E(P1)*E(P2) )
   17 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION DL(X)
      IMPLICIT REAL*8(A-H,O-Z)
      F1 = 0.D0
      IF(X.NE.1.D0) F1=2.D0*(1.D0-X)*DLOG(1.D0-X)
      DL =( 3.D0*X + F1 + X*X*(.25D0 - .18787D0*X + .02559D0*X*X)/
     . (1.D0 - X*(.86258D0 - .17044D0*X + .00498D0*X*X)) )/(1.D0+X)
      RETURN
      END
      SUBROUTINE ZWDTH0(GAZ)
C.............................................................
C   Z BOSON SELF ENERGY, REAL AND IMAGINARY PART
C   AS FUNCTION OF  S = Q**2 AND THE NUMBER OF
C   LEPTON FAMILIES NF.
C
C    WRITEN BY M.BOEHM, H.SPIESBERGER AND W.HOLLIK
C             (DESY 84-027 MARCH 1984)
C    MODIFIED BY M.MARTINEZ TO INCLUDE NEW FORMULAE
C             FOR NEUTRINO CONTRIBUTION
C      (SEE  M.BOEHM, A.DENNER AND W.HOLLIK
C              DESY 86-165 DECEMBER 1986)
C.............................................................
      IMPLICIT REAL*8(A-Z)
      INTEGER I,J,L,NF
      DIMENSION ML(50,2),VL2(2)
      DIMENSION MQ(3,2),VQ2(2)
      DIMENSION RE(100),IM(100)
      COMMON /NFAM/NF
      COMMON /CMS/EB,CME,SS,BE,BE2
     .       /BOS1/MZ,GZ,MW,GW,MH
     .       /BOS2/MZ2,MW2,MH2
     .       /LEPT1/ME,ME2
     .       /LEPT2/MMU,MTAU,MHEAVY
     .       /HAD/MU,MD,MS,MC,MB,MT
      COMMON /WEAK/SW2,CW2,A2,V2,VU2,VD2
     .       /QED/ALF,ALF2,ALDPI
      COMMON /LEPTON/ML,VL2
     .       /QUARK/MQ,VQ2
      COMMON /OUTPUT/ NHSE,NHWE,NHEV,NHST,NREC
C...PG
        COMMON/INPOUT/LWRITE
C---------------------------------------------------
C
C TABULATION OF THE Z0 SELF ENERGY FUNCTION
C AND CALCULATION OF THE Z0 WIDTH
C
C... PG
C        IF(NHSE.NE.0)THEN
C     CALL HBOOK1(71,'RE(SIGMA_Z)$',100,0.,100.,0.)
C     CALL HBOOK1(72,'IM(SIGMA_Z)$',100,0.,100.,0.)
C        ENDIF
C..FERMION MASSES IN ARRAY FORM:
      ML(1,2)=ME
      ML(2,2)=MMU
      ML(3,2)=MTAU
      DO 110 I=1,NF
      ML(I,1)=0.D0
      IF(I.GT.3)ML(I,2)=MHEAVY
110   CONTINUE
      MQ(1,1)=MU
      MQ(1,2)=MD
      MQ(2,1)=MC
      MQ(2,2)=MS
      MQ(3,1)=MT
      MQ(3,2)=MB
C..VECTOR COUPLINGS IN ARRAY FORM:
      VL2(1)=A2
      VL2(2)=V2
      VQ2(1)=VU2
      VQ2(2)=VD2
      GAZ=IMSZ(MZ2)/MZ
      RETURN
      END
      DOUBLE PRECISION FUNCTION IMSZ(X)
C.............................................
C  IMAGINARY PART OF THE Z SELF ENERGY
C.............................................
      IMPLICIT REAL*8(A-Z)
      INTEGER I,L,NF
      DIMENSION T(25)
      DIMENSION ML(50,2),VL2(2)
      DIMENSION MQ(3,2),VQ2(2)
      COMMON /NFAM/NF
      COMMON /BOS1/MZ,GZ,MW,GW,MH
      COMMON /BOS2/Z,W,H
      COMMON /WEAK/S,C,A2,V2,VU2,VD2
     .       /QED/ALF,ALF2,ALDPI
      COMMON /LEPTON/ML,VL2
     .       /QUARK/MQ,VQ2
      COMMON /CONST/PI,DR,SR2,PB
C-----------------------------------------------------
C..LEPTON PART
      T(1)=0.D0
      T(2)=0.D0
      DO 1 I=1,NF
      T(1)=T(1) + (VL2(1)+A2)*X*PI
      GX = G(X,ML(I,2),ML(I,2))
      T(2)=T(2) + (VL2(2)+A2)*(X+2.D0*ML(I,2)**2)*GX
     & - 3.D0*ML(I,2)**2/(8.D0*C*S)*GX
    1 CONTINUE
      T(1) = T(1)*4.D0/3.D0
      T(2) = T(2)*4.D0/3.D0
C..QUARK PART
      T(3)=0.D0
      DO 311 L=1,3
      DO 113 I=1,2
      GX = G(X,MQ(L,I),MQ(L,I))
      T(3)=T(3) + (VQ2(I)+A2)*(X+2.D0*MQ(L,I)**2)*GX
     & - 3.D0*MQ(L,I)**2/(8.D0*C*S)*GX
  113 CONTINUE
  311 CONTINUE
      T(3)=T(3)*4.D0
C..BOSON PART
      T(4)=(10.D0*X+20.D0*W)*(-C)*C+3.D0*W+(2.D0*W+X/4.D0)*(C-S)*(C-S)
      T(4)=T(4)*G(X,MW,MW)/(3.D0*S*C)
      T(5)=10.D0*Z-2.D0*H+X + (H-Z)*(H-Z)/X
      T(5)=T(5)*G(X,MH,MZ)/(12.D0*S*C)
C----------------------------------------------------------------
      IMSZ=0.D0
      DO 100 I=1,5
      IMSZ=IMSZ+T(I)
  100 CONTINUE
      IMSZ=IMSZ*ALDPI/4.D0
      RETURN
      END
      DOUBLE PRECISION FUNCTION G(Y,A,B)
C...............................................
C  IMAGINARY PART OF THE COMPLEX FUNCTION F
C...............................................
      IMPLICIT REAL*8(A-Z)
      COMMON /CONST/PI,DR,SR2,PB
C     G(Y,M1,M2) = IM F(Y,M1,M2)
      P=(A+B)*(A+B)
      Q=(A-B)*(A-B)
      IF ( Y .LE. P ) GO TO 10
      G=DSQRT(Y-P)*DSQRT(Y-Q)*PI/Y
      GO TO 20
   10 CONTINUE
      G=0.D0
   20 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION DELT1(Z,Y)
C-----------------------------------------------------------------------
C   QED RADIATIVE CORRECTIONS (SOFT + VIRTUAL, EXCLUDED THE IR PART)
C-----------------------------------------------------------------------
C
      IMPLICIT REAL*8 (A-H,M,O-Z)
      COMMON /CMS/EB,CME,S,BE,BE2
     .       /LEPT1/ME,ME2
     .       /QED/ALF,ALF2,ALDPI
      COMMON /CONST/PI,DR,SR2,PB
C
      DL1  = DLOG(S/ME2)
CC  DEL1 = DV + D1 + DSOFT
CC-RM      DVS  = 2.D0*(DL1-1.)*DLOG(CME/ME)-2.D0-
CC-RM     .            DL1*DL1+2.5D0*DL1+PI*PI/3.D0
CC-RM THE SAME EXPRESSION WRITTEN DIFFERENTLY
CC-RM
      DVS  = 1.5D0*DL1+PI*PI/3.D0-2.D0
      D1   = 1.5D0*DLOG(1.-Z)
      DEL1 = ALDPI * (DVS + D1)
CC  DEL2
      RT = .5D0*S*Z*(1.D0-Y)
      RK = .5D0*S*Z*(1.D0+Y)
      SS = S*(1.D0-Z)
      DEL20= S*S*(1.D0+(1.D0-Z)**2)/(RT*RK) - 2.D0 - 2.D0*ME2*SS*
     .       (1.D0/RT/RT + 1.D0/RK/RK)
      DEL21= D2(RT,RK,S,Z,SS)
      DEL22= D2(RK,RT,S,Z,SS)
      DEL23= 2.D0* ALDPI * (DEL21 + DEL22)
      DEL2 = DEL23 / DEL20
CC
      DELT1= DEL1 + DEL2
      END
      FUNCTION E2STRE(SR,IND)
C*********** SSTAR PREPARED FOR INTEGRATION INTO BREMMUS *************
C
C   GLASHOW-SALAM-WEINBERG RENORMALIZATION SCHEME-INVARIANT PARAMETERS
C
C   CALCULATED BY B.W.LYNN AND D.C.KENNEDY
C   BASED ON RADIATIVE CORRECTIONS BY B.W.LYNN AND R.G.STUART
C   CODED BY D.C.KENNEDY 10/1/86
C
C   MAIN PROGRAM. INPUT AND DEFINITION OF FIXED PARAMETERS.
C   OUTPUTS SINE-STAR, E-STAR, GMU-STAR, RHO-STAR.  ALL MASSES
C   IN GEV.  N.B.: S=Q2=-MZSQ AT Z POLE!
C
C
C
C   BUFFER BLOCK: INTERFACE TO BREMMUS
C
C  E2STAR CONVERTED TO REAL FUNCTION
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      REAL*8 E2STRE,SR
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C       SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/PRMGSW/ALFAR,GMUR,MWR,MZR,MHR
      REAL*8 ALFAR,GMUR,MWR,MZR,MHR
      COMMON/FERMAS/NLPT,NQRK,XLPTR,XQRKR
      REAL*8 XLPTR(40,2),XQRKR(40,2)
C  EXTERNAL ENTRY: INITIALIZE
      RHOTRE=(1.D0,0.D0)
      XSC=(100.D0)**2
      PI=(3.141592653589793238462643D0,0.D0)
      NGEN=INT(NQRK/2)
      QCD=1
      DQQ=1
      D3Q=0
      MIX=0
C  EXTERNAL ENTRY: CONVERT PARAMETERS TO COMPLEX
      DO 10 I=1,NQRK
        XQRK(I,1)=DCMPLX(XQRKR(I,1)*XQRKR(I,1))
        IF(XQRKR(I,1).EQ.0.D0)XQRK(I,1)=(0.D0,0.D0)
        XQRK(I,2)=DCMPLX(XQRKR(I,2))
        IF(XQRKR(I,2).EQ.0.D0)XQRK(I,2)=(0.D0,0.D0)
   10 CONTINUE
      DO 20 I=1,NLPT
        XLPT(I,1)=DCMPLX(XLPTR(I,1)*XLPTR(I,1))
        IF(XLPTR(I,1).EQ.0.D0)XLPT(I,1)=(0.D0,0.D0)
        XLPT(I,2)=DCMPLX(XLPTR(I,2))
        IF(XLPTR(I,2).EQ.0.D0)XLPT(I,2)=(0.D0,0.D0)
   20 CONTINUE
      MWSQ=DCMPLX(MWR*MWR)
      MZSQ=DCMPLX(MZR*MZR)
      MHSQ=DCMPLX(MHR*MHR)
      ALFA=DCMPLX(ALFAR)
      GMU=DCMPLX(GMUR)
      CWSQ=MWSQ/MZSQ
      SWSQ=1.D0-CWSQ
C   COMPUTES NEW GMU = GMUSTAR(0) WITH NON-ABELIAN BOXES AND VERTICES
      DELVER=(ALFA/(2.D0*PI*SWSQ))
     C      *(3.D0-(CWSQ-3.D0*(CWSQ/SWSQ))*CDLOG(CWSQ))
      DELBOX=-(ALFA/(8.D0*PI))
     C      *(5.D0*(CWSQ/SWSQ)**2-3.D0)*CDLOG(CWSQ)
      GMU=GMU/(1.D0+DELVER+DELBOX)
C  CALL E2STAR: CONVERT TO REAL
      S=-DCMPLX(SR)
      IF(SR.EQ.0.D0)S=(0.D0,0.D0)
      E2STRE=DREAL(E2STAR(S,IND))
      RETURN
      END
      FUNCTION S2STRE(SR,IND)
C  S2STAR CONVERTED TO REAL FUNCTION
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      REAL*8 S2STRE,SR
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C       SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/PRMGSW/ALFAR,GMUR,MWR,MZR,MHR
      REAL*8 ALFAR,GMUR,MWR,MZR,MHR
      COMMON/FERMAS/NLPT,NQRK,XLPTR,XQRKR
      REAL*8 XLPTR(40,2),XQRKR(40,2)
C  EXTERNAL ENTRY: INITIALIZE
      RHOTRE=(1.D0,0.D0)
      XSC=(100.D0)**2
      PI=(3.141592653589793238462643D0,0.D0)
      NGEN=INT(NQRK/2)
      QCD=1
      DQQ=1
      D3Q=0
      MIX=0
C  EXTERNAL ENTRY: CONVERT PARAMETERS TO COMPLEX
      DO 10 I=1,NQRK
        XQRK(I,1)=DCMPLX(XQRKR(I,1)*XQRKR(I,1))
        IF(XQRKR(I,1).EQ.0.D0)XQRK(I,1)=(0.D0,0.D0)
        XQRK(I,2)=DCMPLX(XQRKR(I,2))
        IF(XQRKR(I,2).EQ.0.D0)XQRK(I,2)=(0.D0,0.D0)
   10 CONTINUE
      DO 20 I=1,NLPT
        XLPT(I,1)=DCMPLX(XLPTR(I,1)*XLPTR(I,1))
        IF(XLPTR(I,1).EQ.0.D0)XLPT(I,1)=(0.D0,0.D0)
        XLPT(I,2)=DCMPLX(XLPTR(I,2))
        IF(XLPTR(I,2).EQ.0.D0)XLPT(I,2)=(0.D0,0.D0)
   20 CONTINUE
      MWSQ=DCMPLX(MWR*MWR)
      MZSQ=DCMPLX(MZR*MZR)
      MHSQ=DCMPLX(MHR*MHR)
      ALFA=DCMPLX(ALFAR)
      GMU=DCMPLX(GMUR)
      CWSQ=MWSQ/MZSQ
      SWSQ=1.D0-CWSQ
C   COMPUTES NEW GMU = GMUSTAR(0) WITH NON-ABELIAN BOXES AND VERTICES
      DELVER=(ALFA/(2.D0*PI*SWSQ))
     C      *(3.D0-(CWSQ-3.D0*(CWSQ/SWSQ))*CDLOG(CWSQ))
      DELBOX=-(ALFA/(8.D0*PI))
     C      *(5.D0*(CWSQ/SWSQ)**2-3.D0)*CDLOG(CWSQ)
      GMU=GMU/(1.D0+DELVER+DELBOX)
C  CALL S2STAR: CONVERT TO REAL
      S=-DCMPLX(SR)
      IF(SR.EQ.0.D0)S=(0.D0,0.D0)
      S2STRE=DREAL(S2STAR(S,IND))
      RETURN
      END
      FUNCTION GMSTRE(SR,IND)
C  GMUSTR CONVERTED TO REAL FUNCTION
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      REAL*8 GMSTRE,SR
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C       SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/PRMGSW/ALFAR,GMUR,MWR,MZR,MHR
      REAL*8 ALFAR,GMUR,MWR,MZR,MHR
      COMMON/FERMAS/NLPT,NQRK,XLPTR,XQRKR
      REAL*8 XLPTR(40,2),XQRKR(40,2)
C  EXTERNAL ENTRY: INITIALIZE
      RHOTRE=(1.D0,0.D0)
      XSC=(100.D0)**2
      PI=(3.141592653589793238462643D0,0.D0)
      NGEN=INT(NQRK/2)
      QCD=1
      DQQ=1
      D3Q=0
      MIX=0
C  EXTERNAL ENTRY: CONVERT PARAMETERS TO COMPLEX
      DO 10 I=1,NQRK
        XQRK(I,1)=DCMPLX(XQRKR(I,1)*XQRKR(I,1))
        IF(XQRKR(I,1).EQ.0.D0)XQRK(I,1)=(0.D0,0.D0)
        XQRK(I,2)=DCMPLX(XQRKR(I,2))
        IF(XQRKR(I,2).EQ.0.D0)XQRK(I,2)=(0.D0,0.D0)
   10 CONTINUE
      DO 20 I=1,NLPT
        XLPT(I,1)=DCMPLX(XLPTR(I,1)*XLPTR(I,1))
        IF(XLPTR(I,1).EQ.0.D0)XLPT(I,1)=(0.D0,0.D0)
        XLPT(I,2)=DCMPLX(XLPTR(I,2))
        IF(XLPTR(I,2).EQ.0.D0)XLPT(I,2)=(0.D0,0.D0)
   20 CONTINUE
      MWSQ=DCMPLX(MWR*MWR)
      MZSQ=DCMPLX(MZR*MZR)
      MHSQ=DCMPLX(MHR*MHR)
      ALFA=DCMPLX(ALFAR)
      GMU=DCMPLX(GMUR)
      CWSQ=MWSQ/MZSQ
      SWSQ=1.D0-CWSQ
C   COMPUTES NEW GMU = GMUSTAR(0) WITH NON-ABELIAN BOXES AND VERTICES
      DELVER=(ALFA/(2.D0*PI*SWSQ))
     C      *(3.D0-(CWSQ-3.D0*(CWSQ/SWSQ))*CDLOG(CWSQ))
      DELBOX=-(ALFA/(8.D0*PI))
     C      *(5.D0*(CWSQ/SWSQ)**2-3.D0)*CDLOG(CWSQ)
      GMU=GMU/(1.D0+DELVER+DELBOX)
C  CALL GMUSTR: CONVERT TO REAL
      S=-DCMPLX(SR)
      IF(SR.EQ.0.D0)S=(0.D0,0.D0)
      GMSTRE=DREAL(GMUSTR(S,IND))
      RETURN
      END
      FUNCTION RHSTRE(SR,IND)
C  RHOSTR CONVERTED TO REAL FUNCTION
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      REAL*8 RHSTRE,SR
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C       SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/PRMGSW/ALFAR,GMUR,MWR,MZR,MHR
      REAL*8 ALFAR,GMUR,MWR,MZR,MHR
      COMMON/FERMAS/NLPT,NQRK,XLPTR,XQRKR
      REAL*8 XLPTR(40,2),XQRKR(40,2)
C  EXTERNAL ENTRY: INITIALIZE
      RHOTRE=(1.D0,0.D0)
      XSC=(100.D0)**2
      PI=(3.141592653589793238462643D0,0.D0)
      NGEN=INT(NQRK/2)
      QCD=1
      DQQ=1
      D3Q=0
      MIX=0
C  EXTERNAL ENTRY: CONVERT PARAMETERS TO COMPLEX
      DO 10 I=1,NQRK
        XQRK(I,1)=DCMPLX(XQRKR(I,1)*XQRKR(I,1))
        IF(XQRKR(I,1).EQ.0.D0)XQRK(I,1)=(0.D0,0.D0)
        XQRK(I,2)=DCMPLX(XQRKR(I,2))
        IF(XQRKR(I,2).EQ.0.D0)XQRK(I,2)=(0.D0,0.D0)
   10 CONTINUE
      DO 20 I=1,NLPT
        XLPT(I,1)=DCMPLX(XLPTR(I,1)*XLPTR(I,1))
        IF(XLPTR(I,1).EQ.0.D0)XLPT(I,1)=(0.D0,0.D0)
        XLPT(I,2)=DCMPLX(XLPTR(I,2))
        IF(XLPTR(I,2).EQ.0.D0)XLPT(I,2)=(0.D0,0.D0)
   20 CONTINUE
      MWSQ=DCMPLX(MWR*MWR)
      MZSQ=DCMPLX(MZR*MZR)
      MHSQ=DCMPLX(MHR*MHR)
      ALFA=DCMPLX(ALFAR)
      GMU=DCMPLX(GMUR)
      CWSQ=MWSQ/MZSQ
      SWSQ=1.D0-CWSQ
C   COMPUTES NEW GMU = GMUSTAR(0) WITH NON-ABELIAN BOXES AND VERTICES
      DELVER=(ALFA/(2.D0*PI*SWSQ))
     C      *(3.D0-(CWSQ-3.D0*(CWSQ/SWSQ))*CDLOG(CWSQ))
      DELBOX=-(ALFA/(8.D0*PI))
     C      *(5.D0*(CWSQ/SWSQ)**2-3.D0)*CDLOG(CWSQ)
      GMU=GMU/(1.D0+DELVER+DELBOX)
C  CALL RHOSTR: CONVERT TO REAL
      S=-DCMPLX(SR)
      IF(SR.EQ.0.D0)S=(0.D0,0.D0)
      RHSTRE=DREAL(RHOSTR(S,IND))
      RETURN
      END
      FUNCTION MPAARE(SR,IND)
C  MPIAAP CONVERTED TO REAL FUNCTION
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      REAL*8 MPAARE,SR
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C       SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/PRMGSW/ALFAR,GMUR,MWR,MZR,MHR
      REAL*8 ALFAR,GMUR,MWR,MZR,MHR
      COMMON/FERMAS/NLPT,NQRK,XLPTR,XQRKR
      REAL*8 XLPTR(40,2),XQRKR(40,2)
C  EXTERNAL ENTRY: INITIALIZE
      RHOTRE=(1.D0,0.D0)
      XSC=(100.D0)**2
      PI=(3.141592653589793238462643D0,0.D0)
      NGEN=INT(NQRK/2)
      QCD=1
      DQQ=1
      D3Q=0
      MIX=0
C  EXTERNAL ENTRY: CONVERT PARAMETERS TO COMPLEX
      DO 10 I=1,NQRK
        XQRK(I,1)=DCMPLX(XQRKR(I,1)*XQRKR(I,1))
        IF(XQRKR(I,1).EQ.0.D0)XQRK(I,1)=(0.D0,0.D0)
        XQRK(I,2)=DCMPLX(XQRKR(I,2))
        IF(XQRKR(I,2).EQ.0.D0)XQRK(I,2)=(0.D0,0.D0)
   10 CONTINUE
      DO 20 I=1,NLPT
        XLPT(I,1)=DCMPLX(XLPTR(I,1)*XLPTR(I,1))
        IF(XLPTR(I,1).EQ.0.D0)XLPT(I,1)=(0.D0,0.D0)
        XLPT(I,2)=DCMPLX(XLPTR(I,2))
        IF(XLPTR(I,2).EQ.0.D0)XLPT(I,2)=(0.D0,0.D0)
   20 CONTINUE
      MWSQ=DCMPLX(MWR*MWR)
      MZSQ=DCMPLX(MZR*MZR)
      MHSQ=DCMPLX(MHR*MHR)
      ALFA=DCMPLX(ALFAR)
      GMU=DCMPLX(GMUR)
      CWSQ=MWSQ/MZSQ
      SWSQ=1.D0-CWSQ
C   COMPUTES NEW GMU = GMUSTAR(0) WITH NON-ABELIAN BOXES AND VERTICES
      DELVER=(ALFA/(2.D0*PI*SWSQ))
     C      *(3.D0-(CWSQ-3.D0*(CWSQ/SWSQ))*CDLOG(CWSQ))
      DELBOX=-(ALFA/(8.D0*PI))
     C      *(5.D0*(CWSQ/SWSQ)**2-3.D0)*CDLOG(CWSQ)
      GMU=GMU/(1.D0+DELVER+DELBOX)
C  CALL MPIAAP: CONVERT TO REAL
      S=-DCMPLX(SR)
      IF(SR.EQ.0.D0)S=(0.D0,0.D0)
      MPAARE=DREAL(MPIAAP(S,IND))
      RETURN
      END
      FUNCTION MPZARE(SR,IND)
C  MPIZAP CONVERTED TO REAL FUNCTION
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      REAL*8 MPZARE,SR
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C       SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/PRMGSW/ALFAR,GMUR,MWR,MZR,MHR
      REAL*8 ALFAR,GMUR,MWR,MZR,MHR
      COMMON/FERMAS/NLPT,NQRK,XLPTR,XQRKR
      REAL*8 XLPTR(40,2),XQRKR(40,2)
C  EXTERNAL ENTRY: INITIALIZE
      RHOTRE=(1.D0,0.D0)
      XSC=(100.D0)**2
      PI=(3.141592653589793238462643D0,0.D0)
      NGEN=INT(NQRK/2)
      QCD=1
      DQQ=1
      D3Q=0
      MIX=0
C  EXTERNAL ENTRY: CONVERT PARAMETERS TO COMPLEX
      DO 10 I=1,NQRK
        XQRK(I,1)=DCMPLX(XQRKR(I,1)*XQRKR(I,1))
        IF(XQRKR(I,1).EQ.0.D0)XQRK(I,1)=(0.D0,0.D0)
        XQRK(I,2)=DCMPLX(XQRKR(I,2))
        IF(XQRKR(I,2).EQ.0.D0)XQRK(I,2)=(0.D0,0.D0)
   10 CONTINUE
      DO 20 I=1,NLPT
        XLPT(I,1)=DCMPLX(XLPTR(I,1)*XLPTR(I,1))
        IF(XLPTR(I,1).EQ.0.D0)XLPT(I,1)=(0.D0,0.D0)
        XLPT(I,2)=DCMPLX(XLPTR(I,2))
        IF(XLPTR(I,2).EQ.0.D0)XLPT(I,2)=(0.D0,0.D0)
   20 CONTINUE
      MWSQ=DCMPLX(MWR*MWR)
      MZSQ=DCMPLX(MZR*MZR)
      MHSQ=DCMPLX(MHR*MHR)
      ALFA=DCMPLX(ALFAR)
      GMU=DCMPLX(GMUR)
      CWSQ=MWSQ/MZSQ
      SWSQ=1.D0-CWSQ
C   COMPUTES NEW GMU = GMUSTAR(0) WITH NON-ABELIAN BOXES AND VERTICES
      DELVER=(ALFA/(2.D0*PI*SWSQ))
     C      *(3.D0-(CWSQ-3.D0*(CWSQ/SWSQ))*CDLOG(CWSQ))
      DELBOX=-(ALFA/(8.D0*PI))
     C      *(5.D0*(CWSQ/SWSQ)**2-3.D0)*CDLOG(CWSQ)
      GMU=GMU/(1.D0+DELVER+DELBOX)
C  CALL MPIZAP: CONVERT TO REAL
      S=-DCMPLX(SR)
      IF(SR.EQ.0.D0)S=(0.D0,0.D0)
      MPZARE=DREAL(MPIZAP(S,IND))
      RETURN
      END
      FUNCTION ZWIDRE(SR,IND)
C  ZWIDTH CONVERTED TO REAL FUNCTION
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      REAL*8 ZWIDRE,SR
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C       SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/PRMGSW/ALFAR,GMUR,MWR,MZR,MHR
      REAL*8 ALFAR,GMUR,MWR,MZR,MHR
      COMMON/FERMAS/NLPT,NQRK,XLPTR,XQRKR
      REAL*8 XLPTR(40,2),XQRKR(40,2)
C  EXTERNAL ENTRY: INITIALIZE
      RHOTRE=(1.D0,0.D0)
      XSC=(100.D0)**2
      PI=(3.141592653589793238462643D0,0.D0)
      NGEN=INT(NQRK/2)
      QCD=1
      DQQ=1
      D3Q=0
      MIX=0
C  EXTERNAL ENTRY: CONVERT PARAMETERS TO COMPLEX
      DO 10 I=1,NQRK
        XQRK(I,1)=DCMPLX(XQRKR(I,1)*XQRKR(I,1))
        IF(XQRKR(I,1).EQ.0.D0)XQRK(I,1)=(0.D0,0.D0)
        XQRK(I,2)=DCMPLX(XQRKR(I,2))
        IF(XQRKR(I,2).EQ.0.D0)XQRK(I,2)=(0.D0,0.D0)
   10 CONTINUE
      DO 20 I=1,NLPT
        XLPT(I,1)=DCMPLX(XLPTR(I,1)*XLPTR(I,1))
        IF(XLPTR(I,1).EQ.0.D0)XLPT(I,1)=(0.D0,0.D0)
        XLPT(I,2)=DCMPLX(XLPTR(I,2))
        IF(XLPTR(I,2).EQ.0.D0)XLPT(I,2)=(0.D0,0.D0)
   20 CONTINUE
      MWSQ=DCMPLX(MWR*MWR)
      MZSQ=DCMPLX(MZR*MZR)
      MHSQ=DCMPLX(MHR*MHR)
      ALFA=DCMPLX(ALFAR)
      GMU=DCMPLX(GMUR)
      CWSQ=MWSQ/MZSQ
      SWSQ=1.D0-CWSQ
C   COMPUTES NEW GMU = GMUSTAR(0) WITH NON-ABELIAN BOXES AND VERTICES
      DELVER=(ALFA/(2.D0*PI*SWSQ))
     C      *(3.D0-(CWSQ-3.D0*(CWSQ/SWSQ))*CDLOG(CWSQ))
      DELBOX=-(ALFA/(8.D0*PI))
     C      *(5.D0*(CWSQ/SWSQ)**2-3.D0)*CDLOG(CWSQ)
      GMU=GMU/(1.D0+DELVER+DELBOX)
C  CALL ZWIDTH: CONVERT TO REAL
      S=-DCMPLX(SR)
      IF(SR.EQ.0.D0)S=(0.D0,0.D0)
      ZWIDRE=DREAL(ZWIDTH(S,IND))
      RETURN
      END
      FUNCTION B(N,S,M1SQ,M2SQ,XSC)
C   B BLOCK: DEFINES PRIMITIVE B FORM FACTORS
C  GENERATES -B0,B1,-B21 FOR N=1,2,3
C  M2SQ IS EXTERNALLY ARRANGED TO BE NON-ZERO USING IDENTITIES
C  FOR SWITCHING MASS ARGUEMENTS IN THE B'S
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      XN=DCMPLX(DFLOAT(N))
      IF(S.NE.0.D0)THEN
        RS=1.D0/S
        CALL QDRT(-S,S+M2SQ-M1SQ,M1SQ,X1,X2)
        B=(CDLN(-S/XSC,(-1.D0,0.D0))+FPLN(N,X1,-RS)+FPLN(N,X2,RS))/XN
        IF(DREAL(S).GE.-DREAL(CDSQRT(M1SQ)+CDSQRT(M2SQ))**2)B=DREAL(B)
        RETURN
      END IF
      B=CDLN(M2SQ/XSC,(-1.D0,0.D0))/XN
      IF(M1SQ.NE.M2SQ)THEN
        RMD=1.D0/(M1SQ-M2SQ)
        B=B+F(N,M1SQ*RMD,-RMD)/XN
      END IF
      B=DREAL(B)
      RETURN
      END
      FUNCTION FPLN(N,X,A)
C  EVALUATES F(N,X+I*REAL(A))+LOG(X+I*REAL(A)), WHERE A IS INFINI-
C  TESIMAL, TAKING ACCOUNT OF STRONG NUMERICAL CANCELLATION
C  NEAR X=1 AND X=0
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      PI=3.141592653589793238462643D0
      IF(CDABS(X-1.D0).GE.5.D-2.AND.CDABS(X).GE.5.D-2)THEN
        FPLN=CDLN(1.D0-X,-A)+F(N,X,A)
        RETURN
      END IF
      PROD=1.D0
      FPLN=-1.D0/DCMPLX(DFLOAT(N))
      DO 10 I=1,N-1
        PROD=PROD*X
10      FPLN=FPLN-PROD/DCMPLX(DFLOAT(N-I))
      IF(CDABS(X-1.D0).LT.1.D-20)THEN
        FPLN=FPLN-(0.D0,1.D0)*PI*DSIGN(1.D0,DREAL(A))
        RETURN
      ENDIF
      IF(CDABS(X).LT.1.D-20)RETURN
      XN=PROD*X
      FPLN=FPLN+(1.D0-XN)*CDLN(1.D0-X,-A)+XN*CDLN(-X,-A)
      RETURN
      END
      FUNCTION F(N,X,A)
C  EVALUATES THE FUNCTION F(N,X+i*REAL(A)) WHERE A IS AN INFINITESIMAL
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
C  IF ABS(X)<4, F IS EVALUATED DIRECTLY IN TERMS OF LOGARITHMS
C  OTHERWISE, F IS EVALUATED AS A SERIES
      IF(CDABS(X).LE.4.D0)THEN
        PROD=1.D0
        F=-1.D0/DCMPLX(DFLOAT(N))
        IF(CDABS(X).LT.1.D-08)RETURN
        DO 10 I=1,N-1
          PROD=PROD*X
10        F=F-PROD/DCMPLX(DFLOAT(N-I))
        F=F-PROD*X*CDLN(1.D0-1.D0/X,A)
      ELSE
        PROD=X
        M=N+1
        F=1.D0/(M*X)
20        PROD=PROD*X
          M=M+1.D0
          TERM=1.D0/(M*PROD)
          F=F+TERM
        IF(CDABS(TERM/F).GT.1.D-20)GO TO 20
      END IF
      RETURN
      END
      FUNCTION B0(S,M1SQ,M2SQ,XSC)
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      IF(M2SQ.NE.0.D0)THEN
        B0=-B(1,S,M1SQ,M2SQ,XSC)
      ELSE
        B0=-B(1,S,M2SQ,M1SQ,XSC)
      END IF
      RETURN
      END
      FUNCTION B1(S,M1SQ,M2SQ,XSC)
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      IF(M2SQ.NE.0.D0)THEN
        B1=B(2,S,M1SQ,M2SQ,XSC)
      ELSE
        B1=B(1,S,M2SQ,M1SQ,XSC)-B(2,S,M2SQ,M1SQ,XSC)
      END IF
      RETURN
      END
      FUNCTION B3(S,M1SQ,M2SQ,XSC)
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      IF(M2SQ.NE.0.D0)THEN
        B3=B(2,S,M1SQ,M2SQ,XSC)-B(3,S,M1SQ,M2SQ,XSC)
      ELSE
        B3=B(2,S,M2SQ,M1SQ,XSC)-B(3,S,M2SQ,M1SQ,XSC)
      END IF
      RETURN
      END
      FUNCTION ALFQCD(S)
C   QCD BLOCK: DEFINES QCD CORRECTION FACTORS
C  RS-INVARIANT RUNNING QCD COUPLING
C  REVIEW OF PARTICLE PROPERTIES, PHYS. LETT. 170B, APRIL 1986
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      COMPLEX*16 LAMBDA
      INTEGER*4 QCD,DQQ,D3Q,MIX
C... PG
      REAL*8 AQCD
      COMMON/ALFSTR/AQCD
C...
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C      SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
      IF(S.EQ.0.D0)THEN
        ALFQCD=0.D0
        RETURN
      ENDIF
      ABS=CDABS(S)
C  LAMBDA IS QCD SCALE PARAMETER, SET TO 300 MEV
      LAMBDA=(0.300D0)**2
C  USE THREE LIGHT QUARK FLAVORS; TEST FOR THRESHOLD OF HEAVY QUARKS
      NFLAV=3
      MCHRM2=XQRK(3,1)
      MTOP2=XQRK(5,1)
      MBTTM2=XQRK(6,1)
      IF(DREAL(S).LT.-4.D0*DREAL(MCHRM2))NFLAV=4
      IF(DREAL(S).LT.-4.D0*DREAL(MBTTM2))NFLAV=5
      IF(DREAL(S).LT.-4.D0*DREAL(MTOP2))NFLAV=6
      NFAC1=33-2*NFLAV
      NFAC2=6*(153-19*NFLAV)
      FACLOG=CDLOG(ABS/LAMBDA)
C  ALFAC1 IS SECOND-ORDER RUNNING COUPLING
      ALFAC1=(12.D0*PI)/(DCMPLX(DFLOAT(NFAC1))*FACLOG)
      TEMP1=DCMPLX(DFLOAT(NFAC2/NFAC1**2))
      TEMP2=CDLOG(FACLOG)/FACLOG
C  ALFAC2 CONVERTS ALFAC1 TO FOURTH-ORDER
      ALFAC2=1.D0-TEMP1*TEMP2
      ALFQCD=ALFAC1*ALFAC2
CC-RM & PG
      ALFQCD=CMPLX(AQCD)
CC-RM
      RETURN
      END
      FUNCTION OPI33T(S)
C   PI BLOCK: DEFINES PROPER SELF-ENERGIES
C  ONE-PARTICLE IRREDUCIBLE 33 BOSON SELF-ENERGY: TRANSVERSE
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C      SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
     .       /NFAM/NF
C  BOSONIC CONTRIBUTIONS
      BOPI33=-0.25D0*(4.D0*B3(S,MZSQ,MHSQ,XSC)+B0(S,MZSQ,MHSQ,XSC))
     C      +(2.D0/3.D0)-9.D0*B3(S,MWSQ,MWSQ,XSC)
     C      +(7.D0/4.D0)*B0(S,MWSQ,MWSQ,XSC)
C  LEPTONIC CONTRIBUTIONS
      FOPI33=0.D0
      DO 10 I=1,2*NF
        DT3SQ=(1.D0/2.D0)**2
        MSQ=XLPT(I,1)
        QSQ=XLPT(I,2)*XLPT(I,2)
        IF(S.EQ.0.D0.AND.MSQ.EQ.0.D0)GO TO 10
        VEC=1.D0+QSQ*(3.D0*ALFA)/(4.D0*PI)
        AXI=1.D0+QSQ*(3.D0*ALFA)/(4.D0*PI)
        FOPI33=FOPI33+2.D0*DT3SQ*
     C        (DREAL(2.D0*B3(S,MSQ,MSQ,XSC))
     C        +(0.D0,1.D0)*(VEC+AXI)*DIMAG(B3(S,MSQ,MSQ,XSC)))
10    CONTINUE
C  QUARK CONTRIBUTIONS
      QOPI33=0.D0
      DO 20 I=1,2*NGEN
        DT3SQ=(1.D0/2.D0)**2
        QSQ=XQRK(I,2)*XQRK(I,2)
        MSQ=XQRK(I,1)
        IF(S.EQ.0.D0.AND.MSQ.EQ.0.D0)GO TO 20
C  QCD CORRECTIONS TO VECTOR AND AXIAL COUPLINGS
        IF(QCD.EQ.1)THEN
          VEC=(1.D0+ALFQCD(S)/PI)*(1.D0+QSQ*(3.D0*ALFA)/(4.D0*PI))
          AXI=(1.D0+ALFQCD(S)/PI)*(1.D0+QSQ*(3.D0*ALFA)/(4.D0*PI))
        ELSE
          VEC=1.D0+QSQ*(3.D0*ALFA)/(4.D0*PI)
          AXI=1.D0+QSQ*(3.D0*ALFA)/(4.D0*PI)
        ENDIF
        QOPI33=QOPI33+2.D0*DT3SQ*
     C        (DREAL(2.D0*B3(S,MSQ,MSQ,XSC))
     C        +(0.D0,1.D0)*(VEC+AXI)*DIMAG(B3(S,MSQ,MSQ,XSC)))
20    CONTINUE
      OPI33T=(BOPI33+FOPI33+3.D0*QOPI33)/(16.D0*PI**2)
      RETURN
      END
      FUNCTION OPI33L(S)
C  ONE-PARTICLE IRREDUCIBLE 33 BOSON SELF-ENERGY: LONGITUDINAL
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C      SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
     .       /NFAM/NF
C  BOSONIC CONTRIBUTIONS
      BOPI33=MZSQ*B0(S,MZSQ,MHSQ,XSC)-2.D0*MWSQ*B0(S,MWSQ,MWSQ,XSC)
     C      +0.25D0*(MZSQ-MHSQ)*(2.D0*B1(S,MZSQ,MHSQ,XSC)
     C                          +B0(S,MZSQ,MHSQ,XSC))
C  LEPTONIC CONTRIBUTIONS
      FOPI33=0.D0
      DO 10 I=1,2*NF
        DT3SQ=(1.D0/2.D0)**2
        MSQ=XLPT(I,1)
        QSQ=XLPT(I,2)*XLPT(I,2)
        IF(S.EQ.0.D0.AND.MSQ.EQ.0.D0)GO TO 10
        AXI=1.D0+QSQ*(3.D0*ALFA)/(4.D0*PI)
        FOPI33=FOPI33-2.D0*DT3SQ*MSQ*
     C        (DREAL(B0(S,MSQ,MSQ,XSC))
     C        +(0.D0,1.D0)*AXI*DIMAG(B0(S,MSQ,MSQ,XSC)))
10    CONTINUE
C  QUARK CONTRIBUTIONS
      QOPI33=0.D0
      DO 20 I=1,2*NGEN
        DT3SQ=(1.D0/2.D0)**2
        MSQ=XQRK(I,1)
        IF(S.EQ.0.D0.AND.MSQ.EQ.0.D0)GO TO 20
C  QCD CORRECTIONS TO VECTOR AND AXIAL COUPLINGS
        IF(QCD.EQ.1)THEN
          AXI=(1.D0+ALFQCD(S)/PI)*(1.D0+QSQ*(3.D0*ALFA)/(4.D0*PI))
        ELSE
          AXI=1.D0+QSQ*(3.D0*ALFA)/(4.D0*PI)
        ENDIF
        QOPI33=QOPI33-2.D0*DT3SQ*MSQ*
     C        (DREAL(B0(S,MSQ,MSQ,XSC))
     C        +(0.D0,1.D0)*AXI*DIMAG(B0(S,MSQ,MSQ,XSC)))
20    CONTINUE
      OPI33L=(BOPI33+FOPI33+3.D0*QOPI33)/(16.D0*PI**2)
      RETURN
      END
      FUNCTION OPI11T(S)
C  ONE-PARTICLE IRREDUCIBLE 11 BOSON SELF-ENERGY: TRANSVERSE
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C      SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
     .       /NFAM/NF
      ZERO=0.D0
C  BOSONIC CONTRIBUTIONS
      BOPI11=-B3(S,MWSQ,MZSQ,XSC)-0.25D0*B0(S,MWSQ,MZSQ,XSC)
     C +SWSQ*(-8.D0*B3(S,MWSQ,ZERO,XSC)+2.D0*B0(S,MWSQ,ZERO,XSC))
     C +CWSQ*(-8.D0*B3(S,MWSQ,MZSQ,XSC)+2.D0*B0(S,MWSQ,MZSQ,XSC))
     C      -0.25D0*(4.D0*B3(S,MWSQ,MHSQ,XSC)+B0(S,MWSQ,MHSQ,XSC))
     C      +(2.D0/3.D0)
C  LEPTONIC CONTRIBUTIONS
      FOPI11=0.D0
      DO 10 I=1,2*NF-1,2
        M1SQ=XLPT(I,1)
        M2SQ=XLPT(I+1,1)
        Q1SQ=XLPT(I,2)*XLPT(I,2)
        Q2SQ=XLPT(I+1,2)*XLPT(I+1,2)
        IF(S.EQ.0.D0.AND.M1SQ.EQ.0.D0.AND.M2SQ.EQ.0.D0)GO TO 10
        QEDFAC=1.D0+((Q1SQ+Q2SQ)/2.D0)*(3.D0*ALFA)/(4.D0*PI)
        FOPI11=FOPI11+2.D0*
     C        (DREAL(B3(S,M1SQ,M2SQ,XSC))
     C        +(0.D0,1.D0)*QEDFAC*DIMAG(B3(S,M1SQ,M2SQ,XSC)))
10    CONTINUE
C  QUARK CONTRIBUTIONS
      QOPI11=0.D0
      DO 20 I=1,2*NGEN-1,2
        M1SQ=XQRK(I,1)
        M2SQ=XQRK(I+1,1)
        Q1SQ=XQRK(I,2)*XQRK(I,2)
        Q2SQ=XQRK(I+1,2)*XQRK(I+1,2)
        IF(S.EQ.0.D0.AND.M1SQ.EQ.0.D0.AND.M2SQ.EQ.0.D0)GO TO 20
C  CKM MIXING FOR FIRST THREE GENERATIONS INCLUDED
C  PERTURBATIVE QCD CORRECTIONS INCLUDED FOR LIGHT QUARKS
        IF(QCD.EQ.1)THEN
          QCDFAC=(1.D0+ALFQCD(S)/PI)*
     C          (1.D0+((Q1SQ+Q2SQ)/2.D0)*(3.D0*ALFA)/(4.D0*PI))
        ELSE
          QCDFAC=1.D0+((Q1SQ+Q2SQ)/2.D0)*(3.D0*ALFA)/(4.D0*PI)
        ENDIF
        IF(I.LE.6.AND.MIX.EQ.1)THEN
          DO 15 J=1,3
            M1SQ=XQRK(I,1)
            M2SQ=XQRK(2*J,1)
            QOPI11=QOPI11
     C            +CKM(INT((I+1)/2),J)*QCDFAC*
     C             2.D0*B3(S,M1SQ,M2SQ,XSC)
15        CONTINUE
        ELSE
          QOPI11=QOPI11+2.D0*
     C          (DREAL(B3(S,M1SQ,M2SQ,XSC))
     C          +(0.D0,1.D0)*QCDFAC*DIMAG(B3(S,M1SQ,M2SQ,XSC)))
        ENDIF
20    CONTINUE
      OPI11T=(BOPI11+FOPI11+3.D0*QOPI11)/(16.D0*PI**2)
      RETURN
      END
      FUNCTION OPI11L(S)
C  ONE-PARTICLE IRREDUCIBLE 11 BOSON SELF-ENERGY: LONGITUDINAL
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C      SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
     .       /NFAM/NF
      ZERO=0.D0
C  BOSONIC CONTRIBUTIONS
      BOPI11=0.25D0*(MWSQ-MHSQ)*(2.D0*B1(S,MWSQ,MHSQ,XSC)
     C                          +B0(S,MWSQ,MHSQ,XSC))
     C      +MWSQ*B0(S,MWSQ,MHSQ,XSC)
     C      +(MZSQ-3.D0*MWSQ)*B0(S,MWSQ,MZSQ,XSC)
     C +SWSQ*2.D0*MWSQ*(2.D0*B1(S,MWSQ,ZERO,XSC)+B0(S,MWSQ,ZERO,XSC))
     C -CWSQ*2.D0*MZSQ*SWSQ*(2.D0*B1(S,MWSQ,MZSQ,XSC)
     C                      +B0(S,MWSQ,MZSQ,XSC))
     C      -0.25D0*MZSQ*SWSQ*(2.D0*B1(S,MWSQ,MZSQ,XSC)
     C                        +B0(S,MWSQ,MZSQ,XSC))
C  LEPTONIC CONTRIBUTIONS
      FOPI11=0.D0
      DO 10 I=1,2*NF-1,2
        M1SQ=XLPT(I,1)
        M2SQ=XLPT(I+1,1)
        Q1SQ=XLPT(I,2)*XLPT(I,2)
        Q2SQ=XLPT(I+1,2)*XLPT(I+1,2)
        QEDFAC=1.D0+((Q1SQ+Q2SQ)/2.D0)*(3.D0*ALFA)/(4.D0*PI)
        IF(S.EQ.0.D0.AND.M1SQ.EQ.0.D0.AND.M2SQ.EQ.0.D0)GO TO 10
        FOPI11=FOPI11
     C        +M2SQ*(DREAL(B1(S,M1SQ,M2SQ,XSC))
     C              +(0.D0,1.D0)*QEDFAC*DIMAG(B1(S,M1SQ,M2SQ,XSC)))
     C        +M1SQ*(DREAL(B1(S,M2SQ,M1SQ,XSC))
     C              +(0.D0,1.D0)*QEDFAC*DIMAG(B1(S,M2SQ,M1SQ,XSC)))
10    CONTINUE
C  QUARK CONTRIBUTIONS
      QOPI11=0.D0
      DO 20 I=1,2*NGEN-1,2
        M1SQ=XQRK(I,1)
        M2SQ=XQRK(I+1,1)
        Q1SQ=XQRK(I,2)*XQRK(I,2)
        Q2SQ=XQRK(I+1,2)*XQRK(I+1,2)
        IF(S.EQ.0.D0.AND.M1SQ.EQ.0.D0.AND.M2SQ.EQ.0.D0)GO TO 20
C  CKM MIXING FOR FIRST THREE GENERATIONS INCLUDED
C  PERTURBATIVE QCD CORRECTIONS INCLUDED FOR LIGHT QUARKS
        IF(QCD.EQ.1)THEN
          QCDFAC=(1.D0+ALFQCD(S)/PI)*
     C           (1.D0+((Q1SQ+Q2SQ)/2.D0)*(3.D0*ALFA)/(4.D0*PI))
        ELSE
          QCDFAC=1.D0+((Q1SQ+Q2SQ)/2.D0)*(3.D0*ALFA)/(4.D0*PI)
        ENDIF
        IF(I.LE.6.AND.MIX.EQ.1)THEN
          DO 15 J=1,3
            M1SQ=XQRK(I,1)
            M2SQ=XQRK(2*J,1)
            QOPI11=QOPI11
     C            +CKM(INT((I+1)/2),J)*QCDFAC*
     C             (M2SQ*B1(S,M1SQ,M2SQ,XSC)
     C             +M1SQ*B1(S,M2SQ,M1SQ,XSC))
15        CONTINUE
        ELSE
          QOPI11=QOPI11
     C          +M2SQ*(DREAL(B1(S,M1SQ,M2SQ,XSC))
     C                +(0.D0,1.D0)*QCDFAC*DIMAG(B1(S,M1SQ,M2SQ,XSC)))
     C          +M1SQ*(DREAL(B1(S,M2SQ,M1SQ,XSC))
     C                +(0.D0,1.D0)*QCDFAC*DIMAG(B1(S,M2SQ,M1SQ,XSC)))
        ENDIF
20    CONTINUE
      OPI11L=(BOPI11+FOPI11+3.D0*QOPI11)/(16.D0*PI**2)
      RETURN
      END
      FUNCTION OPIQQP(S)
C  ONE-PARTICLE IRREDUCIBLE QQ SELF-ENERGY, WITH Q2=S REMOVED
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C      SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
     .       /NFAM/NF
C  BOSONIC CONTRIBUTION
      BOPIQQ=-12.D0*B3(S,MWSQ,MWSQ,XSC)
     C      +B0(S,MWSQ,MWSQ,XSC)
     C      +(2.D0/3.D0)
C  LEPTONIC CONTRIBUTION
      FOPIQQ=0.D0
      DO 10 I=1,2*NF
        QSQ=XLPT(I,2)*XLPT(I,2)
        MSQ=XLPT(I,1)
        QEDFAC=1.D0+QSQ*(3.D0*ALFA)/(4.D0*PI)
        IF(QSQ.NE.0.D0)FOPIQQ=FOPIQQ+8.D0*QSQ*
     C                        (DREAL(B3(S,MSQ,MSQ,XSC))
     C                        +(0.D0,1.D0)*QEDFAC*
     C                         DIMAG(B3(S,MSQ,MSQ,XSC)))
10    CONTINUE
C  QUARK CONTRIBUTION
      P=S
C  IF S LESS THAN 79 GEV**2, USE TASSO DISPERSION RELATION
C  FOR LIGHT QUARKS
      IF(CDABS(S).LT.79.D0)P=79.D0
      QOPIQQ=0.D0
      DO 20 I=1,2*NGEN
        QSQ=XQRK(I,2)*XQRK(I,2)
        MSQ=XQRK(I,1)
        IF(QSQ.EQ.0.D0)GO TO 20
C  HEAVY QUARK CONTRIBUTIONS CALCULATED PERTURBATIVELY
C  PERTURBATIVE QCD CORRECTIONS INCLUDED
        IF(QCD.EQ.1)THEN
          QCDFAC=(1.D0+ALFQCD(S)/PI)*(1.D0+QSQ*(3.D0*ALFA)/(4.D0*PI))
        ELSE
          QCDFAC=1.D0+QSQ*(3.D0*ALFA)/(4.D0*PI)
        ENDIF
        IF(CDABS(S).LT.79.AND.I.NE.5.AND.I.LE.6.AND.NGEN.GE.3.AND.
     C    DQQ.EQ.1)THEN
          QOPIQQ=QOPIQQ+8.D0*QSQ*B3(P,MSQ,MSQ,XSC)
        ELSE
          QOPIQQ=QOPIQQ+8.D0*QSQ*
     C          (DREAL(B3(S,MSQ,MSQ,XSC))
     C          +(0.D0,1.D0)*QCDFAC*DIMAG(B3(S,MSQ,MSQ,XSC)))
        END IF
20    CONTINUE
      OPIQQP=(BOPIQQ+FOPIQQ+3.D0*QOPIQQ)/(16.D0*PI**2)
C  ADD CORRECTION FROM HADRON DATA
      IF(CDABS(S).LT.79.AND.NGEN.GE.3.AND.DQQ.EQ.1)
     C  OPIQQP=OPIQQP-HADRQQ(P)+HADRQQ(S)
      RETURN
      END
      FUNCTION HADRQQ(S)
C  HADRONIC IRREDUCIBLE QQ SELF-ENERGY: TRANSVERSE
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      INTEGER*4 QCD,DQQ,D3Q,MIX
      REAL*8 AB
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C      SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
C  DIPERSION RELATION: H. BURKHARDT, TASSO NOTE NO. 192, 1981
C  MODIFIED SLIGHTLY BY ROBIN G. STUART
      AB=CDABS(S)
      IF(AB.LE.1.D0)
     C  HADRQQ=(2.3020D-03)*DLOG(1.D0+4.091D0*AB)
      IF(1.D0.LT.AB.AND.AB.LE.64.D0)
     C  HADRQQ=1.5120D-03+(2.8220D-03)*DLOG(1.D0+1.218D0*AB)
      IF(64.D0.LT.AB.AND.AB.LT.1600.D0)
     C  HADRQQ=1.3144D-03+(3.0680D-03)*DLOG(1.D0+0.99992D0*AB)
      IF(AB.GE.1600.D0)
     C  HADRQQ=0.D0
      HADRQQ=HADRQQ/(4.D0*PI*ALFA)
      RETURN
      END
      FUNCTION OPI3QP(S)
C  ONE-PARTICLE IRREDUCIBLE 3Q MIXING: TRANSVERSE
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C      SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
     .       /NFAM/NF
C  BOSONIC CONTRIBUTION
      BOPI3Q=(2.D0/3.D0)+(3.D0/2.D0)*B0(S,MWSQ,MWSQ,XSC)
     C      -10.D0*B3(S,MWSQ,MWSQ,XSC)
C  LEPTONIC CONTRIBUTION
      FOPI3Q=0.D0
      DO 10 I=1,2*NF
        Q=XLPT(I,2)
        QSQ=Q*Q
        DT3=Q+(1.D0/2.D0)
        MSQ=XLPT(I,1)
        IF(Q.EQ.0.D0)GO TO 10
        QEDFAC=1.D0+QSQ*(3.D0*ALFA)/(4.D0*PI)
        FOPI3Q=FOPI3Q+4.D0*DT3*Q*
     C        (DREAL(B3(S,MSQ,MSQ,XSC))
     C        +(0.D0,1.D0)*QEDFAC*DIMAG(B3(S,MSQ,MSQ,XSC)))
10    CONTINUE
C  QUARK CONTRIBUTION
      P=S
C  IF S LESS THAN 79 GEV**2, USE TASSO DISPERSION RELATION
C  FOR LIGHT QUARKS
      IF(CDABS(S).LT.79.D0)P=79.D0
      QOPI3Q=0.D0
      DO 20 I=1,2*NGEN
        Q=XQRK(I,2)
        QSQ=Q*Q
        DT3=Q-(1.D0/6.D0)
        MSQ=XQRK(I,1)
        IF(Q.EQ.0.D0)GO TO 20
C  HEAVY QUARK CONTIBUTIONS CALCULATED PERTURBATIVELY
C  PERTURBATIVE QCD CORRECTIONS INCLUDED
        IF(QCD.EQ.1)THEN
          QCDFAC=(1.D0+ALFQCD(S)/PI)*(1.D0+QSQ*(3.D0*ALFA)/(4.D0*PI))
        ELSE
          QCDFAC=1.D0+QSQ*(3.D0*ALFA)/(4.D0*PI)
        ENDIF
        IF(CDABS(S).LT.40.AND.I.NE.5.AND.I.LE.6.AND.NGEN.GE.3.AND.
     C    D3Q.EQ.1)THEN
          QOPI3Q=QOPI3Q+4.D0*DT3*Q*B3(P,MSQ,MSQ,XSC)
        ELSE
          QOPI3Q=QOPI3Q+4.D0*DT3*Q*
     C          (DREAL(B3(S,MSQ,MSQ,XSC))
     C          +(0.D0,1.D0)*QCDFAC*DIMAG(B3(S,MSQ,MSQ,XSC)))
        ENDIF
20    CONTINUE
      OPI3QP=(BOPI3Q+FOPI3Q+3.D0*QOPI3Q)/(16.D0*PI**2)
C  ADD CORRECTION FROM HADRON DATA
CMMR      IF(CDABS(S).LT.79.AND.NGEN.GE.3.AND.D3Q.EQ.1)
CMMR     C  OPI3QP=OPI3QP-HADR3Q(P)+HADR3Q(S)
      RETURN
      END
      FUNCTION OPI3QL(S)
C  ONE-PARTICLE IRREDUCIBLE 3Q MIXING: LONGITUDINAL
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C       SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
C  BOSONIC CONTRIBUTION
      BOPI3Q=-2.D0*MWSQ*B0(S,MWSQ,MWSQ,XSC)
      OPI3QL=BOPI3Q/(16.D0*PI**2)
      RETURN
      END
      FUNCTION GAMMA3(S)
C   GAMMA BLOCK: DEFINES PROPER NON-ABELIAN VERTEX CORRECTIONS
C  NEUTRAL CURRENT NON-ABELIAN VERTEX
CC-RM GAMMA3 = GAMMA^PRIME + GAMMA^TILDE NON-ABELIAN
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C       SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
CC-RM      GAMMA3=2.D0*B0(S,MWSQ,MWSQ,XSC)
      ZERO = (0.D0,0.D0)
      IF (S.EQ.ZERO) THEN
          GAMMA3= 2.D0*B0(ZERO,MWSQ,MWSQ,XSC)
      ELSE
          GAMMA3=-RHO(-S,MWSQ)-XLMDA(-S,MWSQ)+
     .            2.D0*B0(ZERO,MWSQ,MWSQ,XSC)
      ENDIF
      GAMMA3=GAMMA3/(16.D0*PI**2)
      RETURN
      END
      FUNCTION GNAB(S)
C  NEUTRAL CURRENT NON-ABELIAN VERTEX
CC-RM GNAB = GAMMA^TILDE^NON-ABELIAN
CC-RM CREATED BY R. MIQUEL
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C       SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
      ZERO = (0.D0,0.D0)
      IF (S.EQ.ZERO) THEN
          GNAB= ZERO
      ELSE
          GNAB=-RHO(-S,MWSQ)-XLMDA(-S,MWSQ)+
     .          2.D0*B0(ZERO,MWSQ,MWSQ,XSC)-2.D0*B0(S,MWSQ,MWSQ,XSC)
      ENDIF
      GNAB=GNAB/(16.D0*PI**2)
      END
      FUNCTION DELQ(S)
C   DELTA BLOCK: DEFINES FINITE RS-INVARIANT FUNCTIONS
C  PROPERLY-SUBTRACTED VACUUM POLARIZATION
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      DELQ=DREAL(OPIQQP(S)-OPIQQP((0.D0,0.D0)))
     C    +2.D0*DREAL(GAMMA3(S)-GAMMA3((0.D0,0.D0)))
      RETURN
      END
      FUNCTION DELRHO(S)
C  GLOBAL ISOPSIN BREAKING DELTA-RHO
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C       SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
      DELRHO=DREAL(S*(OPI33T(S)-OPI11T(S)))
     C      +DREAL(OPI33L(S)-OPI11L(S))
      RETURN
      END
      FUNCTION DEL3(S)
C  PROPERLY-SUBTRACTED 33 BOSON SELF-ENERGY
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C       SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
      DEL3=DREAL(S*(OPI33T(S)-OPI3QP(S)))
     C    +DREAL(OPI33L(S)-OPI33L((0.D0,0.D0)))
     C    -DREAL(OPI3QL(S)-OPI3QL((0.D0,0.D0)))
     C    +MWSQ*DREAL(GAMMA3(S)-GAMMA3((0.D0,0.D0)))
      RETURN
      END
      FUNCTION DEL1(S)
C  PROPERLY-SUBTRACTED 11 BOSON SELF-ENERGY
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C       SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
      DEL1=DREAL(S*(OPI11T(S)-OPI3QP(S)))
     C    +DREAL(OPI11L(S)-OPI11L((0.D0,0.D0)))
     C    -DREAL(OPI3QL(S)-OPI3QL((0.D0,0.D0)))
     C    +MWSQ*DREAL(GAMMA3(S)-GAMMA3((0.D0,0.D0)))
      RETURN
      END
      FUNCTION MPIAAP(S,IND)
C   IMAG BLOCK: DEFINES RS-INVARIANT IMAGINARY PARTS
C  RS-INVARIANT IMAGINARY VACUUM POLARIZATION
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      REAL*8 E2TAB
      IF (IND.EQ.0) THEN
          MPIAAP=E2STAR(S,IND)*DIMAG(OPIQQP(S)+2.D0*GAMMA3(S))
      ELSE
          MPIAAP=E2TAB(-S)*DIMAG(OPIQQP(S)+2.D0*GAMMA3(S))
      ENDIF
      RETURN
      END
      FUNCTION MPIZAP(S,IND)
C  RS-INVARIANT IMAGINARY Z-A MIXING
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      REAL*8 E2TAB,S2TAB
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C       SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
      IF (IND.EQ.0) THEN
         S2=S2STAR(S,IND)
         C2=1.D0-S2
         MPIZAP=(E2STAR(S,IND)/CDSQRT(S2*C2))*
CC-RM     C       (DIMAG(OPI3QP(S)+2.D0*GAMMA3(S))
     C       (DIMAG(OPI3QP(S)+2.D0*GAMMA3(S)+MWSQ*GNAB(S)/S)
     C       -S2*DIMAG(OPIQQP(S)+2.D0*GAMMA3(S)))
      ELSE
         S2=S2TAB(-S)
         C2=1.D0-S2
         MPIZAP=(E2TAB(-S)/CDSQRT(S2*C2))*
CC-RM     C       (DIMAG(OPI3QP(S)+2.D0*GAMMA3(S))
     C       (DIMAG(OPI3QP(S)+2.D0*GAMMA3(S)+MWSQ*GNAB(S)/S)
     C       -S2*DIMAG(OPIQQP(S)+2.D0*GAMMA3(S)))
      ENDIF
      RETURN
      END
      FUNCTION MPIZZ(S,IND)
C  RS-INVARIANT IMAGINARY Z-BOSON SELF-ENERGY
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      REAL*8 E2TAB,S2TAB
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C       SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
      IF (IND.EQ.0) THEN
          S2=S2STAR(S,IND)
          C2=1.D0-S2
          MPIZZ=(E2STAR(S,IND)/(S2*C2))*
     C      (S*DIMAG(OPI33T(S)-2.D0*S2*OPI3QP(S)+(S2**2)*OPIQQP(S))
CC-RM     C      +DIMAG(OPI33L(S)+MWSQ*GAMMA3(S)))
     C      +DIMAG(OPI33L(S)+2.D0*S*C2**2*GAMMA3(S)
     C            +2.D0*MZSQ*C2*C2*GNAB(S)-2.D0*OPI3QL(S)))
      ELSE
          S2=S2TAB(-S)
          C2=1.D0-S2
          MPIZZ=(E2TAB(-S)/(S2*C2))*
     C      (S*DIMAG(OPI33T(S)-2.D0*S2*OPI3QP(S)+(S2**2)*OPIQQP(S))
CC-RM     C      +DIMAG(OPI33L(S)+MWSQ*GAMMA3(S)))
     C      +DIMAG(OPI33L(S)+2.D0*S*C2**2*GAMMA3(S)
     C            +2.D0*MZSQ*C2*C2*GNAB(S)-2.D0*OPI3QL(S)))
      ENDIF
      RETURN
      END
      FUNCTION ZWIDTH(S,IND)
C  RS-INVARIANT Z WIDTH IN BREIT-WIGNER FORM
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      ZWIDTH=(MPIZZ(S,IND)+S*MPIAAP(S,IND)*(MPIZAP(S,IND))**2)
     .        /CDSQRT(-S)
      RETURN
      END
      FUNCTION E2STAR(S,IND)
C   STAR BLOCK: DEFINES RS-INVARIANT GWS PARAMETERS
C  RS-INVARIANT RUNNING EM COUPLING
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C       SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
      E2STAR=(4.D0*PI*ALFA)/(1.D0-4.D0*PI*ALFA*DELQ(S))
      RETURN
      END
      FUNCTION GMUSTR(S,IND)
C  RS-INVARIANT RUNNING FERMI CONSTANT
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C       SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
      GMUSTR=GMU/(1.D0-4.D0*DSQRT(2.D0)*GMU*DEL1(S))
      RETURN
      END
      FUNCTION RHOSTR(S,IND)
C  RS-INVARIANT RUNNING RHO PARAMETER
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      REAL*8 GMUTAB
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C       SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
      IF (IND.EQ.0) THEN
          RHOSTR=RHOTRE/(1.D0-RHOTRE*4.D0*DSQRT(2.D0)*GMUSTR(S,IND)
     .          *DELRHO(S))
      ELSE
          RHOSTR=RHOTRE/(1.D0-RHOTRE*4.D0*DSQRT(2.D0)*GMUTAB(-S)
     .          *DELRHO(S))
      ENDIF
      RETURN
      END
      FUNCTION S2STAR(S,IND)
C  RS-INVARIANT RUNNING SINE-WEAK-SQUARED
      IMPLICIT COMPLEX*16 (A-H,M,O-Z)
      COMPLEX*16 LAMBDA
      REAL*8 E2TAB,MPAATB,GMUTAB,RHOTAB
      INTEGER*4 QCD,DQQ,D3Q,MIX
      COMMON/GSWPRM/XQRK(40,2),XLPT(40,2),CKM(3,3),MWSQ,MZSQ,MHSQ,
     C       SWSQ,CWSQ,ALFA,GMU,RHOTRE,XSC,PI,NGEN,QCD,DQQ,D3Q,MIX
      DATA INIT/0/
C  FIRST COMPUTE S2STAR AT S=-MZSQ
      IF     (IND.EQ.0) THEN
              E2STRZ=E2STAR(-MZSQ,IND)
              MPIAAZ=MPIAAP(-MZSQ,IND)
CC-RM              O3QG3Z=OPI3QP(-MZSQ)+2.D0*GAMMA3(-MZSQ)
              O3QG3Z=OPI3QP(-MZSQ)+2.D0*GAMMA3(-MZSQ)
     .                            +MWSQ*GNAB(-MZSQ)/(-MZSQ)
              Y=-E2STRZ*DIMAG(O3QG3Z)
              LAMBDA=(Y**2+Y*MPIAAZ)/(1.D0+MPIAAZ**2)
              XISTAR=E2STRZ/(4.D0*DSQRT(2.D0)*MZSQ
     .               *GMUSTR(-MZSQ,IND)*RHOSTR(-MZSQ,IND))
              BETAZ=(XISTAR/(1.D0+MPIAAZ**2))-LAMBDA
              S2STRZ=(0.5D0)*(1.D0-CDSQRT(1.D0-4.D0*BETAZ))
              S2TEMP=(1.D0+MPIAAZ**2)*(S2STRZ/E2STRZ)
      ELSEIF (IND.EQ.1.AND.INIT.EQ.0) THEN
              INIT = 1
              E2STRZ=E2TAB(MZSQ)
              MPIAAZ=MPAATB(MZSQ)
CC-RM              O3QG3Z=OPI3QP(-MZSQ)+2.D0*GAMMA3(-MZSQ)
              O3QG3Z=OPI3QP(-MZSQ)+2.D0*GAMMA3(-MZSQ)
     .                            +MWSQ*GNAB(-MZSQ)/(-MZSQ)
              Y=-E2STRZ*DIMAG(O3QG3Z)
              LAMBDA=(Y**2+Y*MPIAAZ)/(1.D0+MPIAAZ**2)
              XISTAR=E2STRZ/(4.D0*DSQRT(2.D0)*MZSQ
     .              *GMUTAB(MZSQ)*RHOTAB(MZSQ))
              BETAZ=(XISTAR/(1.D0+MPIAAZ**2))-LAMBDA
              S2STRZ=(0.5D0)*(1.D0-CDSQRT(1.D0-4.D0*BETAZ))
              S2TEMP=(1.D0+MPIAAZ**2)*(S2STRZ/E2STRZ)
      ENDIF
C  SECOND COMPUTE S2STAR AT S USING RG EQUATION
      IF (IND.EQ.0) THEN
          E2STRS=E2STAR(S,IND)
          MPIAAS=MPIAAP(S,IND)
      ELSE
          E2STRS=E2TAB(-S)
          MPIAAS=MPAATB(-S)
      ENDIF
CC-RM      O3QG3S=OPI3QP(S)+2.D0*GAMMA3(S)
      O3QG3S=OPI3QP(S)+2.D0*GAMMA3(S)+MWSQ*GNAB(S)/S
      TEMP3Q=DREAL(O3QG3S
     C            -O3QG3Z)
      TEMPIM=MPIAAS*
     C       DIMAG(O3QG3S)
     C      -MPIAAZ*
     C       DIMAG(O3QG3Z)
      S2STAR=E2STRS*(S2TEMP-TEMP3Q+TEMPIM)/(1.D0+MPIAAS**2)
      RETURN
      END
      SUBROUTINE WMASET
C**********************
C SETS W MASS TO A VALUE CONSISTENT WITH THE MEASURED MUON LIFETIME
      IMPLICIT LOGICAL (A-H,O-Z)
      COMMON/FERMAS/NLPT,NQRK,XLPT,XQRK
      REAL*8 XLPT(40,2),XQRK(40,2)
      COMMON/PRMGSW/ALFA,GMU,MW,MZ,MH
      REAL*8 ALFA,GMU,MW,MZ,MH
      REAL*8 A0SQ,CWSQ,ESQ,FC,GSU2SQ,GMURUN,
     C       MWSQ,MWSQ0,MZSQ,PI,PISQ,SWSQ,
     C       PIAAP,PIZAP,XSC
      REAL*8 E2STRE,GMSTRE,MPAARE,MPZARE,S2STRE
      MZSQ=MZ*MZ
      PI=3.141592653589793238462643D0
      PISQ=PI*PI
      A0SQ=ALFA*PI/(DSQRT(2.D0)*GMU)
C  CALCULATE LOWEST ORDER APPROXIMATION TO MW FROM MUON LIFE-TIME
      MWSQ=MZSQ*(1.D0+DSQRT(1.D0-4.D0*A0SQ/MZSQ))/2.D0
C  ITERATE TO FIND SELF CONSISTENT VALUE OF MW, SWSQ
10    MWSQ0=MWSQ
      MW=DSQRT(MWSQ)
      ESQ=E2STRE(MWSQ,0)
      SWSQ=S2STRE(MWSQ,0)
      CWSQ=1.D0-SWSQ
      GSU2SQ=ESQ/SWSQ
      GMURUN=GMSTRE(MWSQ,0)
      PIZAP=-MPZARE(MWSQ,0)
      PIAAP=-MPAARE(MWSQ,0)
      FC=-DSQRT(CWSQ/SWSQ)*PIAAP*PIZAP
      MWSQ=GSU2SQ/(4.D0*DSQRT(2.D0)*GMURUN)/(1+FC)
      IF(DABS(MWSQ/MWSQ0-1.D0).GT.1.D-5)GO TO 10
      MW=DSQRT(MWSQ)
      RETURN
      END
      FUNCTION CDLN(X,A)
C=======================================================================
C=======================================================================
C
C  COMPLEX FUNCTIONS THAT TAKE ACCOUNT OF THE I*EPSILON PRESCRIPTION
C              TO CALCULATE ANALYTIC STRUCTURE
C
C=======================================================================
C***********************
C  COMPLEX LOGARITHM OF X+I*REAL(A) WHERE A IS AN INFINITESIMAL
      IMPLICIT LOGICAL (A-H,O-Z)
      COMPLEX*16 A,X,CDLN
      COMPLEX*16 PI
      PI=(3.141592653589793238462643D0,0.D0)
      IF(DIMAG(X).EQ.0.D0.AND.DREAL(X).LE.0.D0)THEN
        CDLN=CDLOG(-X)+(0.D0,1.D0)*PI*DSIGN(1.D0,DREAL(A))
      ELSE
        CDLN=CDLOG(X)
      END IF
      IF(DIMAG(CDLN).GT.DREAL(PI))CDLN=CDLN-(0.D0,1.D0)*PI
      IF(DIMAG(CDLN).LT.DREAL(-PI))CDLN=CDLN+(0.D0,1.D0)*PI
      RETURN
      END
      SUBROUTINE QDRT(A,B,C,R1,R2)
C*********************************
C  ROOTS OF THE QUADRATIC A*X**2+B*X+C=0 WITH REAL(R1)>REAL(R2)
      IMPLICIT LOGICAL (A-H,O-Z)
      COMPLEX*16 A,B,C,R1,R2
      COMPLEX*16 CNT,DIS
      CNT=-B/(2.D0*A)
      DIS=CDSQRT(CNT*CNT-C/A)
      DIS=DIS*DCMPLX(DSIGN(1.D0,DREAL(DIS)))
      R1=CNT+DIS
      R2=CNT-DIS
      RETURN
      END
      FUNCTION E2TAB(S)
C**********************
      IMPLICIT REAL*8 (A-H,O-Z)
CC-RM
      COMMON /CMS/EB,CME,SSS,BE,BE2
      REAL*8 MZ,MW,MH
      COMMON /BOS1/MZ,GZ,MW,GW,MH
CC-RM
      COMMON/INPOUT/LWRITE
      DATA INIT,NPOINT,EPS/0,100,1.D-4/
      DIMENSION TAB(101)
C
      IF (INIT.EQ.0) THEN
          INIT = 1
          WMIN = EPS
          WMAX = 160.D0
          DO 1 I = 0,NPOINT
               X = DFLOAT(I)/DFLOAT(NPOINT)
               W = WMIN+(WMAX-WMIN)*X
               SS= W*W
               TAB(I+1) = E2STRE(SS,1)
               IF (W.GT.CME.AND.W.GT.MZ) GOTO 11
 1        CONTINUE
 11       CONTINUE
          E2TAB = 0.D0
      ELSE
          IF (S.LT.0.D0) THEN
              WRITE (LWRITE,*) 'S < 0 IN E2TAB : S = ', S
              GOTO 999
          ENDIF
          W  = DSQRT(S)
          IF (W.LT.WMIN.OR.W.GT.WMAX) THEN
              WRITE (LWRITE,*) 'S OUT OF BOUNDS IN E2TAB : S = ', S
              GOTO 999
          ENDIF
          IF (W.EQ.WMAX) W = W - EPS
          X  = (W-WMIN)/(WMAX-WMIN)
          I  = INT(NPOINT*X)+1
          H  = X*NPOINT-DFLOAT(I-1)
          E2TAB = TAB(I)*(1-H)+TAB(I+1)*H
      ENDIF
 999  END
      FUNCTION S2TAB(S)
C**********************
      IMPLICIT REAL*8 (A-H,O-Z)
CC-RM
      COMMON /CMS/EB,CME,SSS,BE,BE2
      REAL*8 MZ,MW,MH
      COMMON /BOS1/MZ,GZ,MW,GW,MH
C...PG
        COMMON/INPOUT/LWRITE
CC-RM
      DATA INIT,NPOINT,EPS/0,100,1.D-4/
      DIMENSION TAB(101)
C-PG
      DATA S2TUN /.2379295488021655/
C
      IF (INIT.EQ.0) THEN
          INIT = 1
          WMIN = EPS
          WMAX = 160.D0
          DO 1 I = 0,NPOINT
               X = DFLOAT(I)/DFLOAT(NPOINT)
               W = WMIN+(WMAX-WMIN)*X
               SS= W*W
               TAB(I+1) = S2STRE(SS,1)
C-PG
               IF(I.EQ.0) TAB(I+1)=S2TUN
               IF (W.GT.CME.AND.W.GT.MZ) GOTO 11
 1        CONTINUE
 11       CONTINUE
          S2TAB = 0.D0
      ELSE
          IF (S.LT.0.D0) THEN
              WRITE (LWRITE,*) 'S < 0 IN S2TAB : S = ', S
              GOTO 999
          ENDIF
          W  = DSQRT(S)
          IF (W.LT.WMIN.OR.W.GT.WMAX) THEN
              WRITE (LWRITE,*) 'S OUT OF BOUNDS IN S2TAB : S = ', S
              GOTO 999
          ENDIF
          IF (W.EQ.WMAX) W = W - EPS
          X  = (W-WMIN)/(WMAX-WMIN)
          I  = INT(NPOINT*X)+1
          H  = X*NPOINT-DFLOAT(I-1)
          S2TAB = TAB(I)*(1-H)+TAB(I+1)*H
      ENDIF
 999  END
      FUNCTION GMUTAB(S)
C**********************
      IMPLICIT REAL*8 (A-H,O-Z)
CC-RM
      COMMON /CMS/EB,CME,SSS,BE,BE2
      REAL*8 MZ,MW,MH
      COMMON /BOS1/MZ,GZ,MW,GW,MH
C...PG
        COMMON/INPOUT/LWRITE
CC-RM
      DATA INIT,NPOINT,EPS/0,100,1.D-4/
      DIMENSION TAB(101)
C
      IF (INIT.EQ.0) THEN
          INIT = 1
          WMIN = EPS
          WMAX = 160.D0
          DO 1 I = 0,NPOINT
               X = DFLOAT(I)/DFLOAT(NPOINT)
               W = WMIN+(WMAX-WMIN)*X
               SS= W*W
               TAB(I+1) = GMSTRE(SS,1)
               IF (W.GT.CME.AND.W.GT.MZ) GOTO 11
 1        CONTINUE
 11       CONTINUE
          GMUTAB = 0.D0
      ELSE
          IF (S.LT.0.D0) THEN
              WRITE (LWRITE,*) 'S < 0 IN GMUTAB : S = ', S
              GOTO 999
          ENDIF
          W  = DSQRT(S)
          IF (W.LT.WMIN.OR.W.GT.WMAX) THEN
              WRITE (LWRITE,*) 'S OUT OF BOUNDS IN GMUTAB : S = ', S
              GOTO 999
          ENDIF
          IF (W.EQ.WMAX) W = W - EPS
          X  = (W-WMIN)/(WMAX-WMIN)
          I  = INT(NPOINT*X)+1
          H  = X*NPOINT-DFLOAT(I-1)
          GMUTAB = TAB(I)*(1-H)+TAB(I+1)*H
      ENDIF
 999  END
      FUNCTION RHOTAB(S)
C**********************
      IMPLICIT REAL*8 (A-H,O-Z)
CC-RM
      COMMON /CMS/EB,CME,SSS,BE,BE2
      REAL*8 MZ,MW,MH
      COMMON /BOS1/MZ,GZ,MW,GW,MH
C...PG
        COMMON/INPOUT/LWRITE
CC-RM
      DATA INIT,NPOINT,EPS/0,100,1.D-4/
      DIMENSION TAB(101)
C
      IF (INIT.EQ.0) THEN
          INIT = 1
          WMIN = EPS
          WMAX = 160.D0
          DO 1 I = 0,NPOINT
               X = DFLOAT(I)/DFLOAT(NPOINT)
               W = WMIN+(WMAX-WMIN)*X
               SS= W*W
               TAB(I+1) = RHSTRE(SS,1)
               IF (W.GT.CME.AND.W.GT.MZ) GOTO 11
 1        CONTINUE
 11       CONTINUE
          RHOTAB = 0.D0
      ELSE
          IF (S.LT.0.D0) THEN
              WRITE (LWRITE,*) 'S < 0 IN RHOTAB : S = ', S
              GOTO 999
          ENDIF
          W  = DSQRT(S)
          IF (W.LT.WMIN.OR.W.GT.WMAX) THEN
              WRITE (LWRITE,*) 'S OUT OF BOUNDS IN RHOTAB : S = ', S
              GOTO 999
          ENDIF
          IF (W.EQ.WMAX) W = W - EPS
          X  = (W-WMIN)/(WMAX-WMIN)
          I  = INT(NPOINT*X)+1
          H  = X*NPOINT-DFLOAT(I-1)
          RHOTAB = TAB(I)*(1-H)+TAB(I+1)*H
      ENDIF
 999  END
      FUNCTION MPAATB(S)
C**********************
      IMPLICIT REAL*8 (A-H,O-Z)
CC-RM
      COMMON /CMS/EB,CME,SSS,BE,BE2
      REAL*8 MZ,MW,MH
      COMMON /BOS1/MZ,GZ,MW,GW,MH
C...PG
        COMMON/INPOUT/LWRITE
CC-RM
      REAL*8 MPAATB,MPAARE
      DATA INIT,NPOINT,EPS/0,100,1.D-4/
      DIMENSION TAB(101)
C
      IF (INIT.EQ.0) THEN
          INIT = 1
          WMIN = EPS
          WMAX = 160.D0
          DO 1 I = 0,NPOINT
               X = DFLOAT(I)/DFLOAT(NPOINT)
               W = WMIN+(WMAX-WMIN)*X
               SS= W*W
               TAB(I+1) = MPAARE(SS,1)
               IF (W.GT.CME.AND.W.GT.MZ) GOTO 11
 1        CONTINUE
 11       CONTINUE
          MPAATB = 0.D0
      ELSE
          IF (S.LT.0.D0) THEN
              WRITE (LWRITE,*) 'S < 0 IN MPAATB : S = ', S
              GOTO 999
          ENDIF
          W  = DSQRT(S)
          IF (W.LT.WMIN.OR.W.GT.WMAX) THEN
              WRITE (LWRITE,*) 'S OUT OF BOUNDS IN MPAATB : S = ', S
              GOTO 999
          ENDIF
          IF (W.EQ.WMAX) W = W - EPS
          X  = (W-WMIN)/(WMAX-WMIN)
          I  = INT(NPOINT*X)+1
          H  = X*NPOINT-DFLOAT(I-1)
          MPAATB = TAB(I)*(1-H)+TAB(I+1)*H
      ENDIF
 999  END
      FUNCTION MPZATB(S)
C**********************
      IMPLICIT REAL*8 (A-H,O-Z)
CC-RM
      COMMON /CMS/EB,CME,SSS,BE,BE2
      REAL*8 MZ,MW,MH
      COMMON /BOS1/MZ,GZ,MW,GW,MH
C...PG
        COMMON/INPOUT/LWRITE
CC-RM
      REAL*8 MPZATB,MPZARE
      DATA INIT,NPOINT,EPS/0,100,1.D-4/
      DIMENSION TAB(101)
C
      IF (INIT.EQ.0) THEN
          INIT = 1
          WMIN = EPS
          WMAX = 160.D0
          DO 1 I = 0,NPOINT
               X = DFLOAT(I)/DFLOAT(NPOINT)
               W = WMIN+(WMAX-WMIN)*X
               SS= W*W
               TAB(I+1) = MPZARE(SS,1)
               IF (W.GT.CME.AND.W.GT.MZ) GOTO 11
 1        CONTINUE
 11       CONTINUE
          MPZATB = 0.D0
      ELSE
          IF (S.LT.0.D0) THEN
              WRITE (LWRITE,*) 'S < 0 IN MPZATB : S = ', S
              GOTO 999
          ENDIF
          W  = DSQRT(S)
          IF (W.LT.WMIN.OR.W.GT.WMAX) THEN
              WRITE (LWRITE,*) 'S OUT OF BOUNDS IN MPZATB : S = ', S
              GOTO 999
          ENDIF
          IF (W.EQ.WMAX) W = W - EPS
          X  = (W-WMIN)/(WMAX-WMIN)
          I  = INT(NPOINT*X)+1
          H  = X*NPOINT-DFLOAT(I-1)
          MPZATB = TAB(I)*(1-H)+TAB(I+1)*H
      ENDIF
 999  END
      FUNCTION GAMTAB(S)
C**********************
      IMPLICIT REAL*8 (A-H,O-Z)
CC-RM
      COMMON /CMS/EB,CME,SSS,BE,BE2
      REAL*8 MZ,MW,MH
      COMMON /BOS1/MZ,GZ,MW,GW,MH
C...PG
        COMMON/INPOUT/LWRITE
CC-RM
      DATA INIT,NPOINT,EPS/0,100,1.D-4/
      DIMENSION TAB(101)
C
      IF (INIT.EQ.0) THEN
          INIT = 1
          WMIN = EPS
          WMAX = 160.D0
          DO 1 I = 0,NPOINT
               X = DFLOAT(I)/DFLOAT(NPOINT)
               W = WMIN+(WMAX-WMIN)*X
               SS= W*W
               TAB(I+1) = ZWIDRE(SS,1)/W
               IF (W.GT.CME.AND.W.GT.MZ) GOTO 11
 1        CONTINUE
 11       CONTINUE
          GAMTAB = 0.D0
      ELSE
          IF (S.LT.0.D0) THEN
              WRITE (LWRITE,*) 'S < 0 IN GAMTAB : S = ', S
              GOTO 999
          ENDIF
          W  = DSQRT(S)
          IF (W.LT.WMIN.OR.W.GT.WMAX) THEN
              WRITE (LWRITE,*) 'S OUT OF BOUNDS IN GAMTAB : S = ', S
              GOTO 999
          ENDIF
          IF (W.EQ.WMAX) W = W - EPS
          X  = (W-WMIN)/(WMAX-WMIN)
          I  = INT(NPOINT*X)+1
          H  = X*NPOINT-DFLOAT(I-1)
          GAMTAB = W*(TAB(I)*(1-H)+TAB(I+1)*H)
      ENDIF
 999  END
      FUNCTION RHO(S,MSQ)
C************************
C  RENORMALIZED MASSLESS FERMION VERTEX FORM FACTOR ASSOCIATED WITH
C  THE VERTEX GRAPH CONTAINING A SINGLE VIRTUAL HEAVY BOSON
C  S = SQUARE OF EXTERNAL BOSON MOMENTUM
C  MSQ = SQUARE OF MASS OF INTERNAL VIRTUAL BOSON
      IMPLICIT LOGICAL (A-H,O-Z)
      REAL*8 S
      COMPLEX*16 MSQ,RHO
      COMPLEX*16 BETA,CLG,PISQ6
      COMPLEX*16 CDLN,SP
CC-RM
      IF (DABS(S).LE.1.D0) THEN
        RHO = (0.D0,0.D0)
        RETURN
      ENDIF
CC-RM
      PISQ6=(1.6449340668482264D0,0.D0)
      BETA=-MSQ/S
      CLG=CDLN(BETA,(1.D0,0.D0))
      RHO=CLG+0.5D0
     C   +(2.D0*BETA-4.D0)*(CLG+1.D0)
     C   -2.D0*(1.D0-BETA)**2*(PISQ6-SP(1.D0-1.D0/BETA,(1.D0,0.D0)))
      RETURN
      END
      FUNCTION XLMDA(S,MSQ)
C**************************
C  RENORMALIZED MASSLESS FERMION VERTEX FORM FACTOR ASSOCIATED WITH THE
C  VERTEX GRAPH CONTAINING 2 IDENTICAL HEAVY BOSONS
C  S = SQUARE OF EXTERNAL BOSON MOMENTUM
C  MSQ = SQUARE OF MASS OF INTERNAL VIRTUAL BOSONS
      IMPLICIT LOGICAL (A-H,O-Z)
      REAL*8 S
      COMPLEX*16 MSQ,XLMDA
      COMPLEX*16 BETA,CLG,X1,X2
      COMPLEX*16 CDLN,SP
      BETA=-MSQ/S
      CALL QDRT(DCMPLX(S),-DCMPLX(S),MSQ,X1,X2)
C==============================================
      CLG=CDLN(-X2,(1.D0,0.D0))-CDLN(X1,(1.D0,0.D0))
      XLMDA=(1.D0-2.D0*BETA)*(X1-X2)*CLG
     C     +(4.D0-2.D0*BETA)*BETA*CLG**2
     C     -2.D0*BETA-2.5D0
      RETURN
C=======================================================================
C=======================================================================
      END
      FUNCTION SP(Y,E)
C*********************
C  SPENCE FUNCTION OF Y+I*REAL(E) WHERE E IS AN INFINITESIMAL
      IMPLICIT LOGICAL (A-H,O-Z)
      COMPLEX*16 Y,E,SP
      REAL *8 B(9),FACT
      COMPLEX*16 A,CLN,PISQ6,PROD,TERM,X,Z,ZSQ
      COMPLEX*16 CDLN
      B(1)=1.D0/6.D0
      B(2)=-1.D0/30.D0
      B(3)=1.D0/42.D0
      B(4)=B(2)
      B(5)=5.D0/66.D0
      B(6)=-691.D0/2730.D0
      B(7)=7.D0/6.D0
      B(8)=-3617.D0/510.D0
      B(9)=43867.D0/798.D0
      PISQ6=(1.6449340668482264D0,0.D0)
      I1=0
      I2=0
      X=Y
      A=E
      IF(X.EQ.1.D0)THEN
        SP=PISQ6
        RETURN
      END IF
C  IF X LIES OUTSIDE THE UNIT CIRCLE THEN EVALUATE SP(1/X)
      IF(CDABS(X).GT.1.D0)THEN
        X=1.D0/X
        A=-A
        I1=1
      END IF
C  IF REAL(X)>1/2 THEN EVALUATE SP(1-X)
      IF(DREAL(X).GT.0.5D0)THEN
        X=1.D0-X
        A=-A
        I2=1
      END IF
C  EVALUATE SERIES FOR SP(X)
      Z=-CDLN(1.D0-X,-A)
      ZSQ=Z*Z
      SP=Z-ZSQ/4.D0
      PROD=Z
      FACT=1.D0
      DO 10 J=2,18,2
        FACT=FACT*DCMPLX(DBLE((J+1)*J))
        PROD=PROD*ZSQ
        TERM=B(J/2)/FACT*PROD
        SP=SP+TERM
        IF(CDABS(TERM/SP).LT.1.D-20)GO TO 20
10      CONTINUE
C  ADD APPROPRIATE LOGS TO OBTAIN SPENCE FUNCTION OF ORIGINAL ARGUEMENT
20    IF(I2.EQ.1)THEN
        SP=-SP+PISQ6-CDLN(X,A)*CDLN(1.D0-X,-A)
        X=1.D0-X
        A=-A
      END IF
      IF(I1.EQ.1)THEN
        CLN=CDLN(-X,-A)
        SP=-SP-PISQ6-CLN*CLN/2.D0
      END IF
      RETURN
C=======================================================================
C=======================================================================
      END
