      SUBROUTINE ASKUSI(IGCOD)                                          2
C------------------------------------------------------------           3
C! Written A.J.FINCH 3/11/1995                                          4
C! Interface routine for PHOJ01 generator -Initialisation               5
C!     output arguments :                                               6
C!          IGCOD /I : Generator identification code                    7
C------------------------------------------------------------           8
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               9
      SAVE                                                              
C                                                                       
      INTEGER LMHLEN, LMHCOL, LMHROW                                    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          3
C                                                                       4
      COMMON /BCS/   IW(1000)                                           5
      INTEGER IW                                                        6
      REAL RW(1000)                                                     7
      EQUIVALENCE (RW(1),IW(1))                                         8
C                                                                       9
C                                                                       
      DOUBLE PRECISION EE1,EE2                                          2
      COMMON/CPHJ01/EE1,EE2,LOUT,ILUMI,ILTOT                            3
C                                                                       
C                                                                       
      PARAMETER (NMAXD=100)                                             
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,        
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD          
C                                                                       
      COMMON /LEPCUT/ ECMIN,ECMAX,EEMIN1,EEMIN2,                        
     &                YMIN1,YMAX1,YMIN2,YMAX2,                          
     &                Q2MIN1,Q2MAX1,Q2MIN2,Q2MAX2,                      
     &                THMIN1,THMAX1,THMIN2,THMAX2                       
                                                                        
C                                                                       
      common /evtsta/nevent(10)                                         1
      REAL EJMN1,AMAX                                                   
      REAL AVERS                                                        
                                                                        
      PARAMETER(KINTYP=6009)                                            
      PARAMETER(NVERS=103)                                              
      IGCOD = KINTYP                                                    
      AVERS = NVERS/100.0                                               
C                                                                       
C                                                                       
      WRITE(IW(6),601)IGCOD,AVERS                                       
601   FORMAT(/////,15X,'---------------------------------------'/,      
     1             15X,'-                                     -'/,      
     1             15X,'-             P H O J 0 1             -'/,      
     1             15X,'-            ===============          -'/,      
     1             15X,'-            generator code ',I5,'     -'/,     
     1             15X,'-            version ',F5.2,'            -'/,   
     1             15X,'-                                     -'/,      
     1             15X,'-   Aleph interface to PHOJET         -'/,      
     1             15X,'-   Author: A.J.Finch                 -'/,      
     1             15X,'-   (A.Finch@lancaster.ac.uk)         -'/,      
     1             15X,'-  last modification  9 May    2000   -'/,      
     1             15X,'---------------------------------------',       
     1        /////)                                                    
                                                                        
      WRITE(6,*)                                                        
      WRITE(6,*) ' ===================================================' 
      WRITE(6,*) '  ---------- PHOJET v.1.05b(27-Apr-96) ----------- '  
      WRITE(6,*) ' ===================================================' 
      WRITE(6,*) '       please send suggestions / bug reports to:'     
      WRITE(6,*) '     eng@tph200.physik.uni-leipzig.de (Ralph Engel)'  
      WRITE(6,*) ' ===================================================' 
      WRITE(6,*)                                                        
C                                                                       
C  standard initializations                                             
C                                                                       
      CALL PHODAT                                                       
      CALL HADDAT                                                       
      CALL FITDAT                                                       ASKUSI64
C                                                                       ASKUSI65
C SET DEFAULT SETTINGS                                                  ASKUSI66
C                                                                       ASKUSI67
      CALL SETCUT                                                       ASKUSI68
C                                                                       ASKUSI69
C NOW READ ANY USER CARDS THAT OVERRIDE DEFAULTS                        ASKUSI70
C                                                                       ASKUSI71
      CALL USECUT                                                       ASKUSI72
C                                                                       ASKUSI73
C Print out some summary information to reassure user                   ASKUSI74
C                                                                       ASKUSI75
      IF(ECMAX.GT.2*EE1)ECMAX=2*EE1                                     ASKUSI76
      WRITE(6,60)EE1,ECMIN,ECMAX                                        ASKUSI77
60    FORMAT(///'  Generation of e+e-->e+e-X at a beam energy of ',F8.3,ASKUSI78
     1' where X is an hadronic final state with '/                      ASKUSI79
     1' invariant mass between ',F8.3,' and ',F8.3,' GeV '///)          ASKUSI80
                                                                        ASKUSI81
      WRITE(6,61)1,EEMIN1, THMIN1,THMAX1                                ASKUSI82
      WRITE(6,61)2,EEMIN2,THMIN2,THMAX2                                 ASKUSI83
61    FORMAT(' electron ',i1,                                           ASKUSI84
     1' is antitagged if it has energy greater than ',                  ASKUSI85
     1F8.3/'  and theta between ',F8.3,' and ',F8.3,' radians ')        ASKUSI86
                                                                        ASKUSI87
C ------------------------------------------------------------------    ASKUSI88
C                                                                       ASKUSI89
C book histograms                                                       ASKUSI90
C                                                                       ASKUSI91
C ------------------------------------------------------------------    ASKUSI92
       EBEAM = EE1                                                      ASKUSI93
       EJMN1 = 1.2*EBEAM                                                ASKUSI94
       IB = 1000                                                        ASKUSI95
       CALL HBOOK1( IB+1,' ENERGY OF SCATTERED ELECTRON$'               ASKUSI96
     1 ,80,0.0,EJMN1,0.)                                                ASKUSI97
       CALL HBOOK1( IB+3,'ENERGY OF OTHER ELECTRON $'                   ASKUSI98
     1,80,0.0,EJMN1,0.)                                                 ASKUSI99
       AMAX = EJMN1                                                     ASKUS100
       CALL HBOOK1( IB+9,'INVARIANT MASS OF FINAL STATE $'              ASKUS101
     1,40,0.0,AMAX,0.)                                                  ASKUS102
       CALL HBOOK1( IB+10,'cos theta of tracks$'                        ASKUS103
     1,40,-1.0,1.0,0.)                                                  ASKUS104
       CALL HBOOK1( IB+11,'Pt of tracks$'                               ASKUS105
     1,40,0.0,10.0,0.)                                                  ASKUS106
                                                                        ASKUS107
C ------------------------------------------------------------------    ASKUS108
C                                                                       ASKUS109
C aleph specific initialisation                                         ASKUS110
C (INITIALISE ALEPH PARTICLE BANKS)                                     ASKUS111
C                                                                       ASKUS112
C ------------------------------------------------------------------    ASKUS113
      IPPART = 0                                                        ASKUS114
      CALL KPARTI(IPPART)                                               ASKUS115
C ------------------------------------------------------------------    ASKUS116
C                                                                       ASKUS117
C STORE INFORMATION IN BANK KPAR   AND RLEP                             ASKUS118
C                                                                       ASKUS119
C ------------------------------------------------------------------    ASKUS120
      CALL PHORUN                                                       ASKUS121
C ------------------------------------------------------------------    ASKUS122
C                                                                       ASKUS123
C SET DEBUG LEVELS                                                      ASKUS124
C                                                                       ASKUS125
C ------------------------------------------------------------------    ASKUS126
      IDEB(87)=-5                                                       ASKUS127
      IDEB(21)=-5                                                       ASKUS128
      RETURN                                                            ASKUS129
      END                                                               ASKUS130
      SUBROUTINE KPARTI(IPPART)                                         KPARTI 2
C------------------------------------------------------------           KPARTI 3
C! Interface routine for PHOJ01 generator - initialise PART             KPARTI 4
C!                                                                      KPARTI 5
C! Author   :- A.J.FINCH 1/6/1996                                       KPARTI 6
C!     input arguments :                                                KPARTI 7
C!          IPPART /I : PART bank is printed out if                     KPARTI 8
C!                       IPPART > 0                                     KPARTI 9
C! -- complete PART bank with LUND  particles                           KPARTI10
C! -- Print PART and KLIN banks                                         KPARTI11
C! -- get list of  particle# which should not be decayed                KPARTI12
C! -- inhibit decays in LUND                                            KPARTI13
C------------------------------------------------------------           KPARTI14
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      PARAMETER (L1MST=200, L1PAR=200)                                  LUCOM  2
      PARAMETER (L2PAR=500, L2PARF=2000 )                               LUCOM  3
      PARAMETER (LJNPAR=4000)                                           LUCOM  4
      REAL    PARU,PARJ,PMAS,PARF,VCKM,BRAT,P7LU,V7LU                   LUCOM  5
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)   LUCOM  6
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)LUCOM  7
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),        LUCOM  8
     &                KFDP(L2PARF,5)                                    LUCOM  9
      COMMON /LUDAT4/ CHAF(L2PAR)                                       LUCOM 10
      CHARACTER*8 CHAF                                                  LUCOM 11
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5) LUCOM 12
                                                                        KPARTI17
      PARAMETER (LPDEC=48)                                              KPARTI18
      INTEGER NODEC(LPDEC)                                              KPARTI19
      PMAS(LUCOMP(25),1)= 100.                                          KPARTI20
      PMAS(LUCOMP( 6),1)= 100.                                          KPARTI21
      PMAS(LUCOMP(23),1)= 91.2                                          KPARTI22
C                                                                       KPARTI23
C   HIGGS Mass , TOP Mass and Z0 mass defined, can be overwritten by    KPARTI24
C   a PMA1 card                                                         KPARTI25
C                                                                       KPARTI26
C -- complete PART bank with LUND  particles                            KPARTI27
C    use the library routine KXL74A                                     KPARTI28
      CALL KXL74A (IPART,IKLIN)                                         KPARTI29
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN                              KPARTI30
         WRITE (IW(6),                                                  KPARTI31
     1    '(1X,''error in PART or KLIN bank - STOP - '',2I3)')          KPARTI32
     1   IPART,IKLIN                                                    KPARTI33
         CALL EXIT                                                      KPARTI34
      ENDIF                                                             KPARTI35
C                                                                       KPARTI36
C - Print PART and KLIN banks                                           KPARTI37
      IF (IPPART.GT.0) CALL PRPART                                      KPARTI38
C                                                                       KPARTI39
C -- get list of  particle# which should not be decayed                 KPARTI40
C    in LUND  because they are decayed in GALEPH.                       KPARTI41
C    the routines uses the KLIN bank and fills the user array           KPARTI42
C    NODEC in the range [1-LPDEC]                                       KPARTI43
      MXDEC = KNODEC (NODEC,LPDEC)                                      KPARTI44
      MXDEC = MIN (MXDEC,LPDEC)                                         KPARTI45
C                                                                       KPARTI46
C -- inhibit decays in LUND                                             KPARTI47
C    If the user has set some decay channels by data cards they will    KPARTI48
C    will not be overwritten                                            KPARTI49
      IF (MXDEC .GT. 0) THEN                                            KPARTI50
         DO 10 I=1,MXDEC                                                KPARTI51
            IF (NODEC(I).GT.0) THEN                                     KPARTI52
               JIDB = NLINK('MDC1',NODEC(I))                            KPARTI53
               IF (JIDB .EQ. 0) MDCY(LUCOMP(NODEC(I)),1) = 0            KPARTI54
            ENDIF                                                       KPARTI55
   10    CONTINUE                                                       KPARTI56
      ENDIF                                                             KPARTI57
      RETURN                                                            KPARTI58
      END                                                               KPARTI59
      SUBROUTINE ASKUSE(IDPR,ISTAT,NTRK,NVRT,ECMS,WEIT)                 ASKUSE 2
C------------------------------------------------------------           ASKUSE 3
C! Interface routine for PHOJ01 generator                               ASKUSE 4
C!  Generates one event                                                 ASKUSE 5
C! Author :-  A.J. Finch 1/6/96                                         ASKUSE 6
C!         - modified B.Bloch 13/05/97 to apply standard selection      BBL001 3
C!           in production ( courtesy of A.Finch) with GSTD card        BBL001 4
C!     structure : subroutine                                           ASKUSE 7
C!     output arguments :                                               ASKUSE 8
C!          IDPR   : process identification,each digit corresponds to   ASKUSE 9
C!          the flavor of the evnt ( several flavors /event is possible)ASKUSE10
C!          ISTA   : status flag ( 0 means ok), use it to reject        ASKUSE11
C!                   unwanted events                                    ASKUSE12
C!          NTRK   : number of tracks generated and kept                ASKUSE13
C!                  (i.e. # KINE banks  written)                        ASKUSE14
C!          NVRT   : number of vertices generated                       ASKUSE15
C!                  (i.e. # VERT banks written)                         ASKUSE16
C!          ECMS   : center of mass energy for the event (may be        ASKUSE17
C!                   different from nominal cms energy)                 ASKUSE18
C!          WEIT   : event weight ( not 1 if a weighting method is used)ASKUSE19
C!                                                                      ASKUSE20
C! common variables to control photon flux calculation:                 ASKUSE21
C!     input:                                                           ASKUSE22
C!           from COMMON /CPHJ01/:                                      ASKUSE23
C!                EE1     LAB system energy of electron/positron 1      ASKUSE24
C!                EE2     LAB system energy of electron/positron 2      ASKUSE25
C!            from COMMON /LEPCUT/:                                     ASKUSE26
C!                YMIN1   lower limit of Y1                             ASKUSE27
C!                        (energy fraction taken by photon from electronASKUSE28
C!                YMAX1   upper limit of Y1                             ASKUSE29
C!                Q2MIN1  lower limit of photon virtuality              ASKUSE30
C!                Q2MAX1  upper limit of photon virtuality              ASKUSE31
C!                THMIN1  lower limit of scattered electron             ASKUSE32
C!                THMAX1  upper limit of scattered electron             ASKUSE33
C!                YMIN2   lower limit of Y2                             ASKUSE34
C!                        (energy fraction taken by photon from electronASKUSE35
C!                YMAX2   upper limit of Y2                             ASKUSE36
C!                Q2MIN2  lower limit of photon virtuality              ASKUSE37
C!                Q2MAX2  upper limit of photon virtuality              ASKUSE38
C!                THMIN2  lower limit of scattered electron             ASKUSE39
C!                THMAX2  upper limit of scattered electron             ASKUSE40
C!                                                                      ASKUSE41
C -----------------------------------------------------------------     ASKUSE42
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               ASKUSE43
      REAL*4   ECMS,WEIT                                                ASKUSE44
                                                                        ASKUSE45
      SAVE                                                              ASKUSE46
C                                                                       ASKUSE47
      PARAMETER ( ZERO = 0.D0,                                          ASKUSE48
     &            ONE  = 1.D0,                                          ASKUSE49
     &            TWO  = 2.D0,                                          ASKUSE50
     &            PI   = 3.1415927D0 )                                  ASKUSE51
C                                                                       ASKUSE52
C  lepton/hadron-photon kinematics                                      ASKUSE53
      COMMON /PHOSRC/ PINI(5,2),PFIN(5,2),PGAM(5,2),GYY(2),GQ2(2),      ASKUSE54
     &                GGECM,GAIMP(2),PFTHE(2),PFPHI(2),IDPSRC(2),       ASKUSE55
     &                IDBSRC(2)                                         ASKUSE56
      COMMON /LEPCUT/ ECMIN,ECMAX,EEMIN1,EEMIN2,                        ASKUSE57
     &                YMIN1,YMAX1,YMIN2,YMAX2,                          ASKUSE58
     &                Q2MIN1,Q2MAX1,Q2MIN2,Q2MAX2,                      ASKUSE59
     &                THMIN1,THMAX1,THMIN2,THMAX2                       ASKUSE60
C                                                                       ASKUSE61
      DOUBLE PRECISION EE1,EE2                                          CPHOJ  2
      COMMON/CPHJ01/EE1,EE2,LOUT,ILUMI,ILTOT                            CPHOJ  3
C                                                                       
      INTEGER LMHLEN, LMHCOL, LMHROW                                    
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          
C                                                                       
      COMMON /BCS/   IW(1000)                                           
      INTEGER IW                                                        
      REAL RW(1000)                                                     
      EQUIVALENCE (RW(1),IW(1))                                         
C                                                                       
C                                                                       
      PARAMETER (L1MST=200, L1PAR=200)                                  
      PARAMETER (L2PAR=500, L2PARF=2000 )                               
      PARAMETER (LJNPAR=4000)                                           
      REAL    PARU,PARJ,PMAS,PARF,VCKM,BRAT,P7LU,V7LU                   
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)   
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),        
     &                KFDP(L2PARF,5)                                    
      COMMON /LUDAT4/ CHAF(L2PAR)                                       
      CHARACTER*8 CHAF                                                  
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5) 
                                                                        
      COMMON /PROCES/ IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON(15)      
      common /evtsta/nevent(10)                                         
      DIMENSION P1(4),P2(4)                                             
                                                                        
      REAL*4 VRTEX(4),SDVRT(3),RX,RY,RZ,dum                             
      real*4 EBEAM,WMIN,ELEC(5),POSI(5),LUMIF,QQ1,QQ2                   
                                                                        
      LOGICAL FIRST,DEBUG,KEEP,KSTD,KUSR                                
                                                                        
      DATA    FIRST/.TRUE./                                             
      DATA    ICALL/0/,IFAIL/0/                                         
      DATA    VRTEX/0.0,0.0,0.0,0.0/                                    
      DATA    SDVRT/0.0180,0.0010,1.00 /                                
C-------------------------------------------------------------------    
C                                                                       
C INITIALISE OUTPUT ARGUMENTS                                           
C                                                                       
C-------------------------------------------------------------------    
      ISTAT = 0                                                         
      NTRK = 0                                                          ASKUSE87
      NVRT = 1                                                          ASKUSE88
      ECMS = 2*EE1                                                      ASKUSE89
      WEIT = 1.0                                                        ASKUSE90
      IDPR = 1                                                          ASKUSE91
      IF(FIRST)THEN                                                     ASKUSE92
       FIRST = .FALSE.                                                  ASKUSE93
C-------------------------------------------------------------------    ASKUSE94
C                                                                       ASKUSE95
C ON FIRST CALL INITIALISE SDVRT,DEBUG,and PHOJET                       ASKUSE96
C                                                                       ASKUSE97
C-------------------------------------------------------------------    ASKUSE98
       JSVRT = NLINK('SVRT',0)                                          ASKUSE99
        IF(JSVRT.NE.0)THEN                                              ASKUS100
          SDVRT(1) = RW(JSVRT+1)                                        ASKUS101
          SDVRT(2) = RW(JSVRT+2)                                        ASKUS102
          SDVRT(3) = RW(JSVRT+3)                                        ASKUS103
        ENDIF                                                           ASKUS104
C                                                                       BBL001 7
C GSTD card switch on call to standard selection routine                BBL001 8
C                                                                       BBL001 9
      ISTD = 0                                                          BBL00110
      MSTD = 0                                                          BBL00111
      JGSTD = NLINK('GSTD',0)                                           BBL00112
      IF(JGSTD.NE.0)THEN                                                BBL00113
         ISTD = IW(JGSTD+1)                                             BBL00114
         MSTD = IW(JGSTD+2)                                             BBL00115
      ENDIF                                                             BBL00116
C-------------------------------------------------------------------    ASKUS105
C                                                                       ASKUS106
C use the DEBU card to initialise event debugging                       ASKUS107
C  DEBUG lout / ndeb1  ndeb2                                            ASKUS108
C                                                                       ASKUS109
C-------------------------------------------------------------------    ASKUS110
        IDB1 = 0                                                        ASKUS111
        IDB2 = 0                                                        ASKUS112
        LOUT = 6                                                        ASKUS113
        NADEB = NAMIND('DEBU')                                          ASKUS114
        JDEBU = IW(NADEB)                                               ASKUS115
        IF(JDEBU.NE.0) THEN                                             ASKUS116
         IDB1 = IW(JDEBU+1)                                             ASKUS117
         IDB2 = IW(JDEBU+2)                                             ASKUS118
         IF(IW(JDEBU-2).GT.0)LOUT = IW(JDEBU-2)                         ASKUS119
         MSTU(11) = LOUT                                                ASKUS120
        ENDIF                                                           ASKUS121
C                                                                       ASKUS122
C  lepton numbers                                                       ASKUS123
C                                                                       ASKUS124
       IDPSRC(1) = 11                                                   ASKUS125
       IDPSRC(2) = -11                                                  ASKUS126
       IDBSRC(1) = 3                                                    ASKUS127
       IDBSRC(2) = 4                                                    ASKUS128
C                                                                       ASKUS129
C  photon 1                                                             ASKUS130
C                                                                       ASKUS131
       EGAM = YMAX1*EE1                                                 ASKUS132
       P1(1) = ZERO                                                     ASKUS133
       P1(2) = ZERO                                                     ASKUS134
       P1(3) = EGAM                                                     ASKUS135
       P1(4) = EGAM                                                     ASKUS136
C                                                                       ASKUS137
C  photon 2                                                             ASKUS138
C                                                                       ASKUS139
       EGAM = YMAX2*EE2                                                 ASKUS140
       P2(1) = ZERO                                                     ASKUS141
       P2(2) = ZERO                                                     ASKUS142
       P2(3) = -EGAM                                                    ASKUS143
       P2(4) = EGAM                                                     ASKUS144
       CALL SETPAR(1,22,0,ZERO)                                         ASKUS145
       CALL SETPAR(2,22,0,ZERO)                                         ASKUS146
       CALL EVENT(-1,P1,P2,SIGMAX,IREJ)                                 ASKUS147
C                                                                       ASKUS148
C                                                                       ASKUS149
       ILTOT = 0                                                        ASKUS150
       ILUMI = 0                                                        ASKUS151
      ENDIF                                                             ASKUS152
C                                                                       ASKUS153
C                                                                       ASKUS154
      ICALL = ICALL + 1                                                 ASKUS155
                                                                        ASKUS156
      DEBUG = .FALSE.                                                   ASKUS157
      IF(ICALL.GE.IDB1.AND.ICALL.LE.IDB2)DEBUG=.TRUE.                   ASKUS158
C                                                                       ASKUS159
C  generation of events, flux calculation                               ASKUS160
C                                                                       ASKUS161
c                                                                       ASKUS162
 150    CONTINUE                                                        ASKUS163
 175    CONTINUE                                                        ASKUS164
       ILTOT = ILTOT + 1                                                ASKUS165
       IFLAG = 0                                                        ASKUS166
       WMIN = ECMIN                                                     ASKUS167
       EBEAM = EE1                                                      ASKUS168
       CALL GENLUM(IFLAG,EBEAM,WMIN,ELEC,POSI,LUMIF,QQ1,QQ2)            ASKUS169
       NTAG = 0                                                         ASKUS170
C                                                                       ASKUS171
C now apply cuts to electron energy/direction to select antit tagged eveASKUS172
C                                                                       ASKUS173
C  incoming electron 1                                                  ASKUS174
C                                                                       ASKUS175
        PINI(1,1) = ZERO                                                ASKUS176
        PINI(2,1) = ZERO                                                ASKUS177
        PINI(3,1) = -EE1                                                ASKUS178
        PINI(4,1) = EE1                                                 ASKUS179
        PINI(5,1) = ZERO                                                ASKUS180
C                                                                       ASKUS181
C  outgoing electron 1                                                  ASKUS182
C                                                                       ASKUS183
        DO JJJ = 1,5                                                    ASKUS184
         PFIN(JJJ,1) = ELEC(JJJ)                                        ASKUS185
        ENDDO                                                           ASKUS186
        PTOT = SQRT(elec(1)**2 +                                        ASKUS187
     1                 elec(2)**2 +                                     ASKUS188
     1                 elec(3)**2 )                                     ASKUS189
        CT = elec(3)/PTOT                                               ASKUS190
        theta = acos(ct)                                                ASKUS191
c                                                                       ASKUS192
c this theta iS close to pi normally                                    ASKUS193
c                                                                       ASKUS194
C  HAVE WE HIT electron tagger 2 ?                                      ASKUS195
C                                                                       ASKUS196
        IF(PFIN(4,1).GT.EEMIN1) THEN                                    ASKUS197
          IF((THETA.GT.THMIN1).AND.(THETA.LT.THMAX1)) NTAG=NTAG+1       ASKUS198
        ENDIF                                                           ASKUS199
C  photon 1                                                             ASKUS200
        P1(1) = -PFIN(1,1)                                              ASKUS201
        P1(2) = -PFIN(2,1)                                              ASKUS202
        P1(3) = PINI(3,1)-PFIN(3,1)                                     ASKUS203
        P1(4) = PINI(4,1)-PFIN(4,1)                                     ASKUS204
C  incoming electron 2                                                  ASKUS205
        PINI(1,2) = ZERO                                                ASKUS206
        PINI(2,2) = ZERO                                                ASKUS207
        PINI(3,2) = EE2                                                 ASKUS208
        PINI(4,2) = EE2                                                 ASKUS209
        PINI(5,2) = ZERO                                                ASKUS210
C  outgoing electron 2                                                  ASKUS211
        DO JJJ = 1,5                                                    ASKUS212
         PFIN(JJJ,2) = POSI(JJJ)                                        ASKUS213
        ENDDO                                                           ASKUS214
        PTOT = SQRT(POSI(1)**2 +                                        ASKUS215
     1                 POSI(2)**2 +                                     ASKUS216
     1                 POSI(3)**2 )                                     ASKUS217
        CT = POSI(3)/PTOT                                               ASKUS218
        THETA = ACOS(CT)                                                ASKUS219
C  electron tagger 2                                                    ASKUS220
        IF(PFIN(4,2).GT.EEMIN2) THEN                                    ASKUS221
          IF((THETA.GT.THMIN2).AND.(THETA.LT.THMAX2)) NTAG=NTAG+1       ASKUS222
        ENDIF                                                           ASKUS223
C  photon 2                                                             ASKUS224
        P2(1) = -PFIN(1,2)                                              ASKUS225
        P2(2) = -PFIN(2,2)                                              ASKUS226
        P2(3) = PINI(3,2)-PFIN(3,2)                                     ASKUS227
        P2(4) = PINI(4,2)-PFIN(4,2)                                     ASKUS228
C  ECMS cut                                                             ASKUS229
        GGECM = (P1(4)+P2(4))**2-(P1(1)+P2(1))**2                       ASKUS230
     &         -(P1(2)+P2(2))**2-(P1(3)+P2(3))**2                       ASKUS231
        IF(GGECM.LT.0.1D0) GOTO 175                                     ASKUS232
        GGECM = SQRT(GGECM)                                             ASKUS233
        IF((GGECM.LT.ECMIN).OR.(GGECM.GT.ECMAX)) GOTO 175               ASKUS234
        IF(NTAG.NE.0)GOTO 175                                           ASKUS235
c        CALL PRESEL(5,IREJ)                                            ASKUS236
c        IF(IREJ.NE.0) GOTO 175                                         ASKUS237
C                                                                       ASKUS238
C this event has survived anti tag cuts                                 ASKUS239
c                                                                       ASKUS240
        ILUMI=ILUMI+1                                                   ASKUS241
        GYY(1) = p1(4)/ebeam                                            ASKUS242
        GQ2(1) = QQ1                                                    ASKUS243
        GYY(2) = p2(4)/ebeam                                            ASKUS244
        GQ2(2) = QQ2                                                    ASKUS245
C  event generation                                                     ASKUS246
        CALL EVENT(1,P1,P2,SIGCUR,IREJ)                                 ASKUS247
      nevent(1) = nevent(1) + 1                                         BBL00117
        IF(IREJ.NE.0) GOTO 150                                          ASKUS248
      IF(DEBUG)CALL LULIST(3)                                           ASKUS249
C-------------------------------------------------------------------    ASKUS250
C                                                                       ASKUS251
C APPLY SELECTION TO LUND ARRAY                                         ASKUS252
C                                                                       ASKUS253
C-------------------------------------------------------------------    ASKUS254
      nevent(2) = nevent(2) + 1                                         
      KSTD = .TRUE.
      IF ( ISTD.eq.1) call STDSEL(KSTD)                                 BBL00119
      if ( KSTD ) nevent(3) = nevent(3) +1                              BBL00120
C                                                                       BBL00121
      CALL AMCSEL(KUSR)                                                 BBL00122
      if ( KUSR ) nevent(4) = nevent(4) +1                              BBL00123
      KEEP = KSTD.and.KUSR                                              BBL00124
      if (KEEP) nevent(5) = nevent(5) +1                                BBL00125
      IF(.NOT.KEEP)THEN                                                 ASKUS256
        ISTAT = 1                                                       ASKUS257
        IFAIL = IFAIL + 1                                               ASKUS258
        IF((MSTD.GT.0).and.(IFAIL.GE.MSTD))THEN                         BBL00126
           IFAIL = 0                                                    ASKUS260
           WEIT  = 10.0                                                 ASKUS261
           ISTAT = 0                                                    ASKUS262
        ELSE                                                            ASKUS263
           RETURN                                                       ASKUS264
        ENDIF                                                           ASKUS265
      ENDIF                                                             ASKUS266
      if ((.not.KEEP).and.(WEIT.eq.10.)) nevent(6) = nevent(6) +1       BBL00127
C-------------------------------------------------------------------    ASKUS267
C                                                                       ASKUS268
C Fill histograms                                                       ASKUS269
C                                                                       ASKUS270
C-------------------------------------------------------------------    ASKUS271
      CALL PHFILL                                                       ASKUS272
C-------------------------------------------------------------------    ASKUS273
C                                                                       ASKUS274
C GENERATE PRIMARY VERTEX                                               ASKUS275
C                                                                       ASKUS276
C-------------------------------------------------------------------    ASKUS277
      CALL RANNOR (RX,RY)                                               ASKUS278
      CALL RANNOR (RZ,DUM)                                              ASKUS279
      VRTEX(1) = RX*SDVRT(1)                                            ASKUS280
      VRTEX(2) = RY*SDVRT(2)                                            ASKUS281
      VRTEX(3) = RZ*SDVRT(3)                                            ASKUS282
      VRTEX(4) = 0.                                                     ASKUS283
C-------------------------------------------------------------------    ASKUS284
C                                                                       ASKUS285
C COPY EVENT FROM LUND ARRAY TO ALEPH OUTPUT BANKS                      ASKUS286
C                                                                       ASKUS287
C-------------------------------------------------------------------    ASKUS288
      CALL KXL7AL (VRTEX,ISTAT,NVRT,NTRK)                               ASKUS289
C-------------------------------------------------------------------    ASKUS290
C                                                                       ASKUS291
C set event type from /PROCES/                                          ASKUS292
C                                                                       ASKUS293
C-------------------------------------------------------------------    ASKUS294
      IDPR = IDNODF+(10*IPROCE)                                         ASKUS295
c                                                                       ASKUS296
                                                                        ASKUS297
      RETURN                                                            ASKUS298
      END                                                               ASKUS299
      SUBROUTINE USCJOB                                                 2
C------------------------------------------------------------           3
C! Interface routine for PHOJ01 generator                               4
C!  End of job - print out statistics etc.                              5
C! Author :-  A.J. Finch 1/6/96                                         6
C!     structure : subroutine                                           7
C -----------------------------------------------------------------     8
                                                                        9
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)                                
      INTEGER LMHLEN, LMHCOL, LMHROW                                    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          3
C                                                                       4
      COMMON /BCS/   IW(1000)                                           5
      INTEGER IW                                                        6
      REAL RW(1000)                                                     7
      EQUIVALENCE (RW(1),IW(1))                                         8
C                                                                       9
      common /evtsta/nevent(10)                                         
      COMMON /XSECTP/ SIGTOT,SIGELA,SIGVM(0:4,0:4),SIGINE,SIGDIR,       
     &                SIGLSD(2),SIGHSD(2),SIGLDD,SIGHDD,SIGCDF,         
     &                SIGPOM,SIGREG,SIGHAR,SIGTR1,SIGTR2,SIGLOO,SIGDPO, 
     &                SIG1SO,SIG1HA,SLOEL,SLOVM(4,4),SIGGEN(4),         
     &                FSUP(2)                                           
                                                                        
      COMMON /LEPCUT/ ECMIN,ECMAX,EEMIN1,EEMIN2,                        
     &                YMIN1,YMAX1,YMIN2,YMAX2,                          
     &                Q2MIN1,Q2MAX1,Q2MIN2,Q2MAX2,                      
     &                THMIN1,THMAX1,THMIN2,THMAX2                       
                                                                        
C                                                                       
      DOUBLE PRECISION EE1,EE2                                          
      COMMON/CPHJ01/EE1,EE2,LOUT,ILUMI,ILTOT 
      DIMENSION P1(4),P2(4)      
      real*4 EBEAM,WMIN,ELEC(5),POSI(5),LUMIF,QQ1,QQ2                   
      LOGICAL DEBUG                                                     
      PARAMETER(ALEPH_FUDGE=0.833)                                      
C                                                                       
      IFLAG = -1                                                        
      wmin = ecmin                                                      
      EBEAM = EE1                                                       
c                                                                       
c find the total lumi function for these settings                       
C                                                                       
      CALL GENLUM(IFLAG,EBEAM,WMIN,ELEC,POSI,LUMIF,QQ1,QQ2)             
C                                                                       
      ALUMI=LUMIF*FLOAT(ILUMI)/FLOAT(ILTOT)                             
      WRITE(6,10)                                                       
10    FORMAT(1H0///,                                                    
     1' Event generation complete statistics etc. follows...'           
     1////)                                                             
      WRITE(6,*)'                  '                                    
      WRITE(6,*)'                  '                                    
      WRITE(6,*)' Luminosity function =  ',ALUMI                        
      WRITE(6,*)' '                                                     
      WRITE(6,*)' '                                                     
      CALL EVENT(-2,P1,P2,FAC,IREJ)                                     
      WRITE(6,*)'                  '                                    
      WRITE(6,*)'                  '                                    
      WRITE(6,60)1000000.*SIGGEN(2)*ALUMI*ALEPH_FUDGE                   
60    FORMAT(1H0,//////' Total (e+ e-) cross section = ',               
     1F8.3,' nb '/////)                                                 
      WRITE(6,*)' '                                                     
      WRITE(6,*)' '                                                     
      ISTD = 0                                                          
      JGSTD = NLINK('GSTD',0)                                           
      IF(JGSTD.NE.0)THEN                                                
         ISTD = IW(JGSTD+1)                                             
      ENDIF                                                             
      IF ( ISTD.eq.1) call STDSTA                                       
      CALL AMCSTA                                                       
      WRITE(6,*)' General statistics    '                               
      WRITE(6,*)'                  '                                    
      WRITE(6,*)' total generated =             ',nevent(1)             
      WRITE(6,*)' kept after generation         ',nevent(2)             
      WRITE(6,*)' kept after standard selection ',nevent(3)             
      WRITE(6,*)' kept after user selection     ',nevent(4)             
      WRITE(6,*)' kept after both selection     ',nevent(5)             
      WRITE(6,*)' kept for control but failed   ',nevent(6)             
      frac = float(nevent(5)) / float(nevent(2))                        
      efrac = frac * sqrt(1./float(nevent(5))-1./float(nevent(2)))      
      WRITE(6,*)' |--------------------------------------- '            
      WRITE(6,*)' |  cross-section should be multiplied by '            
      WRITE(6,*)' |  selection efficiency :',frac,' +- ',efrac          
      WRITE(6,*)' |--------------------------------------- '            
      Call ugtsec
      RETURN                                                            
      END                                                               
      SUBROUTINE UGTSEC
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      common /evtsta/nevent(10)                                         
      COMMON /XSECTP/ SIGTOT,SIGELA,SIGVM(0:4,0:4),SIGINE,SIGDIR,       
     &                SIGLSD(2),SIGHSD(2),SIGLDD,SIGHDD,SIGCDF,         
     &                SIGPOM,SIGREG,SIGHAR,SIGTR1,SIGTR2,SIGLOO,SIGDPO, 
     &                SIG1SO,SIG1HA,SLOEL,SLOVM(4,4),SIGGEN(4),         
     &                FSUP(2)                                           
      COMMON /LEPCUT/ ECMIN,ECMAX,EEMIN1,EEMIN2,                        
     &                YMIN1,YMAX1,YMIN2,YMAX2,                          
     &                Q2MIN1,Q2MAX1,Q2MIN2,Q2MAX2,                      
     &                THMIN1,THMAX1,THMIN2,THMAX2                       
      real*4 EBEAM,WMIN,ELEC(5),POSI(5),LUMIF,QQ1,QQ2 
      DIMENSION P1(4),P2(4)                           
      real*4 XTOT,XACC,RTOT,RACC
C                                                                       
      DOUBLE PRECISION EE1,EE2                                          
      COMMON/CPHJ01/EE1,EE2,LOUT,ILUMI,ILTOT 

      PARAMETER(ALEPH_FUDGE=0.833) 
      PARAMETER(KINTYP=6009)                                            
      PARAMETER(NVERS=103) 
      IFLAG = -1                    
      wmin = ecmin                  
      EBEAM = EE1
      CALL GENLUM(IFLAG,EBEAM,WMIN,ELEC,POSI,LUMIF,QQ1,QQ2)
      ALUMI=LUMIF*FLOAT(ILUMI)/FLOAT(ILTOT) 
      NTOT = nevent(2) 
      NACC = nevent(5)
      XTOT = 1000000.*SIGGEN(2)*ALUMI*ALEPH_FUDGE
      RTOT = xtot /sqrt(float(ntot))
      frac = float(nevent(5)) / float(nevent(2)) 
      efrac = frac * sqrt(1./float(nevent(5))-1./float(nevent(2)))
      XACC = xtot * frac
      RACC = xacc/sqrt(float(nacc))
      is =1
      IDC = KINTYP
      IVER= NVERS
C
      ISEC =  KSECBK(IS,IDC,IVER,NTOT,NACC,XTOT,RTOT,XACC,RACC)
      CALL PRTABL('KSEC',0)
      RETURN
      END 
      SUBROUTINE SETCUT                                                 SETCUT 2
C------------------------------------------------------------           SETCUT 3
C! Interface routine for PHOJ01 generator                               SETCUT 4
C!  Set PHOJET parameters to standard values                            SETCUT 5
C! Author :-  A.J. Finch 1/6/96                                         SETCUT 6
C!     structure : subroutine                                           SETCUT 7
C -----------------------------------------------------------------     SETCUT 8
                                                                        SETCUT 9
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               SETCUT10
      SAVE                                                              SETCUT11
c                                                                       SETCUT12
      COMMON /LEPCUT/ ECMIN,ECMAX,EEMIN1,EEMIN2,                        SETCUT13
     &                YMIN1,YMAX1,YMIN2,YMAX2,                          SETCUT14
     &                Q2MIN1,Q2MAX1,Q2MIN2,Q2MAX2,                      SETCUT15
     &                THMIN1,THMAX1,THMIN2,THMAX2                       SETCUT16
c                                                                       SETCUT17
      COMMON /PROCES/ IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON(15)      SETCUT18
                                                                        SETCUT19
                                                                        SETCUT20
      DOUBLE PRECISION EE1,EE2                                          CPHOJ  2
      COMMON/CPHJ01/EE1,EE2,LOUT,ILUMI,ILTOT                            CPHOJ  3
                                                                        SETCUT22
                                                                        SETCUT23
      PARAMETER(PI=3.1416)                                              SETCUT24
C                                                                       SETCUT25
C  initialize standard PDFs                                             SETCUT26
      CALL SETPDF(2212,IDUM,5,6,0,0,-1)                                 SETCUT27
      CALL SETPDF(-2212,IDUM,5,6,0,0,-1)                                SETCUT28
      CALL SETPDF(22,IDUM,5,3,0,0,-1)                                   SETCUT29
      CALL SETPDF(45,IDUM,2,0,0,0,-1)                                   SETCUT30
      CALL SETPDF(211,IDUM,5,2,0,0,-1)                                  SETCUT31
      CALL SETPDF(-211,IDUM,5,2,0,0,-1)                                 SETCUT32
      CALL SETPDF(111,IDUM,5,2,0,0,-1)                                  SETCUT33
C                                                                       SETCUT34
C enable all processes                                                  SETCUT35
C                                                                       SETCUT36
CPROCESS       1  1  1  1  1  1  1  1  1  1  1                          SETCUT37
          DO KK = 1,11                                                  SETCUT38
           IPRON(KK) = 1                                                SETCUT39
          ENDDO                                                         SETCUT40
C*                                                                      SETCUT41
C*  electron taggers, any electron inside Aleph acceptance is regarded aSETCUT42
C   tagged provided it has energy greater than 10.0 GeV                 SETCUT43
C*                                                                      SETCUT44
           EEMIN1 = 20.0                                                SETCUT45
           THMIN1 = 0.03                                                SETCUT46
           THMAX1 = PI-0.03                                             SETCUT47
           EEMIN2 = 20.0                                                SETCUT48
           THMAX2 = THMAX1                                              SETCUT49
           THMIN2 = thmIN1                                              SETCUT50
C*                                                                      SETCUT51
C*  set beam energies                                                   SETCUT52
C*                                                                      SETCUT53
      EE1 = 87.5                                                        SETCUT54
      EE2 = 87.5                                                        SETCUT55
C*                                                                      SETCUT56
C*  gamma-gamma energy range                                            SETCUT57
C*                                                                      SETCUT58
           ECMIN = 2.0                                                  SETCUT59
           ECMAX = EE1+EE2                                              SETCUT60
C*                                                                      SETCUT61
C*  copy electrons to final state particles                             SETCUT62
C*                                                                      SETCUT63
CSETMODEL      -11    1                                                 SETCUT64
CSETMODEL      -12    1                                                 SETCUT65
          I = -11                                                       SETCUT66
          IVAL = 1                                                      SETCUT67
          CALL SETMDL(I,IVAL,1)                                         SETCUT68
          I = -12                                                       SETCUT69
          IVAL = 1                                                      SETCUT70
          CALL SETMDL(I,IVAL,1)                                         SETCUT71
                                                                        SETCUT72
      RETURN                                                            SETCUT73
      END                                                               SETCUT74
C                                                                       SETCUT75
      SUBROUTINE PHFILL                                                 PHFILL 2
C------------------------------------------------------------           PHFILL 3
C! Interface routine for PHOJ01 generator                               PHFILL 4
C!  HISTOGRAM LUND PARTICLES                                            PHFILL 5
C! Author :-  A.J. Finch 1/6/96                                         PHFILL 6
C!     structure : subroutine                                           PHFILL 7
C -----------------------------------------------------------------     PHFILL 8
C                                                                       PHFILL 9
C                                                                       PHFILL10
      PARAMETER (L1MST=200, L1PAR=200)                                  LUCOM  2
      PARAMETER (L2PAR=500, L2PARF=2000 )                               LUCOM  3
      PARAMETER (LJNPAR=4000)                                           LUCOM  4
      REAL    PARU,PARJ,PMAS,PARF,VCKM,BRAT,P7LU,V7LU                   LUCOM  5
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)   LUCOM  6
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)LUCOM  7
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),        LUCOM  8
     &                KFDP(L2PARF,5)                                    LUCOM  9
      COMMON /LUDAT4/ CHAF(L2PAR)                                       LUCOM 10
      CHARACTER*8 CHAF                                                  LUCOM 11
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5) LUCOM 12
C                                                                       PHFILL12
      DOUBLE PRECISION EE1,EE2                                          CPHOJ  2
      COMMON/CPHJ01/EE1,EE2,LOUT,ILUMI,ILTOT                            CPHOJ  3
                                                                        PHFILL14
C                                                                       PHFILL15
      DATA IB/1000/                                                     PHFILL16
C                                                                       PHFILL17
C first find the scattered electrons                                    PHFILL18
c                                                                       PHFILL19
      IE1 = 1                                                           PHFILL20
      IE2 = 2                                                           PHFILL21
C                                                                       PHFILL22
        PXW =                      - P7LU(IE1,1) - P7LU(IE2,1)          PHFILL23
        PYW =                      - P7LU(IE1,2) - P7LU(IE2,2)          PHFILL24
        PZW =                      - P7LU(IE1,3) - P7LU(IE2,3)          PHFILL25
        EW = EE1 + EE2            - P7LU(IE1,4) - P7LU(IE2,4)           PHFILL26
        WSQ = EW**2-PXW**2-PYW**2-PZW**2                                PHFILL27
        W = -1.0                                                        PHFILL28
        IF(WSQ.GE.0)W=SQRT(WSQ)                                         PHFILL29
        CALL HFILL(IB+1,P7LU(IE1,4),0.0,1.0)                            PHFILL30
        CALL HFILL(IB+3,P7LU(IE2,4),0.0,1.0)                            PHFILL31
        CALL HFILL(IB+9,W,0.0,1.0)                                      PHFILL32
C      ENDIF                                                            PHFILL33
C                                                                       PHFILL34
C                                                                       PHFILL35
C LOOP OVER THE LUND PARICLES LOOKING FOR FINAL STATE PARTICLES         PHFILL36
C   AND HISTOGRAM THEM                                                  PHFILL37
C                                                                       PHFILL38
      DO  I = 3,N7LU                                                    PHFILL39
       IF(K7LU(I,1).EQ.1)THEN                                           PHFILL40
           PTOT = SQRT(P7LU(I,1)**2 +                                   PHFILL41
     1                 P7LU(I,2)**2 +                                   PHFILL42
     1                 P7LU(I,3)**2)                                    PHFILL43
           PTS  = P7LU(I,1)**2 + P7LU(I,2)**2                           PHFILL44
           PT = SQRT(PTS)                                               PHFILL45
           CT = P7LU(I,3)/PTOT                                          PHFILL46
                                                                        PHFILL47
            CALL HFILL(IB+11,PT,0.0,1.0)                                PHFILL48
            CALL HFILL(IB+10,CT,0.0,1.0)                                PHFILL49
       ENDIF                                                            PHFILL50
      ENDDO                                                             PHFILL51
      RETURN                                                            PHFILL52
      END                                                               PHFILL53
      SUBROUTINE USECUT                                                 USECUT 2
C------------------------------------------------------------           USECUT 3
C! Interface routine for PHOJ01 generator                               USECUT 4
C!  set cuts from the data cards                                        USECUT 5
C! Author :-  A.J. Finch 1/6/96                                         USECUT 6
C!     structure : subroutine                                           USECUT 7
C -----------------------------------------------------------------     USECUT 8
                                                                        USECUT 9
C                                                                       USECUT10
C                                                                       USECUT11
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               USECUT12
      SAVE                                                              USECUT13
C                                                                       USECUT14
c                                                                       USECUT15
      COMMON /LEPCUT/ ECMIN,ECMAX,EEMIN1,EEMIN2,                        USECUT16
     &                YMIN1,YMAX1,YMIN2,YMAX2,                          USECUT17
     &                Q2MIN1,Q2MAX1,Q2MIN2,Q2MAX2,                      USECUT18
     &                THMIN1,THMAX1,THMIN2,THMAX2                       USECUT19
c                                                                       USECUT20
      COMMON /PROCES/ IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON(15)      USECUT21
      CHARACTER*8 MDLNA                                                 USECUT22
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)      USECUT23
      COMMON /CUTOFF/ PTCUT(4),CUTMU(4),FPS(4),FPH(4),PSOMIN,XSOMIN     USECUT24
      COMMON /HADRON/ QMASS(6),BET,PCOUDI,PNORM,VALPRG(2),NFS           USECUT25
                                                                        USECUT26
                                                                        USECUT27
      DOUBLE PRECISION EE1,EE2                                          CPHOJ  2
      COMMON/CPHJ01/EE1,EE2,LOUT,ILUMI,ILTOT                            CPHOJ  3
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
C                                                                       USECUT30
                                                                        USECUT31
C                                                                       USECUT32
C PDFL           IDPDG IPAR ISET IEXT                                   USECUT33
C                                                                       USECUT34
C PDFL card can override choice of Parton                               USECUT35
C  distribution function, for particle type                             USECUT36
C  IDPDG (PDG code), use IPAR (authors)                                 USECUT37
C  set ISET,IEXT.See Phojet manual                                      USECUT38
C                                                                       USECUT39
                                                                        USECUT40
      IND = NAMIND('PDFL') + 1                                          USECUT41
 9       IND = IW(IND-1)                                                USECUT42
         IF(IND.NE.0)THEN                                               USECUT43
          IDPDG = IW(IND-2)                                             USECUT44
          LENGTH = IW(IND)                                              USECUT45
          IF(LENGTH.GE.1)IGRP  = IW(IND+1)                              USECUT46
          IF(LENGTH.GE.2)ISET  = IW(IND+2)                              USECUT47
          IF(LENGTH.GE.3)IEXT  = IW(IND+3)                              USECUT48
          IPAVAL=0                                                      USECUT49
          CALL SETPDF(IDPDG,ITYPE,IGRP,ISET,IEXT,IPAVAL,-1)             USECUT50
          GOTO 9                                                        USECUT51
         ENDIF                                                          USECUT52
C                                                                       USECUT53
C                                                                       USECUT54
C                                                                       USECUT55
C PROC      I1 - I11                                                    USECUT56
C                                                                       USECUT57
C the default is to generate all process types, but                     USECUT58
C with PROC card one can select type 1 to 11, 1= on, 0 = off            USECUT59
C                                                                       USECUT60
        NAMEP  = NAMIND('PROC')                                         USECUT61
        NBANK   = IW(NAMEP)                                             USECUT62
        IF(NBANK.NE.0) THEN                                             USECUT63
          LENGTH = IW(NBANK)                                            USECUT64
          DO KK = 1,LENGTH                                              USECUT65
           IPRON(KK) = IW(NBANK+KK)                                     USECUT66
          ENDDO                                                         USECUT67
        ENDIF                                                           USECUT68
C                                                                       USECUT69
C ETG1        1.0   0.001   0.9999   0.0  5.0   0.0   0.03              USECUT70
C                                                                       USECUT71
C this card controls the selection applied to electron 1                USECUT72
C                                                                       USECUT73
        NAMEP  = NAMIND('ETG1')                                         USECUT74
        NBANK   = IW(NAMEP)                                             USECUT75
        IF(NBANK.NE.0) THEN                                             USECUT76
          LENGTH = IW(NBANK)                                            USECUT77
           IF(LENGTH.GE.1)EEMIN1 = RW(NBANK+1)                          USECUT78
           IF(LENGTH.GE.2)THMIN1 = RW(NBANK+2)                          USECUT79
           IF(LENGTH.GE.3)THMAX1 = RW(NBANK+3)                          USECUT80
        ENDIF                                                           USECUT81
C                                                                       USECUT82
C                                                                       USECUT83
C this card controls the selectio applied to electron 2                 USECUT84
C                                                                       USECUT85
        NAMEP  = NAMIND('ETG2')                                         USECUT86
        NBANK   = IW(NAMEP)                                             USECUT87
        IF(NBANK.NE.0) THEN                                             USECUT88
          LENGTH = IW(NBANK)                                            USECUT89
           IF(LENGTH.GE.1)EEMIN2 = RW(NBANK+1)                          USECUT90
           IF(LENGTH.GE.2)THMIN2 = RW(NBANK+2)                          USECUT91
           IF(LENGTH.GE.3)THMAX2 = RW(NBANK+3)                          USECUT92
        ENDIF                                                           USECUT93
C                                                                       USECUT94
C GCON EBEAM WMIN WMAX                                                  USECUT95
C                                                                       USECUT96
C GCON is the overall control of beam energy,and cuts on W of final     USECUT97
C              state                                                    USECUT98
        NAMEP  = NAMIND('GCON')                                         USECUT99
        NBANK   = IW(NAMEP)                                             USECU100
        IF(NBANK.NE.0) THEN                                             USECU101
          LENGTH = IW(NBANK)                                            USECU102
           IF(LENGTH.GE.1)EBEAM  = RW(NBANK+1)                          USECU103
           EE1 = EBEAM                                                  USECU104
           EE2 = EBEAM                                                  USECU105
           IF(LENGTH.GE.2)ECMIN  = RW(NBANK+2)                          USECU106
           ECMAX = EE1 + EE2                                            BBL00151
           IF(LENGTH.GE.3)ECMAX  = RW(NBANK+3)                          USECU107
        ENDIF                                                           USECU108
C                                                                       USECU109
C GPTC ptcut                                                            USECU110
C                                                                       USECU111
C there are 4 PT cuts which GPTC allows you to set                      USECU112
C   set phojet manual for details                                       USECU113
C                                                                       USECU114
        NAMEP  = NAMIND('GPTC')                                         USECU115
        NBANK   = IW(NAMEP)                                             USECU116
        IF(NBANK.NE.0) THEN                                             USECU117
          LENGTH = IW(NBANK)                                            USECU118
           DO I = 1,4                                                   USECU119
            IF(LENGTH.GE.I)PTCUT(I) = RW(NBANK+I)                       USECU120
            WRITE(6,*)' PTCUT ',I,' = ',PTCUT(I)                        USECU121
           ENDDO                                                        USECU122
        ENDIF                                                           USECU123
C  lower energy limit for initialization                                USECU124
      PARMDL(19) = ECMIN                                                USECU125
C                                                                       USECU126
C                                                                       USECU127
C MODEL parameters ISWMDL, AND PARMDL                                   USECU128
C can be set by cards IMDL, and PMDL                                    USECU129
C                                                                       USECU130
      IND = NAMIND('PMDL') + 1                                          USECU131
 10      IND = IW(IND-1)                                                USECU132
         IF(IND.NE.0)THEN                                               USECU133
          PARAM = RW(IND+1)                                             USECU134
          LNUM = IW(IND-2)                                              USECU135
          PARMDL(LNUM) = PARAM                                          USECU136
          WRITE(6,*)' PARMDL(',LNUM,') = ',PARAM                        USECU137
          GOTO 10                                                       USECU138
         ENDIF                                                          USECU139
      IND = NAMIND('IMDL') + 1                                          USECU140
 11      IND = IW(IND-1)                                                USECU141
         IF(IND.NE.0)THEN                                               USECU142
          IPARAM = IW(IND+1)                                            USECU143
          LNUM = IW(IND-2)                                              USECU144
          ISWMDL(LNUM) = IPARAM                                         USECU145
          WRITE(6,*)' ISWMDL(',LNUM,') = ',IPARAM                       USECU146
          GOTO 11                                                       USECU147
         ENDIF                                                          USECU148
      IND = NAMIND('IPDL') + 1                                          USECU149
 13      IND = IW(IND-1)                                                USECU150
         IF(IND.NE.0)THEN                                               USECU151
          IPARAM = IW(IND+1)                                            USECU152
          LNUM = IW(IND-2)                                              USECU153
          IPAMDL(LNUM) = IPARAM                                         USECU154
          WRITE(6,*)' IPAMDL(',LNUM,') = ',IPARAM                       USECU155
          GOTO 13                                                       USECU156
         ENDIF                                                          USECU157
C                                                                       USECU158
C QMASS can be set by card QMAS                                         USECU159
C                                                                       USECU160
      IND = NAMIND('QMAS') + 1                                          USECU161
 12      IND = IW(IND-1)                                                USECU162
         IF(IND.NE.0)THEN                                               USECU163
          PARAM = RW(IND+1)                                             USECU164
          LNUM = IW(IND-2)                                              USECU165
          QMASS(LNUM) = PARAM                                           USECU166
          WRITE(6,*)' QMASS(',LNUM,') = ',PARAM                         USECU167
          GOTO 12                                                       USECU168
         ENDIF                                                          USECU169
                                                                        USECU170
      RETURN                                                            USECU171
      END                                                               USECU172
      SUBROUTINE AMCSEL(KEEP)                                           AMCSEL 2
C------------------------------------------------------------           AMCSEL 3
C! Interface routine for PHOJ01 generator                               AMCSEL 4
C! event selection routine... uses lund common to select event          AMCSEL 5
C! Author :-  A.J. Finch 1/6/96                                         AMCSEL 6
C!     structure : subroutine                                           AMCSEL 7
C -----------------------------------------------------------------     AMCSEL 8
      LOGICAL KEEP,FIRST                                                AMCSEL 9
      SAVE FIRST                                                        AMCSEL10
      DATA FIRST/.TRUE./                                                AMCSEL11
      DATA IREAD/0/,IKEPT/0/,IREJECT/0/                                 BBL00152
      IF(FIRST)THEN                                                     AMCSEL12
C                                                                       AMCSEL13
      FIRST=.FALSE.                                                     AMCSEL14
      WRITE(6,*)' '                                                     AMCSEL15
      WRITE(6,*)' AMCSEL : This is a ''do nothing'' version from'       AMCSEL16
      WRITE(6,*)'            PHOJ01 source                      '       AMCSEL17
      WRITE(6,*)'   You may provide your own version to reject  '       AMCSEL18
      WRITE(6,*)'   events using information in the LUND commons.'      AMCSEL19
      WRITE(6,*)'   Please consult the documentation for more   '       AMCSEL20
      WRITE(6,*)'    details.'                                          AMCSEL21
      WRITE(6,*)'   '                                                   AMCSEL22
      WRITE(6,*)'  Here is an example... '                              AMCSEL23
      WRITE(6,*)'   '                                                   AMCSEL24
      WRITE(6,*)'      SUBROUTINE AMCSEL(KEEP)'                         AMCSEL25
      WRITE(6,*)                                                        AMCSEL26
     1'      PARAMETER (L1MST=200, L1PAR=200) '                         AMCSEL27
      WRITE(6,*)                                                        AMCSEL28
     1'      PARAMETER (L2PAR=500, L2PARF=2000 )'                       AMCSEL29
      WRITE(6,*)                                                        AMCSEL30
     1'      PARAMETER (LJNPAR=4000)             '                      AMCSEL31
      WRITE(6,*)                                                        AMCSEL32
     1'      REAL    PARU,PARJ,PMAS,PARF,VCKM,BRAT,P7LU,V7LU'           AMCSEL33
      WRITE(6,*)                                                        AMCSEL34
     1'      COMMON /LUDAT1/ MSTU(L1MST),'//                            AMCSEL35
     1'PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)   '                          AMCSEL36
      WRITE(6,*)                                                        AMCSEL37
     1'      COMMON /LUDAT2/ KCHG(L2PAR,3),'//                          AMCSEL38
     1'PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)'                            AMCSEL39
      WRITE(6,*)                                                        AMCSEL40
     1'      COMMON /LUDAT3/ MDCY(L2PAR,3),'//                          AMCSEL41
     1'MDME(L2PARF,2),BRAT(L2PARF),        '                            AMCSEL42
      WRITE(6,*)                                                        AMCSEL43
     1'     &                KFDP(L2PARF,5)'                            AMCSEL44
      WRITE(6,*)                                                        AMCSEL45
     1'      COMMON /LUDAT4/ CHAF(L2PAR)   '                            AMCSEL46
      WRITE(6,*)                                                        AMCSEL47
     1'      CHARACTER*8 CHAF              '                            AMCSEL48
      WRITE(6,*)                                                        AMCSEL49
     1'      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),'//                    AMCSEL50
     1'P7LU(LJNPAR,5),V7LU(LJNPAR,5)'                                   AMCSEL51
      WRITE(6,*)                                                        AMCSEL52
     1'      LOGICAL KEEP'                                              AMCSEL53
      WRITE(6,*)'      NCHRG = 0'                                       AMCSEL54
      WRITE(6,*)'      ECHRG = 0.0'                                     AMCSEL55
      WRITE(6,*)'      DO  I = 1,N7LU'                                  AMCSEL56
      WRITE(6,*)'       IF(K7LU(I,1).EQ.1)THEN'                         AMCSEL57
      WRITE(6,*)'           PTOT = SQRT(P7LU(I,1)**2 + '                AMCSEL58
      WRITE(6,*)'     1                 P7LU(I,2)**2 + '                AMCSEL59
      WRITE(6,*)'     1                 P7LU(I,3)**2)'                  AMCSEL60
      WRITE(6,*)'           PTS  = P7LU(I,1)**2 + P7LU(I,2)**2'         AMCSEL61
      WRITE(6,*)'           PT = SQRT(PTS)'                             AMCSEL62
      WRITE(6,*)'           CT = P7LU(I,3)/PTOT'                        AMCSEL63
      WRITE(6,*)'           CHRGE = FLOAT(LUCHGE(K7LU(I,2)))'           AMCSEL64
      WRITE(6,*)'           IF((CHRGE.NE.0).AND.(ABS(CT).LT.0.98))THEN' AMCSEL65
      WRITE(6,*)'                 NCHRG = NCHRG + 1'                    AMCSEL66
      WRITE(6,*)'                 ECHRG = ECHRG + P7LU(I,4)'            AMCSEL67
      WRITE(6,*)'           ENDIF'                                      AMCSEL68
      WRITE(6,*)'       ENDIF'                                          AMCSEL69
      WRITE(6,*)'      ENDDO'                                           AMCSEL70
      WRITE(6,*)'      KEEP = .FALSE.'                                  AMCSEL71
      WRITE(6,*)'      IF(ECHRG.GT.2.0.AND.NCHRG.GT.1)KEEP = .TRUE.'    AMCSEL72
      WRITE(6,*)'      RETURN'                                          AMCSEL73
      WRITE(6,*)'      END'                                             AMCSEL74
      ENDIF                                                             AMCSEL75
      KEEP = .TRUE.                                                     AMCSEL76
        IREAD = IREAD + 1                                               BBL00153
C    include your selection code here                                   BBL00154
      IF(KEEP)THEN                                                      BBL00155
        IKEPT = IKEPT + 1                                               BBL00156
      ELSE                                                              BBL00157
        IREJECT = IREJECT + 1                                           BBL00158
      ENDIF                                                             BBL00159
      RETURN                                                            BBL00160
      ENTRY AMCSTA                                                      BBL00161
      WRITE(6,*)' '                                                     BBL00162
      WRITE(6,*)'  AMCSEL STATISTICS '                                  BBL00163
      WRITE(6,*)' ================== '                                  BBL00164
      WRITE(6,*)' '                                                     BBL00165
      WRITE(6,*)' ',IREAD,' EVENTS ENTERED AMCSEL '                     BBL00166
      WRITE(6,*)' ',IKEPT,' WERE KEPT '                                 BBL00167
      WRITE(6,*)' ',IREJECT,' WERE REJECTED '                           BBL00168
      WRITE(6,*)' '                                                     BBL00169
      WRITE(6,*)' '                                                     BBL00170
      RETURN                                                            AMCSEL77
      END                                                               AMCSEL78
      SUBROUTINE   PHORUN                                               PHORUN 2
C---------------------------------------------------------------        PHORUN 3
C!  Build the KPAR and  RLEP bank    (PHOJ01)                           PHORUN 4
C                                                                       PHORUN 5
C CALLED BY - ASKUSI                                                    PHORUN 6
C CALLS     - ALTABL,PRTABL                                             PHORUN 7
C  A.J.Finch June 1996 adapted to PHOJ01 from PHOT02 version            PHORUN 8
C --------------------------------------------------------------------  PHORUN 9
C - AJF ,BBL                                                            PHORUN10
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               PHORUN11
      PARAMETER (LMCGCO=20)                                             PHORUN12
      INTEGER ALTABL,ITABL,ALRLEP                                       PHORUN13
      REAL TABL                                                         PHORUN14
      EXTERNAL ALTABL,ALRLEP                                            PHORUN15
      DIMENSION TABL(LMCGCO),ITABL(LMCGCO)                              PHORUN16
C                                                                       PHORUN17
      COMMON /LEPCUT/ ECMIN,ECMAX,EEMIN1,EEMIN2,                        PHORUN18
     &                YMIN1,YMAX1,YMIN2,YMAX2,                          PHORUN19
     &                Q2MIN1,Q2MAX1,Q2MIN2,Q2MAX2,                      PHORUN20
     &                THMIN1,THMAX1,THMIN2,THMAX2                       PHORUN21
c                                                                       PHORUN22
      COMMON /PROCES/ IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON(15)      PHORUN23
c                                                                       PHORUN24
c                                                                       PHORUN25
      DOUBLE PRECISION EE1,EE2                                          CPHOJ  2
      COMMON/CPHJ01/EE1,EE2,LOUT,ILUMI,ILTOT                            CPHOJ  3
                                                                        PHORUN27
C                                                                       PHORUN28
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
C                                                                       PHORUN30
       EQUIVALENCE (ITABL(1),TABL(1))                                   PHORUN31
       TABL(1)  = EE1                                                   PHORUN32
       TABL(2)  = ECMIN                                                 PHORUN33
       TABL(3)  = ECMAX                                                 PHORUN34
       TABL(4)  = 0.0                                                   PHORUN35
       TABL(5)  = 0.0                                                   PHORUN36
       TABL(6)  = 0.0                                                   PHORUN37
       JSVRT = NLINK('SVRT',0)                                          PHORUN38
       IF(JSVRT.NE.0)THEN                                               PHORUN39
            TABL( 4) = RW(JSVRT + 1)                                    PHORUN40
            TABL( 5) = RW(JSVRT + 2)                                    PHORUN41
            TABL( 6) = RW(JSVRT + 3)                                    PHORUN42
       ENDIF                                                            PHORUN43
                                                                        PHORUN44
C  Fill the KPAR bank with the generator parameters                     PHORUN45
       NCOL = 6                                                         PHORUN46
       NROW = 1                                                         PHORUN47
       JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'F','C')                    PHORUN48
       CALL PRTABL('KPAR',0)                                            PHORUN49
C  Fill RLEP bank                                                       PHORUN50
       IEBEAM = NINT(EE1 *1000  )                                       PHORUN51
       JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)                              PHORUN52
       CALL PRTABL('RLEP',0)                                            PHORUN53
      RETURN                                                            PHORUN54
      END                                                               PHORUN55
      SUBROUTINE GENLUM(IFLAG,EBEAM,WMIN,ELEC,POSI,TOTAL,QQ1,QQ2)       GENLUM 2
C------------------------------------------------------------           GENLUM 3
C! Interface routine for PHOJ01 generator                               GENLUM 4
C!  Control calculation of the luminosity function                      GENLUM 5
C!  Author :-  A.J. Finch 1/6/96                                        GENLUM 6
C!  Modified :- AJF  8/1/97 add variable test to prevent                GENLUM 7
C!                   HP optimization removing calls to rndm             GENLUM 8
C!     structure : subroutine                                           GENLUM 9
C!  Input:                                                              GENLUM10
C!     IFLAG : control flag                                             GENLUM11
C!                 0 - return one sample of lumi function               GENLUM12
C!                 1 - as 0 but times FGVDM                             GENLUM13
C!                -1 - return total F as calculated on first call       GENLUM14
C!     EBEAM :  beam energy - must not change after first call          GENLUM15
C!     WMIN  :  minimum invariant mass                                  GENLUM16
C!  Output                                                              GENLUM17
C!      ELEC :  Real array dimension>4 4 vector of electron             GENLUM18
C!      POSI :  Real array dimension>4 4 vector of positron             GENLUM19
C!      TOTAL:  Total F if IFLAG = -1                                   GENLUM20
C!      QQ1  :   Q^2 of electron                                        GENLUM21
C!      QQ2  :   Q^2 of positron                                        GENLUM22
C -----------------------------------------------------------------     GENLUM23
C                                                                       GENLUM24
      REAL ELEC(5),POSI(5)                                              GENLUM25
      DIMENSION X(10)                                                   GENLUM26
      REAL STEP(10),STOTAL                                              GENLUM27
      LOGICAL FIRST                                                     GENLUM28
      SAVE FIRST,FMAX,STOTAL                                            GENLUM29
      PARAMETER(NDIM=5)                                                 GENLUM30
      PARAMETER(xmin = 0.0)                                             GENLUM31
      PARAMETER(xmax = 1.0)                                             GENLUM32
      PARAMETER(sstart=(xmax-xmin)/14)                                  GENLUM33
      DATA FIRST/.TRUE./                                                GENLUM34
C                                                                       GENLUM35
C                                                                       GENLUM36
C ON THE FIRST CALL INTEGRATE THE FUNCTION                              GENLUM37
C AND IN THE PROCESS FIND FMAX                                          GENLUM38
C                                                                       GENLUM39
      IF(FIRST)THEN                                                     GENLUM40
       FIRST=.FALSE.                                                    GENLUM41
       FMAX   = 0.0                                                     GENLUM42
C                                                                       GENLUM43
C INTEGRATE FUNCTION                                                    GENLUM44
C                                                                       GENLUM45
      TOTAL = 0.0                                                       GENLUM46
      DO I = 1,NDIM                                                     GENLUM47
        X(I) = 0.0                                                      GENLUM48
        STEP(I)=SSTART                                                  GENLUM49
      ENDDO                                                             GENLUM50
      VOLUME=1.0                                                        GENLUM51
      DO I = 1,NDIM                                                     GENLUM52
        VOLUME=VOLUME*STEP(I)                                           GENLUM53
      ENDDO                                                             GENLUM54
      IDIM = 1                                                          GENLUM55
C      STEP = (XMAX-XMIN)/FLOAT(NLOOP)                                  GENLUM56
                                                                        GENLUM57
98    CALL FUNGG(X,EBEAM,WMIN,FX,ELEC,POSI,QQ1,QQ2)                     GENLUM58
      TEST=FX                                                           GENLUM59
      IF(IFLAG.EQ.1)TEST = TEST*FGVDM(QQ1)*FGVDM(QQ2)                   GENLUM60
      IF(TEST.GT.FMAX)THEN                                              GENLUM61
           FMAX =TEST                                                   GENLUM62
      ENDIF                                                             GENLUM63
C                                                                       GENLUM64
       TOTAL = TOTAL + TEST*VOLUME                                      GENLUM65
C                                                                       GENLUM66
C INCREMENT STEPS THROUGH THE VOLUME                                    GENLUM67
C                                                                       GENLUM68
99     X(IDIM) = X(IDIM) + STEP(IDIM)                                   GENLUM69
       IF(X(IDIM).GT.XMAX)THEN                                          GENLUM70
        X(IDIM)=XMIN                                                    GENLUM71
        STEP(IDIM)=SSTART                                               GENLUM72
        IF(IDIM.EQ.NDIM)STEP(IDIM)=SSTART/2.                            GENLUM73
        IDIM = IDIM + 1                                                 GENLUM74
        IF(IDIM.GT.NDIM)GOTO 1000                                       GENLUM75
        GOTO 99                                                         GENLUM76
       ENDIF                                                            GENLUM77
       IDIM = 1                                                         GENLUM78
       GOTO 98                                                          GENLUM79
1000  CONTINUE                                                          GENLUM80
C                                                                       GENLUM81
C                                                                       GENLUM82
      STOTAL=TOTAL                                                      GENLUM83
      ENDIF                                                             GENLUM84
c                                                                       GENLUM85
c for flag = -1 just return the saved total from                        GENLUM86
c initial call                                                          GENLUM87
c                                                                       GENLUM88
      IF(IFLAG.EQ.-1)THEN                                               GENLUM89
        TOTAL=STOTAL                                                    GENLUM90
        RETURN                                                          GENLUM91
      ENDIF                                                             GENLUM92
C                                                                       GENLUM93
C GENERATE ONE EVENT BY MONTE CARLO SAMPLING                            GENLUM94
C                                                                       GENLUM95
        NTRIAL = 0                                                      GENLUM96
1       CONTINUE                                                        GENLUM97
        NTRIAL = NTRIAL + 1                                             GENLUM98
        DO J = 1,NDIM                                                   GENLUM99
c this should force compiler to call rndm!                              GENLU100
           test = rndm(test)                                            GENLU101
           x(j) = test                                                  GENLU102
        ENDDO                                                           GENLU103
        CALL FUNGG(X,EBEAM,WMIN,FX,ELEC,POSI,QQ1,QQ2)                   GENLU104
        TEST_fx=FX                                                      GENLU105
        IF(IFLAG.EQ.1)TEST_fx = TEST_fx*FGVDM(QQ1)*FGVDM(QQ2)           GENLU106
        IF(TEST_fx.GT.FMAX)THEN                                         GENLU107
           FMAX =TEST_fx                                                GENLU108
        ENDIF                                                           GENLU109
c this should force compiler to call rndm!                              GENLU110
        test = rndm(test)                                               GENLU111
        IF(test*FMAX.GT.TEST_fx)GOTO 1                                  GENLU112
      RETURN                                                            GENLU113
      END                                                               GENLU114
C                                                                       GENLU115
      SUBROUTINE FUNGG(Y,EBEAM,WMIN,FX,ELEC,POSI,QQ1,QQ2)               FUNGG  2
C------------------------------------------------------------           FUNGG  3
C! Interface routine for PHOJ01 generator                               FUNGG  4
C!  Calculate the luminosity function                                   FUNGG  5
C!  Author :-  A.J. Finch 1/6/96                                        FUNGG  6
C!     structure : subroutine                                           FUNGG  7
C! Calculate the Luminosity Function in gamma gamma event               FUNGG  8
C                                                                       FUNGG  9
C INPUT  parameters :                                                   FUNGG 10
C                     Y : random variables (array)                      FUNGG 11
C                EBEAM  : beam energy                                   FUNGG 12
C                WMIN   : minimum invariant mass                        FUNGG 13
C                                                                       FUNGG 14
C                                                                       FUNGG 15
C OUTPUT parameters :                                                   FUNGG 16
C                    FX   : value of lumi function at these x           FUNGG 17
C            ELEC,POSI    : array of scattered electron and positron    FUNGG 18
C                            4-momenta,mass                             FUNGG 19
C                    QQ1  : Q^2 of electron 1                           FUNGG 20
C                    QQ2  : Q^2 of election 2                           FUNGG 21
C CALLS     - MAPVAR                                                    FUNGG 22
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version        FUNGG 23
C  A.J.Finch April    1996 cleaned up for stand alone use               FUNGG 24
C----------------------------------------------------------------       FUNGG 25
C      THIS PROGRAM CALCULATES DL/DZ OF LUMINOSITY FUNCTION             FUNGG 26
C      WRITTEN BY S.KAWABATA                                            FUNGG 27
C      MODIFIED BY H.WRIEDT     COPIED     12.03.81   19:55             FUNGG 28
C      MODIFIED BY H.WRIEDT     LAST MOD   14.03.81   16:15             FUNGG 29
C                                                                       FUNGG 30
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)                              FUNGG 31
       DOUBLE PRECISION ME,ME2,MU,MU2                                   FUNGG 32
                                                                        FUNGG 33
       REAL*4 ELEC(5),POSI(5)                                           FUNGG 34
       REAL*4 QQ1,QQ2,Y(10),DJDX,SIG,EBEAM,ZS4,W,WMIN,FX                FUNGG 35
c                                                                       FUNGG 36
C            X1    : Y(1)                                               FUNGG 37
C            TH1   : Y(2)                                               FUNGG 38
C            TH2   : Y(3)                                               FUNGG 39
C            PH    : Y(4)                                               FUNGG 40
C            W     : Y(5)                                               FUNGG 41
                                                                        FUNGG 42
C                                                                       FUNGG 43
      LOGICAL FIRST                                                     FUNGG 44
C                                                                       FUNGG 45
      DATA XMAX / 1./                                                   FUNGG 46
      DATA FIRST/.TRUE./                                                FUNGG 47
      SAVE    FIRST,ALF,CONST,ZMIN,ZMAX,PI,PIDEG,RADDEG,                FUNGG 48
     1        ME,ME2,TMIN,TMAX,DJ                                       FUNGG 49
                                                                        FUNGG 50
       IF(FIRST)THEN                                                    FUNGG 51
          ALF    = 1.0 / 137.04                                         FUNGG 52
          PI     = ACOS(-1.0)                                           FUNGG 53
          CONST  =  ( ALF**2 )  /  ( 32.0 * PI**3 )                     FUNGG 54
          ZMIN   = WMIN / EBEAM * 0.5                                   FUNGG 55
          ZMAX   = 1.0                                                  FUNGG 56
          PIDEG  = 180                                                  FUNGG 57
          RADDEG = PIDEG / PI                                           FUNGG 58
          ME     = 0.000511                                             FUNGG 59
          ME2    = ME*ME                                                FUNGG 60
          TMIN  = 1.0E-10                                               FUNGG 61
          TMAX   =DBLE( PI)                                             FUNGG 62
          DJ    = 2.*PI                                                 FUNGG 63
          FIRST = .FALSE.                                               FUNGG 64
       ENDIF                                                            FUNGG 65
       X8    =DBLE( Y(5))                                               FUNGG 66
       CALL MAPVAR(ZS,X8,ZMIN,ZMAX,PROB)                                FUNGG 67
c                                                                       FUNGG 68
      NERR  = 0                                                         FUNGG 69
      E     = DBLE( EBEAM)                                              FUNGG 70
      E2    = E*E                                                       FUNGG 71
      FME   = ME/E                                                      FUNGG 72
      FME2  = FME*FME                                                   FUNGG 73
      FMA   = (1.-FME)*(1.+FME)                                         FUNGG 74
      XMAX  = 1.D0-FME                                                  FUNGG 75
      PB    = (E-ME)*(E+ME)                                             FUNGG 76
      PB    = SQRT(PB)                                                  FUNGG 77
      PB    = E- ME2/(E+PB)                                             FUNGG 78
      S     = 4.0 * EBEAM * EBEAM                                       FUNGG 79
      RAMD  = S*(S-4.*ME2)                                              FUNGG 80
      RAMD  = 1./SQRT(RAMD)                                             FUNGG 81
      Z2    = ZS*ZS                                                     FUNGG 82
      XMIN  = (Z2+0.5*FME+0.5*FME2)/(1.D0-0.5*FME)                      FUNGG 83
C                                                                       FUNGG 84
C                                                                       FUNGG 85
C==========> SAMPLE X1                                                  FUNGG 86
C ( YTEMP - DOUBLE PRECISION, Y - REAL*4)                               FUNGG 87
      YTEMP =DBLE( Y(1))                                                FUNGG 88
      CALL MAPX1(X1,YTEMP,XMIN,XMAX,DX1)                                FUNGG 89
C==========> SAMPLE THETA 1                                             FUNGG 90
      YTEMP =DBLE( Y(2))                                                FUNGG 91
      CALL MAPX1(TH1,YTEMP,TMIN,TMAX,DT1)                               FUNGG 92
      CT1   = COS(TH1)                                                  FUNGG 93
      ST1   = SIN(TH1)                                                  FUNGG 94
      DT1   = DT1*ST1                                                   FUNGG 95
C++++++++++++++++++++++++++++++++                                       FUNGG 96
C==========> SAMPLE THETA 2                                             FUNGG 97
      YTEMP =DBLE( Y(3))                                                FUNGG 98
      CALL MAPX1(TH2,YTEMP,TMIN,TMAX,DT2)                               FUNGG 99
      CT2   = COS(TH2)                                                  FUNGG100
      ST2   = SIN(TH2)                                                  FUNGG101
      DT2   = DT2*ST2                                                   FUNGG102
C++++++++++++++++++++++++++++++++                                       FUNGG103
C==========> SAMPLE PHI                                                 FUNGG104
      PH    = 2.*PI*DBLE(Y(4))                                          FUNGG105
      CPH   = COS(PH)                                                   FUNGG106
C==========> DEFINE X2                                                  FUNGG107
      CT    = ST1*ST2*CPH-CT1*CT2                                       FUNGG108
      alimit = 1 - 1e-10                                                FUNGG109
      IF(ABS(CT).GT.alimit) GO TO 1000                                  FUNGG110
      CTSQ  = CT*CT                                                     FUNGG111
      X1MM  = (1.-X1)                                                   FUNGG112
      X1MP  = (1.+X1)                                                   FUNGG113
      FAC1  = (X1MM+FME)*(X1MM-FME)                                     FUNGG114
      FAC2  = 1.-X1+2.*Z2-FME2                                          FUNGG115
      A     = FAC1*CTSQ - X1MP*X1MP                                     FUNGG116
      B     = FAC1*CTSQ - X1MP*FAC2                                     FUNGG117
      C     = FMA*FAC1*CTSQ - FAC2*FAC2                                 FUNGG118
      D     = B*B - A*C                                                 FUNGG119
C-----------------------------------------                              FUNGG120
      IF(D.LT.0.) GO TO 1000                                            FUNGG121
      PXX   = (1.-X1)**2-FME2                                           FUNGG122
      IF(PXX.LT.0.) GO TO 1000                                          FUNGG123
      PX1   = SQRT(PXX)*CT                                              FUNGG124
      PY1   = X1-1.-2.*Z2+FME2                                          FUNGG125
      SD    = SQRT(D)                                                   FUNGG126
      X2    = (B + SD)/A                                                FUNGG127
      IF(X2.LT.XMIN.OR.X2.GT.XMAX) GO TO 500                            FUNGG128
      PXX   = (1.-X2)**2-FME2                                           FUNGG129
      IF(PXX.LT.0.) GO TO 1000                                          FUNGG130
      PXT   = SQRT(PXX)*PX1                                             FUNGG131
      PYT   = X2+X1*X2+PY1                                              FUNGG132
      IF(PXT*PYT.GE.0.) GO TO 600                                       FUNGG133
  500 X2    = (B - SD)/A                                                FUNGG134
      PXX   = (1.-X2)**2-FME2                                           FUNGG135
      IF(PXX.LT.0.) GO TO 1000                                          FUNGG136
      PXT   = SQRT(PXX)*PX1                                             FUNGG137
      PYT   = X2+X1*X2+PY1                                              FUNGG138
      IF(PXT*PYT.LT.0.) GO TO 1000                                      FUNGG139
  600 CONTINUE                                                          FUNGG140
C------------------------------                                         FUNGG141
      E3    = E-E*X1                                                    FUNGG142
      P3    = (E3-ME)*(E3+ME)                                           FUNGG143
      IF(P3.LT.0.) GO TO 1000                                           FUNGG144
      P3    = SQRT(P3)                                                  FUNGG145
      P3    = E3- ME2/(E3+P3)                                           FUNGG146
      CT3   = CT1                                                       FUNGG147
      ST3   = ST1                                                       FUNGG148
      CP3   = 1.                                                        FUNGG149
      SP3   = 0.                                                        FUNGG150
      E5    = E-E*X2                                                    FUNGG151
      P5    = (E5-ME)*(E5+ME)                                           FUNGG152
      IF(P5.LT.0.) GO TO 1000                                           FUNGG153
      P5    = SQRT(P5)                                                  FUNGG154
      P5    = E5- ME2/(E5+P5)                                           FUNGG155
      CT5   = -CT2                                                      FUNGG156
      ST5   = ST2                                                       FUNGG157
      CP5   = CPH                                                       FUNGG158
      SP5   = SIN(PH)                                                   FUNGG159
C-----------------------                                                FUNGG160
      EW    = 2.*E-E3-E5                                                FUNGG161
      W2    = 4.*E2*Z2                                                  FUNGG162
      PW    = EW*EW-W2                                                  FUNGG163
      IF(PW.LT.0.) GO TO 1000                                           FUNGG164
      PW    = SQRT(PW)                                                  FUNGG165
      PW    = EW-W2/(EW+PW)                                             FUNGG166
      W     = SQRT(W2)                                                  FUNGG167
      PWX   = -(P3*ST3*CP3+P5*ST5*CP5)                                  FUNGG168
      PWY   = -(P3*ST3*SP3+P5*ST5*SP5)                                  FUNGG169
      PWZ   = -(P3*CT3+P5*CT5)                                          FUNGG170
      CTW   = PWZ/PW                                                    FUNGG171
      IF(ABS(CTW).GT.alimit) GO TO 1000                                 FUNGG172
      STW   = SQRT((1.-CTW)*(1.+CTW))                                   FUNGG173
      PWXY  = PW*STW                                                    FUNGG174
      IF(PWXY.NE.0.) GO TO 700                                          FUNGG175
      CPW   = 1.D0                                                      FUNGG176
      SPW   = 0.D0                                                      FUNGG177
      GO TO 750                                                         FUNGG178
  700 CPW   = PWX/PWXY                                                  FUNGG179
      SPW   = PWY/PWXY                                                  FUNGG180
C-----------------------                                                FUNGG181
  750 Q12   = -2.*E*E3+2.*PB*P3*CT3                                     FUNGG182
      Q12   = Q12+2.*ME2                                                FUNGG183
      QQ1 = -Q12                                                        FUNGG184
      IF(Q12.GE.0.) GO TO 1000                                          FUNGG185
      Q22   = -2.*E*E5-2.*PB*P5*CT5                                     FUNGG186
      Q22   = Q22+2.*ME2                                                FUNGG187
      QQ2 = -1.0*sngl(Q22)                                              FUNGG188
      IF(Q22.GE.0.) GO TO 1000                                          FUNGG189
      Q12 = -0.25*Q12/E2                                                FUNGG190
      Q22 = -0.25*Q22/E2                                                FUNGG191
C====================================================================== FUNGG192
      XK = Z2 + Q12 + Q22                                               FUNGG193
      CH12 = XK*XK - 4.*Q12*Q22                                         FUNGG194
  101 CH = SQRT(CH12)                                                   FUNGG195
      XMEQ1 = ME2/(E2*Q12)                                              FUNGG196
      XMEQ2 = ME2/(E2*Q22)                                              FUNGG197
      C1 = 1. - XMEQ1                                                   FUNGG198
      C2 = 1. - XMEQ2                                                   FUNGG199
      FR1 = XK - 2.*(X2 + Q22)                                          FUNGG200
      FR2 = XK - 2.*(X1 + Q12)                                          FUNGG201
      FR11 = FR1*FR1                                                    FUNGG202
      FR22 = FR2*FR2                                                    FUNGG203
      BRA1 = FR11/CH12 + C1                                             FUNGG204
      BRA2 = FR22/CH12 + C2                                             FUNGG205
      COT1   = P3*DT1/Q12                                               FUNGG206
      COT2   = P5*DT2/Q22                                               FUNGG207
      X2MM  = 1.-X2                                                     FUNGG208
      FAC   = SQRT(FAC1/((X2MM-FME)*(X2MM+FME)))                        FUNGG209
      FAC   = X1MP+X2MM*FAC*CT                                          FUNGG210
      FACT  = 4.*ZS*RAMD/FAC                                            FUNGG211
      FL1 = CH*COT1*BRA1                                                FUNGG212
      FL2 = FACT*COT2*BRA2                                              FUNGG213
      SIG = FL1*FL2                                                     FUNGG214
C====================================================================== FUNGG215
      IF(SIG.LE.0.) GO TO 1000                                          FUNGG216
      DJDX  = DX1*DJ                                                    FUNGG217
       SPH = SIN( PH )                                                  FUNGG218
       CPH = COS( PH )                                                  FUNGG219
C                                                                       FUNGG220
       R3   = P3                                                        FUNGG221
       PX   = R3*ST3*CP3                                                FUNGG222
       PY   = R3*ST3*SP3                                                FUNGG223
       POSI( 1 ) = PX*CPH - PY*SPH                                      FUNGG224
       POSI( 2 ) = PX*SPH + PY*CPH                                      FUNGG225
       POSI( 3 ) = R3*CT3                                               FUNGG226
       POSI( 4 ) = E3                                                   FUNGG227
       POSI( 5 ) = ME                                                   FUNGG228
       R5   = P5                                                        FUNGG229
       PX   = R5*ST5*CP5                                                FUNGG230
       PY   = R5*ST5*SP5                                                FUNGG231
       ELEC( 1 ) = PX*CPH - PY*SPH                                      FUNGG232
       ELEC( 2 ) = PX*SPH + PY*CPH                                      FUNGG233
       ELEC( 3 ) = R5*CT5                                               FUNGG234
       ELEC( 4 ) = E5                                                   FUNGG235
       ELEC( 5 ) = ME                                                   FUNGG236
       FX = DJDX * CONST * PROB * SIG                                   FUNGG237
       RETURN                                                           FUNGG238
 1000 NERR  = 1                                                         FUNGG239
      FX = 0.0                                                          FUNGG240
      RETURN                                                            FUNGG241
      END                                                               FUNGG242
      SUBROUTINE MAPVAR(T,X,XMIN,XMAX,DX)                               MAPVAR 2
C---------------------------------------------------------------        MAPVAR 3
C! MAP routine for integrating an exponential (PHOJ01)                  MAPVAR 4
C                                                                       MAPVAR 5
C INPUT  parameters : X,XMIN,XMAX                                       MAPVAR 6
C                                                                       MAPVAR 7
C OUTPUT parameters : T,DX                                              MAPVAR 8
C                                                                       MAPVAR 9
C CALLED BY - various                                                   MAPVAR10
C  A.J.Finch February 1996 adapted to PHOJ01 from PHOT02 version        MAPVAR11
C----------------------------------------------------------------       MAPVAR12
      ENTRY      MAPX1 (T,X,XMIN,XMAX,DX)                               MAPVAR13
      ENTRY      MAPS2 (T,X,XMIN,XMAX,DX)                               MAPVAR14
      ENTRY      MAPW2 (T,X,XMIN,XMAX,DX)                               MAPVAR15
      IMPLICIT DOUBLE PRECISION (A-Z)                                   MAPVAR16
      Y     = XMAX/XMIN                                                 MAPVAR17
      T     = XMIN*Y**X                                                 MAPVAR18
      DX    = T*DLOG(Y)                                                 MAPVAR19
      RETURN                                                            MAPVAR20
      END                                                               MAPVAR21
      REAL FUNCTION FGVDM(QQ)                                           FGVDM  2
C---------------------------------------------------------------        FGVDM  3
C! Return the GVDM form factor (PHOT02)                                 FGVDM  4
C!                                                                      FGVDM  5
C! INPUT  parameters :                                                  FGVDM  6
C!                      QQ : Q squared of photon                        FGVDM  7
C! OUTPUT parameters :                                                  FGVDM  8
C!                   FGVDM : Value of form factor                       FGVDM  9
C---------------------------------------------------------------        FGVDM 10
       LOGICAL FIRST,LKEEP                                              FGVDM 11
       SAVE    FIRST                                                    FGVDM 12
C                                                                       FGVDM 13
C---  CALCULATE THE GVDM-FF ACCORDING TO GINZBURG & SERBO,              FGVDM 14
C     PHYS. LETT. 109B(1982), NO.3, P.231FF, EQ.(2)                     FGVDM 15
C     H.WRIEDT    25.11.82    11:45                                     FGVDM 16
C     LAST MOD    25.11.82    11:45                                     FGVDM 17
C                                                                       FGVDM 18
      DIMENSION R(3),XMQ(3)                                             FGVDM 19
      DATA R/0.65,0.08,0.05/,XMQ/0.591,0.612,1.04/                      FGVDM 20
      DATA    FIRST/.TRUE./                                             FGVDM 21
C                                                                       FGVDM 22
      FGVDM = 1.                                                        FGVDM 23
      IF (QQ.EQ.0.) RETURN                                              FGVDM 24
      SUM = 0.                                                          FGVDM 25
        DO 10 I = 1,3                                                   FGVDM 26
        F = QQ/XMQ(I)                                                   FGVDM 27
        T = R(I)*(1.+0.25*F)/((1.+F)*(1.+F))                            FGVDM 28
   10   SUM = SUM + T                                                   FGVDM 29
      T0 = 0.22/(1.+QQ/1.96)                                            FGVDM 30
      FGVDM = SUM + T0                                                  FGVDM 31
      RETURN                                                            FGVDM 32
      END                                                               FGVDM 33
      SUBROUTINE STDSEL(KEEP)                                           STDSEL 2
c                                                                       STDSEL 3
c event selection routine... uses lund common to select event           STDSEL 4
c                                                                       STDSEL 5
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      LOGICAL KEEP,FIRST                                                STDSEL 7
      save FIRST                                                        STDSEL 8
      PARAMETER (LJNPAR=4000)                                           STDSEL 9
      INTEGER XD0,XPI,XPI0,XKAON                                        STDSEL10
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5) STDSEL11
      DATA FIRST/.TRUE./                                                STDSEL12
      DATA IREAD/0/,IKEPT/0/,IREJECT/0/,PSTD/0.2/,CSTD/0.95/,NSTD/1/    STDSEL13
      IF(FIRST)THEN                                                     STDSEL14
        ISTD = 0                                                        STDSEL15
        JGSTD = NLINK('GSTD',0)                                         STDSEL16
        IF(JGSTD.NE.0)THEN                                              STDSEL17
           ISTD = IW(JGSTD+1)                                           STDSEL18
           NSTD = IW(JGSTD+3)                                           STDSEL19
           PSTD = RW(JGSTD+4)                                           STDSEL20
           CSTD = RW(JGSTD+5)                                           STDSEL21
        ENDIF                                                           STDSEL22
        WRITE(6,*)' ***********************************************'    STDSEL23
        WRITE(6,*)'     '                                               STDSEL24
        WRITE(6,*)' STDSEL - PRODUCTION VERSION OF AMCSEL '             STDSEL25
        WRITE(6,*)' REQUIRE AT LEAST',NSTD,' CHARGED PARTICLE IN ALEPH' STDSEL26
        WRITE(6,*)' with momentum >',pstd,' and cost < ',cstd           STDSEL27
        WRITE(6,*)'     '                                               STDSEL28
        WRITE(6,*)' ***********************************************'    STDSEL29
        FIRST = .FALSE.                                                 STDSEL30
      ENDIF                                                             STDSEL31
      KEEP = .FALSE.                                                    STDSEL32
      IREAD = IREAD  + 1                                                STDSEL33
C                                                                       STDSEL34
C LOOP OVER THE LUND PARICLES                                           STDSEL35
C                                                                       STDSEL36
10    NCHRG = 0                                                         STDSEL37
      ECHRG = 0.0                                                       STDSEL38
      DO  I = 1,N7LU                                                    STDSEL39
       IF(K7LU(I,1).EQ.1)THEN                                           STDSEL40
           PTOT = SQRT(P7LU(I,1)**2 +                                   STDSEL41
     1                 P7LU(I,2)**2 +                                   STDSEL42
     1                 P7LU(I,3)**2)                                    STDSEL43
           PTS  = P7LU(I,1)**2 + P7LU(I,2)**2                           STDSEL44
           PT = SQRT(PTS)                                               STDSEL45
           CT = P7LU(I,3)/PTOT                                          STDSEL46
           CHRGE = FLOAT(LUCHGE(K7LU(I,2)))                             STDSEL47
           IF(PTOT.GT.PSTD.AND.                                         STDSEL48
     1       (CHRGE.NE.0).AND.                                          STDSEL49
     1     (ABS(CT).LT.CSTD))THEN                                       STDSEL50
                 NCHRG = NCHRG + 1                                      STDSEL51
                 ECHRG = ECHRG + P7LU(I,4)                              STDSEL52
           ENDIF                                                        STDSEL53
       ENDIF                                                            STDSEL54
      ENDDO                                                             STDSEL55
      KEEP = .FALSE.                                                    STDSEL56
C      IF(ECHRG.GT.2.0.AND.NCHRG.GT.1)KEEP = .TRUE.                     STDSEL57
      IF(NCHRG.GE.NSTD) KEEP = .TRUE.                                   STDSEL58
      IF(KEEP)THEN                                                      STDSEL59
        IKEPT = IKEPT + 1                                               STDSEL60
      ELSE                                                              STDSEL61
        IREJECT = IREJECT + 1                                           STDSEL62
      ENDIF                                                             STDSEL63
      RETURN                                                            STDSEL64
      ENTRY STDSTA                                                      STDSEL65
      WRITE(6,*)' =============================================='       STDSEL66
      WRITE(6,*)' REQUIRE AT LEAST ',NSTD,' CHARGED PARTICLE IN ALEPH'  STDSEL67
      WRITE(6,*)' with momentum >',pstd,' and cost < ',cstd             STDSEL68
      WRITE(6,*)' STDSEL STATISTICS '                                   STDSEL69
      WRITE(6,*)' =============================================='       STDSEL70
      WRITE(6,*)' '                                                     STDSEL71
      WRITE(6,*)' ',IREAD,' EVENTS ENTERED STDSEL '                     STDSEL72
      WRITE(6,*)' ',IKEPT,' WERE KEPT '                                 STDSEL73
      WRITE(6,*)' ',IREJECT,' WERE REJECTED '                           STDSEL74
      WRITE(6,*)' '                                                     STDSEL75
      WRITE(6,*)' '                                                     STDSEL76
      END                                                               STDSEL77
