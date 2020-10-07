      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)                 2
C --------------------------------------------------------------------  3
C!                                                                      4
C!   Author   :- Jim Wear              23-FEB-1993                      5
C! This is the interface to COSM01 converted to BMPHOT by J.Wear        6
C!  from B. Bloch-Devaux March 1988                                     7
C!                                                                      8
C!   Inputs:                                                            9
C!        -                                                             
C!                                                                      
C!   Outputs:                                                           
C!        -                                                             
C!                                                                      
C!   Libraries required:                                                
C!                                                                      
C!   Description                                                        
C!   ===========                                                        
C!  Generate one photon and propagate almost to beampipe                
C!                                                                      
C?                                                                      
C!======================================================================
C*IF .NOT.DOC                                                           
      PARAMETER (LBCS=1000,LCHAR=4,LMHLEN=2)                            2
      COMMON/BCS/ IW(LBCS)                                              3
      REAL RW(LBCS)                                                     4
      EQUIVALENCE (RW(1),IW(1))                                         5
C                                                                       6
      COMMON /BPHPAR/ EMIN,EMAX,ZMIN,ZMAX,THMIN,THMAX,PHIMIN,PHIMAX,    2
     &  TBEAM,IGEN,IFWBW,ANGMIX                                         3
      COMMON /BPHSTT/ NEVENT(5)                                         4
      COMMON /BPHIO/ IOU,IDB                                            5
      COMMON /BPHCON/ PI,PIBY2,TWOPI,KEVGEV                             2
      COMMON/SMBCOM/NSMPI,NSMIN                                         3
      DIMENSION VRTEX(4),EPHOT(4)                                       
      INTEGER ALTABL                                                    
      EXTERNAL ALTABL                                                   
      DATA IFI /0/ ,RBP /5.3/                                           
C DL0 is the extrapolation distance -- all distance in centimeters.     
C      DATA DL0 /0.1/                                                   
      DATA DL0 /274.0/                                                  
C-------------------------------------------------------------          
      IF ( IFI.EQ.0 ) THEN                                              
         NAPAR=NAMIND('PART')                                           
         ID=IW(NAPAR)                                                   
         NR=1                                                           
         APHOT=RW(ID+LMHLEN+(NR-1)*IW(ID+1)+6)                          
         IDB1=1                                                         
         IDB2=10                                                        
         JDEBU=IW(NAMIND('DEBU'))                                       
         IF (JDEBU.NE.0) THEN                                           
            IDB1=IW(JDEBU+1)                                            
            IDB2=IW(JDEBU+2)                                            
         ENDIF                                                          
         IFI=IFI+1                                                      
      ENDIF                                                             
      IDB=0                                                             
      IF (NEVENT(2).GE.IDB1-1 .AND. NEVENT(2).LE.IDB2) IDB =1           
 1    CALL BPHGEN ( EPHOT, FPIPE, ZPIPE, TPHOT, TZPIPE)                 
      IDPR = 0                                                          
      NTRK = 1                                                          
      NVRT = 1                                                          
      ISTA = 0                                                          
      ECMS = EPHOT(4)                                                   
      EPHOT(4) = 0.                                                     
      JT   = 1                                                          
      WEIT = 1.                                                         
C                                                                       
C  Now fill 'KINE' and 'VERT' banks                                     
C                                                                       
C                                                                       
C   Generate the origin of the photon. (just inside the beampipe)       
C   For now some special case assumptions                               
C     i) no azimuthal component of photon 4 momentum                    
C    ii) EPHOT is (px,py,pz,E) of photon in ALEPH coordinate system     
C                                                                       
C      ERHO = SQRT(EPHOT(1)*EPHOT(1) + EPHOT(2)*EPHOT(2))               
C      EZ   = EPHOT(3)                                                  
C      FPHOT = ATAN2(EPHOT(2),EPHOT(1))      ! phi range of 0 to 2*pi   
C      TPHOT = PI/2.                                                    
C      IF (ABS(EZ/EPHOT(4)).GT.0.00002)                                 
C     &  TPHOT = ATAN2(ERHO,EZ)  ! theta range of -pi to +pi            
C                                                                       
C   Extrapolate backwards from beampipe interaction point by DL         
C     If DL0 is large, photons at shallow angles MAY strike beampipe fla
C     at +- 38 cm before hitting beampipe.                              
C     Beampipe inner radius is 5.3 cm out to z=274 cm from BPG1         
C     may be necessary to get further out in later version              
C                                                                       
      DL = DL0                                                          
C                                                                       
C   If DL0 and incident angle are both large, restrict photon vertex to 
C     cylinder w/ radius 0.1 cm less than beampipe radius by reducing th
C     extrapolating distance, DL.                                       
C                                                                       
C Generator 8 is 8 keV copper photons generated at TPC inner wall       
C  Therefore move radius to 31.7 cm                                     
      IF (IGEN.EQ.8) THEN                                               
        RBP = 31.8                                                      
        DL = 0.1                                                        
        DRHO = ABS(DL*SIN(TPHOT))                                       
        GOTO 11                                                         
      ENDIF                                                             
      DRHO = ABS(DL*SIN(TPHOT))            ! DRHO always positive (into 
      IF (DRHO.GE.(2.*RBP - 0.1)) THEN                                  
        DL = (2.*RBP - 0.1)/SIN(TPHOT)                                  
        DRHO = ABS(DL*SIN(TPHOT))                                       
      ENDIF                                                             
 11   DZ   = ABS(DL*COS(TPHOT))                                         
      DZ   = SIGN(DZ,TPHOT)                ! DZ signed by the incident a
      RH0  = RBP - DRHO                                                 
      Z0   = ZPIPE - DZ                                                 
      PHI0 = FPIPE                  ! SPECIAL CASE - no azi. component  
c t.o.f. from photon starting pt.  in seconds                           
      TOF0 = (TZPIPE - ABS(DZ)/0.29954/100.)*1.E-9                      
C                                                                       
      CALL HFILL(10010,ZPIPE,0.,1.)                                     
      CALL HFILL(10011,Z0,0.,1.)                                        
      CALL HFILL(10015,TZPIPE,0.,1.)                                    
      CALL HFILL(10016,TOF0,0.,1.)                                      ASKUS112
      CALL HFILL(10020,FPIPE,0.,1.)                                     ASKUS113
      CALL HFILL(10021,PHI0,0.,1.)                                      ASKUS114
      CALL HFILL(10030,TPHOT,0.,1.)                                     ASKUS115
      CALL HFILL(10031,ABS(TPHOT),0.,1.)                                ASKUS116
      CALL HFILL(10032,TPHOT,0.,1.)                                     ASKUS117
      CALL HFILL(10033,ABS(TPHOT),0.,1.)                                ASKUS118
      CALL HFILL(10040,ECMS*KEVGEV,0.,1.)                               ASKUS119
      CALL HFILL(10100,ZPIPE,FPIPE,1.)                                  ASKUS120
      CALL HFILL(10101,ZPIPE,TPHOT,1.)                                  ASKUS121
C                                                                       ASKUS122
C convert vertex to (x,y,z) and fill VERT bank                          ASKUS123
      VRTEX(1) = RH0*COS(FPIPE)                                         ASKUS124
      VRTEX(2) = RH0*SIN(FPIPE)                                         ASKUS125
      VRTEX(3) = Z0                                                     ASKUS126
      VRTEX(4) = TOF0                      ! time of flight             ASKUS127
      JVERT = KBVERT(NVRT,VRTEX,0)                                      ASKUS128
      IF(JVERT.EQ.0) THEN                                               ASKUS129
        ISTA = 3                                                        ASKUS130
        NEVENT(5) = NEVENT(5) + 1                                       ASKUS131
        GO TO 99                                                        ASKUS132
      ENDIF                                                             ASKUS133
C                                                                       ASKUS134
C fill KINE bank                                                        ASKUS135
      JKINE = KBKINE(NTRK,EPHOT,JT,NVRT)                                ASKUS136
      IF(JKINE.EQ.0) THEN                                               ASKUS137
        ISTA = 3                                                        ASKUS138
        NEVENT(5) = NEVENT(5) + 1                                       ASKUS139
        GO TO 99                                                        ASKUS140
      ENDIF                                                             ASKUS141
C Fill KHIS bank                                                        ASKUS142
      IHIS = 0                                                          ASKUS143
      JKHIS = ALTABL('KHIS',1,1,IHIS,'I','E')                           ASKUS144
      IF(JKHIS.EQ.0) THEN                                               ASKUS145
        ISTA = 3                                                        ASKUS146
        NEVENT(5) = NEVENT(5) + 1                                       ASKUS147
        GO TO 99                                                        ASKUS148
      ENDIF                                                             ASKUS149
 99   RETURN                                                            ASKUS150
      END                                                               ASKUS151
      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C --------------------------------------------------------------------  ASKUSI 3
C! Init generator parameters                                            ASKUSI 4
C --------------------------------------------------------------------  ASKUSI 5
      PARAMETER (LBCS=1000,LCHAR=4,LMHLEN=2)                            BCS    2
      COMMON/BCS/ IW(LBCS)                                              BCS    3
      REAL RW(LBCS)                                                     BCS    4
      EQUIVALENCE (RW(1),IW(1))                                         BCS    5
C                                                                       BCS    6
      COMMON /BPHPAR/ EMIN,EMAX,ZMIN,ZMAX,THMIN,THMAX,PHIMIN,PHIMAX,    BPHCOM 2
     &  TBEAM,IGEN,IFWBW,ANGMIX                                         BPHCOM 3
      COMMON /BPHSTT/ NEVENT(5)                                         BPHCOM 4
      COMMON /BPHIO/ IOU,IDB                                            BPHCOM 5
      COMMON /BPHCON/ PI,PIBY2,TWOPI,KEVGEV                             BPHCON 2
      COMMON/SMBCOM/NSMPI,NSMIN                                         BPHCON 3
      DIMENSION TABL(13)                                                ASKUSI 9
      INTEGER ALTABL                                                    ASKUSI10
      EXTERNAL ALTABL                                                   ASKUSI11
      PARAMETER ( IGCO = 8003)  ! define BMPHOT as generator 8003       ASKUSI12
C                                                                       ASKUSI13
C   Return the generator code as defined in the KINGAL library          ASKUSI14
C                                                                       ASKUSI15
      IGCOD = IGCO                                                      ASKUSI16
c   Constants                                                           ASKUSI17
      PI = ATAN(1.)*4.                                                  ASKUSI18
      PIBY2 = PI/2.                                                     ASKUSI19
      TWOPI = 2.*PI                                                     ASKUSI20
C   Default values                                                      ASKUSI21
      IOU = IW(6)                                                       ASKUSI22
      EMIN   = 0.000010         ! Energy of incident photons (10 keV minASKUSI23
      EMAX   = 0.010            !                            (10 MeV maxASKUSI24
      ZMIN   = -274.            ! Z   of photon impact on beampipe (cm) ASKUSI25
      ZMAX   =  274.                                                    ASKUSI26
      PHIMIN =  0.              ! PHI of photon impact on beampipe (radiASKUSI27
      PHIMAX =  2*PI                                                    ASKUSI28
      THMIN  = 0.001            ! ABS(THETA) of photon w.r.t. ALEPH +z (ASKUSI29
      THMAX  =  PI/2.                                                   ASKUSI30
c  THETA w.r.t. which beam +1 => e- beam , -1=> e+ beam ,0=> either     ASKUSI31
c  if TBEAM < 0, then  THETA => PI - THETA                              ASKUSI32
      TBEAM  = 0.               ! photon impacts from either direction  ASKUSI33
      IGEN = 0                                                          ASKUSI34
      IFWBW = 2                                                         ASKUSI35
c                                                                       ASKUSI36
c Store the number of generated events in the KPAR bank word 1          ASKUSI37
c                                                                       ASKUSI38
      JNEVT=IW(NAMIND('NEVT'))                                          ASKUSI39
      IF (JNEVT.NE.0) NEVT=IW(JNEVT+1)                                  ASKUSI40
      TABL(1) = FLOAT(NEVT)                                             ASKUSI41
c                                                                       ASKUSI42
C                                                                       ASKUSI43
C   READ here input parameters                                          ASKUSI44
C  Energy spectrum generator input specified by card EGEN               ASKUSI45
C  The default values can be changed by the data cards EBPH and KIBP    ASKUSI46
C                                                                       ASKUSI47
C  Data card EGEN --- Energy spectrum GENerator                         ASKUSI48
C    Choice of generator types are in word 1:                           ASKUSI49
C     0:  'FLAT' , 1:  '31 mm' , 2:  '40 mm'                            ASKUSI50
C    NOTE: At moment, they also UNconstrain the photon energy from EBPH ASKUSI51
C    Choice of forward or backward scattering in word 2                 ASKUSI52
C     0:  mixture (not yet), 1:  forward at 10 mrad (8.5 m coll)        ASKUSI53
C     2:  backward at 2 mrad (57 m vacuum chamber) 3:  backward at 10 mrASKUSI54
C    NOTE: This overrides the theta range except in flat spectrum case  ASKUSI55
C                                                                       ASKUSI56
      JEGEN = NLINK('EGEN',0)                                           ASKUSI57
      IF(JEGEN.NE.0) THEN                                               ASKUSI58
       IGEN  = NINT(RW(JEGEN+1))                                        ASKUSI59
       IFWBW = NINT(RW(JEGEN+2))                                        ASKUSI60
       ANGMIX = RW(JEGEN+3)                                             ASKUSI61
      ENDIF                                                             ASKUSI62
      TABL(2) = FLOAT(IGEN)                                             ASKUSI63
      TABL(3) = FLOAT(IFWBW)                                            ASKUSI64
      TABL(4) = ANGMIX                                                  ASKUSI65
C                                                                       ASKUSI66
C  Data card EBPH --- Energy of Beam PHoton                             ASKUSI67
C    Energy range of beam photons from generator                        ASKUSI68
C                                                                       ASKUSI69
      JEBPH = NLINK('EBPH',0)                                           ASKUSI70
      IF(JEBPH.NE.0) THEN                                               ASKUSI71
       EMIN = RW(JEBPH+1)                                               ASKUSI72
       EMAX = RW(JEBPH+2)                                               ASKUSI73
      ENDIF                                                             ASKUSI74
      TABL(5) = EMIN                                                    ASKUSI75
      TABL(6) = EMAX                                                    ASKUSI76
C                                                                       ASKUSI77
C  Data card KIBP --- Kinematics of Impact on BeamPipe                  ASKUSI78
C    Beampipe impact point and impact angle range  (NO AZIMUTHAL COMPONEASKUSI79
C                                                                       ASKUSI80
      JKIBP = NLINK('KIBP',0)                                           ASKUSI81
      IF(JKIBP.NE.0) THEN                                               ASKUSI82
       ZMIN   = RW(JKIBP+1)                                             ASKUSI83
       ZMAX   = RW(JKIBP+2)                                             ASKUSI84
       PHIMIN = RW(JKIBP+3)                                             ASKUSI85
       PHIMAX = RW(JKIBP+4)                                             ASKUSI86
       THMIN  = ABS(RW(JKIBP+5))                                        ASKUSI87
       THMAX  = ABS(RW(JKIBP+6))                                        ASKUSI88
       TBEAM  = RW(JKIBP+7)                                             ASKUSI89
      ENDIF                                                             ASKUSI90
      TABL(7) = ZMIN                                                    ASKUSI91
      TABL(8) = ZMAX                                                    ASKUSI92
      TABL(9) = PHIMIN                                                  ASKUSI93
      TABL(10)= PHIMAX                                                  ASKUSI94
      TABL(11)= THMIN                                                   ASKUSI95
      TABL(12)= THMAX                                                   ASKUSI96
      TABL(13)= TBEAM                                                   ASKUSI97
C                                                                       ASKUSI98
C  Fill the KPAR bank with the generator parameters                     ASKUSI99
C                                                                       ASKUS100
      NCOL = 13                                                         ASKUS101
      NROW = 1                                                          ASKUS102
      JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')                ASKUS103
C                                                                       ASKUS104
C  Initialize event counters                                            ASKUS105
C                                                                       ASKUS106
       DO 10 I=1,5                                                      ASKUS107
         NEVENT(I)=0                                                    ASKUS108
 10    CONTINUE                                                         ASKUS109
C                                                                       ASKUS110
C  Initialize event generator                                           ASKUS111
C                                                                       ASKUS112
       CALL BMPHIN                                                      ASKUS113
C                                                                       ASKUS114
C   Print input photon spectrum                                         ASKUS115
C                                                                       ASKUS116
cc      CALL BMPHPR                                                     ASKUS117
C                                                                       ASKUS118
C                                                                       ASKUS119
C  Book histograms to control generator performance                     ASKUS120
      CALL HBOOK1(10010,'Z of impact point',60,-300.,300.,0.)           ASKUS121
      CALL HBOOK1(10011,'Z of initial point',60,-600.,600.,0.)          ASKUS122
      CALL HBOOK1(10015,'Time of flight at impact point'                ASKUS123
     &  ,50,-20.,480.,0.)                                               ASKUS124
      CALL HBOOK1(10016,'Time of flight at initial point'               ASKUS125
     &  ,50,-20.,480.,0.)                                               ASKUS126
      CALL HBOOK1(10020,'[f] of impact point',63,0.,6.3,0.)             ASKUS127
      CALL HBOOK1(10021,'[f] of initial point',     63,0.,6.3,0.)       ASKUS128
      CALL HBOOK1(10030,'[dQ] of photon at impact', 32,-1.6,1.6,0.)     ASKUS129
      CALL HBOOK1(10031,'Abs([dQ]) of photon at impact',32,0.,1.6,0.)   ASKUS130
      CALL HBOOK1(10032,'[dQ] of photon at impact', 40,-0.1,0.1,0.)     ASKUS131
      CALL HBOOK1(10033,'Abs([dQ]) of photon at impact',40,0.,0.1,0.)   ASKUS132
      CALL HBOOK1(10040,'Generated Photon Energy (keV)',50,0.,500.,0.)  ASKUS133
      CALL HBOOK2(10100,'Z vs. [f] of impact point',30,-300.,300.,      ASKUS134
     &       30,0.,6.3,0.)                                              ASKUS135
      CALL HBOOK2(10101,'Z vs. [dQ] of impact point',30,-300.,300.,     ASKUS136
     &       40,-0.1,0.1,0.)                                            ASKUS137
      RETURN                                                            ASKUS138
      END                                                               ASKUS139
C*EI                                                                    ASKUS140
      SUBROUTINE USCEVE                                                 USCEVE 2
C ----------------------------------------------------------------------USCEVE 3
C D. Rousseau                                                           USCEVE 4
C Trigger event saving only if interesting banks are                    USCEVE 5
C there                                                                 USCEVE 6
C -----------------------------------------------------                 USCEVE 7
      SAVE                                                              USCEVE 8
      PARAMETER (LBCS=1000,LCHAR=4,LMHLEN=2)                            BCS    2
      COMMON/BCS/ IW(LBCS)                                              BCS    3
      REAL RW(LBCS)                                                     BCS    4
      EQUIVALENCE (RW(1),IW(1))                                         BCS    5
C                                                                       BCS    6
      LOGICAL FIRST,xkeepv,xkeepi,xkeept                                USCEVE10
      DATA FIRST,xkeepv,xkeepi,xkeept/.TRUE.,.TRUE.,.TRUE.,.TRUE./      USCEVE11
C ----------------------------------------------------------------------USCEVE12
C                                                                       USCEVE13
      IF (FIRST) THEN                                                   USCEVE14
        NVDHT=NAMIND('VDHT')                                            USCEVE15
        NVHLS=NAMIND('VHLS')                                            USCEVE16
        NVPLH=NAMIND('VPLH')                                            USCEVE17
        NVDTD=NAMIND('VDTD')                                            USCEVE18
        NIHIT=NAMIND('IHIT')                                            USCEVE19
        NIDIG=NAMIND('IDIG')                                            USCEVE20
        NIDHR=NAMIND('IDHR')                                            USCEVE21
        NITTR=NAMIND('ITTR')                                            USCEVE22
        NIXPP=NAMIND('IXPP')                                            USCEVE23
        NTPAD=NAMIND('TPAD')                                            USCEVE24
        NTPDI=NAMIND('TPDI')                                            USCEVE25
        NTSDI=NAMIND('TSDI')                                            USCEVE26
        NTSLE=NAMIND('TSLE')                                            USCEVE27
        FIRST = .FALSE.                                                 USCEVE28
C save status of MSAVJO                                                 USCEVE29
        MSAVJOMY=MSAVJO                                                 USCEVE30
      ENDIF                                                             USCEVE31
Cerase uninteresting bank from E list                                   USCEVE32
      CALL BLIST(IW,'E-','ETTREWTREWHELTTRSIX2')                        USCEVE33
Cadd kine bank                                                          USCEVE34
C      CALL BLIST(IW,'E+','FKINFVERFPOI')                               USCEVE35
C                                                                       USCEVE36
      MSAVJO=MSAVJOMY                                                   USCEVE37
      IF (MSAVJOMY.EQ.0) RETURN                                         USCEVE38
c writing out events                                                    USCEVE39
      XKEEPV = .FALSE.                                                  USCEVE40
      XKEEPI = .FALSE.                                                  USCEVE41
      XKEEPT = .FALSE.                                                  USCEVE42
      IF (IW(NVDHT).NE.0) XKEEPV = .TRUE.                               USCEVE43
      IF (IW(NVHLS).NE.0) XKEEPV = .TRUE.                               USCEVE44
      IF (IW(NVPLH).NE.0) XKEEPV = .TRUE.                               USCEVE45
      IF (IW(NVDTD).NE.0) XKEEPV = .TRUE.                               USCEVE46
      IF (IW(NIHIT).NE.0) XKEEPI = .TRUE.                               USCEVE47
      IF (IW(NIDIG).NE.0) XKEEPI = .TRUE.                               USCEVE48
      IF (IW(NIDHR).NE.0) XKEEPI = .TRUE.                               USCEVE49
      IF (IW(NITTR).NE.0) XKEEPI = .TRUE.                               USCEVE50
      IF (IW(NIXPP).NE.0) XKEEPI = .TRUE.                               USCEVE51
      IF (IW(NTPAD).NE.0) XKEEPT = .TRUE.                               USCEVE52
      IF (IW(NTPDI).NE.0) XKEEPT = .TRUE.                               USCEVE53
      IF (IW(NTSDI).NE.0) XKEEPT = .TRUE.                               USCEVE54
      IF (IW(NTSLE).NE.0) XKEEPT = .TRUE.                               USCEVE55
      IF (.NOT.(XKEEPV.OR.XKEEPI.OR.XKEEPT)) THEN                       USCEVE56
         MSAVJO=0                                                       USCEVE57
      ENDIF                                                             USCEVE58
C                                                                       USCEVE59
      RETURN                                                            USCEVE60
      END                                                               USCEVE61
      SUBROUTINE BMPHIN                                                 BMPHIN 2
C---------------------------------------------------------------------- BMPHIN 3
C!  -                                                                   BMPHIN 4
C!                                                                      BMPHIN 5
C!   Author   :- Jim Wear              24-FEB-1993                      BMPHIN 6
C!                                                                      BMPHIN 7
C!   Inputs: Contents of common block BPHPAR                            BMPHIN 8
C!        -                                                             BMPHIN 9
C!                                                                      BMPHIN10
C!   Outputs: Contents of common block BPHPAR                           BMPHIN11
C!        -                                                             BMPHIN12
C!                                                                      BMPHIN13
C!   Libraries required:                                                BMPHIN14
C!                                                                      BMPHIN15
C!   Description  Initialize event generator:                           BMPHIN16
C!   ===========  i) constrain user set limits by physical limits       BMPHIN17
C!                                                                      BMPHIN18
C?                                                                      BMPHIN19
C!======================================================================BMPHIN20
      COMMON /BPHPAR/ EMIN,EMAX,ZMIN,ZMAX,THMIN,THMAX,PHIMIN,PHIMAX,    BPHCOM 2
     &  TBEAM,IGEN,IFWBW,ANGMIX                                         BPHCOM 3
      COMMON /BPHSTT/ NEVENT(5)                                         BPHCOM 4
      COMMON /BPHIO/ IOU,IDB                                            BPHCOM 5
      COMMON /BPHCON/ PI,PIBY2,TWOPI,KEVGEV                             BPHCON 2
      COMMON/SMBCOM/NSMPI,NSMIN                                         BPHCON 3
      DATA ZMXAB/274./,EMINAB/0.000001/,EMAXAB/0.001/,THMNAB/0.001/     BMPHIN24
      DIMENSION CDIR(3),CSPECTRA(6),CFWBW(3),IGLIST(6)                  BMPHIN25
      CHARACTER*3 CDIR/' -z','any',' +z'/                               BMPHIN26
      CHARACTER*60 CSPECTRA/                                            BMPHIN27
     &  ' Uniform energy distribution',                                 BMPHIN28
     &  ' LEP I physics -- 35 nm emittance',                            BMPHIN29
     &  ' LEP I physics -- 40 nm emittance',                            BMPHIN30
     &  ' LEP II physics -- 50 nm emittance',                           BMPHIN31
     &  ' LEP I Nov 92 -- Fill 1460 with 8.5 m collimator at 31 mm',    BMPHIN32
     &  ' LEP I Nov 92 -- Fill 1460 with 8.5 m collimator at 40 mm'/    BMPHIN33
      DATA IGLIST/0,11,12,21,31,32/                                     BMPHIN34
      CHARACTER*40 CFWBW/' Forward- and Back-scattering mixture ',      BMPHIN35
     &  ' Forward scattering only',' Backward scattering only'/         BMPHIN36
C                                                                       BMPHIN37
      INIT = 0                                                          BMPHIN38
      WRITE(IOU,*) '   Initializing BMPHOT Generator using'             BMPHIN39
      IF (IGEN.GE.10.AND.IGEN.LE.29) THEN                               BMPHIN40
        CALL BKGSPEC(IFWBW,IGEN,ANGMIX,ENERGY,TPHOT,TSCAT,IBCK)         BMPHIN41
       ELSEIF (IGEN.GE.30) THEN                                         BMPHIN42
        CALL BKGSPCO(INIT,IGEN,ETOT)                                    BMPHIN43
       ELSE                                                             BMPHIN44
C        ! Flat spectrum -- use this routine for initialization         BMPHIN45
      ENDIF                                                             BMPHIN46
      do i=1,6                                                          BMPHIN47
        if (iglist(i).eq.igen) ipoint = i                               BMPHIN48
      enddo                                                             BMPHIN49
      WRITE(IOU,*) CSPECTRA(IPOINT)                                     BMPHIN50
      WRITE(IOU,*) CFWBW(IFWBW+1)                                       BMPHIN51
C                                                                       BMPHIN52
C Verify input energy cuts                                              BMPHIN53
      IF (EMIN.GT.EMAX) THEN                                            BMPHIN54
        WRITE(IOU,*)                                                    BMPHIN55
        WRITE(IOU,*)                                                    BMPHIN56
     &    '   !!! Min. energy greater than Max. energy !!!'             BMPHIN57
        WRITE(IOU,*)                                                    BMPHIN58
     &    '       Check data card input of EBPH and try again'          BMPHIN59
        STOP                                                            BMPHIN60
      ENDIF                                                             BMPHIN61
      IF (EMIN.LT.EMINAB) THEN                                          BMPHIN62
        WRITE(IOU,*)                                                    BMPHIN63
        WRITE(IOU,*)                                                    BMPHIN64
     &    '   !!! Min. energy too low. Set to ',EMINAB,' GeV  !!!'      BMPHIN65
        EMIN = EMINAB                                                   BMPHIN66
      ENDIF                                                             BMPHIN67
      IF (EMAX.GT.EMAXAB) THEN                                          BMPHIN68
        WRITE(IOU,*)                                                    BMPHIN69
        WRITE(IOU,*)                                                    BMPHIN70
     &    '   !!! Max. energy too high. Set to ',EMAXAB,' GeV  !!!'     BMPHIN71
        EMAX = EMAXAB                                                   BMPHIN72
      ENDIF                                                             BMPHIN73
      IF (IGEN.EQ.0)                                                    BMPHIN74
     & WRITE(IOU,*) '      USED  Min, Max photon energy:' , EMIN,EMAX   BMPHIN75
C                                                                       BMPHIN76
C Verify values of z                                                    BMPHIN77
      IF (ZMIN.GT.ZMAX) THEN                                            BMPHIN78
        WRITE(IOU,*)                                                    BMPHIN79
        WRITE(IOU,*)                                                    BMPHIN80
     &    '   !!! Z - range of photon impact improperly specified !!!'  BMPHIN81
        WRITE(IOU,*)                                                    BMPHIN82
     &    '       Check data card input of KIBP and try again'          BMPHIN83
        STOP                                                            BMPHIN84
      ENDIF                                                             BMPHIN85
      IF (ZMAX.GT.ZMXAB) THEN                                           BMPHIN86
        WRITE(IOU,*)                                                    BMPHIN87
        WRITE(IOU,*) '   !!! Max. Z impact on beampipe out of range.',  BMPHIN88
     &    '  Set to ',ZMXAB,' cm  !!!'                                  BMPHIN89
        ZMAX = ZMXAB                                                    BMPHIN90
      ENDIF                                                             BMPHIN91
      IF (ZMIN.LT.-1*ZMXAB) THEN                                        BMPHIN92
        WRITE(IOU,*)                                                    BMPHIN93
        WRITE(IOU,*) '   !!! Min. Z impact on beampipe out of range.',  BMPHIN94
     &    '  Set to ',-1*ZMXAB,' cm  !!!'                               BMPHIN95
        ZMIN = -1*ZMXAB                                                 BMPHIN96
      ENDIF                                                             BMPHIN97
C                                                                       BMPHIN98
C Verify min theta range, adjust MAX theta ranges to PI/2 if necessary  BMPHIN99
C                                                                       BMPHI100
      IF (THMIN.LT.THMNAB) THEN                                         BMPHI101
        WRITE(IOU,*)                                                    BMPHI102
        WRITE(IOU,*) '   !!! Min. THETA impact on beampipe too low!',   BMPHI103
     &    '  Set to ',THMNAB,' radians !!!'                             BMPHI104
        THMIN = THMNAB                                                  BMPHI105
      ENDIF                                                             BMPHI106
      IF (THMAX.GT.PIBY2) THEN                                          BMPHI107
        WRITE(IOU,*)                                                    BMPHI108
        WRITE(IOU,*)                                                    BMPHI109
     &    '   !!!  Max.THETA greater than PI/2.   Set to PI/2 !!! '     BMPHI110
        THMAX = PIBY2                                                   BMPHI111
      ENDIF                                                             BMPHI112
C                                                                       BMPHI113
C Adjust phi range -- if phi1 > phi2 then generate from phi1 to phi2 - 2BMPHI114
C   clockwise range rather than counterclockwise                        BMPHI115
      IF (ABS(PHIMAX-PHIMIN).GT.TWOPI) PHIMAX=PHIMIN+TWOPI              BMPHI116
      IF (PHIMAX.LT.PHIMIN) THEN                                        BMPHI117
        FMAX = PHIMIN                                                   BMPHI118
        PHIMIN = PHIMAX - TWOPI                                         BMPHI119
        PHIMAX = FMAX                                                   BMPHI120
      ENDIF                                                             BMPHI121
C Print out used values                                                 BMPHI122
      KEVGEV = 1000000.                                                 BMPHI123
      WRITE(IOU,*)                                                      BMPHI124
      IF (IGEN.EQ.0)                                                    BMPHI125
     &  WRITE(IOU,*) '     Energy range: ',EMIN*KEVGEV,' to ',          BMPHI126
     &  EMAX*KEVGEV,' keV'                                              BMPHI127
      WRITE(IOU,*) '          Z range: ',  ZMIN,' to ',  ZMAX,' cm'     BMPHI128
      IF (IGEN.EQ.0) THEN                                               BMPHI129
        WRITE(IOU,*) '  Theta of impact: ', THMIN,' to ', THMAX,        BMPHI130
     &  ' radians from ',CDIR(NINT(TBEAM)+2),' direction'               BMPHI131
        WRITE(IOU,*) '  Azimuthal range: ',PHIMIN,' to ',PHIMAX,        BMPHI132
     &  ' radians'                                                      BMPHI133
      ENDIF                                                             BMPHI134
c                                                                       BMPHI135
  999 RETURN                                                            BMPHI136
      END                                                               BMPHI137
      SUBROUTINE BPHGEN(EPHOT,FPIPE,ZPIPE,TPHOT,TZPIPE)                 BPHGEN 2
C---------------------------------------------------------------------- BPHGEN 3
C!  -                                                                   BPHGEN 4
C!                                                                      BPHGEN 5
C!   Author   :- Jim Wear              23-FEB-1993                      BPHGEN 6
C!                                                                      BPHGEN 7
C!   Inputs:                                                            BPHGEN 8
C!        - Common blocks BPHPAR                                        BPHGEN 9
C!                                                                      BPHGEN10
C!   Outputs:                                                           BPHGEN11
C!        - EPHOT - 4-vector of single photon (px,py,pz,E)              BPHGEN12
C!        - FPIPE - phi of impact on beampipe                           BPHGEN13
C!        - ZPIPE - z of impact on beampipe                             BPHGEN14
C!        - TPHOT - theta of impact on beampipe                         BPHGEN15
C!        - TZPIPE- time of flight before impact on beampipe at z=0     BPHGEN16
C!                                                                      BPHGEN17
C!   Libraries required:                                                BPHGEN18
C!                                                                      BPHGEN19
C!   Description                                                        BPHGEN20
C!   ===========                                                        BPHGEN21
C!                                                                      BPHGEN22
C?                                                                      BPHGEN23
C!======================================================================BPHGEN24
      COMMON /BPHPAR/ EMIN,EMAX,ZMIN,ZMAX,THMIN,THMAX,PHIMIN,PHIMAX,    BPHCOM 2
     &  TBEAM,IGEN,IFWBW,ANGMIX                                         BPHCOM 3
      COMMON /BPHSTT/ NEVENT(5)                                         BPHCOM 4
      COMMON /BPHIO/ IOU,IDB                                            BPHCOM 5
      COMMON /BPHCON/ PI,PIBY2,TWOPI,KEVGEV                             BPHCON 2
      COMMON/SMBCOM/NSMPI,NSMIN                                         BPHCON 3
      DIMENSION EPHOT(4)                                                BPHGEN28
      DIMENSION THSCAT(2,3)  ! i: generator type , j: scattering type   BPHGEN29
      REAL THSCAT/2*0.010,2*0.002,2*0.010/                              BPHGEN30
C                                                                       BPHGEN31
C-----------------------------------------------------------------------BPHGEN32
                                                                        BPHGEN33
      IF (IGEN.LE.9) THEN                       ! FLAT spectrum         BPHGEN34
        CALL BKGFLAT(ETOT,TPHOT)                                        BPHGEN35
        TSCAT = 0.                                                      BPHGEN36
        IBCK = 0                                                        BPHGEN37
       ELSEIF(IGEN.GE.10.AND.IGEN.LE.29) THEN   ! LEP I/II spectrum     BPHGEN38
        CALL BKGSPEC(IFWBW,IGEN,ANGMIX,ETOT,TPHOT,TSCAT,IBCK)           BPHGEN39
                                                                        BPHGEN40
       ELSEIF(IGEN.GE.30) THEN                  ! ALEPH Nov.92 spec.    BPHGEN41
        IF (IFWBW.LE.0.OR.IFWBW.GE.4) IFWBW = 2  ! no mixing allowed yetBPHGEN42
        CALL BKGSPCO(IFWBW,IGEN,ETOT)                                   BPHGEN43
        IGENOLD = IGEN - 30                                             BPHGEN44
        TPHOT = THSCAT(IGENOLD,IFWBW)                                   BPHGEN45
      ENDIF                                                             BPHGEN46
C                                                                       BPHGEN47
C  Skip calls to random number generator in cases of single-valued selecBPHGEN48
      IF (ZMIN.EQ.ZMAX) THEN                                            BPHGEN49
         ZPIPE  = ZMIN                                                  BPHGEN50
        ELSE                                                            BPHGEN51
         ZPIPE = (ZMAX - ZMIN) * RNDM(DUM) + ZMIN  ! flat distribution  BPHGEN52
      ENDIF                                                             BPHGEN53
      FPIPE = PHIBP(IGEN,PHIMIN,PHIMAX)                                 BPHGEN54
C                                                                       BPHGEN55
C Now TBEAM actually indicates from which beam the photon radiated.     BPHGEN56
C Backscattered photons have their theta reversed w.r.t. TBEAM          BPHGEN57
      IF (TBEAM.EQ.0) THEN                                              BPHGEN58
        RBEAM = (RNDM(DUM)-0.5)                                         BPHGEN59
        TPHOT = SIGN(TPHOT,RBEAM)                                       BPHGEN60
       ELSE                                                             BPHGEN61
        RBEAM = TBEAM                                                   BPHGEN62
        TPHOT = SIGN(TPHOT,TBEAM)                                       BPHGEN63
      ENDIF                                                             BPHGEN64
      IF (IBCK.EQ.1) TPHOT=-1*TPHOT                                     BPHGEN65
C                                                                       BPHGEN66
C Calculate photon time-of-flight to the beampipe in nanoseconds        BPHGEN67
C   Two contributions:                                                  BPHGEN68
C       TSCAT = nominal t.o.f. for photon hitting z=0                   BPHGEN69
C       TZPOFF= time offset due to the location of impact on the pipe   BPHGEN70
C               forw or back scat., which beam radiated, and sign of impBPHGEN71
                                                                        BPHGEN72
      TZPOFF = (-2*IBCK+1)*RBEAM*ZPIPE/0.29955/100.    ! c=0.3 m/ns     BPHGEN73
      TZPIPE = TSCAT + TZPOFF                                           BPHGEN74
C                                                                       BPHGEN75
C Calculate photon four vector at the origin of ALEPH                   BPHGEN76
C  make sure it points radially outward after translation to beampipe   BPHGEN77
      EPHOT(1) = ETOT*ABS(SIN(TPHOT))*COS(FPIPE)                        BPHGEN78
      EPHOT(2) = ETOT*ABS(SIN(TPHOT))*SIN(FPIPE)                        BPHGEN79
      EPHOT(3) = ETOT*COS(TPHOT)                                        BPHGEN80
      EPHOT(3) = SIGN(EPHOT(3),TPHOT)                                   BPHGEN81
      EPHOT(4) = ETOT                                                   BPHGEN82
C      WRITE(6,*) 'EPHOT = ',(EPHOT(I),I=1,4),' TPHOT =',TPHOT          BPHGEN83
c      WRITE(6,*) ' TSCAT = ',TSCAT,'  TZPOFF = ',TZPOFF,               BPHGEN84
c     &  ' TZPIPE = ',TZPIPE,'  IBCK = ', IBCK,' ZPIPE =', ZPIPE        BPHGEN85
  999 RETURN                                                            BPHGEN86
      END                                                               BPHGEN87
      SUBROUTINE BKGFLAT(ETOT,TPHOT)                                    BKGFLAT2
C---------------------------------------------------------------------- BKGFLAT3
C!  -                                                                   BKGFLAT4
C!                                                                      BKGFLAT5
C!   Author   :- Jim Wear              24-MAR-1993                      BKGFLAT6
C!                                                                      BKGFLAT7
C!   Inputs: Common block BPHPAR                                        BKGFLAT8
C!                                                                      BKGFLAT9
C!   Outputs: ETOT, TPHOT as generated from a flat distribution         BKGFLA10
C!======================================================================BKGFLA11
      COMMON /BPHPAR/ EMIN,EMAX,ZMIN,ZMAX,THMIN,THMAX,PHIMIN,PHIMAX,    BPHCOM 2
     &  TBEAM,IGEN,IFWBW,ANGMIX                                         BPHCOM 3
      COMMON /BPHSTT/ NEVENT(5)                                         BPHCOM 4
      COMMON /BPHIO/ IOU,IDB                                            BPHCOM 5
C                                                                       BKGFLA14
C  Skip calls to random number generator in cases of single-valued selecBKGFLA15
      IF (EMIN.EQ.EMAX) THEN                                            BKGFLA16
         ETOT  = EMIN                                                   BKGFLA17
        ELSE                                                            BKGFLA18
         ETOT = (EMAX - EMIN) * RNDM(DUM) + EMIN  ! flat distribution   BKGFLA19
      ENDIF                                                             BKGFLA20
      IF (THMIN.EQ.THMAX) THEN                                          BKGFLA21
         TPHOT  = THMIN                                                 BKGFLA22
        ELSE                                                            BKGFLA23
         TPHOT = (THMAX - THMIN) * RNDM(DUM) + THMIN  ! flat distributioBKGFLA24
      ENDIF                                                             BKGFLA25
  999 RETURN                                                            BKGFLA26
      END                                                               BKGFLA27
      SUBROUTINE BKGSPEC(MODE,IGEN,ANGMIX,ENERGY,THETA,TOF,IBCK)        BKGSPEC2
C                                          J. Rothberg   15 Marc        BKGSPEC3
C                                                                       BKGSPEC4
C                                                                       BKGSPEC5
C   raw LEP  data per keV ( not binned)       22 April 1993             BKGSPEC6
C   relative contributions of diff. scattering points. 26 April JAW     BKGSPEC7
C-----------------------------------------------------------------------BKGSPEC8
C   read, integrate, interpolate background photon spectrum             BKGSPEC9
C   generate random variable according to energy spectrum               BKGSPE10
C-----------------------------------------------------------------------BKGSPE11
      COMMON /BPHCON/ PI,PIBY2,TWOPI,KEVGEV                             BPHCON 2
      COMMON/SMBCOM/NSMPI,NSMIN                                         BPHCON 3
           LOGICAL FIRST /.TRUE./                                       BKGSPE13
           INTEGER mode ! 0 = mixture f/b, 1=return forward   2=return bBKGSPE14
           real angmix  ! 0 = full mix of angle, >0 single source angle BKGSPE15
           REAL energy    ! return value of energy                      BKGSPE16
           REAL theta     ! return value of theta                       BKGSPE17
           REAL tof       ! nominal time-of-flight of scattered photons BKGSPE18
                                                                        BKGSPE19
           INTEGER ndat                ! Raw data channels              BKGSPE20
C           PARAMETER (ndat=35)                                         BKGSPE21
           PARAMETER (ndat=60)                                          BKGSPE22
                                                                        BKGSPE23
           REAL Y(ndat),E(ndat),Bfw(ndat),Bbw(ndat)                     BKGSPE24
                                                                        BKGSPE25
           INTEGER nint                ! interpolated channels          BKGSPE26
c          PARAMETER(nint=80)                                           BKGSPE27
c          PARAMETER(nint=200)                                          BKGSPE28
           PARAMETER(nint=100)                                          BKGSPE29
           REAL en(nint),Dfw(nint),Dbw(nint)                            BKGSPE30
           REAL CNfw(nint),CNbw(nint),Cfw(nint),Cbw(nint)               BKGSPE31
C----------------------------------------------------------------       BKGSPE32
           REAL XLOW/15.0/         ! first energy point                 BKGSPE33
           REAL XWID/5.0/          ! bin width, keV                     BKGSPE34
           REAL XEDGE/12.5/        ! low edge of first bin, keV         BKGSPE35
C----------------------------------------------------------------       BKGSPE36
           PARAMETER( nsrc = 10 )                                       BKGSPE37
           INTEGER nfwbw(nsrc),list(nsrc),nang(nsrc)                    BKGSPE38
           REAL    angle(nsrc),fractot(nsrc),pbgen(nsrc),prob(0:nsrc),  BKGSPE39
     &       tdelay(nsrc)                                               BKGSPE40
           CHARACTER *4 CNAME,DMODE,FDEVI,CNAM2,DMOD2,FDEV2             BKGSPE41
           CHARACTER *10 DTYPE,ATYPE,DTYP2,ATYP2                        BKGSPE42
           CHARACTER *80 FNAME,FNAM2                                    BKGSPE43
           DATA CNAME,CNAM2 /'GPHY','GANG'/                             BKGSPE44
           DATA DTYPE,DTYP2 /'NATIVE    ','NATIVE    '/                 BKGSPE45
           DATA DMODE,DMOD2 /'   A','   A'/                             BKGSPE46
C=======================================================================BKGSPE47
        IF(FIRST) THEN                                                  BKGSPE48
            FIRST = .FALSE.                                             BKGSPE49
C-----------------------------------------------------------------------BKGSPE50
            CALL ACDARG(CNAME,DTYPE,DMODE,FNAME,ATYPE,FDEVI,IER1)       BKGSPE51
            CALL ACDARG(CNAM2,DTYP2,DMOD2,FNAM2,ATYP2,FDEV2,IER2)       BKGSPE52
            if ( ier1.ne.0 .or.  ier2.ne.0) then                        BKGSPE53
C            IF (IGEN.EQ.11) THEN                         !I 35 nm colliBKGSPE54
C              OPEN( 7,file='ph100c35.phy',status='old')                BKGSPE55
C              OPEN(17,file='ph100c35.ang',status='old')                BKGSPE56
C            ELSEIF (IGEN.EQ.12) THEN                     !I 40 nm colliBKGSPE57
C              OPEN( 7,file='ph100c40.phy',status='old')                BKGSPE58
C              OPEN(17,file='ph100c40.ang',status='old')                BKGSPE59
C            ELSEIF (IGEN.EQ.21) THEN                     ! LEP II      BKGSPE60
C              OPEN( 7,file='ph200np.phy',status='old')                 BKGSPE61
C              OPEN(17,file='ph200np.ang',status='old')                 BKGSPE62
C            ELSE                                                       BKGSPE63
C               WRITE(6,*)                                              BKGSPE64
C     &      ' BKGSPEC: Undefined generator number!! IGEN = ', IGEN     BKGSPE65
           WRITE(6,*) ' I will stop program now.  Check card files.'    BKGSPE66
           WRITE(6,*)' ERROR',IER1,IER2,'FROM ACDARG CALLED BY BKGSPEC' BKGSPE67
           WRITE(6,*)' Did you provide a valid GPHY and GANG card ? '   BKGSPE68
             CALL EXIT                                                  BKGSPE69
             ELSE                                                       BKGSPE70
                CALL AOPEN(7,FNAME,ATYPE,FDEVI,IER1)                    BKGSPE71
                CALL AOPEN(17,FNAM2,ATYP2,FDEV2,IER2)                   BKGSPE72
                IF(IER1.NE.0)THEN                                       BKGSPE73
                  WRITE(6,*)' ERROR',IER1,'FROM AOPEN CALLED BY BKGSPEC'BKGSPE74
                  IF(IER1.EQ.-1)WRITE(6,*)                              BKGSPE75
     1         ' THE FILE :',FNAME,' DOES NOT EXIST '                   BKGSPE76
                  call exit                                             BKGSPE77
                ENDIF                                                   BKGSPE78
                IF(IER2.NE.0)THEN                                       BKGSPE79
                  WRITE(6,*)' ERROR',IER2,'FROM AOPEN CALLED BY BKGSPEC'BKGSPE80
                  IF(IER2.EQ.-1)WRITE(6,*)                              BKGSPE81
     1         ' THE FILE :',FNAM2,' DOES NOT EXIST '                   BKGSPE82
                  call exit                                             BKGSPE83
                ENDIF                                                   BKGSPE84
                                                                        BKGSPE85
             ENDIF                                                      BKGSPE86
C-----------------------------------------------------------------------BKGSPE87
                                                                        BKGSPE88
c           OPEN(8,file='lepiic.dat',status='new')    ! integrated      BKGSPE89
c           OPEN(9,file='lepiii.dat',status='new')    ! interpolated    BKGSPE90
                                                                        BKGSPE91
C           READ(7,'(A)')                                               BKGSPE92
           i=0                                                          BKGSPE93
 1         CONTINUE                                                     BKGSPE94
           i=i+1                                                        BKGSPE95
           READ(7,'(1x,3F12.6)',end= 990) E(i),Bfw(i),Bbw(i)            BKGSPE96
C           PRINT *,E(i),Bfw(i),Bbw(i)                                  BKGSPE97
           GOTO 1                                                       BKGSPE98
 990       CONTINUE                                                     BKGSPE99
           num = i-2                                                    BKGSP100
C-----------------------------------------------------------------------BKGSP101
C Interpolate                                                           BKGSP102
           DO j=1,nint                                                  BKGSP103
            en(j)= XLOW + XWID*(j-1)                                    BKGSP104
            DO kk = 1,num                                               BKGSP105
              IF(en(j) .LT. e(kk)) GOTO 201                             BKGSP106
            ENDDO                                                       BKGSP107
201         CONTINUE                                                    BKGSP108
            k=kk                                                        BKGSP109
            DINT = (en(j)-e(k-1))/(e(k)-e(k-1))                         BKGSP110
            Dfw(j)= Bfw(k-1) + DINT*(Bfw(k)-Bfw(k-1))                   BKGSP111
            Dbw(j)= Bbw(k-1) + DINT*(Bbw(k)-Bbw(k-1))                   BKGSP112
C            WRITE(9,'(1x,3F12.5,2F12.2)')en(j),Dfw(j),Dbw(j)           BKGSP113
          ENDDO                                                         BKGSP114
C-----------------------------------------------------------------------BKGSP115
C Accumulate                                                            BKGSP116
           Cfw(1) = Dfw(1)*xwid                                         BKGSP117
           Cbw(1) = Dbw(1)*xwid                                         BKGSP118
           DO i = 2, nint                                               BKGSP119
             Cfw(i) = Cfw(i-1) + Dfw(i)*xwid                            BKGSP120
             Cbw(i) = Cbw(i-1) + Dbw(i)*xwid                            BKGSP121
           ENDDO                                                        BKGSP122
C   normalize cumulative distribution                                   BKGSP123
           Cfwl=Cfw(nint)                                               BKGSP124
           Cbwl=Cbw(nint)                                               BKGSP125
C           WRITE(6,*)' Sum of rebinned data', Cfwl, Cbwl               BKGSP126
           DO i = 1,nint                                                BKGSP127
             CNfw(i)=Cfw(i)/Cfwl                                        BKGSP128
             CNbw(i)=Cbw(i)/Cbwl                                        BKGSP129
c            WRITE(8,'(1x,F8.0,2(F10.4,F12.0,F10.4,2x))')               BKGSP130
c    &           EN(i),Dfw(i),Cfw(i),CNfw(i),Dbw(i),Cbw(i),CNbw(i)      BKGSP131
           ENDDO                                                        BKGSP132
           CNfw(nint) = 1.0                                             BKGSP133
           CNbw(nint) = 1.0                                             BKGSP134
          CLOSE(7)                                                      BKGSP135
C-----------------------------------------------------------------------BKGSP136
c                                                                       BKGSP137
c Read in photon incident angle file, count up different angles which coBKGSP138
c  sum prob.                                                            BKGSP139
           PBTOT = 0.                                                   BKGSP140
           PBFW  = 0.                                                   BKGSP141
           NA    = 0                                                    BKGSP142
           IANG  = 0                                                    BKGSP143
           j=0                                                          BKGSP144
 2         CONTINUE                                                     BKGSP145
           j=j+1                                                        BKGSP146
           READ(17,'(1x,i10,f12.4,2f12.3)',END= 991)                    BKGSP147
     &       NFWBW(J),ANGLE(J),FRACTOT(J),TDELAY(J)                     BKGSP148
c          PRINT *,NFWBW(J),ANGLE(J),FRACTOT(J),TDELAY(J)               BKGSP149
           IF ((MODE.EQ.0).OR.                        ! all angles/direcBKGSP150
     &         (MODE.EQ.NFWBW(J))) THEN              ! only one directioBKGSP151
             NA = NA + 1                                                BKGSP152
             PBTOT = FRACTOT(J) + PBTOT                                 BKGSP153
             LIST(NA) = J                                               BKGSP154
             IF (NFWBW(J).EQ.1) PBFW = PBFW + FRACTOT(J)                BKGSP155
           ENDIF                                                        BKGSP156
           DANGMIX = ANGLE(J)-ANGMIX                                    BKGSP157
           IF (ANGMIX.NE.0.AND.(ABS(DANGMIX).LE.0.0003))                BKGSP158
     &       IANG = J                                                   BKGSP159
c          PRINT *,IANG,J,angmix,DANGMIX                                BKGSP160
           GOTO 2                                                       BKGSP161
 991       CONTINUE                                                     BKGSP162
c                                                                       BKGSP163
c look for error conditions in initialization                           BKGSP164
           IF (NA.GT.NSRC) THEN                                         BKGSP165
             WRITE(6,*)                                                 BKGSP166
     &         ' ERROR: too many source angles specified-- STOP'        BKGSP167
             STOP                                                       BKGSP168
           ENDIF                                                        BKGSP169
           IF (PBTOT.EQ.0.OR.NA.EQ.0) THEN                              BKGSP170
             WRITE(6,*)                                                 BKGSP171
     &         ' ERROR:in initialization of angle probs.-- STOP'        BKGSP172
             STOP                                                       BKGSP173
           ENDIF                                                        BKGSP174
           IF (ANGMIX.NE.0.AND.IANG.EQ.0) THEN                          BKGSP175
             WRITE(6,*)                                                 BKGSP176
     &        ' ERROR: specified single source angle not available'     BKGSP177
             WRITE(6,*) ' -- STOP'                                      BKGSP178
             STOP                                                       BKGSP179
           ENDIF                                                        BKGSP180
                                                                        BKGSP181
c recalculate angle probabilities according to user selection           BKGSP182
c create array for later random sampling                                BKGSP183
           SUMPB   = 0.                                                 BKGSP184
           PROB(0)  = 0.                                                BKGSP185
           DO K  = 1,NA                                                 BKGSP186
             PBGEN(K) = FRACTOT(LIST(K))/PBTOT                          BKGSP187
             SUMPB    = SUMPB + PBGEN(K)                                BKGSP188
             PROB(K)  = SUMPB                                           BKGSP189
c            PRINT *,LIST(K),PBGEN(K),PROB(K),ANGLE(LIST(K))            BKGSP190
           ENDDO                                                        BKGSP191
           PROB(NA) = 1.                                                BKGSP192
                                                                        BKGSP193
          CLOSE(17)                                                     BKGSP194
          GOTO 999                                                      BKGSP195
         ENDIF     ! initialization                                     BKGSP196
C=====================================================================  BKGSP197
C normal entry                                                          BKGSP198
C get energy in mixed mode or single direction mode, set backscatter flaBKGSP199
           IBCK = MODE - 1                                              BKGSP200
           IF(mode .EQ. 0) THEN                                         BKGSP201
             RNDFW = RNDM(DUM)                                          BKGSP202
             IF (RNDFW.LE.PBFW) THEN                                    BKGSP203
               IBCK = 0                                                 BKGSP204
               CALL HISRAN(CNfw,nint,XEDGE,XWID,xrtn)                   BKGSP205
              ELSE                                                      BKGSP206
               IBCK = 1                                                 BKGSP207
               CALL HISRAN(CNbw,nint,XEDGE,XWID,xrtn)                   BKGSP208
             ENDIF                                                      BKGSP209
           ENDIF                                                        BKGSP210
           IF(mode .EQ. 1)  CALL HISRAN(CNfw,nint,XEDGE,XWID,xrtn)      BKGSP211
           IF(mode .EQ. 2)  CALL HISRAN(CNbw,nint,XEDGE,XWID,xrtn)      BKGSP212
           energy= xrtn/KEVGEV                                          BKGSP213
c                                                                       BKGSP214
C get angle from multiple source or single source                       BKGSP215
           IF (ANGMIX.EQ.0.) THEN                                       BKGSP216
             RNDANG = RNDM(DUM)                                         BKGSP217
             IANG = 0                                                   BKGSP218
             DO 3 K = 0,NA-1                                            BKGSP219
 3            IF (RNDANG.GT.PROB(K).AND.RNDANG.LE.PROB(K+1))            BKGSP220
     &          IANG = LIST(K+1)                                        BKGSP221
           ENDIF                                                        BKGSP222
           THETA = ANGLE(IANG)                                          BKGSP223
           TOF   = TDELAY(IANG)                                         BKGSP224
c                                                                       BKGSP225
c           print *,'BKGSPEC: mode,energy,theta,tof:',mode,energy,theta,BKGSP226
 999       CONTINUE                                                     BKGSP227
           RETURN                                                       BKGSP228
           END                                                          BKGSP229
           SUBROUTINE BKGSPCO(MODE,IGENE,ENERGY)                        BKGSPCO2
C                                                  J. Rothberg   15 MarcBKGSPCO3
C-----------------------------------------------------------------------BKGSPCO4
C               read, integrate, interpolate background photon spectrum BKGSPCO5
C               generate random variable according to energy spectrum   BKGSPCO6
C          Input spectrum is original Nov.92 Collimator scan spectra    BKGSPCO7
C            May be superceded by April 20, 1993 files.                 BKGSPCO8
C-----------------------------------------------------------------------BKGSPCO9
      COMMON /BPHCON/ PI,PIBY2,TWOPI,KEVGEV                             BPHCON 2
      COMMON/SMBCOM/NSMPI,NSMIN                                         BPHCON 3
           LOGICAL FIRST /.TRUE./                                       BKGSPC11
           INTEGER mode  ! 0 = mix of f/b, 1=return forward   2=return bBKGSPC12
           INTEGER IGENE                                                BKGSPC13
           REAL energy   ! return value of energy                       BKGSPC14
           INTEGER ndat                ! Raw data channels              BKGSPC15
           PARAMETER (ndat=35)                                          BKGSPC16
           REAL Y(ndat),E(ndat),Bfw(ndat),Bbw(ndat)                     BKGSPC17
           REAL CNfw(ndat),CNbw(ndat),Cfw(ndat),Cbw(ndat)               BKGSPC18
           INTEGER nint                ! interpolated channels          BKGSPC19
           PARAMETER(nint=80)                                           BKGSPC20
           REAL en(nint),Dfw(nint),Dbw(nint)                            BKGSPC21
C----------------------------------------------------------------       BKGSPC22
           REAL XLOW/10.0/         ! low edge of first bin, keV         BKGSPC23
           REAL XWID/5.0/          ! bin width, keV                     BKGSPC24
C=======================================================================BKGSPC25
        IF (FIRST) THEN                                                 BKGSPC26
            FIRST = .FALSE.                                             BKGSPC27
C           WRITE(6,*) '  INITIALIZING BKGSPEC: IGEN,MODE = ',IGENE,MODEBKGSPC28
           IF (IGENE.EQ.31 ) THEN                                       BKGSPC29
             OPEN(7,file='bkg31mm.dat',status='old')     ! 31 mm collim BKGSPC30
           ELSEIF (IGENE.EQ.32 ) THEN                                   BKGSPC31
             OPEN(7,file='bkg40mm.dat',status='old')      ! 40 mm collimBKGSPC32
           ELSE                                                         BKGSPC33
             WRITE(6,*)                                                 BKGSPC34
     &      ' BKGSPEC_COLLSCAN: Undefined generator number!! IGEN = ',  BKGSPC35
     &       IGENE                                                      BKGSPC36
             WRITE(6,*) ' I will stop program now.  Check card files.'  BKGSPC37
             STOP                                                       BKGSPC38
           ENDIF                                                        BKGSPC39
c           OPEN(8,file='bkg40mmc.dat',status='new')    ! integrated    BKGSPC40
c           OPEN(9,file='bkg40mmi.dat',status='new')    ! interpolated  BKGSPC41
           READ(7,'(A)')                                                BKGSPC42
           i=0                                                          BKGSPC43
 1         CONTINUE                                                     BKGSPC44
           i=i+1                                                        BKGSPC45
           READ(7,'(3F10.3)',end= 990) E(i),Bfw(i),Bbw(i)               BKGSPC46
C           PRINT *,E(i),Bfw(i),Bbw(i)                                  BKGSPC47
           GOTO 1                                                       BKGSPC48
 990       CONTINUE                                                     BKGSPC49
           num = i-2                                                    BKGSPC50
C-----------------------------------------------------------------------BKGSPC51
C  the lines below were changed the 25 March 1993 for input binned data BKGSPC52
           Cfw(1) = Bfw(1)                                              BKGSPC53
           Cbw(1) = Bbw(1)                                              BKGSPC54
           DO i=2, num                                                  BKGSPC55
               Cfw(i)=Cfw(i-1)+ Bfw(i)                                  BKGSPC56
               Cbw(i)=Cbw(i-1)+ Bbw(i)                                  BKGSPC57
           ENDDO                                                        BKGSPC58
C-----------------------------------------------------------------------BKGSPC59
C   normalize cumulative distribution                                   BKGSPC60
           Cfwl=Cfw(num)                                                BKGSPC61
           Cbwl=Cbw(num)                                                BKGSPC62
           DO i = 1,num                                                 BKGSPC63
             CNfw(i)=Cfw(i)/Cfwl                                        BKGSPC64
             CNbw(i)=Cbw(i)/Cbwl                                        BKGSPC65
cc           WRITE(8,'(1x,F12.4,2(F10.5,F12.1,4x))')                    BKGSPC66
cc     &             E(i),CNfw(i),Cfw(i),CNbw(i),Cbw(i)                 BKGSPC67
           ENDDO                                                        BKGSPC68
           CNfw(num) = 1.0                                              BKGSPC69
           CNbw(num) = 1.0                                              BKGSPC70
C Interpolate                                                           BKGSPC71
           pref = 0.0                                                   BKGSPC72
           preb = 0.0                                                   BKGSPC73
           DO j=1,nint                                                  BKGSPC74
            en(j)= XLOW + XWID*(j-1)                                    BKGSPC75
            DO kk = 1,num                                               BKGSPC76
              IF(en(j) .LT. e(kk)) GOTO 201                             BKGSPC77
            ENDDO                                                       BKGSPC78
201         CONTINUE                                                    BKGSPC79
            k=kk                                                        BKGSPC80
            DINT = (en(j)-e(k-1))/(e(k)-e(k-1))                         BKGSPC81
            Dfw(j)= CNfw(k-1) + DINT*(CNfw(k)-CNfw(k-1))                BKGSPC82
            Dbw(j)= CNbw(k-1) + DINT*(CNbw(k)-CNbw(k-1))                BKGSPC83
            IF(j .GT. 1) THEN                                           BKGSPC84
                  pref = Dfw(j-1)                                       BKGSPC85
                  preb = Dbw(j-1)                                       BKGSPC86
            ENDIF                                                       BKGSPC87
             delf =306.6*( Dfw(j)-pref)                                 BKGSPC88
             delb =55526.7*( Dbw(j)-preb)                               BKGSPC89
cc         WRITE(9,'(1x,3F12.5,2F12.1)')en(j),Dfw(j),Dbw(j),delf,delb   BKGSPC90
           ENDDO                                                        BKGSPC91
           Dfw(nint) = 1.0                                              BKGSPC92
           Dbw(nint) = 1.0                                              BKGSPC93
          CLOSE(7)                                                      BKGSPC94
          GOTO 999                                                      BKGSPC95
         ENDIF     ! initialization                                     BKGSPC96
C=======================================================================BKGSPC97
C normal entry                                                          BKGSPC98
                                                                        BKGSPC99
           IF(mode .EQ. 1)  CALL HISRAN(Dfw,nint,XLOW,XWID,xrtn)        BKGSP100
           IF(mode .EQ. 2)  CALL HISRAN(Dbw,nint,XLOW,XWID,xrtn)        BKGSP101
           IF(mode .EQ. 3)  CALL HISRAN(Dbw,nint,XLOW,XWID,xrtn)        BKGSP102
           energy= xrtn/KEVGEV                                          BKGSP103
 999       CONTINUE                                                     BKGSP104
           RETURN                                                       BKGSP105
           END                                                          BKGSP106
      REAL FUNCTION PHIBP(IGENE,PHIMIN,PHIMAX)                          PHIBP  2
C---------------------------------------------------------------------- PHIBP  3
C!                                                                      PHIBP  4
C!   Author   :- Jim Wear              25-APR-1993                      PHIBP  5
C!                                                                      PHIBP  6
C!   Inputs: BKG photon generator number                                PHIBP  7
C!                                                                      PHIBP  8
C!   Outputs:   PHIBP = phi of photon impact on beampipe                PHIBP  9
C!                                                                      PHIBP 10
C!   Description                                                        PHIBP 11
C!   ===========  LEP beam photon spectra phi range is limited to the   PHIBP 12
C!                region around the horizontal plane.  Flat spectrum    PHIBP 13
C!                may go anywhere as specified by input cards.          PHIBP 14
C!======================================================================PHIBP 15
      COMMON /BPHCON/ PI,PIBY2,TWOPI,KEVGEV                             BPHCON 2
      COMMON/SMBCOM/NSMPI,NSMIN                                         BPHCON 3
      REAL DPHIBP/0.500/        ! 0.5 radian range in phi (28.6 deg)    PHIBP 18
c                                                                       PHIBP 19
      PHI = 0.                                                          PHIBP 20
      IF (IGENE.LE.9) THEN          ! Flat spectrum                     PHIBP 21
        IF (PHIMIN.EQ.PHIMAX) THEN                                      PHIBP 22
           PHI  = PHIMIN                                                PHIBP 23
          ELSE                                                          PHIBP 24
           PHI = (PHIMAX - PHIMIN) * RNDM(DUM) + PHIMIN  ! flat distr.  PHIBP 25
        ENDIF                                                           PHIBP 26
       ELSE                        ! LEP Spectrum centered at 0 or PI   PHIBP 27
        RNDMIO = RNDM(DUM)                                              PHIBP 28
        IF (RNDMIO.GE.0.5) PHI = PI                                     PHIBP 29
        PHI = PHI + DPHIBP * (RNDM(DUM) - 0.5)                          PHIBP 30
        IF (PHI.LT.0) PHI = TWOPI + PHI                                 PHIBP 31
      ENDIF                                                             PHIBP 32
      PHIBP = PHI                                                       PHIBP 33
  999 RETURN                                                            PHIBP 34
      END                                                               PHIBP 35
      SUBROUTINE USCJOB                                                 USCJOB 2
      COMMON /BPHCON/ PI,PIBY2,TWOPI,KEVGEV                             BPHCON 2
      COMMON/SMBCOM/NSMPI,NSMIN                                         BPHCON 3
      CLOSE (19)                                                        USCJOB 4
      WRITE (6,*)                                                       USCJOB 5
      WRITE (6,*) '   ----------------------------------------------'   USCJOB 6
      WRITE (6,*) '    Number of photons hitting Samba pads:         '  USCJOB 7
     &  , NSMPI                                                         USCJOB 8
      WRITE (6,*) '    Number of photons entering Samba via beampipe '  USCJOB 9
     &  , NSMIN                                                         USCJOB10
      WRITE (6,*) '   ----------------------------------------------'   USCJOB11
 999  RETURN                                                            USCJOB12
      END                                                               USCJOB13
