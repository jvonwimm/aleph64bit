 SUBSCHEMA LcalRAWBanks
 : 'Description of LCAL Raw data banks '
     
 AUTHOR   'P.H.Hansen'
 REVIEWER 'F.Ranjard'
 VERSION  '4.1'
 DATE     '24/03/97'
     
  DEFINE ESET
     
 LOLE
      :      'Lcal online error flags \
              number of words / row\
              number of rows'
              STATIC

      = (FastBus(4),
         Spark   = INTE [0,230] : 'plane + (module-1)*64',
         TimeOut = INTE [0,1]  : 'GBX timeout flag',
         Marker(4),
         HVtrip = INTE [0,15] : 'HV trip bitpat',
         ERrorflag = INTE [-1,1] : 'Error flag')
      ;

 LPAS
      :      'Lcal Pastis record  \
              number of words / row\
              number of rows'
              STATIC

             = (Module, Digit(16))
             KEY  * = (Module)  END KEY
      ;

 LCRA
      :      'Lcal Crack Scintillator  \
              number of words/scintillator\
              number of scintillators (=4)'
              STATIC

             = (Counts(2) = INTE [0,*] : 'ADC counts of PM1 and PM2')
      ;

  LTDI
             : 'Lc Storey data\
                number of words/tower\
                number of towers'
            STATIC
             = (Address, Digit(3))
             KEY  * = (Address)  END KEY
            ;
     
  LWDI
             : 'Lc Wire data\
                number of words/module\
                number of modules'
            STATIC
             = (Module,Digit(38))
             KEY  * = (Module)  END KEY
            ;

 END ESET

 END SUBSCHEMA

     
 SUBSCHEMA LcalJULBanks
 : 'Description of Lcal banks used in JULIA'
     
 AUTHOR   'P.H.Hansen'
 REVIEWER 'J.Knobloch, H. Meinhard'
 VERSION  '1.1'
 DATE     '24-Apr-1991'
     
    DEFINE ATTRIBUTE
     
    Module
             = INTE [1,4]
             : 'Lcal module number';
     
    Address  = INTE [1,2016]
             : 'column + (row-1)*16 + (module-1)*512';
     
    Addrplane = INTE [1,230]
             : ' plane + (module-1)*64';

    Energy
             = REAL [0.000,300.000]
             : 'Energy in GeV';
     
    Digit
             = INTE [0,*]
             : 'Energy in MeV';
     
   FastBus   = INTE [0,*]      : 'Fastbus error bitpattern';

   Marker    = INTE [0,*]      : 'Missing marker bitpattern';

     
   END ATTRIBUTE
     
 DEFINE ESET
     
  LSDA
             : 'Lc storey data after preparation\
                number of words/tower\
                number of towers'
            STATIC
             = (Address, Energy(3))
             KEY  * = (Address)  END KEY
            ;
  LPDA
             : 'Lc wire plane data after preparation\
                number of words/module\
                number of modules'
            STATIC
             = (Module,Energy(38),
                ESum = REAL[0.000,300.000]   :'Sum of wire energies')
             KEY  * = (Module)  END KEY
            ;
     
  LCLU
            : 'Lcal Clusters \
               Number of words/cluster\
               Number of clusters'
           STATIC
     
      = (Energy1      = REAL [0.000,300.000] : 'Energy of stack 1',
         Energy2      = REAL [0.000,300.000] : 'Energy of stack 2',
         Energy3      = REAL [0.000,300.000] : 'Energy of stack 3',
         Energyclust4 = REAL [0.000,90.000] : 'Cluster energy',
         Theta1       = REAL [0.0000,3.1417]: 'Theta of stack 1',
         Theta2       = REAL [0.0000,3.1417]: 'Theta of stack 2',
         Theta3       = REAL [0.0000,3.1417]: 'Theta of stack 3',
         Thetaclust4  = REAL [0.0000,3.1417]: 'Cluster theta',
         Fhi1         = REAL [0.000,6.284]  : 'Phi of stack 1',
         Fhi2         = REAL [0.000,6.284]  : 'Phi of stack 2',
         Fhi3         = REAL [0.000,6.284]  : 'Phi of stack 3',
         Fhiclust4    = REAL [0.000,6.284]  : 'Cluster phi',
         Radius1      = REAL [0.0,310.0]    : 'Distance of stack 1',
         Radius2      = REAL [0.0,310.0]    : 'Distance of stack 2',
         Radius3      = REAL [0.0,310.0]    : 'Distance of stack 3',
         Radiusclust4 = REAL [0.0,310.0]    : 'Cluster distance',
         Width1       = REAL [0.0,25.0]     : 'Shower width in stack 1',
         Width2       = REAL [0.0,25.0]     : 'Shower width in stack 2',
         Width3       = REAL [0.0,25.0]     : 'Shower width in stack 3',
         Widthclust4  = REAL [0.0,25.0]     : 'Shower width',
         widthXX      = REAL [0.00,25.5]    : 'Shower width in X',
         widthYY      = REAL [0.00,25.5]    : 'Shower width in Y',
         corrXY       = REAL [-1.00,1.00]   : 'XY correlation',
         Module,
         NTow         = INTE [1,100]        : 'Number of towers')
      ;
     
 LOBJ
        : 'Lcal object (cluster)\
           number of words/cluster\
           number of clusters'
        STATIC
     
         = (CHarge     = INTE [-1,1]         :'Charged track flag',
            EnergyCorr = REAL [0.000,300.000]:'Corrected energy',
            ThetaCorr  = REAL [0.0000,3.1417]:'Corrected theta',
            PhiCorr    = REAL [0.000,6.2832] :'Corrected phi',
            KDrg       = INTE [0,255]        :'Region code',
            KTyp       = INTE [0,255]        :'Hypothesis type',
            R1esti     = REAL [-10.0,10.0]   :'Prob(1/alpha using storeys)',
            R2esti     = REAL [-10.0,10.0]   :'Prob(alpha/beta using storeys)',
            R3esti     = REAL [-10.0,10.0]   :'Prob(E4/ERaw)',
            R4esti     = REAL [-10.0,10.0]   :'Prob(E9/ERaw)')
            ;
     
  LTRK
             : 'Cluster and tower pointers\
                number of words / STRK track\
                number of tracks'
             STATIC
             =(DisClust = REAL[0.0,25.5]    :'Dist. to nearest cluster',
               DisAcc(2)= REAL[-1.0,24.5]   :'Dist. to acceptance edges')
             ;
     
     
 LIDT
      :      'Lumi electron identification bank\
              Number of words/Lumi electron\
              Number of Lumi electrons'
           STATIC
     
      = (IFlag        = INTE [0,255]       : 'Quality flag for tracks',
         ECorr        = REAL [0.00,300.00] : 'Corrected energy',
         TCorr        = REAL [0,3.1416]    : 'Corrected Theta',
         PCorr        = REAL [0,6.2832]    : 'Corrected Phi',
         E4tower      = REAL [0.00,300.00] : 'Energy in 4 towers (5MeV)',
         Esta1        = REAL [0.000,1.000] : 'Fraction in stack 1',
         Esta2        = REAL [0.000,1.000] : 'Fraction in stack 2',
         R1esti       = REAL [-10.0,10.0]  : 'Prob(1/alpha using wires)',
         R2esti       = REAL [-10.0,10.0]  : 'Prob(beta/alpha using wires)',
         widthXX      = REAL [0.00,25.5]   : 'Shower width in X',
         widthYY      = REAL [0.00,25.5]   : 'Shower width in Y',
         corrXY       = REAL [-1.00,1.00]  : 'XY correlation',
         DisAcc(4)    = REAL [-1.0,24.4]   : 'Distances from acceptance edges',
         NTrack       = INTE [0,255]       : 'Number of tracks')
      ;
     
     
 LIFL
      :      'Lumi electron flag bank\
              Number of words/Lumi electron\
              Number of Lumi electrons'
           STATIC
     
      = (Kode1        = INTE [0,*]   : 'Bit pattern for small area hits',
         Kode2        = INTE [0,*]   : 'Bit pattern for large area hits',
         KodeH        = INTE [0,*]   : 'Bit pattern for high thresholds',
         KodeL        = INTE [0,*]   : 'Bit pattern for low thresholds')
      ;
     
 LMAP
      :      'Lcal tower map\
              Number of words/tower\
              Number of towers'
           STATIC
      ;
     
 LOCL
      :      'Local coordinates for clusters in LIDT\
              Number of words/cluster\
              Number of clusters in LIDT'
           STATIC
      = (XLocal       = REAL [-60.000,60.] : 'Local x cluster coordinate',
         YLocal       = REAL [-60.000,60.] : 'Local y cluster coordinate',
         ZLocal       = REAL [-320.000,320.]:'Local z cluster coordinate')
      ;

 END ESET
     
  DEFINE RSET
     
    (LSDA [0,1] -> [1,*] LCLU)
             : 'Index of LcCLUster';
     
    (LSDA [0,1] -> [0,1] LSDA)
             : 'Index of next tower';
     
    (LMAP [0,1] -> [1,1] LSDA)
             : 'Index of hit tower';
     
    (LCLU [1,1] -> [0,1] LSDA)
             : 'Index of first tower';
     
    (LTRK [0,1] -> [0,*] LCLU)
             : 'Index of LcCLUster';
     
    (LTRK [0,1] -> [0,*] LSDA)
             : 'Index of hit tower';
     
    (LOBJ [0,1] -> [0,1] LIDT)
             : 'Index of electron candidate (LIDT)';
     
    (LIDT [1,1] -> [0,1] LOBJ)
             : 'Index of LcOBJect (=LcCLUster)';
     
    (LIDT [1,1] -> [0,1] LSDA)
             : 'Index of central tower';
     
    (LIDT [0,1] -> [0,1] STRK)
             : 'Index of best track';
     
  END RSET
     
END SUBSCHEMA
     

 SUBSCHEMA LcalPOTBanks
 : 'Description of Lcal banks on POT/DST'
     
 AUTHOR   'P.H.Hansen'
 REVIEWER 'J.Knobloch'
 VERSION  '1.0'
 DATE     '01/12/88'
     
     
 DEFINE ESET
     
  PLSD
             : 'Pot version of LSDA \
                number of words/tower\
                number of towers'
            STATIC
             = (Address,
               Esta1 = INTE[0,60000] :'Energy stack1 (5MeV)',
               Esta2 = INTE[0,60000] :'Energy stack2 (5MeV)',
               Esta3 = INTE[0,60000] :'Energy stack3 (5MeV)')
     
             KEY  * = (Address)  END KEY
            ;
  PLPD
             : 'Pot version of LPDA \
                number of words/plane\
                number of planes'
            STATIC
             = (Addr    = INTE[0,230]       : 'plane + (module-1)*64',
                ENergy  = INTE[0,65535]     : 'Energy (1MeV)')
     
             KEY  * = (Addr)  END KEY
            ;
     
     
 PLID
      :      'Pot/dst version of LIDT\
              Number of words/Lumi electron candidate\
              Number of Lumi electron candidates'
           STATIC
     
      = (IFlag        = INTE [0,255]       : 'quality flag for identification',
         ECorr        = INTE [0,65535]     : 'Corrected energy (5MeV)',
         TCorr        = INTE [0,65535]     : 'Corrected Theta (0.05 mr)',
         PCorr        = INTE [-31416,31416]: 'Corrected Phi (0.1mr)',
         E4towers     = INTE [0,60000]     : 'Energy in 4 towers (5MeV)',
         Esta1        = INTE [0,255]       : 'Percent in stack 1',
         Esta2        = INTE [0,255]       : 'Percent in stack 2',
         R1esti       = INTE [-127,128]    : 'Prob(1/alpha using wires)',
         R2esti       = INTE [-127,128]    : 'Prob(beta/alpha using wires)',
         widthXX      = INTE [0,255]       : 'Shower width in X (mm)',
         widthYY      = INTE [0,255]       : 'Shower width in X (mm)',
         widthXY      = INTE [-127,128]    : 'XY correlation (0.01)',
         DAccept(4)   = INTE [-10,244]     : 'Distances from edges (mm)',
         NTrack       = INTE [0,255]       : 'Number of tracks')
      ;
     
     
 END ESET
     
  DEFINE RSET
     
    (PLSD [0,1] -> [1,*] PEOB)
             : 'Index of EcOBJect';
     
    (PLID [1,1] -> [0,1] PEOB)
             : 'Index of EcOBJect';
     
    (PLID [1,1] -> [0,1] PLSD)
             : 'Index of hit tower';
     
    (PLID [0,1] -> [0,1] STRK)
             : 'Index of best track';
     
  END RSET
     
 END SUBSCHEMA

 SUBSCHEMA LcalGALBanks

  : ' Lcal banks used in GALEPH only'

 AUTHOR   'P.H.Hansen'
 REVIEWER 'A. Putzer'
 VERSION  '2.1'
 DATE     '15/3/88'

    DEFINE ATTRIBUTE


    Hit
             = INTE [0,*]
             : 'Energy in Ec units, where dE/E = SQRT(Ec/E)';
  
   END ATTRIBUTE

  DEFINE ESET


    LTHT
	     : 'MC storey hits\
                number of words/tower\
                number of towers'
            STATIC
             = (Address, Hit(3))
	     KEY  * = (Address)  END KEY
            ;

    LWHT
	     : 'MC wire plane hits\
                number of words/module\
                number of modules'
            STATIC
	     = (Module,Hit(38))
	     KEY  * = (Module)  END KEY
            ;

    LTTR
             : 'MC storey trigger data\
                number of words/trigger segment\
                number of segments'
            STATIC
             = (Digit(3)) 
           ;
  
    LWTR
             : 'MC odd,even wire plane sums\
                number of words/module\
                number of modules'
            STATIC
             = (Digit(2))
            ;

    LCWS
           : 'Lcal MC job summary\
              number of words/job\
              number of jobs'
            STATIC
            = (EVents = INTE [0,*] : ' events with a particle in Lcal',
               BHabha = INTE [0,*] : ' bhabha triggers',
               FRee(10) = INTE [0,*]) 
            ;


 LSHI
      :      'Lcal Storey HIstory bank \
              Number of words per prim track energy deposit \
              Number of prim tracks energy deposits'
           STATIC
 
    = (PrimaryTrack    = INTE [1,999]       : 'primary track number ',
       TowerIndex      = INTE [1,*]         : 'I,J compressed index of tower',
       DepositEnerg(3) = INTE [0,100000]    : 'energy (ADC) deposited by the \
                                               primary track in the 3 storeys')
      ;
 
 LWHI
      :      'Lcal Wire plane HIstory  bank \
              Number of words per prim track energy deposit \
              Number of prim tracks energy deposits'
           STATIC
 
    = (PrimaryTrack     = INTE [1,999]       : 'primary track number ',
       ModuleIndex      = INTE [1,*]         : 'module #',
       DepositEnerg(38) = INTE [0,100000]    : 'energy (ADC) deposited by the \
                                                primary track in the 38 planes')
      ;
 
 
  END ESET

END SUBSCHEMA



 SUBSCHEMA LUMINOSITY
 : 'Description of banks used for luminosity calculation'

 AUTHOR   'P.H.Hansen'
 REVIEWER 'J.Knobloch, H. Meinhard'
 VERSION  '2.7'
 DATE     '30-Apr-1991'


 DEFINE ESET
    LUMI
             : 'Luminosity run statistics (successive cuts)\
                number of words / selection\
                number of selections'

             STATIC
             =(HIthr     = REAL[0.,*]  :' E1,E2 > Thresh.',
               LOthr     = REAL[0.,*]  :' E1+E2 > Thresh.',
               CoPlanar  = REAL[0.,*]  :' after coplanarity cut',
               SAtr      = REAL[0.,*]  :' in SATR region',
               TRack     = REAL[0.,*]  :' with tracks',
               Fidu1     = REAL[0.,*]  :' one track in small SATR area',
               Fidu2     = REAL[0.,*]  :' the other in large SATR area',
               NoSatr    = REAL[0.,*]  :' in NOSATR region',
               Fidu3     = REAL[0.,*]  :' one cluster in small NOSATR area',
               Fidu4     = REAL[0.,*]  :' the other in large NOSATR area',
               BHabhas   = REAL[0.,*]  :' coplanar Bhabha events',
               NotPlanar = REAL[0.,*]  :' failing coplanarity cut',
               Bhab1     = REAL[0.,*]  :' with LOLE, TPC and ECAL HV bits',
               Bhab2     = REAL[0.,*]  :' with LOLE and ECAL HV bits',
               LowLow    = REAL[0.,*]  :' only LowLow bit fired',
               VeryHigh  = REAL[0.,*]  :' only VeryHigh bit fired',
               NotVeryhi = REAL[0.,*]  :' VeryHigh bit not fired',
               Luminosity = REAL[0.,*]  :' Luminosity',
               StatError = REAL[0.,*]   :' Statistical error',
               SYstematic = REAL[0.,*]  :' Estimated systematic error')

             ;
    LBAK
             : 'Background statistics (successive cuts)\
                number of words / selection\
                number of selections'

             STATIC
             =(HIthr(2)  = REAL[0.,*]  :' E1,E2 > Thresh.\
                                          for Hi-Lo and Hi-Hi combinations',
               CoPlan(2) = REAL[0.,*]  :' after coplanarity cut',
               LOthr(2)  = REAL[0.,*]  :' E1+E2 > Thresh.',
               SAtr(2)   = REAL[0.,*]  :' in SATR region',
               TRack(2)   = REAL[0.,*]  :' with tracks',
               Fidu1(2)  = REAL[0.,*]  :' one track in small SATR area',
               Fidu2(2)  = REAL[0.,*]  :' the other in large SATR area',
               NoSatr(2) = REAL[0.,*]  :' in NOSATR region',
               Fidu3(2)  = REAL[0.,*]  :' one cluster in small NOSATR area',
               Fidu4(2)  = REAL[0.,*]  :' the other in large NOSATR area',
               BHabhas(2) = REAL[0.,*]  :' coplanar fake events',
               NotPlan(2) = REAL[0.,*]  :' failing coplanarity cut',
               Background = REAL[0.,*]  :' Bhabha background',
               StatError  = REAL[0.,*]  :' Statistical error')

             ;


     LSIN
             :      'Lumi single arm triggers\
                     Number of words/single\
                     Number of singles'
            STATIC
            = (Kode1        = INTE [0,*]   : 'Bit pattern for small area hit\
                                              bit m-1 set if method m accepts',
               Kode2        = INTE [0,*]   : 'Bit pattern for large area hit',
               KodeH        = INTE [0,*]   : 'Bit pattern for E > Hi offl. thr.',
               KodeL        = INTE [0,*]   : 'Bit pattern for E > Lo offl. thr.',
               Theta        = REAL [0.,3.14160]: 'Theta',
               Phi          = REAL [0.,6.28320]: 'Phi',
               Energy       = REAL [0.000,300.000]: 'Energy in GeV',
               Iflag        = INTE [0,3]   : 'Track quality flag\
                                              bit 0: track or patch\
                                              bit 1: track\
                                              bit 2: SATR region',
               Itype        = INTE [0,5]   : 'Trigger type\
                                              0 = non lumi\
                                              1 = exclusive single low thr.\
                                              2 = exclusive single high thr.\
                                              3 = exclusive single very high\
                                              4 = exclusive low-low single\
                                              5 = high-low' )
             ;
    LONL
             : 'Trigger statistics\
                number of words / row\
                number of rows'

             STATIC
             =(GBxcount  = REAL[0.,*]  :' #GBXs scaled by 1024',
               Triggers  = REAL[0.,*]  :' #non lumi triggers',
               Lowsing   = REAL[0.,*]  :' #exclusive low thr. singles',
               Highsing  = REAL[0.,*]  :' #exclusive high thr. singles',
               VeryHigh  = REAL[0.,*]  :' #exclusive very high singles',
               LowLow    = REAL[0.,*]  :' #exclusive low-low coincidences',
               HighLow   = REAL[0.,*]  :' #high-low coincidences',
               Random    = REAL[0.,*]  :' #Background updates')

             ;
    LEHI
             : 'Energy histograms (8 rows: Es m1, Ew m1,...,Ew m4)\
                number of words / row\
                number of rows'

             STATIC
             =(Histogram(32)  = REAL[0.,*]  :' Energy histogram',
               Binsize    = REAL[0.0,10.0]  :' Bin size',
               Elow       = REAL[0.0,200.0] :'Low edge')

             ;
    LVHI
             : 'Vertex histograms (4 rows: X m1+4, Y m1+4, X m2+3, Y m2+3)\
                number of words / row\
                number of rows'

             STATIC
             =(Histogram(60)  = REAL[0.,*]  :' Energy histogram',
               Binsize    = REAL[0.0,10.0]  :' Bin size',
               Xlow       = REAL[0.0,200.0] :'Low edge')

             ;

    LUPA     : 'Luminosity parameter bank; this bank contains all relevant
                information from the LCAL and SATR to determine the 
                luminosity. This is the only LUMI bank which will be written to
                the disk POTs. All other LCAL and SATR banks will be dropped.
                Side 1 always refers to the side with the tight fiducial cut,
                side 2 is the opposite one.\
                Number of words / row\
                Number of rows'
             STATIC
             = (
                GBxcnt = INTE[0,*]: 'GBX counter',
                HVolt = INTE[*,*]: 'HV bit pattern',
                Trl1 = INTE[*,*]: 'Trigger level 1 bit pattern',
                L2bits = INTE[*,*]: 'Trigger level 2 bit pattern after 
                                appplication of the trigger mask',
                TrEna = INTE[*,*]: 'Trigger enable bit pattern',
                LOle = INTE[*,*]: 'Lole information word, if 0: LOLE okay',
                AccMet = INTE[*,*]: 'Acceptance method bit pattern',
                MinDown = INTE[1,*]: 'Minimum downscale factor',
                EFid = REAL[0.00,300.]: 'Total fiducial LCAL energy (GeV)',
                EC(2) = REAL[0.00,300.]: 'Cluster energies of most energetic
                                cluster on either side (GeV); side 1 = side 
                                with tight fiducial cut, 2 = other side',
                EW(2) = REAL[0.00,300.]: 'Total wire energies on either side
                                (GeV)',
                XC(2) = REAL[-60.00,60.]: 'Cluster x positions (cm)',
                YC(2) = REAL[-60.00,60.]: 'Cluster y positions (cm)',
                TC(2) = REAL[-200.00,200.]: 'Cluster thetas (mrad)',
                PC(2) = REAL[-180.00,180.]: 'Cluster phis (deg)',
                AX(2) = REAL[-1.000,1.]: 'Cluster x asymmetries',
                AY(2) = REAL[-1.000,1.]: 'Cluster y asymmetries',
                XX(2) = REAL[0.00,25.5]: 'Shower width in x (cm)',
                YY(2) = REAL[0.00,25.5]: 'Shower width in y (cm)',
                XY(2) = REAL[-1.000,1.]: 'Shower width correlation',
                XA(2) = REAL[-120.00,120.]: 'Cluster x averages (cm)
                                (x[stack1]+x[stack3]-2*x[stack2])',
                YA(2) = REAL[-120.00,120.]: 'Cluster y averages (cm)
                                (y[stack1]+y[stack3]-2*y[stack2])',
                XD(2) = REAL[-10.00,10.]: 'Cluster x differences (cm)
                                (x[stack3]-x[stack1])',
                YD(2) = REAL[-10.00,10.]: 'Cluster y differences (cm)
                                (y[stack3]-y[stack1])',
                AD(2) = INTE[0,3000]: 'Address of most energetic LCAL tower',
                IT(2) = INTE[0,*]: 'Track information words for either side:
                                bits 0,1 : Track quality; bits 2,3,4,5: NDF
                                of fit; bits 6...15: number of SATR coordinates
                                on side',
                CT(2) = REAL[0.00,*]: 'Track fit chi squares',
                TT(2) = REAL[-100.00,100.]: 'Track thetas (mrad)',
                PT(2) = REAL[-180.00,180.]: 'Track phis (deg)',
                XT(2) = REAL[-10.00,10.]: 'Track vertex x position at z=0 (cm)',
                YT(2) = REAL[-10.00,10.]: 'Track vertex y position at z=0 (cm)',
                E1(2) = REAL[0.000,1.]: 'Energy fraction in stack 1',
                E2(2) = REAL[0.000,1.]: 'Energy fraction in stack 2',
                XL(2) = REAL[-60.00,60.]: 'Cluster x local positions (cm)',
                YL(2) = REAL[-60.00,60.]: 'Cluster y local positions (cm)',
                AI(2) = INTE[0,3000]: 'Address of LCAL intersection tower
                                of highest triplets',
                ES(2) = REAL[0.00,300.]: 'Energies of second most energetic
                                cluster on either side (GeV); side 1 = side 
                                with tight fiducial cut, 2 = other side',
                XS(2) = REAL[-60.00,60.]: 'Second highest cluster x local
                                positions (cm)',
                YS(2) = REAL[-60.00,60.]: 'Second highest cluster y local
                                positions (cm)',
                AS(2) = INTE[0,3000]: 'Address of LCAL intersection tower
                                of highest triplets for second highest
                                cluster')

             ;

     END ESET

 END SUBSCHEMA
     
     

