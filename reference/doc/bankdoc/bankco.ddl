 SUBSCHEMA CobjPOTBanks
 : 'Description of CalObject banks'
 
 AUTHOR   'J. Boucrot,J.F. Grivaz,A.Bonissent'
 REVIEWER 'J.Hilgart'
 VERSION  '2.0'
 DATE     '16/05/91'
 
 DEFINE ESET
 
  PCRL
      :      ' Calobject ReLations \
              Number of words/calobject relation\
              Number of calobject relations'
           STATIC
 
      ;
 
  PFER
      :      ' Fitted track-Ecalobject Relations \
              Number of words/relation\
              Number of relations'
           STATIC
 
      ;
 
  PFHR
      :      ' Fitted track-Hcalobject Relations \
              Number of words/relation\
              Number of relations'
           STATIC
 
      ;
 
  PHER
      :      ' Hcal-Ecalobject Relations \
              Number of words/relation\
              Number of relations'
           STATIC
 
      ;
 
  PHPR
      :      ' Hcalobject-digital Pattern Relations \
              Number of words/relation\
              Number of relations'
           STATIC
 
      ;
 
  PCOB
      :      ' Calorimeter OBject\
              Number of words/calobject \
              Number of calobjects'
           STATIC
 
      ;
 
  PCHY
      :      'Calobject HYpothesis\
              Number of words/hypothesis \
              Number of hypotheses'
           STATIC
 
      = ( BestHyp      = INTE [0,1]        : 'Best Hypothesis flag\
                                              = 0 for best hyp , = 1 otherwise',
          KTyp         = INTE [0,255]      : 'Hypothesis type')
     ;
 
  PCPA
      :      'Calobject  neutral PArticle  \
              Number of words/particle \
              Number of particles'
           STATIC
 
      = ( NAtu         = INTE [0,255]      : 'Nature of Neutral Particle\
                                              Implemented for the moment being:\
                                              1    Isolated gamma\
                                              2    Gamma from multi gammas neutral cluster\
                                              3    Gamma from identified pi 0\
                                              4    Gamma from electron bremstrhalung\
                                              5    Gamma from electromagnetic charged cluster\
                                             10    Unresolved gamma-gamma\
                                             12    Residual electromagnetic energy from neutral cluster\
                                             13    Residual electromagnetic energy from charged cluster\
                                             17    Neutral hadron\
                                             18    Residual hadronic energy from neutral calobject\
                                             19    Residual hadronic energy from charged calobject with no hcal component\
                                             20    Residual hadronic energy from charged calobject with hcal component\
                                             21    contibution from an ecal cluster for which EBNEUT was in error\
                                             22    contibution from an LcObject',
          ENpa   = REAL [-300.00,300.00]   : 'Best estimate of the particle Energy (Gev)\
                                              All known relevant corrections to the energy\
                                              have been applied to get this energy\
                                              For "imbalance particles" (residual energy of calobjects) \
                                              it is defined as :Total calorimetric energy of the calobj.\
                                            -(Sum of energies of identified neutrals+Sum of energies of\
                                             charged particles linked to this calobject\
                                             Due to energy sampling fluctuations , ENpa may therefore\
                                             be NEGATIVE for "imbalance particles"  ',
          TEpa         = REAL [0.,3.1416]  : 'Teta line of flight',
          FIpa         = REAL [0.,6.2833]  : 'Phi line of flight',
          R1esti       = REAL [-10.0,10.0] : 'R1 estimator \
                                             (transverse profile for photons only)\
                                             For residual particles (19 and 20),\
                                             R1 contains the momentum which may differ from E',
          R2esti       = REAL [-10.0,10.0] : 'R2 estimator \
                                             (transverse profile for photons)\
                                             For residual particles (19 and 20),\
                                             R2 contains the sum of momenta of associated tracks')
      ;
 
  PCQA
      :      'Calobject  neutral particle for alpha Qvector from pcpA \
              Number of words/particle \
              Number of particles'
           STATIC
 
      = ( NAtu         = INTE [0,255]      : 'Nature of Neutral Particle\
                                              Implemented for the moment being:\
                                              1    Isolated gamma\
                                              2    Gamma from multi gammas neutral cluster\
                                              3    Gamma from identified pi 0\
                                              4    Gamma from electron bremstrhalung\
                                              5    Gamma from electromagnetic charged cluster\
                                             10    Unresolved gamma-gamma\
                                             12    Residual electromagnetic energy from neutral cluster\
                                             13    Residual electromagnetic energy from charged cluster\
                                             17    Neutral hadron\
                                             18    Residual hadronic energy from neutral calobject\
                                             19    Residual hadronic energy from charged calobject with no hcal component\
                                             20    Residual hadronic energy from charged calobject with hcal component\
                                             21    contibution from an ecal cluster for which EBNEUT was in error\
                                             22    contibution from an LcObject',
          PXpa   = REAL [-300.00,300.00]   : 'Momentum X component (Gev)',
          PYpa   = REAL [-300.00,300.00]   : 'Momentum Y component (Gev)',
          PZpa   = REAL [-300.00,300.00]   : 'Momentum Z component (Gev)',
          ENpa   = REAL [-300.00,300.00]   : 'Particle energy ' )
      ;
 
  PKST
      :      'Pot Electromagnetic calorimeter SToreys\
           Number of words/storey \
           Number of storeys'
         STATIC
 
     =  (  AD          = INTE [1,*]             : 'Storey address, encoded :\
                                                   k+iph*2**2+jth*2**16+idead*2**26\
                                                   Idead=0 or 1, 1=dead storey',
           EcoR        = REAL [0.000,300.000]   : 'Energy in Gev, eventually corrected for the slow control factor (this\
                                                   factor comes from gas variations and is the same throughout one ECAL\
                                                   module)' )
     ;

   PESM
      :   'Pot Electomagnetic calorimeter Sum of energy per Module\
            Number of words/module\
            Number of Modules'
          STATIC

    = (  SEnergy(36)        = REAL [ 0.000,150.000]  : ' Energy in Gev of summed storeys per Module')
    
    ;
 
    PPRL
      :      ' hypothesis Particle ReLations \
              Number of words/particle relation\
              Number of particle relations'
           STATIC
 
      ;
 
 PEMH
      :      'Relation between ECAL and LCAL objects and FKIN tracks \
              Number of words/relation\
              Number of relations'
           STATIC
 
      = (CalObjectnumber  = INTE [1,*] : 'PECO or PEOB number',
         TrackNumber      = INTE [1,*] : 'FKIN number',
         SharedEnergy     = INTE [1,*] : 'Energy FKIN track contributes to \
                                          this Cal Obj in MeV')
      ;

 PHMH
      :      'Relation between HCAL objects and FKIN tracks \
              Number of words/relation\
              Number of relations'
           STATIC
 
      = (CalObjectnumber  = INTE [1,*] : 'PHCO or PHOB number',
         TrackNumber      = INTE [1,*] : 'FKIN number')
      ;
 
  PCAL

      :      'Dst CALorimeter object\
              Number of words/calobject (=12)\
              Number of calobjects'
           STATIC

      = ( NChr         = INTE [1,7]        : 'Number of associated charged tracks',
          NHyp         = INTE [1,31]       : 'Number of hypothesis',
          EClt         = REAL [0.0,200.]   : 'Raw Energy',
          Sta1         = REAL [0.0,1.00]   : 'Proportion of energy in stack 1',
          Sta2         = REAL [0.0,1.00]   : 'Proportion of energy in stack 2',
          Sta3         = REAL [0.0,1.00]   : 'Proportion of energy in stack 3',
          Sta5         = REAL [0.0,1.00]   : 'Proportion of energy in stack 5',
          KDrg         = INTE [0,255]      : 'Region code',
          FlagWords(4) = REAL [*,*]        : 'Words to characterize the Calobject')

      ;

 

  PCST
     :   'ElectromagneticCalorimeter SToreys\
           Number of words/storey (=5)\
           Number of storeys'
         STATIC
     =  (  KSto        = INTE [1,3]        : 'Stack level',
           EcoR        = INTE [0,100000000]: 'Corrected Energy in Kev',
           EDDB        = INTE [1,10000]    : 'Row index of EcalDeadStorey' 
        )
    
     ;

 END ESET
 
 DEFINE RSET
 
   (PCRL [1,1] -> [1,*] PCOB)
     : 'Calobject number';
 
   (PCRL [0,1] -> [0,*] PFRF)
     : 'Row index of Fitted Track';
 
   (PCRL [0,1] -> [1,*] PECO)
     : 'Row index of Electromagnetic Calobject';
 
   (PCRL [0,1] -> [1,*] PHCO)
     : 'Row index of Hadronic Calobject';
 
   (PCRL [0,1] -> [1,*] PPOB)
     : 'Row index of Hadronic Digital Pattern';
 
   (PFER [1,1] -> [0,*] PFRF)
     : 'Row index of Fitted Track';
 
   (PFER [1,1] -> [0,*] PECO)
     : 'Row index of EcalObject';
 
   (PFHR [1,1] -> [0,*] PFRF)
     : 'Row index of Fitted Track';
 
   (PFHR [1,1] -> [0,*] PHCO)
     : 'Row index of HcalObject';
 
   (PHER [1,1] -> [0,*] PECO)
     : 'Row index of EcalObject';
 
   (PHER [1,1] -> [0,*] PHCO)
     : 'Row index of HcalObject';
 
   (PHPR [1,1] -> [0,*] PHCO)
     : 'Row index of HcalObject';
 
   (PHPR [1,1] -> [0,*] PPOB)
     : 'Row index of Hadronic Digital Pattern';
 
   (PCHY [1,1] -> [1,*] PCOB)
     : 'CalObject number';
 
   (PCPA [1,1] -> [0,*] PCHY)
     : 'Row index of CalObject Hypothesis';

   (PKST [1,1] -> [1,*] PECO)
     : 'Row index of Electromagnetic CalObject';
 
   (PPRL [1,1] -> [1,*] PCPA)
     : 'Index of Neutral Particle';
 
   (PPRL [0,1] -> [0,*] PEHY)
     : 'Index of Electromagnetic CalObject Hypothesis';
 
   (PPRL [0,1] -> [0,*] PHHY)
     : 'Index of Hadronic CalObject Hypothesis';
 
   (PPRL [0,1] -> [0,*] PPHY)
     : 'Index of Hadronic Digital Pattern Hypothesis';
 
 
   (PCST [0,1] -> [1,*]PCAL)
     : 'Index of Calobject for a Ecalstorey';
 
   (PCST [0,1] -> [1,*]ETDI)
     : 'Index of a Ecaltower for a Ecalstorey';

 
 
 END RSET
 
 END SUBSCHEMA

 SUBSCHEMA EobjPOTBanks
 : 'Description of Electromagnetic CalObject banks'
 
 AUTHOR   'J. Boucrot,J.F. Grivaz'
 REVIEWER 'J.Hilgart'
 VERSION  '1.3'
 DATE     '20/03/89'
 
 DEFINE ESET
 
  PEOB
      :      ' Electromagnetic calorimeter OBject
             (PEOB and PEOT are replaced by PECO and PEPT ) \
              Number of words/calobject \
              Number of calobjects'
           STATIC
 
      = ( ERaw         = REAL [0.0,300.00] : 'Raw Energy (Gev)',
          Esta1         = REAL [0.0,1.00]   : 'Proportion of energy in stack 1\
                                               "Stack 1" is defined properly for the overlap\
                                               region according to the tower number',
          Esta2         = REAL [0.0,1.00]   : 'Proportion of energy in stack 2\
                                               "Stack 2" is defined properly for the overlap\
                                               region according to the tower number',
          ECorr        = REAL [0.0,300.00] : 'Energy (Gev) corrected for missing storeys\
                                              set to ERaw in JULIA 224',
          KDrg         = INTE [0,255]      : 'Region code ; see note ALEPH # 88-134\
                                              for a detailed description of this code',
          R1esti       = REAL [-10.0,10.0] : 'R1 estimator (longitudinal profile)',
          R2esti       = REAL [-10.0,10.0] : 'R2 estimator (longitudinal profile)')
      ;
 
  PEOT
      :      ' Electromagnetic calObject Topology
             (PEOB and PEOT are replaced by PECO and PEPT ) \
              Number of words/calobject \
              Number of calobjects'
           STATIC
 
      = ( Testa1       = REAL [0.0,3.142]   : 'Theta in stack 1',
          Phsta1       = REAL     [0.,6.284] : 'Phi in stack 1',
          Sigsta1      = REAL [0.0,0.255]     : 'Shower width (radians) in stack 1\
                                              not yet filled in JULIA 224 ',
          Testa2       = REAL [0.0,3.142]   : 'Theta in stack 2',
          Phsta2       = REAL     [0.,6.284] : 'Phi in stack 2',
          Sigsta2      = REAL [0.0,0.255]     : 'Shower width (radians) in stack 2\
                                              not yet filled in JULIA 224 ',
          Testa3       = REAL [0.0,3.142]   : 'Theta in stack 3',
          Phsta3       = REAL     [0.,6.284] : 'Phi in stack 3',
          Sigsta3      = REAL [0.0,0.255]     : 'Shower width (radians) in stack 3\
                                              not yet filled in JULIA 224 \
                                              Warning !! "Stacks 1 , 2 , 3 " in this bank\
                                              are not yet properly defined in the overlap')
      ;
 
 
  PECO
      :      ' Electromagnetic Calorimeter Object
             (PEOB and PEOT are replaced by PECO and PEPT ) \
              Number of words/calobject \
              Number of calobjects'
           STATIC
 
      = ( ERaw         = REAL [0.000,300.000] : 'Raw Energy (Gev)',
          Esta1         = REAL [0.00,1.00]   : 'Proportion of raw energy in stack 1\
                                               "Stack 1" is defined properly for the overlap\
                                               region according to the tower number',
          Esta2         = REAL [0.00,1.00]   : 'Proportion of raw energy in stack 2\
                                               "Stack 2" is defined properly for the overlap\
                                               region according to the tower number',
          THeta        = REAL [0.0000,3.1416]   : 'Theta. The theta, phi of stacks 1 - 3 are weighted by the stack energy',
          PHi          = REAL   [0.0000,6.2833] : 'Phi. The theta, phi of stacks 1 - 3 are weighted by the stack energy',
          ECorr        = REAL [0.000,300.000] : 'Energy (Gev) corrected for geometrical effects (cracks, detector\
                                                 overlap, dead storeys)\
                                                 Set to ERaw in JULIA 224',
          KDrg         = INTE [0,255]      : 'Region code ; see note ALEPH # 88-134\
                                              for a detailed description of this code',
          CCode        = INTE [0,255]      : 'Correction code for the cluster analysis method; \
                                               CCode = \
                                               0 : Standard\
                                               1 : Crack between modules\
                                               2 : Overlap between detectors\
                                               3 : Dead storey',
          RBits        = INTE [0,255]      : 'Relation bits\
                                              RBITS = \
                                              0 : no relations\
                                              1 : charged track relation\
                                              2 : HCAL relation\
                                              3 : Both ch tk and HC relation' )
      ;
 
  PEPT
      :      ' Electromagnetic calorimeter object Phi and Theta values
              (PEOB and PEOT are replaced by PECO and PEPT ) \
              Number of words/calobject \
              Number of calobjects'
           STATIC
 
      = ( Testa12     = REAL [0.000,3.142]  : 'Theta in stacks 1 and 2',
          Phi12       = REAL [0.000,6.284]  : 'Phi in stacks 1 and 2',
          Testa3      = REAL [0.000,3.142]  : 'Theta in stack 3',
          Phi3        = REAL [0.000,6.284]  : 'Phi in stack 3' )
      ;
 
  PEHY
      :      ' Electromagnetic calobject HYpothesis\
              Number of words/hypothesis \
              Number of hypotheses'
           STATIC
 
      = ( BestHyp      = INTE [0,1]        : 'Best Hypothesis flag\
                                              = 0 for best hyp , = 1 otherwise',
          KTyp         = INTE [0,255]      : 'Hypothesis type :\
                                             Coded as :  64 * ITY4 + 16 * ITY3 + 4 * ITY2 + ITY1\
                                             - ITY4 concerns the cluster analysis method\
                                               ITY4 = 0 : Standard\
                                                    = 1 : Crack between modules\
                                                    = 2 : Overlap between detectors\
                                                    = 3 : Dead storey\
                                             - ITY3 concerns the validity of the identification parameter\
                                               ITY3 = 0 : The two identifiers may be used\
                                                    = 1 : Only the first one may be used\
                                                    = 2 : Only the second one may be used\
                                                    = 3 : The two identifiers are uncalculable\
                                             - ITY2 concerns the nature of the hypothesis\
                                               ITY2 = 0 : The cluster is unambiguously identified as an\
                                                          electromagnetic object\
                                                    = 1 : The cluster is unambiguously identified as an\
                                                          hadronic object\
                                                    = 2 : The cluster identification is ambiguous ,the\
                                                          considered hypothesis is : electromagnetic\
                                                    = 3 : The cluster identification is ambiguous , the\
                                                          considered hypothesis is the hadronic one\
                                             - ITY1 concerns the transverse identification\
                                                    It depends of the nature of the hypothesis\
                                                    If ITY2 is 0 or 2 ( electromagnetic ) :\
                                               ITY1 = 0 : Single isolated gamma\
                                                    = 1 : Unresolved multi gammas\
                                                    = 2 : One gamma extracted from a multi gammas\
                                                    = 3 : Two gammas extracted from the cluster\
                                                    If ITY2 is 1 or 3 ( hadronic ) :\
                                               ITY1 = 0 : Purely hadronic cluster\
                                                    = 1 : Mixed cluster\
                                                    = 2 : One identified gamma extracted\
                                                          from the cluster')
 
     ;
 
  PEST
      :      'Pot Electromagnetic calorimeter SToreys\
           Number of words/storey \
           Number of storeys'
         STATIC
 
     =  (  KSto        = INTE [1,3]             : 'Stack level',
           EcoR        = REAL [0.000,300.000]   : 'Energy in Gev, eventually corrected for the slow control factor (this\
                                                   factor comes from gas variations and is the same throughout one ECAL\
                                                   module)' ,
           EDDB        = INTE [1,10000]         : 'Row index of EcalDeadStorey')
     ;

 
 
 END ESET
 
 DEFINE RSET
 
   (PEOT [0,1] -> [1,1] PEOB)
     : 'Row index of Electromagnetic CalObject';
 
   (PEHY [1,1] -> [1,*] PECO)
     : 'Row index of Electromagnetic CalObject';
 
   (PEST [1,1] -> [1,*] PECO)
     : 'Row index of Electromagnetic CalObject';
 
   (PEST [1,1] -> [1,*] ETDI)
     : 'Row index of Ecaltower';
 
   (PECO [1,1] -> [0,*] PCOB)
     :  'CalObject number';
 
 END RSET
 
 END SUBSCHEMA
 
  SUBSCHEMA HobjPOTBanks
 : 'Description of Hadronic CalObject banks'
 
 AUTHOR   'J. Boucrot,J.F. Grivaz,R.Tenchini'
 REVIEWER 'G.Capon'
 VERSION  '1.5'
 DATE     '05/06/90'
 
 DEFINE ESET
 
  PHOB
      :      ' Hadronic calorimeter OBject (PHOB and PHTO are replaced by PHCO ) \
              Number of words/calobject \
              Number of calobjects'
           STATIC
 
      = ( ERaw         = REAL [0.0,300.00] : 'Raw Energy (Gev)',
         Esta1         = REAL [0.0,1.00]   : 'Proportion of energy in stack 1',
          ECorr        = REAL [0.0,300.00] : 'Energy (Gev) corrected for geometrical losses\
                                              ( cracks , spacers and coil )',
          KDrg         = INTE [0,255]      : 'Region code =0 normal\
                                                          =1 overlap\
                                                          =2 above ECAL crack\
                                                          =3 overal and Ecal crack',
          R1esti       = REAL [-10.,10.0]  : 'R1 estimator \
                                              not yet filled in JULIA 224',
          R2esti       = REAL [-10.,10.0]  : 'R2 estimator \
                                              not yet filled in JULIA 224 ')
      ;
 
  PHTO
      :      ' Topology of Hadronic calObject\
              Number of words/calobject \
              Number of calobjects'
           STATIC
 
      = ( Testa1       = REAL [0.0,3.142]   : 'Theta in stack 1',
          Phsta1       = REAL     [0.,6.284] : 'Phi in stack 1',
          Sigsta1      = REAL [0.0,0.255]     : 'Shower width (radians) in stack 1',
          Testa2       = REAL [0.0,3.142]   : 'Theta in stack 2',
          Phsta2       = REAL     [0.,6.284] : 'Phi in stack 2',
          Sigsta2      = REAL [0.0,0.255]     : 'Shower width (radians) in stack 2')
      ;
 
  PHCO
      :      ' Hadron Calorimeter Object (PHOB and PHTO are replaced by PHCO ) \
              Number of words/calobject \
              Number of calobjects'
           STATIC
 
      = ( ERaw         = REAL [0.00,300.00] : 'Raw Energy (Gev)',
          THeta        = REAL [0.000,3.142]   : 'Theta',
          PHi          = REAL   [0.000,6.284] : 'Phi',
          ECorr        = REAL [0.00,300.00] : 'Energy (Gev) corrected for geometrical losses\
                                              ( cracks , spacers and coil )',
          KDrg         = INTE [0,255]      : 'Region code =1 in overlap region\
                                              = 0 otherwise',
          CCode        = INTE [0,255]      : 'Correction code \
                                              (not filled in JULIA 224)',
          RBits        = INTE [0,255]      : 'Relation bits\
                                              RBITS = \
                                              0 : no relations\
                                              1 : charged track relation\
                                              2 : ECAL relation\
                                              3 : Both ch tk and EC relation' ,
         NoiseFlag    = INTE [0,64]        : 'Noise Flag from Topological Analysis\
                                              and Digital Pattern matching\
                                              bits 0-4 Top. An. (0=OK,1=Noise)\
                                              bit  5 Dig. Matching (0=OK,1=Noise)')
 
      ;
 
  PHHY
      :      ' Hadronic calobject HYpothesis ( Booked , but not yet filled in JULIA 224 )\
              Number of words/hypothesis \
              Number of hypotheses'
           STATIC
 
      = ( BestHyp      = INTE [0,1]        : 'Best Hypothesis flag ( not yet filled in JULIA 224)\
                                              = 0 for best hyp , = 1 otherwise',
          KTyp         = INTE [0,255]      : 'Hypothesis type ( Not yet filled in JULIA 224 )')
     ;
 
 PHST
      :      'P.o.t. Hcal STorey data \
              Number of words/storey\
              Number of fired storeys'
           STATIC
 
      = (ThetaIndex   = INTE [1,190]       : 'theta index (+128 if stack 2) ',
         PhiIndex     = INTE [1,96]        : 'phi index',
         CorrectEnerg = REAL [-1.,100.00]  : 'corrected energy (Gev)')
      ;
  PPOB
      :      ' hadronic digital Pattern OBject\
              Number of words/Digital Pattern \
              Number of Digital Patterns'
           STATIC
 
 
 
      = ( DIrect       = INTE [-1,5]      : 'Direction index',
          DEnerg       = REAL [0.0,300.00] : 'Digital Energy (Gev)',
          Cor1        = REAL [-400.,400.] : ' (Cor1,DirectionIndex)\
                                                 (R,1) (Z,2-5)',
          Cor2        = REAL [-500.,500.] : ' (Cor2,DirectionIndex)\
                                                 (PHI,1) (Y,2-3)\
                                                 (S-up,4) (S-down,5)',
          IPlane       = INTE [0,24]      : 'Interaction Plane',
          FPlane       = INTE [0,24]      : 'First Plane',
          LPlane       = INTE [0,24]      : 'Last Plane',
          MDista       = REAL [0.0,500.]      : 'Maximum Distance',
          PDensi       = REAL [0.0,100.]      : 'Pattern Density')
      ;
 
  PPDS
 
      :      'Pot Pattern DiScriminant analysis\
               Number of words/layer\
               Number of layers'
 
         STATIC
 
 
 
     =  (NumLayers     = INTE [0,25]       : 'Number of current layer',
           FLengt      = REAL [0.0,500.]      : 'Fired Length',
           MDista      = REAL [0.0,500.]      : 'Maximum Distance')
     ;
 
  PPHY
      :      ' hadronic digital Pattern HYpothesis\
              Number of words/hypothesis \
              Number of hypotheses'
           STATIC
 
      = ( BestHyp      = INTE [0,1]        : 'Best Hypothesis flag\
                                              = 0 for best hyp , = 1 otherwise',
          KTyp         = INTE [0,255]      : 'Hypothesis type')
     ;
 
 
 END ESET
 
 
 DEFINE RSET
 
   (PHTO [1,1] -> [1,1] PHOB)
     : 'Row index of Hadronic CalObject';
 
   (PHHY [1,1] -> [1,*] PHCO)
     : 'Row index of Hadronic CalObject';
 
   (PHST [1,1] -> [1,*] PHCO)
     : 'Row index of Hadronic CalObject';
 
   (PHCO [1,1] -> [0,*] PCOB)
     :  'CalObject number';
 
   (PPHY [1,1] -> [1,*] PPOB)
     : 'Row index of Hadronic Digital Pattern';
 
   (PPDS [1,1] -> [1,*] PPOB)
     : 'Row index of Hadronic Digital Pattern';
 
 
 END RSET
 
 END SUBSCHEMA
 
 SUBSCHEMA EflowJULPOTBanks
 : 'Description of EFLOW banks'
 
 AUTHOR   'M.N.Minard,M.Pepe,P.Janot'
 REVIEWER 'F.Ranjard'
 VERSION  '1.6'
 DATE     '27/08/92'
 
 DEFINE ESET
 
  
 
  ECTY
      :      'Definition of Ecal cluster type
              NR=0. (JUL)\
              Number of words/cluster\
              Number of Ecal clusters'
           STATIC
 
      = (ClusterType  = INTE  [-1,4]        : '-1 = isolated cell\
                                               0  = gamma or multigamma\
                                               1  = not elm. neutral\
                                               2  = isolated electron\
                                               3  = not isolated electron\
                                               4  = other charged cluster')
                                                 
      ;
        
 
                                                                
  EMSK 
      :     'Energy found inside inner and outer road per stack
             NR=0. (JUL)\
             Number of words per extrapolated TPC fitted track \
             Number of extrapolated TPC fitted tracks '
         STATIC
 
      = (E1(15)    = REAL [0.,*]      : '  Element E1+3*(i-1)  : energy in GeV in inner road per stack i\
                                          Element E1+3*(i-1)+1: theta in inner road per stack i\
                                          Element E1+3*(i-1)+2: phi in inner road per stack i',
         E2(15)    = REAL [0.,*]      : '  Element E2+3*(i-1)  : energy in GeV in outer road per stack i\
                                          Element E2+3*(i-1)+1: theta in outer road per stack i\
                                          Element E2+3*(i-1)+2: phi in inner road per stack i')
      ;
  
  ECTE
      :     'Masked Ecal storeys
             NR=0. (POT)\
             Number of words per masked storey \
             Number of masked Ecal storeys '
 
           STATIC
 
      =  (CA       = INTE [1,221184]  : ' Coded i,j,k address (as in ESDA)',
          CE       = REAL [0.,50.]    : ' Masked energy in Gev')
      ; 
 
  HCTE
      :     'Masked Hcal storeys
             NR=0. (POT)\
             Number of words per masked storey \
             Number of masked Hcal storeys '
 
           STATIC
 
      =  (CA       = INTE [0,*]      : ' Coded i,j,k address \
                                          (j-1)*256+(i-1)*2+k-1',
         CE       = REAL [0.,50.]    : ' Masked energy in Gev')
      ;  
   
  PRTM
      :     'Fitted TPC tracks to masks relation
             NR=0. (POT)\
             Number of words per TPC fitted track \
             Number of extrapolated TPC fitted tracks) '
           STATIC
 
      = ( MA     = INTE [0,100]  : 'Address in PMSK',
          CT     = INTE [0,2]    : 'Particle type : \
                                                    0 = hadron\
                                                    1 = electron\
                                                    2 = muon')            
      ;  
   
 
  PMSK
      :   'MaSKs used in energy flow analysis
           NR=0. (POT)\ 
           Number of words/track\
           Number of extrapolated tracks'
         STATIC
 
     =  (  KTyp        = INTE [0,255]      : 'Subtraction type',
           EMask       = REAL [0.,300.00]  : 'Energy (Gev) in mask',
           Fsta12      = REAL [0.,1.00]    : 'Fraction in stack 1+2',
           Fsta3       = REAL [0.,1.00]    : 'Fraction in stack 3',
           Teta12      = REAL [0.,3.142]    : 'Theta in stack 1+2',
           Phi12       = REAL     [0.,6.284] : 'Phi in stack 1+2',
           Teta3       = REAL [0.,3.142]    : 'Theta in stack 3',
           Phi3        = REAL     [0.,6.284] : 'Phi in stack 3',
           TetaH       = REAL [0.,3.142]    : 'Theta in hcal',
           PhiH        = REAL     [0.,6.284] : 'Phi in hcal')
     ;
 
                                                                                
EFOL                                                                            
      :       'Energy FlOw eLements\                                            
               Number of words/element\                                         
               Number of elements'                                              
      SIZE 1,1                                                                  
      STATIC                                                                    
      = (PX      = REAL [-999.,999.]       :  'Weighted component x',           
         PY      = REAL [-999.,999.]       :  'Weighted component y',           
         PZ      = REAL [-999.,999.]       :  'Weighted component z',           
         EnergyW = REAL [-999.,999.]       :  'Element weighted by E-Flow coefficients',            
         WEight  = REAL [0.,99.]           :  'Weight applied to E-Flow element',               
         TYpe    = INTE [0,10]             :  'Object type\                     
                                               0 = Track\                       
                                               1 = Electron\                        
                                               2 = Muon\                    
                                               3 = Track from V0\               
                                               4 = Electromagnetic\             
                                               5 = Ecal hadron/residu\          
                                               6 = Hcal element\                
                                               7 = Lcal element',                
         LinkEcal= INTE [0,999]            :  'Peco # associated',               
         LinkTrak= INTE [0,999]            :  'Track # associated',              
         LinkHcal= INTE [0,999]            :  'Phco # associated',               
         LinkCalo= INTE [0,999]            :  'Calobject # associated',          
         LinkJet = INTE [0,100]            :  'Jet # associated')                
                                                                                
      ;                                                                         
 
 
 EJET
      :      'Eflow  JET bank\
              Number of words/Jet\
              Number of Jets'
           STATIC
 
      = (PX             = REAL   : 'X component of jet(Gev) ',
         PY             = REAL   : 'Y component of jet(Gev) ',
         PZ             = REAL   : 'Z component of jet(Gev) ',
         PE             = REAL   : 'energy of jet     (GEV) ' )
     ;
 
 
EAUX
     : 'Information from ENFLW \
        Number of words \
        Number of rows  (=1)'
        STATIC
 
     = (TR              = INTE [0,*]   :'Trigger information',
        CA              = INTE [0,*]   :'Number of ECAL/HCAL objects',
        HV              = INTE [0,1]   :'HV status of the calorimeters',
        HE              = REAL [0.,*]  :'HCAL energy validated by ENFLW',
        HK              = REAL [0.,*]  :'HCAL fake energy, thus killed',
        E2              = REAL [0.,*]  :'Energy measured below 12 degrees')
      ;
 
 
 
 END ESET
 
 
 DEFINE RSET
 
   (PMSK [1,1] -> [0,1] PFRF)
         : 'Index in PFRF of the fitted track';
 
 END RSET

 
 END SUBSCHEMA
 

 
 SUBSCHEMA ElidJULPOTBanks
 
 AUTHOR   'D.Pallin'
 REVIEWER 'F.Ranjard'
 VERSION  '2.3'
 DATE     '15/01/90'
 
 DEFINE ESET

 E4DE
       :    'dead storeys pattern bank for electron identification.
             NR=0 (JUL)\
             Number of words/track\    
             Number of FRFT tracks'

       = (IK1(3)      = INTE [0,1]         : 'pattern for each ecal stack 0=dead storeys')

         STATIC
  ;

 EIDT
      :      'Electron IDenTification.
              NR=0 (JUL) \
              Number of words/track associated with an ecal cluster\
              Number of associated tracks '
           STATIC
 
      = (Iflag        = INTE [0,255]       : 'quality flag for identification\
                                              =0 o.k.\
                                              =1 estimation done in crack region\
                                              =2 estimation done in overlap region',
         R1esti       = REAL [*,*]         : 'R1 estimator(energy balance)\
                                              R1=(E_cluster-P_track)/reso(E,P)',
         R2esti       = REAL [*,*]         : 'R2 estimator(compactness)\
                                              R2=(X-X0)/sigma(X)\
                                              with X=(E4(1)+E4(2)+E4(3))/P_track\
                                                   X0=.825 and E4(i) being the sum of\
                                              4 storeys centered on the track
                                              extrapolation for stack i',
         R3esti       = REAL [*,*]         : 'R3 estimator(longitud. profile)\
                                              R3=(a-a0)/sigma(a)\
                                              a being the first parameter of the
                                              longitudinal profil equation:\
                                              dE/DS=S**(-1+1/b)*exp(-aS/b)',
         R4esti       = REAL [*,*]         : 'R4 estimator(longitud. profile)\
                                              R4=(b-b0)/sigma(b)',
         R5esti       = REAL [*,*]         : 'R5 estimator(de/dx)\
                                              R5=I-I0/SIGMA(I)\
                                              I = ionisation value given by TPC track\
                                              I0 = expected ionisation value for an electron',
         R6esti       = REAL [*,*]         : 'R6 estimator(dteta barycenter)\
                                              (not yet filled in julia 224',
         R7esti       = REAL [*,*]         : 'R7 estimator(dphi barycenter)\
                                              (not yet filled in julia 224',
         ECorr        = REAL [*,*]         : 'corrected energy with elec. hyp.\
                                              (not yet totaly define)',
         IPpoth       = INTE [0,255]       : 'Particle hypothesis \
                                              deduce from estimators values\
                                              R2 >-3. ; -2.4< R3 < 3.\
                                              =0 if "non electron"\
                                              =1 if electron',
         E1           = REAL [*,*]         : 'raw energy in centered storeys stack 1\
                                              definition of stacks non standard for overlap region',
         E2           = REAL [*,*]         : 'raw energy in centered storeys stack 2',
         E3           = REAL [*,*]         : 'raw energy in centered storeys stack 3')
                                              ;
 
 PETB
       :    'Electron To Bremsstrahlung relation
             NR=0. (POT)\ 
             Number of words/ Brem relation\
             Number of Brem relations'
         STATIC
  ;
 
 PEBR
       :    'Electron data corrected for Bremsstrahlung 
             NR=0. (POT)\ 
             Number of words/ electron with Brem\
             Number of electrons with Brem'
 
       = (PXele       = REAL [*,*]         : 'Px of electron at first measured point',
          PYele       = REAL [*,*]         : 'Py of electron at first measured point',
          PZele       = REAL [*,*]         : 'Pz of electron at first measured point')
         STATIC
  ;
 
 PEID
      :      'Electron IDentification compressed bank
              NR=0. (POT) \
              Number of words/track associated with an ecal cluster\
              Number of associated track '
           STATIC
 
      = (Iflag        = INTE [0,255]       : 'quality flag for identification',
         R1esti       = INTE [-127,128]    : 'R1 estimator ( energy balance )
                                              value=128 if bad calculation',
         R2esti       = INTE [-127,128]    : 'R2 estimator(transverse profile)
                                              value=128 if bad calculation',
         R3esti       = INTE [-127,128]    : 'R3 estimator(longitud. profile)
                                              value=128 if bad calculation',
         R4esti       = INTE [-127,128]    : 'R4 estimator(longitud. profile)
                                              value=128 if bad calculation',
         R6esti       = INTE [-127,128]    : 'R6 estimator(dteta barycenter)
                                              value=128 if bad calculation',
         R7esti       = INTE [-127,128]    : 'R7 estimator( dphi barycenter)
                                              value=128 if bad calculation',
         ECorr        = INTE [0,65535]     : 'corrected energy with elec. hyp.',
         ETot         = INTE [0,65535]     : 'Total energy in 4 centered storeys
                                              istack1  + stack2  + stack 3',
         P1           = INTE [0,255]       : 'proportion of energy in 4 centered storeys
                                              stack1 / total energy',
         P2           = INTE [0,255]       : 'proportion of energy in 4 centered storeys
                                              stack2 / total energy')
       ;
 
 END ESET
 
 DEFINE RSET
 
       (EIDT [1,1] -> [0,1] FRFT)
         : 'Row index in FRFT of the fitted track';
 
       (EIDT [1,1] -> [1,1] PECO)
         : 'Row index in PECO of the electromagnetic calobject';
 
   (PEID [1,1] -> [0,1] PFRF)
     : 'Row index in PFRF of the fitted track';

   (PETB [1,1] -> [0,*] PEBR)
     : 'Row index of electron with Bremsstrahlung correction in PEBR';
 
   (PEBR [1,1] -> [0,1] PEID)
     : 'Row index of electron in bank PEID';

   (PETB [1,1] -> [0,*] PCPA)
     : 'Row index of Bremsstrahlung photon in PCPA';
 
 
    END RSET
 
 END SUBSCHEMA


 SUBSCHEMA CobjJULBanks
 : 'Description of CalObject banks'

 AUTHOR   'A. Bonissent,R.Tenchini'
 REVIEWER 'F.Ranjard'
 VERSION  '1.5'
 DATE     '30/04/91'

 DEFINE ESET


 CALO
      :      'Calorimeter Object\
              Number of words/relation\
              Number of relations'
           STATIC

      = ( EnergyRaw  = REAL [-45.,45.]   : 'Raw Energy')
      ;

 CPAR
      :      'Calorimeter Particle\
              Number of words/particle\
              Number of particles'
           STATIC

      = ( NAtu         = INTE [0,255]      : 'Nature',                                                                              
          ENpa         = REAL [-300.00,300.00]   : 'Energy (Gev)',
          TEpa         = REAL [0.,3.1416]  : 'Teta line of flight',                                                                 
          FIpa         = REAL [-3.1416,3.1416] : 'Phi line of flight',                                                              
          R1esti       = REAL [-10.0,10.0] : 'R1 estimator %                                                                        
                                             (transverse profile for photons)',                                                     
          R2esti       = REAL [-10.0,10.0] : 'R2 estimator %                                                                        
                                             (transverse profile for photons)')                                                     
      ;                                                                                                                             
                                                                                                                                    
 CHPR
      :      ' Calorimeter hypothesis Particle ReLations \
              Number of words/particle relation\
              Number of particle relations'
           STATIC
 
      ;

 CHYP                                                                                                                              
      :      'Calobject HYpothesis\
              Number of words/hypothesis \                                                                                          
              Number of hypotheses'                                                                                                 
           STATIC                                                                                                                   
                                                                                                                                    
      = ( BestHyp      = INTE [0,1]        : 'Best Hypothesis flag\
                                              = 0 for best hyp , = 1 otherwise',
          KTyp         = INTE [0,255]      : 'Hypothesis type')
     ;                                                                                                                              
                                                                                                                                    
 CEXT                                                                           
       :     'Calorimeter EXTended object. 
              NR=number of EXT calobj (JUL)\             
              Number of words/excob element\                                    
              Number of excob elements'                                         
           STATIC                                                               
                                                                                
       = (CextAddress   = INTE [10000,*]    : 'CEXT address of\                 
                                               pattern + 10000\                 
                                               ECOB    + 20000\                 
                                               HCLU    + 30000')                
       ;                                                                        
                                                                                                                                    
 CHRL                                                                                                                              
      :      ' Calobject Hpatt ReLations \
              Number of words/calobject relation\
              Number of calobject relations'                                                                                        
           STATIC                                                                                                                   
                                                                                                                                    
      ;                                                                                                                             
                                                                                                                                    
                                                                                                                                    
 END ESET

 DEFINE RSET

   (CHYP [1,1] -> [1,*] CALO)                                                                                                       
     : 'Index of CalObject';                                                                                                        

   (CPAR [1,1] -> [0,*] CHPR)                                                                                                       
     : 'Index of CalObject Relations';
                                                                                                                                    
   (CHPR [1,1] -> [1,*] CPAR)
     : 'Index of Neutral Particle';
 
   (CHPR [0,1] -> [0,*] EHYP)
     : 'Index of Electromagnetic CalObject Hypothesis';
 
   (CHPR [0,1] -> [0,*] PHHY)
     : 'Index of Hadronic CalObject';
 
   (CHPR [0,1] -> [0,*] PPHY)
     : 'Index of Hadronic Digital Pattern';
 
   (CHRL [1,1] -> [1,*] CALO)                                                                                                       
     : 'Index of  Calobject';                                                                                                       
                                                                                                                                    
   (CHRL [0,1] -> [1,*] ECOB)                                                                                                       
     : 'Index of Electromagnetic Calobject';                                                                                        
                                                                                                                                    
   (CHRL [0,1] -> [1,*] HCLU)                                                                                                       
     : 'Index of Hadronic Calobject';                                                                                               
                                                                                                                                    
   (CHRL [0,1] -> [1,*] HPCO)
     : 'Index of Hadronic Digital Pattern';                                                                                         
                                                                                                                                    
   (CHRL [0,1] -> [0,*] FRFT)                                                                                                       
     : 'Index of Fitted Track';                                                                                                     

 END RSET

 END SUBSCHEMA


