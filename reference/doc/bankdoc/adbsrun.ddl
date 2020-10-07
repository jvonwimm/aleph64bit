 SUBSCHEMA CaloRunConsts
 : 'Description of Calorimeters (EC and LC) Run constants'
 
 AUTHOR   'A. Bonissent'
 REVIEWER 'B.Bloch-Devaux  '
 VERSION  '2.0'
 DATE     '06/10/89'
 
 DEFINE ESET
 
 ECMC
      :      'Ec Module Correction (STC)\
              Number of words/module\
              Number of modules = 36 '
 
       STATIC
      = (Correction   = REAL [0.00,2.0000] : 'Correction factor to the energy',
         PlatinePeak  = REAL [0.00,256.00] : 'Peak position on the Fe55 platine',
         Pressure     = REAL [800.,1050.00]: 'Pressure inside the module (millibars)',
         Temperature  = REAL [280.,310.00] : 'Temperature of the module (Kelvin)')
      ;
 
 LCMC
      :      'Lcal Module Correction(STC)\
              Number of words/module\
              Number of modules = 4 '
              STATIC
 
      = (Correction   = REAL [0.00,2.0000] : 'Correction factor to the energy',
         PlatinePeak  = REAL [0.00,256.00] : 'Peak position on the Fe55 platine',
         Pressure     = REAL [800.,1050.00]: 'Pressure inside the module (millibars)',
         Temperature  = REAL [280.,310.00] : 'Temperature of the module (Kelvin)')
;
 END ESET
 
 END SUBSCHEMA


 SUBSCHEMA EcalCalConsts
 : 'Description of EC calibration constants per module'

 AUTHOR   'B.Bloch-Devaux,M.N.Minard'
 REVIEWER 'M.Minard'
 VERSION  '2.0'
 DATE     '26/10/98'

 DEFINE ESET

 ECCA
     :  'ECAL Calibration constants (STC)
              NR=setup code or run number \
          Number of words/module   \
          Number of modules '
         STATIC
      = (  CalibPad  = REAL [0.,2.0000] : 'Calibration constant for pads ',
           CalibWire = REAL [0.,2.000]  : 'Calibration constant for wires')
     ;

 EWGA
      : 'Ec Wire gain values (STC)\
         Number of words/module\
         Number of modules = 36 '
 
       STATIC
        = (COnstants(4,53)   = INTE [0,4095]   : 'gain8 ped8 gain 1 ped1 ofmodule 53 ADCs')
      ;
                                                                                
 ECGF                                                                           
      : 'Ecal Correction for Gas Flow effect  \                            
         Number of words/Module\                                           
         Number of Module'                                                 
           STATIC                                                               
                                                                                
      = (CorrectionModule = REAL   : 'Correction for module')                   
     ;                                                                          
                                                                                
 EHGF                                                                           
      : 'Ecal History for Gas Flow effect \                                
         Number of words/Module\                                           
         Number of Module'                                                 
           STATIC                                                               
                                                                                
      = (CorrectionModule = REAL   : 'Correction for module')                   
     ;                                                                          
                                                                                
 ELZE                                                                           
      : 'ELectron ZEro suppression correction                              
           NR=setup code or run number \
         Number of words/Teta intervall\                                   
         Number of Intervall'                                              
           STATIC                                                               
                                                                                
      = (TowermiN       = REAL   : 'Tower miNimum range',                       
         TowermaX       = REAL   : 'Tower maXimum in range',                    
         F4             = REAL   : 'Fraction of energy in 4 towers',            
         GeomFactor     = REAL   : 'Geometry factor',                           
         Correction1    = REAL   : 'Correction factor 1 of zero suppression',               
         Correction2    = REAL   : 'Correction factor 2 of zero suppression')                
     ;                                                                          
                                                                                
 ECIL                                                                           
      : 'Electron Correction for Ionisation loss                          
           NR=setup code or run number \
         Number of words/region\                                           
         Number of region'                                                 
           STATIC                                                               
                                                                                
      = (RegionNumber   = INTE   : 'Region Number \                             
                                    (3=endcap , 0=barrel)',                      
         Correction1    = REAL   : 'Correction factor 1 for ionisation loss',                  
         Correction2    = REAL   : 'Correction factor 2 for ionisation loss')                   
     ;                                                                          
                                                                                
 EF4N                                                                           
      : 'Gamma F4 Normalisation parameters                                
         normalise fraction of energy in 4 towers                         
         NR=setup code or run number \
         Number of words/Teta intervall\                                   
         Number of Intervall'                                              
           STATIC                                                               
                                                                                
      = (MinThetarange  = REAL   : 'Theta range lower bound ',                  
         maXThetarange  = REAL   : 'Theta range upper bound ',                  
         Parameter1     = REAL   : 'Parameter 1 for function normalisation',                   
         Parameter2     = REAL   : 'Parameter 2 for function normalisation',                   
         Parameter3     = REAL   : 'Parameter 3 for function normalisation',                   
         Parameter4     = REAL   : 'Parameter 4 for function normalisation',                   
         Parameter5     = REAL   : 'Parameter 3 for function normalisation',                   
         Parameter6     = REAL   : 'Parameter 4 for function normalisation',                   
         Sigmaparam1    = REAL   : 'Sigma of parameter 1',                      
         Sigmaparam2    = REAL   : 'Sigma of parameter 2',                      
         Sigmaparam3    = REAL   : 'Sigma of parameter 3',                      
         Sigmaparam4    = REAL   : 'Sigma of parameter 4',                      
         Sigmaparam5    = REAL   : 'Sigma of parameter 5',                      
         Sigmaparam6    = REAL   : 'Sigma of parameter 6')                      
      ;

 EW1N                                                                           
      : 'Gamma W1 Normalisation parameters                                
         normalise W1 parameter of Bulos                                  
         NR=setup code or run number \
         Number of words/Teta intervall\                                   
         Number of Intervall'                                              
           STATIC                                                               
                                                                                
      = (MinThetarange  = REAL   : 'Theta range lower bound ',                  
         maXThetarange  = REAL   : 'Theta range upper bound ',                  
         Parameter1     = REAL   : 'Parameter 1 for function normalisation',                   
         Parameter2     = REAL   : 'Parameter 2 for function normalisation',                   
         Parameter3     = REAL   : 'Parameter 3 for function normalisation',                   
         Parameter4     = REAL   : 'Parameter 4 for function normalisation',                   
         Parameter5     = REAL   : 'Parameter 3 for function normalisation',                   
         Parameter6     = REAL   : 'Parameter 4 for function normalisation',                   
         Sigmaparam1    = REAL   : 'Sigma of parameter 1',                      
         Sigmaparam2    = REAL   : 'Sigma of parameter 2',                      
         Sigmaparam3    = REAL   : 'Sigma of parameter 3',                      
         Sigmaparam4    = REAL   : 'Sigma of parameter 4',                      
         Sigmaparam5    = REAL   : 'Sigma of parameter 5',                      
         Sigmaparam6    = REAL   : 'Sigma of parameter 6')                      
      ;

 EW2N                                                                           
      : 'Gamma W2 Normalisation parameters                                
         normalise W2 parameter of Bulos                                  
         NR=setup code or run number \
         Number of words/Teta intervall\                                   
         Number of Intervall'                                              
           STATIC                                                               
                                                                                
      = (MinThetarange  = REAL   : 'Theta range lower bound ',                  
         maXThetarange  = REAL   : 'Theta range upper bound ',                  
         Parameter1     = REAL   : 'Parameter 1 for function normalisation',                   
         Parameter2     = REAL   : 'Parameter 2 for function normalisation',                   
         Parameter3     = REAL   : 'Parameter 3 for function normalisation',                   
         Parameter4     = REAL   : 'Parameter 4 for function normalisation',                   
         Parameter5     = REAL   : 'Parameter 3 for function normalisation',                   
         Parameter6     = REAL   : 'Parameter 4 for function normalisation',                   
         Sigmaparam1    = REAL   : 'Sigma of parameter 1',                      
         Sigmaparam2    = REAL   : 'Sigma of parameter 2',                      
         Sigmaparam3    = REAL   : 'Sigma of parameter 3',                      
         Sigmaparam4    = REAL   : 'Sigma of parameter 4',                      
         Sigmaparam5    = REAL   : 'Sigma of parameter 5',                      
         Sigmaparam6    = REAL   : 'Sigma of parameter 6')                      
      ;

 EL1N                                                                           
      : 'Gamma 1st longitudinal estimators Normalisation                  
         normalise 1st longitudinal estimator EBNEUT                      
         NR=setup code or run number \
         Number of words/Teta intervall\                                   
         Number of Intervall'                                              
           STATIC                                                               
                                                                                
      = (MinThetarange  = REAL   : 'Theta range lower bound ',                  
         maXThetarange  = REAL   : 'Theta range upper bound ',                  
         Parameter1     = REAL   : 'Parameter 1 for function normalisation',                   
         Parameter2     = REAL   : 'Parameter 2 for function normalisation',                   
         Parameter3     = REAL   : 'Parameter 3 for function normalisation',                   
         Parameter4     = REAL   : 'Parameter 4 for function normalisation',                   
         Parameter5     = REAL   : 'Parameter 3 for function normalisation',                   
         Parameter6     = REAL   : 'Parameter 4 for function normalisation',                   
         Sigmaparam1    = REAL   : 'Sigma of parameter 1',                      
         Sigmaparam2    = REAL   : 'Sigma of parameter 2',                      
         Sigmaparam3    = REAL   : 'Sigma of parameter 3',                      
         Sigmaparam4    = REAL   : 'Sigma of parameter 4',                      
         Sigmaparam5    = REAL   : 'Sigma of parameter 5',                      
         Sigmaparam6    = REAL   : 'Sigma of parameter 6')                      
      ;

 EL2N                                                                           
      : 'Gamma 2nd Longitudinal estimators Normalisation                  
         normalise 2nd longitudinal estimator EBNEUT                      
         NR=setup code or run number \
         Number of words/Teta intervall\                                   
         Number of Intervall'                                              
           STATIC                                                               
                                                                                
      = (MinThetarange  = REAL   : 'Theta range lower bound ',                   
         maXThetarange  = REAL   : 'Theta range upper bound ',                  
         Parameter1     = REAL   : 'Parameter 1 for function normalisation',                   
         Parameter2     = REAL   : 'Parameter 2 for function normalisation',                   
         Parameter3     = REAL   : 'Parameter 3 for function normalisation',                   
         Parameter4     = REAL   : 'Parameter 4 for function normalisation',                   
         Parameter5     = REAL   : 'Parameter 3 for function normalisation',                   
         Parameter6     = REAL   : 'Parameter 4 for function normalisation',                   
         Sigmaparam1    = REAL   : 'Sigma of parameter 1',                      
         Sigmaparam2    = REAL   : 'Sigma of parameter 2',                      
         Sigmaparam3    = REAL   : 'Sigma of parameter 3',                      
         Sigmaparam4    = REAL   : 'Sigma of parameter 4',                      
         Sigmaparam5    = REAL   : 'Sigma of parameter 5',                      
         Sigmaparam6    = REAL   : 'Sigma of parameter 6')                      
     ;                                                                          
                                                                                
 EGTE                                                                           
      : 'Electromagnetic Gamma ThrEshold                                  
          NR=setup code or run number \
         Number of words/Gamma type\                                       
         Number of gamma type'                                             
           STATIC                                                               
                                                                                
                                                                                
      = (EnergyThreshold= REAL   : 'Cluster minimum energy',                    
         TYpecluster    = INTE   : 'Cluster Type used to build\                 
                                    EGNC bank : 1 =Neutral cluster\             
                                                2 =Charged cluster')            
     ;                                                                          
                                                                                
 EGCO                                                                           
      : 'Electromagnetic Gamma COrrection  \                               
         Number of words/Angular range\                                    
         Number of angular range'                                          
           STATIC                                                               
                                                                                
      = (ThetaMinimum     = REAL   : 'Mininum cosine theta range',            
         ThetamaXimum     = REAL   : 'Maximum cosine theta range',             
         Coefficient1     = REAL   : 'Coefficient 1 to PECO/PGPC adjustement',                            
         Coefficient2     = REAL   : 'Coefficient 2 to PECO/PGPC adjustement',                            
         Coefficient3     = REAL   : 'Coefficient 3 to PECO/PGPC ajustement',                            
         Coefficient4     = REAL   : 'Coefficient 4 to PECO/PGPC adjustement')                            
     ;                                                                          
                                                                                
 ENNO                                                                           
      : 'Electromagnetic Noise NOrmalisation\
         Number of words/Zone intervall\
         Number of Zone'                                                   
           STATIC                                                               
                                                                                
                                                                                
      = (REgionnumber   = INTE   : 'Region number',                             
         Stack1         = REAL   : 'Normalisation Stack1  ',                    
         Stack2         = REAL   : 'Normalisation Stack2 ',                     
         Stack3         = REAL   : 'Normalisation Stack3 ')                     
     ;                                                                          
                                                                                
 EGOZ                                                                           
      : 'Electromagnetic gamma cOrreCtion of Zero suppression\             
         Number of words/Angular range\                                    
         Number of angular range'                                          
           STATIC                                                               
                                                                                
      = (ThetaMinimum     = REAL   : 'Mininum cosine theta range ',            
         ThetamaXimum     = REAL   : 'Maximum cosine theta range ',             
         Coefficient1     = REAL   : 'Coefficient 1 to PECO/PGAC adjustement ',              
         Coefficient2     = REAL   : 'Coefficient 2 to PECO/PGAC adjustement ',              
         Coefficient3     = REAL   : 'Coefficient 3 to PECO/PGAC adjustement ',              
         Coefficient4     = REAL   : 'Coefficient 4 to PECO/PGAC adjustement ',              
         Coefficient5     = REAL   : 'Coefficient 5 to PECO/PGAC adjustement ')              
     ;                                                                          
                                                                                
    EDPA
         :    'Ecal Digital PArameter \
               Number of constant \
               Number of rows(=1)'
         SIZE 1,1
         STATIC
 
         = (Noisestack1     = REAL [*,*] : ' Tower noise conversion factor for stack1 (Kev/fc)',
            Noisestack2     = REAL [*,*] : ' Tower noise conversion factor for stack2(kev/fc)',
            Noisestack3     = REAL [*,*] : ' Tower noise conversion factor for stack3 (kev/fc)',
            Gainstack1      = INTE [*,*] : ' Gain for stack1 Kev/Channel ',
            Gainstack2      = INTE [*,*] : ' Gain for stack2 Kev/Channel ',
            Gainstack3      = INTE [*,*] : ' Gain for stack3 Kev/Channel ',
            Errorgains1     = REAL [*,*] : ' Error on gain for stack1 in percent',
            Errorgains2     = REAL [*,*] : ' Error on gain for stack2 in percent',
            Errorgains3     = REAL [*,*] : ' Error on gain for stack3 in percent',
            nOisetriggers1  = REAL [*,*] : ' Noise on theta trigger bin for \
	                                     stack 1 (kev)',
	    nOisetriggers2  = REAL [*,*] : ' Noise on theta trigger bin for \
	                                     stack 2 (kev)',
            nOisetriggers3  = REAL [*,*] : ' Noise on theta trigger bin for \
	                                     stack 3 (kev)')
         ;

 EGAZ                                                                           
      : 'Electromagnetic GAmma correction of Zero suppression\             
         Number of words/Angular range\                                    
         Number of angular range'                                          
           STATIC                                                               
                                                                                
      = (ThetaMinimum     = REAL   : 'Mininum cosine theta range ',            
         ThetamaXimum     = REAL   : 'Maximum cosine theta range ',             
         Coefficient1     = REAL   : 'Coefficient 1 to PECO/PGAC adjustement ',              
         Coefficient2     = REAL   : 'Coefficient 2 to PECO/PGAC adjustement ',              
         Coefficient3     = REAL   : 'Coefficient 3 to PECO/PGAC adjustement ',              
         Coefficient4     = REAL   : 'Coefficient 4 to PECO/PGAC adjustement ',              
         Coefficient5     = REAL   : 'Coefficient 5 to PECO/PGAC adjustement ',
         Coefficient6     = REAL   : 'Coefficient 6 to PECO/PGAC adjustement ')              
     ;                                                                          

 END ESET                                                                       

 END SUBSCHEMA
 

 SUBSCHEMA EcalDefects
 : 'Description of EC defects '

 AUTHOR   'B.Bloch-Devaux,M.N.Minard'
 REVIEWER 'F.Ranjard'
 VERSION  '2.9'
 DATE     '06/10/94'

 DEFINE ESET


  EBOK
      : 'Ecal BOx to be Killed. (STC)\
         Number of words/box\
         Number of Box'
           STATIC
 
      = (RegionNumber = INTE   :' Region number \
                                    =1 Endcap A \
                                    =2 Barrel   \
                                    =3 Endcap B ',
         BoxNumber  = INTE     : ' Box number to kill')
    ;
 
  EDDB                                                                        
     :  'ECAL Dead storeys.
          NR=setup code or Run number (RUNSTC)\
          Number of words/dead storey \                                         
          Number of dead storeys'                                               
         STATIC                                                                 
      = (  encodedADdress = INTE [0,*]        : 'K*2**26+J*2**16+I*2**2')       
     ;                                                                          
                                                                                
  EDKB                                                                        
     :  'ECAL Dead storeys for Killed  Boxes (STC)\                             
          Number of words/dead storey \                                         
          Number of dead storeys'                                               
         STATIC                                                                 
      = (  encodedADdress = INTE [0,*]        : 'K*2**26+J*2**16+I*2**2')       
     ;                                                                          
                                                                                
  EKLS                                                                        
     :  'ECAL killed storeys in the ROC (RUNSTC)\
          Number of words/killed storey \                                       
          Number of killed storeys'                                             
         STATIC                                                                 
      = (  encodedADdress = INTE [0,*]        : 'K*2**26+J*2**16+I*2**2')       
      ;                                                                         
                                                                                
  EKPL                                                                        
     :  'ECAL killed planes in the ROC (STC)\                                   
          Number of words/killed plane   \                                      
          Number of killed planes '                                             
         STATIC                                                                 
      = (  encodedADdress = INTE [0,*]        : 'Module# *2**16+Plane # ')      
      ;                                                                         
                                                                                
  EWRG                                                                       
      :  'Ec Pad gain correction                                            
          NR=run number (ECAL)\                                             
          Number of words/pad address\                                      
          Number of addresses '                                             
                                                                                
       STATIC                                                                   
        = (encodedADdress   = INTE [0,*]    : 'K*2**26+J*2**16+I*2**2',         
           COrrectionfactor = REAL [*,*]    : 'pad correction factor ' )        
      ;                                                                               
                                                                                
  E2SC                                                                        
     :  'ECAL Shorted doublets (STCRUN)\
          Number of words/shorted doublet \                                     
          Number of shorted doublets'                                           
         STATIC                                                                 
      = (  encodedAddress1 = INTE [0,*]        : 'K*2**26+J*2**16+I*2**2',      
           encodedAddress2 = INTE [0,*]        : 'K*2**26+J*2**16+I*2**2')      
      ;                                                                          
                                                                                
  E3SC                                                                        
     :  'ECAL Shorted triplets (STCRUN)\
          Number of words/shorted triplet \                                     
          Number of shorted triplets'                                           
         STATIC                                                                 
      = (  encodedAddress1 = INTE [0,*]        : 'K*2**26+J*2**16+I*2**2',      
           encodedAddress2 = INTE [0,*]        : 'K*2**26+J*2**16+I*2**2',      
           encodedAddress3 = INTE [0,*]        : 'K*2**26+J*2**16+I*2**2')      
     ;                                                                          
                                                                                
  E4SC                                                                        
     :  'ECAL Shorted quadruplets (STCRUN)\
          Number of words/shorted quadruplet \                                  
          Number of shorted quadruplets'                                        
         STATIC                                                                 
      = (  encodedAddress1 = INTE [0,*]        : 'K*2**26+J*2**16+I*2**2',      
           encodedAddress2 = INTE [0,*]        : 'K*2**26+J*2**16+I*2**2',      
           encodedAddress3 = INTE [0,*]        : 'K*2**26+J*2**16+I*2**2',      
           encodedAddress4 = INTE [0,*]        : 'K*2**26+J*2**16+I*2**2')      
     ;                                                                          
                                                                                
  E5SC                                                                        
     :  'ECAL Shorted quintuplets (STCRUN)\
          Number of words/shorted quintuplet \                                  
          Number of shorted quintuplets'                                        
         STATIC                                                                 
      = (  encodedAddress1 = INTE [0,*]        : 'K*2**26+J*2**16+I*2**2',      
           encodedAddress2 = INTE [0,*]        : 'K*2**26+J*2**16+I*2**2',      
           encodedAddress3 = INTE [0,*]        : 'K*2**26+J*2**16+I*2**2',      
           encodedAddress4 = INTE [0,*]        : 'K*2**26+J*2**16+I*2**2',      
           encodedAddress5 = INTE [0,*]        : 'K*2**26+J*2**16+I*2**2')      
     ;                                                                          
 
  ECDF                                                                           
      : 'Electromagnetic Correction to DEfault from wire energy\
         number of words/module\
         number of module'                                                 
  
         STATIC                                                               
                                                                                
      = (WireModule     = INTE   : 'Wire module number to shift',               
         EnergyShift    = REAL   : 'Energy shift in Kev ')                      
     ;                                                                          

  EWKW                                                                           
      :      'Electromagnetic Wire Killed in a Wrong way\
              number of words/module\                                           
              number of module'                                                 

           STATIC                                                               
                                                                                
      = (WireModule     = INTE   : 'Wire module number to correct',             
         PlaneNumber    = INTE   : 'Plane number to correct',                   
         ActionTyper    = INTE   : 'Action Type \
                                                = 0 to Kill \
                                                = 1 compensate')                
     ;                                                                          
                                                                                
  EGLO                                                                        
     :  'ECAL glowing storeys killed on the POT \
          Number of words/glowing storey \                                      
          Number of glowing storeys'                                            
         STATIC                                                                 
      = (  encodedADdress = INTE [0,*]        : 'K*2**26+J*2**16+I*2**2')       
      ;                                                                         
                                                                                
  EAGC
      :  'Ecal GAin Correction for endcap A\
          Number of words per storey\
          Number of storeys '
        STATIC
 
      = (Gainratiolevel1 = REAL :' Correction to apply for low gain',
         Gainratiolevel2 = REAL :' Correction to apply for high gain')
    ;
 
 END ESET
 
 END SUBSCHEMA


SUBSCHEMA EflowRunConsts
 :'Run Constants used in the Eflow code'                                            
                                                                                
 AUTHOR   'MN. Minard, M. Pepe' 
 REVIEWER 'F.Ranjard'                                                
 VERSION  '2.2'                                                                 
 DATE     '18/04/90'                                                            
                                                                                
 DEFINE ESET                                                                    

EFIP
      :       'T0 calculation parameters. 
               NR=run number (STC)\
               Number of words/row\
               Number of rows'
           STATIC
 
      = (TM(36)= REAL [-1000.,1000.]  :   ' T0 offset for modul 1',
        C1     = REAL [0.0,1000.0]    :  'parameter 1 for t0 energy dependence
                                             function',
        C2     = REAL [0.0,1000.0]    :  'parameter 2 for t0 energy dependence
                                             function')
 
      ;
 
                                                                                
EFCA                                                                           
      :  'Values of theta dependent weights for each                            
          of the 5 zones starting from the central                              
          region (T5<theta<T6  as defined in bank EFZC).                        
          For each of the two fits (calorimeters only                           
          or calorimeters + tracks) a different set                             
          of weighting factors is defined.
          NR=run number (STC)\
          Value of weights for each fit\                                        
          Number of fits=2' 
        STATIC                                                                  
                                                                                
      = (TRacks(5)    = REAL [0.00,10.]     : 'weight for selected tracks\      
                                               in the 5 theta regions',         
         MUons(5)     = REAL [0.00,10.]     : 'weight for identified muons ',   
         Gamma1(5)    = REAL [0.00,10.]     : 'weight for identified gammas in\
                                               stacks 1+2 (Egamma1+2) ',        
         Gamma3(5)    = REAL [0.00,10.]     : 'weight for identified gammas in\
                                               stack 3    (Egamma3)  ',         
         Ecal1(5)     = REAL [0.00,10.]     : 'weight for E(1+2), Egamma(1+2), \
                                               Emask(1+2)',                     
         Ecal3(5)     = REAL [0.00,10.]     : 'weight for E3, Egamma3, Emask3',
         ecalMask1(5) = REAL [0.00,10.]     : 'weight for Emask(1+2)',          
         ecalMask3(5) = REAL [0.00,10.]     : 'weight for Emask3 ',             
         HCal(5)      = REAL [0.00,10.]     : 'weight for H-Hmask ',            
         HcalMask(5)  = REAL [0.00,10.]     : 'weight for Hmask ',              
         Lcal1(5)     = REAL [0.00,10.]     : 'weight for Lcal(1+2)',           
         Lcal3(5)     = REAL [0.00,10.]     : 'weight for Lcal3 ')              
      ;                                                                         
                                                                                
 EHDS                                                                           
      :  'It gives the status of the Hcal digital information                   
          for a given run range as used in the Eflow.                           
          If the status = 1 the information is used.
          NR=run number (STC)\
          Number of words per row\                                              
          Number of rows=1'
        STATIC                                                                  
                                                                                
      = (BArrel       = INTE [0,1]          : 'status Hcal digital Barrel',     
         OVerlap      = INTE [0,1]          : 'status Hcal digital Overlap',    
         EndcapA      = INTE [0,1]          : 'status Hcal digital Endcap A',   
         EndcapB      = INTE [0,1]          : 'status Hcal digital Endcap B',   
         HCal         = INTE [0,1]          : '= 0 if Hcal is missing ')        
                                                                                
       ;

 END ESET                                                                       
                                                                                
 END SUBSCHEMA                                                                  


 SUBSCHEMA HcalCalRunConsts
 : 'HCAL Towers Calibration constants (RUN dependent)'
 
 AUTHOR   'R. Tenchini'
 REVIEWER 'A.Messineo'                                                
 VERSION  '2.0'
 DATE     '01/08/95'
 
 DEFINE ESET
 
 
HSTD
      :      'Hc Subcomponents Time Dependence for towers calibration.
              NR=run number (STC)\
              Number of Subcomponents \
              Number of rows (=1)'
           STATIC
 
      = (BarreL     = REAL [0.000,*]     : 'Time constant for
                                              Barrel  ',
         EndcapS     = REAL [0.000,*]     : 'Time constant for
                                              Endcaps   ')
      ;
 
HT0C
      :      'Hc Subcomponents normalization at the start of physics run.
              NR=run number (STC)\
              Number of constants \
              Number of rows (=1)'
           STATIC
 
      = (BarrelMc    = REAL [0.000,*]     : 'Average energy
                                          in qqbar events for the Barrel
                                          modules (MonteCarlo)',
         BarrelDa    = REAL [0.000,*]     : 'Average energy
                                          in qqbar events for the Barrel
                                          modules (Data at T=0)',
         EndcapMc    = REAL [0.000,*]     : 'Average energy
                                          in qqbar events for the Endcap
                                          petals (MonteCarlo)',
         EndcapDa    = REAL [0.000,*]     : 'Average energy
                                          in qqbar events for the Endcap
                                          petals (Data at T=0)')
      ;
HIMC
      :      'Hc InterModule Calibration constants.
              NR=run number (STC)\
              Number of Modules \
              Number of rows (=1)'
           STATIC
 
      = (BarrelConst(24)    = REAL [0.000,*]     : 'Barrel equalization
                                                          Constants',
         EndcapConst(12)    = REAL [0.000,*]     : 'Endcap equalization
                                                          Constants')
      ;

HT0W
     :  'Hc Subcomponents vs. wagon
          normalization at the start of
           physics run. NR=run number
           (STC)\
          Number of columns    \
          Number of rows'
         STATIC
      = (  Barrell1   = REAL [0,*] : 'Barrel wagon 1',
           Barrell2   = REAL [0,*] : 'Barrel wagon 2',
           Barrell3   = REAL [0,*] : 'Barrel wagon 3',
           Barrell4   = REAL [0,*] : 'Barrel wagon 4',
           Endcapab1  = REAL [0,*] : 'Endcap A & B wagon 1',
           Endcapab2  = REAL [0,*] : 'Endcap A & B wagon 2',
           Endcapab3  = REAL [0,*] : 'Endcap A & B wagon 3',
           Endcapab4  = REAL [0,*] : 'Endcap A & B wagon 4')
;

END ESET

END SUBSCHEMA
 
                                                                        
 SUBSCHEMA HcalDefects
 : 'HCAL Dead Channels banks'
 
 AUTHOR   'G. Catanesi,R.Tenchini,E.Mannelli,L.Silvestris'
 REVIEWER 'L.Silvestris'
 VERSION  '1.3'
 DATE     '22/08/90'

 DEFINE ESET
 
 HTXD
      :      'Hcal Unrecoverable Eightfold Tubes Dead \
              Number of words/Eightfold  \
              Number of Dead Eightfold Tubes'
           STATIC
 
      = (EightuAddr = INTE [10101001,32423040]: 'Eightfold Tube Address\
                              Addr = Eightube+lay*1000+\
                                     mod*100000+SubCom*10000000')
      ;
 HRDT
      :      'Hcal Recoverable Eightfold Tubes Dead \
              Number of words/Eightfold  \
              Number of Dead Eightfold Tubes'
           STATIC
 
      = (EightuAddr = INTE [10101001,32423040]: 'Eightfold Tube Address\
                              Addr = Eightube+lay*1000+\
                                     mod*100000+SubCom*10000000')
      ;
 
 
    HNTO
     :  'HCAL Noisy towers (STC)\
          Number of words/noisy tower\
          Number of noisy towers'
         STATIC
      = (  encodedADdress = INTE [0,*]        : 'see HTDI bank',
           MeanEnergy     = INTE [0,100000]   : 'Average Energy MeV'
        )
 
     ;
 
    HWNO
     :  'HCAL Noisy tubes clusters (STC)\
          Number of words/noisy tubes clusters\
          Number of noisy tubes clusters'
         STATIC
      = (  encodedADdress = INTE [0,*]        : 'See HWDI bank')
 
      ;
 
    HCIN
     :  'HCAL tower address inversions (STC)\
          Number of words/inversion \
          Number of inverted towers'
         STATIC
      = (  WrongAddress = INTE [0,*]        : 'see HTDI bank',
           CorrectAddress = INTE [0,*]      : 'see HTDI bank'
        )
      ;
 
    HLAY
     :  'HCAL dead layers (tubes) (STC) \
          Number of words/layer \
          Number of inverted towers'
         STATIC
      = (  AstrosModule = INTE [0,172]      : 'Module Number',
           DeadLayer = INTE [0,23]      : 'Layer Number'
        )
      ;
 
    HCUE
     :  'HCAL cuts for noise supp.(STC)\
          Number of columns    \
          Number of rows'
         STATIC
      = (  EnergyHicut = INTE [0,100000] : 'MeV cut for Noisy Tow supp',
           EnergyLocut = INTE [0,100000] : 'MeV cut for Pedestal supp'
        );

    HCNF
     :  'HCAL ADC and Splitter Boards Masks.(STC)\
          Number of columns    \
          Number of rows'
         STATIC
      = (  TOwermasks(4) = INTE [0,20000000] : 'ADC masks ',
           HcbaRdig(13)  = INTE [0,20000000] : 'HCAl Barrel modules digital \
                                                 masks',
           HcendAdig(13) = INTE [0,20000000] : 'HCAl Endcap A modules digital \
                                                 masks',
           HcendBdig(13) = INTE [0,20000000] : 'HCAl Endcap B modules digital \
                                                 masks',
           McbaRdig(13)  = INTE [0,20000000] : 'Mu Chamb. Barrel modules \
                                                 digital masks',
           McendAdig(13) = INTE [0,20000000] : 'Mu Chamb. Endcap A modules \
                                                 digital masks',
           McendBdig(13) = INTE [0,20000000] : 'Mu Chamb. Endcap B modules \
                                                 digital masks'
        );

 END ESET
 
 
 END SUBSCHEMA

 SUBSCHEMA MuonDefects
 : 'Description of MU chamber defects '
 
 AUTHOR   'P. Zografou '
 REVIEWER ''
 VERSION  '1.0'
 DATE     '20/06/90'
 
 DEFINE ESET
 
    MUNO
     :  'MUON Noisy clusters .
          NR=run number (STC)\
          Number of words/noisy clusters\
          Number of noisy clusters'
         STATIC
      = (  encodedADdress = INTE [0,*]        : 'See MUDG bank')
 
      ;
 
 END ESET
 
 END SUBSCHEMA

 
SUBSCHEMA TpcRunConsts
 : 'Description of TPC online data base banks'
 
 AUTHOR   'E.Lancon,F.Ranjard,D.Casper,I.Tomalin'
 REVIEWER 'W.Wiedenmann'
 VERSION  '5.0'
 DATE     '18/05/99'
 
 DEFINE ESET
 

TSWP :      'Sector Wire Parameters, used both in TPPs
              and in JULIA to control the wire reduction.NR=1  \
              Number of columns  \
              Number of rows'
 
           STATIC
 
      = (MinLen       = INTE [1,10]        :'Minimum pulse length',
         MaXlen       = INTE [1,100]       :'Maximum pulse length',
         PreSamp      = INTE [0,100]       :'Number of pre samples',
         POstsamp     = INTE [0,100]       :'Number of post samples',
         PeDestal     = INTE [0,255]       :'Pedestal',
         THresh       = INTE [0,255]       :'Average threshold',
         MaxAbove     = INTE [1,100]       :'Maximum number of samples above
                                             (THresh+PeDestal)',
         MaxSatur     = INTE [1,100]       :'Maximum number of saturated
                                             samples',
         ChargeNorm   = INTE [1,10]        :'Charge renorm coeff (2)',
         Reserved1    = INTE [*,*]         :'Algorithm number:\
                                             0=sum of all\
                                             1=sum of 3 highest',
         Reserved2    = REAL [0.10,1.]     :'Threshold for time est. as
                                             fraction of peak',
         Reserved3    = INTE [*,*]         :'minimum valid t0',
         Reserved4    = INTE [*,*]         :'maximum propagation length',
         Reserved5    = INTE [*,*]         :'not used',
         Reserved6    = INTE [*,*]         :'not used',
         Reserved7    = INTE [*,*]         :'not used');

 TSOR
      :      'Tpc-Start-Of Run block (STC)'

           STATIC

      = (bFIeld       = INTE [-15000,15000]     :'magnetic field in Gauss  ',
         BfieldFlag   = INTE [0,1]              :'DC (=0) or AC (=1) magnet',
         SenseWire    = INTE [0,13000]          :'Sense wire potential [V/10]',
         WireGrid     = INTE [-1000,-400]       :'Gating grid potential [V/10]',
         LastElect    = INTE [-1000,-400]       :'Potential of last electr.[V/10]',
         ChainCurrent = INTE [0,7000]           :'resistor chain current [muA/100]',
         PotWire      = INTE [0,500]            :'Potential wire potential [V]',
         ClockHertz   = INTE [6000000,12500000] :'Clock in Hz',
         Clock        = INTE [6000,12500]       :'Clock in kHz',
         RepRate      = INTE [0,99999]          :'Trigger Repetion rate',
         Scalelevel1  = INTE [1,10000000]       :'Scale factor level 1 trigger',
         Scalelevel2  = INTE [1,1000]           :'Scale factor level 2 trigger',
         Delaylevel1  = INTE [0,10000]          :'Delay level 1 trigger [nsec]',
         Delaylevel2  = INTE [0,100000]         :'Delay level 2 trigger [nsec]',
         DelayEbx     = INTE [0,10000]          :'Ebx delay  [nsec]',
         DelayLaser   = INTE [0,50000]          :'Laser trigger delay [nsec]',
         AtmPressure  = INTE [9500,12000]       :'atmospheric pressure [hPa/10]',
         AtmTemperatur = INTE [100,500]         :'atmospheric temperature [C/10]',
         GasPressure  = INTE [9500,12000]       :'gas pressure [hPa/10]',
         GasTemperatur = INTE [100,500]         :'gas temperature [C/10]')

      ;
 
 TCLB :     'Tpc CaLiBration for all TPC sectors (STC)\
             Number of words per sector \
             Number of sectors '
        STATIC
       = (CalDate     = INTE [0,*]     : 'Calibration data',
          CalTime     = INTE [0,*]     : 'Calibration time',
          NumThresh   = INTE [0,*]     : 'Number of abnormal thresholds',
          PeDestal    = INTE [0,255]   : 'TPD pedestal',
          PadThresh   = INTE [0,255]   : 'Global pad threshold',
          WireThresh  = INTE [0,255]   : 'Global wire threshold',
          PadLimit    = INTE [0,*]     : 'Pad limit register value',
          WireLimit   = INTE [0,*]     : 'Wire limit register value')
        ;

 TKAP :     'Tpc KAPut channels.  Abnormal channel thresholds ordered
             by sector number.  See TCLB for the number of entries for
             each sector. (STC)\
             Number of words per abnormal threshold \
             Number of abnormal thresholds'
        STATIC
        = (AbnormalThreshold = INTE [*,*]  : 'bits:\
                                              0-7 : pad or wire #\
                                              8-15: row # (0 for wires)\
                                              16-23: threshold (1-255)\
                                              24-31: sector # (1-36)')
         ;
 
 T2FC :      'Tpc magnetic Field Corrections, table 2.                                                                      
              Coordinate displacement table.(STC)
              Used for pre-1997 alignment\                                                                             
              Number of space points+3\                                                                                    
              Number of row=1'
       = (ValRng(2)       = INTE [1,*]    : 'Validity range',                                                               
          Rdisp1(8,19,6)  = REAL [*,*]    : 'Radial displacement for z>0',                                                  
          Rdisp2(8,19,6)  = REAL [*,*]    : 'Radial displacement for z<0',                                                  
          Phidisp1(8,19,6)= REAL [*,*]    : 'Azimuthal displacement for z>0',                                               
          Phidisp2(8,19,6)= REAL [*,*]    : 'Azimuthal displacement for z<0');                                              
           
T3RR
     :     'TPC range of runs with field corrections,NR=1 \
            number of words / run range \
            number of run ranges'
        STATIC
 
     = (Run1       = INTE [10000,99999] : '1st run of the range',
        Run2       = INTE [10000,99999] : 'last run of the range',
        BanKnumb   = INTE [10000,99999] : 'T3FC (T3CC) bank number',
        Side1      = INTE [0,1]         : 'side 1 flag (=1 correction
                                           must be applied to side A)',
        Side2      = INTE [0,2]         : 'side 2 flag (=2 correction
                                           must be applied to side B)',
        Numside1   = INTE [0,99]        : 'number of corrections to be
                                           applied to side A',
        Numside2   = INTE [0,99]        : 'number of corrections to be
                                           applied to side B')
     ;
 
 T4RR
      :     'TPC range of runs with short corrections,NR=1 \
             number of words / run range \
             number of run ranges'
         STATIC
 
      = (Run1       = INTE [10000,99999] : '1st run of the range',
         Run2       = INTE [10000,99999] : 'last run of the range',
         SIde       = INTE [1,2]         : 'side flag (=1 correction
                                            must be applied to endcapA,
                                                       =2 correction
                                            must be applied to endcapB)',
         FieldCage  = INTE [1,2]         : 'Fieldcage flag (=1 correction
                                            must be applied to outer
                                            field cage,
                                            =2 correction must be applied
                                            to inner field cage)',
         DeltaU     = REAL [*,*]         : 'Voltage drop relative to
                                            normal Voltage (dU/U)',
         ZShort     = REAL [-220.,220]   : 'z position of short',
         CorrectionScale = REAL [*,*]    : 'fitted scale factor to
                                            multiply correction
                                            (default=1)')
      ;

T3FC
     :      'TPC field corrections,NR=run \
             number of words / side\
             number of sides which need corrections'
        STATIC
 
     = (SIdenumber  = INTE [1,2]          : 'Side A=1,Side B=2',
        NDimension  = INTE [1,3]          : 'Dimension of parameter space',
        NCoeff      = INTE [1,50]         : 'Number coefficients used for parametrization',
        IPolynom    = INTE [1,9999]       : 'Code for parametrization type.\
                                             =1000*im+100*id+10*ie+ip.\
                                             ip gives polynomial type.\
                                             ie makes zero at endcaps.\
                                             id dampens where no data.\
                                             im multiplies by additional 
                                             function given in T3CC bank.',
        IOrder      = INTE [1,321]        : 'Code for ordering coordinates\
                                             r=1,phi=2,z=3,\
                                             IO = 100*c1 + 10*c2 + c3,\
                                             ci = coordinate number,\
                                             eg. (r,z,phi) -> IO = 132',
        ICoeff(150) = INTE [-10000,10000] : 'Power of parametrization function',
        COeff(50)   = REAL [*,*]          : 'Coefficients for parametrization',
        CorrWhat    = INTE [1,3]          : 'Correction to be applied to\
                                             r=1,phi=2,z=3')
     ;

 TLAS
      :     'TPC laser measurments, NR=run \
             number of words / side \
             number of sides ,if 1 side data are valid for side A and B'
         STATIC

      = (LastRunnumber   = INTE      : 'run number of last laser measurment',
         ElapsedTime     = REAL      : 'elapsed time since last laser measurment  [hour]',
         LastVelocity    = REAL      : 'last measured drift velocity  [cm/musec]',
         LastError       = REAL      : 'error on the last drift velocity [cm/musec]',
         LastChi2        = REAL      : 'chi2 of the last fit',
         NewVelocity     = REAL      : 'new measurment of the drift velocity [cm/musec]',
         NewError        = REAL      : 'error on the new drift velocity [cm/musec]',
         NewChi2         = REAL      : 'chi2 of the new fit',
         ErrorFlag       = INTE      : 'Error flag',
         CorrFactor      = REAL      : 'Correction Factor')
      ;

 TDPV
      :     'TPC PASS0 drift velocity, NR=run \
             number of words / side \
             number of sides = 2'
         STATIC
      
      = (DriftVel(3)           = REAL     : 'vx,vy,vz  drift velocity [cm/musec]',
         ErrorZ                = REAL     : 'error on vz')
      ;
 
TCUR
     :     'Current drawn by TPC.
            (Only available if needed for field correction.)
            NR=run \
            number of words/time period \
            number of time periods in run'
        STATIC
 
     = (PacKedcur  = INTE [*,*] : 'Bits 0-9: Time since beginning of run
                                   in units of 1/10 min.\
                                   Bits 10-20: Current (nA) in TPC side A.\
                                   Bits 21-31: Current (nA) in TPC side B.')
     ;
 
T3CC
     :      'TPC field corrections,
             multiplying those produced by T3FC. 
             (Rows correspond 1 to 1 with T3FC), NR=run \
             number of words / side\
             number of sides which need corrections'
        STATIC
 
     = (SIdenumber = INTE [1,2]  : 'Side A=1,Side B=2',
        NumCoeff   = INTE [1,18] : 'No. of coefficients used 
                                    by parameterization',
        COeff(18)  = REAL [*,*]  : 'Coefficients')
     ;

 TNFC :      'Tpc new magnetic Field Corrections, table 2.                                                                      
              Coordinate displacement table.  Used for
              1997-style calibration. (STC)\
              Number of space points+3\           
              Number of row=1'                                            
       = (ValRng(2)       = INTE [1,*]    : 'Validity range',             
          Rdisp1(8,19,6)  = REAL [*,*]    : 'Radial displacement for z>0',
          Rdisp2(8,19,6)  = REAL [*,*]    : 'Radial displacement for z<0',                                                  
          Phidisp1(8,19,6)= REAL [*,*]    : 'Azimuthal displacement for z>0',                                               
          Phidisp2(8,19,6)= REAL [*,*]    : 'Azimuthal displacement for z<0');                                              
           
 TFT0 :  'TPC t0 corrections for period in 1997 when TPC t0 was
          unstable at start of fills due to hardware problem.
          (NR = run number).\
          Number of columns \                                       
          Number of rows = time divisions needed per run'
              STATIC

       = (EVent           = INTE [1,*]    : 'Last event number of this period',
          T0fix           = REAL [*,*]    : 'Correction needed to TPC t0 (ns)');


 END ESET
                                                                                                                            
 END SUBSCHEMA


 SUBSCHEMA ScalRUNConsts
 : 'Description of Scal RUN Constants '
 
 AUTHOR   'B.Bloch-Devaux'
 REVIEWER ''
 VERSION  '4.0'
 DATE     '23/06/93'
 
 DEFINE ESET
 
 
 
 SZTH
      :      'Sical on line thresholds applied in ROC for each pad
              NR=setup code or run number (RUNSTC)\
              Number of words/component\
              Number of components  '
 
       STATIC
      = (ADdress    = INTE [0    ,*]  : 'subcomponent coded address',
         THresh     = REAL [0.00,* ]  : 'Threshold applied in Mev   ')
      ;
 
 
 SFTH
      :      'Sical on line thresholds applied on Fast OR channels
              NR=setup code or run number (RUNSTC)\
              Number of words/component\
              Number of components  '
 
       STATIC
      = (ADdress    = INTE [0    ,*]  : 'subcomponent coded address',
         THresh     = INTE [0   ,* ]  : 'Threshold applied in ADC count')
      ;
 
 
 SITC
      :      'SIcal Trigger Conditions (LTC)\
              Words per row  \
              Number of rows'
      STATIC
      = (TrigThresh             = INTE [0,*] : 'Trigger threshhold word\
                                             bit 0-7 very low\
                                             bit 8-15 low\
                                             bit 16-23 high\
                                             bit 24-31 very high',
         DisabledOddsect        = INTE [0,*] : 'Disabled sector word odd layers\
                                             bit 0-15 =road 1\
                                             bit 16-31=road 2',
         DisabledEvensect       = INTE [0,*] : 'Disabled sector word even layers\
                                             bit 0-15 =road 1\
                                             bit 16-31=road 2')
      ;
 
 
 SIXP
 
      :      'SIcal Trigger sectors Pedestal bank
              (RUNSTC)\
              Words per trigger sector   \
              Number of trigger sectors '
      STATIC
      = (AdcoDdlayers         = INTE [0,*] : 'Trigger Pedestal value, odd layers\
                                             bit 0-15 road 1, bit 16-31 road 2',
         AdceVenlayers        = INTE [0,*] : 'Trigger Pedestal value, even layers\
                                             bit 0-15 road 1, bit 16-31 road 2')
      ;
 
 SINF
 
      :      'SIcal Trigger Noisy (suppressed) Fastor bank
              (RUNSTC)\
              Words per fastor  \
              Number of fastors '
      STATIC
      = (F1                   = CHA4     : 'Fastor name letters 1-4',
         F2                   = CHA4     : 'Fastor name letters 5-8',
         F3                   = CHA4     : 'Fastor name letters 9-12')
      ;
 
 SRCO
 
      :      'Sical Run COnditions
              (RUNSTC)\
              Words per condition  \
              Number of conditions'
      STATIC
      = (conditionNAme        = CHA4       : 'Condition key name ',
         conditionVAlue       = INTE [*,*] : 'Value for key condition')
      ;
 
 SITR
 
      :      'SIcal Trigger Readout configuration
              (RUNSTC)\
              Words per configuration  \
              Number of configurations'
      STATIC
      = (AE                   = INTE [0,*] : 'A even readout (1=in 0=out)',
         AO                   = INTE [0,*] : 'A odd readout  (1=in 0=out)',
         BE                   = INTE [0,*] : 'B even readout (1=in 0=out)',
         BO                   = INTE [0,*] : 'A odd readout  (1=in 0=out)')
      ;
 
 SISC
 
      :      'SIcal Slow Control values at Start of Run
              (RUNSTC)\
              Words per configuration  \
              Number of configurations'
      STATIC
      = (ContVal(92)    = REAL [*,*] : 'Slow Controlled values :\
                              1-12 are Bias voltages           \
                             13-68 are 14x4 Temperatures probes\
                             69-72 are Pt100 Temperature probes\
                             73-80 are Low voltage values      \
                             81-88 are Low voltage currents    \
                             89-92 are Level adapter Low voltage')
      ;
 END ESET
 
 END SUBSCHEMA
 

 SUBSCHEMA ScalDefects
 :'Defects identified in Scal '
 
 AUTHOR   'B.Bloch-Devaux '
 REVIEWER '   '
 VERSION  '2.0'
 DATE     '18/08/92'
 
 
 
 DEFINE ESET
 
 SDPD
     :  'Scal permanent Dead PaDs.
          NR=setup code or Run number (RUNSTC)\
          Number of words/dead pad \
          Number of dead pads'
         STATIC
      = ( encodedADdress = INTE [1,28612] : 'address of dead component \
                                           bit 0-1 = triplet index 0 to 3\
                                           bit 2-5 = radius index 0 to 15\
                                           bit 6-10= Phi index 0 to 31\
                                           bit 11  =  Module index 0=A 1=B\
                                           bit 12-13= Z index inside triplet 0 to 2\
                                           bit 14  =  Dead amplex bit-if on R bits are off\
                                           then 1 unit is added to get address in range')
      ;
 
 SKPD
     :  'Scal Killed PaDs by human intervention.
          NR=setup code or Run number (RUNSTC)\
          Number of words/dead pad \
          Number of dead pads'
         STATIC
      = ( encodedADdress = INTE [1,28612] : 'address of killed component \
                                           bit 0-1 = triplet index 0 to 3\
                                           bit 2-5 = radius index 0 to 15\
                                           bit 6-10= Phi index 0 to 31\
                                           bit 11  =  Module index 0=A 1=B\
                                           bit 12-13= Z index inside triplet 0 to 2\
                                           bit 14  =  Dead amplex bit-if on R bits are off\
                                           then 1 unit is added to get address in range')
      ;
 
 
 END ESET
 END SUBSCHEMA
 

 SUBSCHEMA JULOFFrunsummary
 : 'Description of run summaries: FALCON results are kept into JSUM which is written on POT,DST,MDST'
 
 AUTHOR   'J.Boucrot,E.Blucher,S.Wasserbaech,B.Bloch'
 REVIEWER 'S.Wasserbaech'
 VERSION  '6.0'
 DATE     '25/04/01'
 
 DEFINE ESET
 
 
 LFIL
      :      'LEP Fill description. The table is filled by the BOOKKEEPING procedure.
              The LEP energy is then modified when a better value is found.
              NR=first run (STC)\
              Number of words/LEP fill\
              Number of LEP fills '
 
       STATIC
      = (LepFill      = INTE [0,*]         : 'LEP Fill Number',
         FirstRun     = INTE [0,*]         : 'First run to which these data belong',
         LastRun      = INTE [0,*]         : 'Last Run to which these data belong',
         NumVer       = INTE [0,*]         : 'Number of tracks used to compute BX, BY',
         LepEnergy    = REAL [0.,250.000]  : 'LEP center-of-mass energy, average for this fill, in GeV',
         BeamX        = REAL [-1.,1.0000]  : 'Mean x coordinate of beam crossing.\
                                                 BeamX and BeamY are computed from the distribution of d0 vs. phi
                                                 for selected tracks (the Toby Burnett Method).',
         BeamY        = REAL [-1.,1.0000]  : 'Mean y coordinate of beam crossing',
         BeamZ        = REAL [-1.,1.0000]  : 'Mean z coordinate of beam crossing, from vertices of Z0 -> Hadrons',
         ErroX        = REAL [0.,1.0000]   : 'Error on BeamX',
         ErroY        = REAL [0.,1.0000]   : 'Error on BeamY',
         ErroZ        = REAL [0.,10.0000]  : 'Error on BeamZ',
         OFfset       = REAL [-1.,1.0000]  : 'Average d0 (to account for possible systematic offsets).
                                                 The corrected d0 of a track, relative to the average beam crossing is\
                                                    d0beam = d0frft - BeamX*sin(phi0)\
                                                             + BeamY*cos(phi0) - OFfset,\
                                                 where d0frft is the d0 with respect to the z-axis, and phi0 is the
                                                 track phi direction at closest approach to the z-axis.')
      ;
 
 
 RLUM
      :      'RUN luminosity from LCAL . The table is filled by LUMI,RUN_Q or HAD specialist one row per run.
              NR=first run number (STC)\
              Number of words/run\
              Number of runs'
 
       STATIC
      = (RunNumber    = INTE [0,*]         : 'Run number',
         RunQuality   = INTE [*,*]         : 'Bit pattern for Run Selection by Run_Qual or by Physics Groups\
                                                  Bit 0  Run DUCK\
                                                  Bit 1  Run MAYB\
                                                  Bit 2  Run PERF\
                                                  Bit 3  Run selected by ElectroWeak Physics Goup\
                                                  Bit 4  Run selected by Heavy Flavour , ECAL Physics Group\
                                                  Bit 5  Run selected by Heavy Flavour , HCAL Physics Group\
                                                  Bit 6  Run selected by Heavy Flavour , MUON Physics Group\
                                                  Bit 7  Run selected by Heavy Flavour , DEDX Physics Group\
                                                  Bit 8  Run selected by Searches , Photon Physics Group\
                                                  Bit 9  Run selected by Searches , Muon Physics Group\
                                                  Bit 10 Run selected by Searches , Standard Physics Group\
                                                  Bit 11 Run selected by Searches , Multihadronic Physics Group\
                                                  Bit 12 Run selected by VDET\
                                                  Bit 13 Run selected by Electroweak Asymmetries Physics Group',
         NumbZhad     = INTE [0,*]         : 'Number of Z0 - > Hadrons Events in the Luminosity Sample',
         NumbBhab     = INTE [0,*]         : 'Number of Bhabha Events used in the Luminosity Calculation ,\
                                              i.e. with good HV/trigger conditions',
         LUmi         = REAL [0.,1000.00]  : 'Best estimate of Luminosity in nb**-1',
         BacKground   = REAL [0.,*]        : 'Number of Background Bhabha events in Luminosity Sample',
         BhabTrig     = REAL [0.,1.0000]   : 'Bhabha Trigger Efficiency')
 
      ;
 
 
 RXYZ
       :      'Parameters for Beam Position , run by run
               the table is filled by the Bookkeeping procedure .
               NR = first run number\
               Number of words per run\
               Number of runs ( max 500) '
           STATIC
 
       = (RunNumer     = INTE [0,*]          : 'Run Number',
          NumVer       = INTE [0,*]          : 'Number of vertices used to compute XY of beam',
          FLagcat      = INTE [0,*]          : 'Flag = 1 if X,Y position obtained with tracks with VDET \
                                                     = 0 position obtained with tracks from ITC TPC only\
                                                     = -1 if no beam position can be calculated ',
          BeamposX     = REAL [-1.,1.0000]   : 'Mean value of beam position in X',
          BeamposY     = REAL [-1.,1.0000]   : 'Mean value of beam position in Y',
          BeamposZ     = REAL [-1.,10.0000]  : 'Mean value of beam position in Z',
          ErrorX       = REAL [0.,1.0000]    : 'Error on X beam position',
          ErrorY       = REAL [0.,1.0000]    : 'Error on X beam position',
          ErrorZ       = REAL [0.,10.0000]   : 'Error on X beam position',
          LUmin        = REAL [-1.000,*]     : 'Luminosity for VDET events , in nb**-1',
          OFfset       = REAL [-1.000,*]     : 'Offset  or mean d0 , see bank LFIL')
       ;
 
 
 RSLU
      :      'RUN luminosity from SICAL. The table is filled by LUMI,RUN_Q or HAD specialist one row per run.
              NR=first run number (STC)\
              Number of words/run\
              Number of runs'
 
       STATIC
      = (RunNumber    = INTE [0,*]         : 'Run number',
         LumQuality   = INTE [*,*]         : 'Flag for Sical Lumi status by Sical expert\
                                                   =  0  unknown\
                                                   =  1  temporary\
                                                   =  2  Final ',
         NumbBhab     = INTE [0,*]         : 'Number of Bhabha Events used in the Luminosity Calculation ,\
                                              i.e. with good HV/trigger conditions including SICAL',
         LUmi         = REAL [0.,1000.00]  : 'Best estimate of Luminosity in nb**-1',
         BacKground   = REAL [0.,*]        : 'Number of Background Bhabha events in Luminosity Sample',
         ElecWeak     = REAL [0.,5.0000]   : 'Electroweak correction to Xsection')
 
      ;
 
 
 RS4B
      :      'RUN Luminosity from SICAL for 4-bunch runs, filled by SICAL specialist one row per run.
              NR=First run Number\
              Number of words/run\
              Number of runs'
 
       STATIC
      = (RunNumber    = INTE [0,*]         : 'Run number',
         LumQuality   = INTE [*,*]         : 'Flag for Sical Lumi p[er bunch status by Sical expert\
                                                   =  0  unknown\
                                                   =  1  temporary\
                                                   =  2  Final ',
         numbBhab1    = INTE [0,*]         : 'Number of Bhabha Events used in Luminosity Calculation , bunch 1',
         Lumi1        = REAL [0.,1000.00]  : 'Best estimate of Luminosity in nb**-1, bunch 1',
         bacKground1  = REAL [0.,*]        : 'Number of Background Bhabhas , bunch 1',
         numbBhab2    = INTE [0,*]         : 'Number of Bhabha Events used in Luminosity Calculation , bunch 2',
         Lumi2        = REAL [0.,1000.00]  : 'Best estimate of Luminosity in nb**-1, bunch 2',
         bacKground2  = REAL [0.,*]        : 'Number of Background Bhabhas , bunch 2',
         numbBhab3    = INTE [0,*]         : 'Number of Bhabha Events used in Luminosity Calculation , bunch 3',
         Lumi3        = REAL [0.,1000.00]  : 'Best estimate of Luminosity in nb**-1, bunch 3',
         bacKground3  = REAL [0.,*]        : 'Number of Background Bhabhas , bunch 3',
         numbBhab4    = INTE [0,*]         : 'Number of Bhabha Events used in Luminosity Calculation , bunch 4',
         Lumi4        = REAL [0.,1000.00]  : 'Best estimate of Luminosity in nb**-1, bunch 4',
         bacKground4  = REAL [0.,*]        : 'Number of Background Bhabhas , bunch 4')
 
      ;
 

 RNR2
      :      'RUN averaged energy for LEP 2 runs.                                                             
              NR=year  number (STC)\
              Number of words/run\
              Number of runs'

       STATIC
      = (RunNumber    = INTE [0,*]         : 'Run number',
         ENergy       = REAL [0.,*]        : 'Energy in GeV, average for this run') 
      
      ;

 RNL2
      :      'LEP2 instantaneous energy given by 15 minutes time slices.
              NR=year  number (STC)\
              Number of words/slice\
              Number of slices'

       STATIC
      = (DAtestart    = INTE [0,*]         : 'Date of start of 15 minutes time slice, YYMMDD',
         TImestart    = INTE [0,*]         : 'Time of start of 15 minutes time slice, hhmmss',
         ENergy       = REAL [0.,*]        : 'Energy during the time slice, in GeV')
      ;

 RNF2
      :      'Description of fills for LEP2 precise energy values.       
              NR=year  number (STC)\
              Number of words/fill\
              Number of fills'

       STATIC
      = (FillNumber   = INTE [0,*]         : 'Fill number',
         DateStart    = INTE [0,*]         : 'Date of start of fill, YYMMDD',
         TimeStart    = INTE [0,*]         : 'Time of start of fill, hhmmss',
         DateEnd      = INTE [0,*]         : 'Date of   end of fill, YYMMDD',
         TimeEnd      = INTE [0,*]         : 'Time of   end of fill, hhmmss',
         FirstSlice   = INTE [0,*]         : 'First row in RNL2 for time slice informations for this fill',
         LastSlice    = INTE [0,*]         : 'Last row in RNL2 for time slice informations for this fill')
       
        ;

 
 BPBB
      :   'Chunk-by-chunk Beam Position, Julia version < 275.03 (NR=BE setup code)\
           number of columns\
           number of rows = number of chunks in setup'
           STATIC
 
   = (FirstMark   = INTE        : 'Packed run/event number for
                                    first event in chunk;
                                    bits 0-15: event;
                                    bits 16-30: run (must be less than 32768!)',
      LastMark    = INTE        : 'Packed run/event number
                                    for last event in chunk',
      XWord       = INTE        : 'Packed x position and uncertainty;
                                    bits 0-15: delta_x/(0.1 micron);
                                    bits 16-30: 20000 + x/(0.5 micron)',
      YWord       = INTE        : 'Packed y position and uncertainty')
      ;
 
 
 WIDE
      :   'Metachunk-by-metachunk luminous region sigma_x (NR=BE setup code)\
           number of columns\
           number of rows = number of metachunks'
           STATIC
 
   = (Word1       = INTE        : 'Packed metachunk information;
                                    bits 13-30: run number for first event
                                    in metachunk;
                                    bits 0-12: raw sigma_x in units of 0.1 micron
                                    (the bias in VBWP must be subtracted to get
                                    the corrected sigma_x)
                                    (if raw sigma_x = 0 then no measurement is
                                    available)',
      Word2       = INTE        : 'Bits 9-30: event number for first event
                                    in metachunk;
                                    bits 0-8: upper half of run number for
                                    last event in metachunk',
      Word3       = INTE        : 'Bits 22-30: lower half of run number for
                                    last event in metachunk;
                                    bits 0-21: event number for last event
                                    in metachunk')
      ;
 
 
 WIDN
      :   'NEVE dist for luminous region sigma_x meas (NR=BE setup code)\
           number of columns\
           number of rows (=1)'
           STATIC
 
   = (NumBins       = INTE         : 'Number of bins used to store NEVE
                                     distribution (=100)',
      NeveMax       = INTE         : 'Maximum value of NEVE in last bin
                                     (the first bin starts at 1)',
      INtegral(100) = REAL [0.,1.] : 'Array of integral[f(NEVE) * NEVE * dNEVE];
                                     the bin index corresponding to the value NEVE
                                     is INT((NEVE-1)*NB/NM) + 1')
      ;
 

 RKSE
       :      'Description of KINGAL Productions, NR=KINGAL code\
               Number of words per production\
               Number of productions'
           STATIC

      = (VSnfile(3)       = CHA4            : 'Tape+File number, or Castor identifier',
         RunNumb          = INTE [0,*]      : 'Run Number',
         YearGeom         = INTE [0,*]      : 'Year of Galeph Geometry e.g. 1999',
         NatDat           = INTE [0,*]      : 'Nature of Data: 2 for POT, 5 for MINI',
         LepEnergy        = INTE [0,*]      : 'Simulated LEP centre-of-mass energy in MeV',           
         Ranm1            = INTE[0,30000]   : 'First Ranmar seed in KINGAL',
         Ranm2            = INTE[0,30000]   : 'Second Ranmar seed in KINGAL',
         KinVers          = INTE [0,*]      : 'KINGAL version number',
         JulVers          = INTE [0,*]      : 'JULIA version number',
         GalVers          = INTE [0,*]      : 'GALEPH version number',
         MinVers          = INTE [0,*]      : 'MINI version number',
         DbsVers          = INTE [0,*]      : 'ADBSCONS DAF version number',
         NumbEvent        = INTE [0,*]      : 'Number of events generated for the present run',
         LTag             = INTE [0,*]      : 'tag for this production',
         KingalXsec       = REAL [0.,*]     : 'Accepted cross section for the current run, in pb',
         KingalErr        = REAL [0.,*]     : 'Error on Accepted cross section for the current run, in pb',
         ProdXsec         = REAL [-1.000,*] : 'Cross section of total production, in pb, -1. if unknown',
         ProdxErr         = REAL [-1.000,*] : 'Error on cross section of total production, in pb, -1. if unknown',
         KeyWord(15)      = CHA4            : 'Keyword of Kingal production')

       ;
   


 
 END ESET
 
 END SUBSCHEMA
 
 
