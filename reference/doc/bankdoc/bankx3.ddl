  SUBSCHEMA TRIGGERLEVEL3
   : 'Event record from the Trigger Level 3 of the Aleph detector.'

  AUTHOR   'M.Fernandez-Bosman'
  REVIEWER 'G.Luetjens'
  VERSION  '2.1'
  DATE     '20/05/94'

     DEFINE ESET

X3L2
     :       'Level 2 Check : (NR=Trigger segment nb); Tracks reconstructed in the (R,Z) plane from
              the  TPC trigger pads coordinates (X2DF bank) pointing to trigger segment.\
              Number of words per track \
              Number of tracks'
           STATIC
        = (Z0          = REAL [-250.,250.]     : 'Z0 track intersection with  beam orbit',
           TL          = REAL [*,*]            : 'Tangent of the dip angle',
           EcoVarm(3)  = REAL [*,*]            : 'Triangular covariance matrix saved in the order:\
                                                               1 2\
                                                                 3',
           CHi2        = REAL [0.,*]           : 'Chi2 per no of degrees of freedom',
           NdF         = INTE [0,13]           : 'number of degrees of freedom =\
                                                         nb of points on track - 2')
      ;


X3X3
     :       'History of level3 trigger trials: (NR=0);\ 
              Number of words per trial \
              Number of trials'
           STATIC
        = (TRial       = INTE [*,*]            : 'Description of the trial: \
                                                  bits 0 - 7  segment nb      \
                                                  bits 8 -11  calorimeter nb  \
                                                  bits 12-16  trigger nb      \
                                                  bits 17     =1 if accepted ',
           SuccTool    = INTE [*,*]            : 'Succesfull analysis tools',
           FailTool    = INTE [*,*]            : 'Failing analysis tools')
      ;


X3IT
     :       'ITC tracks (NR=Trigger segment nb); Tracks reconstructed in the (R,phi) plane
              pointing to the trigger segment.\
              Number of words per track \
              Number of tracks'
           STATIC
        = (MaskNb      = INTE [*,*]            : 'ITC mask number for current track',
           TrigBit     = INTE [*,*]            : 'Trigger bit number',
           EnerL1      = REAL [*,*]            : 'Energy observed in Level1 ADC\
                                                 (not read at present)',
           CHi2        = REAL [0.,*]           : 'Chi2 of the fit',
           DgF         = REAL [0.,*]           : 'number of degrees of freedom', 
           RAdius      = REAL [*,*]            : 'Radius :  circle parameter in mask frame',
           X0          = REAL [*,*]            : 'X0     :  circle parameter in mask frame',              
           Y0          = REAL [*,*]            : 'Y0     :  circle parameter in mask frame',              
           D0          = REAL [*,*]            : 'D0 distance of track from beam orbit',
           RTube       = REAL [*,*]            : 'Radius at beam tube',
           PhiTube     = REAL [0.,6.2832]      : 'PHI at beam tube',
           AssTrack    = INTE [0,*]            : 'X3TP track nb with nearest PHI',
           DeltaPhi    = REAL [-3.1416,3.1416] : 'DeltaPHI at beam tube to nearest TPC track')
      ;
X3TP
     :       'TPC tracks (NR=Trigger segment nb); Tracks reconstructed from TPAD information
              pointing to the trigger segment.\
              Number of words per track \
              Number of tracks'
           STATIC
        = (TrigBit     = INTE [*,*]            : 'Trigger bit number',
           EnerL1      = REAL [*,*]            : 'Energy observed in Level1 ADC\
                                                 (not read at present)',
           NbHits      = INTE [0,*]            : 'Nb of hits on the track',
           BuckLen     = INTE [0,*]            : 'bucket length (averaged)', 
           InnerR      = REAL [0.,*]           : 'Inner radius ',
           OuterR      = REAL [0.,*]           : 'Outer radius ',
           dPhidR      = REAL [*,*]            : 'Delta PHI / Delta R (averaged)',              
           PhiInner    = REAL [*,*]            : 'Phi at inner radius',              
           ThetInner   = REAL [*,*]            : 'Theta at inner radius',
           ZInner      = REAL [*,*]            : 'Z at inner radius',
           PhiOuter    = REAL [*,*]            : 'Phi at outer radius',              
           ThetOuter   = REAL [*,*]            : 'Theta at outer radius',
           ZOuter      = REAL [*,*]            : 'Z at outer radius',
           AssTrack    = INTE [0,*]            : 'X3IT track nb with nearest Phi,Theta')
      ;

X3EC
     :       'Ecal Clusters (NR=Trigger segment nb); Clusters found in the trigger segment.\
              Number of words per cluster \
              Number of clusters'
           STATIC
        = (EsumLow     = REAL [0.,*]           : 'Energy sum above low threshold \
                                                  (all stacks)',
           EsumHigh    = REAL [0.,*]           : 'Energy sum above high threshold \
                                                  (all stacks)',
           clShapeL(3) = INTE [*,*]            : 'Cluster structure for each stack \
                                                  (low threshold)\
                                                  1000000 * nb of Phi bins \
                                                  + 1000 * nb of Theta bins\
                                                  + nb of towers',
           clShapeH(3) = INTE [*,*]            : 'Cluster structure for each stack \
                                                  (low threshold)\
                                                  1000000 * nb of Phi bins \
                                                  + 1000 * nb of Theta bins\
                                                  + nb of towers',
           CleneL(3)   = REAL [0.,*]           : 'Cluster energy for each stack \
                                                  (low threshold)', 
           grLowX(3)   = REAL [0.,*]           : 'Cluster center of gravity X \
                                                  for each stack',
           grLowY(3)   = REAL [0.,*]           : 'Cluster center of gravity Y \
                                                  for each stack',
           grLowZ(3)   = REAL [0.,*]           : 'Cluster center of gravity Z \
                                                  for each stack',
           CleneH(3)   = REAL [0.,*]           : 'Cluster energy for each stack \
                                                  (high threshold)', 
           grHigX(3)   = REAL [0.,*]           : 'Cluster center of gravity X \
                                                  for each stack',
           grHigY(3)   = REAL [0.,*]           : 'Cluster center of gravity Y \
                                                  for each stack',
           grHigZ(3)   = REAL [0.,*]           : 'Cluster center of gravity Z \
                                                  for each stack')
      ;

X3EW
     :       'Ecal Wires (NR=Trigger segment nb); Energy in the wires.\
              Number of words per segment \
              Number of rows(=1)'
           STATIC
        = (EnerSum(3)  = REAL [0.,*]           : 'Energy sum for the 3 stacks',
           EsumOdd     = REAL [0.,*]           : 'Energy sum in odd planes',
           EsumEven    = REAL [0.,*]           : 'Energy sum in even planes',
           EnerCoinc   = REAL [0.,*]           : 'Energy above low threshold in even/odd coindence',
           MaxPlanes   = INTE [0,45]	       : 'Maximum Nr of neighboured planes \
						  above high threshold',
           FirstPlane  = INTE [0,45]	       : 'First plane nr of pair above low threshold')						 
      ;

X3HC
     :       'Hcal clusters (NR=Trigger segment nb); Clusters seen in the trigger segment.\
              Number of words / cluster \
              Number of clusters'
           STATIC
        = (NbTow       = INTE [0,*]            : 'Nb of towers in cluster',
           nbTowTh     = INTE [0,*]            : 'Nb of towers above energy threshold',
           EnClus      = REAL [0.,*]           : 'Energy in the cluster',
           EncluTh     = REAL [0.,*]           : 'Energy in the cluster for towers above threshold',
           EnFront     = REAL [0.,*]           : 'Energy in the cluster for front stack',
           EnBack      = REAL [0.,*]           : 'Energy in the cluster for back stack',
           ThetClu     = REAL [-3.1416,3.1416] : 'Theta of energy barycenter',
           ThetFront   = REAL [-3.1416,3.1416] : 'Theta of front stack energy barycenter',
           ThetBack    = REAL [-3.1416,3.1416] : 'Theta of back stack energy barycenter',
           PhiClu      = REAL [0.,6.2832]      : 'Phi of energy barycenter',
           PhiFront    = REAL [0.,6.2832]      : 'Phi of front stack energy barycenter',
           PhiBack     = REAL [0.,6.2832]      : 'Phi of back stack energy barycenter',
           FirstTow    = INTE [*,*]            : 'Pointer to first tower of cluster in HTDI')
      ;

X3LU
     :       'Lcal clusters (NR=0); Clusters seen in the Luminosity calorimeter.\
              Number of words / cluster \
              Number of clusters'
           STATIC
        = (ModuNb      = INTE [1,4]            : 'Lcal module number',
           EnClus      = REAL [0.,*]           : 'Energy in the cluster',
           ThetClu     = REAL [-3.1416,3.1416] : 'Theta of energy barycenter',
           PhiClu      = REAL [0.,6.2832]      : 'Phi of energy barycenter',
           CluWidth    = REAL [0.,*]           : 'Cluster width',
           NbTow       = INTE [0,*]            : 'Nb of towers in cluster',
           EnFront     = REAL [0.,*]           : 'Front stack energy in the cluster',
           EnMiddle    = REAL [0.,*]           : 'Middle stack energy in the cluster',
           EnBack      = REAL [0.,*]           : 'Back stack energy in the cluster',
           REserve     = INTE [*,*]            : 'Reserve')
      ;

X3TO
     :       'Total cal energy (NR=0); Total energy seen in ECAL,HCAL,LCAL.\
              Number of words \
              Number of rows(=1)'
           STATIC
        = (Hcal1(3)    = REAL [0.,*]           : 'Hcal front stack energy, \
                                                  towers > threshold 1 \
                                                  for ECA,BARR,ECB',
           Hcal2(3)    = REAL [0.,*]           : 'Hcal back stack energy, \
                                                  towers > threshold 1 \
                                                  for ECA,BARR,ECB',
           Hcal3(3)    = REAL [0.,*]           : 'Hcal front stack energy, \
                                                  towers > threshold 2 \
                                                  for ECA,BARR,ECB',
           Hcal4(3)    = REAL [0.,*]           : 'Hcal back stack energy, \
                                                  towers > threshold 2 \
                                                  for ECA,BARR,ECB',
           Hcal5(3)    = INTE [0,*]            : 'Nb of towers with energy > threshold 1 \
                                                  for ECA,BARR,ECB',
           Hcal6(3)    = INTE [0,*]            : 'Nb of towers with energy > threshold 2 \
                                                  for ECA,BARR,ECB',
           Ecal1(3)    = REAL [0.,*]           : 'Ecal front stack energy, \
                                                  towers > threshold 1 \
                                                  for ECA,BARR,ECB',
           Ecal2(3)    = REAL [0.,*]           : 'Ecal middle stack energy, \
                                                  towers > threshold 1 \
                                                  for ECA,BARR,ECB',
           Ecal3(3)    = REAL [0.,*]           : 'Ecal back stack energy, \
                                                  towers > threshold 1 \
                                                  for ECA,BARR,ECB',
           Ecal4(3)    = REAL [0.,*]           : 'Ecal front stack energy, \
                                                  towers > threshold 2 \
                                                  for ECA,BARR,ECB',
           Ecal5(3)    = REAL [0.,*]           : 'Ecal middle stack energy, \
                                                  towers > threshold 2 \
                                                  for ECA,BARR,ECB',
           Ecal6(3)    = REAL [0.,*]           : 'Ecal back stack energy, \
                                                  towers > threshold 2 \
                                                  for ECA,BARR,ECB',
           Ecal7(3)    = INTE [0,*]            : 'Nb of towers with energy > threshold 1 \
                                                  for ECA,BARR,ECB',
           Ecal8(3)    = INTE [0,*]            : 'Nb of towers with energy > threshold 2 \
                                                  for ECA,BARR,ECB',
           ecalW1(3)   = REAL [0.,*]           : 'Ecal wire energy, \
                                                  odd planes > threshold 1 \
                                                  for ECA,BARR,ECB',
           ecalW2(3)   = REAL [0.,*]           : 'Ecal wire energy, \
                                                  even planes > threshold 1 \
                                                  for ECA,BARR,ECB',
           ecalW3(3)   = REAL [0.,*]           : 'Ecal wire energy, \
                                                  odd planes > threshold 2 \
                                                  for ECA,BARR,ECB',
           ecalW4(3)   = REAL [0.,*]           : 'Ecal wire energy, \
                                                  even planes > threshold 2 \
                                                  for ECA,BARR,ECB',
           ecalW5(3)   = REAL [0.,*]           : 'Ecal wire energy, odd/even coinc \
                                                  planes > threshold 1 \
                                                  for ECA,BARR,ECB',
           ecalW6(3)   = REAL [0.,*]           : 'Ecal wire energy, odd/even coinc \
                                                  planes > threshold 2 \
                                                  for ECA,BARR,ECB',
           Lcal1(2)    = REAL [0.,*]           : 'Lcal front stack energy, \
                                                  towers > threshold 1 \
                                                  for side A,B',
           Lcal2(2)    = REAL [0.,*]           : 'Lcal middle stack energy, \
                                                  towers > threshold 1 \
                                                  for side A,B',
           Lcal3(2)    = REAL [0.,*]           : 'Lcal back stack energy, \
                                                  towers > threshold 1 \
                                                  for side A,B',
           Lcal4(2)    = REAL [0.,*]           : 'Lcal front stack energy, \
                                                  towers > threshold 2 \
                                                  for side A,B',
           Lcal5(2)    = REAL [0.,*]           : 'Lcal middle stack energy, \
                                                  towers > threshold 2 \
                                                  for side A,B',
           Lcal6(2)    = REAL [0.,*]           : 'Lcal back stack energy, \
                                                  towers > threshold 2 \
                                                  for side A,B',
           Lcal7(2)    = INTE [0,*]            : 'Nb of towers with energy > threshold 1 \
                                                  for side A,B',
           Lcal8(2)    = INTE [0,*]            : 'Nb of towers with energy > threshold 2 \
                                                  for side A,B')
      ;

X3TM
     :       'Processor timing . \ 
              Number of columns = 33\
              Number of rows = 1 '
           STATIC
        = (TimingSum       = INTE [0,*]            : 'CPU time used for Event',
           TimingTool(32)  = INTE [0,*]            : 'CPU time per tool')
      ;

X31X
     :       'Internal variables . \ 
              Number of columns = 16\
              Number of rows  '
           STATIC
        = (InterVar(16)    = INTE [*,*]            : 'Internal variables LEVEL 3')
      ;

X3PS
     :       'Level 3 vetoes, fraction passed . \ 
              Number of columns \
              Number of rows = 1 '
           STATIC
        = (TranRej(32)    = REAL [0.,*]            : 'fractions passed/triggerbit LEVEL 3')
      ;

  END ESET
 
  END SUBSCHEMA
