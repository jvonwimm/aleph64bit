 SUBSCHEMA BcalJULBanks
 : 'Description of JULIA banks containing Bcal data'
     
 AUTHOR   'P.Mato,F.Sanchez,I.Riu,G.Boix'
 REVIEWER 'P.Morawitz'
 VERSION  '4.2'
 DATE     '12/11/98'
     
    DEFINE ATTRIBUTE
    
    AddressBcal
             = INTE [1,*]
             : 'Trgtype + (count length)*256 + (count first word)*65536';
     
    Channel
             = INTE [1,180] 
             : 'Channel no. 1-20=Scintillators, 21-180=strips';
     
    DigitBcal
             = INTE [1,*]
             : 'module + channel*256 + ADC*65536';

    EnergyBcal
             = REAL [0.000,300.000]
             : 'Energy in GeV';

    Scaler   = INTE [0,*]
             : 'Scaler for luminosity measurements';

    Trgtype  = INTE [1,10]
             : 'trigger type (1 to 10 as in scalers)';

    Xin
             = REAL [-10.00,10.00]
             : 'Point of inpact in cm';      
 
    SIlBcal
             = INTE [1,*]
             : ' ADC counts + channel * 65536 ';

    SCnBcal
             = INTE [1,*]
             : ' ADC counts + channel * 65536 ';

    SEaBcal
             = INTE [1,*]
             : ' ADC counts + channel * 65536 ';

    RadIus 
             = INTE [0,16]
             : ' The pad position number in r';
    PhI 
             = INTE [0,6]
             : ' The pad position number in phi';

           
   END ATTRIBUTE
     
 DEFINE ESET
     
  BCNT     
             : 'Luminosity Scalers  Obsolete! (17/04/98)\
                number of scalers per event\
                number of events per event'

            STATIC
             = (BcounT     = Scaler : '# Bhabha triggers',
                Bcnt13     = Scaler : '# Bhabha triggers from monitors 1.3',
                Bcnt24     = Scaler : '# Bhabha triggers from monitors 2.4',
                SinTrg(4)  = Scaler : '# Single triggers in Monitor 1-4',
                Dlay13     = Scaler : '# Delayed coincidence trig. from 1-3',
                Dlay24     = Scaler : '# Delayed coincidence trig. from 2-4',
                CounT      = Scaler : 'reserved') 
              ;

  BENE
             : 'Energies and points of impact   Obsolete! (17/04/98)\
                number of variables/count\
                number of counts (BCAL events)'
            STATIC
             = (Trgtype,EnergyBcal(4),Xin(4)) 
             ;
     
  BHIT
             : 'Packed word of ADC counts and addresses\
                number of packed words/hit\
                number of hits'
            STATIC
             = (DigitBcal)
             KEY  * = (DigitBcal)  END KEY
            ;
     
  BPTR
             : 'Pointers for counts in BHIT bank\
               number of packed words/count\
               number of counts'
            STATIC
             = (AddressBcal)
             KEY  * = (AddressBcal)  END KEY
            ;
     
  BLUM
      : 'Lep and Bcal Luminosities. 
         NB: This bank has no miniheader'

      STATIC

      = (TimeIn      = INTE : 'Time duration of luminosity measurements (secs)',
         NofInt      = INTE : '# of Bcal luminosity measurements counter',
         luBcTo      = INTE : 'Integrated luminosity from Bcal  (ub-1)',
         lAbcTo      = INTE : 'Integrated Aleph luminosity from Bcal   (ub-1)',
         luBcIn      = INTE : 'Instantaneous luminosity from Bcal (10**27)',
         lAbcIn      = INTE : 'Instantaneous Aleph luminosity from Bcal (10**27)',
         UlbcIn      = INTE : 'Uncertainty Inst. lum. from Bcal (10**27)',
         UAbcIn      = INTE : 'Uncertainty Aleph Inst. lum. from Bcal (10**27)',
         luBcinB(8)  = INTE : 'Instantaneous luminosity from Bcal per bunch and train (ub-1)',
	 UlbcinB(8)  = INTE : 'Uncertainty in the instantaneous luminosity from Bcal per bunch and train (ub-1)',
	 LuBctob(8)  = INTE : 'Integrated luminosity from Bcal per bunch and train (ub-1)',
         luHVint     = INTE : 'Integrated luminosity with ALEPH HV on and GBX',
         luLep14     = INTE : 'Instantaneous luminosity C14',
	 luLep23     = INTE : 'Instantaneous luminosity C23',
         NumSgn(4)   = INTE : 'Number of single tag in each module',
         nCoin14     = INTE : 'Total number of coincidences 14',
         nCoin23     = INTE : 'Total number of coincidences 23',
         rallsng(4)  = INTE : 'Rate of ALL singles',
         rcutsng(4)  = INTE : 'Rate of singles above the cut',
         Rallcoin14  = INTE : 'Rate of ALL coincidences 14',
         Rallcoin23  = INTE : 'Rate of ALL coincidences 23',
         rcutcoiN14  = INTE : 'Rate of coincidences 14 above the cut',
         rcutcoiN23  = INTE : 'Rate of coincidences 23 above the cut',
         ReserV(13)  = INTE : 'Reserved')
      ;

  BLCT
      : 'Bcal Luminosity counters\
         number of words/event\
         number of events'
         

      STATIC

      = (NumSgn(4)   = INTE : 'Number of single tag in each module',
         nCoin14     = INTE : 'Total number of coincidences 14',
         nCoin23     = INTE : 'Total number of coincidences 23')
      ;
 
  BCGN 
      : ' BCAL constant gains \
          Number of channels \
          number of gains per channel '

      STATIC 

      = (VPmt(4)    = REAL : 'VME PMT constant gains (GeV/count)', 
         VApd(4)    = REAL : 'VME APD constant gains (GeV/count)',

         FPmt(4)    = REAL : 'FB  PMT constant gains (GeV/count)',
         FApd(4)    = REAL : 'FB  APD constant gains (GeV/count)')
      ;

   BCPD
      : ' BCAL pedestals  Obsolete! (17/04/98)\
          Number of channels \
          number of pedestals per channel'

      STATIC

      = (PmtAve(4)    = INTE : 'VME PMT mean pedestals (ADC count)',
         ApdAve(4)    = INTE : 'VME APD mean pedestals (ADC count)',

         SilcAve(384) = INTE : 'Silicon mean pedestals (ADC count)',

         fpMtAve(4)   = INTE : 'FB PMT mean pedestals (ADC count)',
         fapDAve(4)   = INTE : 'FB APD mean pedestals (ADC count)',

         PmtSgm(4)    = INTE : 'VME PMT sigma pedestals (ADC count)',
         ApdSgm(4)    = INTE : 'VME APD sigma pedestals (ADC count)',

         SilcSgm(384) = INTE : 'Silicon sigma pedestals (ADC count)',

         fpMtSgm(4)    = INTE : 'FB PMT sigma pedestals (ADC count)',
         fapDSgm(4)   = INTE : 'FB APD sigma pedestals (ADC count)')
       ;  

   BCCL 
      : ' BCAL pedestals/calibration New structure, taken with ALEPH\
          Number of channels \
          number of pedestals per channel'

      STATIC 

       = (pmtLcAve(4) = INTE : 'LeCroy PMT mean pedestals (ADC count)',
         apdlCAve(4)  = INTE : 'LeCroy APD mean pedestals (ADC count)',

         fpMtAve(4)   = INTE : 'FB PMT mean pedestals (ADC count)',
         fapDAve(4)   = INTE : 'FB APD mean pedestals (ADC count)',

         SilcAve(384) = INTE : 'Silicon mean pedestals (ADC count)',

         PmtAve(4)    = INTE : 'VME PMT mean pedestals (ADC count)',
         ApdAve(4)    = INTE : 'VME APD mean pedestals (ADC count)',

         TempAve(4)   = INTE : 'Temperature mean pedestals (ADC count)',

         pmtLcSgm(4)  = INTE : 'LeCroy PMT sigma pedestals (ADC count)',
         apdlCSgm(4)  = INTE : 'LeCroy APD sigma pedestals (ADC count)',

         fpMtSgm(4)   = INTE : 'FB PMT sigma pedestals (ADC count)',
         fapDSgm(4)   = INTE : 'FB APD sigma pedestals (ADC count)',

         SilcSgm(384) = INTE : 'Silicon sigma pedestals (ADC count)',

         PmtSgm(4)    = INTE : 'VME PMT sigma pedestals (ADC count)',
         ApdSgm(4)    = INTE : 'VME APD sigma pedestals (ADC count)',

         TempSgm(4)   = INTE : 'Temperature sigma pedestals (ADC count)')
       ;

   BCTR 
      : ' BCAL trigger mask for ALEPH events \
          number of trigger masks \
          number of words per trigger '

      STATIC

       =(Triggmask    = INTE  : ' BCAL trigger mask for ALEPH events ')
      ;

   BCSL 
      : 'Packed word of silicon ADC counts and addresses\
         number of packed words/hit\
         number of hits'

      STATIC 

       =(SIlBcal) ;

   BCSC
      : 'Packed word of scintillators ADC counts and addresses\
         number of packed words/hit\
         number of hits'

      STATIC 

       =(SCnBcal) ;

   BCHG
      : 'Packed word of scint LeCroy-ADC (Searches) counts and addresses\
         number of packed words/hit\
         number of hits'

      STATIC 

       =(SEaBcal) ;

   BCPO
      : 'Bcal Luminosity info to be written to POTs by JULIA. 
         Used to keep info for offline calibration. 
         Only keeps info of BCAL coincidences.\
         Number of Variables \
         Number of BCAL events'
         
      STATIC

      = (iTRm = INTE : 'Trigger Mask',
         iPm1 = INTE : 'PMT counts of first Module',
         iAp1 = INTE : 'PMT counts of first Module',
         iRm1 = RadIus : 'Radial pos of hit in first Module', 
         ipH1 = PhI :    'Phi pos of hit in first Module', 
	 iPm2 = INTE : 'PMT counts of first Module',
         iAp2 = INTE : 'PMT counts of first Module',
         iRm2 = RadIus : 'Radial pos of hit in first Module', 
         ipH2 = PhI :    'Phi pos of hit in first Module')
      ;


 END ESET
     
END SUBSCHEMA
     
