*CD edpara
      COMMON /EDPARA/ EDTMCO,EDTSCO,EDTNOI(3),EDWMCO,EDWSCO,EDWNOI,
     +                EDTGAI(3),EDTGER(3),KALEDT(3),EDWGAI,EDWGER,
     +                KALEDW,EDCUT1,EDCUT2,EDWCUT,ETTNOI(12,3),
     +                ETSTWE(3), EDZTHR(3,3)
#if defined(DOC)
      EDTMCO is the mean value for the tower correlated noise
      EDTSCO is the sigma for the tower correlated noise
      EDTNOI is the tower noise's conversion factor ( keV / fC )
      EDTGAI is the nominal gain for each stack ( chanel / keV )
      EDTGER is the uncertainety in the gain for each stack ( fraction )
      KALEDT is the ADC calibration ( keV per chanel )
      EDWGAI is the nominal gain for wires (channel/kev)
      EDWGER is the uncertainty on the gain for wires
      EDWSCO is the ADC calibration for wires
      EDCUT1 is the number of sigma for Z sup in towers(SINGLE)
      EDCUT2 is the number of sigma for Z sup in towers(DOUBLE)
      EDWCUT is the absolute threshold on one wire plane module (ADC cha )
      ETTNOI is the noise on each theta trigger bin ( keV )
      ETSTWE is the wheight for each stack in the trigger sum
      EDZTHR is the Z sup values in MeV
#endif
