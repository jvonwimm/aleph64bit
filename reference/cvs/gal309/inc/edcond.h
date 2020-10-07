*CD edcond
      COMMON /EDCOND/ EDNOXT,EDMIXR,EDSAVG,EDZSUP
      CHARACTER*16    EDNOXT,EDMIXR,EDSAVG,EDZSUP
#if defined(DOC)
      EDNOXT         define the selected zone for noise generation
      EDMIXR = 'YES' if raw data event is to be added to simulated digit
      EDSAVG = 'YES' if one save the noise and gain value for output
      EDZSUP         define condition for Tower zeroes suppress
#endif
