*CD ittrgc
      INTEGER ITRPMN,ITZPMN,ITMSKT,ITZMSK,ITIB16,ITBINS
      COMMON/ITTRGC/ITRPMN,ITZPMN,ITMSKT(0:255),ITZMSK(0:255),
     +              ITIB16,ITBINS(2,8)
C
#if defined(DOC)
      Trigger parameters for the ITC.
      ITRPMN = Minimum number of hit wires to activate an R-PHI mask.
      ITZPMN = Minimum number of hit wires to activate an R-PHI-Z mask.
      ITMSKT = 256 word memory addressed by trigger masks. (R-PHI)
               1 => valid wire combination for trigger.
               2 => invalid wire combination.
      ITZMSK = Same as ITMSKT for R-PHI-Z trigger.
      ITIB16 = word containing 16 software-loadable trigger bits used
               to control firing of adjacent sectors for correlation
               with the calorimeters.
      ITBINS = Limits of ITC theta bin numbers to be matched to the
               8 calorimeter theta bins.
#endif
