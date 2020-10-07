*CD ehcond
C
C     EC Analog signals conditions
      COMMON/EHCOND/ TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
      CHARACTER * 16 TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
C
#if defined(DOC)
      TCUTRK = Define if track elements must be cut in several segments
               Values 'NO CUT'/'CUT TRACK'
      TSMEAR = Define if smearing tobe done on energy deposition point
               Values 'NO SMEARING'/'SMEARING'  (Not used)
      TIRAGE = Method for generation of Shower points
               Values 'FLUCTUATED'/'VERY FAST'
      TEDEPO = Method for Energy deposition for "track segments"
               Values 'FLUCTUATED'/'VERY FAST'
      TPARAM = Define if parametrisation requested
               Values 'NO'   No parametrisation requested
                      'ELEC' Parametrisation requested for e+,e-
                             Triggered by : RUNC 0 0 0 0 1 0 0 0 0 0
                      'ALL'  Parametrisation requested for e+,e-,hadrons
                             Triggered by : RUNC 0 0 0 0 2 0 0 0 0 0
#endif
