#if defined(DOC)
C! 1st entry in S_set
 ! JULIA 312
    SICAOK : uses XLUM2K instead of XLUMOK to compute Luminosity
             whatever status has the Energy stability bit       (B.Bloch)
    SILUAD : stores two luminosities in 2000 in SLUM
             with XLUMOK = including only Energy stability bit ON events
             with XLUM2K = ignoring Energy stability bit
             backward compatible for previous years/MC/Z runs.. (B.Bloch)
    SICLEAN: makes use of the Alephlib routine SIBKCL           (B.Bloch)
    SIESUM : SIFSUM moved to Alephlib                           (M.Cattaneo)

 ! JULIA 311
    SIPEDFX: Bug fix to correction of julia 31.0 - units are MeV, 
		   not ADC counts                                    (B.Bloch)

 ! JULIA 310
    SICLUS : Apply better calibration in delayed timing mode    (B.Bloch)
    SIEPCR : Introduce bunch dependent phi modulation           (B.Bloch)
    SIINIRU: Load SCBP instead of SCPH when runnning in HE mode (B.Bloch)
    SIPEDFX: Correct wrong pedestals in runs 50118-50122        (B.Bloch) 
    SICLC2,SRFIT0,SRFIT2,SRSELO,SRUPAR : 
             Double precision fixes for Linux                   (D.Smith)

 ! JULIA 309
    SICLEAN: Extend background identification to other types,
             create and fill SIID  bank                       (B.Bloch)
    SICRPOT: Update pointer to PECO bank in SIID. 
             All clusters are transmitted                     (B.Bloch)
    SILUMI : Update pointer to SILU bank in SIID. Only valid
             clusters are considered to build the SILU bank   (B.Bloch)
    SICLUS : Move call to SICLEAN to the end                  (B.Bloch)

 * corr file 308.3
    SIPEDFX: New - Fix pedestal of specific runs          (B.Bloch)
    SIPREDA: Call SIPEDFX to fix pedestal problem if any  (B. Bloch)

 * corr file 307.2
    SICLEAN: NEW. Checks for quality flag of SiCAL cluster     (B.Bloch)
    SICLUS : Extend LMAXL to 400, 
             Compute quality flag for each cluster             (B.Bloch)
    SICRPOT: Exclude from PECO/PCRL clusters identified as
             electronic noise                                  (B.Bloch)
    SIINIRU: Load SNOI bank from database                      (B.Bloch)
    SILUMI : Skip identified electronic noise                  (B.Bloch)
    SIPREDA: Apply bunch dependent calibration also after 1997 (B.Bloch)

 * corr file 305.4
    SICREC : Skip if setup code is zero (< 9209)                   (M.Cattaneo)

 * corr file 305.2
    SIINIRU: Suppress error message for missing SECT when setup<7  (M.Cattaneo)

 * corr file 305.1
    SICAOK : use XLUMOK from alephlib (>306) for complete checks   (B.Bloch)

 ! JULIA 305
    SIBUN.H: New common block                                      (B.Bloch)
    SICLUS : In 1997 correct for time dependent gain if not 
             already done in SIPREDA                               (B.Bloch)
    SIINIRU: Load SECT bank                                        (B.Bloch)
    SIPREDA: Apply a bunch dependant calibration for delayed 
             timimg in 1997                                        (B.Bloch)
    SILHCR : Store XTBP,XTCN,X1SC name indices locally          (M.Cattaneo)
    STMASK,STRIGF : Deleted from Julia library. Identical routines
                    exist in Alephlib                           (M.Cattaneo)

 ! JULIA 304
    SIINIRU,SIINIJO : Move booking of SSTA internal bank from siiniru to
                      siinijo, to avoid problems when processing multiple
                      runs with different bunch train setups.   (M.Cattaneo)

 * corr file 302.2
    SIPREDA : Apply bunch train correction only to 1995 data (B.Bloch)

 * corr file 281.3
   SILUMI : Fix energy cuts for high energy data (B.Bloch 11-Jul-96)

 * corr file 281.2
   SILUMI : Add protection for new acceptance cuts (M.Cattaneo 30-Jun-96)

 ! JULIA 280
   SICLUS,SIPREDA : opening "'" should have a closing "'" within
           the same line for cvs (F. Ranjard, Feb 96)

 * corr file 279.1
   ALGTWA : fix number of wagons for some fills (B. Bloch, Nov 95)
 ! JULIA 279
   SILUAD : automatise the energy dependence of the Lumi x-section
           (B. Bloch, Nov 95)
   SIEVST : pick up a better curve from the Data Base if available
           (E. Lancon, Nov 95)
   SIINIRU: get SHLD bank from Data Base (B. Bloch, Nov 95)
   SHLDJJ : new comdeck added (B. Bloch, Nov 95)

 * corr file 278.2
   SICAL modifications for the high energy run and 95 reprocessing
   SECAJJ, SIBXJJ : new comdecks added (B. Bloch, Nov 95)
   SIEVST : new routine: returns the energy at a given time along
           the Hold timing curve (E. Lancon, Nov 95)
   SIJUKE : new routine: decode juke box configuration (E. Lancon,
           Nov 95)
   SIGTWA : decode SICAL Hold number from SILH or SCHU bank (B. Bloch,
           Nov 95)
   ALGTWA : for the 140 Gev , SICAL will be set with one Hold except
           for 4 runs (B. Bloch, Nov 95)
   SIINIRU: get from Data base SECB and SIBX banks (B. Bloch, Nov 95)
   SILUMI : in Lumi anlysis of 1995 consider only in-time clusters
           (B. Bloch, Nov 95)
   SIPREDA: correct energy of SICAL in physics triggers according to
           bunch number (B. Bloch, Nov 95)
 ! JULIA 278
   ALGTWA : The 95 scan is done with 3 wagons (B.Bloch, P.Comas, Aug 95)

 * corr file 277.2
   ALGTWA : integer function to get the number of Sical wagons per train
   SIINIRU: use ALGTWA instead of RQBUNC
   SIPRLUM: use ALGTWA instead of RQBUNC
   SILUAD : fix the statistical error on Lumi when bunch train operation
 ! JULIA 277

 * corr file 276.1
   SIPREDA: Allow reprocessing from POT to POT when SIDI not available
            apply overall energy scale factor in calibration mode only
   SILHCR : Allow reprocessing from POT to POT
   SILUMI : Allow reprocessing from POT to POT
   SINAMC : add SCHU namindex to common
   SIGTWA : integer function to get the current wagon number in the train
   SIINIRU: Extend length of SLUM for maximum number of bunches
   SIINIJO: Extend Statistics info to maximum number of bunches
   SILHCR : Append wagon info as 15th word in SILH
   SILUAD : Modification for Bunch train running
   SILUMI : Modification for Bunch train running
   SINAMX : Modification for Bunch train bank
   SIPRLUM: Print Luminosity summary per wagon in the train
   SIPRSUM: Print reconstruction summary per wagon in the train
   SISTAT : Cumulate reconstruction statistics per wagon in the train
 ! JULIA 276

 * corr file 275.4
   SIPREDA: Process next pad in case of 'Invalid SIDI Address' (B.Bloch,
           Feb 1995)
 ! JULIA 275

 ! JULIA 274
 * corr file 273.3
   SILUAD : Take into account the downscaling factor of the coincidence
            trigger when computing the preliminary Luminosity
 * corr file 273.1
   SICAOK: fix usage of new trigger banks X1RG, X1TT, X1HI
   SILHCR: adapt to new trigger banks
 ! JULIA 273

 * corr file 272.3
   SICLUS: apply new cluster correction instead of old one.
   SILUMI: use planes 3 and 4 for loose side selection as for tight side
   SICLC2: new routine
 ! JULIA 272

 * corr file 271.4
   SIPVOI : array of energies in 9 pads in 5 first layers.
 * corr file 271.1
   SILHCR : Keep integrated SICAL scalers content in SILH
   SILUMI : Make Luminosity energy cuts depend upon CMS energy
   SIBOOK : HBPRO was called with a wrong number of arguments
#endif
