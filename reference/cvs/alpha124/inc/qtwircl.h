      COMMON/TWIRCL/ RNRMCL(LTSECT),SLOPCL,JTRNCL,ADSPCL,GRNMCL,
     &               RNLNCL,PWDPCL(0:3,LTSTYP),EDGECL(2,LTSTYP)
#if defined(DOC)
C! Calibration constants for TPC dE/dx correction.  These come from
C the database banks TC1X and TC5X and TC6X
C
C     RNRMCL =  Normalization to convert ADC/cm to dE/dx with
C               minimum ionizing equal to unity, for each sector.
C     SLOPCL =  Slope for correction of dE/dx with sample length
C               dE/dx= (charge)/dx/(1.0+SLOPCL*loge(dx))
C     JTRNCL =  Percent of dE/dx samples to include in truncated mean
C     ADSPCL =  Correction for adsorption of ionization during drift
C               charge= uncorrected_charge/(1.0-ADSPCL*drift_length)
C               where the drift_length is in centimeters
C     GRNMCL =  Gain renormalization in TRKELS after nonlinear correction
C     RNLNCL =  Nonlinear correction in TRKELS
C     PWDPCL =  Polynomial correction for wire number dependence for
C               each sector type
C     EDGECL =  Edge effect correction for each sector type
C-----------------------------------------------------------------------
#endif
