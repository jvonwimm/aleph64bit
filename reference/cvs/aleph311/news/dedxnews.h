CKEY DEDX TPC
C! TPC DE/DX package
 ! ALEPHLIB 31.0
     TDEDXV : Do no apply HV correction from 1998 (KRUN>45000) (F.Cerutti)

 ! ALEPHLIB 30.7
    TCHKHV - Call XHVBIT instead of ALTRHV, 
             return .TRUE. for MonteCarlo      (M.Cattaneo)

 ! ALEPHLIB 21.1
   add pad dE/dx (D.Casper)
   TBTBLK, TMDEDX, TIDHYP, TDXERR, TXDEDX are obsolete and replaced by
   TBTBLP, TMPDDX, TPDHYP, TPXERR, TXPDDX
   the new routines work for WIRE or PAD dE/dx
   the first argument of the routines is 'PAD' or 'WIRE', the rest
   of the calling sequence stays unchanged.

 ! ALEPHLIB 21.0
   TDEDXV -  IF run# >= 40000 THEN
        The sector dE/dx modification for high voltage is only applied
        to the TCSX constants if the voltage is more than 10 volts from
        the nominal value.  If the voltage is at the nominal value
        (1250 volts), the TCSX constants are not modified. (J.Nachtman)

 ! ALEPHLIB 20.9
   TDEDXV -
   IF run# >= 40000 THEN  (140Gev runs and above)
      The global De/Dx normalisation factor is calculated
      using a parametrization given in TCPX bank.
      The sector De/Dx normalisation factor is taken from
      the TCSX bank and maybe modified by the TPHV bank.
      (J.Nachtman)

 ! ALEPHLIB 20.5
   TIDHYP - rewrite a statment to avoid floating point overflow.

 ! ALEPHLIB 20.1
   TBTBLK - set return code to 5 when TBTBLK<0.
   TXDEDX - if TBTBLK<0. then TBTBLK=0.
   TCHKEX - call TDEDXR to get DEDX calibration for IRUN run#
            returns GNR=SNR=0. if run is not calibrated on DAF
   TDEDXV - use only DAF banks TCGX/TCSX or TC2X
   TDEDXR - entry point into TDEDXV to set run number

 ! ALEPHLIB 15.5
   TCGXRU, TDEDXV - new routine to handle new banks TCGX/TCSX
                    (W.Wiedenman)
   TCHKEX, TMDEDX - modified to use new banks TCGX/TCSX (W.Wiedenman)
   TBTBLK - choose the parametrization type depending on the presence
            or absence of parametrization coefficients in the TC4X
            bank and not on the data type (MC or real).  (D.Casper)
 ! 930303 - ALEPHLIB 14.6
   TIDHYP - remove obsolete code for DTRA bank (Mini)
   TMDSTX - obsolete routine removed (Mini)
 ! 930202 - ALEPHLIB 14.5
   TWRRED - fix a bug in production of TSDI bank

