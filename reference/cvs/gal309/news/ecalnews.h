*CD ecalnews
C! 1st entry in ECAL set
 ! GALEPH 30.3 corr file no. 2
   EHSITW - call EVERIF to check consistency of ecal address.
   EVERIF - new routine from (M.N.Minard) to check and correct
            row number if wrong.

 ! GALEPH 30.1
   ECIRUN - read EDPA bank from DB, and call EUPPAR.
   EUPPAR - update ECAL parameters from EDPA bank.

 ! GALEPH 25.6
   ECIRUN : get ENNO bank from DB using setup code
   EDSNOI : use ENNO bank if it exists, otherwise use data statment
 * corr.file no.3
   ECIRUN : add EZTH bank format

 ! GALEPH 25.4
   ECIRUN : use ALGTDB instecd of MDARD to get bank depending on
            the setup code

