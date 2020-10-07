*CD vdetnews
C! 1st entry into VDET set

 ! GALEPH 30.4 correction 10.
   - VFNDEL : access properly parallel noise in VDEP bank

 ! GALEPH 30.2
   - GUSTEP,AGVDET,VDHIT,VDHITE,VDASIG,VDMKLO,VDMKTE,GDRAY

     1. NEW improved treatment of DELTA RAY production.
        Delta rays are important since they generate
        on average a nearby 3-D hit for 5% of the real hits.
     2. Let GEANT treat the Landau fluctuations in the energy
        deposition within the silicon.

     Induced by changes in the above routines and the
     database banks VTME,VVOL,VDEP,VGPA,VVOL,VPOS and VMAT
     which are made for setup code 6 onwards.
     Thus these changes will only take effect for Monte Carlo
     simulations of data from DATE 94 onwards.
     (Easy to change if needed.)
     The routine GDRAY is a copy of a GEANT routine in which
     the energy of each delta ray produced in the VDET region
     is multiplied by a FACTOR (1.4), which effectively give
     a cross-section increase of the same amount. This factor is
     used to give optimal agreement between data and monte carlo.

   - VDLAND,VDMKLA
      No longer used could be droped

   VDFOOB - bug fix related to maximum pulse height allowed
   VFNDMP - Take into account different multiplexing
            schemes between VDET92 and VDET200 for the computation
            of strip capacitances
   VDCLU  - Stop clustering when the wafer boundary is reached
            (for VDET92, z-view)
   VDPRTE - create electron clouds only if the track element is
            in the active region

 ! GALEPH 30.1
   - VFNDEL, VDFOOB, VDMKLO : remove bugs

 ! GALEPH 30.0
   ALEPHLIB 204 is mandatory.

   remove OLD VDET code.
   redesign VDET package using NEW VDET geometry package.

   Suppress manipulation of lists of strips
   Process one wafer at a time, use array of strips
   Process one module at a time, use array of readout channels

   Include correct treatment of Landau fluctuations inside silicon
   (optional)

   Details are given in Aleph note :  ALEPH 94-152, MINIV 94-05

 ! GALEPH 25.4
   VRDGAL,VDDIGI : use ALGTDB instecd of MDARD to get bank depending
                   on the setup code
   VDCLU,VDNOCL  : setupcode < or = LROWS('VDEL')

