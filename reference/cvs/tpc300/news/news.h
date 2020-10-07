*CD news
C! last changes
   GAL:TPCSIM.OLB, _D.OLB, .NEWS
   source is on TPC300:*.F, .h with
   define TPC300 $2$DKB200:[GENERAL_A.GAL.TPC300]

   ALEPH DECS stations, SHIFT and CSF :
   source is on      /aleph/src/tpcsim ---> $ALROOT/tpc300/
   library           /aleph/lib/libtpcsim.a
   debug library     /aleph/gal/libtpcsim_dbx.a
   ---------------------------------------------------------------
 ! TPCSIM 300 corr # 1 970818 10:00

   TPBRTP: Fix scandalous bugs in treatment of track with non-unit
           charge (D.Casper).
 
 ! TPCSIM 300   960508   13.00
                CVS version of TPCSIM 218
                compile on AXP/VMS with -DVAX
                replace TIMAx by TIMEx in tgteer.F

   ---------------------------------------------------------------
 ! TPCSIM 218   940331   16.00
                was made from TPCSIM 2.16
                TPCSIM 2.17 is kept as a private FLORIDA version.

   TPCOND: Add Gaussian lookup table to /AVLNCH/
           (previously was in a standalone
           common block.

   TRAKEL: Add an array to treat time-of-flight properly.

   TBLOCH: Change the dE/dx curve to reproduce the data
           better at the ends of the curve (high and low
           beta*gamma)

   TGTEER: keep TPTE bank (short, and needed for debugging).

   TPAVAL: Fix bug which caused all pulses to be skewed in
           the same direction causing a bias.

   TPBRTK: Convert TOF from GALEPH into nanoseconds before
           using it.

   TPFBRT: Add TOF calculation to prevent reset to zero at
           every sector boundary.

   TPGETR: Pass TOF to broken tracks.

   TSCDEF: Suppress debugging printout of shaping amplifier
           response.

   TGINIT: Fill Gaussian lookup table, demanding that it
           is symmetric about zero and contains no entries
           beyond +/- 3 sigma.
   ----------------------------------------------------------------
 ! TPCSIM 217   931200

   private version given to Florida
   -----------------------------------------------------------------
 ! TPCSIM 216   930908   10.00

   FASTER : see change in TPC215
   TGTEER : add 3% smearing instead of 2%
   TPAVAL :
   TSCDEF :
   TSRESP :
   T2TRAN :
   ------------------------------------------------------------
   ---------------------------------------------------------------
 ! TPCSIM 215   921113   18.00

   program is 700000 words longer than before because there is a
   look-up table to handle electon avalanche.

   TBLOCH:  Change the parameterization of the energy loss function
   to raise the relativistic rise relative to minimum ionizing.

   TBLOCH, TGTEER, T2TEER, TSTEER:  Put in a 2.25% variation of dE/dx,
   event by event, to account for various systematics in calibration,
   atmosperic pressure, etc., which are not completely compensated
   in the data.

   FASTER: Increase number of possible clusters in various arrays to 4
   (was 2). This is not used in the final version of the program, but
   was used in tests which were later removed from the final
   implementation.

   TGTEER, T2TEER: Calculate pad response separately for each cluster,
   include TPCONS common deck.

   TPCONS: Add variable POICON, the (unchanging) base number of
   collisions per cm for a minimum ionizing particle.  The variable
   POIMIN is now varied by 2% event-by-event, as described above.

   TSTEER: Include TPCONS common deck.

   TPACHR: Remove error in fluctuations of the avalanche at the
   wires.  Old version was off by factor 80-100.

   TPAVAL: Model the time and pulseheight of the two clusters passed
   to the electronics (compressed from several dozen in the fast
   simulation) better by fitting the width, mean, and skewness in time.

   TPDRAY, TPRIMA, TPRIMB: Improve treatment of delta-rays.  Include
   the beta dependence of the delta cross-section, also treat the
   momentum dependence of the delta/non-delta ratio better.

   TPRIMA, TPRIMB: Clean up generation of delta ray energy

   TSCNST:  Change EBIN, CRUTH parameters for better delta ray
   agreement. Add variable POICON, change THETA (Polya distribution
   parameter) to make avalanche fluctuations larger.

   TSDEDX, T2DEDX: put charge^2/beta^2 and dE/dx into call to TPDRAY

   TSDIGT: Put channel type (wire, pad) into call.  Make treatment of
   pedestals, gain variation, non-linearity of electronics usable.
   Make variation for pads half that for wires.

   TSRESP: Add channel type to call of TSDIGT.  Kill 1.25% of channels
   to represent dead channels not found by calibration.

   TSTEER: Allow NEL to vary in call to TSTRAN, TSAVAL

   TSTRAN, T2TRAN: Add fluctuations due to adsorption during drift,
   change treatment of diffusion and ExB effect slightly.

   TPSIZE: Change TLAN bank on database to model pulseheight
   distribution better

   ----------------------------------------------------------------
 ! TPCSIM 214   920910   18.00

   mods in  : TWRRED
              to put TSLE bank on E-list
   ----------------------------------------------------------------
 ! TPCSIM 2.13  920710   18.00

   new
   comdecks : NEWS, VERSION
   mods in  : TPCVER
              to get version number from VERSION
              TGINIT
              to print TSIM bank written on output file
              TPTCST
              change a comment to please DECS
              TSCDEF
              to get TSIM bank from DB depending on setup code
              and set TPCSIM parameters. TSIM is dropped afterwards
              TSAVCN
              move TSIM bank format to TSCDEF
   ------------------------------------------------------------
