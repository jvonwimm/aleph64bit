*CD news
#if defined(DOC)
   on AXP/VMS:
             $ define GAL308 $2$DKB200:[GENERAL_A.GAL.GAL308]
             $ define ALINC  $2$DKB200:[GENERAL_A.ALE.INC]

              source on GAL308:*.F, *.h 

              library   GAL:GAL308.OLB, _D.OLB, .NEWS
   
   on UNIX :  source on $ALROOT/gal308/
              library   $ALEPH/gal/libgal308.a
                        $ALEPH/gal/libgal308_dbx.a
              link      $ALEPH/lib/libgaleph.a
                        $ALEPH/lib/libgaleph_dbx.a
                        $ALEPH/src/galeph
 -----------------------------------------------------------------------
 ! 991116 - galeph 308.2
   VDMKTE - fix a mistyping error.

 ! 990511 - galeph 308.1
            ================BECAREFUL!!!=========================== 
            the $GUSER environment variable has been modified to add
            ",-u,gntube_".
            GAL:GALRUN.COM has been modified to include GNTUBE
            from the galeph library.
            ========================================================

   GPGHEI - make a character string from the integer NAMATE to please
            Linux compiler

   GNTUBE - modify the geant321/ggeom/gntube.F routine to add a protection
            for particles strickly parallel to z-axis

 ! 981203 - galeph 308.0
            ================BECAREFUL!!!=========================== 
            the $GUSER environment variable has been modified to add
            ",-u,gpghei_".
            GAL:GALRUN.COM has been modified to include GDRAY and
            GPGHEI from the galeph library.
            ========================================================

   AGCOIL - reduce the density of the average material used to describe
            the volumes at the edges of the coil since in this volume
            there is not the Al cylinder where the col is wound around.
            (A.Venturi)

   AGHCAL - reduce the radius of the HCAL endcap pole from 217cm to 210cm.
            (A.Venturi)

   GPGHEI - modify this GEANT321 routine: 
            the probability of interaction, for (in)elastic interactions
            of stable particles on proton and neutron, is increased by 8%
            in ECAL (M.N.Minard).

            this routine is added in guse/ and in the $GUSER environment
            variable
 -----------------------------------------------------------------------
 ! 981102 - galeph 307.2

   HCCRTO - replace data statment with data base value

 ! 980821 - galeph 307.1

   HCRDCA, HCALTO, HCALCT.H, HCTWTH.H - 
            implement the possibility equilizing the HCAL modules
            in the MC as in real data.
   HCCRTO - is recompiled because include files have changed.

 ! 980703 - galeph 307.0
            BECAREFUL : This version requires ADBSCONS => 239

   ASIGAL - call RECUST as a FUNCTION to please Lynux compiler.
   ASREVE - modify a FORMAT statment.
   ASWSUM - modify a FORMAT statment.
   AGTPCH - change a comment.

   new routines in hcal/
   HCALTO - function to evaluate the calibration constant applied
            to the HCAL module if they were real data. (A.Scabia`)
   HCMODN - find the module(s) number a tower belongs to. (A.Scabia`)
   HCRDCA - read the HCAL calibration constant used for the real data
            that are simulated. (A.Scabia`)
   mods in hcal/
   HCASIG - increase the size of HTHT to be ready for the HCAL signal spread
            along the tubes.
   HCFITU - spacers are taken into account in the determination of the center
            and of the length of the track segment.
   HCSTSE - change of one input argument to allow a better determination of the
            absolute coordinate of each track sub-segment.
   HCBACK - bug fix in the determination of a hit coordinate.
   HCTKSP - consistency checks and protections added.
   HCSHOW - modified to take into account the change in HCSTSE.
   HCCOIN - bug fixes in the evaluation of the hit local coordinates and
            better evaluation of the hit absolute coordinates;
            protections and consistency checks added.
   HCIRUN - add a call to HCRDCA.
   HCFORA - implementation of the tower thresholds and half-tower miscalibration.
  
   new include files in /inc
   HCTWTH.H, HCALCT.H

 ---------------------------------------------------------------------------------
 ! 980305 - galeph 306.3
            BECAREFUL : To simulate the LSCIntillator correctly you need 
                        ADBSCONS => 234

   LSCHIT - The routine and the database bank should change simultaneously.
            Since this simulation is more realistic, it should be applied
            in future simulations of any year. The main difference is that a
            particle hitting the scintillators give rise to a finite number
            of photoelectrons (on average 0.6), instead of a Gaussian signal.
            This explains why there half the time is signal in only one of
            the two PMs looking at each scintillator.(P.Hansen)

 ! 980225 - galeph 306.2
            BECAREFUL : To simulate 96-97-98 MCarlo it must be used with
                        ADBSCONS => 233

   AGGEAN - use the xPCO bank to introduce TRAP parameters(6-11) in GEANT.
 
 ! 980113 - galeph 306.1
            BECAREFUL : To simulate 98 MCarlo it must be used with
                        ADBSCONS => 232

   LCIRUN, AGLCAL - replace the lead absorber in front of the LSCIntillator
                    with a tungsten one.
 
 ! 971219 - galeph 306
            BECAREFUL : this version MUST be used with adbscons => 230

   AGVDET - use same geometry description for VDET 91-95
   AGTPCH - add TPC resistor chain and laser optics.
            augment the reinforcing ring on the inner field cage.

 ------------------------------------------------------------------------
 ! 970822 - galeph 305.3
   ASGPAR, ASWKIN - remove bugs which prevent new particles with 
                    geant# > 52 and .ne.NOTRK(=100) to be stored properly.

 ! 970505 - galeph 305.2
	    BECAREFUL : this version MUST be used for 97 MCarlo
                        it can handle the LCAL scintillator if in DB.
                        To simulate 97 MCarlo it must be used with
                        ADBSCONS => 224 
                        The LSCIntillator is NOT in ADBS224.

   AGLCAL - in modifying the code to remove the LSCintillator from the
            LCAL geometry, 4 screws were lost.
            restore them.

 ! 970429 - galeph 305.1
	    BECAREFUL : this version MUST be used for 97 MCarlo
                        it can handle the LCAL scintillator if in DB.
                        To simulate 97 MCarlo it must be used with
                        ADBSCONS => 223

   AGLCAL - do NOT STOP if LCAL scintillator not on DB.
   CAPSTO - replace a call to ALBOS with a call to ALTELL.
   

 ! 970317 - galeph 305.0
	    BECAREFUL : this version MUST be used for 97 MCarlo
                        it contains the LCAL scintillator.
                        To simulate 97 MCarlo it must be used with
                        ADBSCONS => 222

   AGLCAL - add LCAL scintillators.
   GUSTEP - call LSCHIT to store LCAL scintillator hits.
   LCIRUN, LCBKPR - introduce LCAL scintillators.
   LCSHOW - corrections to limit the amount of fluctuations in the shower 
            parameters, and to optimize parameters for high energy.
   LSCHIT - new routine to store LCAL scintillator hits.

 -------------------------------------------------------------------------
   GAL304   First version maintained with CVS.
            flags are -DALEPH_DEC on AXP/VMS
                      -DUNIX -DALEPH_${VENDOR} on (DEC SGI HP)

 ! 961113 - galeph 304.11
	    BECAREFUL : This version can handle both VDET description
                        VDET95 description from ADBSCONS < 219
                        VDET96 description from ADBSCONS => 219
                        older version of galeph CANNOT handle VDET96
                        ===> DON'T RUN galeph < 304.11 with ADBSCONS > 218   
                             ===============================================

 ! 961113 - galeph 304.11
            introduce VDET96 a new GEANT description of VDET.
   AGVDET - calls AGVF95 when ADBSCONS < 219
            calls AGVF96 when ADBSCONS => 219
   AGVF96 - positions faces into VDET volume.
   GUSTEP - in geantino parametrization in LCAL check that FSENS is true
            AND igsvol=idetjo(5)
   USER   - cut user.F in pieces, remove user.F, add usieve.F, usceve.F,
            usirun.F, uscrun.F, usijob.F, uscjob.F
   ASDUMMY- cut asdummy.F in pieces, remove asdummy.F, add askusi.F, askuse.F

 ! 961009 - galeph 304.10
   VFNDEL - access VDEP bank correctly.

 ! 960822 - galeph 304.9
   ALDUMMY, USER, LCXYPA, MUINCB, MUINCE, MUINCM - add #ifndef DOC.. #endif
   ITHIT  - remove call to ITAMP  (dummy routine).
   MUFLHT - remove call to MUDSTP (dummy routine).
   MUHIT  - remove call to MUDTUB (dummy routine).
   ITAMP, MUDSTP, MUDTUB - remove these dummy routines.

 ! 960624 - galeph 304.8
   HCSHOW - reset IHCPOR hcal portion no. and IHCMOD hcal module no.
            to those where the shower starts between 2 shower spots.
               
   960611 - galeph 304.7
   ASKTRK - correct definition of UPTPC
            
   960531 - galeph 304.6
   LCSHOW - code introduced in 304.3 was not the right one.

   960529 - galeph 304.5
   AGVDET - wrong setup code used for 9501.         

   960528 - galeph 304.4
            a part of the corrections made to built gal303 were missing.
   CATINO, ASKTRK - store vertex in GPOS (mandatory for GEANT321)
   GCKING - add common /GCKIN3/
   AGTPCH - adapt TPC gas mixture to Geneva conditions.
   HCFITU - add some protections
   ITDTGO - add some protections
   VAENCL, VDFILL - use different encoding schemes for old and new VDET.

   960523 - galeph 304.3
   GUSTEP, LCHIT, LCSHOW - implement geantino parametrisation in LCAL
            as it is done in ECAL (M.N.Minard)               

   960520 - galeph 304.2
   AGGEAN - in case of error call PRTABL (bank,IW(jbank-2))
   AGEOME - call AGSATR which calls AGSMBA if necessary.
   AGVDET - call AGVF92 in case of 91 geometry.
   AGCHCK - remove a conditionnal jump which is now made in ASREDC.
   ASREDC - it is not allowed to suppress SATR or SAMBA with 
            GEOM 'SATR' 0 because it is passive material.
   SAASIG - remove double quotes in a WRITE statment.
              	
   960513 - galeph 304.1
   ASKTRK - do not keep primaries in mask region (Beam pipe region)
   AGLCAL - add 2mm of Lead at inner circumference from Oct 95 onwards
   
   960508 - GALEPH 304.0
 * corr no.1 to galeph 30.3
   introduce GERA data card to allow very specific studies.   
 * corr no.2 to galeph 30.3
   EHSITW - to call EVERIF.
   EVERIF - new routine to check tower address consitency. 
 * corr no.3 to galeph 30.3
   replace TIMAx by TIMEx
   remove tpg*.h which are stored in ALINC. 
 
         In /geom
   AGTPCH - move #if defined(INTER) to another place because
            cpp on HP does not like #if defined between continuation 
            cards.
 ----------------------------------------------------------------------- 
 ! 950821 - GALEPH 30.3
   BECAREFUL : because of the changes in VDET
               GALEPH 30.2 should NOT be used with ADBSCONS > 200 to
               simulate DATE > 93.
               To simulate DATE => 94 with all VDET improvments:
               USE GALEPH 30.3 with ADBSCONS => 201.

 * corr no.1
   corrections to adapt to cvs (FLR302A)
   VDCLU  - Stop clustering when the wafer boundary is reached
            (for VDET92, z-view)
   VDPRTE - create electron clouds only if the track element is
            in the active region
 * corr no.2
   VDFOOB -remove an old bug (Manoj)
        In *CD -> GUSER
   GUSTEP - call VDHITE at end of track in VDET.
   GDRAY  - GEANT modified version.

        In *CD -> GEOMNEWS
   AGVDET - set name of sensitive volume.

        In *CD -> VDETNEWS
   VDHIT,VDHITE,VDASIG,VDMKLO,VDMKTE -

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

 ----------------------------------------------------------
 ! 950512 - GALEPH 30.2
            1st version capable to simulate VDET95.
            To simulate VDET95 :
            - use ADBSCONS => 199  on test as ADBSTEST
            - put a card DATE 96. in your galeph cards file

        In *CD -> GALNEWS
   GALEPH - call ASIGAL to initialize GALEPH.
            call GPAW in interactive mode.
            call GZEBRA in NOT interactive mode.

        In *CD -> GUSENEWS
   GUINTI - is purged
   GAXEPH - reorganized to run with GEANT 3.21

        In *CD -> GEOMNEWS
   remove calls to GSORD which is now a 'do nothing' routine.
   AGGORD - remove the routine.
   AGSATT - new routine , copied from GUINTI which is removed.
            set volume attributes which are not set by *ATT bank.
   AGGORD - remove the routine, code is included into AGGEAN.
   AGGEAN - read *ATT banks and call GSATT.
   AGEOME - call AGSATT before closing the geometry.
   AGVF92 - '92 vdet face geometry.
   AGVF95 - '95 vdet face geometry.
   AGVDET - call AGVF92 or AGVF95 depending on the year.

        In *CD -> SKELNEWS
    ASIGAL - new routine to initialize GALEPH , called by Main program.
    ASIEVE, ASPRUN - move reading of input file from ASPRUN to ASIEVE.

        In *CD -> VDETNEWS
    VFNDMP - Take into account different multiplexing
             schemes between VDET92 and VDET200 for the computation
             of strip capacitances.

 ---------------------------------------------------------------
 ! 950420 - GALEPH 30.1
   use AUTONEST in HISTORIAN to facilitate transfer to CVS.
   update HCVLJJ, CAPAJJ, KRUNJJ

        In *CD -> VDETNEWS
   - VFNDEL, VDFOOB, VDMKLO : remove bugs

        In *CD -> SKELNEWS
    remove LEVEL 3 trigger
    call new LEVEL 1 trigger
    ASRUST - store GEANT version no. * 10000.
    ASIGAL - GALEPH initialization (new routine)

         In *SET -> IFINTER
    adapt to GEANX321

        In *CD -> ECALNEWS
   ECIRUN - read EDPA bank from DB, and call EUPPAR.
   EUPPAR - new routine
            update ECAL parameters from EDPA bank.

        In *CD -> TRIGNEWS
   remove the TRIG set : all routines are in ALEPH205

        In *CD -> LCALNEWS
 There has been problems with the simulated Energy
 spectrum in Lcal for qqbar events. While the data spectrum
 falls smoothly with energy, the simulated spectrum has a
 strange 'shoulder' at about 3 GeV.

 Another complaint is that the energy in wireplanes
 in the MC look like discrete lines when plotted in
 fine bins of a few MeV.

 a) A new shower parametrization, used
    for the last Bhabha production, is incorporated.
    This makes better use of the results from Ecal testbeam
    at low energies where there was no Lcal testbeam.
    It also fluctuates some of the parameters, and reproduces
    better the longitudinal shower fluctuations seen in data.

    While this does not help on any of the above problems,
    it does make the EM showers a bit more realistic.
    The parameters of the new shower is given in a data-
    base bank LSHO, which I have submitted to Francoise.
    I have also changed the bank LECA 1 so that the energy
    is calibrated like data with 45.5 GeV electrons.

 b) The mysterious shoulder turns out to be a minimum ionising
    peak from hadrons that are tracked with GEANT through the
    volume. The energy deposited in each plane by the
    routine LCTRAK was way too high (typically 56 MeV).
    I have reduced it to 9.1 MeV (the min ionising energy
    in bank LSHO). This number may change a little after
    more studies.

 c) The energy is quantised in 40 MeV packages. This
    causes the right sampling fluctuations in EM showers
    and keeps down the computertime. This is why discrete
    lines are seen in the plots.
    The 40 MeV corresponds reasonably well to the zero
    suppression in the pads, but the wires are actually 16
    times more sensitive.

    Each "hit" in LCSHOW is weighed by a
    factor which is Poisson fluctuated around 16, and
    also multiplied the hit multiplicity in LCTRAK by 16.
    In LADC I divide again by 16. This way the wire energy
    is better reproduced - especially for min ionising
    tracks, and some smearing of the lines is provided.

 d) The end-of-job summary is changed so that it becomes
    meaningful.

   -----------------------------------------------------------------
 ! 941121 - GALEPH 30.0
   BECAREFUL ===> this version requires ALEPHLIB => 20.4

   remove OLD VDET code.
   redesign VDET package using NEW VDET geometry package.

   Suppress manipulation of lists of strips
   Process one wafer at a time, use array of strips
   Process one module at a time, use array of readout channels

   Include correct treatment of Landau fluctuations inside silicon
   (optional)

   Details are given in Aleph note :  ALEPH 94-152, MINIV 94-05

 ------------------------------------------------------------------------
 ! 941115 - GALEPH 25.7
  * corr.file # 1
    increase ZEBRA size to run GEANT321
    add a protection in CAHERB
  * corr.file # 2
    add many protections to run cosmolep
    ASCTRA,ASKTRK,ASREVE,HCHISE,MUSGNL,MUSTSG
    increase BOS size
    increase TIMEST argument

----------------------------------------------------------------
 ! 940111 - GALEPH 25.6
        In *CD -> SCALNEWS
 Modifications by B.Bloch  May 1993 including update of SIDOC deck
  o New comdeck SICONST with constants loaded from Data Base
  o New subroutines :
    SIZINI  Get zero suppression scheme and conversion constants
    SITRA3  generate transverse shower distribution in Sical (3 Gauss.)
    SIDFRT  initialise transverse em-shower distribution in Sical
  o Modifications in :
    SIPARM: update parametrisation common content
    SIBOOK: update histos
    SIDFPA: update parametrization constants
    SIIRUN: get BANKS from Data base for all constants
    SISIDI: apply Zero suppression on Raw Data
    SITRAK: use constant from Bank
    SISHOW: update parametrization steering
    SITRAN: update transverse parametrization
    SITRIG: apply Zero suppression to SIFO bank

        In *CD -> ECALNEWS
   ECIRUN : get ENNO bank from DB using setup code
   EDSNOI : use ENNO bank if it exists, otherwise use data statment

        In *CD -> GALNEWS
   GALEPH - increase size of ZEBRA common to fit SiCAL geometry.

        In *CD -> CALONEWS
   * corr no. 1
   CAASBS -  a wrong version was introduced in GALEPH 255 corrected
             in corr.file no.1

   -----------------------------------------------------------------
 ! 930910 - GALEPH 25.5
        In *CD -> CALONEWS
   CAPGET  - calls new routine CAABSB
   CAABSB  - new routine to stop geantino when all energy is deposited.
        In *CD -> SKELNEWS
  Modifications activated only with flag IFINTER  (B.Bloch May 1993)
    ASPRUN: update to call KUWHAT instead of GINTER
    GALEPH: update Zebra common length for interactive graphic version
        In *CD -> GUSENEWS
    GUINTI : call AGGATT for the ITC banks (B. Bloch May 1993 )
        In *CD -> GALNEWS
    GALEPH - add a CALL TIMEST(99999.) to please new TIMEL
             routine on VAX when running interactively
        In *CD -> GEOMNEWS
    AGEOME : update description of central region     (B.Bloch May 1993)
    AGITCH : implement geometry from Data base banks  (B.Bloch May 1993)
    AGHCAL : update materials (L.Silvestris)
        In *CD -> ITCNEWS
  Modifications for new drift time parameterisation.
  Database bank IDTC is replaced by IDRP to allow higher order
  polynomial.  For backwards compatibility versions of IDRP for old
  Monte Carlo data are provided which duplicate the existing IDTC
  banks.   NB. Requires ADBSCONS >= 183
  purged   IDTCJJ
  modified IJJPAR, ITPARC
           ITDCNS, ITDRFT, ITDTGO, ITIRUN, ITPGEO, ITRES
        In *CD -> HCALNEWS
   HCCONG, HCDIGI : cleaning of the code
   HCHIT  : suppress the possibility (never used) to create a geantino.
            to develop hadronic shower.
   HCSHOW : improvments in the hadronic shower development.

   -----------------------------------------------------------------
 ! GALEPH 25.4   930416   18:25:21
         in GAL253 CORR
   921001:1 - change in SiCAL
   921007:2 - GALEPH: suppress call to ALGTENV replace it by GETENVF
   921104:3 - suppress correction in VDET introduced in corr # 1
              create VDET face 92
              VDNOCL: redefine a row index after bank extension
   921119:4 - VDNOCL: reset ELNOI and ELNOP to zero to avoid
                      edge effects
              GALEPH: redefine length of GEANT ZEBRA array to fit SiCAL
   930113:5 - LCFRAL: compute XOUT when there is a full track element
                      (ITRKEL(8).NE.1)
   930113:6 - HCAL mods in shower development
   930208:7 - CAPGET remove correction introduced in previous
                     correction file

        In *CD -> GALNEWS
   suppress all references to CRAY
   replace ALCONS by ALBITW and ALCONS without any reference to the
   number of bits in a word.
   new historian flag BIT64 to select 64-bit words machine.


        In *CD -> SKELNEWS
    to avoid linking problems on UNIX machines, the KINE options 'LUND'
    ans 'SJET' are removed.
    They can be replaced by KINE 'USER' or by reading a KINGAL file
    ASKLUN, ASKLUI, ASKJET - purged
    ASREDC, ASRTYP, ASKINE - modified

        In *CD -> GEOMNEWS
   AGCHCK, AGGEAN, AGGATT, AGGORD:
   calls ALGTDB instead of MDARD with the setup code to avoid to
   repeat on the data base geometry banks with different NR but
   with identical content.
   AGCHCK, AGSCAL, AGVDET:
   change test on setupcode from .EQ.last to .GE.last

        In *CD -> ECALNEWS
   ECIRUN : use ALGTDB instecd of MDARD to get bank depending on
            the setup code

        In *CD -> HCALNEWS
   HCFITU : call HCTKSP with different argument names to please
            SHIFT compiler

        In *CD -> LCALNEWS
   LCIRUN : use ALGTDB instecd of MDARD to get bank depending on
            the setup code

        In *CD -> VDETNEWS
   VRDGAL,VDDIGI : use ALGTDB instecd of MDARD to get bank depending
                   on the setup code
   VDCLU,VDNOCL  : setupcode < or = LROWS('VDEL')

 -----------------------------------------------------------------
 ! GALEPH 25.3   920910   18.00
   ++++++++++++++++++ BECAREFUL +++++++++++++++++++++++++++
              GALEPH 25.3 needs ALEPHLIB 14.0
              it should be linked to GEANT 315
   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   corr. in  :
     1         VDHIT   do not retain hits with ENER=0.
               ASxxxx  get PART bank before run intialisation
               ASWRTP  drop T-list before using it and then make T=0
               ASREAD  when no input file make sure RUNH comes 1st
                       on C-list.
     2         ITTOFE  limit Z hit position to the wire max. length
               ISBEND  when Z=limit return 1st or last TDIF value
     3         ASIMOD  keep all detectors in the geometry even when
                       they are not required on a SETS card.
               GALEPH   set IW(5) and IW(6).
               ASINIT   set LINPIO and LOUTIO as IW(5) and IW(6).
               ASIGEA   change TCUT(3)=CUTHAD from 10Mev to 6Mev
                        when using GEANT 315.
               AGVDET   same vdet face geometry in 91 and 92.
     4         ASINIT   do not kill HCAL dead tubes in GALEPH,
                        it is done in QMUIDO.
     5         ASKTRK   redefine TPC region for the 92 beam pipe.
     6         CAGERB   reset the output argument NRJDP=0 when
               CAHGRB   entering the routine.
               EHSHOW   fill a new row when there is energy to
                        deposit (NRJDP .gt. 0)
     7         ASIEVE   print correctly the trigger # being processed
                        when reading an input file and using NEVT card.
   mods in :   ASREDC   suppress opening of data cards file
               GALEPH   move it to GALEPH
               HCCRPL   decode plane # for different period.
               HTDEAD   take into account the shift of tubes in barrel.
               VDCLU    small bug

   -----------------------------------------------------------------
 ! GALEPH 25.2   920325   18.00

   GAL:GAL252.HLB, .CORR, _.FOR, _B.FOR, .OLB, _D.OLB, .INPUT,
                   .NEWS/
   GAL:GCOR252.FOR, .OBJ(in debug mode)

   GAL252 OLDLIB GAL, TXTLIB, CORR, NEWS, GEO
   GCOR252 FORTRAN GAL, TEXT GAL

   GAL252 CRAYFOR CRAYXU.401, CRAYLIB, GCOR252 CRAYFOR, OBJ
   $KEEP/gal252.lib    $KEEP/gcor252.obj

   ++++++++++++++++ BECAREFUL +++++++++++++++++++++++++++
           GALEPH 25.2 needs ADBSCONS 165
   remember that ADBS8990 DAF is forseen to analize REAL DATA
   taken in 1989-1990.
   GALEPH should always use ADBSCONS DAF.
   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   mods in     : SKEL set
                 reorganization to use the ALPHARD package.
                 TRIG data card is removed and replaced by
                 NEVT, SEVT data cards
                 NEVT card refers to serial event number on input file
                 SEVT card refers to real trigger number
                 DEBU card refers to real trigger number

                 i.e. - TRIG 1 100    is replaced by   NEVT 100
                      - TRIG 5 10     is replaced by   NEVT 5 -10
                      - TRIG 1 100 10 is replaced by   NEVT 100   and
                                                       DEBU 0 0 10
                      - TRIG 1 100 1  are replaced by  NEVT 100   and
                        DEBU 1 10                      DEBU 25 125 1
                        if 1st trigger on input file has number 25
                 introduce DRAW Historian flag to protect calls
                 to GEANT drawing package
                 when selecting DRAW the following routines have to
                 be recompiles: ASCEVE, ASIGEA, ASIPAC,ASTDEB.
                 AGHCAL in GEOM set
                 make sure that there is no overlap regions in HCAL.
                 GEOM set
                 check the GEANT version number where it is necessary.
                 TPCOOR, TPWSUM
                 suppress reference to FASGAL and IEV1JO
                 HCINDU in HCAL
                 stop processing in case of stupid POISSN response.
                 HCCOIN in HCAL
                 set module number IHCMOD
                 ITC set
                 Assume all database banks are accessed via running
                 period scheme, previously only IGEO was.
                 Use new database banks IXCR, IXBW to set correct
                 trigger conditions for the ITC.
                 Modify Z resolution and S-bend simulation to use IZRS
                 and IZNL (new bank).
                 Replace use of IRES by IRRF for r-phi resolution.
                 Use new database bank IEFF to specify wire inefficiency
                 Where any new database banks are used for 1992 onwards
                 suitable constants are chosen for the previous running
                 period for backwards compatibility.
                 GRANEQ, QNEXT,ASDUMMY in GAL set
                 suppress misplaced or unecessary SAVE statment.
                 EHGEOB, EHGEOE in ECAL set
                 the decision flag to parametrize or not incident e+/e-
                 was not reset at the entrance of these subroutines
                 EDTONO in ECAL set
                 add protection against work bank too short
                 GUSTEP in GUSER set
                 allow track length up to 20m. instead of 10m.
                 before ( necessary for tracking cosmics events)
                 AGMEDI
                 use GEANT version to cope with the change of argument
                 type in GFTMED between GEANT 313 and 315
  new
  comdecks :     IRRFJJ, IZNLJJ, IZRSJJ, IEFFJJ, IXBWJJ, IXCRJJ
  purged
  comdecks :     ITDCJJ, IWSTJJ, IZSCJJ
  new
  subroutines:   introduce a *DK ALDUMMY in GAL set with ALEPHLIB
                 dummy subroutines referenced by the ALPHARD package
                 but never called by GALEPH :
                 AUNPCK - GALEPH does not unpack POT banks

   -----------------------------------------------------------
 ! GALEPH 25.1   920123   18.00

   GAL:GAL251.HLB, .CORR, _.FOR, _B.FOR, .OLB, _D.OLB, .INPUT,
                   .NEWS/
   GAL:GCOR251.FOR, .OBJ(in debug mode)

   GAL251 OLDLIB GAL, TXTLIB, CORR, NEWS, GEO
   GCOR251 FORTRAN GAL, TEXT GAL

   GAL251 CRAYFOR CRAYXU.401, CRAYLIB, GCOR251 CRAYFOR, OBJ
   $KEEP/gal251.lib    $KEEP/gcor251.obj

   mods in     : add SAVE statment in every subroutines.
                 GEOM and FAST
                 yank AGFAST and FAST set which are obsolete
                 TPCOOR
                 suppress reference to FAST common block.
                 VDAABB, VDROST
                 small bugs removed
                 GALEPH
                 increase GEANT ZEBRA array  to 400000 words.
                 AGHCAL, HRDGAL
                 adapt HCAL tracking parameters to GEANT version and
                 computer used.
                 HCINDU, HCFORD
                 remove bugs.
                 HCNAMC, HCASIG, HCNAMI
                 create a new bank HLWD to remember hit position along
                 the wire.
   new
   routines    : GSTMED in GUSER set
                 GEANT subroutine modified to not use the old
                 parameter DMAXMS as new parameter STEMAX
                 ( STEMAX is set to 1.E9) when running with
                 GEANT version > or = 3.15
                 HCHLWD in HCAL set
                 fill HLWD bank to remember the hit position
                 along the tube.

   -------------------------------------------------------------
 ! GALEPH 25.0   911010   18.00
    ========> this version is identical to GALEPH 24.3 <========
    ========> BUT ADAPTED TO GEANT 3.14                <========

   GAL:GAL250.HLB, .CORR, _.FOR, _B.FOR, .OLB, _D.OLB, .INPUT,
                   .NEWS/
   GAL:GCOR250.FOR, .OBJ(in debug mode)

   GAL250 OLDLIB GAL, TXTLIB, CORR, NEWS, GEO
   GCOR250 FORTRAN GAL, TEXT GAL

   GAL250 CRAYFOR CRAYXU.401, CRAYLIB, GCOR250 CRAYFOR, OBJ
   $KEEP/gal250.lib    $KEEP/gcor250.obj

   mods in     : GCTRAK, GCJLOC
                 modified according to GEANT 3.14
                 GALEPH
                 increase LGB (GEANT ZEBRA array) to 300000
                 decrease LIW (BOS array) to 600000 on CRAY
                 ASGPAR
                 do not call GPART for particles with type=
                 NOtrackingmarker
                 ASIGEA
                 set  IGAUTO = 0 . if AUTO data card is there
                 set IGAUTO=1.
                 ASIRUN,AGEOME
                 print GEANT geometry banks after call to GPHYSI
                 ASRUST,AJOBJJ
                 add the GEANT version number * 100 in AJOB bank
                 AGHCAL
                 new cuts adapted at GEANT 3.14
                 use HGEA bank got from DB depending on the GEANT
                 version number
                 HRDGAL
                 use MDARD to get HCOS, HGEA, HTRE banks from DB
                 get HCOS and HGEA with NR=GEANT version number

 ----------------------------------------------------------------------
 ! GALEPH 24.3   911009   18.00
   GAL:GAL243.HLB, .CORR, _.FOR, _B.FOR, .OLB, _D.OLB, .INPUT,
                   .NEWS/
   GAL:GCOR243.FOR, .OBJ(in debug mode)

   GAL243 OLDLIB GAL, TXTLIB, CORR, NEWS, GEO
   GCOR243 FORTRAN GAL, TEXT GAL

   GAL243 CRAYFOR CRAYXU.401, CRAYLIB, GCOR243 CRAYFOR, OBJ
   $KEEP/gal243.lib    $KEEP/gcor243.obj

   mods in     : AGECAL, AGHCAL, AGMUCH, GUSTEP, HCCOIN
                 use GEANT division in geometry definition.
                 replace all 'MANY' volumes with 'ONLY' volumes.
                 AGVDET
                 calls new routine AGGEAN.
                 TPWSUM
                 prints the right time per event.
                 VDDIGI
                 replace digit pointer in VDTD with strip address.
                 VDNOCL
                 correct length of strips
                 ASxxxx, AGCHCK, AGEOME, AGHCAL, GUINTI
                 prepare introduction of SICAL.
                 SICAL gets nmber 9 in the list of detectors
                 TS is deleted from the list of components
                 COIL gets number 13 in the list of components
                 MUSPMD
                 bug correction
   new routines: SUBROUTINE AGGEAN ('xx',setup)
                 xx = 2-letter code of a detector
                 returns the setup code
                 builds the GEANT geometry for this detector
                 from xMAT,xVOL,xTME,xGPA,xMED data base banks
                 INTEGER FUNCTION AGMATE (NAME)
                 returns the GEANT material # known by its name
                 SUBROUTINE GETRAN (TRAN)
                 get the translation vector of HBLA when the
                 hit is in HBL1 or HBL2.

 ---------------------------------------------------------------
 ! GALEPH 24.2   910425   18.00
    ++++++++++++++++++++++++++++++++++++++++++
        BECAREFUL THIS VERSION REQUIRES :
                ADBSCONS =>151
     +++++++++++++++++++++++++++++++++++++++++
   GAL:GAL242.HLB, .CORR, _.FOR, _B.FOR, .OLB, _D.OLB, .INPUT,
                   .NEWS/
   GAL:GCOR242.FOR, .OBJ(in debug mode)

   GAL242 OLDLIB GAL, TXTLIB, CORR, NEWS, GEO
   GCOR242 FORTRAN GAL, TEXT GAL

   GAL242 CRAYFOR CRAYXU.401, CRAYLIB, GCOR242 CRAYFOR, OBJ
   $KEEP/gal242.lib    $KEEP/gcor242.obj

   mods in     : VDET
                 VPAR bank in read at 1st entry in VDAABB
                 VDEL bank is read at 1st entry in VDCLU, VDNOCL
                 VDEL bank replaces VELE bank
                 remove some bugs and simplify the code
                 HCAL
                 HCCRPL adopt online numbering
                 GEOM
                 AGTPCH remove bug in TPHF numbering and add some
                        materials.
                 AGBEAM wrong material in the end region.
                 AGECAL use division of space
                 AGHCAL use division of space, declare HBLA as MANY,
                        remove overlap between MUB1 and HBAR.
                 AGITCH small change
                 INTER
                 GEANT interactive graphics

 ---------------------------------------------------------------------
 ! GALEPH 24.1   910213   18.00
    ++++++++++++++++++++++++++++++++++++++++++
        BECAREFUL THIS VERSION REQUIRES :
                ALEPHLIB =>12.2 AND
                ADBSCONS =>140
     +++++++++++++++++++++++++++++++++++++++++
   GAL:GAL241.HLB, .CORR, _.FOR, _B.FOR, .OLB, _D.OLB, .INPUT,
                   .NEWS/
   GAL:GCOR241.FOR, .OBJ(in debug mode)

   GAL241 OLDLIB GAL, TXTLIB, CORR, NEWS, GEO
   GCOR241 FORTRAN GAL, TEXT GAL

   GAL241 CRAYFOR CRAYXU.401, CRAYLIB, GCOR241 CRAYFOR, OBJ
   $KEEP/gal241.lib    $KEEP/gcor241.obj

   mods in        : VDET
                    few bugs removed in digitizings
                    GEOM
                    new beam pipe for '91
                    a lot of screws in the LCAL + TPC support
                    GUINTI
                    introduce SATR
                    ECWSUM
                    update a print statment
                    HCAL
                    new calibration

  ----------------------------------------------------------------
 ! GALEPH 24.0   901213   18.00
    ++++++++++++++++++++++++++++++++++++++++++
        BECAREFUL THIS VERSION REQUIRES :
                ALEPHLIB 12.2 AND
                ADBSCONS 138
     +++++++++++++++++++++++++++++++++++++++++
   GAL:GAL240.HLB, .CORR, _.FOR, _B.FOR, .OLB, _D.OLB, .INPUT,
                   .NEWS/
   GAL:GCOR240.FOR, .OBJ(in debug mode)

   GAL240 OLDLIB GAL, TXTLIB, CORR, NEWS, GEO
   GCOR240 FORTRAN GAL, TEXT GAL

   GAL240 CRAYFOR CRAYXU.401, CRAYLIB, GCOR240 CRAYFOR, OBJ
   $KEEP/gal240.lib    $KEEP/gcor240.obj

   mods in        : VDET
                    new description of the geometry for 90 and 91.
                    ITC
                    remove a bug in the hit computation
                    introduce ITC geometry for 91
                    ASIEVE
                    remove a bug when  skipping of events
                    AGCHCK
                    restore SATR geometry
                    ASINIT,ASREADC
                    make use of the DATE data card
                    create ASIM bank in the run header record
                    APOLLO flag necessary in some routines
                    ECAL
                    changes in the digitization
                    generate noise in the adjacent towers.

  ---------------------------------------------------------------
 ! GALEPH 23.9   900922   18.00
   GAL:GAL239.HLB, .CORR, _.FOR, _B.FOR, .OLB, _D.OLB, .INPUT,
                   .NEWS/
   GAL:GCOR239.FOR, .OBJ(in debug mode)

   GAL239 OLDLIB GAL, TXTLIB, CORR, NEWS, GEO
   GCOR239 FORTRAN GAL, TEXT GAL

   GAL239 CRAYFOR CRAYXU.401, CRAYLIB, GCOR239 CRAYFOR, OBJ
   $KEEP/gal239.lib    $KEEP/gcor239.obj

   mods in         : VDET
                     better new geometry.
                     HCAL
                     introduce killing of unrecoverable dead tubes.
                     LCAL
                     move internal wire-supports to the right place
                     SATR
                     better description of the material in front of
                     LCAL. Alignment constants taken from LALI bank
                     instead of SPOS.

   -----------------------------------------------------------
 ! GALEPH 23.8   900606   18.00
   GAL:GAL238.HLB, .CORR, _.FOR, _B.FOR, .OLB, _D.OLB, .INPUT,
                   .NEWS/
   GAL:GCOR238.FOR, .OBJ(in debug mode)

   GAL238 OLDLIB GAL, TXTLIB, CORR, NEWS, GEO
   GCOR238 FORTRAN GAL, TEXT GAL

   GAL238 CRAYFOR CRAYXU.401, CRAYLIB, GCOR238 CRAYFOR, OBJ
   $KEEP/gal238.lib    $KEEP/gcor238.obj

   mods in         : VDET
                     new geometry. The old one cannot be used.
                     ADBSCONS DAF 128 must be used. When using
                     older version of the data base the VDET must
                     be switched off.
                     ITHIT
                     has been completely rewritten to improve the
                     number of hits found, especially for non-radial
                     tracks. The ITHT bank has been replaced by the
                     IHIT bank and the data describing a hit has been
                     changed.
                     Calculation of the drift time and R-Phi resolution
                     has been changed to conform to the scheme used by
                     JULIA.
                     The relevant database banks are IDTC and IRES.
                     SATR
                     introduce alignment corrections
                     FAST
                     because of new VDET and ITC code the FAST package
                     does not compile any longer. For this reason the
                     following routines as been YANKed: AGFAST,FAIHIT.
                     LCAL
                     Placed the energy depositions always at the WIRE
                     planes.
                     Remembered to transform ALSO THE DIRECTION into the
                     loca system - and not just the impact point of a
                     GEANT track.
                     Instead of trying to improve the material description
                     in front of Lcal, I have simply mapped the differences
                     in energy response between data and Monte Carlo into a
                     new table LCCA, which is used to correct the Monte
                     Carlo energies (in LCADC).
                     ASKTRK, EHDEPT
                     CRAY correction to get CHARACTER variable from
                     GEANT hollerith variable. This error affected
                     the history of energetic Bremstrahlung.

   -------------------------------------------------------------
 ! GALEPH 23.7   891123   18.00
   GAL:GAL237.HLB, .CORR, _.FOR, _B.FOR, .OLB, _D.OLB, .INPUT,
                   .NEWS/
   GAL:GCOR237.FOR, .OBJ(in debug mode)

   GAL237 OLDLIB GAL, TXTLIB, CORR, NEWS, GEO
   GCOR237 FORTRAN GAL, TEXT GAL

   GAL237 CRAYFOR CRAYXU.401, CRAYLIB, GCOR237 CRAYFOR, OBJ
   $KEEP/gal237.lib    $KEEP/gcor237.obj

   mods in             : ECAL
                         few bugs fixed
                         HCAl
                         update some tracking parameter to follow
                         GEANT 3.13 improvments (CUTH=0.02).
                         LCAL
                         better handling of dead zone
                         GEOM - SATR
                         add some materials in front of LCAL
                         ASxxxx
                         to make use of RLEP bank which
                         contains the LEP energy.
                         ITC
                         new trigger

   ------------------------------------------------------------
 ! GALEPH 23.6   890900   18.00
   GAL:GAL236.HLB, .CORR, _.FOR, _B.FOR, .OLB, _D.OLB, .INPUT,
                   .NEWS/
   GAL:GCOR236.FOR, .OBJ(in debug mode)

   GAL236 OLDLIB GAL, TXTLIB, CORR, NEWS, GEO
   GCOR236 FORTRAN GAL, TEXT GAL

   GAL236 CRAYFOR CRAYXU.401, CRAYLIB, GCOR236 CRAYFOR, OBJ
   $KEEP/gal236.lib    $KEEP/gcor236.obj

   Becareful the correction file has been renamed. It has the same
   name on VAX CRAY and IBM : GAL:GCOR236.FOR, .OBJ
                              GCOR236 FORTRAN GAL, TEXT GAL
                              $KEEP/gcor236.obj

   GALEPH does not contain the RDST package which is an independant
   library kept as:
         GAL:RDST.HLB, .FOR, .NEWS, .OLB, _D.OLB
         RDST OLDLIB GAL, NEWS GAL, TXTLIB GAL

   The GALEPH steering routines with the flag RDST ON are kept on:
         GAL:GRDST236.FOR, .OBJ / GRDST236 FORTRAN GAL, TEXT GAL

   RDST is called by GALEPH through the data card:
         PROC  'RDST'
   in this case the program must be linked with:
         GAL:GRDST236.OBJ / GRDST236 TEXT GAL
         GAL:RDST.OLB     / RDST TXTLIB GAL
   GALRUN has been modified accordingly.

   BCS size has been increased to 900000 words.

   ECAL geometry is set by default to 2 (3 stacks) : the data card
        GEOM   'ECAL'  2    is no longer necessary

   mods in              :LCAL
         - the default running mode is changed again to be without
           noise generation. It does not affect the important
           features of the data anyway. And you gain a lot on
           both speed and size by having no noise. (LCDIGI)
         - If you choose to run with noise (LCNOIS), then the
           noise generation has also been changed a bit
           The energy dependent noise and coherent noise are
           fluctuated independently in the four modules, and it
           runs faster since only non-empty channels are considered.
         - Energy is not deposited in the dead channels listed
           in the database. (LCSHOW)
         - The previous code assumed that the Event Builder
           did not use floating point operations. Well it does,
           so I changed that (although it makes little difference)
           (LCADC).
         - We have observed a large difference between the wire
           and pad energy near the wire support. This I have
           tried to simulate by increasing the live area of the
           wires which is then different from the live area
           of the pads. (LCXYPA)

   new package           : MUON
            complete rewriting of the package to follow the new
            geometry

   mods in               : HCAL
            correction of bugs in history banks, and tube address

                           SKEL
            'S'list is used to collect banks belonging to a run, in
             order to drop them at end of run.(mods in ITC and SATR)

                           TRIGGER
             Level 2 has been moved to ALEPHLIB 9.9
   --------------------------------------------------------------
 ! GALEPH 23.1   890627   18.00
   GAL:GAL231.HLB, .CORR, _.FOR, _B.FOR, .OLB, _D.OLB, .INPUT,
                   .NEWS/
   GAL:CORR231.FOR, .OBJ(in debug mode)

   GAL231 OLDLIB GAL, TXTLIB, CORR, NEWS, GEO
   CORR231 FORTRAN GAL, TEXT GAL

   GAL231 CRAYFOR CRAYXU.401, CRAYLIB, GCOR231 CRAYFOR, OBJ
   $KEEP/gal231.lib    $KEEP/gcor231.obj

   new VDET digitizings : is NOT implemented because of problems with
                          the code.
   new package          : bookkeeping can be called with BOOK data card.
   new routine          : TPCOOR
   mods in              : ITC digitizings
                          SATR digitizings
                          TRIGGER package, trigger level 1 routines are
                          purged, routines from the ALEPHLIB are used.
                          TCUT data card, cuts are kinetic energy cuts
                          (documentation DATDOC was wrong).
                          update AFID beam energy with the EVEH beam
                          energy (ASCEVE).
                          change calls to GSPART to use char. variable
                          instead of integer (ASGPAR).
                          GALEPH - initialization of RANECU
   mods in              : ECAL - 3 stacks and mods in parametrization.
                                 treatment of minimum ionizing particles.
                                 several changes in digizitation part.
                          HCAL - add tube efficiency
                                 changes in digitization part: new banks.

   reorganisation of the historian library in view of the documentation
   the following sets have been defined:
      *SET GAL,GALNEWS.GALEND
      *SET SKEL,SKELNEWS.SKELEND
      *SET DRAW,DRAWNEWS.DRAWEND
      *SET GEOM,GEOMNEWS.GEOMEND
      *SET CALO,CALONEWS.CALOEND
      *SET ECAL,ECALNEWS.ECALEND
      *SET FAST,FASTNEWS.FASTEND
      *SET GUSER,GUSENEWS.GUSEEND
      *SET HCAL,HCALNEWS.HCALEND
      *SET LCAL,LCALNEWS.LCALEND
      *SET MUON,MUONNEWS.MUONEND
      *SET RDST,RDSTNEWS.RDSTEND
      *SET SATR,SATRNEWS.SATREND
      *SET TPC,TPCNEWS.TPCEND
      *SET VDET,VDETNEWS.VDETEND
      *SET TRIG,TRIGNEWS.TRIGEND
      *SET DOC,DOCNEWS.DOCEND
      *SET HAC,HACNEWS.HACEND

   ---------------------------------------------------------------
   GAL:GAL230.HLB, .CORR, _A.FOR, _B.FOR, .OLB, _D.OLB, .INPUT,
                   .NEWS, .GEO/
   GAL230 OLDLIB GAL, TXTLIB, CORR, NEWS, GEO
   The correction files are kept as:
   GAL:CORR230.FOR, .OBJ(in debug mode) / CORR230 FORTRAN GAL, TEXT

   the version needs a .GEO files because of some banks not yet put
   on the data base: GAL:GAL230.GEO / GAL230 GEO GAL

   Trigger banks xxTR are built in DIGI process. That was true for
   ECAL and ITC, but not for HCAL and LCAL. The corresponding code
   has been moved from HCASIG/LCASIG to HCDIGI/LCDIGI. So it is now
   possible to reprocess the TRIGger without reprocessing the TRACking
   but the DIGItization has to be reprocessed too.

   Work has been done to modified word(4) of KINE and FKIN banks:
   the mass of the track replaces the energy.
   It is still possible to read KINGAL files created with ALEPHLIB
   version < 9.0 ( that means with the energy instead of the mass).

   KMACRO has been modified such that:
   ENERVK(JVK) is the energy of the track known by its BOS index JVK.
   PMASVK(JVK) is the mass of the track known by its BOS index JVK.
   PARMAS(JPA) is the mass of the aleph particle # JPA.

   A version # has been introduced in the correction file GALxxx CORR
   it is printed in the title and stored in the AJOB bank.

   Work has been prepared to be able to use RNDM or RANMAR or RANEQU
   as random number generator. It is forseen to use RANMAR in KINGAL
   and RANECU in GALEPH.
   To cope with this change the variable NRNDJO has been dimensioned
   to LRND=3 and IRNDJO(LPRO) to IRNDJO(LRND,LPRO).
   Routines RDMIN and RDMOUT are called with NRNDJO(1) or IRNDJO(1,n)
   such that if RNDM is used it will work as before, if RUNECU is used
   the 1st 2 elements of NRNDJO / IRNDJO will be set/used, if RANMAR
   is used then the 3 elements of NRNDJO / IRNDJO will be set/used.
   When using RNDM 1 seed is used to restart on a given event:
                   RNDM    234567
   When using RANECU 2 seeds are used to restart on a given event:
                   RNDM    3657489   77668943
   When using RANMAR 3 seeds are used to reastart on a given event:
                   RNDM    5486953   98100     0
   to start a GALEPH run using RANMAR and the specific lab. seeds:
                   RNDM    10001     123      -1

   For more details refer to B.Bloch who is in charge of the
   implementation of these generators.

   - update PARTJJ to cope with the new content of the PART bank.

   - introduce new history banks ESHI, EWHI, LSHI, LWHI
     the banks ETHT is derived from ESHI
               EWHT                 EWHI
               LTHT                 LSHI
               LWHT                 LWHI
     in calling a new INTEGER FUNCTION from the ALEPHLIB 8.92 :
               JxyHT = CAFIHT (NAxzHI,'xyHT')
     the following banks ARE written onto the output:
               ESHI, LSHI
     the following banks ARE NOT written onto the output:
               EWHI, ETHT, EWHT, LWHI, LTHT, LWHT
     the following banks DO NOT EXIST any longer:
               ETTD, EWTD, LTTD, LWTD

   - call ASRETP internally inside ASIEVE to skip events before the
     1st event to be processed.

   - new code for the ITC.

   - new code for the VDET (the geometry is still the old one).
   ---------------------------------------------------------------
   GAL:GAL220.HLB, .CORR, .FOR, .OLB, _D.OLB, .INPUT, .NEWS/
   GAL220 OLDLIB GAL, TXTLIB, CORR, NEWS
   The correction files are kept as:
   GAL:CORR220.FOR, .OBJ(in debug mode) / CORR220 FORTRAN GAL, TEXT

   THE VERSION WORKS WITH GEANT 3.12

   THE VERSION HAS BEEN RESEQUENCED
   The version before resequencing is kept as GAL210, only the
   Historian library is kept.
   -----------------------------------------------------------------
   GAL:GAL203.HLB, .CORR, .FOR, .OLB, _D.OLB, .INPUT/
   GAL203 OLDLIB GAL, TXTLIB, CORR
   GAL:TPCSIM.HLB, .OLD / TPCSIM OLDLIB GAL, TXTLIB
   the correction files are kept as:
   GAL:CORR203.FOR, .OBJ(in debug mode) / CORR203 FORTRAN GAL, TEXT
   GAL:TPCCORR.FOR, .OBJ(in debug mode) / TPCCORR FORTRAN GAL, TEXT

   The version requires ALEPHLIB 8.5

   There is a new GAL:GALRUN.COM / GALRUN EXEC GAL

   The following features have been implemented:
    - GET and SAVE data cards are obsolete. Use instead:
      FILI  'fname ftype fmode | alephtype | GIME ......'
      FILO  'fname ftype A  | alephtype'
      i.e. : FILI 'ALEPHDATA:FLRLUN01.EPIO'
             FILI 'FLRLUN01 EPIO *  | GIME PUBXU 403 Z'
             FILO 'SCWEEK:MCDIG312.DAT | NATIVE'
             FILO 'MCDIG312 DAT A | NATIVE'
    - to read a .GEO file:
      FGEO  'GAL:GAL201.GEO | CARDS'
      FGEO  'MYGEO 203 * | CARDS | GIME FLR 192 D'
    - to read a non-standard data base:
    - FDBA  'DBASE:ADBSCONS DAF107 '
    - FDBA  'ADBSCONS TEST109 * | DAF | GIME PUBXU 210 *'

    - Trigger Level 2 is implemented:TTHT is produced by default.
    - Trigger Level 1 is modified.

    - New HCALDES is implemented.
    - Historian flags HCDETAIL and HCAVERAG are removed.

    - primary track definition has been updated to include:
      * muons, daughters of primaries.
      * particles with a mementum above 40Mev in the ITC and daughters
        of primaries. (the previous cut was at 100Mev).

   ==================================================================
   GAL:GAL202.HLB, .CORR, .FOR, .OLB, _D.OLB, .INPUT/
   GAL202 OLDLIB GAL, TXTLIB, CORR
   GAL:TPCSIM.HLB, .OLB / TPCSIM OLDLIB GAL, TXTLIB
   the correction files are kept as:
   GAL:CORR202.FOR, .OBJ(in debug mode) / CORR202 FORTRAN GAL, TEXT
   GAL:TPCCORR.FOR, .OBJ(in debug mode) / TPCCORR FORTRAN GAL, TEXT

   TPCSIM can be called from GALEPH on request (data card: TPCSIM)
   the BOS array as been enlarged to 800000 words to contain TPCSIM
   banks.
   the GAL:GALRUN.COM / GALRUN EXEC GAL has been modified to call
   TPCSIM on request.

   the FXXX package can be called from GALEPH on request:
       data card  FXXX

   the definition of a PRIMARY track has been changed to be:
       a track which is a daughter of a primary with its origin in
       the central detector up to the outer wall of the TPC but the
       endplate of the ITC and the endplate of the TPC, with a
       momentum above 100Mev.

   a primary which produces a bremstrahlung or a delta ray which takes
   more than 15% of its energy will be stopped and a new particle will
   be recorded.

   an end vertex is recorded for charged primary tracks.

   bug corrected in LCTRAK, RDST, EHSHOW, HCFORA, AGECAL, AGLCAL

   the outside detectors (ECAL,LCAL,SATR,HCAL,MUON) are physically
   removed (geometry level set to 0) if they are not digitized ( do
   not appear ob the data card SETS).
   Remember that MUON detector is attached to the HCAL so you cannot
   require MUON without HCAL.
   It is not reasonnable to required HCAL without ECAL. In any case the
   computer time per event will increase if ECAL is not require because
   the shower parametrization is not turned on.

   geantino parametrization is ON by default for electrons/positrons.
   to use the previous parametrization give a data card:
       RUNC  'ECAL' 0 0 0 0 1

   GAL202 is still working with GEANT311.
   GAL210 should work with GEANT312 and be resequenced!!!!!!!!!!

   ----------------------------------------------------------------
   GAL:GAL201.HLB, .CORR, .FOR, .OBJ, _D.OBJ, .INPUT, .GEO/
   GAL201 OLDLIB GAL, TXTLIB, CORR, GEO
   The correction files are kept as:
   GAL:CORR201.FOR, .OBJ(in debug mode) / CORR201 FORTRAN GAL, TEXT

   The RDST option must load GAL:RDST.OBJ / RDST TEXT GAL
   The FAST RDST option must load GAL:FRDST.OBJ / FRDST TEXT GAL

   The version works with HBOOK4. Because of incompatibilities of
   GEANT and HBOOK4/HPLOT5, to run the interactive version of GALEPH
   one has to link with the previous libraries. To do so use the
   command file GAL:GALINT.COM instead of GAL:GALRUN.COM.
   Use the Historian flag INTER to select the interactive version,
   recompile the Historian set IFINTER.

   The GEO file contains LCAL banks which should go to the data base:
   wait for the A.Putzer's message. In the meantime use GAL201 GEO .

   Bugs are removed from  RDST, ASxx.

   New code in LCAL, RDST, GUFLD.

   EPIO and NATIVE files are written with BLKSIZE 32760. For EPIO files
   BOS has to be told through a call to BUNIT that the record length is
   16380 16-bit words.

   New Historian flag HCAVERAG to get average medium geometry and
   parametrization a la "Della Negra" in the HCAL.
   New Historian set IFINTER to get routines to be recompiled when
   Historian flag INTER is used.

   The code for the "geantino" parametrization is implemented for the
   ECAL/HCAL code writers ONLY.

   =================================================================
   GAL:GAL200.HLB, .CORR, .FOR, .OBJ, _D.OBJ, .INPUT, .GEO/
   GAL200 OLDLIB GAL, TXTLIB, CORR, GEO
   The correction files are kept as:
   GAL:CORR200.FOR, .OBJ(in debug mode) / CORR200 FORTRAN GAL, TEXT

   The RDST option must load GAL:RDST.OBJ / RDST TEXT GAL
   The FAST RDST option must load GAL:FRDST.OBJ / FRDST TEXT GAL

   This version works with ALEPHLIB 7.8

   The Historian flags TATINA, CASCADE, GHEISHA have been removed: GHEISHA
   is used by default. It is no longer necessary to specified the historian
   flag GHEISHA in the Historian input file.

   The GEO file is empty. It can be used for new data base banks (not yet
   on the data base) or to change the content of a data base bank. Banks
   which are read from the GEO file will be written onto the output file.

   Extra banks which have to be written onto the output file can be read
   either from the GEO file as before or from the CARDS file as long as
   the card UNIT 14 is removed and the extra bank cards are placed behind
   the card ENDQ.

   If no GEO file has to be read it is enough to suppress the card UNIT 14.

   Bugs are removed from MUON, HCAL, RDST, ASxx, KMACRO.

   Add DEDX simulation in RDST.

   Use OPENSQ to access the input file if any in ASREIN, This allows a
   READONLY access on VAX.

   =================================================================
   GAL:GAL199.HLB, .CORR, .FOR, .OBJ, _D.OBJ, .INPUT, .GEO/
   GAL199 OLDLIB GAL, TXTLIB, CORR, GEO
   The correction files are kept as:
   GAL:CORR199.FOR, .OBJ(in debug mode) / CORR199 FORTRAN GAL, TEXT

   This version works with ALEPHLIB 7.7

   The GEO file has changed.

   Bugs are removed from HCAL, RDST, ASxx.

   ITDCNS, ITRGPA have been modified so that they no longer require banks
   from the geometry file. Default values are set in these routines. If the
   banks ITCB or ITPA are provided in the steering datacards file the
   values from there will override the defaults.
   ITHIT now checks for stack tracks in the ITC. Hits due to stack tracks
   are flagged by setting the track number negative.
   ITDRFT no longer calculates random ionisation centres along track
   elements as a component of the ITC resolution.  Instead, the drift time
   corresponding to the closest point to the sense wire is used. All the
   resolution effects are simulated in ITRES.

   A new correlation bank, IDHR, has been added to relate digits to hits.
   (The banks already existing relate digits <=> tracks.)

   The ITC chapter of the writeup has been updated.

   Implementation of the RUNR as 1st bank of a run record.
   At the same time implementation of a new facility:
     write the RAW data and the MC data on 2 different BOS records.
     The default stays as before (1 record per event)
   new intrinsic function in KMACRO:
     MOTHVK(jvk) = mother track # of a track known by its BOS
                   index jvk.

   Clean up the /JOBCOM/ and /JQCOM/ : remove unused flags and name
   indices.
   Remove the non-tabular bank ASRU (galeph run header) . Write
   instead the following tabular banks :
     ACUT : galeph tracking cuts
     AFID : galeph fiducial parameters
            magnetic field value = RTABL (JAFID,1,3) should be used
            by downstream programs (TPCSIM, JULIA, etc..)
     AJOB : galeph job conditions
     AKIN : galeph kine parameters
     APRO : galeph process conditions
     ARUN : detector run conditions
     ATIT : galeph run title
   Full description can be found with SBANK.

   Use OPENDB to access the data base in ASREDB

   Do not use the FIDU data card unless you want to change the defaults
               Rmx=650. Zmx=600. magfld=15.  Ecms=90.

   Do not use the TCUT data card : cuts are set by the algorithm used.

   Check carefully the run summary to be sure that data card parameters
   were not mixed up.

   =================================================================
   GAL:GAL198.HLB, .CORR, .FOR, .OBJ, _D.OBJ, .INPUT, .GEO/
   GAL198 OLDLIB GAL, TXTLIB, CORR, GEO
   The correction files are kept as:
   GAL:CORR198.FOR, .OBJ(in debug mode) / CORR198 FORTRAN GAL, TEXT

   This is still an intermediate version, the ECAL is still using
   data statments in the ECALDES, and the ITC still needs banks from
   the .GEO file.

   This version works with ALEPHLIB 7.5 .

   So there is a new .GEO file which contents 2 banks for the HCAL
   and few banks for the ITC.

   This version has been installed with HCDETAIL flag ON, full
   generator ON in HCAL with the following cuts in the HCAL part:
     CUTGAM = 2.4MeV, CUTELE = 2.4MeV
   in the ECAL the parametrization for the electrons in ON with the
   default cuts:
     CUTGAM = 5.MeV, CUTELE = 5.MeV, CUTMUO=CUTHAD=CUTNEU = 10.MeV

   The HCAL reading of the data base has been reorganized to be later
   on incorporated into the ALEPHLIB.

   There is a new VDET package which reads the data base and implements
   the new design of the VDET.

   Bugs has been found everywhere

   A new GALEPH write-up will be published.

   ===================================================================
   GAL:GAL197.HLB, .CORR, .FOR, .OBJ, _D.OBJ, .INPUT, .GEO/
   GAL197 OLDLIB GAL, TXTLIB, CORR, GEO

   This is still an intermediate version, the data base is not
   yet fully implemented.

   There is a new .GEO file which still contents few banks.

   This version has been implemented with HCDETAIL flag ON,
   full generator ON in HCAL and the following cuts on particle
   energy:  TCUT   0.002   0.002   0.01   0.01   0.01
            *      photon  elec    neuth  chargh muon
   and the parametrization of the electrons in ECAL.
   The time per event is longer but the ECAL/HCAL test beam
   data are well reproduced.

   ITC  - remove code using ITCA and ITCB banks
   ECAL - new ECALDES (ALEPHLIB 7.5)
          new geometry description ready to accept 3 different
          media for the 3 different stacks.
          set of standard histogramms is implemented. Look at
          ECDOC for the definition.
          new ECAL documentation kept on CERNVM:
              ECALDOC LISTING GENERA
   HCAL - bug corrections
          changes in few geometrical numbers
   MUON - use now the data base
   LCAL - use now the data base


   -----------------------------------------------------------
   GAL:GAL196.HLB, .CORR, .FOR, .OBJ, _D.OBJ, .INPUT, .GEO/
   GAL196 OLDLIB GAL, TXTLIB, CORR ,GEO

   It is an intermediate version which used the data base to get
   the following banks:
       - PART, GDEC
       - ITC geometry and readout
       - TPC geometry
       - HCAL geometry and readout
       - passive materials BPIP, COIL, QUAD
       - SATR geometry and readout
       - TRIG readout

   MUON will be implemented later, ECAL simulates data base with
   data statments (so no more ECAL banks on GEO file).
   VDET and LCAL are still missing.

   data base banks are not written onto the output file, but
   the banks which have been modified either by data cards (read
   from the GEO file) or by program (i.e. the PART bank).

   mods in    : EDTONO
                ASFAST, ASFXXX
                ASKLUI (use KXLUCO to set LUND parameters by
                        data cards)
                ASKLUN, ASKSIN, ASPEVE, ASKTRK, ASINIT ( book
                and fill the new bank 'KVOL' defined below)
                KVOL, NR=0   filled once per event with a list
                             of vertex volume names
                +1         # of columns (=1)
                +2         # of vertices in the event
                =========================================
                +1         volume name of the vertex
                ==========================================

                AGCHCK (book and fill the new bank 'VOLU')
                VOLU, NR=0   filled once per run with a list
                             of all existing volume names.
                +1         # of columns (=1)
                +2         # of sensitive volume names
                ==========================================
                +1         sensitive volume name
                ==========================================

  new statment functions in KMACRO to access content of the VERT and
  KINE banks:

  - # of vertices on a track known by its BOS index JVK /
    # of outgoing tracks of a vertex known by its BOS index JVK
      NOFVK(JVK)
  - Particle type of a track known by its BOS index JVK  : KINTYP(JVK)
  - incoming track # of a vertex known by its BOS index JVK: INPTRK(JVK)
  - origin vertex # of a track known by its BOS index JVK: INPVRT(JVK)
  - momentum of a track known by its BOS index JVK : PMODVK(JVK)
  - energy of a track known by its BOS index JVK : ENERVK(JVK)
  - time of flight of the icoming particle to the vertex known by its
    BOS index JVK : TOFLIT(JVK)
  - radius of the cylinder defined by the vertex known by its BOS
    index JVK : RADVK(JVK)

   ------------------------------------------------------------
   GAL:GAL195.HLB, .CORR, .FOR, .OBJ, _D.OBJ, .INPUT, .GEO/
   GAL195 OLDLIB GAL, TXTLIB, CORR ,GEO

   It is an intermediate version which uses the data base to get
   the following banks:
       - PART, GDEC
       - TPC geometry
       - ITC geometry (read from the GEO file but in the new format)
       - passive materials BPIP, COIL, QUAD geometry

   MUON ,ECAL ,SATR and HCAL will be implemented in the next release.

   mods to clean up the program and be ready for the data base.

   new KINE module (see GAL193.NEWS)

   magnetic field in iron is introduced according to the J.Steinberger's
   region definition.
      -------------------------------------------------------------------
   GAL:GAL193.HLB, .CORR, .FOR, .OBJ, _D.OBJ, .INPUT, .GEO/
   GAL193 OLDLIB GAL, TXTLIB, CORR, INPUT, GEO

   It is an intermediate version which uses the data base to get
   the 'PART' and 'GDEC' banks.

   this version must be run with the new GAL:GALRUN.COM /
   GALRUN EXEC GAL.

   this version calls subroutines from ALEPHLIB 7.0

   It contains:
   - reading of the data base to get 'PART' and 'GDEC' banks (ASREDB)
   - mods in the KINE module in order to use the ASKUSI, ASKUSE
     routines in the same way in GALEPH and in the stand-alone
     KINGAL program.
     ==> mods in /KINCOM/
     ==> move the initial vertex calculation to ASKLUN, ASKSIN, ASKUSE
     ==> call ASKLUN, ASKSIN, ASKUSE with the same set of arguments
         ASK... (IDEV,ISTA,NITR,NIVX,ECMS,WEIT)
    - new format of the PRIN and HIST data cards:
      PRIN   'VDET'  'ITC '  'TPC '
      is equivalent to:
      PRIN     1       2       3
      the possible 'name's for the PRIN card are:
      VDET, ITC , TPC , ECAL, LCAL, SATR, HCAL, MUON, TRIG, RDST,
      INPU, OUTP, KINE, TREL, DRAW, PART, GEOM
      the possible 'name's for the HIST card are:
      VDET, ITC , TPC , ECAL, LCAL, SATR, HCAL, MUON, TRIG, RDST,
      KINE
      both formats are compatible and can be used simultaneously.
    - implement the mini-dst format FXXX:
      mods in /JOBCOM/ : new logical flag FXXXJO
      mods in ASCEVE to call ASFXXX in case the FXXX format is required.
      mods in SAVE data card: add a new parameter 'FXXX'
      SAVE  unit# / 'FXXX'   means write FXXX format in native mode.
      SAVE  unit# / 'EPIO'  'FXXX' means write it in EPIO mode
      SAVE  unit# / 'EPIO'   means write standard format in EPIO mode.
    - mods in KMACRO to used name-indices from /JQCOM/ .
    - bug removal in FS.... module.
    - cleaning of the RDST module.
  ----------------------------------------------------------------
   GAL:GAL192.HLB, .CORR, .FOR, .OBJ, _D.OBJ, .INPUT/
   GAL192 OLDLIB GAL, TXTLIB, CORR, INPUT

   It is an intermediate version which works with GAL190.GEO .

   It contains :
   - mods in the fast tracking process: FSxxxx
     must be used with ALEPHLIB 6.1
     mods in TPC module, LCAL module, ECAL module
   - rewriting of GUSTEP, ECHIT, HCHIT to introduce the common
     parametrization to ECAL and HCAL.
   - new routines ASTDEB called by at the end of GUSTEP to debug the
     step or compute IMPAct points.
   - new *CD KMACRO , used also in the KINGAL package, to access directly
     contents of PART, KINE, VERT banks.
     new *CD GMACRO , used in GUSTEP to access directly GEANT common blocks.
     new *CD TMACROD and TMACROF to access directly TRKCOM content.
   - new variables in /TRKCOM/ : ITRKEL(11)  = particle tracking type
                                 TRKVOL      = volume name (character*4)
                                 FTRHAD      = hadron stopping by hadronic
                                               interaction.
                                 TINOX0      = if it is a geantino:
                                               current X0 value
                                 TINOL0      = current L0 value
   - change in the definition of the PRIMARY particles ( the ones which are
     kept in KINE and VERT banks) : all particles with a Pmod > 1.Gev and
     particles produced in the central part with a Pmod > 0.1 Gev are
     called primaries and kept in KINE / VERT banks.
   - new routine MUWSUM : to print a muon run summary.
   - corrections of bugs in:
   GAL:GAL190.HLB, .CORR, .GEO, .FOR, .OBJ, _D.OBJ, .INPUT, .CARDS /
   GAL190 OLDLIB GAL, TXTLIB, GEO, CORR, INPUT, CARDS
   the geometryfile used by GAL190 has few modifications in COG1
   (coil) and HBG1 (hcal barrel) and the HQG2 AND HSG2 have been
   removed.
   It contains:
   - Make use of the ALEPHLIB 04.0 which contains the KINGAL event
     generator interface kept in the *SET KINE
   - mods in SATR and HCAL
   - mods in ASKxxx module (KINEmatic module)
   - mods in KINCOM : VRTXKI and PLABKI are now dimensioned to 4
   - full doc for SATR and HCAL in decks SADOC and HCDOC
   - mods in ECAL and new ECDOC
   - mods in RDST
   - mods in AGxxxx
   - minor bug corrections in TRIG
   - new routine TPCOOR called by default in DIGI process
   - creation of a *SET for each module containing *CD and *DK
     belonging to a module:
     ALEPH   : ALCONS, BCS, BMACRO, JOBERR
     GCOM    : GEANT *CD
     SKEL    : general *CD and *DK ASxxxx
     GEOM    : AGCONS,ALFGEO,AGxxxx
     GUSER   : GCKING, GCMATE, GUxxxx
     DISPLAY : GAXEPH,ADxxxx
     VDET, ITC, TPC, ECAL, LCAL, SATR, HCAL, MUON, FAST, RDST, TRIG
  ------------------------------------------------------------------
   GAL:GAL180.HLB, .CORR, .GEO, .FOR, .OBJ, _D.OBJ, .INPUT, .CARDS /
   GAL180 OLDLIB GAL, TXTLIB, GEO, CORR, INPUT, CARDS
   is stored on minidisk GAL on VM / directory GAL: on VAX
   It contains:
   - Make use of the ALEPHLIB 03.0 and GAL180.GEO
     AUBOS has been renamed as ALBOS on ALEPHLIB 03.0 (calls ALTELL
     when not enough space when booking a bank).
     There is a new routine AUBOS with the same calling sequence than
     ALBOS BUT does not call ALTELL , return IGARB=2 and KNDX=0 when
     not enough space.
   - *IF CYBER for CYBER205 type of machines : used in BMACRO and
     GALEPH
   - new ECAL shower parameters.
   _ mods in ASxxxx to read event generator input file (not final).
   - minor mods in VDET, ITC, HCAL, LCAL, SATR, FAST, RDST
   - major mods in ITC : linear drift-time relation
                         make use of the time of flight given by GEANT
                         new bank ITCB in the geometry file
                         better documentation
  - mods in TPC        : TPCO bank format is changed to 2I,(I,6F)
                         the content follows now the definition
                         TPCO is filled only on request:
                         ICTPJO(6)=1 means fill TPCO
                         the routine TPRHIT has been moved to
                         the ALEPHLIB.
  ----------------------------------------------------------------------
   GAL:GAL170.HLB, .CORR, .GEO, .FOR, .OBJ, _D.OBJ, .INPUT, .CARDS /
   GAL170 OLDLIB GAL, TXTLIB, GEO, CORR, INPUT, CARDS
   is stored on minidisk GAL on VM / directory GAL: on VAX
   It contains:
   - Make use of ECALDES from the ALEPHLIB 02.0
   - empty *CD's and *DK's such as AAA and ZZZ to help documentation
     tools.
   - *IF CYBER for CYBER205 type of machines : used in BMACRO and
     GALEPH
   - new TRIGGER module : reorganisation of the old one.
   - new routine ASIMOD to initialize various modules (detectors, FAST,
     RDST) before calling the geometry package. This initialization was
     done before in ASIPAC after calling the geometry package.
   - new ECAL shower parameters.
   _ mods in ASRKIN to read event generator input file (not final).
   - mods in VDET, ITC, HCAL, LCAL, SATR, FAST, RDST
   - TCUTJO is now preset in ASINIT to : .005, .005, .01, .01, .01
   ---------------------------------------------------------------
  MC:GAL163.HLB, .CORR, .GEO, .FOR, .OBJ, _D.OBJ /
  GAL163 OLDLIB MCARLO, TXTLIB MCARLO, GEO MCARLO, CORR MCARLO
  is an intermediate version which contains:
   - the use of the MC:ALEPHLIB.HLB, .OLB / ALEPHLIB OLDLIB GENERA,
                                                     TXTLIB GENERA
   - the use of the routines:
     ALBOS ('NAME',NR,LE,INDX,IGARB) from ALEPHLIB
      with  IGARB = 0  the bank is booked/enlarged
                    1  Ok but a garbage collection occured
                    2  not enough space to book/enlarge
                       so the routine ALTELL from ALEPHLIB is called
                       to go to the NEXT event.
     ALTELL ('MESSAGE',IERROR,'ACTION') from the ALEPHLIB
      with  MESSAGE = a character string to be printed
            IERROR  = an error code in the range [1,LERR]
            ACTION  = an action to be taken:
                      RETURN, NEXT, END, FATAL
  - creation of historian SETS
     *ST IFRDST,ASRDST,ASIMOD,ASPEVE,ASCRUN,ASASIG,GUSTEP
     *ST IFFAST,ASIMOD,ASPEVE
     *ST IFPACK,ASIPAC,GUHADR,GUPHAD
     *ST IFHCDET,AGHCAL,HCCRTO,HCIEVE,HCHIT
    these SETS must be compiled when using the corresponding
    historian flag:
     *DF RDST
     *E IFRDST
  or *DF TATINA
     *E IFPACK
  or *DF FAST,RDST
     *E IFFAST,IFRDST
  or *DF HCDETAIL
     *E IFHCDET
  _ introduction of the FAST tracking package (J.Higart, M.Mermikides)
    must be used with: *DF FAST,RDST
                       *E IFFAST,IFRDST
    and a data card PROCess 'KINE'  'RDST' ('TRAC' and 'HITS' are set
                                             by default)
    no need for a SETS card : the necessary detectors are set by default
  - use the new BOS facility: BUNIT, BREAD, BWRITE
  - new format for the data card RNDM:
    RNDM process# / root  (process# 1:KINE, 2:TRAC, 3:HITS, 4:DIGI
                                    5:TRIG, 6:RDST)
  - many bugs corrected everywhere
  - changes in the ITC code to book directly named bank where it is
    possible with the expected maximum length.
    rename the bank ITL1 to ITTR for homogeneity.
  ---------------------------------------------------------------------
  MC:GAL160.HLB, .CORR, .GEO, .FOR, .OBJ, _D.OBJ /
  GAL160 OLDLIB MCARLO, TXTLIB MCARLO, GEO MCARLO, CORR MCARLO
  This version contains:
   - implementation of SATR
   - implementation of the shower parametrization in HCAL
     this is now the default. To use the detailed version:
     *DF HCDETAIL
     *E AGHCAL,HCHIT,HCIEVE,HCCRTO
      these routines are compiled and kept in :
      HCDET160 FORTRAN MCARLO,TEXT MCARLO / MC:HCDET160.FOR, .OBJ
   - mods in MUON (new geometry)
   - new implementation of IMPAct banks: they are created on request
     the filling is steered by a data card
     IMPA  'detname1'   ' ...'
            VDET, ITC, TPCI, ECAL, LCAL, SATR, HCAL, MUON
            track parameters will be stored at the entrance of the
            detector volume 'detname' for all primaries.
            TPCO
            track parameters will be stored at the exit point of the
            TPC gas volume for charged particles only.
   - The INTERACTIVE version is still NOT implemented.
   - When using the flag RDST the routines ASRDST,ASIPAC,ASPEVE,
     ASCRUN,ASASIG,GUSTEP have to be recompiled. The fortran and
     compiled files are kept as :
     RDST160 FORTRAN MCARLO, TEXT MCARLO / MC:RDST160.FOR, .OBJ
   ------------------------------------------------------------------
  MC:GAL150.HLB, .CORR, .FOR, .OBJ, _D.OBJ /
  GAL150 OLDLIB MCARLO, TXTLIB MCARLO, CORR MCARLO
  This is the 1st version which works with GEANT310, GHEISHA7, ZEBRA.
  MUST be used with GAL140 GEO and *DF HCDETAIL.
  The GAL150 part is identical to GAL140 with CORR except the banks
  VERT and GVER which contain now the time of flight (look at the
  ALEPH BOS BANK CONTENTS: ALEPH 86-57 version# 1).
  The files written by GAL140 are not readable by GAL150.
  The Historian library has been RESEQUENCED.
  Only updates to this version will be accepted in the future.
  The INTERACTIVE version does NOT WORK yet.
  -----------------------------------------------------------
   MC:GAL140.HLB, .CORR, .FOR, .OBJ, _D.OBJ, .GEO/
   GAL140 OLDLIB MCARLO, TXTLIB MCARLO, CORR MCARLO, GEO MCARLO
   This is the last version which works with GEANT309+GHEISHA6
   must be used with GAL140 GEO and *DF HCDETAIL.
   This version contains the following mods or features:
   - various corrections in all modules.
   - new subroutine ASCTRA to close the tracking propely:
     the banks 'VERT' and 'KINE' are created there.
   - the data card TCUT should not be used, standard values have
     been set in ASINIT. Only authors of algoritms should play with it.
   - Remember that you MUST have a FIDUcial card in your data card file.
   - mods in ALBOS and AUBLIS to take out all common blocks except
     JQCOM.
   - implementation of LCAL.
   - mods in VDET.
   - write TPC coordinates bank to simulate the 'prepare data' process
     'TPCO' bank.
   - new banks 'RUNH' 'ASRU' 'EVEH' 'ASEV' 'RUNE'
     RUNH, RUNE, EVEH are banks defined by online and offline.
     ASRU is the old RUNH : gives specific information to GALEPH
     ASEV is the old EVEH : gives specific information to GALEPH
     see complete definition of these banks in BANKDOC.HLB .
   - implementation of the RDST process also called SIMDST or ARPEKIN
     to avoid space problem it has been implemented under an Historian
     flag : RDST.
     To run the RDST process add the following cards in the Historian
     input file:
     *DF RDST
     *E ASRDST,ASIPAC,ASPEVE,ASCRUN,ASASIG,GUSTEP
     and use the following data cards:
     PROCESS  'RDST'  'KINE'  'TRAC'  'HITS'
     SETS     'VDET'  'ITC '  'TPC '  'ECAL'   'SATR'   'HCAL'
     PRINT    12      ! print flag for RDST process
     HISTO    12      ! histogramming flag for RDST process
    ...........................................................
   MC:GAL133.HLB, .FOR, .OBJ, _D.OBJ, .GEO /
   GAL133 OLDLIB MCARLO, TXTLIB MCARLO, GEO MCARLO
   This is an intermediate version which works with GAL133 GEO
   must be used with *DF HCDETAIL
   This version contains the following mods or features:
    - better implementation of the beam tube and quadrupoles
    - electronics boxes and other materials in the ECAL region
    - 'IMPA' banks as described in the proposal presented at Munich
    - in case of single particle new definition of a 'primary' track
      as presented in Munich.
    - new routine to print 'KINE' and 'VERT' BOS banks : PRKINE
      new routine to print 'IMPA' BOS banks : PRIMPA
      new routine ASCTRA (close tracking) called after the tracking
                         in ASTRAC. The 'VERT' and 'KINE' Bos banks
                         are built at this level.
   RENAME:  Superconducting Quadrupole SQUA as Quadrupole QUAD
   RENAME:  Luminosity Tracking device LTRK as Small Angle Tracking SATR
  ----------------------------------------------------------------------
#endif
