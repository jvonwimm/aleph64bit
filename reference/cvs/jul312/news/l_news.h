C! 1st entry in lcal set
 ! corr file 312.1
    LBHCOR : Bug fix for BHAB row 10 (M.Cattaneo)

 ! JULIA 312
    LHVSTA, LPRLUM, LSELBH : Calculate luminosity taking into account
             mini-ramp.                                       (P.Hansen) 

 ! JULIA 310
    LBHCOR : Double precision fixes for Linux       (D.Smith)

 ! corr file 308.3
    LPREMC : Remove offline pedestal follower after run 45000 (P.Hansen)

 ! corr file 305.1
    LHVSTA : use XLUMOK from ALEPHLIB (version>306)  (B.Bloch)

 ! JULIA 305
    LENLIS,LSELBA : fix variable type inconsistencies in 
                    function calls, for Linux                  (A.Waananen)
    LUPACR : Store X1RG, XTPB, XTCN, SFTR name indices locally (M.Cattaneo)
    LUPAIN : Suppress filling of x1namc COMMON due to clash with
             Alephlib in definition of same common             (M.Cattaneo)

 ! JULIA 304
    LPREMC : Resurrection of dead wires at storey 2/3 interface corrected,
             Storey energy redistributed in case of dead wires
             Suppress negative energies                          (P.Hansen)
    LPRLUM : Compensate for dead module in early 1997 running    (P.Hansen)

 ! corr file 303.7
    LINIRU : Enable taking LDST from start of run record (P.Hansen,M.Cattaneo)

 ! corr file 303.4
    LIDCOD : Bug - ref. energy for method<9, run>40000 was wrong (P.Hansen)
    LSELBH : Bug - ref. energy for method<9, run>40000 was wrong (P.Hansen)

 ! corr file 303.3
   LCNAMC.H Add name index for BHZ0 bank                 (P.H.Hansen)
   LBHCOR : Use BHZ0 bank for LEP1 runs during LEP2 era  (P.H.Hansen)
   LBINDX : Initialize name index for BHZ0 bank          (P.H.Hansen)
   LIDCOD : Use BHZ0 bank for LEP1 runs during LEP2 era  (P.H.Hansen)
   LINIRU : Get BHZ0 bank from database                  (M.Cattaneo)
   LPRLUM : Use BHZ0 bank for LEP1 runs during LEP2 era  (P.H.Hansen)

 ! JULIA 285
   LUPACR : Reinstate filling of trigger enable mask in LUPA (M.Cattaneo Jan96)

 ! JULIA 282
   LINIRU : Force picking up of LDST bank from database (F.Ranjard, July 96)

 ! corr file 281.3
   LOLERR : Fix cut against "sparks" at LEP 2 energy (P.H.Hansen July 96)

 ! corr file 281.2
   LHVSTA : Make same event selection as XLUMOK for lumi. (P.H.Hansen July 96)

 ! JULIA 281
   LBDLBA, LSELBH, LTRIGS : replace WRITE(6, by WRITE(IW(6),
                                               (H.Drevermann, Feb 96)
   LBLDBA : Enable debug print                  (P.H. Hansen, May 96)
   LBHCOR : Fix reference energy from BHAB card (P.H. Hansen, May 96)
   LBNKPR : Only print when Lcal clusters exist (P.H. Hansen, May 96)
   LCALIB : Calibration sample is now method 10
            Flexible low edges on energy histos (P.H. Hansen, May 96)
   LCBOOK, LCHIST: Histogram Bhabha, single arm and other trigs
                   Do not use dropped banks from 'E' list
                                                (P.H. Hansen, May 96)
   LELECT : Put LIDT back on 'T' list           (P.H. Hansen, May 96)
   LGNGBX : Use X1RG bank                       (P.H. Hansen, May 96)
   LHVSTA : Use X1RG bank                       (P.H. Hansen, May 96)
   LIDCOD : Fix reference energy from BHAB card (P.H. Hansen, May 96)
   LINIRU : Flexible low edges on energy histos (P.H. Hansen, May 96)
   LPRCAL : Print also expected energy          (P.H. Hansen, May 96)
   LPRLUM : Exact Luminosity calculation (if energy calib is OK):
            #bhabhas = method 10 with delta_phi>150 with HVs on)
            #background = (( -"-      delta_phi<30   -"-       ) -
                           (MC method 10  -"-      ))*
                          (singles combinations with delta_phi>150)/
                          (  -"-                     delta_phi<30)*
            lumi = (#babhas - #background)/((sigma from BHAB card*
                   (correction factor from LBHCOR))
            Nicer end-of-run print, also for print level 2
                                                (P.H. Hansen, May 96)
   LSELBA : Fix reference energy from BHAB card
            Count combinations of single arm triggers without weights
            Drop cut on summed energy to gain statistics
                                                (P.H. Hansen, May 96)
   LSINGL : check that HV is on for single arm triggers
            allow only one Lcal cluster
                                                (P.H. Hansen, May 96)
   LTGBIT : Install a default 1996 trigger bit assignment
                                                (P.H. Hansen, May 96)
   LTRIGS : Form combinations every 10 single arm trigger
                                                (P.H. Hansen, May 96)
   LUPACR : Replace obsolete trigger banks by X1RG and X1TT
                                                (P.H. Hansen, May 96)
 ! JULIA 279

 ! corr file 278.3
   LBHCOR : check whether we are on the Z (P. Hansen, Nov 95)
   LPRLUM : scale background by ratio of bhabhas with HV on and
           and total bhabha count (P. Hansen, Nov 95)
 ! JULIA 278

 ! corr file 276.1
   LBUNCA : new routine for bunch-to-bunch calibration in LCAL
           (P.Hansen, Apr 95)
   LPREMC : bunch-to-bunch corrections for bunch trains
           (P.Hansen, Apr 95)
 ! JULIA 276

 ! JULIA 275
   LESTIM : prevent division by zero (P. Hansen)

 ! corr file 273.1
   LPRLUM : use of X1TT instead of XTOP dropped in the readout upgrade
   LSELBA : use of X1RG and X1TT instead of XTOP dropped in the readout
           upgrade
