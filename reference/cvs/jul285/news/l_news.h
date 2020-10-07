C! 1st entry in lcal set

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
