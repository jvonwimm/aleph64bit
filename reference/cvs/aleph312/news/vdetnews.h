CKEY VDET
C! various VDET routines not included into VDETDES
 ! ALEPHLIB 30.5 correction file no. 3
    VTDERV : Improve the protection against rounding errors (A.Bonissent)

 ! ALEPHLIB 30.5 correction file no. 2
    VDTTRA,VTRFIT: Remove CALL from function references, for Linux.(A.Waananen)

 ! ALEPHLIB 30.4 correction file no. 4
    VDHTER : Fix the bug on WERRA ((3,3) instead of (2,2))        (Manoj T.)
    VHERR1 : Modify the error param. (due to change in S/N in MC) (Manoj T.)

 ! ALEPHLIB 30.3 correction file no. 1
   VDTTRA - add UFTR card to flag a track for Kalman filter printout during
            fit.  NR=event number, with entries on the card indicating which
            track(s) of that event should be dumped.  
            Example:  UFTR 10 / 1 2 5 dumps tracks 1, 2, and 5 of event 10. 
            Note there is no selection by run number since the user is presumed
            to be dealing with a small set of events when this feature is 
            enabled.                                                 (D.Casper)

 ! ALEPHLIB 30.2 correction file no. 9
    VDHTER - fix incorrect order of track momentum components in call to VHERR1
                                                                     (D.Casper)

 ! ALEPHLIB 30.2 correction file no. 1
    VDTTRA - new routine which contains the VDET pattern recognition part
             of UFTTRA/alephlib_216 (flr).
             this part has been removed from UFTTRA/alephlib_302.
    VTRFIT   calls VDTTRA instead of UFTTRA (flr).

 ! ALEPHLIB 21.6
    VTRKEX - Place VDMS on E-list for cleanup (D.Casper)

 ! ALEPHLIB 21.5
    VTRUH  :  New routine, count the number of properly associated VDET
              hits by layer and view (A.Bonissent).

 ! ALEPHLIB 21.4
    VDHTER :  Modified to take the space resolution from MC (Manoj T.)
    VHERR1 :  New routine, compute VDET hit error based on MC (Manoj T.)

 ! ALEPHLIB 21.3
    VDMSUP,VTRKEX: fix multiple variable definitions (M.Cattaneo)

 ! ALEPHLIB 21.0
  - VTCLLD :  Correct definition of neighbouring wafer for new VDET
  - VTCLAS :  move local work bank indices setting just before they
              are used
  - VTRFIT :  Access VDET Setup code using ABRUEV and GTSTUP

 ! ALEPHLIB 20.9
 - Unmultiplex strips before clustering rather than after;
   handle the relation between "wafer clusters" and
   "global clusters" (at module level); new adress encoding
   scheme.  Affected :
   VTLINK, VRMWF, VTFILL, VTCLLD, VTRFIT
 - Modify VGRDAL so that alignment banks for monte carlo are accessed
   by setup code; for data we keep going with run number
 - VDMSEC
     get multiple scattering constants from setup dependent bank
     VRLD instead of from FKAL bank
 - VRLDGT
     new routine to give access to VRLD bank
 - VRLDCOM
     new common filled by VRLDGT
 - VDMSUP
     bug fix to a bank index and garbage collection protection
 - VDMSRL
     add more flexibility to allow for layer dependent effects
 ! ALEPHLIB 20.6
   VFACSN - returns the serial slot number of a face.

 ! ALEPHLIB 20.5
   Introduction of the new versions of the VDET alignment routines,
   of the new packing routines.
   Add some routines in the geometry package.
   VSCMSC - remove a bug.
   VRDDAF - remove reference to old VDET geometry package.
   VTRFIT - remove test on 1993 ==> remove bug for all years.
   VDHTER, VTXNWT - new versions to cope with new VDET geometry package.
   VTXTRK -  These are routines which perform the extrapolation
   VDMSUP -  of a track to Vdet wafers and estimate the amount
   VTCLLD -  of scattering material which was encountered. Use
             of old VDGEOS common block was replaced by calls
             to Geom. package routines
   VDMSRL -  get VDRL bank from D.B.
   UFVDMS -  get beampipe setup code.

 ! ALEPHLIB 20.2
   VTRFIT - gets DB banks through ALGTDB instead of MDARD.
            (because MDARD was used and the required bank number did
             not exist, default values were used).
            keep the old code (default values) for '93 MC (setup code=5)

 ! ALEPHLIB 15.8
   VTXNWT - add a protection against asin > 1. (W.Manner)

 ! ALEPHLIB 15.5
   VDMSUP, VTRFIT - decrease the likelihood of garbage collections
                    to protect against " CALL xxx(IW(...)) "
                    (G.Taylor)
   VTXRAW - rewrite the routine to avoid recurrant problem.
 ! ALEPHLIB 15.2
   VTXTRK : check Z cylinder extrapolation

 ! ALEPHLIB 15.1
   VRDDAF : get VDET geometry banks through ALGTDB instead
            of MDARD to avoid to repeat banks with different NR
            but identical contents.

 ! ALEPHLIB 14.1
  Changes submitted on 5/10/92 by G.Taylor, D.Brown, C.Gay

  UFVDMS - add option of using more detailed treatment of VDET
           material if VDMS 0 bank present, no VDMS bank treat as now.
  VTRKEX - modified to provide VDMS banks ( nr = FRFT track number)
  VTXTRK - based on track extrapolation including alignement effects
  VRDDAF - increase z lenght of face, read in VDRL bank
  VTRFIT - call UFTTRA to get new treatment
           call UFTMSO(cf UMSERR) to treat track without vdet hits

  VDMSUP - create VDMS 0 bank and update using measured VDET coords
  VDMSEC - add into VDMS the effect of the VDET supprot rings
  VDMSRL - get the radiation lenght within a face
  UFTTRA - UFTTRK with one modified arguement to call the new code
  UFMS   - get contribution to covariance matrix coming from MS
           between two given radii
  UFTMSO - get contribution to covariance matrix coming from MS
           between innermost coordinaate and origin
  UFTKAL - Change one of the machine precision protections

    ------------------------------------------
