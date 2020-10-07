C! 1st entry in O_set
 * corr file 305.01
    OBSPOT : Call xvdeok instead of VDETOK (Needs Alephlib>306) (M.Cattaneo)

 ! JULIA 304
    OLSPOT : Remove apostrophes from strings passed to RERROR (D.Casper)

 * corr file 284.01
    OLSPOD : Minor bug fix (O.Schneider, November 1996)
    OLSPOT : Minor bug fix (O.Schneider, November 1996)

 * corr file 283.01
    Replace specific intrinsic functions with generic intrinsic functions
    obspot.F,ocbpre.F,ogtblq.F,olspod.F,olspot.F,olspou.F

 ! JULIA 283
   Upgraded OLSPOT to perform the alignment of the BOMs on a
   run-by-run basis; OLSPOT now makes a beam spot fit (using all the tracks
   in the run) with respect to the BOM+QS0 estimate. A new routine 
   OBSPOT, which is a modified copy of VBSPOT, is called by OLSPOT to do 
   that job. (O.Schneider, September 1996)
     olspot.F updated
     olspou.F updated
     olspod.F updated
     obsinr.F new: modified copy of vbsinr.F
     obspot.F new: modified copy of vbspot.F
     obsclr.F new: modified copy of vbsclr.F
     obmfit.F new: modified copy of vbmfit.F
     ocbfit.F new: modified copy of vcbfit.F
     obscom.h new: modified copy of vbscom.h
     ocbpre.F new: called in ocbfit.F to compute new track parameters
     ogtblq.F new: called in ocbpre.F to get unaligned BOM+QS0 position

 ! JULIA 282
   OLSPOD, OLSPOT, OLSPOU : New routines to get beam spot from LEP boms
                            (O.Schneider, June 1996)

 ! corr file 281.1
   OMCORR : Allow beam energy greater than 50 GeV (O.Schneider)
   OSLOWC : Input data comes from banks LXCR,LXSP for LEP 2 (O.Schneider)
   OINIRU : Call OSLOWC also for run header banks (O.Schneider)

 ! JULIA 273
   OMREAD - allow for 1-8 bunches instead of 1-4 (R.Forty).

