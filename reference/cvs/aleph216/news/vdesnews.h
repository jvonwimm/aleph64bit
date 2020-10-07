CKEY VDETDES VDET GEOM
C! VDET geometry package
 ! ALEPHLIB 215
    VCORMP : modify for cases 17 rphi, 10 Z (J.Rothberg)

 ! ALEPHLIB 213
   Improve correction of time dependent bending of the faces (D.Rousseau)
    VGRDAL: call VALFCO, suppress forced reaccess of VALC,VAGB
    VALFCO: (new deck) Does the correction
    VALCOR: Obsolete, removed. 

 ! ALEPHLIB 212
   Correct time dependent bending of the faces of VDET (D.Rousseau)
    VALCOR: (new deck) Does the correction
    VGRDAL: Force reaccess of VALC,VAGB; call VALCOR

 ! ALEPHLIB 210
   many corrections and new features in hadrware error software

 ! ALEPHLIB 209
   Add new routines to handle hardware errors.

 ! ALEPHLIB 208
   The changes include the addition of two new subroutines for
   packing and unpacking VDET hit addresses.  The four old routines
   that did this kind of thing, VADDPK, VADDUN, VADESA, and VAENSA,
   will henceforth only work for VDET91 because the packing scheme
   will be different for VDET95.

   Associated with this change is addition of one column in the
   database bank VRDO.

    VRDOJJ: added one hac parameter by hand
    VHLSBT: deleted obsolete stuff about "corruption bit",
              added parameters pertaining to VDET95
    VRDOCO: added one word for number of bits in wafer number
    VDETGE: added bit shifts and masks (for the things that
              are not identical in VDET91 and VDET95)

    VPKADD: (new deck) the new packing routine
    VUNADD: (new deck) the new unpacking routine
    VFNEAR: (new deck) find faces near a given phi coordinate
    VPHICF: (new deck) returns phi coordinate of face center
    VADDPK: documentation modified--this routine is for VDET91 only!
    VADDUN: same
    VADESA: same
    VAENSA: same
    VDAFRD: initialize new variables and read one more word from VRDO
    VDETGI: fill bit shifts and masks in VDETGE, fill in geometry
            arrays even for empty slots
    VDGDMP: print new variables in VRDOCO and VDETGE
    VRDDAF: print one line when geometry is initialized

 ! ALEPHLIB 205
   VJWABR - These are used in Julia for
   VJWABW - the simulation of
   VBRJWA - alignment errors for Monte Carlo events
   VDMJLS -  This is a crude version of VDMJLS that works with the new
             geometry package, but only for VDET91!  It is designed to
             reproduce the results of the old version.

 ! ALEPHLIB 204
   Introduction of the new package called VDET95 which replaces VDET89

