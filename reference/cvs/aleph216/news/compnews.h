CKEY COMPUTE TRACK / USER
C! various subprograms related to computing of tracks, angles...
 ! ALEPHLIB 21.5
 - AUHCYL Double precision on all platforms (M.Cattaneo)

 ! ALEPHLIB 21.3
 - UFVDMS fix multiple variable definitions

 ! ALEPHLIB 20.9
 - UFMS, UFTKAL, UFVDMS
     get multiple scattering constants from setup dependent bank
     VRLD instead of from FKAL bank
 - UFTTRA
     add a call to vdmsup for consistency

 ! ALEPHLIB 20.8
   UFTTRK, UFTTRA - remove a bug - name-indices were checked instead of
                    indices (D.Casper).

 ! ALEPHLIB 15.7
   UFTKAL - declare ZZ0 double precision, mandatory on AXP (W.Manner)

 ! ALEPHLIB 15.5
   TNRHPA - add a protection to avoid division by 0 when a PT=0.
            charged track is processed.     (A.Venturi)
    UFMS, UFTKAL - protect against unphysical tracks with d0*w > 1
                   (G.Taylor)
