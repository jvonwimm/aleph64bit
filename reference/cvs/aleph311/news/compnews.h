CKEY COMPUTE TRACK / USER
C! various subprograms related to computing of tracks, angles...
 * correction file 1 to ALEPHLIB 30.8
    PPCORR : Use .NEQV. instead of .NE. to compare two logicals (A.Waanenen)

 * correction file 4 to ALEPHLIB 30.7
    THLCIR : Use DOUBLE PRECISION for R, to avoid numerical problems
                                                        (M.Cattaneo)

 * correction file 1 to ALEPHLIB 30.7
    PPCORR : New - Correct particle momenta for effects of residual 
             distortions in the central tracking detector.
             This is the so called "sagitta correction".    (I.Tomalin)

 ! ALEPHLIB 30.6
    UFITQL,UFMS,UMSERR,YV0ONE : fix variable type inconsistencies
        in function calls, for Linux                            (A.Waananen)
 
 ! ALEPHLIB 30.5
    YHELIX : New - Determine the turning angle and momentum of an 
                   FRFT track at a point                           (D.Casper)
    YSENSE : New - Determine the sense of direction of a track     (D.Casper)

 ! ALEPHLIB 30.4 correction file 3
    UFITQL : Put cut on NTPC hits, COS(theta) earlier in the routine 
                                                         (M.Cattaneo)

 ! ALEPHLIB 30.2
   new KALMAN filter (D.Casper) is introduced in /kalm.
   old one is removed from /comp.
    UFMATX -  new routine : matrix operations in double precision
              res, a, b, c are mdim x mdim double precision matrices, 
              v is mdim vector, s is a double precision scalar
              UFMMUL (RES,A,B,MDIM) res = a * b
              UFMMLT (RES,A,B,MDIM) res = a * b^T
              UFMSCL (RES,A,V,MDIM) res(i,j) = a(i,j) / (v(i)*v(j))
              UFMADD (A,S,B,RES)    res = a + s * b
              UFMULT (A,B,C,RES)    res = a * b * c^T
   
    UHELIX2 - new routine: UHELIX2 (R,VV0, VV0COV, POS, POSCOV, IRET)
              Given helix and covariance, compute pos and error at radius.

    UFTKAL UFTTRA UFVDMS - are removed, replaced by /kalm/.

    UFTTRK - is removed, obsolete routine replaced by UFTTRA.

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
