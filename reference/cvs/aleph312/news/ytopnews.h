C! 1st entry into YTOP set
 ! ALEPHLIB 31.1
    YFMVTR : Bug fix - Avoid division by zero when cmas=0 (E.Rohne)

 ! ALEPHLIB 30.8 correction file 2
    YFTVTC : Bug fix - test on vertex location always failed due
             to uninitialised variable                      (P.Hansen)
    The above bug was introduced in Alephlib 30.7 correction file 2. Data
    processed through Julia with Alephlibs 307.2->308.1 have no secondary
    vertices or kinks!

 ! ALEPHLIB 30.7 correction file 2
    YFTVTC : Restrict vertices to reasonable volume
             to avoid numerical problems             (D.Casper)

 ! ALEPHLIB 30.5
    YTCONV : Bug fix - declare WTX, VARWX arrays (W.Manner)
    YCIRCR,YVPOSS : fix variable type inconsistencies in function calls, 
                    for Linux                            (A.Waananen)

 ! ALEPHLIB 30.4 correction file 2
    YFMVTR : Protect against astronomical chi^2 (D.Casper)

 ! ALEPHLIB 21.3
    YTOPNW: Replace call to TIMAL by call to TIMEL (M.Cattaneo)
    YVPOSS: Fix multiline strings (M.Cattaneo)

 ! ALEPHLIB 20.9
  YDIMTO - increase MAXHLX the maximum number of charged tracks
                    MAXNTR the maximum number of neutral tracks
                    MAXTRK = MAXHLX + MAXNTR
                    MKDIMM = (MAXTRK+NMSIZZ-1)/NMSIZZ

 ! ALEPHLIB 15.7
  YFMVTR, YFTVTR, YFVMAP, YFVMC, YFVMC1 -
           protection against division by zero (G.Lutz).
  YFMVTR, YFVMC, YFVMC1 - correction of two sign errors (G.Lutz).
  YTPAR  - correction of a sign error (G.Lutz).
  YFMVTC - new routine with lagrange multiplier mass constraint in
           vertex fit (G.Taylor).

 ! ALEPHLIB 15.6
  YFTVTR AND YFVMAP : correct a minor mathemathical bug
  YMKIDT: correct a logical error
  YVPOSS: prohibit overwriting of input track indices

 ! ALEPHLIB 15.3
  YTPVTX : add a protection

 ! ALEPHLIB 15.2

1) Changes in YTRV0S to reject V0,s with hits before the vertex
2) New routines YBIPAT YPRVER YSTVTX YCIRCR for this purpose
3) YDEFRF changed to cope with new ALPHA
4) Minor changes in YTSTRK to allow running on MINI
5) 3 words added in bank YNTR
6) New routine YFVMC1. It allows the enforcement of one
   or several submass constraints (e.g. psi mass in the
   decay Bs->psi K+ K-) in the vertex fit.
7) Minor changes in routines YTCON, YTIJOB, YTPVTX, YTSTRK, YVDCOF


 ! ALEPHLIB 14.5
YTOIJO - this obsolete routine is purged.

 ! ALEPHLIB 14.4

1) introduction of mass constraint vertex fit routine yfvmc
2) improvement of summary printout
3) minor changes in yfmvtr, ytijob, ytconv, ytrv0s

 ! ALEPHLIB 14.1

1) removal of obsolete routines
the following obsolete routines have been permanently removed
from the ytop package in alephlib. a list of these routines and the
proposed replacements follows below.
the calling parameters of the replacement routines are different
a description can be found in the headers of the routines.

OBSOLETE ROUTINE             PROPOSED REPLACEMENT
_____________________________________________________________
YADHVX ADD HELIX TO VERTEX   YFTVTR VERTEX FIT FROM TRACKS AND VERTICES
YBLDVX BUILD VERTEX          YVXBLD BUILD VERTEX
YFTHVX FIT VTX FROM HELICES  YFTVTR FIT VERTEX FROM TRACKS AND VERTICES
YFUMSC MULTIPLE SCATT. FUDGE
YTOCON DUMMY                 YTCONV PHOTON CONVERSION SEARCH
YTOPAN OBSOLETE STEERING     YTOPNW STEERING ROUTINE
YTOPID DUMMY                 YPIDAS PARTICLE IDENTITY ASSIGNEMENT
YTOPVT PRIM.VTX              YTPVTX PRIMARY VERTEX
YPRIMV                       YPRIVX
YTORV0 DUMMY                 YTRV0S V0 RECONSTRUCTION
YTOSTR CHARGED TRACK SEL.    YTSTRK CHARGED TRACK SELECTION
YVXPOS GENERAL VTX SEARCH    YVPOSS GENERAL VERTEX SEARCH ALL TRACKS
      CHARGED TRACKS ONLY


2) addition of a new fast but approximate vertex fitting and mass
routine.
the new routine yfvmap may be used instead of the routine yfmvtr.
it provides vertex, momentum sum and invariant mass without
performing a refit of the track parameters to the vertex.
it takes the track direction at the closest approach to the
vertex for momentum sum and mass calculation.
values for vertex, mass and momentum sum agree very precisely
with results from the elaborate fit. the evaluation of errors
is somewhat approximate.

3) minor modifications in YFMVTR,YTCONV,YTRV0S,YTIJOB

 ! ALEPHLIB 13.8

Change of default version from "old" to "new". The old version
IS SUPPOSED TO BE SCRATCHED SOON.

increase in the number of of allowed charged reconstructed tracks
from 62 to 93.

moving all selection parameters for gamma conversion, vzero
and primary vertex reconstruction to data banks YTGP,YTPP,YTCP,YTVP.
as before default values are provided if any of these banks do
not exist.

      JYTGDM ..... limit of distance from/between helix for vtx fit
                   used in YFMVTR,YFTVTR
      JYTGMM ..... minimum required charged track momentum
                   for vertex search used YTSTRK
      JYTGPP ..... particle identification probability cut used in YPIDAS

      JYTPMT ... YTPVTX minimum tpc hits required
      JYTPNF ....YTPVTX max # of tracks used for first step of primary
                        vertex finding (highest momenta are selected)
      JYTPMM ... YTPVTX MIN. MOM. FOR FIRST STEP OF PRIM.VTX. SEARCH
      JYTPMA ....YTPVTX MIN.MOM. FOR ATTACHING TRKS IN SECOND STEP

      JYTCVC ... YTCONV MAX.VERTEX CHISQ.
      JYTCPC ... YTCONV MAX.POINTING CHISQ.
      JYTCRD ... YTCONV MIN.RADIUS
      JYTCMM ... YTCONV MAX.INVARIANT MASS
      JYTCZD ... YTCONV MAX.Z-DISTANCE OF TRACK FROM BEAM CROSSING
      JYTCNA ... YTCONV MAX.# OF ADD. TRKS THROUGH VERTEX
      JYTCLI ... YTCONV MIN. EL.PROB. FOR BOTH TRACKS
      JYTCHI ... YTCONV MIN. EL.PROB. AT LEAST ONE TRACK

      JYTVVC ... YTRV0S MAX.VERTEX CHISQ.
      JYTVPC ... YTRV0S MAX.POINTING CHISQ.
      JYTVDC ... YTRV0S MIN.CHISQ VTX DIST.
      JYTVMD ... YTRV0S MAX.INVARIANT MASS CHISQ. DEV.
      JYTVZD ... YTRV0S MAX.Z-DISTANCE OF TRACK FROM BEAM CROSSING
      JYTVNA ... YTRV0S MAX.# OF ADD. TRKS THROUGH VERTEX
      JYTVPI ... YTRV0S MIN. PION PROB. OF K0 DECAY TRACKS
      JYTVPR ... YTRV0S MIN. PROTON PROB. IN LAMBDA DECAY
      JYTVPJ ... YTRV0S MIN. PION PROB. IN LAMBDA DECAY

slight change in the primary vertex procedure in case that
the beam spot is used but no vertex with tracks is found
in the first step of primary vetex search. then the beam spot
with possible added low momentum tracks will be returned.

slight extension of the conversion and vzero selection procedure.
additional requirements on the particle identification may be
made.

booking of banks for charged mother tracks, to be filled from
user programs.

  ----------------------------------------------------------------
      YTOP package

      software package developed for
      a fixed target charm experiment na32
      and adapted to aleph by gerhard lutz
      M.FERNANDEZ-BOSMAN, J.LAUBER, AND W.MANNER

   the new version of YTOP is released in ALEPHLIB version 131
   modified in ALEPHLIB version 137

   the tasks performed by the ytopol software are

      -> to profide tools to fit vertices to charged tracks
      neutral tracks
      other vertices
      in addition to finding a new common vertex,
      one may calculate the refitted decay track parameters
      the invariant mass
      the momentum sum of the mother track
      the various correlations
      and from there the parameters of the incoming mother track

      -> to perform some tasks on request like
      reconstruction of the primary vertex
      reconstruction of gamma conversions
      reconstruction of v0'S

   YTOPOL software :    - routines are in ALEPHLIB
      - can be run from ALPHA or JULIA
      To select the topology software in JULIA,
      use the on 'YTOP' in the PROC card.
      In ALPHA, call directly the steering routine YTOPOL
      and the JOB and RUN initialization routines

      see alephnote ALEPH 91-132, SOFTWR 91-005 (27.9.91)
      for a more detailed descripion of the package
      and subroutine headers for a description of
      the input and output paramaters

      The various otpions are controlled by the YOPT card :

      - YOLD : select the old version of YTOP

      - FRF2 : use FRFT bank nr 2
      with VDET refitted tracks

      - PVTX : reconstruct the primary vertex

      - BCRO : fit the primary vertex with the
      constraint of the beam profile

      - RCON : reconstruct gamma conversions

      - RV0S : reconstruct V0'S

      - RLEP : do not use leptons in the primary
      vertex search

      - VHIT : use in the  first step of primary
      vertex reconstruction only the
      tracks that have a hit in the
      vertex detector

      - SVTX : reconstruct all secondary vertices
      (not yet implemented)

      - USER : call a user routine yruser

