C! 1st entry into GAMPACK set
 ! ALEPHLIB 310
     CORAD94: Apply correction from EGAZ bank. 
              Switched off if OLGA data card is present  (M-N.Minard)
 ! ALEPHLIB 20.4
   GAMPEX, VOISTO, GIVSTO - add several protections.
 ! ALEPHLIB 204
   GAMPEX, VOISTO, GIVSTO - add several protections.
 ! ALEPHLIB 203
   GAMPEX - Call ECETDI  to correct for gain correction from ECMC
          - New feature added to calculate the probability of
            a Gampex photon to be fake from electromagnetic or
            hadronic origine (GFAKE,GEMFAK,GHAFAK)
   CORAD94- Replace CORADOC for the current version of alephlib
   GAPGAC - Built photon bank PGAC
 ! ALEPHLIB 157
   CORADOC - correct a bug
 ! ALEPHLIB 154
   GXMCLM - add a limit on the cluster energy to be treated
 ! ALEPHLIB 152
   various corrections in CORADOC, GAMPEX, GFRAC, GXIMPA
   for the overlap region
 ! ALEPHLIB 151
   CORADO is purged
   CORADOC - new function CORADOC (EN,TH,PHI)
   GAMPEX  - get run number from ABRUEV to distinguish between
             Real and MC data
             call CORADOC with new argument list
   GAPGID  - bug correction
   MORSTO  - suppress reference to BMACRO
   reintroduce old version for backward compatibility:
   GAMPEK, GATRAL, ISPARK, MECRFC behind *DK VOISTO.
   These decks should be deleted when GAMPEK is no longer used.
   the variable E4ETB in routine DEFOVE keeps the new value 0.851
   instead of the old one 0.85

 ! ALEPHLIB 146 : new version
    main routine is called GAMPEX
      SUBROUTINE GAMPEX(IPECO,EMIGA,LTOTGA,NNGA,GAMVE,GAMCE,IRTFG)
    Input :
             IPECO is the PECO cluster number
             EMIGA is the min. energy for a photon
             LTOTGA is the total length of GAMVE
                    in the user program:
                    GAMVE should be dimensioned to (20,NPHOT) and
                    GAMCE should be dimensioned to (20,NPHOT) and
                    LTOTGA = 20*NPHOT
                    in GAMPEK: GAMVE is dimensioned to (20,*)
                    the maximum number of photons will be: LTOTGA/20
    Output:

             NNGA  Number of photons

             GAMVE(I  , Photon number)  Photon Array
                  (1,     Energy    |   Best estimates
                  (2,     Theta     |   assuming only
                  (3,     Phi       |   1 gamma
                  (4,     Ad-hoc  energy correction (multiplicative)
                  (5,     FLAG border region endcap
                  (6,     F4
                  (7,     Normalized estim. for substructure
                  (8,     FLOAT( NST1 + 100*NST2 + 10000*NST3)
                          where  nstX is the # of storey in stack X
                  (9,     Estack1/ETOT
                  (10,    Estack2/ETOT
                  (11,    FLOAT( MORS1 + 10*MORS2 + 100*MORS3)
                          where  morsX is = 1 if there a dead storey
                          in central 3 x 3 matrix in stack X
                  (12,    Eraw
                  (13,    EBNEUT flag ECAL crack
                  (14,    itheta peak
                  (15,    jphi   peak
                  (16,    min.dist. to ch.track with barycen.
                  (17,    theta  barycenter  (from the vertex)
                  (18,    phi    barycenter  (from the vertex)
                  (19,    radius of the shower from barycenter
                  (20,    -1 for isolated germination storeys
                          Egerm(storey)-Eneighb./sqrt(Eneighb.)

             GAMCE(I  ,   Cluster number)  Output array from BULOS
                  (1,     Small sigma of cluster in Eigenframe
                  (2,     Large sigma of cluster in Eigenframe
                  (3,     Third moment of cluster in direction of ALAMS
                  (4,     Third moment of cluster in direction of ALAML
                  (5,     Mass of cluster if two photon cluster
                * Item 6-8 not yet implemented
                  (6,     Corrected small sigma for one photon cluster
                  (7,     Corrected large sigma for one photon cluster
                  (8,     Corrected mass of cluster for one photon
                  (9,     QE of 1st Gam for two photon cluster
                 (10,     QX of 1st Gam for two photon cluster
                 (11,     QY of 1st Gam for two photon cluster
                 (12,     QZ of 1st Gam for two photon cluster
                 (13,     QE of 2nd Gam for two photon cluster
                 (14,     QX of 2nd Gam for two photon cluster
                 (15,     QY of 2nd Gam for two photon cluster
                 (16,     QZ of 2nd Gam for two photon cluster
                 (17,     float(Error code)
                 (18,     float(Warning code)
                 (19,
                 (20,     gamma-gamma mass



           IRTFG  Return code
                  =  1 OK but .GE. 1 storey number out of range
                  =  0 EVERYTHING OK
                  = -1 missing bank (PEST,ETDI,PCRL,PECO)
                  = -2 # of PFRF track > NXM
                  = -3 # of storeys on cluster PECO > NSTRMX
                  = -4 # of cluster found > NKLUMX
                  = -5 # of gammas  found > NFOMAX
                  = -6 1 gamma contain > NSTGAX
                  = -7 ECAL geometry package not initialized before calling
                       GAMPEK
                  = -8 problem in EBNEUT initialization

