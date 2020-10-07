   AUTHOR   : J.Badier    17/04/89
C!  Gammas and neutral clusters analysis.
CKEY PHOTONS / USER

   The PHOTONS package concerns the analysis of the neutral clusters.
   PHOTONS has to be initialised by a call to EBINIT.

   Presently, the main subroutine is EBLONG.
   The input is the content of the 3 stacks and a direction defined
   by the theta and phi polar angles.
   The output gives two calculated estimators and an electromagnetic
   identification is proposed. The validity of this identification
   depends of the physic and has to be checked by the user. The
   estimators may be converted into conditional probability.
   Sometimes they cannot be calculated.
   The proximity of an overlap and of a crack is given.

   The EBCDRG routine allows to generate information on cracks and
   overlap proximity.

   The EBGENE routine gives the parameters of a mean shower.


 ! ALEPHLIB 30.2, correction file no. 9
    EBLEAK,EBLPST,EBPREP,EBPRGA,ECLEAK - increase to 100 GeV the cut on
             maximum energy of a cluster                         (M.N.-Minard)

 ! ALEPHLIB 154
   CLMOMS - add a limit on the cluster energy to be treated

 ! ALEPHLIB 15.2
   GAPGID, GAPGPC  - corrections
   ENOF4, ENOL12, ENOW12, GAPGID, ECLEAK - get necessary data base banks
   using *CA GTDBBK or MDARD for banks with NR=0
   GVERCR - add initialization of some variables.

 ! ALEPHLIB 14.6
   Steering and package added to caracterise photon/pi0

   GAPGID   Steering to built PGID bank using GBNEUT and CLMOMS
   GBNEUT   Modified from EBNEUT to caracterise the transverse
            shape of any neutral cluster . Argument are identical
            to EBNEUT
   GBTRAN   Modified from EBTRAN to caracterise any neutral cluster.
            Argument are identical to EBTRAN
   GBIMPA   Modified from EBIMPA to properly treat the pad in theta and
            impact. Argument are identical to EBIMPA
   GVERCR   Recalculate from individual storeys the cluster barycenter.

 ! ALEPHLIB 12.2
   Another photon identification package is added : Author Nigel Keemer

   CLMONS  guess the number of photons in a cluster on the basis of
           the clusters moments. It uses BULOS routine which calculates
           the eigenvariance of cluster .It  finds a two photon
           decomposition of a cluster based on eigenvariances and 3rd
           moments in direction of major axis.

      SUBROUTINE CLMOMS(IEC,NIMP,CMINFO,IWARN,IERROR,ECOR)

     Input: IEC    - PECO  number of cluster
                 or  ECOB  number of cluster
   Outputs: NIMP   - Number of photons 1 , 2 or 3+
            CMINFO - Additional information
               (1) - Small sigma of cluster in Eigenframe
               (2) - Large sigma of cluster in Eigenframe
               (3) - Third moment of cluster in direction of ALAMS
               (4) - Third moment of cluster in direction of ALAML
               (5) - Mass of cluster if two photon cluster
             * Item 6-8 not yet implemented
               (6) - Corrected small sigma for one photon cluster
               (7) - Corrected large sigma for one photon cluster
               (8) - Corrected mass of cluster for one photon
               (9) - QE of 1st Gam for two photon cluster
              (10) - QX of 1st Gam for two photon cluster
              (11) - QY of 1st Gam for two photon cluster
              (12) - QZ of 1st Gam for two photon cluster
              (13) - QE of 2nd Gam for two photon cluster
              (14) - QX of 2nd Gam for two photon cluster
              (15) - QY of 2nd Gam for two photon cluster
              (16) - QZ of 2nd Gam for two photon cluster
            IWARN  - Warning flag
                     = 1 - invalid storey address ( not serious )
                     = 2 - < 6 storeys (NIMP is set to 1)
            IERROR - Error flag
                     = 1 - banks missing
                     = 2 - arrays too small for No of storeys in cluster
                     = 3 - E < 0
                     = 4 - bulos error
