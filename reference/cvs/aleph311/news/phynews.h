CKEY PHYSICS
C! various subprograms related to physics

 ! ALEPHLIB 31.0
     PAIRFD : Protection to avoid crashes when reading MINIs with
              the ITCO bank                          (T.Greening, J.Boucrot)

 ! ALEPHLIB 30.6
    ALTHRU,PIDECY : fix variable type inconsistencies in function calls, 
                    for Linux                            (A.Waananen)

 ! ALEPHLIB 21.2
   FJMMCL - (JADE algorithm) Speed up by doing linear search (O.Callot)

Name:   FJMMCL
Type:   Subroutine (meant to be user-callable)
Purpose:Find jets using the JADE minimal mass cluster algorithm
Usage:
        +----------------------------------------------------------------+
        |     CALL  FJMMCL( YCUT, ENORM, SCHEME, VERSN, ARRAY1, ARRAY2,  |
        |    &              NPAR, PPARX, PPARY,  PPARZ, PPARE,  NJETMX,  |
        |    &              NJET, PJETX, PJETY,  PJETZ, PJETE,  JNOFP  ) |
        +----------------------------------------------------------------+

Input:  YCUT            REAL    ycut value ( (M/ENORM)**2 )
        ENORM           REAL    normalising energy (if 0.0, use visible E)
        SCHEME          CHAR*2  combination scheme, either 'E ', 'E0', 'P '
        VERSN           CHAR*6  formula, either 'NORMAL' or 'BETTER'
        ARRAY1(k)       REAL    workspace (k = 1,NPAR*NPAR)
        ARRAY2(k)       REAL    workspace (k = 1,NPAR)
        NPAR            INT     number of particles
        PPARX,Y,Z,E(i)  REAL    4-momentum of input particles (i=1,NPAR)
        NJETMX          INT     maximum number of jets there is space for

Output: NJET            INT     +ve number of jets found (normal return)
                                 -1 input  error (NPAR/ENORM)
                                 -2 error from individ. track
                                 -3 too many jets
                                 -4 unknown particle combination scheme wanted
                                 -5 neither NORMAL nor BETTER algorithm wanted
        PJETX,Y,Z,E(j)  REAL    4-momentum of jet j (j=1,NJET < NJETMX)
        JNOFP(i)        INT     jet no. of particle i   (i=1,NPAR)

Calls:  s/r JPCOMB (ALEPHLIB)

Notes:  The JADE method for finding jets has a number of options. The
        standard ('NORMAL') formula is:
                      2*E(i)*E(j)*(1-cos(theta_i_j))         .
        An improved formula ('BETTER') has been proposed by Dokshitzer
        and is:
                      2*Min(E(i),E(j))*(1-cos(theta_i_j))    .

        There is also a choice of particle combination schemes (how the
        particle 4-vectors are added to form the jets). The E scheme
        is the Lorentz-invariant approach; the E0 scheme normalises the
        momentum components to produce massless jets while the P scheme
        sets the energy to the total momentum to achieve the same effect.



Name:   JPCOMB
Type:   Subroutine (not meant to be user-callable)
Purpose:Combine 2 particles according to input scheme (for FJMMCL)
Usage:
        +----------------------------------------------------------------+
        |     CALL  JPCOMB( ( SCHEME, PP, IP1, IP2 )                     |
        +----------------------------------------------------------------+

Input:  SCHEME          CHAR*2  combination scheme, either 'E ', 'E0', 'P '
        PP(5,k)         REAL    array of 5-vectors (k = 1,no_of_particles)
        IP1             INT     pointer to particle 1 in PP ("winner")
        IP2             INT     pointer to particle 2 in PP ("loser")

Output: PP(5,k)         REAL    as defined above (modified)

Calls:  none

Notes:  (See also notes for FJMMCL)
        The 5-vectors describing particle_2 (PX, PY, PZ, E, PTOT) are
        added to those of particle_1 according to the desired scheme and
        the result overwrites the data for particle_1. The data for
        particle_2 are set to (0,0,0,0,-1).



Name:   PJLINK
Type:   Subroutine (meant to be user-callable)
Purpose:Update links between particles and jets after further evolution
        of jets from a low ycut to a higher ycut (JADE method).
Usage:
        +----------------------------------------------------------------+
        |     CALL  PJLINK( KOLDPJ, NP, KJJ, NJ, KNEWPJ, ERROR )         |
        +----------------------------------------------------------------+

Input:  KOLDPJ(i)       INT     Association of particle i to 'old' jet
        NP              INT     Number of 'old' jets
        KJJ(k)          INT     Association of 'old' jet k to 'new' jet
        NJ              INT     Number of 'new' jets

Output: KNEWPJ(i)       INT     Association of particle i to 'new' jet
        ERROR           LOGICAL = .TRUE. if KOLDPJ has an illegal jet number
                                = .FALSE. otherwise

Calls:  none

Notes:  The routine assumes the original particles are numbered 1..NP
        and that the old jets are in the range 1..NJ.



