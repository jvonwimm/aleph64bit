      SUBROUTINE GXMCLM(IEC,NSTGAM,LSTGAM,THEGA,PHIGA,
     &NIMP,CMINFO,IWARN,IERROR,ECOR)
C--------------------------------------------------------------------
CKEY PHOTONS  / USER
C! Guess the number of photons in a GAMPEK photon on the basis of
C! the clusters moments
C
C  Author    :       Nigel Keemer
C  Modified  :       C.Goy             18/12/90
C  Adapted   :       Marc Verderi      Un beau (?) jour d'avril.
C
C    Input: IEC    - PECO  number of cluster
C                or  ECOB  number of cluster
C                  - NSTGAM nombre de storey associe au gamma
C                  - LSTGAM la liste de ces storeys
C                  - THEGA theta du photon
C                  - PHIGA phi   du photon
C                  - ECOR energie GAMPEK --> energie du gamma
C  Outputs: NIMP   - Number of photons 1 , 2 or 3+
C           CMINFO - Additional information
C              (1) - Small sigma of cluster in Eigenframe
C              (2) - Large sigma of cluster in Eigenframe
C              (3) - Third moment of cluster in direction of ALAMS
C              (4) - Third moment of cluster in direction of ALAML
C              (5) - Mass of cluster if two photon cluster
C            * Item 6-8 not yet implemented
C              (6) - Corrected small sigma for one photon cluster
C              (7) - Corrected large sigma for one photon cluster
C              (8) - Corrected mass of cluster for one photon
C              (9) - QE of 1st Gam for two photon cluster
C             (10) - QX of 1st Gam for two photon cluster
C             (11) - QY of 1st Gam for two photon cluster
C             (12) - QZ of 1st Gam for two photon cluster
C             (13) - QE of 2nd Gam for two photon cluster
C             (14) - QX of 2nd Gam for two photon cluster
C             (15) - QY of 2nd Gam for two photon cluster
C             (16) - QZ of 2nd Gam for two photon cluster
C           IWARN  - Warning flag
C                    = 1 - invalid storey address ( not serious )
C                    = 2 - < 6 storeys (NIMP is set to 1)
C           IERROR - Error flag
C                    = 1 - banks missing
C                    = 2 - arrays too small for No of storeys in cluster
C                    = 3 - E < 0
C                    = 4 - bulos error
C    Calls: VUNIT  - CERNLIB
C           GBULOS  - Internal
C Bank ref: PECO,   PEST,   ETDI
C
C Comm ref: GULIMP - Decomposition of cluster into two impacts
C           GUINFO - Values of second and third moments in Eigenframe
C Bank mod: None
C Comm mod: None
C NRK 8/4/90
C--------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JPESKS=1,JPESER=2,JPESED=3,JPESET=4,JPESPE=5,LPESTA=5)
      PARAMETER(JETDTL=1,JETDS1=2,JETDS2=3,JETDS3=4,LETDIA=4)
      PARAMETER(JESDTJ=1,JESDFI=2,JESDDK=3,JESDME=4,JESDSC=5,JESDIO=6,
     +          JESDDI=7,JESDER=8,JESDEC=9,JESDED=10,JESDES=11,
     +          LESDAA=11)
      PARAMETER(JECOEN=1,JECOEG=4,JECORC=5,JECOIT=6,JECOCV=8,JECONH=11,
     +          JECONT=12,JECOPF=13,JECOEB=14,JECOEH=15,JECOET=16,
     +          LECOBA=16)
      PARAMETER(JECLCH=1,JECLE4=2,JECLE1=3,JECLE2=4,JECLE3=5,JECLR4=6,
     +          JECLR1=7,JECLR2=8,JECLR3=9,JECLT4=10,JECLT1=11,
     +          JECLT2=12,JECLT3=13,JECLF4=14,JECLF1=15,JECLF2=16,
     +          JECLF3=17,JECLS1=18,JECLS2=19,JECLS3=20,JECLES=21,
     +          LECLUA=21)
      PARAMETER(JPECER=1,JPECE1=2,JPECE2=3,JPECTH=4,JPECPH=5,JPECEC=6,
     +          JPECKD=7,JPECCC=8,JPECRB=9,JPECPC=10,LPECOA=10)
C Input arguements (see above)
      SAVE
      INTEGER           IEC
      integer nstgam, lstgam(*)
C Output arguements (see above)
      INTEGER           NIMP,   IWARN,  IERROR
      REAL              CMINFO(16)
C Maximum number of storeys allowed in a cluster to be analyzed
      INTEGER           MXSTRY
      PARAMETER         (MXSTRY=100)
C Positions and energies in local coordinate system of two photons
      REAL              E1,     T1,     P1,     E2,     T2,     P2
      COMMON / GULIMP / E1,     T1,     P1,     E2,     T2,     P2
C Sigma and 3rd moments of cluster in Eigenframe
      REAL              RALAMS, RALAML, VM3,    UM3
      COMMON / GUINFO / RALAMS, RALAML, VM3,    UM3
C Pointers to banks
      INTEGER           IPECO,  IPEST,  IETDI
C Vector in direction of cluster, average distance of stories in cluster
      REAL              CVEC(3),        CBAR
C Sum of storey energies
      REAL              ESUM
C Dot product of two vectors
      EXTERNAL          VDOT
      REAL              VDOT
C Theta and Phi vectors projected onto plane normal to CVEC
      REAL              TVEC(3),        PVEC(3)
C JULIA number of ALPHA cluster number IEC
      INTEGER           IECJ
C Row in PEST bank, row in ETDI bank
      INTEGER           IRPEST, IRETDI
C Tower index, stack, theta and phi indicies of storey
      INTEGER           ITOWER, KSTACK, JTHETA, IPHI
C Vector position of bari-centre storey
      REAL              VECS(3)
C Function to check whether storey address is possible (from ECAL geom)
      EXTERNAL          EINTST
      LOGICAL           EINTST
C Positions and energies of stories associated to cluster in local
C coordinate system
      REAL              TSTORY(MXSTRY), PSTORY(MXSTRY), ESTORY(MXSTRY)
      INTEGER           NSTORY
C Error return from bulos routine
      INTEGER           IER
C Average width of a shower, and small width limit for a 1/2 cluster
      REAL              AVLAM,  ALCUT
C Vector direction of two photons
      REAL              VEC1(3),        VEC2(3)
C Limit on the cluster energy to be treated
      PARAMETER ( ESLMX = 300. )
C Vector in Z direction
      REAL              ZVEC(3)
      DATA  ZVEC / 0. , 0. , 1. /
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C--------------------------------------------------------------------
C Zero error flags
      IWARN  = 0
      IERROR = 0
C Pointer to PECO bank
      NAPECO = NAMIND('PECO')
      IPECO  = IW(NAPECO)
      IF     (IPECO.LE.0) THEN
        IERROR = 1
        GO TO 999
      ENDIF
C Pointer to PEST bank
      NAPEST = NAMIND('PEST')
      IPEST  = IW(NAPEST)
      IF (IPEST.LE.0) THEN
        IERROR = 1
        GO TO 999
      ENDIF
C Pointer to ETDI bank
      NAETDI = NAMIND('ETDI')
      IETDI  = IW(NAETDI)
      IF     (IETDI.LE.0) THEN
        IERROR = 1
        GO TO 999
      ENDIF
C Construct unit vectors in plane normal to direction of cluster
      CVEC(1) = SIN(THEGA)*COS(PHIGA)
      CVEC(2) = SIN(THEGA)*SIN(PHIGA)
      CVEC(3) = COS(THEGA)
C
      CALL CROSS(ZVEC,CVEC,PVEC)
      CALL CROSS(PVEC,CVEC,TVEC)
      CALL VUNIT(TVEC,TVEC,3)
      CALL VUNIT(PVEC,PVEC,3)
      CALL VUNIT(CVEC,CVEC,3)
C Now retrieve the constituent storeys of this cluster
C in PEST bank
      NSTORY = 0
      CBAR   = 0.
      ESUM   = 0.
      NPEST = 0
      IF(IPEST.NE.0) NPEST = LROWS(IPEST)
      DO 10 JSTGAM = 1,NSTGAM
C ----------------------------------------------------------
        IRPEST = LSTGAM(JSTGAM)
C Test dead storey
        IF (ITABL(IPEST,IRPEST,JPESED) .NE. 0) GOTO 10
C Find JTHETA,IPHI,KSTACK of storey
        IRETDI = ITABL(IPEST,IRPEST,JPESET)
C Check that IRETDI is non zero (I think 0 = dead or masked)
        IF  (IRETDI.EQ.0) GO TO 10
C Stack
        KSTACK = ITABL(IPEST,IRPEST,JPESKS)
C Tower number
        ITOWER = ITABL(IETDI,IRETDI,JETDTL)
C Theta and phi indicies
        JTHETA = IBITS(ITOWER,16,8)
        IPHI   = IBITS(ITOWER,2,9)
        IF     (.NOT.EINTST(JTHETA,IPHI,KSTACK)) THEN
          IWARN  =  1
          GO TO 10
        ENDIF
C Get position of this storey
        CALL ESRBC('ALEPH',JTHETA,IPHI,KSTACK,VECS)
C Add it to the arrays
        NSTORY = NSTORY + 1
        IF     (NSTORY.GT.MXSTRY) THEN
          IERROR = 2
          GO TO 999
        ENDIF
        TSTORY(NSTORY) = VDOT(VECS,TVEC,3)
        PSTORY(NSTORY) = VDOT(VECS,PVEC,3)
        ESTORY(NSTORY) = RTABL(IPEST,IRPEST,JPESER)
C Average distance of cluster for calculating mass
        CBAR   = CBAR + VDOT(VECS,CVEC,3)*ESTORY(NSTORY)
        ESUM   = ESUM + ESTORY(NSTORY)
  10  CONTINUE
C
      IF     (ESUM.LE.0.OR.ESUM.GT.ESLMX) THEN
        IERROR = 3
        GO TO 999
      ENDIF
      CBAR   = CBAR / ESUM
C
C     IF     (NSTORY.LE.6) WRITE(6,*) 'ONLY ',NSTORY,'STOREYS'
      IF     (NSTORY.GT.6) THEN
C Now find moments of cluster
        CALL GBULOS(TSTORY,PSTORY,ESTORY,NSTORY,IER)
        IF     (IER.NE.0) THEN
          IERROR = 4
          GO TO 999
        ENDIF
C Decide on cluster multiplicity
        AVLAM  = 2.2 - 0.4*EXP( - ESUM / 7.0 )
        ALCUT  = AVLAM + 0.8
        IF     (RALAMS.LT.ALCUT) THEN
          IF     (RALAML.LT.ALCUT) THEN
            NIMP   = 1
          ELSE
            NIMP   = 2
          ENDIF
        ELSE
          NIMP   = 3
        ENDIF
C Pack output info
        CMINFO(1)  = RALAMS
        CMINFO(2)  = RALAML
        CMINFO(3)  = UM3
        CMINFO(4)  = VM3
        IF (RALAML .LE. AVLAM) THEN
          RAL=AVLAM
        ELSE
          RAL=RALAML
        ENDIF
        CMINFO(5)  = ECOR * SQRT(RAL**2-AVLAM**2) / CBAR
        CMINFO(6)  = ESUM
        CMINFO(7)  = 0.
        CMINFO(8)  = 0.
        VEC1(1) = T1*TVEC(1) + P1*PVEC(1) + CBAR*CVEC(1)
        VEC1(2) = T1*TVEC(2) + P1*PVEC(2) + CBAR*CVEC(2)
        VEC1(3) = T1*TVEC(3) + P1*PVEC(3) + CBAR*CVEC(3)
        CALL VUNIT(VEC1,VEC1,3)
        CMINFO(9)  = E1
        CMINFO(10) = E1 * VEC1(1)
        CMINFO(11) = E1 * VEC1(2)
        CMINFO(12) = E1 * VEC1(3)
        VEC2(1) = T2*TVEC(1) + P2*PVEC(1) + CBAR*CVEC(1)
        VEC2(2) = T2*TVEC(2) + P2*PVEC(2) + CBAR*CVEC(2)
        VEC2(3) = T2*TVEC(3) + P2*PVEC(3) + CBAR*CVEC(3)
        CALL VUNIT(VEC2,VEC2,3)
        CMINFO(13) = E2
        CMINFO(14) = E2 * VEC2(1)
        CMINFO(15) = E2 * VEC2(2)
        CMINFO(16) = E2 * VEC2(3)
      ELSE
        IWARN  = 2
        NIMP   = 1
        CALL VZERO(CMINFO,16)
      ENDIF
C
  999 CONTINUE
C
      IF     (IERROR.NE.0) THEN
        NIMP   = -1
        WRITE(6,*) IERROR,'KEVT',KEVT
        CALL VZERO(CMINFO,16)
      ENDIF
C
      END
