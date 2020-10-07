      SUBROUTINE VDMKLA(ITK,XIN,XOUT,NDIM,EREL,XEL,EL,NSTEP)
C!----------------------------------------------------------------------
C! Make Landau track elements
CKEY VDET DIGITIZE
C!
C! Given the GEANT track number, entry-exit points to the wafer face,
C! (ie, a track segment), compute small track segments (NSTEP of them.)
C!
C!  Author         Manoj Thulasidas 15-Mar-1994
C!
C!  Description
C!  ===========
C!  VDMKLA is to make small track elements with LANDAU fluctuations.
C!  We know that a significant part of the VDET resolution comes from
C!  the non-uniformity of the energy deposit as the track traverses
C!  the wafer.  More significantly, the cos(theta) dependence of the
C!  the resolution is almost entirely due to these LANDAU fluctuations.
C!  (A future ALEPH note reference here).
C!  A detailed study showed that the following procedure will reproduce
C!  the test beam data well:
C!  1) Make as many divisions as necessary to have a maximum projection
C!     of 10micron (for each division either in z or r-phi view) in leng
C!  2) Take each division and fluctuate the energy according to the L.
C!     distribution.
C!  3) Normalize the total energy to the totaly energy lost by the track
C!     already given by GEANT.
C!
C!  The call to the subroutine that actually produces the Landau
C!  fluctuations is controlled by a data card VNOL.
C!  i.e. if VNOL is present among the Job control cards, the
C!  result of this subroutine would be without any Landua fluctuations.
C!
C!  NOTES:
C!     - This step is done AFTER GEANT tracking is done.  Experience
C!       taught us that dividing the wafer into smaller GEANT volumes
C!       is not the way to go - too expensive in CPU time.
C!     - The energy lost computed by GEANT is already a L. distribution.
C!       Subdividing it may not be mathematically rigorous and might
C!       lead to subtle effects, somebody will have to study that in
C!       detail, later.
C!
C! Input :  ITK  -  GEANT track number, needed to know what particle it
C!          XIN  -  Entry point to the wafer
C!          XOUT -  Exit point
C!          NDIM -  Maximum number of steps allowed. ie, dimension of th
C!                  arrays in the calling routine
C!          EREL -  Total energy lost by the track segment as given by
C!                  GEANT.
C!
C! Output : XEL   - Midpoints of each small track segment (3,NSTEP)
C!          EL    - Energy deposited by each small tr.seg. (NSTEP)
C!          NSTEP - Number is steps.  (Average is around 15-16)
C!
C-----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C

C     IMPLICIT NONE
      INTEGER NAMIND
      INTEGER ITK, NDIM, NSTEP
      REAL EL(NDIM), EREL, XIN(3), XOUT(3), XEL(3,NDIM)
C
C--  externals
      REAL VMOD, VDIST
      INTEGER NLINK
C
C--  local variables
      REAL PTMP(4), VTMP(4), UBUF, PMOM, ESUM, SCAL, RLAST, PRJ(3),
     $   STEP, ENG, AMASS, CHRGE, TLIFE, RLEN, RLTOT, RPRJ,
     $   TMP(3), FACTOR
      INTEGER IPART, NUBUF, NVTMP, K, ITKTYP, JTK, KKINE
      CHARACTER*20 NAMP
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
C--  ALEPHLIB macros to access GEANT quantities.
C - index of the next vertex/track to be stored in KINE/VERT
C   bank known by its index JVK
      KNEXVK(JVK) = JVK + IW(JVK+1)+IW(JVK+2)+IW(JVK+3)
C - # of vertices/tracks which could be stored in KINE/VERT
C   bank known by its index JVK
      LFRVK(JVK)  = IW(JVK) - (IW(JVK+1)+IW(JVK+2)+IW(JVK+3))
C - index of the 1st parameter of KINE/VERT bank known by its
C   index JVK
      KPARVK(JVK) = JVK + IW(JVK+1)
C - index of 1st vertex/track # contained into the list of
C   bank KINE/VERT known by its index JVK
      KLISVK(JVK) = JVK + IW(JVK+1) + IW(JVK+2)
C - charge of ALEPH particle# JPA
      CHARGE(JPA) = RTABL(IW(NAPART),JPA,7)
C - mass of ALEPH particle# JPA
      PARMAS(JPA) = RTABL(IW(NAPART),JPA,6)
C - time of life of ALEPH particle# JPA
      TIMLIF(JPA) = RTABL(IW(NAPART),JPA,8)
C - # of vertices on a track known by its BOS index /
C   # of outgoing tracks of a vertex known by its BOS index
      NOFVK(JVK)  = IW(JVK+3)
C - Particle type of a track known by its BOS index
      KINTYP(JVK) = IW(KPARVK(JVK)+5)
C - incoming track # of a vertex known by its BOS index
      INPTRK(JVK) = IW(KPARVK(JVK)+5)
C - origin vertex # of a track known by its BOS index
      INPVRT(JVK) = IW(KLISVK(JVK)+1)
C - momentum of a track known by its BOS index
      PMODVK(JVK) = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2
     &                   +RW(KPARVK(JVK)+3)**2)
C - mass of a track known by its BOS index
      PMASVK(JVK) = RW(KPARVK(JVK)+4)
C - energy of a track known by its BOS index
      ENERVK(JVK) = SQRT (PMODVK(JVK)**2 + PMASVK(JVK)**2)
C - time of flight of the icoming particle to the vertex known by its
C   BOS index
      TOFLIT(JVK) = RW(KPARVK(JVK)+4)
C - radius of the vertex known by its BOS index
      RADVK(JVK)  = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2)
C - mother track # of a track known by its BOS index
      MOTHVK(JVK) = INPTRK (NLINK('VERT',INPVRT(JVK)))
C - alephlib version# used in KINGAL (NKJ is the name-index of KJOB)
      ALEKIN(NKJ) = ITABL(IW(NKJ),1,JKJOAV)
C
C
C--  check the input variables
      IF (EREL.LE.0.0)RETURN
C
C-- fudge factor for getting the LANDAU right
      FACTOR = 1.9
C
C-- the length of the track in silicon
      RLTOT = VDIST(XIN,XOUT,3)
C-- ideal leght of the small track elements
      RLEN = 10.0E-4
C-- PRJ is a vector pointing from xin to xout
      CALL VSUB(XOUT,XIN,PRJ,3)
C-- first element  = r-phi proj,
C-- second ellemet = z proj
C-- which projection is larger, rphi or z?
      RPRJ = MAX(ABS(PRJ(1)),ABS(PRJ(2)))
C-- number of steps, at least 1
      NSTEP = INT(RPRJ/RLEN) + 1
C-- at most NDIM
      NSTEP = MIN(NDIM,NSTEP)
C-- the step size, close to (and smaller than) RLEN, but a variable
      STEP = RLTOT/REAL(NSTEP)
C-- PRJ now is the increment vector
      CALL VSCALE(PRJ,1.0/REAL(NSTEP),PRJ,3)
C--  compute the positions of the track elements
      CALL VSCALE(PRJ,0.5,TMP,3)
      CALL VADD(XIN,TMP,XEL(1,1),3)
      DO 11 K = 2, NSTEP
        CALL VADD(XEL(1,K-1),PRJ,XEL(1,K),3)
 11   CONTINUE
C
C If Landau fluctuations are not requested, we have already
C  all we need
C
      IF (IW(NAMIND('VNOL')).NE.0) GO TO 999
C
C--  find the KINE bank
      KKINE = NLINK('KINE',ITK)
      IF (KKINE.NE.0) THEN
        JTK = KKINE
C--  get the GEANT quantities
C--  IPART is the same as aleph part number
        IPART = KINTYP(JTK)
C--  Track momentum and energy
        PMOM = PMODVK(JTK)
        ENG = ENERVK(JTK)
C-- cant do photons and neutrinos
        IF (IPART.EQ.1 .OR. IPART.EQ.4) THEN
          GOTO 999
        ENDIF
        AMASS = SQRT(ENG*ENG-PMOM*PMOM)
C-- protect agains divide by zero in VDLAND
        IF (AMASS.LT.1.0E-15) THEN
          GOTO 999
        ENDIF
        IF (IPART.EQ.2) THEN
          ITKTYP = 2
          CHRGE = 1.0
        ELSEIF (IPART.EQ.3) THEN
          ITKTYP = 2
          CHRGE = -1.0
        ENDIF
      ELSE
C--  assume muons
        AMASS = 0.105
C--  ASSUME (!!) Track momentum and energy
        PMOM = 5.0
        ENG = 5.001102378
        ITKTYP = 4
        CHRGE = 1.0
      ENDIF
C
C--  do the LANDAU and the positions of the elements
C
      ESUM = 0.0

      DO 10 K = 1, NSTEP
         CALL VDLAND(STEP, AMASS, PMOM, ENG, ITKTYP, CHRGE, EL(K))
         ESUM = ESUM + EL(K)
 10   CONTINUE
C
      IF (ESUM.EQ.0.0) GOTO 999
C
C--  scale the whole EL array
      CALL VSCALE(EL, FACTOR, EL, NSTEP)
      RETURN
C
C--  error exit here, put equally divided energies into the output
C--  array
 999  CALL VFILL(EL, NSTEP, EREL/REAL(NSTEP))
C
      RETURN
      END
