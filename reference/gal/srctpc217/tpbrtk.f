      SUBROUTINE TPBRTK(ISECT,ITYPE,MISECT,ISEG,NBRTE)
C-----------------------------------------------------------------------
C!  Get a track element and to break it at the boundary of a sector
C
C  Called from:  TSTEER
C  Calls:        TPROTS, TPGBRT, TPFBRT
C
C  Inputs:  PASSED     --ISECT, the number of the current sector
C                      --ITYPE, the type of this sector
C                      --MISECT, the sector number within the endplate
C                      --ISEG, the number of the current track element
C           BANKS      --Named bank 'TPTE', containing primary track
C                        parameters for each track element
C                      --Work bank id INDBRT, containing secondary
C                        track parameters for each track element
C
C  Outputs: PASSED     --NBRTE, the number of subelements ("broken
C                        track elements") of the full track element
C                        which lie within the boundary of the extended
C                        sector corresponding to sector ISECT
C           /TRAKEL/   --Common blocks containing primary and secondary
C                        parmeters for track element ISEG and each of
C                        its subelements, for use in dE/dX routine
C  D. DeMille
C  Modifications :
C    1.- M. Mermikides  22 sep 1987  --Breaking track elements
C                                      in zero field case.
C    2.- P. Janot       24 may 1988  --Determine Z side by using
C                                      both extremities of the
C                                      track element.
C-----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
C  TPCBOS contains parameters for handling BOS banks used in the
C  generation of analog and digitized signals in the TPC
C  NCHAN = number of channel types for analog signals and digitizations
C  at present, NCHAN = 3; 1 = wires, 2 = pads, 3 = trigger pads.
      PARAMETER ( NCHAN = 3 )
C
C  Work bank id's.  INDREF(ich) = index for signal reference bank for
C  channel of type ich; INDSIG = index for signal bank for current
C  channel.  IDCLUS = index for cluster bank
C
      COMMON/WORKID/INDREF(NCHAN),INDSIG,IDCLUS,ITSHAP,ITDSHP,
     *              ITPNOI,ITSNOI,ITPULS,ITMADC,INDBRT,INDHL,INDDI
C
C  Parameters for analog signal work banks:  for each type of channel,
C  include max number of channels, default number of channels in
C  signal bank, and number of channels by which to extend signal bank
C  if it becomes full; also keep counter for number of blocks actually
C  filled in signal bank
C
      COMMON/ANLWRK/MAXNCH(NCHAN),NDEFCH,NEXTCH,NTSGHT
C
C  Parameters for digitises (TPP) output banks
C
      COMMON/DIGBNK/NDIDEF(3),NDIEXT(3)
C
C  Hit list and digitization bank parameters: for each type of channel
C  include name nam, default length ndd, and length of extension nde.
C
      COMMON/TPBNAM/DIGNAM(2*NCHAN)
      CHARACTER*4 DIGNAM
C  Name index for track element bank
      COMMON/TPNAMI/NATPTE
C
C
C  TPCOND  conditions under which this simulation
C  will be performed
C
      COMMON /DEBUGS/ NTPCDD,NCALDD,NTPCDT,NCALDT,NTPCDA,NCALDA,
     &                NTPCDC,NCALDC,NTPCDS,NCALDS,NTPCDE,NCALDE,
     &                NTPCDI,NCALDI,NTPCSA,NCALSA,NTPCDR,NCALDR,
     &                LTDEBU
      LOGICAL LTDEBU
      COMMON /SIMLEV/ ILEVEL
      CHARACTER*4 ILEVEL
      COMMON /GENRUN/ NUMRUN,MXEVNT,NFEVNT,INSEED(3),LEVPRO
      COMMON /RFILES/ TRKFIL,DIGFIL,HISFIL
      CHARACTER*64 TRKFIL,DIGFIL,HISFIL
      COMMON /TLFLAG/ LTWDIG,LTPDIG,LTTDIG,LWREDC,FTPC90,LPRGEO,
     &                LHISST,LTPCSA,LRDN32,REPIO,WEPIO,LDROP,LWRITE
      COMMON /TRANSP/ MXTRAN,CFIELD,BCFGEV,BCFMEV,
     &                        DRFVEL,SIGMA,SIGTR,ITRCON
      COMMON /TPCLOK/ TPANBN,TPDGBN,NLSHAP,NSHPOF
      COMMON /AVLNCH/ NPOLYA,AMPLIT
      COMMON /COUPCN/ CUTOFF,NCPAD,EFFCP,SIGW,SIGH,HAXCUT
      COMMON /TGCPCN/ TREFCP,SIGR,SIGARC,RAXCUT,TCSCUT
      COMMON /DEFAUL/ PEDDEF,SPEDEF,SGADEF,SDIDEF,WPSCAL,NWSMAX,THRZTW,
     &                LTHRSH,NPRESP,NPOSTS,MINLEN,
     &                LTHRS2,NPRES2,NPOST2,MINLE2
      COMMON /SHAOPT/ WIRNRM,PADNRM,TRGNRM
C
      LOGICAL LTWDIG,LTPDIG,LTTDIG,LPRGEO,
     &        LWREDC,LTPCSA,LHISST,FTPC90,LRND32,
     &        REPIO,WEPIO,LDROP,LWRITE
C
      LOGICAL LTDIGT(3)
      EQUIVALENCE (LTWDIG,LTDIGT(1))
C
      REAL FACNRM(3)
      EQUIVALENCE (WIRNRM,FACNRM(1))
C
C
C  TRAKEL:  track parameters for dE/dX and carrying around broken
C  tracks
C
      COMMON/TRAKEL/NTRK,X(3),VECT(3),ABSMOM,SEGLEN,TOF,AMASS,CHARGE,
     *              RAD,CENT(2),DELPSI,PSI1,ALPH01,ALPH02
C - MXBRK = 2* MAX(NLINES(1..3)) + 2 , NLINES= 8,10,10 in /SCTBND/
      PARAMETER (MXBRK=22, MXBRTE=MXBRK/2)
      COMMON/BRKNTK/XB(3,6),VECTB(3,6),SEGLNB(6)
C
      DIMENSION PSIIN(MXBRTE),PSIOUT(MXBRTE)
C
      LOGICAL LPCUT
C
C  Get primary parameters for this track
C
      INDT = IW(NATPTE)
      NWPTE = IW(INDT+1)
      KTE = INDT + 2 + (ISEG-1)*NWPTE
C
      NTRK = IW(1+KTE)
      X(1) = RW(2+KTE)
      X(2) = RW(3+KTE)
      X(3) = RW(4+KTE)
      VECT(1) = RW(5+KTE)
      VECT(2) = RW(6+KTE)
      VECT(3) = RW(7+KTE)
C
C  Occasionally GALEPH outputs TPTE banks with ABS (direction
C  cosines) > 1.0, so here we correct for this if neccessary.
C
      DO 100 INDEX = 1, 3
         IF (VECT(INDEX) .GE.  1.0) VECT(INDEX) =  0.999999
         IF (VECT(INDEX) .LE. -1.0) VECT(INDEX) = -0.999999
  100 CONTINUE
C
      ABSMOM = RW(8+KTE)
      SEGLEN = RW(9+KTE)
      TOF = RW(10+KTE)
      AMASS = RW(11+KTE)
      CHARGE = RW(12+KTE)
      NBRTE = 0
C
C  Ignore neutral tracks
C
      IF ( CHARGE .EQ. 0. ) GOTO 999
C
C  Check to see if track is on same side (+/- z) of the TPC as the
C  sector
C
      IF ( ISECT .LE. 18 ) THEN
         ISCTS = -1
      ELSE
         ISCTS = 1
      ENDIF
C
C  Determine the Z side of this track element by using both
C  extremities.
C
      ZI   = X(3)
      ZF   = X(3) + SEGLEN*VECT(3)
      IF(ABS(ZF).GT.ABS(ZI)) THEN
        ITRKS = INT(SIGN(1.,ZF))
      ELSE
        ITRKS = INT(SIGN(1.,ZI))
      ENDIF
C
      IF ( ITRKS .NE. ISCTS ) GOTO 999
C
C  Different treatment in zero mag. field case
C
      IF (ABS(CFIELD).LT.0.0001) THEN
C
         CALL TPGBR0(ITYPE,MISECT,NBRTE)
C
      ELSE
C
C  Get additional computed parameters for this track necessary for
C  breaking at sector boundaries
C
         NWPBR = IW(INDBRT+1)
         KBR = INDBRT + 2 + (ISEG-1)*NWPBR
C
         RAD = RW(KBR+1)
         CENT(1) = RW(KBR+2)
         CENT(2) = RW(KBR+3)
         DELPSI = RW(KBR+4)
         ALPH01 = RW(KBR+5)
         ALPH02 = RW(KBR+6)
C
C  Rotate to the sector coordinate frame; make first cut on phi
C  to see whether track crosses sector
C
         CALL TPROTS(ITYPE,MISECT,LPCUT)
         IF ( .NOT. LPCUT ) GOTO 999
C
C  Find the number of broken track elements
C  and the points where the track crosses a sector boundary
C
         CALL TPGBRT(ITYPE,NBRTE,PSIIN,PSIOUT)
         IF ( NBRTE .EQ. 0 ) GOTO 999
C
C  Compute the primary parameters for each broken track and fill a
C  common block containing them
C
         CALL TPFBRT(PSIIN,PSIOUT,NBRTE)
         IF ( NBRTE .EQ. 0 ) GOTO 999
C
      ENDIF
C
C  Done
C
 999  RETURN
C
      END
