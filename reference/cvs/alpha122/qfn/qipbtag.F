      SUBROUTINE QIPBTAG (IRET,NTRACK,NJET,TRKJET,FRF2TRK,
     &                   PROBTRK,PROBJET,PROBHEMI,PROBEVT)
CKEY   QIPBTAG / USER
C-----------------------------------------------------------------------
C! User routine for the B-tagging by the impact parameter method
C  SUBROUTINE QIPBTAG
C  =================
C
C  Called by the user
C  Calls : BTAG_PAR,JETCLU,QFNDIP,QFNDV0,TRSEL,FINDDMIN,TRSEL2,
C          BTAGTR,BTAGEV
C
C     INPUT:
C        none
C
C     OUTPUT:
C        iret :          >0   if event not useful for analysis
C                        <0   if error during analysis
C                        = 0  else = O.K.
C        list of iret:
C               iret=    0   good event , analysis performed
C                        1   Number of jets less than 2
C                        2   Jets outside solid angle cuts
C                       -2   No beam spot information
C                      3,4   No tracks for btag analysis found
C                       -3   error in interaction point finding
C                       -4   Error in finding thrust axis
C
C        ntrack          = # of tracks used to calculate PROBJET/HEMI/EV
C        njet :          # of jet found
C        TRKJET          = ALPHA track number of jets (ordered by energy
C        FRF2TRK         = FRFT,2 row number of tracks used for analysis
C        probtrk(ntrack) array(ntrack) with probabilities of each track
C                               with indices as in FRF2TRK
C        probjet(njet)   array(njet) with probabilities of each jet
C                               with indices as in TRKJET
C        probhemi(2)     array(2)    with probabilities of hemisphere
C                              hemisphere(1) is defined by the leading j
C        probevt         probability of the event
C
C     -- The parameters for this routine are given
C        by the BOS cards 'JETF','FITP','TRA2','VCUT'
C
C     -- The tracks are selected
C
C     -- V0s are made and used (new 10-9-93) DNB
C
C     -- The calculation of of the track probabilities is performed
C
C     -- The calculation of of the event probabilities is performed
C
C                                  D BROWN,  M FRANK   13/01/92
C
C  MODIFIED:
C  ---------
C  03/05/95  STEFAN SCHAEL, New QIPBTCPU timing routine
C  13/02/97  Manoj Thulasidas.
C            Include John Carr's modifications
C                - if running on the new VDET
C            OR  - if NQIP card is present
C            Do not include John Carr's modifications
C                - if running on the old VDET
C            OR  - if OQIP card is present
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "qdecl.h"
#include "qcde.h"
#include "maxtrk.h"
#include "btpar.h"
#include "btagraw.h"
#include "pubraw.h"
C
C  Common for computing 1-track-removed vertex
C
      REAL    DT0(4,MAXTRK), HAT(3,4,MAXTRK), ERR2(5,MAXTRK),
     .        CHI2_DOF_SAVE
      INTEGER IFLAG(MAXTRK), NUSED_SAVE(2)
      REAL    ALPHA_SAVE, BETA_SAVE(3), GAMMA_SAVE(3,3)
      COMMON/IPVTX/DT0,HAT,ERR2,IFLAG,
     &      ALPHA_SAVE,BETA_SAVE,GAMMA_SAVE,NUSED_SAVE,CHI2_DOF_SAVE
C
C   local variables:
C
      INTEGER IER
      INTEGER IALTRK,ICHUNK
      INTEGER IJET,IHEMI,ITRK,ITRAK
      INTEGER ICOR,ITRACK
      INTEGER NUSED(2),MINTRK
      INTEGER NVTYP(3),IV0,ITYPE
      INTEGER ISORT(MAXJET),TRKNEW(MAXJET)
      INTEGER INBIP,ITHRU,IWORD,IDMIN
      INTEGER VDHITP(MAXTRK)
      INTEGER IEF
      REAL BFLD
      REAL PJET(MAXJET)
      REAL WEIGHT,EVECUT,THRUST
      REAL PMIN, JDOT, T0
      REAL BP_ERR(3),BP_SIZE(3)
      CHARACTER*8 CNAM,TNAM
      LOGICAL FJET, LO_QFNDIP
      INTEGER IJET1, ISAVE, I, J
      DATA EVECUT/.5/,PMIN/0.0/
      DATA MINTRK/3/
      DATA CNAM/'QIPBJETS'/,TNAM/'THRUST'/
      DATA ICHUNK/0/
      DATA FJET/.TRUE./
C  to see whether to use the new code
      LOGICAL NEWCOD, FIRST
      DATA FIRST /.TRUE./
      SAVE NEWCOD, FIRST
C
C  Functions
C
      INTEGER NAMIND
C
C  Inlines
C
#include "btaginl.h"
#include "qmacro.h"
C-----------------------------------------------------------------------
C  Initialize return codes, and preset variables just in case
C
      IRET        = 0
      INBIP       = 0
      NTRACK      = 0
      NJET        = 0
      NV0         = 0
      NV0         = 0
      PROBHEMI(1) = 1.1
      PROBHEMI(2) = 1.1
      PROBEVT     = 1.1
C
C---- RESET CPU TIME USAGE
C
      CALL TIMED(T0)
C
C---- Check that the parameters are properly setup; if not, call BTAGPAR
C
      IF (FIRST) THEN
        FIRST = .FALSE.
C
C see which version of the code is to be used,
C John's modifications or standard?
C      
        CALL CODSEL(NEWCOD)
        IF (NEWCOD) THEN
          CALL BTAG_PAR96
          WRITE (IW(6),*)'QIPBTAG: improved QIPBTAG initialized'
        ELSE
          CALL BTAG_PAR
          WRITE (IW(6),*)'QIPBTAG: standard QIPBTAG initialized'
        ENDIF
      ENDIF
C
C---- Don't use NBIP bank on MINIS
      IF (.NOT. XNANO) GOTO 100
C
C  Look for NBIP bank to skip most calculations when reading NANO ;
C
      INBIP = IW(NANBIP)
      IF (INBIP .GT. 0) THEN
C
C------- Call from NANO-dst- extract the necessary information
         NTRACK = LROWS(INBIP)
         NJET = 0
         ITRK = 0
         DO ITRAK=1,NTRACK
C
C---------- First, unpack the integer word
            ITRK          = ITRK + 1
            IWORD         = ITABL(INBIP,ITRAK,1)
            FRF2TRK(ITRK) = IAND(IWORD,255)
            TFLAG(ITRK)   = IAND(ISHFT(IWORD,-8),163839)
            JJET(ITRK)    = IAND(ISHFT(IWORD,-22),15)
            JHEMI(ITRK)   = IAND(ISHFT(IWORD,-26),3)
            IF (JJET(ITRK) .GT. NUMJET) THEN
               ITRK = ITRK - 1
            ELSE
C
C              Take the number of jets as the maximum of the jet
C              numbers. This should be overwritten later on, but
C              just in case.
C
               NJET = MAX(NJET,JJET(ITRK))
C
C              Now, the impact parameter significance. The impact
C              parameter error is set to 1, but since all calculations
C              use only the ratio, this won't matter. The packing
C              factor here is 10**6
C
               IDMIN        = ITABL(INBIP,ITRAK,2)
               DMIN(ITRK)   = IDMIN*1.0E-6
               S_DMIN(ITRK) = 1.0
            ENDIF
         ENDDO
         NTRACK = ITRK
      ENDIF
C
C---- Find the jets; if some already exist, use them. QFNDIP has the
C     same jet definition as QIPBTAG. So just copy them if they exist

 100  CONTINUE
      IJET1     = KPDIR('QFNDJETS',KRECO)
      IJET      = KPDIR(CNAM,KRECO)
      LO_QFNDIP = .FALSE.
      IF (IJET1 .GT. 0.AND.IJET.EQ.0) THEN
         DO WHILE(IJET1.GT.0)
            ISAVE  = KVSAVE(IJET1,CNAM)
            IJET1  = KFOLLO(IJET1)
         ENDDO
         LO_QFNDIP = .TRUE.
      ENDIF

      IJET  = KPDIR(CNAM,KRECO)
C SSC
C---- No pre-existing jets; find them
      IF (IJET .LE. 0) THEN
         CALL QIPBTCPU('JETCLU',0)
         CALL JETCLU(CNAM,PCUT,NJET,JETS,TRKJET)
         CALL QIPBTCPU('JETCLU',1)
C
C------- Skip most of the calculations on NANOS
         IF (INBIP .GT. 0) GOTO 5000
      ELSE
C SSC    13.12.95  same cut on jet momentum as in JETCLU
C------- Use the pre-existing jets
         NJET = 0
         DO WHILE(IJET.GT.0)
            IF (QP(IJET) .GE. PCUT) THEN
               NJET         = NJET + 1
               TRKJET(NJET) = IJET
               PJET(NJET)   = QP(IJET)
            ENDIF
            IJET = KFOLLO(IJET)
         ENDDO
C
C------- Sort and pack the jets
         IF (NJET .GT. 0) CALL SORTZV(PJET,ISORT,NJET,1,1,0)
         DO IJET=1,NJET
            JETS(1,IJET) = QX(TRKJET(ISORT(IJET)))
            JETS(2,IJET) = QY(TRKJET(ISORT(IJET)))
            JETS(3,IJET) = QZ(TRKJET(ISORT(IJET)))
            TRKNEW(IJET) = TRKJET(ISORT(IJET))
         ENDDO
         DO IJET=1,NJET
            TRKJET(IJET) = TRKNEW(IJET)
         ENDDO
C
C------- First time, send a message
         IF (FJET) THEN
            FJET = .FALSE.
            WRITE (IW(6),*)' QIPBTAG: using pre-existing jets'
         ENDIF
      ENDIF
C
C---- Require at least 1 jet
      IF (NJET .LT. 1) THEN
         IRET = 1
         GOTO 9000
      ENDIF
C
C------- Assign the jets and skip most of the calculations on NANOS
         IF (INBIP .GT. 0) THEN
           DO ijet = 1, njet
             DO itrk = 1, ntrack
               DO ief = kfeft, kleft
                 IF ( ktn(ief) .EQ. frf2trk(itrk) ) THEN
                   IF ( xsame(ief,trkjet(ijet)) ) jjet(itrk)=ijet
                   GOTO 1
                 ENDIF
               ENDDO
 1             CONTINUE
             ENDDO
           ENDDO
           GOTO 5000
         ENDIF
C
C---- Find the thrust axis; if absent, make it
      ITHRU = KPDIR(TNAM,KRECO)
      IF (ITHRU .LE. 0) THEN
         CALL QIPBTCPU('THRUST',0)
         CALL QJOPTR('EF',' ')
         CALL QJTHRU(THRUST,TNAM,KRECO)
         ITHRU = KPDIR(TNAM,KRECO)
         CALL QIPBTCPU('THRUST',1)
         IF (ITHRU .LE. 0) THEN
            IRET = -4
            GOTO 9000
         ENDIF
      ENDIF
C
C     Set the hemisphere axis to be the thrust axis, unless the
C     HJET card is present, in which case use the leading jet
C     (old definition)
C
      IF (IW(NAMIND('HJET')) .GT. 0) THEN
         HAXIS(1) = QX(TRKJET(1))/QP(TRKJET(1))
         HAXIS(2) = QY(TRKJET(1))/QP(TRKJET(1))
         HAXIS(3) = QZ(TRKJET(1))/QP(TRKJET(1))
      ELSE
         HAXIS(1) = QX(ITHRU)/QP(ITHRU)
         HAXIS(2) = QY(ITHRU)/QP(ITHRU)
         HAXIS(3) = QZ(ITHRU)/QP(ITHRU)
      ENDIF
C
C     If there are less than 2 jets, use the opposite half of the
C     thrust axis to 'create' a 2nd jet.
C
      IF (NJET .LT. 2) THEN
         NJET      = 2
         TRKJET(2) = KVSAVE(ITHRU,CNAM)
         IF (QCOSA(TRKJET(1),TRKJET(2)) .GT. 0.0) THEN
            CALL QVSCAL(TRKJET(2),-QP(TRKJET(1))/QP(ITHRU))
         ELSE
            CALL QVSCAL(TRKJET(2),QP(TRKJET(1))/QP(ITHRU))
         ENDIF
         JETS(1,NJET) = QX(TRKJET(2))
         JETS(2,NJET) = QY(TRKJET(2))
         JETS(3,NJET) = QZ(TRKJET(2))
      ENDIF
C
C---- Get the beamspot; now from ALPHA variables
      IF (XGETBP) THEN
         DO ICOR = 1,3
            BP(ICOR)      = QVTXBP(ICOR)
            BP_ERR(ICOR)  = QVTEBP(ICOR)
            BP_SIZE(ICOR) = QVTSBP(ICOR)
            ERR2_BP(ICOR) = BP_ERR(ICOR)**2 + BP_SIZE(ICOR)**2
         ENDDO
      ELSE
         IRET = -2
         GOTO 9000
      ENDIF
C
C---- Get the interaction point
      CALL QIPBTCPU('QFNDIP',0)
      IF (IW(NAMIND('QFND')).GT.0.OR.LO_QFNDIP) THEN
         IP(1) = QVX(KFREV)
         IP(2) = QVY(KFREV)
         IP(3) = QVZ(KFREV)
         DO I = 1,3
            DO J = 1,3
               ERR_IP(I,J) = QVEM(KFREV,I,J)
            ENDDO
         ENDDO
         CALL UCOPY(NUSED_SAVE,NUSED,2)
         CHI2_DOF = CHI2_DOF_SAVE
      ELSE
         CALL QFNDIP(BP,ERR2_BP,NJET,JETS,IP,ERR_IP,CHI2_DOF,NUSED)
      ENDIF
      CALL QIPBTCPU('QFNDIP',1)
      IF (NUSED(1).LT.0 .OR. NUSED(2).LT.0) THEN
         IRET = -3
         GOTO 9000
      ENDIF
      IF (NUSED(1)+NUSED(2) .LT. MINTRK) THEN
         IRET = 3
         GOTO 9000
      ENDIF
C
C     Find V0s.  This now creates neutral tracks which are
C     then used in the analysis, as opposed to just cutting out the
C     tracks from V0s.
C
      CALL QIPBTCPU('QFNDV0',0)
      CALL QFNDV0(IP,ERR_IP,NV0,V0TYPE,V0PAIR,V0PAT,
     &      V0DK,V0DERR,V0PROB,V0PAR,V0ERR)
      CALL QIPBTCPU('QFNDV0',1)
C
C---- Find the VDET hit pattern
      CALL QIPBTCPU('BTVDHITS',0)
      CALL BTVDHITS(VDHITP)
      CALL QIPBTCPU('BTVDHITS',1)
      IF (NEWCOD) THEN
C---  create VTXT banks
        BFLD=QMFLD
        CALL QIPBTCPU('VTRKEX',0)
        CALL VTRKEX(BFLD,IER)
        CALL QIPBTCPU('VTRKEX',1)
        IF(IER.GT.0) THEN
          WRITE(*,*) ' ********** ERROR IN VTRKEX *******'
        ENDIF
      ENDIF
C
C---- Select good tracks, flag their quality, assign them to jets/hemi.,
      CALL QIPBTCPU('TRSEL',0)
      CALL TRSEL(IP,NJET,TRKJET,NV0,V0PAIR,V0PAR,V0ERR,V0PAT,HAXIS,
     & NDAU,TRKDAU,NTRACK,TPAR,TERR,TFLAG,VDHITP,FRF2TRK,JJET,JHEMI)
      CALL QIPBTCPU('TRSEL',1)
      IF (NTRACK .LE. 0) THEN
         IRET = 4
         GOTO 9000
      ENDIF
C
C---- Get the impact parameters of the selected tracks
      CALL QIPBTCPU('FINDDMIN',0)
      CALL FINDDMIN(NTRACK,FRF2TRK,TPAR,TERR,NJET,JETS,JJET,TFLAG,
     &    NDAU,TRKDAU,DMIN,S_DMIN,PHIP,JDIST,LDIST,S_LDIST,PERP )
      CALL QIPBTCPU('FINDDMIN',1)
C
C---- Make a final track select cuts, using all available information
      CALL QIPBTCPU('TRSEL2',0)
      CALL TRSEL2(NTRACK,TPAR,TERR,JJET,JHEMI,FRF2TRK,
     &  TFLAG,DMIN,S_DMIN,PHIP,JDIST,LDIST,S_LDIST,PERP,JETS)
      CALL QIPBTCPU('TRSEL2',1)
      IF (NTRACK .LE. 0) THEN
         IRET = 4
         GOTO 9000
      ENDIF
C
C---- Continue point for Nano-dst reading
 5000 CONTINUE
C
C---- Calculate track probabilities
      CALL QIPBTCPU('BTAGTR',0)
      CALL BTAGTR (FITP,NTRACK,DMIN,S_DMIN,TFLAG,PROBTRK)
      CALL QIPBTCPU('BTAGTR',1)
C
C---- Calculate the probabilities of the event,hemispheres and jets
      CALL QIPBTCPU('BTAGEV1',0)
      CALL BTAGEV (NTRACK,NJET,JJET,JHEMI,PROBTRK,NEGPROB,
     &             PROBJET,PROBHEMI,PROBEVT)
      CALL QIPBTCPU('BTAGEV1',1)
C
C---- Calculate the corresponding negative probabilities.
      DO ITRK=1,NTRACK
         NPROBTRK(ITRK) = -PROBTRK(ITRK)
      ENDDO
      CALL QIPBTCPU('BTAGEV2',0)
      CALL BTAGEV(NTRACK,NJET,JJET,JHEMI,NPROBTRK,NEGPROB,
     &            NPROBJET,NPROBHEMI,NPROBEVT)
      CALL QIPBTCPU('BTAGEV2',1)
C
C     If we're calibrating, fill the histograms.  Here, we make a cut
C     on the opposite hemisphere probability, to reduce the
C     mis-signed tracks in our sample.  The original calibration has to
C     be 'reasonable' for this to work (though nothing terrible will
C     happen even if the original calibration is bad).
C
      IF (CALIB .OR. IDB.GT.0) THEN
        DO ITRACK = 1,NTRACK
          IHEMI = MOD(JHEMI(ITRACK),2)+1
          IF (NEWCOD) THEN
            WEIGHT = 1.0
            CALL HFILL(IDB+150+TTYPE(TFLAG(ITRACK)),
     &         -DMIN(ITRACK)/S_DMIN(ITRACK),0.0,WEIGHT)
          ELSE
            IF (PROBHEMI(IHEMI).GT.EVECUT .AND.
     .         DMIN(ITRACK).LT.0.0) THEN
C
C------------- Find the weight; this is an uneven-binned histogram
              IF (-DMIN(ITRACK)/S_DMIN(ITRACK) .LE. 4.) THEN
                WEIGHT = 1.0
              ELSEIF(-DMIN(ITRACK)/S_DMIN(ITRACK) .LE. 36.) THEN
                WEIGHT = 0.1
              ELSE
                WEIGHT = 0.01
              ENDIF
C
C------------- Separate by track flag
              CALL HFILL(IDB+150+TTYPE(TFLAG(ITRACK)),
     &           -DMIN(ITRACK)/S_DMIN(ITRACK),0.0,WEIGHT)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
 9000 CONTINUE
C
C---- Diagnostic histograms
      IF (IDB .GT. 0) THEN
         CALL HFILL(IDB+10,FLOAT(IRET),0.0,1.0)
C
C------- V0 histograms
         CALL HFILL(IDB+60,FLOAT(NV0),0.0,1.0)
         DO ITYPE = 1,3
            NVTYP(ITYPE) = 0
         ENDDO
         DO IV0 = 1,NV0
            NVTYP(V0TYPE(IV0)) = NVTYP(V0TYPE(IV0)) + 1
            CALL HFILL(IDB+62,V0PROB(IV0),0.0,1.0)
         ENDDO
         DO ITYPE = 1,3
            CALL HFILL(IDB+61,FLOAT(ITYPE),0.0,FLOAT(NVTYP(ITYPE)))
         ENDDO
C
C------- Probability histograms
         IF (IRET .EQ. 0)THEN
            DO ITRACK = 1,NTRACK
               CALL HFILL(IDB+20,PROBTRK(ITRACK),0.0,1.0)
            ENDDO
            DO IJET = 1,NJET
               CALL HFILL(IDB+21,PROBJET(IJET),0.0,1.0)
               CALL HFILL(IDB+24,NPROBJET(IJET),0.0,1.0)
            ENDDO
            DO IHEMI = 1,2
               CALL HFILL(IDB+22,PROBHEMI(IHEMI),0.0,1.0)
               CALL HFILL(IDB+25,NPROBHEMI(IHEMI),0.0,1.0)
            ENDDO
            CALL HFILL(IDB+23,PROBEVT,0.0,1.0)
            CALL HFILL(IDB+26,NPROBEVT,0.0,1.0)
         ENDIF
      ENDIF
C
      RETURN
      END
#endif
