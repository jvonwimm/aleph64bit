      SUBROUTINE FINDIP(BP,SIG2_BP,NJET,JETS,NTRACK,TPAR,TERR,IGOOD,
     & IP,IP_ERR,CHI2_DOF,NUSED)
CKEY   QFNDIP  / INTERNAL
C ----------------------------------------------------------------------
C! A standalone routine to find the interaction point, given the beamspo
C  the jet axes, and the tracks.  See ALEPH note 92-47 for a description
C  Called from  QFNDIP
C
C  Updated 15-9-93 Dave Brown, jet error parameterized, vector calculati
C  simplified, chisquared calculation simplified, some cuts retuned.
C ----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
C
C  Inputs;
C  beam point and sigmas**2, number of jets, jet momentum vectors
C  Track parameters and d0-Z0 corner of the error marix
C
      INTEGER NJET,NTRACK,IGOOD(*)
      REAL BP(3),SIG2_BP(3),JETS(3,*)
      REAL TPAR(5,*),TERR(3,*)
C
C  Outputs; Interaction point with full covariance matrix,  chisq/dof,
C  number of tracks used
C
      REAL IP(3),IP_ERR(3,3),CHI2_DOF, CHI2_DOF_SAVE
      REAL NEWIP(3),NEWIP_ERR(3,3)
      INTEGER NUSED(2)
C
C  Local variables
C
C
C --     MAX. NUMBER OF TRACKS, jets, V0s ALLOWED
C
      INTEGER    MAXTRK,MAXJET,MAXV0,MAXDAU
      PARAMETER ( MAXTRK = 400, MAXJET = 40 , MAXV0 = 20 , MAXDAU = 10 )
      INTEGER IALTRK,ICOR,JCOR,IERR
      INTEGER ITRK,IMIN,JMIN,IMETA,JTRK
      INTEGER IJET,JJET,MAX_META,NDOF,NGOOD
      INTEGER KJET(MAXTRK),NUSED_SAVE(2)
      INTEGER ITYP,JTYP
      INTEGER IFLAG(MAXTRK),JFLAG,NTYP(4)
      REAL MAX_CHI2
      REAL D0,PHI0,Z0,TANL,COSL,SINL,IR
      REAL PHI,COSP0,SINP0,COSP,SINP,DS
      REAL T0(3),T1(3),J1(3,MAXJET),JP
      REAL CHI2,CHIDIFF,MAX_DIF,CHI2_SAVE
      REAL MAXDOT, DOT, CHI
      REAL LEN(4),SIG(4)
      REAL DIST,DIST_ERR
      REAL DELTA(3,3)
      REAL NU(MAXTRK)
      REAL HAT(3,4,MAXTRK)
      REAL DT0(4,MAXTRK),JT0(MAXTRK)
      REAL ERR2(5,MAXTRK)
      REAL OLD_IP(3)
      REAL JET_SIG2(MAXJET),JERR2,PREC
      REAL JMOM,NSIG
      REAL ALPHA, BETA(3), GAMMA(3,3)
      REAL ALPHA_SAVE, BETA_SAVE(3), GAMMA_SAVE(3,3)
      LOGICAL GOOD,USEIT
      DATA DELTA/1.,0.,0.,0.,1.,0.,0.,0.,1./
C
C  Common for computing 1-track-removed vertex
C
      COMMON/IPVTX/DT0,HAT,ERR2,IFLAG,
     &      ALPHA_SAVE,BETA_SAVE,GAMMA_SAVE,NUSED_SAVE, CHI2_DOF_SAVE
C
C  Cut values
C
      DATA MAX_CHI2/9.0/
      DATA MAX_META/5/
      DATA PREC/1.0/
      DATA NSIG/4.0/
C ----------------------------------------------------------------------
C  Inlines for defining good tracks
      GOOD(ITRK) = IGOOD(ITRK).GT.0
      USEIT(JFLAG,ITYP) = IAND(JFLAG,ISHFT(1,ITYP-1)).GT.0
C ----------------------------------------------------------------------
C
C  Normalize the jets
C
      DO IJET=1,NJET
        JP = 0.0
        DO ICOR=1,3
          JP = JP + JETS(ICOR,IJET)**2
        END DO
        JP = SQRT(JP)
        DO ICOR=1,3
          J1(ICOR,IJET) = JETS(ICOR,IJET)/JP
        END DO
      END DO
C
C  Get jet errors
C
      CALL JETERR(NJET,JETS,JET_SIG2)
C
C  loop over the tracks
C
      NDOF = 0
      DO ITRK=1,NTRACK
C
C  Remove tracks locked off in the previous routine.
C
        IF(.NOT.GOOD(ITRK))THEN
          IFLAG(ITRK) = 0
          GOTO 1231
        END IF
C
C  Preset the tracks not to be skipped and not to be used along the jet
C
        IFLAG(ITRK) = 4
C
C  Convert the track parameters into vector notation.
C  Correct for the curvature due to beamspot displacement.
C
        IR  = TPAR(1,ITRK)
        TANL= TPAR(2,ITRK)
        PHI0 = TPAR(3,ITRK)
        D0  = TPAR(4,ITRK)
        Z0  = TPAR(5,ITRK)
        COSL = 1./SQRT(1.+TANL**2)
        SINL = SIGN(SQRT(1.-COSL**2),TANL)
        COSP0 = COS(PHI0)
        SINP0 = SIN(PHI0)
        DS = BP(1)*COSP0+BP(2)*SINP0
        PHI = PHI0+IR*DS
        COSP = COS(PHI)
        SINP = SIN(PHI)
        T1(1) =  COSL*COSP
        T1(2) =  COSL*SINP
        T1(3) =  SINL
        T0(1) =  D0*SINP0 + (SINP-SINP0)/IR
        T0(2) = -D0*COSP0 - (COSP-COSP0)/IR
        T0(3) =  Z0 + TANL*DS
C
C  Correct the error matrix for the projection of Z
C
        TERR(1,ITRK) = TERR(1,ITRK)
        TERR(2,ITRK) = TERR(2,ITRK)*COSL**2
        TERR(3,ITRK) = TERR(3,ITRK)*COSL
C
C  Associate the track with one of the jets by minimizing the dot
C  product
C
        MAXDOT = -1000.
        DO IJET=1,NJET
          DOT = 0.0
          DO ICOR=1,3
            DOT = DOT + T1(ICOR)*J1(ICOR,IJET)
          END DO
          IF(DOT .GT. MAXDOT)THEN
            JJET = IJET
            MAXDOT = DOT
          END IF
        END DO
        KJET(ITRK) = JJET
C
C  Find the track-jet angle this implies
C
        NU(ITRK) = ACOS(MIN(MAXDOT,1.0))
        CHI = NU(ITRK)/SQRT(JET_SIG2(JJET))
C
C  Compute the unit vectors and errors for this track.
C
        CALL TROT(TERR(1,ITRK),T1,J1(1,JJET),NU(ITRK),
     &        HAT(1,1,ITRK),ERR2(1,ITRK))
        ERR2(5,ITRK) = ERR2(3,ITRK)
C
C  Save projections of T0 onto the different unit vectors.
C
        DO ITYP=1,4
          DT0(ITYP,ITRK) = 0.0
          DO ICOR=1,3
            DT0(ITYP,ITRK) = DT0(ITYP,ITRK) +
     &            HAT(ICOR,ITYP,ITRK)*T0(ICOR)
          END DO
        END DO
        JT0(ITRK) = 0.0
        DO ICOR=1,3
          JT0(ITRK) = JT0(ITRK) + J1(ICOR,JJET)*T0(ICOR)
        END DO
 1231   CONTINUE
      END DO
C ----------------------------------------------------------------------
C  Setup tensors and iterations to find the IP
C
      DO ICOR=1,3
        OLD_IP(ICOR) = -1000.
      END DO
      IMETA = 0
C
C  Meta-iteration loop on track errors
C
 1444 CONTINUE
C
C  Put the beamspot information into the intial tensors
C
      ALPHA = 0.0
      DO ICOR=1,3
        ALPHA = ALPHA + BP(ICOR)**2/SIG2_BP(ICOR)
        BETA(ICOR) = BP(ICOR)/SIG2_BP(ICOR)
        DO JCOR=1,3
          GAMMA(JCOR,ICOR) = DELTA(JCOR,ICOR)/SIG2_BP(ICOR)
        END DO
      END DO
C
C  Loop over the different track components; first, put
C  in all the tracks, then do the outlyer search. Loop in
C  the order; Perp to the jet, phi direction, theta direction.
C
C
C  Put track information into the tensors
C
      NGOOD = 0
      DO ITRK=1,NTRACK
        DO ITYP=1,3
          IF(GOOD(ITRK).AND.USEIT(IFLAG(ITRK),ITYP))THEN
            ALPHA = ALPHA + DT0(ITYP,ITRK)**2/ERR2(ITYP,ITRK)
            DO ICOR=1,3
              BETA(ICOR) = BETA(ICOR) +
     &              DT0(ITYP,ITRK)*HAT(ICOR,ITYP,ITRK)/ERR2(ITYP,ITRK)
              DO JCOR=1,3
                GAMMA(JCOR,ICOR) = GAMMA(JCOR,ICOR) +
     &                HAT(ICOR,ITYP,ITRK)*HAT(JCOR,ITYP,ITRK)/
     &                ERR2(ITYP,ITRK)
              END DO
            END DO
            NGOOD = NGOOD + 1
          END IF
        END DO
      END DO
C
C  Save the original tensors
C
      ALPHA_SAVE = ALPHA
      DO ICOR=1,3
        BETA_SAVE(ICOR) = BETA(ICOR)
        DO JCOR=1,3
          GAMMA_SAVE(JCOR,ICOR) = GAMMA(JCOR,ICOR)
        END DO
      END DO
C
C  Solve for chisquared
C
      CALL RSINV(3,GAMMA,3,IERR)
      IF(IERR .NE. 0)THEN
        NUSED(1) = -1
        NUSED(2) = -1
        GOTO 1000
      END IF
      CHI2_SAVE = ALPHA
      DO ICOR=1,3
        DO JCOR=1,3
          CHI2_SAVE = CHI2_SAVE -
     &          BETA(ICOR)*BETA(JCOR)*GAMMA(JCOR,ICOR)
        END DO
      END DO
C
C  Iterate over outlyers; this is a DO WHILE for stupid compilers.  This
C  checks to see how the chi2 changes if we remove any one track.
C  The track which contributes most to the chisquared is removed, if
C  it's contribution is large.
C
 1111 CONTINUE
      MAX_DIF = 0.0
      DO ITRK=1,NTRACK
        DO ITYP=1,3
          IF(GOOD(ITRK).AND.USEIT(IFLAG(ITRK),ITYP))THEN
C
C  Subtract the track contribution to the tensors
C
            ALPHA = ALPHA_SAVE - DT0(ITYP,ITRK)**2/ERR2(ITYP,ITRK)
            DO ICOR=1,3
              BETA(ICOR) = BETA_SAVE(ICOR) -
     &              DT0(ITYP,ITRK)*HAT(ICOR,ITYP,ITRK)/
     &              ERR2(ITYP,ITRK)
              DO JCOR=1,3
                GAMMA(JCOR,ICOR) = GAMMA_SAVE(JCOR,ICOR) -
     &                HAT(ICOR,ITYP,ITRK)*HAT(JCOR,ITYP,ITRK)/
     &                ERR2(ITYP,ITRK)
              END DO
            END DO
C
C  Solve for 1-removed chisquared
C
            CALL RSINV(3,GAMMA,3,IERR)
            IF(IERR .NE. 0)THEN
              NUSED(1) = -1
              NUSED(2) = -1
              GOTO 1000
            END IF
            CHI2 = ALPHA
            DO ICOR=1,3
              DO JCOR=1,3
                CHI2 = CHI2 - BETA(ICOR)*BETA(JCOR)*GAMMA(JCOR,ICOR)
              END DO
            END DO
C
C  Latch on the biggest difference
C
            CHIDIFF = CHI2_SAVE - CHI2
            IF(CHIDIFF .GT. MAX_DIF)THEN
              MAX_DIF = CHIDIFF
              IMIN = ITRK
              JTYP = ITYP
            END IF
          END IF
        END DO
      END DO
C
C  If the biggest difference is above the limit, flag off the
C  offending track and try again.
C
      IF (MAX_DIF .GE. MAX_CHI2) THEN
        IFLAG(IMIN) = IFLAG(IMIN) - ISHFT(1,JTYP-1)
        NDOF = NDOF - 1
        CHI2_SAVE = CHI2_SAVE - MAX_DIF
C
C  Subtract the bad track from the saved tensors
C
        ALPHA_SAVE = ALPHA_SAVE - DT0(JTYP,IMIN)**2/
     &        ERR2(JTYP,IMIN)
        DO ICOR=1,3
          BETA_SAVE(ICOR) = BETA_SAVE(ICOR) -
     &          DT0(JTYP,IMIN)*HAT(ICOR,JTYP,IMIN)/
     &          ERR2(JTYP,IMIN)
          DO JCOR=1,3
            GAMMA_SAVE(JCOR,ICOR) = GAMMA_SAVE(JCOR,ICOR) -
     &            HAT(ICOR,JTYP,IMIN)*HAT(JCOR,JTYP,IMIN)/
     &            ERR2(JTYP,IMIN)
          END DO
        END DO
        GOTO 1111
      END IF
C
C  Copy the saved tensors
C
      ALPHA = ALPHA_SAVE
      DO ICOR=1,3
        BETA(ICOR) = BETA_SAVE(ICOR)
        DO JCOR=1,3
          GAMMA(JCOR,ICOR) = GAMMA_SAVE(JCOR,ICOR)
        END DO
      END DO
C
C  Solve for the IP
C
      CALL RSINV(3,GAMMA,3,IERR)
      IF(IERR .NE. 0)THEN
        NUSED(1) = -1
        NUSED(2) = -1
        GOTO 1000
      END IF
      DO ICOR=1,3
        IP(ICOR) =  0.0
        DO JCOR=1,3
          IP(ICOR) = IP(ICOR) + BETA(JCOR)*GAMMA(JCOR,ICOR)
        END DO
      END DO
C
C  Check to see if the IP has moved significantly (>1 sigma) this
C  meta-iteration.  If so, re-calculate the errors and start again
C
      DIST = 0.0
      DO ICOR=1,3
        DIST = DIST + (OLD_IP(ICOR)-IP(ICOR))**2
      END DO
      DIST_ERR = 0.0
      IF(DIST .GT. 0.0)THEN
        DO ICOR=1,3
          DO JCOR=1,3
            DIST_ERR = DIST_ERR + (OLD_IP(ICOR)-IP(ICOR))*
     &      (OLD_IP(JCOR)-IP(JCOR))*GAMMA(JCOR,ICOR)/DIST
          END DO
        END DO
      END IF
      IF(DIST .GT. PREC*DIST_ERR .AND. IMETA .LT. MAX_META)THEN
        IMETA = IMETA + 1
C
C  Loop over the tracks, checking to see which degrees of freedom
C  are useable
C
        NDOF = 0
        DO ITRK=1,NTRACK
          JJET = KJET(ITRK)
C
C  Check the quality of all tracks
C
          IF(GOOD(ITRK))THEN
C
C  Find the distance between the track and the new IP
C
            DO ITYP=1,4
              LEN(ITYP) = DT0(ITYP,ITRK)
              SIG(ITYP) = SQRT(ERR2(ITYP,ITRK))
              DO ICOR=1,3
                LEN(ITYP) = LEN(ITYP) -  HAT(ICOR,ITYP,ITRK)*IP(ICOR)
              END DO
            END DO
C
C  Now, quality checks for re-flagging the tracks.  If the
C  projection along the jet is negative, and the track is
C  close in 3-d, then call this a good full track
C
            IF(LEN(4) .LT. 0.0 .AND.
     &         ABS(LEN(1)) .LT. NSIG*SIG(1) .AND.
     &         ABS(LEN(2)) .LT. NSIG*SIG(2) )THEN
              IFLAG(ITRK) = 3
              NDOF = NDOF + 2
C
C  Otherwise, if the track is close in the perpendicular plane,
C  keep it
C
            ELSE IF(ABS(LEN(3)) .LT. NSIG*SIG(3))THEN
              IFLAG(ITRK) = 4
              NDOF = NDOF + 1
C
C  Add a term to the error for the jet resolution.
C
              IF(NU(ITRK).GT.0.0)THEN
                JERR2 = MIN(LEN(4)**2,
     &                JET_SIG2(JJET)*(LEN(4)/NU(ITRK))**2)
              ELSE
                JERR2 = LEN(4)**2
              END IF
              ERR2(3,ITRK) = ERR2(5,ITRK) + JERR2
C
C  Else forget about this track
C
            ELSE
              IFLAG(ITRK) = 0
            END IF
          END IF
        END DO
C
C  Save the IP for the next comparison
C
        DO ICOR=1,3
          OLD_IP(ICOR) = IP(ICOR)
        END DO
C
C  Iterate
C
        GOTO 1444
      END IF
C ----------------------------------------------------------------------
C  Final convergence; solve for the chisquared and pack the error matrix
C
      CHI2 = ALPHA
      DO ICOR=1,3
        DO JCOR=1,3
          IP_ERR(JCOR,ICOR) = GAMMA(JCOR,ICOR)
          CHI2 = CHI2 - BETA(ICOR)*BETA(JCOR)*GAMMA(JCOR,ICOR)
        END DO
      END DO
C
C  2 constraints from the BP, 3 variables solved for
C
      CHI2_DOF = CHI2/MAX(1,NDOF-1)
      CHI2_DOF_SAVE = CHI2_DOF
C
C  Number of tracks used, separated by direction
C
      DO ITYP=1,3
        NTYP(ITYP) = 0
        DO ITRK=1,NTRACK
          IF(GOOD(ITRK).AND.USEIT(IFLAG(ITRK),ITYP))THEN
            NTYP(ITYP)=NTYP(ITYP)+1
          END IF
        END DO
      END DO
      NUSED(1) = NTYP(3)+NTYP(1)
      NUSED(2) = NTYP(2)
      NUSED_SAVE(1) = NUSED(1)
      NUSED_SAVE(2) = NUSED(2)
1000  CONTINUE
      RETURN
      END