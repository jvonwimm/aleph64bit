      SUBROUTINE FLV0DX(ITK,QCHARGE,BFIELD,CHI)
C---------------------------------------------------------------------------
C!    Calculate the dE/dX of track ITK assuming Proton, Electron and Pion
C     ITK    : Track number
C     QCHARGE: charge of ITK track
C     BFIELD : magnetic field in the TPC
C     CHI(3) : dE/dX pull of ITK assuming Proton, Electron, Pion
C-----------------------------------------------------------------------
#ifndef DOC
      PARAMETER (NHYP = 3)
      DIMENSION RMASS(NHYP),QDE(NHYP),RIEXP(NHYP),SIGMA(NHYP),CHI(NHYP)
      DATA RMASS/0.938272, 0.000511, 0.1395675/

      DO IHYP = 1, NHYP
        QDE(IHYP) = QCHARGE
        CHI(IHYP) = 1000.
      ENDDO
      CALL TIDHYP(ITK,BFIELD,NHYP,RMASS,QDE,RI,NS,TL,
     &              RIEXP,SIGMA,IER)
      IF (IER .EQ. 0 .AND. NS .GE. 50)THEN
        DO IHYP = 1, NHYP
            IF(SIGMA(IHYP).NE.0)THEN
                CHI(IHYP) = (RI-RIEXP(IHYP))/SIGMA(IHYP)
            ELSE
                CHI(IHYP) = 1000.
            ENDIF
        ENDDO
      ENDIF
      RETURN
      END
#endif
