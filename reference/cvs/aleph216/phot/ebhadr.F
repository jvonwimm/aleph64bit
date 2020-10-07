      SUBROUTINE EBHADR(IHAD,NUST,INDX,ESTO,PHOT,NATU,GAMM)
C -----------------------------------------------------------------
C   AUTHOR   : J.Badier   09/10/89
C!  Hadronic neutral clusters.
CKEY PHOTONS HADRON / INTERNAL
C
C   The energy is corrected by the pion/electron ratio.
C   A transverse analysis is performed (not yet implemented)
C
C   Input  :    IHAD    Correction option.
C               NUST    Number of storeys of the cluster ICLN.
C               INDX(1,IST) Theta index of the storey IST.
C               INDX(2,IST) Phi index of the storey IST.
C               INDX(3,IST) Stack number of the storey IST.
C               ESTO(IST)   Raw content of the storey IST.
C                           IST = 1 , NUST
C               PHOT
C
C   Output :    NATU(1)  Transverse identification.
C               NATU(2)  Nature of the first identified particle.
C               NATU(3)  Nature of the second identified particle.
C               GAMM(1)  First transverse estimator : proportion of
C                        energy in the four central towers.
C               GAMM(2)  Second transverse estimator : deviation from the
C                        mean shape of a single photon. The distribution
C                        is a normal law for an isolated gamma.
C               GAMM(3)  Best estimate of the first particle energy (Gev)
C               GAMM(4)  Teta line of flight of the first particle.
C               GAMM(5)  Phi line of flight of the first particle.
C               GAMM(6)  Best estimate of the 2nd particle Energy (Gev)
C               GAMM(7)  Teta line of flight of the second particle.
C               GAMM(8)  Phi line of flight of the second particle.
C               IER      Error code
C                        = 0 No error.
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C     CREATED : NONE
C
C ----------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION INDX(3,*) , ESTO(*) , PHOT(*) , NATU(*) , GAMM(*)
      NATU(1) = 0
      GAMM(1) = PHOT(12)
      NATU(2) = 17
      GAMM(2) = PHOT(13)
      IF( IHAD .NE. 1 ) THEN
        CALL EBCOHD( PHOT , CORH )
        GAMM(3) = CORH * PHOT(1)
      ELSE
        GAMM(3) = PHOT(1)
      ENDIF
      GAMM(4) = PHOT(5)
      GAMM(5) = PHOT(6)
      NATU(3) = 0
      GAMM(6) = .0
      GAMM(7) = .0
      GAMM(8) = .0
      RETURN
      END
#endif
