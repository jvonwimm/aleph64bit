      COMMON / ECPASH / CUTAEC(3),KLASEC(3),EMINEC,ETRAEC,SEUSEC,
     + ASB1EC(4),ASB2EC(4),USBEC1(4),USBEC2(4),
     + SGA1EC(4),SGA2EC(4),SGB1EC(4),SGB2EC(4),SGA3EC(4),SGB3EC(4),
     + SEUTEC,SAMBEC,STRAEC,SMT1EC,TOW4EC,
     + ALAPEC,TLAPEC,SLAPEC,CFNTEC(3)
#if defined(DOC)
C
C!  Parameters used for neutral Ecal Objects.
C
C     CUTAEC  Stack energy cuts for classe definition.
C     KLASEC  Index for classification.
C     EMINEC  Seuil en energie pour analyse longitudinale.
C     ETRAEC  Energies de transition pour electrons.
C     SEUSEC  Energie de transition pour SIGM**
C     ASB1EC  A/B for 1 Gev
C     ASB2EC  A/B dependance in Log(E).
C     USBEC1  1/B for 1 Gev.
C     USBEC2  1/B dependance in Log(E).
C     SGA1EC  (d(1/A)/(1/A))**2 for E --> infinite.
C     SGA2EC  (d(1/A)/(1/A))**2 dependance in 1/E.
C     SGA3EC  (d(1/A)/(1/A))**2 for E < SEUSEC
C     SGB1EC  (d(B/A)/(B/A))**2 for E --> infinite.
C     SGB2EC  (d(B/A)/(B/A))**2 dependance in 1/E.
C     SGB3EC  (d(B/A)/(B/A))**2 for E < SEUSEC
C     SEUTEC  Neutron decision from class probability.
C     SAMBEC  Ambiguous decision from longitudinal shape.
C     SMT1EC  Gamma cross section factor.
C     TOW4EC  Pi0 decision from tower #4.
C     STRAEC  4 towers threshold decision.
C     ALAPEC  Parameter to correct overlap energy.
C     TLAPEC  Parameter to correct overlap energy.
C     SLAPEC  Parameter to correct overlap energy.
C     CFNTEC  Hadronic weights.
#endif
