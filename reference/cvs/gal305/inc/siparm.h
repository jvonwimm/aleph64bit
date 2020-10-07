*CD siparm
      LOGICAL SIPARF
      COMMON /SIPARM/   SINORM,SIALPH(2),
     &                  SIGMAA(12),SIGMAB(12),SIGMAC(12),
     &                  SIRAAB(12),SIRABC(12),
     &                  SIFLUC,SIPERG,SIPARF
#if defined(DOC)
C!    Quantities related to parametrisation of electromagnetic showers
      in SICAL
#endif
