      SUBROUTINE SIDFPA (ENERG)
C--------------------------------------------------------------
C! Defines shower parametrisation parameters
C - J.Rander and B.Bloch-Devaux        920110
CKEY SICAL SHOWER
C - Input   :   ENERG / R   = particle's energy
C - Output  :   /SIPARM/
C
C - Called by  SISHOW             from this .HLB
C - Calls      RANNOR,GAMMA       from Cernlibs
C--------------------------------------------------------------------
      SAVE
      LOGICAL SIPARF
      COMMON /SIPARM/   SINORM,SIALPH(2),
     &                  SIGMAA(12),SIGMAB(12),SIGMAC(12),
     &                  SIRAAB(12),SIRABC(12),
     &                  SIFLUC,SIPERG,SIPARF
      DIMENSION C1(2),C2(2),D1(2),D2(2),DA(2),DB(2),ALIM(3)
      DATA IPAS/0/
C ------------------------------------------------------------
C - 1st entry
      IF (IPAS.EQ.0) THEN
C => initialize constants
         ELOW=  0.02
         ELIM=  0.1
         C1(1)=  5.552
         C1(2)=  6.36
         C2(1)=  1.105
         C2(2)=  1.53
         D1(1)=  2.357
         D1(2)=  4.31
         D2(1)= -0.170
         D2(2)=  1.07
C JOHN SCALE FACTOR..ADJUST SHOWER LONGIT FLUCS
         DJOHNA= 0.900
         DJOHNB= 1.000
         DA(1)=  0.041*DJOHNA
         DA(2)=  .073*DJOHNA
         DB(1)=  0.013*DJOHNB
         DB(2)=  0.023*DJOHNB
         ALIM(1)=  0.4
         ALIM(2)=  0.5
         ALIM(3)=  0.1
         BMAX   =  10.
         SIFLUC =  0.242
         SIPERG =  100.
C
C get radial tranverse profile
C
         CALL SIDFRT
C
         IPAS=1
      ENDIF
C
C - next entry
C=> Calculate parameters as function of particle's energy
C   skip if energy too low
      IF (ENERG .LT. ELOW ) THEN
         SIALPH(1)=1.
         SIALPH(2)=BMAX
      ELSE
         K=2
         ENLOG=ALOG(ENERG )
         IF( ENERG .GT. ELIM) K=1
         ASURB = C1(K) + C2(K) * ENLOG
         UNSRB = D1(K) + D2(K) * ENLOG
         SIGA = SQRT( DA(1) + DA(2) / ENERG )
         SIGB = SQRT( DB(1) + DB(2) / ENERG )
         CALL RANNOR(ALEA,ALEB)
         SIGA = SIGA * ALEA
         SIGB = SIGB * ALEB
         UNSRA =( 1. + SIGA ) * UNSRB / ASURB
         IF ( UNSRA .LE. ALIM(3)) UNSRA = ALIM(3)
         SIALPH ( 1 ) = 1. / UNSRA
         IF ( SIALPH ( 1 ) .LT. ALIM(1) ) SIALPH ( 1 ) = ALIM(1)
         BSURA = ( 1. + SIGB ) / ASURB
         SIALPH(2) = BSURA / UNSRA
         IF ( SIALPH ( 2 ) .LT. ALIM(2)  ) SIALPH ( 2 ) = ALIM(2)
      ENDIF
C ----- Normalisation.
      SINORM = SIALPH(2)**SIALPH(1)*ENERG/GAMMA(SIALPH(1))
C
C--- calculate useful quantities for radial distribution
C
  999 RETURN
C
      END
