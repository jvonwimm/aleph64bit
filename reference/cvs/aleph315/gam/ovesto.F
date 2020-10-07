      SUBROUTINE OVESTO(ITHETA,KSTACK,IBAREC)
C--------------------------------------------------------------------
C Marc Verderi                                                 2-10-94
C! Pour une storey itheta,kstack dit si elle est dans la liste des
C storeys pathologiques du barrel ou dans celle des storeys de l'endcap
C en regard des storeys pathologiques du barrel
C  Input : ITHETA,KSTACK(physique) storey
C  Output: IBAREC    =   0   : storey non overlap
C                    =   1   : storey overlap Barrel ie pathologique
C                    =   2   : storey overlap EndCap ie face a une patho
C -----------------------------------------------------------------------
#ifndef DOC
      SAVE
      INTEGER  ITHETA,KSTACK,IBAREC
      INTEGER  NP
      PARAMETER (NP=17)
      INTEGER  PATHOL(NP),PATHOS(NP)
      INTEGER  I
      DATA PATHOL
c    &      /45,46,47,48,49,50,51,52,53,53,54,55,56,57,57,58,59,60/
     &         /46,47,48,49,50,51,52,53,53,54,55,56,57,57,58,59,60/
      DATA PATHOS
C    &      /-1,-1,-1,-1,-1,-1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3/
     &         /-1,-1,-1,-1,-1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3/
C     Rq : pathol = 45 est une definition "large" de l'overlap EndCap
C          pathos < 0 pour endcap

      IBAREC = 0
C storey dans EndCap:
      IF (ITHETA.LT.PATHOL(01) .OR. ITHETA.GT.229-PATHOL(01)) GOTO 999
C storey dans Barrel:
      IF (ITHETA.GT.PATHOL(NP).AND. ITHETA.LT.229-PATHOL(NP)) GOTO 999
C candidate overlap:
      DO I = 1,NP
         IF (  ( ITHETA.EQ.PATHOL(I).OR.ITHETA.EQ.229-PATHOL(I)  )
     &      .AND. KSTACK.EQ.ABS(PATHOS(I))  ) THEN
            IBAREC = 1
            IF (PATHOS(I).LT.0) IBAREC = 2
            GOTO 999
         ENDIF
      ENDDO

999   RETURN
      END
#endif
