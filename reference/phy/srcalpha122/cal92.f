      SUBROUTINE CAL92(KRUN)
C ---------------------------------------------------------------------
C! Correction for 1992
C  Called from CALYEAR
C  Author A.ROUGE 15/01/94
C    END-CAP A Z>0.
C      ' '   B Z<0.
C ---------------------------------------------------------------------
      COMMON / ECCMF / ECCM(36) ,NRMIN,NRMAX
      DIMENSION SCECA(12) , SCBAR(12) , SCECB1(12) , SCECB2(12)
      DIMENSION  EREF(3)
c
       DATA SCECA /43.04,43.04,43.29,43.34,43.46,43.18,43.11,
     &  43.08,43.19,43.16,43.21,42.86/
       DATA SCBAR /43.31,43.35,43.54,43.50,43.47,43.50,43.55,
     &  43.45,43.51,43.48,43.37,43.42/
       DATA SCECB1/43.55,42.92,43.31,43.13,42.94,42.99,42.97,
     &  43.28,43.39,43.16,43.18,43.18/
       DATA SCECB2/43.33,42.99,43.29,43.01,42.61,43.25,43.06,
     &  43.38,43.17,43.29,43.36,43.24/
c
       DATA EREF/43.15,43.42,43.15/
C ---------------------------------------------------------------------
       DO NMODU =1,36
       CORF = 1
       IREG=2
       IF(NMODU.GT.24) IREG=3
       IF(NMODU.LT.13) IREG=1
       IMODU = NMODU-(IREG-1)*12
C
       IF(IREG .EQ. 1 ) THEN
          CORF = 0.988
          IF(KRUN .LE. 16500 ) THEN
             CORF = CORF/43.13*EREF(IREG)
          ENDIF
          IF(KRUN .GT. 16500 .AND. KRUN .LE. 17000)THEN
             CORF = CORF /42.99*EREF(IREG)
          ENDIF
          IF(KRUN .GT. 17000) THEN
             CORF = CORF /43.55*EREF(IREG)
          ENDIF
          CORF = CORF / SCECA(IMODU)*EREF(IREG)
       ENDIF
       IF(IREG .EQ. 2 ) THEN
          CORF = CORF*0.992
          IF(KRUN .GT. 16500 .AND. KRUN .LE. 17000) THEN
             CORF = CORF / 43.49*EREF(IREG)
          ENDIF
          IF(KRUN .GT. 17000 .AND. KRUN .LE. 18000) THEN
             CORF = CORF/43.54*EREF(IREG)
          ENDIF
          CORF = CORF / SCBAR(IMODU)*EREF(IREG)
       ENDIF
       IF(IREG .EQ. 3 ) THEN
          CORF = CORF*0.987
          IF(KRUN .GT. 14500 .AND. KRUN .LE. 16500 ) THEN
             CORF = CORF/42.99*EREF(IREG)
          ENDIF
          IF(KRUN .GT. 16500 .AND. KRUN .LE. 17000 ) THEN
             CORF = CORF / 43.19*EREF(IREG)
          ENDIF
          IF(KRUN .GT. 17000 .AND. KRUN .LE. 18000 ) THEN
             CORF = CORF/43.49*EREF(IREG)
          ENDIF
          IF(KRUN .LT. 17000) THEN
             CORF = CORF / SCECB1(IMODU)*EREF(IREG)
          ELSE
             CORF = CORF / SCECB2(IMODU)*EREF(IREG)
          ENDIF
       ENDIF
C
       ECCM(NMODU) = ECCM(NMODU)*CORF
       ENDDO
       END
