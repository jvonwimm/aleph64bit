      SUBROUTINE SMCOPY(USETRK,
     +    NTRACK  ,JJET  ,JHEMI  ,FRF2TRK  ,TFLAG  ,DMIN  ,S_DMIN  ,
     +    PHIP  ,JDIST  ,LDIST  ,S_LDIST  ,TPAR  ,TERR  ,PERP  ,
     +    NTRACK_B,JJET_B,JHEMI_B,FRF2TRK_B,TFLAG_B,DMIN_B,S_DMIN_B,
     +    PHIP_B,JDIST_B,LDIST_B,S_LDIST_B,TPAR_B,TERR_B,PERP_B)
CKEY QIPBTAG / INTERNAL
C---------------------------------------------------------------------
C!  Make copy of QIPBTAG track information. Auxiliary to QSMEAR.
C!  Author: Ian Tomalin 30-5-96
C!
C!    INPUT: 
C!      LOGICAL USETRK: .TRUE. if track is to be copied.
C!         +    List of QIPBTAG track arrays.
C!    OUTPUT:
C!              List of QIPBTAG track arrays.
C---------------------------------------------------------------------
#ifndef DOC
      LOGICAL USETRK(*)
C QIPBTAG track information.
      INTEGER NTRACK
      INTEGER FRF2TRK(*)
      INTEGER TFLAG(*)
      INTEGER JHEMI(*),JJET(*)
      REAL    TPAR(5,*),TERR(4,4,*)
      REAL    DMIN(*),S_DMIN(*)
      REAL    PHIP(*)
      REAL    JDIST(*),LDIST(*)
      REAL    S_LDIST(*),PERP(3,*)
C Used to backup QIPBTAG track information.
      INTEGER NTRACK_B
      INTEGER FRF2TRK_B(*)
      INTEGER TFLAG_B(*)
      INTEGER JHEMI_B(*),JJET_B(*)
      REAL    TPAR_B(5,*),TERR_B(4,4,*)
      REAL    DMIN_B(*),S_DMIN_B(*)
      REAL    PHIP_B(*)
      REAL    JDIST_B(*),LDIST_B(*)
      REAL    S_LDIST_B(*),PERP_B(3,*)
C---------------------------------------------------------------------
      JTRK = 0
      DO 50 ITRK = 1,NTRACK
        IF (USETRK(ITRK)) THEN
          JTRK = JTRK + 1
          JJET_B(JTRK)     = JJET(ITRK)
          JHEMI_B(JTRK)    = JHEMI(ITRK)
          FRF2TRK_B(JTRK)  = FRF2TRK(ITRK)
          TFLAG_B(JTRK)    = TFLAG(ITRK)
          DMIN_B(JTRK)     = DMIN(ITRK)
          S_DMIN_B(JTRK)   = S_DMIN(ITRK)
          PHIP_B(JTRK)     = PHIP(ITRK)
          JDIST_B(JTRK)    = JDIST(ITRK)
          LDIST_B(JTRK)    = LDIST(ITRK)
          S_LDIST_B(JTRK)  = S_LDIST(ITRK)
          DO IPAR=1,5
            TPAR_B(IPAR,JTRK) = TPAR(IPAR,ITRK)
          END DO
          DO IPAR=1,4
            DO JPAR=1,4
              TERR_B(IPAR,JPAR,JTRK) = TERR(IPAR,JPAR,ITRK)
            END DO
          END DO
          DO IPAR=1,3
            PERP_B(IPAR,JTRK) = PERP(IPAR,ITRK)
          END DO
        END IF
   50 CONTINUE
      NTRACK_B = JTRK
      END
#endif
