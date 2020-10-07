      SUBROUTINE ENOISE
C-----------------------------------------------------------------------
C! Monitor subroutine to identify noisy or shifted channels
CKEY ECAL CLEAN / JULIA USER
C        B. Bloch    September 89
C
C  Structure : SUBROUTINE
C           External references:WBANK,WDROP(BOS77)
C                               UCOPY (CERNLIB)
C           Comdecks references:BCS,BMACRO
C - Input  : none
C - Output : none
C
C - Banks  : input  - JDETDI (NEV) work banks
C            output - JNWORK       work bank with all ETDI banks ,
C                                  dropped at the end
C                   - JFWORK       work bank of means and sigmas (7 col)
C                                  IAD(teta,phi),Means(1:3),Sigmas(1:3)
C?
C!======================================================================
#ifndef DOC
      DIMENSION SUMX(3),SUMX2(3)
#include "etdipt.h"
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      DATA SUMX /3*0./, SUMX2 /3*0./
#include "bmacro.h"
C-----------------------------------------------------------------------
C   Compute length of bank with all addresses from the JDETDI banks
      NLEN = LMHLEN
      DO 9 II = 1,NEV
        NLEN = NLEN + 4 * LROWS(JDETDI(II))
  9   CONTINUE
      CALL WBANK(IW,JNWORK,NLEN,*999)
      IW(JNWORK+LMHCOL) = 4
C   Store all addresses and energies from the work banks
      DO 10 II = 1,NEV
C
         JETDI = JDETDI(II)
         IF ( JETDI.GT.0) THEN
           NETDI = LROWS(JETDI)
           IF ( NETDI.GT.0) THEN
C
C-    Loop on ETDI
C
             DO 100 IETDI = 1,NETDI
                KETDI = KROW(JETDI,IETDI)
                KWORK = KNEXT(JNWORK)
                CALL UCOPY(IW(KETDI+1),IW(KWORK+1),LCOLS(JETDI))
                IW(JNWORK+LMHROW) = IW(JNWORK+LMHROW)+1
 100         CONTINUE
           ENDIF
C
         ENDIF
  10  CONTINUE
C-----------------------------------------------------------------------
C  Sort big bank by increasing addresses
      CALL SORTIQ (IW(JNWORK+3),LCOLS(JNWORK),LROWS(JNWORK),1)
C   Book final bank of means and sigmas per address
      IF ( JFWORK.NE.0) CALL WDROP(IW,JFWORK)
      CALL WBANK(IW,JFWORK,LMHLEN+7*LROWS(JNWORK),*999)
      IW(JFWORK+LMHCOL) = 7
C.. Compute mean and sigma for each of the different addresses
      IOLD = -1
      IFOI = 0
      DO 12 IROW = 1,LROWS(JNWORK)
        IAD = ITABL(JNWORK,IROW,1)
        IF (IAD.NE.IOLD) THEN
C   Compute means and sigmas of previous row if any
           IF ( IFOI.GT.0) THEN
C  Create this row in KFWORK bank
              KFWORK = KNEXT(JFWORK)
              IW(KFWORK+1) = IOLD
              DO 14 IS = 1,3
                SUMX(IS) = SUMX(IS)/NEV
                SUMX2(IS) = SUMX2(IS)/NEV
                SUMX2(IS) = SUMX2(IS)-SUMX(IS)*SUMX(IS)
                IF (SUMX2(IS).LE.0.) SUMX2(IS) = 0.
                SUMX2(IS) = SQRT(SUMX2(IS))
C  Fill means and sigmas for this row in KFWORK
                RW(KFWORK+1+IS) = SUMX(IS)
                RW(KFWORK+4+IS) = SUMX2(IS)
                SUMX(IS) =0.
                SUMX2(IS) = 0.
  14           CONTINUE
               IFOI = 0
               IW(JFWORK+LMHROW) = IW(JFWORK+LMHROW)+1
           ENDIF
        ENDIF
        DO 13 IS = 1,3
C  Update mean and sigma
          ENE =ITABL(JNWORK,IROW,IS+1)*1.E-03
          SUMX(IS) = SUMX(IS) + ENE
          SUMX2(IS) = SUMX2(IS) + ENE*ENE
  13    CONTINUE
        IFOI = IFOI + 1
        IOLD = IAD
 12   CONTINUE
C   Fill last row
      IF ( IFOI.GT.0) THEN
C  Create this row in KFWORK bank
         KFWORK = KNEXT(JFWORK)
         IW(KFWORK+1) = IOLD
         DO 15 IS = 1,3
           SUMX(IS) = SUMX(IS)/NEV
           SUMX2(IS) = SUMX2(IS)/NEV
           SUMX2(IS) = SUMX2(IS)-SUMX(IS)*SUMX(IS)
           IF (SUMX2(IS).LE.0.) SUMX2(IS) = 0.
           SUMX2(IS) = SQRT(SUMX2(IS))
C  Fill means and sigmas for this row in KFWORK
           RW(KFWORK+1+IS) = SUMX(IS)
           RW(KFWORK+4+IS) = SUMX2(IS)
           SUMX(IS) =0.
           SUMX2(IS) = 0.
  15     CONTINUE
         IFOI = 0
         IW(JFWORK+LMHROW) = IW(JFWORK+LMHROW)+1
      ENDIF
      LEN = LROWS(JFWORK)*LCOLS(JFWORK)+LMHLEN
      CALL WBANK(IW,JFWORK,LEN,*999)
  999 CALL WDROP (IW,JNWORK)
      RETURN
      END
#endif
#endif
