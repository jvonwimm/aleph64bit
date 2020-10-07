      SUBROUTINE ECGATI(NRUN)
C ---------------------------------------------------------------------
C - Author: M.N Minard          940121
C!    Fix Gampek photon
C     Called from FIXGAEN
C-    For all processing JULIA before ALEPHLIB 156
C-    find Gain correction either from ECMC or Bank file
C ---------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "qcnami.h"
#include "rhahjj.h"
      SAVE NAECMC,NAEGAC,IFIRST,IPRRI
      COMMON / ECCMF / ECCM(36) ,NRMIN,NRMAX
      DATA IROLD / -99/
      DATA NMCMX / 2000 /
      DATA IFIRST,IPRRI  / 0 , 0 /
      DATA ILVGCH / 157 /
      EXTERNAL CHAINT
      CHARACTER*4 CHAINT
      CHARACTER*4 NAMJUL
      CHARACTER*8 PRNAM
#include "bmacro.h"
C ---------------------------------------------------------------------
C
C-    Initialise pointers
C
      IF(IFIRST.EQ.0 ) THEN
        NAECMC = NAMIND('ECMC')
        NARHAH = NAMIND('RHAH')
        NAEGAC = NAMIND('EGAC')
        IFIRST = 1
      ENDIF
C
C-    Check if ALEPHLIB version has the correction
C

      IACOR = 0
      KRHAH = IW(NARHAH)
        IMVER = MINGTV(DUM)
        NRHAH=LROWS(KRHAH)
        DO 10 IRHAH = 1, NRHAH
          PRNAM(1:4) = CHAINT(ITABL(KRHAH,IRHAH,JRHAPN))
          PRNAM(5:8) = CHAINT(ITABL(KRHAH,IRHAH,JRHAPN+1))
          IF (PRNAM.EQ.'JULIA') THEN
            IJVER = ITABL(KRHAH,IRHAH,JRHAPV)
            ILVER = ITABL(KRHAH,IRHAH,JRHAAV)
          ENDIF
   10   CONTINUE
C
C-    Look if new run
C
      IMC = 0
      IF(NRUN.LT.NMCMX) IMC = 1
C
C-    Look for ECMC bank
C
      IF (IMC.EQ.1) THEN
        DO IC = 1, 36
        ECCM(IC) = 1.
        ENDDO
        NRMIN = 1
        NRMAX = 2000
      ELSE
        IF (ILVER.GT.ILVGCH ) THEN
        DO IC = 1, 36
        ECCM(IC) = 1.
        ENDDO
        IROLD = NRUN

        NRMIN = NRUN
        NRMAX = NRUN
        ELSE
        IF ( NRUN.NE.IROLD ) THEN
C
C-    ECMC bank exist
C
          KECMC = IW(NAECMC)
          NECMC = 0
          IF ( KECMC.GT.0) THEN
            NECMC = LROWS(KECMC)
            DO IECMC = 1,NECMC
              ECCM(IECMC) = RTABL(KECMC,IECMC,1)
            ENDDO
            CALL CALYEAR(NRUN)
            IROLD = NRUN
            NRMIN = NRUN
            NRMAX = NRUN
          ELSE
C
C-     Look for EGAC bank
C
          IF(NRUN.LT.NRMIN.OR.NRUN.GT.NRMAX) THEN
            KEGAC = IW(NAEGAC)
            NEGAC = 0
            IF ( KEGAC.EQ.0) THEN
              DO IC=1,36
                ECCM (IC) = 1.
              ENDDO
              IROLD=NRUN
              NRMIN=NRUN
              NRMAX=NRUN
              IPRRI=IPRRI+1
              IF (IPRRI.LE.10) WRITE (IW(6),'(///,5(/,10X,A),///)')
     &'+------------------------------------------------------------+',
     &'|  Data Processed no gain correction for photon available    |',
     &'|                                                            |',
     &'+------------------------------------------------------------+'
             CALL CALYEAR(NRUN)
            ELSE
C
C-   Search for new correction factors
C
             NEGAC = LROWS(KEGAC)
             IRMIN = 99999
             IRMAX = 0
             DO IEGAC = 1,NEGAC-1
               IF ( NRUN.GE.ITABL(KEGAC,IEGAC,1).AND.
     &              NRUN.LT.ITABL(KEGAC,IEGAC+1,1)) THEN
               IRMAX = IEGAC
               DO IL =1,36
               ECCM (IL) = RTABL(KEGAC,IEGAC,IL+2)
               NRMIN = ITABL(KEGAC,IEGAC,1)
               NRMAX = ITABL(KEGAC,IEGAC+1,1) -1
               ENDDO
               IROLD = NRUN
               CALL CALYEAR(NRUN)
               ENDIF
             ENDDO
             IF ( IRMAX.EQ.0) THEN
               IPRRI=IPRRI+1
               IF (IPRRI.LE.10) WRITE (IW(6),'(///,5(/,10X,A),///)')
     &'+------------------------------------------------------------+',
     &'|  Data Processed no gain correction for photon available    |',
     &'|                                                            |',
     &'+------------------------------------------------------------+'
              DO IC=1,36
              ECCM (IC) = 1.
              ENDDO
             ENDIF
           ENDIF
         ENDIF
       ENDIF
      ENDIF
      ENDIF
      ENDIF
      RETURN
      END
#endif
