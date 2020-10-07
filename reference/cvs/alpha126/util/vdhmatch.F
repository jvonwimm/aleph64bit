CKEY MATCHING / USER
C-------------------------------------------------------------------------------
      SUBROUTINE VDHMATCH(IALTRK,IMCTRK,NTRUE,NWRONG,NMISS,NDOUBLE)
C
C! Count matching VDET hits for a given reconstructed/MCtrue track.
C  Author   Dave Brown, 18-7-94
C  Modified: Paul Rensing 30-1-96  to be put in ALPHA 122 :
C       Use new VDET geometry package
C
C  The association of recontructed with MC track must have already been
C  done (for example, by calling JULMATCH).
C
C  This routine will ONLY work for POT/DST files, IE NOT ON MINIS !
C
C  Input arguments :
C   IALTRK  =  ALPHA track number of RECONSTRUCTED track
C   IMCTRK  =  ALPHA track number of associated MONTE CARLO track
C
C  Output arguments :
C   NTRUE      =  # of true VDET hits on this track
C   NWRONG(2)  =  # of hits on reconstructed track which truely came from
C                    a track other than IMCTRK (split z/rphi)
C   NMISS(2)   =  # of true hits not used in reconstruction (split z/rphi)
C   NDOUBLE(2) =  # of hits on reconstructed track which are associated
C                   with >=2 MC true tracks (split z/rphi)
C-------------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C  IO variables
C
      INTEGER IALTRK, IMCTRK
      INTEGER NTRUE,NWRONG(2),NMISS(2),NDOUBLE(2)
C
C  Alpha commons :
#include "qdecl.h"
#include "qcde.h"
C  VDET  commons :
#include "vhmat.h"
C
C  Local variables
C
      INTEGER OLDEVE/0/
      INTEGER ITRK,IFKIN
      INTEGER INDEX,INDXY,IGAL,IUHIT,IWHIT
      INTEGER ILAY, IPHI, IWAF, INUM, IVIEW
C-------------------------------------------------------------------------------
C  See if this is a new event; if so, setup the arrays
C
      IF (KEVT.NE.OLDEVE) THEN
        OLDEVE = KEVT
        CALL VDHITMATCH
      END IF
C
C  Convert ALPHA track numbers
C
      ITRK = IALTRK-KFCHT+1
      IFKIN = IMCTRK-KFMCT+1
C
C  Initialize variables
C
      NTRUE = 0
      NWRONG(1) = 0
      NWRONG(2) = 0
      NMISS(1) = 0
      NMISS(2) = 0
      NDOUBLE(1) = 0
      NDOUBLE(2) = 0
      DO INDEX = 1, NVWMAX
C
C Determine the global wafer number of the corresponding XY wafer
C   (complex, but uses the functions completely!)
C
         CALL VFWAFI(INDEX, ILAY, IPHI, IWAF)
         CALL VAENWA(INUM,ILAY,IWAF,IPHI,2)
         CALL VRMWF(INUM, 2, INUM)
         CALL VADEWA(INUM,ILAY,IWAF,IPHI,IVIEW)
         CALL VJWFFW(ILAY, IPHI, IWAF, INDXY)
C
         DO IGAL = 1,NGAL(INDEX)
            IF (ITRK_G(IGAL,INDEX).EQ.IFKIN) THEN
               NTRUE = NTRUE + 1
               IUHIT = IUMATCH(IGAL,INDEX)
               IWHIT = IWMATCH(IGAL,INDEX)
               IF (IUHIT.EQ.0.OR.ITRK.NE.ITRK_U(IUHIT,INDXY)) THEN
                  NMISS(2) = NMISS(2) + 1
               ELSE IF (ITRK.EQ.ITRK_U(IUHIT,INDXY) .AND.
     &                 NUMATCH(IUHIT,INDXY) .GT. 1) THEN
                  NDOUBLE(2) = NDOUBLE(2) + 1
               END IF
               IF (IWHIT.EQ.0.OR.ITRK.NE.ITRK_W(IWHIT,INDEX)) THEN
                  NMISS(1) = NMISS(1) + 1
               ELSE IF (ITRK.EQ.ITRK_W(IWHIT,INDEX) .AND.
     &                 NWMATCH(IWHIT,INDEX) .GT. 1) THEN
                  NDOUBLE(1) = NDOUBLE(1) + 1
               END IF
            END IF
         END DO
         DO IUHIT = 1,NU(INDXY)
            IF (ITRK_U(IUHIT,INDXY).EQ.ITRK) THEN
               DO IGAL = 1,NGAL(INDEX)
                  IF (IUHIT.EQ.IUMATCH(IGAL,INDEX)) THEN
                     IF (ITRK_G(IGAL,INDEX).NE.IFKIN) THEN
                        NWRONG(2) = NWRONG(2) + 1
                     END IF
                  END IF
               END DO
            END IF
         END DO
         DO IWHIT = 1,NW(INDEX)
            IF (ITRK_W(IWHIT,INDEX).EQ.ITRK) THEN
               DO IGAL = 1,NGAL(INDEX)
                  IF (IWHIT.EQ.IWMATCH(IGAL,INDEX)) THEN
                     IF (ITRK_G(IGAL,INDEX).NE.IFKIN) THEN
                        NWRONG(1) = NWRONG(1) + 1
                     END IF
                  END IF
               END DO
            END IF
         END DO
      END DO
C
C  Correct for double hits
C
      NWRONG(1) = MAX(NWRONG(1) - NDOUBLE(1),0)
      NWRONG(2) = MAX(NWRONG(2) - NDOUBLE(2),0)
      RETURN
      END
#endif
