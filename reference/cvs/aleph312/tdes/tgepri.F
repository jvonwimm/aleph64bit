      SUBROUTINE TGEPRI (LOUT)
C -----------------------------------------------------------------
C - M.Mermikides, F.Ranjard - 880420
C! Short printout of the TPC geometry on unit# LOUT
C - called by USER program after a call to TRDDAF
#ifndef DOC
      SAVE
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeow.h"
#include "tpgeop.h"
#include "tpgeot.h"
C -----------------------------------------------------------------
C
C  Debug
C
         WRITE(LOUT,901) RTPCMN,RTPCMX,ZTPCMX,DRTPMN,DRTPMX,
     *                     DZTPMX,TPFRDZ,TPFRDW,TPAVDZ,TPFOF1,
     *                     TPFOF2,TPFOF3, TPPROW
 901     FORMAT(' +++TGEPRI+++ TPC Geometry ',//
     *        ' RMIN,RMAX,ZMAX,DRMIN,DRMAX,DZ ',6F10.4,/,
     *        ' Sector support frame dimens. ',6F10.4,//,
     *        ' PADROW RADII ',17X,5F10.4/(31X,5F10.4))
         WRITE(LOUT,914) TPTROW
 914     FORMAT(/, ' TRIGGER PADROW RADII ',9X,5F10.4/(31X,5F10.4))
         WRITE(LOUT,920) ITPTYP,ITPSEC
 920     FORMAT(//' Sector geometry',//,' ITPTYP ',18I3/8X,18I3//,
     *             ' ITPSEC ',18I3/8X,18I3)
         WRITE(LOUT,902) TPPHI0
 902     FORMAT (' Slot positions PHI0',6F10.4/(20X,6F10.4))
C
         DO 201 IT = 1,LTSTYP
            WRITE(LOUT, 903) IT, (TPDPHF(M,IT),M=1,NTPDRW(IT))
 903        FORMAT(//,' SECTOR TYPE',I3,' PHIF',6F10.4/20X,6F10.4)
            WRITE (LOUT,913) (TPDPHW(M,IT),M=1,NTPDRW(IT))
 913        FORMAT(7X,'PAD PHI START',6F10.5/20X,6F10.5)
            WRITE(LOUT,905) (TPCORN(M,1,IT),M=1,2*NTPCRN(IT))
 905        FORMAT(16X,'CORN',6F10.4/20X,6F10.4)
            WRITE(LOUT,911) TPDRBG(IT),TPDSEP(IT),TPDWID(IT),
     &                        TPDHWD(IT),TPDHGT(IT),TPDRST(IT)
 911        FORMAT(/, ' PADS ',/ 7X,'R1, PITCH, WID , HALFW , H, DR ',
     *          1X,6F8.4/)
            WRITE(LOUT,906) TPTRBG(IT),TPTRST(IT),TPTRHG(IT),
     *                        (NTPTPR(M,IT),M=1,NTPTRW(IT))
  906        FORMAT(/,' TRIGGER PADS  ',/ 7X,'R1, DR, H/2 =', 3F10.4,
     *            /,12X,'PADS/ROW ',11I4)
            WRITE (LOUT,907)
  907       FORMAT(/' ROW,  #,  Phipos / Phiwid '/)
            DO 211 IR = 1,NTPTRW(IT)
               WRITE (LOUT,908) IR, (ITPADG(IP,IR,IT),IP=1,4),
     &                  (TPTPHC(IP,IR,IT), IP=1,NTPTPR(IR,IT))
               WRITE (LOUT,909) (TPTPHW(IP,IR,IT),IP=1,NTPTPR(IR,IT))
  908          FORMAT (/,2X, I3,')', 4I4, 4F10.5)
  909          FORMAT (22X, 4F10.5)
  211       CONTINUE
            WRITE(LOUT,912) NTWIRE(IT),TWSTEP(IT),TWIRE1(IT)
  912       FORMAT(/,' SENSE WIRES (No, Step, R0 ',I8,2F10.4)
  201    CONTINUE
C
      END
#endif
