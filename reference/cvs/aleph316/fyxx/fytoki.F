      SUBROUTINE FYTOKI
C-------------------------------------------------------
CKEY FYXX  / INTERNAL
C - J. Hilgart 14/06/88
C                         modified by - F.Ranjard - 881107
C                                       B.Bloch   - 901010,980316
C! Convert from FVER,FKIN,FPOL,FZFR,FSHO 
C!           to KINE,VERT,KHIS,KVOL,KPOL,KZFR,KSHO
C  drop banks FKIN, FVER ,FPOL,FZFR,FSHO
C  add KINE, VERT, KVOL, KHIS ,KPOL,KZFR,KSHO to the E-list
C - Calls: AUBOS                        from ALEPHLIB.HLB
C
#ifndef DOC
      SAVE
#include "bcs.h"
#include "fxjjpar.h"
#include "kgjjpar.h"
      PARAMETER (LHVER=3,LPVER=5,LHKIN=3,LPKIN=5)
      DATA IONC /0/
#include "bmacro.h"
C ============================================================
      IF (IONC.EQ.0) THEN
         NFKIN = NAMIND('FKIN')
         NFVER = NAMIND('FVER')
         NFZFR = NAMIND('FZFR')
         NFPOL = NAMIND('FPOL')
         NFSHO = NAMIND('FSHO')
         NKEVH = NAMIND('KEVH')
         IONC = 1
         CALL BKFMT ('VERT','3I,4F,(I)')
         CALL BKFMT ('KINE','3I,4F,(I)')
         CALL BKFMT ('KVOL','2I,(A)')
         CALL BKFMT ('KHIS','I')
         CALL BKFMT ('KPOL','2I,(I,3F)')
         CALL BKFMT ('KZFR','2I,(F)')
         CALL BKFMT ('KSHO','I')
      ENDIF
      JFKIN = IW(NFKIN)
      JFVER = IW(NFVER)
      JKEVH = IW(NKEVH)
      IF (JFKIN.EQ.0.OR.JFVER.EQ.0) GO TO 999
C Store vertices

      DO 10 IV = 1, LROWS(JFVER)
         KFVER = KROW(JFVER,IV)
         IOF = IW(KFVER + JFVEIS)
         NOUT = IW(KFVER + JFVENS)
         CALL AUBOS('VERT',IV,LHVER+LPVER+NOUT,JVERT,IGARB)
         IF (JVERT.EQ.0) GO TO 999
         IF (IGARB.NE.0) THEN
            JFVER = IW(NFVER)
            KFVER = KROW(JFVER,IV)
            JFKIN = IW(NFKIN)
         ENDIF
         IW(JVERT + 1) = LHVER
         IW(JVERT + 2) = LPVER
         IW(JVERT + 3) = NOUT
         CALL UCOPY (RW(KFVER+1),RW(JVERT+LHVER+1),4)
         IW(JVERT+LHVER+5) = IW(KFVER+JFVEIP)
         DO 12 IT = 1,NOUT
            IW(JVERT+LHVER+LPVER+IT) = IOF + IT
   12    CONTINUE
   10 CONTINUE
C
C Store vertex volume names
      LE = LROWS(JFVER)*LKVOLA + LMHLEN
      CALL AUBOS('KVOL',0,LE,JKVOL,IGARB)
      IF (JKVOL.EQ.0) GO TO 999
      IF (IGARB.NE.0)  JFVER = IW(NFVER)
      IW(JKVOL+LMHCOL) = LKVOLA
      IW(JKVOL+LMHROW) = LROWS(JFVER)
      KKVOL = JKVOL + LMHLEN
      DO 20 IV = 1, LROWS(JFVER)
         IW(KKVOL+JKVOVN) = ITABL(JFVER,IV,JFVEVN)
         IF (LCOLS(JFVER).GE.JFVEVM) THEN
            IW(KKVOL+JKVOVM) = ITABL(JFVER,IV,JFVEVM)
         ELSE
            IW(KKVOL+JKVOVM) = INTCHA ('    ')
         ENDIF
 20   KKVOL = KKVOL + LCOLS(JKVOL)
C
C Store particles
      IPAR = LROWS(JFKIN)
      DO 30   IP = 1, IPAR
         KFKIN = KROW(JFKIN,IP)
         IPART = IW(KFKIN+JFKIPA)
         IVOUT = IW(KFKIN+JFKIEV)
         IVOR = IW(KFKIN+JFKIOV)
         NVX = 1
         IF (IVOUT .GT. 0) NVX = 2
         CALL AUBOS('KINE',IP,LHKIN+LPKIN+NVX,JKINE,IGARB)
         IF(JKINE.EQ.0) GO TO 999
         IF (IGARB.NE.0) THEN
            JFKIN = IW(NFKIN)
            KFKIN = KROW(JFKIN,IP)
         ENDIF
         IW(JKINE + 1) = LHKIN
         IW(JKINE + 2) = LPKIN
         IW(JKINE + 3) = NVX
         CALL UCOPY(RW(KFKIN+1),RW(JKINE+LHKIN+1),4)
         IW(JKINE+LHKIN+5) = IPART
         IW(JKINE+LHKIN+LPKIN+1) = IVOR
         IF (NVX.EQ.2) IW(JKINE+LHKIN+LPKIN+2) = IVOUT
   30 CONTINUE
C
C Store history information for the tracks produced by ev. gen.
      IF (JKEVH .GT. 0) THEN
         NITRK = ITABL(JKEVH,1,JKEVNT)
      ELSE
         NITRK = LROWS(JFKIN)
      ENDIF
      LE = NITRK + LMHLEN
      CALL AUBOS('KHIS',0,LE,JKHIS,IGARB)
      IF (JKHIS.EQ.0) GO TO 999
      IF (IGARB.NE.0) JFKIN = IW(NFKIN)
      IW(JKHIS+LMHCOL) = LKHISA
      IW(JKHIS+LMHROW) = NITRK
      KKHIS = JKHIS + LMHLEN
      DO 40   IP = 1, NITRK
         IW(KKHIS+1) = ITABL(JFKIN,IP,JFKIHC)
 40   KKHIS = KKHIS + LCOLS(JKHIS)
C
C Store fragmentation information for the tracks produced by ev. gen.
C and parton shower + polarisation as well
      CALL BSWAP(IW,'FZFR','KZFR')
      CALL BSWAP(IW,'FPOL','KPOL')
      CALL BSWAP(IW,'FSHO','KSHO')
C
C - Add new banks to the E-list
C
      CALL BLIST (IW,'E+','KINEVERTKVOLKHISKPOLKZFRKSHO')
C
C - Drop old banks
C
      CALL BDROP (IW,'FKINFVERFPOLFZFRFSHO')
      CALL BLIST (IW,'E-','FKINFVERFPOLFZFRFSHO')
C
  999 RETURN
      END
#endif
