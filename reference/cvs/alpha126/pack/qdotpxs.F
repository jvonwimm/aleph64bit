      SUBROUTINE QDOTPXS
C----------------------------------------------------------------------
C! Create TPXS bank of truncated means for pad dE/dx
C!
C!         Author: F. Palla 14-Nov-1998
C!         Called from QFPTPX
C?
C!=====================================================================
#ifndef DOC
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "qtpadcl.h"
#include "tpdxjj.h"
#include "tpxsjj.h"
#include "tplsjj.h"
#include "bcs.h"
C
C++ Local variables
C
      REAL RAWDDX(21)
      INTEGER INDEDX(21)
      CHARACTER*115 TEXT
      DIMENSION XCLST(LTSECT),SZLST(LTSECT),SPLST(LTSECT),
     &     SLLST(LTSECT),NCLST(LTSECT)
      LOGICAL FIRST/.TRUE./

      SAVE NTPXS,NTPLS,NTPDX,NFRFT
C
#include "bmacro.h"
C
C-----------------------------------------------------------------------
C
      IF(FIRST)THEN
        NTPXS = NAMIND('TPXS')
        NTPLS = NAMIND('TPLS')
        NTPDX = NAMIND('TPDX')
        NFRFT = NAMIND('FRFT')
        FIRST = .FALSE.
      ENDIF
C
C++ Drop the output banks they still exist
C
      IF(IW(NTPXS).NE.0) CALL BDROP(IW,'TPXS')
      IF(IW(NTPLS).NE.0) CALL BDROP(IW,'TPLS')
C
C++ Check existence of needed input banks
C
      KFRFT = IW(NFRFT)
      IF(IW(NAMIND('FR12')).NE.0) THEN
        KFRFT = NLINK('FRFT',3)
      ELSEIF(IW(NAMIND('FR10')).NE.0) THEN
        KFRFT = NLINK('FRFT',3)
      ELSEIF(IW(NAMIND('FRF0')).EQ.0) THEN
        KFRFT = NLINK('FRFT',3)
      ENDIF

      IF (KFRFT.EQ.0)GO TO 999
      KTPDX = IW(NTPDX)
      IF (KTPDX.EQ.0)GO TO 999
C
C++ Create workbank for sorting dE/dx samples
C
      NPTOT = NTPDRW(1) + NTPDRW(2)
C
C++ Create named banks for output
C
      LEN = LROWS(KFRFT) * LTPLSA + LMHLEN
      IW(1) = 1
      CALL AUBOS('TPLS',0,LEN,KTPLS,IGARB)
      IF (IGARB.EQ.2) GO TO 997
      CALL BLIST(IW,'E+','TPLS')
      IW(KTPLS+LMHCOL) = LTPLSA
      IW(KTPLS+LMHROW) = 0

      KFRFT = IW(NFRFT)
      IF(IW(NAMIND('FR12')).NE.0) THEN
        KFRFT = NLINK('FRFT',3)
      ELSEIF(IW(NAMIND('FR10')).NE.0) THEN
        KFRFT = NLINK('FRFT',3)
      ELSEIF(IW(NAMIND('FRF0')).EQ.0) THEN
        KFRFT = NLINK('FRFT',3)
      ENDIF

      IF (KFRFT.EQ.0)GO TO 999

      MXTPX = 3 * LROWS(KFRFT)
      LEN = MXTPX * LTPXSA + LMHLEN
      IW(1) = 1
      CALL AUBOS('TPXS',0,LEN,KTPXS,IGARB)
      IF(IGARB.EQ.2) GO TO 997
      CALL BLIST(IW,'E+','TPXS')
      IW(KTPXS+LMHCOL) = LTPXSA
      IW(KTPXS+LMHROW) = 0
C
      KTPLS = IW(NTPLS)
      KFRFT = IW(NFRFT)
      IF(IW(NAMIND('FR12')).NE.0) THEN
        KFRFT = NLINK('FRFT',3)
      ELSEIF(IW(NAMIND('FR10')).NE.0) THEN
        KFRFT = NLINK('FRFT',3)
      ELSEIF(IW(NAMIND('FRF0')).EQ.0) THEN
        KFRFT = NLINK('FRFT',3)
      ENDIF

      KTPDX = IW(NTPDX)

C
C++  Loop over all tracks
C
      DO 500 WHILE(KTPDX.NE.0)
        ITK = IW(KTPDX-2)
C
C++  Cut on number of pad samples
C
        NSMP = LROWS(KTPDX)
        IF (NSMP .LT. MNSPTP) GO TO 499
        IF (NSMP .GT. NTPDRW(1)+NTPDRW(2)) THEN
          WRITE(6,360) NSMP
 360      FORMAT('Too many (',I3,') pad samples on a track',
     &      'The excess data are being lost.  ',
     &      'This indicates a problem in TPADDX.')
          NSMP = NTPDRW(1) + NTPDRW(2)
        ENDIF
C
        IW(KTPLS+LMHROW) = LROWS(KTPLS) + 1
        MTPLS = LROWS(KTPLS)
C
C++  Loop through coordinates and gather up subcluster charges
C
        DO 400 ISMP = 1, NSMP
          CHRGS = RTABL(KTPDX,ISMP,JTPDTC)
          SMP = RTABL(KTPDX,ISMP,JTPDSL)
          IDD = ITABL(KTPDX,ISMP,JTPDIN)
          IROWG = IDD/100000
          ISLOT = IDD/1000 - IROWG*100
          QCNL = CHRGS / SMP / (1.+0.15*LOG10(SMP/3.))          
          RAWDDX(ISMP) = QCNL
  400   CONTINUE
C
C++  Sort the dE/dx values
C
        CALL SORTZV(RAWDDX,INDEDX,NSMP,1,0,0)
C
C++  Calculate the truncated sum of dE/dx and the average drift separately
C++  for each sector.
C
        CALL VZERO(NCLST(1),LTSECT)
        CALL VZERO(XCLST(1),LTSECT)
        CALL VZERO(SZLST(1),LTSECT)
        CALL VZERO(SPLST(1),LTSECT)
        CALL VZERO(SLLST(1),LTSECT)
        NHTOT = NSMP * JPTRCL / 100
        XHTOT = NSMP * FLOAT(JPTRCL)/100. - NHTOT
        IF (XHTOT.GT.0.) NHTOT = NHTOT + 1
        DO 450 K = 1, NHTOT
          IPD = INDEDX(K)
          IDD = ITABL(KTPDX, IPD, JTPDIN)
          IROWG = IDD/100000
          ISLOT = IDD/1000 - IROWG*100
          CHRGS = RAWDDX(IPD)
          IF (K.LT.NHTOT .OR. XHTOT.EQ.0.) THEN
            WT = 1.
          ELSE
            WT = XHTOT
          ENDIF
          NCLST(ISLOT) = NCLST(ISLOT) + 1
          XCLST(ISLOT) = XCLST(ISLOT) + WT
          SLLST(ISLOT) = SLLST(ISLOT) + WT*RTABL(KTPDX,IPD,JTPDSL)
          SPLST(ISLOT) = SPLST(ISLOT) + WT*CHRGS
          ZPUL = RTABL(KTPDX,IPD,JTPDRZ)
          SZLST(ISLOT) = SZLST(ISLOT) + WT*ZPUL
  450   CONTINUE
C
C++  Fill an entry into TPXS for each sector with hits on this track
C
        IOFXS = LROWS(KTPXS)
        NSECT = 0
        DO 470 ISLOT = 1, LTSECT
          IF (XCLST(ISLOT).EQ.0.) GO TO 470
C
C++  Extend the TPXS bank in the unlikely event that it
C++  runs out of space.
C
          IF (LROWS(KTPXS).EQ.MXTPX) THEN
            LEN = IW(KTPXS) + MXTPX*LTPXSA
            MXTPX = 2 * MXTPX
            CALL AUBOS('TPXS',0,LEN,KTPXS,IGARB)
            IF (IGARB.EQ.2) GO TO 997
            IF (IGARB.NE.0) THEN
              KTPLS = IW(NTPLS)
              KTPDX = IW(NTPDX)
              KFRFT = IW(NFRFT)
              IF(IW(NAMIND('FR12')).NE.0) THEN
                KFRFT = NLINK('FRFT',3)
              ELSEIF(IW(NAMIND('FR10')).NE.0) THEN
                KFRFT = NLINK('FRFT',3)
              ELSEIF(IW(NAMIND('FRF0')).EQ.0) THEN
                KFRFT = NLINK('FRFT',3)
              ENDIF              
            ENDIF
          ENDIF
          NSECT = NSECT + 1
          IW(KTPXS+LMHROW) = LROWS(KTPXS) + 1
          MTPXS = LROWS(KTPXS)
C
          IW(KROW(KTPXS,MTPXS)+JTPXSI) = ISLOT
          RW(KROW(KTPXS,MTPXS)+JTPXTM) = SPLST(ISLOT)/XCLST(ISLOT)
          RW(KROW(KTPXS,MTPXS)+JTPXTL) = SLLST(ISLOT)
          RW(KROW(KTPXS,MTPXS)+JTPXNS) = XCLST(ISLOT)
          RW(KROW(KTPXS,MTPXS)+JTPXAD) = SZLST(ISLOT)/XCLST(ISLOT)
          IW(KROW(KTPXS,MTPXS)+JTPXTN) = ITK
  470   CONTINUE
C
C++ Fill the pointer bank TPLS
C
        IW(KROW(KTPLS,MTPLS)+JTPLNG) = NSECT
        IW(KROW(KTPLS,MTPLS)+JTPLSO) = IOFXS
        IW(KROW(KTPLS,MTPLS)+JTPLTN) = ITK
  499   CONTINUE
        KTPDX = IW(KTPDX-1)
  500 CONTINUE
C
C++  Set final sizes of the output banks
C      
      CALL AUBPRS('TPXS')
      CALL AUBPRS('TPLS')
      GO TO 999
C
  997 CONTINUE
      IER = 2
      WRITE(6,*) 'No room in BOS for named banks'
      GO TO 999
  998 CONTINUE
      IER = 3
      WRITE(6,*) 'No room in BOS for work banks'
  999 CONTINUE
      RETURN
      END
#endif
