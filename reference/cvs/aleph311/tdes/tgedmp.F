      SUBROUTINE TGEDMP(IUNIT)
C -----------------------------------------------------------
C! Dump TPC geometry
C!
C! Author:   R. Johnson  7-09-86
C! Modified: R. Johnson  1-10-87
C! Modified: R. Richter  9-05-88
C!
C! Description
C! ===========
C!
C!      This routine dumps the commons TPGEOM,TPGEOP,TPGEOW
C!      and TPGEOT to unit IUNIT.
C!
C! Called by USER program after TRDDAF is called
C!
C!-----------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeow.h"
#include "tpgeop.h"
#include "tpgeot.h"
C
      CHARACTER   CSEC(3)*1, CTYP(3)*2, CREG(3)*5
      DIMENSION IEL (3), IELMX (3), TRW(3)
      DATA CSEC /'K','M','W'/
C
C++   Global geometry information: /TPGEOM/
C
      WRITE(IUNIT,1) (TPPROW(I),I=1,21)
    1 FORMAT(//' +++TGEDMP+++ Dump of TPC Geometry Commons ========'/
     &       /' Global geometry information:'/
     &       /' Radius of each padrow in global system:'/7(2X,F10.5))
      WRITE(IUNIT,2) TPTROW
    2 FORMAT(/' Radius of each trigger padrow in global system:'/
     &       7(2X,F10.5))
      WRITE(IUNIT,27) RTPCMN,RTPCMX,ZTPCMX,DRTPMN,DRTPMX,DZTPMX,
     &                TPFRDZ,TPFRDW,TPAVDZ,TPFOF1,TPFOF2,TPFOF3
   27 FORMAT(/' Minimum radius of active volume',T50,F12.5,' cm'/
     &        ' Maximum radius of active volume',T50,F12.5,' cm'/
     &        ' Half length of active volume',T50,F12.5,' cm'/
     &        ' Inner wall thickness',T50,F12.5,' cm'/
     &        ' Outer wall thickness',T50,F12.5,' cm'/
     &        ' Endplate total thickness',T50,F12.5,' cm'/
     &        ' Thickness of Al support frame along Z',
     &          T50,F12.5,' cm'/
     &        ' Width of Al sector support frame in Rphi dir.',
     &          T50,F12.5,' cm'/
     &        ' Equivalent thickness of Al support frame along Z',
     &          T50,F12.5,' cm'/
     &        ' Endplate frame zigzag offsets',T50,3F12.5,' cm')
      WRITE(IUNIT,21)
   21 FORMAT(/' Global sector geometry:'
     &       /'   Sector type, type number, and phi position',
     &        ' for each slot.')
      WRITE(IUNIT,22) (J,J=1,12),(ITPTYP(J),J=1,12),
     &                (ITPSEC(J),J=1,12),(TPPHI0(J),J=1,12)
      WRITE(IUNIT,22) (J,J=13,24),(ITPTYP(J),J=13,24),
     &                (ITPSEC(J),J=13,24),(TPPHI0(J),J=13,24)
      WRITE(IUNIT,22) (J,J=25,36),(ITPTYP(J),J=25,36),
     &                (ITPSEC(J),J=25,36),(TPPHI0(J),J=25,36)
   22 FORMAT(/'   Slot  ',12(1X,I7)/'   ITPTYP',12(1X,I7)/
     &       '   ITPSEC',12(1X,I7)/'   TPPHI0',12(1X,F7.4))
      DO 50 I=1,LTSTYP
         WRITE(IUNIT,23) I,CSEC(I),(J,J=1,NTPCRN(I))
         WRITE(IUNIT,24) (TPCORN(1,J,I),J=1,NTPCRN(I))
         WRITE(IUNIT,25) (TPCORN(2,J,I),J=1,NTPCRN(I))
   23    FORMAT('   Corner positions for sector type ',I1,' (',A1,'):',
     &          /7X,12(1X,I8))
   24    FORMAT(4X,' X=',12(1X,F8.4))
   25    FORMAT(4X,' Y=',12(1X,F8.4))
   50 CONTINUE
C
C++   Pad information:  /TPGEOP/
C
      WRITE(IUNIT,4)
    4 FORMAT(/' Pad information:')
      DO 100 I=1,LTSTYP
        IF (NTPDRW(I).EQ.0) GO TO 100
        WRITE(IUNIT,3) I,CSEC(I),NTPDRW(I),TPDRBG(I),TPDRST(I),
     &                 TPDHGT(I),TPDSEP(I),TPDWID(I),TPDHWD(I)
        WRITE(IUNIT,5)  (J,J=1,NTPDRW(I))
        WRITE(IUNIT,26) (NTPDPR(J,I),J=1,NTPDRW(I))
        WRITE(IUNIT,31) (J,J=1,NTPDRW(I))
        WRITE(IUNIT,32) (TPDPHF(J,I),J=1,NTPDRW(I))
        WRITE(IUNIT,33) (TPDPHW(J,I),J=1,NTPDRW(I))
        WRITE(IUNIT,34) (TPDPHS(J,I),J=1,NTPDRW(I))
  100 CONTINUE
   31 FORMAT('   Half angle subtended by frame at each padrow,'/
     &       '   phi at leading edge of first pad of row,'/
     &       '   and phi width of single pad or row:'/
     &       5X,'row       ',12(1X,I8))
   32 FORMAT(5X,'Half angle',12(1X,F8.5))
   33 FORMAT(5X,'Phi lead  ',12(1X,F8.5))
   34 FORMAT(5X,'Phi pad   ',12(1X,F8.5))
    3 FORMAT(/' Sector type ',I1,T50,5X,A1/
     &        '   Number of padrows=',T50,I12/
     &        '   Radius of first padrow=',T50,F12.5,' cm'/
     &        '   Radial step for padrows=',T50,F12.5,' cm'/
     &        '   Pad height=',T50,F12.5,' cm'/
     &        '   Pad center-center spacing in R*Phi=',T50,F12.5,' cm'/
     &        '   Width of one pad in R*Phi=',T50,F12.5,' cm'/
     &        '   Width of half pad in R*Phi=',T50,F12.5,' cm')
    5 FORMAT('   Number of pads in each row:'/5X,'     row:',12(2X,I3))
   26 FORMAT(5X,'#   pads:',12(2X,I3))
C
C++   Trigger pad information: /TPGEOT/
C
      WRITE(IUNIT,'(/'' Trigger pad information:'')')
      DO 150 I = 1,LTSTYP
         IF (NTPTRW(I).EQ.0) GO TO 150
         WRITE(IUNIT,40) I,CSEC(I),NTPTRW(I),TPTRBG(I),
     &                                       TPTRST(I),TPTRHG(I)
         WRITE(IUNIT,5)  (J,J=1,NTPTRW(I))
         WRITE(IUNIT,41) (NTPTPR(J,I),J=1,NTPTRW(I))
         WRITE(IUNIT,'(/''      Row   ID/Phipos/Half phi width'',/)')
         DO 152 IR = 1,NTPTRW(I)
            NTR = NTPTPR(IR,I)
            WRITE(IUNIT,42) IR, (ITPADG(IP,IR,I),IP=1,NTR)
            WRITE(IUNIT,43)     (TPTPHC(IP,IR,I),IP=1,NTR)
            WRITE(IUNIT,43)     (TPTPHW(IP,IR,I),IP=1,NTR)
  152    CONTINUE
  150 CONTINUE
   40 FORMAT(/' Sector type ',I1,T50,3X,A1/
     &        '   Number of Trigger padrows=',T50,I12/
     &        '   Radius of first padrow=',T50,F12.5,' cm'/
     &        '   Radial step for padrows=',T50,F12.5,' cm'/
     &        '   Pad height=',T50,F12.5,' cm')
   41 FORMAT(5X,'# Trpads:',11(2X,I3))
   42 FORMAT(10X,I5,4I10)
   43 FORMAT(15X,4F10.5)
C
C++   Wire information: /TPGEOW/
C
      WRITE (IUNIT,200)
  200 FORMAT ('1',/' Wire Information:'/1X,17('-'))
C
      WRITE (IUNIT,205) (CSEC(K),K=1,3),   (TWSTEP(K),K=1,3),
     &                  (TWIRE1(K),K=1,3), (NTWIRE(K),K=1,3),
     &                  (ITLWIF(K),K=1,3), (ITLWIL(K),K=1,3),
     &                  (J,(NTREG1(J,K),K=1,3),J=1,4)
C
  205   FORMAT(/'   Sector type ',             T50,3(9X,A1,2X)//
     &          '   Spacing of sense wires:',  T50,3F12.3/
     &          '   First wire position in R:',T50,3F12.3/
     &          '   Number of sense wires:',   T50,3I12/
     &          '   Wire on first el. chan.:', T50,3I12/
     &          '   Wire on last  el. chan.:', T50,3I12//
     &        4('   First wire in REGION#',I3, T50,3I12/)/)

C
         WRITE (IUNIT,210)
  210 FORMAT (/'  Detailed map of wire half-lengths:',
     +  3X,'(for all wires of sectors W + M: MINIMUM = 0., thus',
     +     ' LENGTH/2 = MAXIMUM)'/2X,34('-')//
     + T22,'S E C T O R    K ', T80,'S E C T O R   M / W'/
     + T2,  58('-'),            T66, 50('-')/)
C
         WRITE (IUNIT,220)
  220 FORMAT (/' Wire#   height   el.ch.#   Minimum',
     +         5X,'Maximum',5X,'Length/2',
     +    T65, ' Wire#   height   el.ch.#   Length/2', 7X,'Length/2'//
     +    T93, 'Sector: M',6X,'Sector: W'//)
C
         DO 400 J = 1, NTWIRE(3)
         DO 350 I = 1, LTSTYP
C        which region ?
         DO 310 K = 1,4
            CREG (I) = '     '
            IF (J .EQ. NTREG1 (K,I)) THEN
               WRITE (CREG (I),'(A4,I1)') '*RE ', K
               GO TO 311
            ENDIF
  310    CONTINUE
  311    IELMX (I) = ITLWIL (I) - ITLWIF (I) + 1
         IEL (I)   = MAX0 ((J - ITLWIF (I) + 1), 1)
         IEL (I)   = MIN0 (IEL(I), IELMX(I))
C     end of group of 16 channels ?
         IF ((IEL(I) .GT. 1) .AND. (IEL(I) .EQ. ((IEL(I)/16)*16))) THEN
            CTYP (I) = '__'
         ELSE
            CTYP (I) = '  '
         ENDIF
         TRW (I) = TWIRE1 (I) + FLOAT (J-1) * TWSTEP (I)
  350    CONTINUE
         IF (J .LE. NTWIRE(1)) THEN
            WRITE (IUNIT,230) J, TRW(1), CTYP(1), IEL(1), CTYP(1),
     *      TWIRMN(J,1), TWIRMX(J,1), TWIRLE(J,1), CREG(1),J,TRW(2),
     *      CTYP(2),IEL(2),CTYP(2),(TWIRLE(J,K), CREG(K),K=2,3)
          ELSE
             WRITE (IUNIT,240) J,TRW(2),
     *       CTYP(2),IEL(2),CTYP(2),(TWIRLE(J,K), CREG(K),K=2,3)
         ENDIF
  400 CONTINUE
C
      WRITE(IUNIT,9)
    9 FORMAT(/'******* End of dump of TPC Geometry ***********'//)
  230 FORMAT      (2X, I3, F9.2, 3X, A2, I3, A2, F11.3, 2F12.3, A5,
     *        T65, 2X, I3, F9.2, 3X, A2, I3, A2, F11.3, A5, F10.3, A5)
  240 FORMAT (T65, 2X, I3, F9.2, 3X, A2, I3, A2, F11.3, A5, F10.3, A5)
      RETURN
      END

#endif
