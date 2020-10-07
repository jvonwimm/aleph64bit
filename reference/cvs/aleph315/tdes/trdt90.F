      SUBROUTINE TRDT90
C --------------------------------------------------------------
C - M.Mermikides, R.Johnson, F.Ranjard  - 880420
C! Initialize TPC90 geometry
C - called by USER program
#ifndef DOC
      SAVE
#include "alcons.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeow.h"
#include "tpgeop.h"
#include "tpgeot.h"
C
C     The following data is for TPC90-II only:
C     ----------------------------------------
C
      INTEGER NPAD(7)
      DATA NWIR/121/
      DATA NPAD/59,69,79,89,99,87,97/
      DATA XDPDR/6.4/,YDPAD/.670207/,XPHGT/3.0/
      DATA XDWIR/.4/,XPRO1/39.871/,XWIR1/32.2/,PADWD/0.59021/
      DATA TG30/.5773503/,TG60/1.732051/
C
C--------------------------------- END TYPES ---------------------------
C
        NTPROW=7
        ISTYP=1
        NTSECT=1
        ITPTYP(NTSECT)=ISTYP
        ITPSEC(NTSECT)=1
        IENDTP(NTSECT)=1
        ZTPCMX=120.
C
        NTPDRW(ISTYP)=7
        TPDRBG(ISTYP)=XPRO1
        TPDHGT(ISTYP)=XPHGT
C
        DO 1 IROW=1,NTPDRW(ISTYP)
          NTPDPR(IROW,ISTYP)=NPAD(IROW)
          TPPROW(IROW)=XPRO1+XDPDR*FLOAT(IROW-1)
    1   CONTINUE
        TPDRST(ISTYP)=XDPDR
        TPDSEP(ISTYP)=YDPAD
        TPDWID(ISTYP)=PADWD
C
C++     Fill in corner positions
C
        NTPCRN(ISTYP)=6
        TPCORN(1,1,ISTYP)=35.0
        TPCORN(2,1,ISTYP)=10.7
        TPCORN(1,2,ISTYP)=31.3
        TPCORN(2,2,ISTYP)=17.1
        TPCORN(1,3,ISTYP)=59.0
        TPCORN(2,3,ISTYP)=33.2
        TPCORN(1,4,ISTYP)=62.7
        TPCORN(2,4,ISTYP)=26.8
        TPCORN(1,5,ISTYP)=73.9
        TPCORN(2,5,ISTYP)=33.3
        TPCORN(1,6,ISTYP)=81.4
        TPCORN(2,6,ISTYP)=20.6
C
        TPPHI0(1)=0.
        DO 147 IR=1,NTPDRW(ISTYP)
          TPDPHS(IR,ISTYP)=TPDWID(ISTYP)/TPPROW(IR)
          TPDPHW(IR,ISTYP)=(FLOAT(NTPDPR(IR,ISTYP)/2)+0.5)
     &                     * TPDSEP(ISTYP)/TPPROW(IR)
  147   CONTINUE
C
C++     Now the wires
C
        DO 124 K=1,NWIR
          IF (K.LE.9) GO TO 124
          XAK=K-9
          IF (XAK.LE.58.) THEN
            XWIRL=((XAK-1.)*4.+342.)/TG60
            TWIRLE(K,ISTYP)=XWIRL
          ELSE IF (XAK.LE.66.) THEN
            XWIRL=(418.5-(XAK-1.)*4.)/TG30
            TWIRLE(K,ISTYP)=XWIRL
          ELSE IF (XAK.LE.94.) THEN
            XWIRL=((XAK-1.)*4.+195.)/TG60
            TWIRLE(K,ISTYP)=XWIRL
          ELSE IF (XAK.LE.113.) THEN
            XWIRL=(569.5-(XAK-1.)*4.)/TG30
            TWIRLE(K,ISTYP)=XWIRL
          ELSE
            TWIRLE(K,ISTYP)=0.
          ENDIF
  124   CONTINUE
        TWIRLE(67,ISTYP)=325.5
        TWIRLE(104,ISTYP)=328.
        TWIRLE(105,ISTYP)=327.
C
C++     Wire position
C
        NTWIRE(ISTYP)=NWIR
        TWIRE1(ISTYP)=XWIR1
        TWSTEP(ISTYP)=XDWIR
        DO 330 I=1,NWIR
          TWIRMN(I,ISTYP)=-0.5*TWIRLE(I,ISTYP)
          TWIRMX(I,ISTYP)=-TWIRMN(I,ISTYP)
  330   CONTINUE
C
      END
#endif
