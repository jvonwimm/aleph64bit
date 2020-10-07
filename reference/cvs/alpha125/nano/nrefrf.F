      SUBROUTINE NREFRF(IRET)
CKEY NANO IN ALPHA /INTERNAL
C----------------------------------------------------------------------
C! NANO unpacking : create FRFT bank from NDTK bank
C!
C!   Author   :- Gerrit Graefe         15-FEB-1994
C!   Modified :- Gerrit Graefe         27-FEB-1994 link to bank NEHE,
C!                                                 if Nano version >115
C!
C!   Inputs:
C!        - none
C!
C!   Outputs:
C!        - IRET / I        return code  0=created FRFT from NDTK
C!                                       1=can't find NDTK
C!                                       2=no space to book FRFT
C!                                       3=unknown error
C!
C!   Libraries required: ALPHA,BOS77
C!
C!   Description
C!   ===========
C!
C?   This subroutine creates the bank FRFT from the NDTK bank. This is a
C?   dangerous thing because not all tracks are kept on the NANO DST and the
C?   remaining are reordered. So the pointers from other banks like YV0V have
C?   to reset and there is no easy way to get the original 'JULIA' number. The
C?   only way is to link to the NDTK bank and ask for the JULIA track number in
C?   this bank. There is a one-to-one relation between the row numbers in NDTK
C?   and FRFT made from NDTK.
C?   Also there is a problem with the track quality. Normally only good tracks
C?   are kept on the NANO DST but although those which are coming from a V0 or
C?   a gamma conversion candidate even if they are not 'good' ones. This was
C?   done for quality studies (dE/dx, # of hits in TPC/ITC etc.). Unfortunately
C?   there is no proper way to put some quality flag into FRFT. So this
C?   information is put into the error matrix which is zero by default. A huge
C?   value of QFRFEM(ITK,1,1) [99999.0] indicates that this is a 'bad' track
C?   according to the NANO DST definitions.
C?
C?
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
#include "nbpdcl.h"
#include "nbnkpo.h"
#include "ntrcor.h"
#include "ndtkjj.h"
#include "ndhejj.h"
#include "nehejj.h"
      INTEGER NLINK,NVERS,NAMIND,NAVERS
      INTEGER I,J,INDNTK,INDFRF,IGARB,IROWN,IROWF,KOFRFT,JULTRK,
     &        NJULTR,IFBADT,INDNHE,IRET,KRNDTK
      REAL    GP(3),P,GPHI,GRHO,GTHETA,GSECL,GTANL,GCHAR,CFACT,
     &        BFACT,DUMMY
      REAL    ALFIEL
      LOGICAL BTEST
      INTEGER MASK1,MASK2,MASK8,MASK9,MASK14,MASK15
      DATA    MASK1,MASK2,MASK8,MASK9,MASK14,MASK15
     &        /1,3,255,511,16383,32767/
      PARAMETER (CFACT=CLGHT/100000.)
#include "qmacro.h"
C----------------------------------------------------------------------
C
C!..LINK TO BANK NDTK
C
      IRET=3
      INDNTK=IW(NAMIND('NDTK'))
      IF(INDNTK.EQ.0)THEN
        IRET=1
        RETURN
      ENDIF
      KONDTK=INDNTK
      KCNDTK=IW(KONDTK+1)
      KRNDTK=IW(KONDTK+2)
C
C!..LINK TO BANK NDHE TO GET ROW NUMBER OF FIRST BAD TRACK
C
      CALL GETNAV(NAVERS)
      IF(NAVERS.EQ.115)THEN
        INDNHE=IW(NAMIND('NDHE'))
        IF(INDNHE.EQ.0)THEN
          IRET=1
          RETURN
        ENDIF
        KONDHE=INDNHE
        KCNDHE=IW(KONDHE+1)
        IFBADT=IAND(IW(KONDHE+LMHLEN+JNDHP2),MASK8)
      ELSE
        INDNHE=IW(NAMIND('NEHE'))
        IF(INDNHE.EQ.0)THEN
          IRET=1
          RETURN
        ENDIF
        KONEHE=INDNHE
        KCNEHE=IW(KONEHE+1)
        IFBADT=IAND(IW(KONEHE+LMHLEN+JNDHP2),MASK8)
      ENDIF
C
C!..CREATE BANK FRFT
C
      CALL AUBOS('FRFT',2,LMHLEN+LFRFTA*KRNDTK,INDFRF,IGARB)
      IF(IGARB.EQ.2)THEN
        CALL QWMESE('### NREFRF ### No space to book FRFT')
        IRET=2
        RETURN
      ENDIF
      CALL BKFMT('FRFT','2I,(8F,2I)')
      NAFRFT=NAMIND('FRFT')
      KOFRFT=INDFRF
      IW(KOFRFT+1)=LFRFTA
      IW(KOFRFT+2)=KRNDTK
C
C!..GET B-FIELD FROM ALEPHLIB ROUTINE ALFIEL
C
      BFACT = - ALFIEL(DUMMY) * CFACT
      IF(BFACT.EQ.0.) BFACT = - 15. * CFACT
C
C!..SET NJUNAT(I) TO 0
C
      CALL VZERO(NJUNAT,255)
C
C!..FILL BANK FRFT
C
      DO 100 I=1,KRNDTK
C!..ROW IN FRFT BANK
        IROWN=KONDTK+LMHLEN+LNDTKA*(I-1)
        IROWF=KOFRFT+LMHLEN+LFRFTA*(I-1)
C!..CORRESPONDENCE BETWEEN OLD AND NEW FRFT BANK
        NJUNAT(I)=IAND(IW(IROWN+JNDTTA),MASK8)
C!..MOMENTA
        GP(1) = FLOAT(IW(IROWN+JNDTPX)) / 10000000.0
        GP(2) = FLOAT(IW(IROWN+JNDTPY)) / 10000000.0
        GP(3) = FLOAT(IW(IROWN+JNDTPZ)) / 10000000.0
        P     = SQRT(GP(1)*GP(1)+GP(2)*GP(2)+GP(3)*GP(3))
        GTHETA = ACOS(GP(3) / P)
        GSECL = 1. / SIN(GTHETA)
        GTANL = TAN( PIBY2-GTHETA )
        IF (IAND(ISHFT(IW(IROWN+JNDTTA),-10),MASK1).EQ.0) THEN
          GCHAR = -1.0
        ELSE
          GCHAR =  1.0
        ENDIF
        GRHO = BFACT * GSECL / (GCHAR * P)
        GPHI = ATAN2(GP(2),GP(1)) + (QQPI-SIGN(QQPI,GP(2)))
        RW(IROWF+JFRFIR) = GRHO
        RW(IROWF+JFRFTL) = GTANL
        RW(IROWF+JFRFP0) = GPHI
C!..D0+Z0
        IF (BTEST(IW(IROWN+JNDTDZ),15)) THEN
          RW(IROWF+JFRFD0)=FLOAT(IAND(IW(IROWN+JNDTDZ),MASK15))/
     &    (-1000.0)
        ELSE
          RW(IROWF+JFRFD0)=FLOAT(IAND(IW(IROWN+JNDTDZ),MASK15))/1000.0
        ENDIF
        IF (BTEST(IW(IROWN+JNDTDZ),31)) THEN
          RW(IROWF+JFRFZ0)=-FLOAT(IAND(ISHFT(IW(IROWN+JNDTDZ),-16),
     &                     MASK15)) / 1000.0
        ELSE
          RW(IROWF+JFRFZ0)=FLOAT(IAND(ISHFT(IW(IROWN+JNDTDZ),-16),
     &                     MASK15)) / 1000.0
        ENDIF
C!..CHI2
        RW(IROWF+JFRFC2)=FLOAT(IAND(IW(IROWN+JNDTTQ),MASK14)) / 100.0
        IW(IROWF+JFRFDF)=1
C!..ERROR ON MOMENTUM
          RW(IROWF+JFRFEM)=(FLOAT(IAND(ISHFT(IW(IROWN+JNDTTQ),-14),
     &                      MASK9))*P)**2
C!..BAD TRACK
        IF (IFBADT.LE.I) THEN
          RW(IROWF+JFRFEM+1)=99999.0
        ELSE
          RW(IROWF+JFRFEM+1)=    0.0
        ENDIF
C!..ERROR MATRIX
        DO 110 J=2,20
          RW(IROWF+JFRFEM+J)=    0.0
  110   CONTINUE
  100 CONTINUE
      CALL BLIST(IW,'S+','FRFT')
      IRET=0
  999 RETURN
      END
#endif
