      SUBROUTINE YPIDAS(IER)
C
C----------------------------------------------------------*
C!    Assign particle identity from data or Monte Carlo generation
CKEY YTOP PARTICLE
C!    Author :     G. Lutz   jan 5 1989
C!    modified:    J.Lauber  May 2 1991
C!    modified:    M.Bosman  jul 15 1991
C!
C!
C!    Description
C!    ===========
C!    This routine assigns particle identification possibilities of
C!    charged tracks either from data or from Monte Carlo generation.
C!    Results are stored in markers KPIDF0
C!    Assignment for real data introduced by J.Lauber
C!
C!
C!---------------------------------------------------------*
#ifndef DOC
      SAVE
#include "bcs.h"
#include "frftjj.h"
#include "fridjj.h"
#include "frtljj.h"
#include "fkinjj.h"
#include "ydimto.h"
#include "ypafma.h"
#include "yparto.h"
#include "ytrkto.h"
#include "ydumto.h"
C
#if defined(YDEBUG)
      LOGICAL LDMP1,LDMP2
#endif
C!---------------------------------------------------------*
#include "bmacro.h"
C!---------------------------------------------------------*
C
C-- Define the logical unit for printout
C
      LOUT = IW(6)
      IER = 0
C
#if defined(YDEBUG)
      IF(IDUMPP(29).GE.1) THEN
        LDMP1=.TRUE.
      ELSE
        LDMP1=.FALSE.
      ENDIF
C
      IF(IDUMPP(30).GE.1) THEN
        LDMP2=.TRUE.
      ELSE
        LDMP2=.FALSE.
      ENDIF
C
C
      IDUMPP(29)=IDUMPP(29)-1
      IDUMPP(30)=IDUMPP(30)-1
C
      IF(LDMP1) THEN
        WRITE(LOUT,901) NGTRTO
  901   FORMAT(' YPIDAS CALLED: NGTRTO=',I5)
      ENDIF
#endif
C----- for the charged tracks the probabilities of being
C----- e,mu,kaon,pion,proton are taken out of bank FRID
      KFRID=IW(NAMIND('FRID'))
      CALL YDEFRF(KFRFT0,KFRFT,KFRTL,IFAIL)
      IF(KFRID.LE.0 .OR. KFRFT.LE.0) THEN
        IER = 1
        RETURN
      ENDIF
C-----process all entries of frid
C----- cut on acceptance of particle identification probability pcut
C----- change to supplyabler parameter(s)
C----- default         pcut =  0.01
C----- to be changed:
      PCUT=PIDACP
#if defined(YDEBUG)
      IF(LDMP2) THEN
        WRITE(LOUT,
     $        '('' Trk Chrg KPIFD0  Elec. Muon  Pion  Kaon  Proton'')')
      ENDIF
#endif
      DO 200 I=1,LROWS(KFRID)
        CALL YMKZER(1,NMSIZZ,IPIDM)
C
C-- electron JFRIPE muon JFRIPM ,pion JFRIPI, kaon JFRIPK , proton JFRIPP
C
        IF(RTABL(KFRID,I,JFRIPE) .GE. PCUT )
     &         CALL YMKSET(1,NMSIZZ,IPIDM,JPAFEP)
        IF(RTABL(KFRID,I,JFRIPM) .GE. PCUT )
     &         CALL YMKSET(1,NMSIZZ,IPIDM,JPAFMP)
        IF(RTABL(KFRID,I,JFRIPI) .GE. PCUT )
     &         CALL YMKSET(1,NMSIZZ,IPIDM,JPAFPP)
        IF(RTABL(KFRID,I,JFRIPK) .GE. PCUT )
     &         CALL YMKSET(1,NMSIZZ,IPIDM,JPAFKP)
        IF(RTABL(KFRID,I,JFRIPP) .GE. PCUT )
     &         CALL YMKSET(1,NMSIZZ,IPIDM,JPAFPR)
C-----get sign of charge as product of curvature and magnetic field
        IF ( (BFIELD * RTABL(KFRFT,I,JFRFIR)) .LE. 0.0 ) THEN
          KPIDF0(I)=IPIDM
          ICHRG=1
        ELSE
C----- negative charged particles
          KPIDF0(I)=ISHFT(IPIDM,1)
          ICHRG=-1
        ENDIF
C-----
#if defined(YDEBUG)
        IF(LDMP2) THEN
          WRITE(LOUT,'('' '',I3,I3,''    '',Z4,''  '',5F6.3)')
     &      I,ICHRG,KPIDF0(I),(RTABL(KFRID,I,JFRIPE+IPO),IPO=0,4)
        ENDIF
#endif
C-----
  200 CONTINUE
      RETURN
      END
#endif
