      SUBROUTINE VDPGEO
C-----------------------------------------------------------------------
C! Print geometry for VDET
C!
C!  Author         P.Cattaneo        1/5/90
C   Modified by    F.Ranjard         11/2/91
C!
C!  Description
C!  ===========
C! Print /VDGEOS/, /VDSUPP/, /VDJGEO/, /VFTKAL/ variables
C! for Minivertex
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "vdgeos.h"
#include "vdsupp.h"
#include "vdjgeo.h"
#include "vftkal.h"
C ---------------------------------------------------------------------
C
C   Print /VDGEOS/
C
      WRITE (IW(6),2005) (I,I=1,LVDL)
      WRITE (IW(6),2004) IOPZVD
 2004 FORMAT(3X,'IOPZVD:',T40,2(3X,I2,5X))
      WRITE (IW(6),2011) (NDIVPV(I),I=1,LVDL)
      WRITE (IW(6),2012) (NSLOAD(J,1),J=1,NDIVPV(1))
      WRITE (IW(6),2013) (NSLOAD(J,2),J=1,NDIVPV(2))
 2005 FORMAT(//20X,'VDGEOM - Layer number'/
     &       7X,'Relevant quantity',T40,2(' Layer #',I1,1X))
 2011 FORMAT(3X,'NDIVPV = Numb of slot in phi dir:',T40,2(3X,I2,5X))
 2012 FORMAT(3X,'NSLOAD = Present phi slots in layer #1:',T40,3X,12I3)
 2013 FORMAT(3X,'NSLOAD = Present phi slots in layer #2:',T40,3X,16I3)
      WRITE (IW(6),112) (NSLOGM(NSLOAD(J,1),1),J=1,NDIVPV(1))
      WRITE (IW(6),113) (NSLOGM(NSLOAD(J,2),2),J=1,NDIVPV(2))
      WRITE (IW(6),114) (NSLOME(NSLOAD(J,1),1),J=1,NDIVPV(1))
      WRITE (IW(6),115) (NSLOME(NSLOAD(J,2),2),J=1,NDIVPV(2))
      WRITE (IW(6),116) (NSLOWA(NSLOAD(J,1),1),J=1,NDIVPV(1))
      WRITE (IW(6),117) (NSLOWA(NSLOAD(J,2),2),J=1,NDIVPV(2))
      WRITE (IW(6),118) (NSLOEL(NSLOAD(J,1),1),J=1,NDIVPV(1))
      WRITE (IW(6),119) (NSLOEL(NSLOAD(J,2),2),J=1,NDIVPV(2))
  112 FORMAT(3X,'NSLOGM in layer #1:',T40,3X,12I3)
  113 FORMAT(3X,'NSLOGM in layer #2:',T40,3X,16I3)
  114 FORMAT(3X,'NSLOME in layer #1:',T40,3X,12I3)
  115 FORMAT(3X,'NSLOME in layer #2:',T40,3X,16I3)
  116 FORMAT(3X,'NSLOWA in layer #1:',T40,3X,12I3)
  117 FORMAT(3X,'NSLOWA in layer #2:',T40,3X,16I3)
  118 FORMAT(3X,'NSLOEL in layer #1:',T40,3X,12I3)
  119 FORMAT(3X,'NSLOEL in layer #2:',T40,3X,16I3)
C
      WRITE (IW(6),2006) (I,I=1,NGEOM)
 2006 FORMAT(//20X,'VDGEOM - Geometry type'/
     &       7X,'Relevant quantity',T40,4(' Geom. #',I1,1X))
      WRITE (IW(6),106)  (NGEOWA(I),I=1,NGEOM)
 106  FORMAT(3X,'NGEOWA:',T40,4(3X,I2,5X))
      WRITE (IW(6),2010) (NDIVZV(I),I=1,NGEOM)
 2010 FORMAT(3X,'NDIVZV = Numb. of wafers in z dir.:',T40,4(3X,I2,5X))
      WRITE (IW(6),2190) (VDPSDM(I),I=1,NGEOM)
 2190 FORMAT(3X,'VDPSDM = r-phi dimension :',T40,4(3X,F6.3,2X))
      WRITE (IW(6),2191) NCERVZ
 2191 FORMAT(3X,'NCERVZ: ',T40,4(3X,I4,4X))
      WRITE (IW(6),109)  VDBXTH
  109 FORMAT(3X,'VDBXTH:',T40,4F9.4)
C
      WRITE (IW(6),2007) (I,I=1,NWAFM)
 2007 FORMAT(//20X,'VDGEOM - Wafer type'/
     &       7X,'Relevant quantity',T40,4(' Wafer #',I1,1X))
      WRITE (IW(6),107)  VDAPPL
      WRITE (IW(6),108)  VDWLEN
      WRITE (IW(6),110)  VDZOFF
      WRITE (IW(6),2192) VDCEZO
 2192 FORMAT(3X,'VDCEZO:',T40,8F9.3/1X,T40,8F9.3)
      WRITE (IW(6),2031) VDDEPL
 2031 FORMAT(3X,'VDDEPL:',T40,4F9.3)
      WRITE (IW(6),2032) VDLESP
 2032 FORMAT(3X,'VDLESP:',T40,4F9.3)
      WRITE (IW(6),2033) VDLESZ
 2033 FORMAT(3X,'VDLESZ:',T40,4F9.3)
      WRITE (IW(6),2034) VDSTPH
 2034 FORMAT(3X,'VDSTPH:',T40,4F9.3)
      WRITE (IW(6),2035) VDSTZE
 2035 FORMAT(3X,'VDSTZE:',T40,4F9.3)
      WRITE (IW(6),2036) NVDPPI
 2036 FORMAT(3X,'NVDPPI:',T40,4(3X,I5,2X))
      WRITE (IW(6),2037) NVDZPI
 2037 FORMAT(3X,'NVDZPI:',T40,4(3X,I5,2X))
      WRITE (IW(6),2038) IPSIGN
 2038 FORMAT(3X,'IPSIGN:',T40,4I3,5X,4I3)
      WRITE (IW(6),2039) IZSIGN
 2039 FORMAT(3X,'IZSIGN:',T40,4I3,5X,4I3)
  107 FORMAT(3X,'VDAPPL:',T40,4F9.3)
  108 FORMAT(3X,'VDWLEN:',T40,8F9.3)
  110 FORMAT(3X,'VDZOFF:',T40,8F9.3/1X,T40,8F9.3)
      WRITE (IW(6),2030) (VDPPIT(I),I=1,NWAFM)
 2030 FORMAT(3X,'VDPPIT = Pitch in r-phi:',T40,4(3X,F6.3,2X))
      WRITE (IW(6),2040) (VDZPIT(I),I=1,NWAFM)
 2040 FORMAT(3X,'VDZPIT = Pitch in zed:',T40,4(3X,F6.3,2X))
      WRITE (IW(6),2070) (VDDIPP(I),I=1,NWAFM)
 2070 FORMAT(3X,'VDDIPT = r-phi strip r-phi border:',T40,4(3X,F6.3,2X))
      WRITE (IW(6),2080) (VDDIPZ(I),I=1,NWAFM)
 2080 FORMAT(3X,'VDDIPZ = r-phi strip zed border:',T40,4(3X,F6.3,2X))
      WRITE (IW(6),2090) (VDDIZP(I),I=1,NWAFM)
 2090 FORMAT(3X,'VDDIZP = zed strip r-phi border:',T40,4(3X,F6.3,2X))
      WRITE (IW(6),2100) (VDDIZZ(I),I=1,NWAFM)
 2100 FORMAT(3X,'VDDIZZ = zed strip zed border:',T40,4(3X,F6.3,2X))
      WRITE (IW(6),2120) (NSTPVD(I),I=1,NWAFM)
 2120 FORMAT(3X,'NSTPVD = Numb of r-phi strips/wafer:',T40,4(3X,I5,2X))
      WRITE (IW(6),2130) (NSTZVD(I),I=1,NWAFM)
 2130 FORMAT(3X,'NSTZVD = Numb of zed strips/wafer:',T40,4(3X,I5,2X))
      WRITE (IW(6),2140) (ACTPVD(I),I=1,NWAFM)
 2140 FORMAT(3X,'ACTPVD = r-phi active length/wafer:',T40,4(3X,F6.3,2X))
      WRITE (IW(6),2150) (ACTZVD(I),I=1,NWAFM)
 2150 FORMAT(3X,'ACTZVD = zed active length/wafer:',T40,4(3X,F6.3,2X))
      WRITE (IW(6),2160) (VDTHCK(I),I=1,NWAFM)
 2160 FORMAT(3X,'VDTHCK = Wafer thickness: ',T40,4(3X,F6.3,2X))
C
      DO 100 IL = 1,LVDL
        WRITE (IW(6),2109) IL,(NSLOAD(I,IL),I=1,NDIVPV(IL))
        WRITE (IW(6),2110) (VDCRHO(I,IL),I=1,NSLOM)
        WRITE (IW(6),2180) (VDCPHI(I,IL),I=1,NSLOM)
        WRITE (IW(6),2170) (VDTILT(I,IL),I=1,NSLOM)
        WRITE (IW(6),111)  (VDPOFF(I,IL),I=1,NSLOM)
        DO 101 N=1,NDIVPV(IL)
          NS = NSLOAD(N,IL)
          WRITE (IW(6),2171) NS,(ZWAFVD(J,NS,IL),J=1,NWAFM)
  101   CONTINUE
  100 CONTINUE
 2171 FORMAT(3X,'slot# ',I3,'  Z wafer position:',T40,4(1X,F8.3,2X))
 2109 FORMAT(3X,'layer # ',I3,'  slot # ',T40,15I4)
 2110 FORMAT(3X,'VDCRHO = Radius to center length:',T40,8F8.3/
     &       1X,T40,8F8.3)
 2170 FORMAT(3X,'VDTILT = Tilt angle of wafers: ',T40,8F8.3/
     &       1X,T40,8F8.3)
 2180 FORMAT(3X,'VDCPHI = Phi angle of the center :',T40,8F8.3/
     &       1X,T40,8F8.3)
  111 FORMAT(3X,'VDPOFF:',T40,8F8.3/1X,T40,8F8.3)
C
C - print /VDSUPP/
C
      WRITE (IW(6),'(/3X,''Print /VDSUPP/  ''/)')
      WRITE (IW(6),'(3X,''VDAPLN:'',T20,4F9.3,5X,4F9.3)') VDAPLN
      WRITE (IW(6),'(3X,''VMETIL:'',T20,4F9.3,5X,4F9.3)') VMETIL
      WRITE (IW(6),'(3X,''VDHYL1:'',T20,F9.3)') VDHYL1
      WRITE (IW(6),'(3X,''VDHYL2:'',T20,F9.3)') VDHYL2
      WRITE (IW(6),'(3X,''VDHYL3:'',T20,F9.3)') VDHYL3
      WRITE (IW(6),'(3X,''VDHYL4:'',T20,F9.3)') VDHYL4
      WRITE (IW(6),'(3X,''VDHYL5:'',T20,F9.3)') VDHYL5
      WRITE (IW(6),'(3X,''VDHYCE:'',T20,F9.3)') VDHYCE
      WRITE (IW(6),'(3X,''VDHYCW:'',T20,F9.3)') VDHYCW
      WRITE (IW(6),'(3X,''VDHYCE:'',T20,F9.3)') VDHYCE
      WRITE (IW(6),'(3X,''VDHYST:'',T20,2F9.3)') VDHYST
C
C - print /VDJGEO/
C
      WRITE (IW(6),'(/3X,''Print /VDJGEO/ ''/)')
C      COMMON /VDJGEO/  NPSLLY(JVDL),JSLOGM(JSLOM,JVDL),
C     &                 VJRIOL(JVDL),VJBIGZ,VJPHIZ(JVDL),VJFPHI(JVDL),
C     &                 VJWACT(JGEO),VJUACT(JGEO),VJZOFF(JWAFN,JGEO)
      WRITE (IW(6),'(3X,''NPSLLY:'',T20,2I5)') NPSLLY
      WRITE (IW(6),'(3X,''JSLOGM:'',T20,30I3)') JSLOGM
      WRITE (IW(6),'(3X,''VJRIOL:'',T20,2F10.3)') VJRIOL
      WRITE (IW(6),'(3X,''VJPHIZ:'',T20,2F10.3)') VJPHIZ
      WRITE (IW(6),'(3X,''VJFPHI:'',T20,2F10.3)') VJFPHI
      WRITE (IW(6),'(3X,''VJWACT:'',T20,4F10.3)') VJWACT
      WRITE (IW(6),'(3X,''VJUACT:'',T20,4F10.3)') VJUACT
      WRITE (IW(6),'(3X,''VJZOFF:'',T20,8F10.3)') VJZOFF
      WRITE (IW(6),'(3X,''VJBIGZ:'',T20,F10.4)') VJBIGZ
C
C - Print /VFTKAL/
C
      WRITE (IW(6),'(/3X,''Print /VFTKAL/ ''/)')
C      COMMON /VFTKAL/ KDIVPV(KVDL),VKRF(KFACE,KVDL),VKUWID(KFACE,KVDL),
C     &                VKZWID(KFACE,KVDL),VKPHIF(KFACE,KVDL),
C     &                VKPHIN(KFACE,KVDL),KBPSTP
      WRITE (IW(6),'(3X,''KBPSTB:'',T15,I5)') KBPSTP
      WRITE (IW(6),'(3X,''KDIVPV:'',T15,2I5)') KDIVPV
      WRITE (IW(6),'(3X,''VKRF:'',T15,15F7.3)') VKRF
      WRITE (IW(6),'(3X,''VKUWID:'',T15,15F7.3)')VKUWID
      WRITE (IW(6),'(3X,''VKZWID:'',T15,15F7.3)')VKZWID
      WRITE (IW(6),'(3X,''VKPHIF:'',T15,15F7.3)')VKPHIF
      WRITE (IW(6),'(3X,''VKPHIN:'',T15,15F7.3)')VKPHIN
C
      RETURN
      END
#endif
