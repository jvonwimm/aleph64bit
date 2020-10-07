      SUBROUTINE YTOSUM
C
C----------------------------------------------------------*
C!    summary printout for the TOPOLOGY reconstruction
CKEY YTOP SUMMARY
C!    Author :     G. Lutz   30/11/87
C!    Modified :   M. Bosman 01/12/88
C!    Modified :   G. Lutz   12/04/92
C!
C!
C!    Description
C!    ===========
C!    This routine prints the summary
C!    for the TOPOLOGY reconstruction program
C!
C!---------------------------------------------------------*
#ifndef DOC
      SAVE
#include "bcs.h"
#include "yparto.h"
#include "ysumto.h"
C!---------------------------------------------------------*
C
C-- Define the logical unit for printout
C
      LOUT = IW(6)
C
      WRITE(LOUT,1000)
      WRITE(LOUT,1001) NEPVTO,NRPVTO
      IF(NRPVTO.GT.0) THEN
        ATPVTO = ATPVTO / FLOAT(NRPVTO)
        AMPVTO = AMPVTO / FLOAT(NRPVTO)
        ACPVTO = ACPVTO / FLOAT(NRPVTO)
        DO 10 I=1,3
          APPVTO(I) = APPVTO(I) / FLOAT(NRPVTO)
   10   CONTINUE
        WRITE(LOUT,1002) ATPVTO,AMPVTO
        WRITE(LOUT,1003) ACPVTO
        WRITE(LOUT,1004) (APPVTO(I),I=1,3)
      ENDIF
C
      IF(LCONVS) THEN
        WRITE(LOUT,1011) NRCOTO
        IF(NRCOTO.GT.0) THEN
          AMCOTO = AMCOTO / FLOAT(NRCOTO)
          ACCOTO = ACCOTO / FLOAT(NRCOTO)
          ARCOTO = ARCOTO / FLOAT(NRCOTO)
          WRITE(LOUT,1012) AMCOTO
          WRITE(LOUT,1013) ACCOTO
          WRITE(LOUT,1014) ARCOTO
        ENDIF
C
      ENDIF
C
      IF(LVZERS) THEN
        WRITE(LOUT,1021) NRK0TO
        IF(NRK0TO.GT.0) THEN
          AMK0TO = AMK0TO / FLOAT(NRK0TO)
          ACK0TO = ACK0TO / FLOAT(NRK0TO)
          ATK0TO = ATK0TO / FLOAT(NRK0TO)
          WRITE(LOUT,1022) AMK0TO
          WRITE(LOUT,1023) ACK0TO
          WRITE(LOUT,1024) ATK0TO
        ENDIF
        WRITE(LOUT,1031) NRLATO
        IF(NRLATO.GT.0) THEN
          AMLATO = AMLATO / FLOAT(NRLATO)
          ACLATO = ACLATO / FLOAT(NRLATO)
          ATLATO = ATLATO / FLOAT(NRLATO)
          WRITE(LOUT,1032) AMLATO
          WRITE(LOUT,1033) ACLATO
          WRITE(LOUT,1034) ATLATO
        ENDIF
        WRITE(LOUT,1041) NRLBTO
        IF(NRLBTO.GT.0) THEN
          AMLBTO = AMLBTO / FLOAT(NRLBTO)
          ACLBTO = ACLBTO / FLOAT(NRLBTO)
          ATLBTO = ATLBTO / FLOAT(NRLBTO)
          WRITE(LOUT,1032) AMLBTO
          WRITE(LOUT,1033) ACLBTO
          WRITE(LOUT,1034) ATLBTO
        ENDIF
C
      ENDIF
C
      WRITE(LOUT,1005) (KYFLAG(I),I=1,10)
      WRITE(LOUT,1006) (KYFLAG(I),I=11,14)
 1000 FORMAT(/,80('-'),///,
     +  ' YTOPOL : Summary of Vertex reconstruction  :',//)
 1001 FORMAT(' Nb of events processed =',I5/,
     +     /,' A primary vertex was found for',I5,' events')
 1002 FORMAT(' Average nb of tracks in primary vertex =',F5.1,/,
     &       ' with an average momentum of',F6.2)
 1003 FORMAT(' The average CHI2 per degree of freedom is',F8.2)
 1004 FORMAT(' The average beam position in X,Y,Z is',3F10.5)
C
 1011 FORMAT(/,' nb of photon conversions = ',I5)
 1012 FORMAT(' average photon momentum =',F6.2)
 1013 FORMAT(' average conversion vertex chisq',F8.2)
 1014 FORMAT(' average conversion radius ',F10.5)
C
 1021 FORMAT(/,' nb of K0-s = ',I5)
 1022 FORMAT(' average K0 momentum =',F6.2)
 1023 FORMAT(' average K0 vertex chisq',F8.2)
 1024 FORMAT(' average K0 lifetime ',E10.5)
C
 1031 FORMAT(/,' nb of lambdas = ',I5)
 1032 FORMAT(' average lambda momentum =',F6.2)
 1033 FORMAT(' average lambda vertex chisq',F8.2)
 1034 FORMAT(' average lambda lifetime ',E10.5)
C
 1041 FORMAT(/,' nb of lambda-bar-s = ',I5)
 1042 FORMAT(' average lambda-bar momentum =',F6.2)
 1043 FORMAT(' average lambda-bar vertex chisq',F8.2)
 1044 FORMAT(' average lambda-bar lifetime ',E10.5)
C
 1005 FORMAT(//,' YTOPOL : Summary of messages reporting'//,
     +  ' Default values used for beam centroid       ',I5/,
     +  ' problem in PIDAS  : particle assignment     ',I5/,
     +  ' problem in YTOSTR : track selection         ',I5/,
     +  ' problem in YTOCON : gamma conversion search ',I5/,
     +  ' problem in YTORV0 : V0 search               ',I5/,
     +  ' problem in YTOPVT : non zero return code    ',I5/,
     +  ' problem in YTOSVT : secondary vertex search ',I5/,
     +  ' problem in YTHIST : no PYER bank found      ',I5/,
     +  ' problem in YTOPVT : no FRTL bank found      ',I5/,
     +  ' YTOPVT : no primary vertex reconstructed    ',I5)
 1006 FORMAT(
     +  ' problem in YTOPVT : no space to create PYFR ',I5/,
     +  ' problem in YTOPVT : no space to create PYER ',I5/,
     +  ' problem in YTOPVT : no FRFT bank found      ',I5/,
     +  ' problem in YTOPVT : error return from UMSERR',I5/)
      RETURN
      END
#endif
