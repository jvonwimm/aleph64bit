      SUBROUTINE HCFCLU(NHCLU)
C-----------------------------------------------------------------
C
C! form Hcal clusters
C
C      G.Capon               date : 861001
C!       output : NHCLU = number of Hcal clusters
C!       called by CALREC
C                         fills temporary bank HCSR
C?
C!======================================================================
#ifndef DOC
      EXTERNAL HCLTST
#include "rparac.h"
#include "hdebug.h"
#include "bcs.h"
#include "hcnamc.h"
#include "rlunit.h"
#include "hclujj.h"
#include "hsdajj.h"
#include "hwbnkw.h"
      PARAMETER(NLNMX=21)
C
      NHCLU=0
      JHSDA=IW(NAHSDA)
      IF (JHSDA.EQ.0) THEN
C                        if HSDA missing, create only HCLU miniheader
         CALL AUBOS('HCLU',0,2,JHCLU,IGARB)
         IF(IGARB.EQ.2)THEN
            CALL RERROR('HCFCLU',1,'no space for bank HCLU')
            GO TO 99
         ENDIF
         IW(JHCLU+1)=LHCLUA
         IW(JHCLU+2)=0
         GO TO 99
      ENDIF
      NHVST=IW(JHSDA+2)
      IF (NHVST.GT.400) THEN
         CALL RERROR('HCFCLU',4,'more than 400 hcal storeys')
         NHVST=400
      ENDIF
C
      LWORK=NHVST*NLNMX
      IFCLHW=0
      CALL WBANK(IW,IFCLHW,LWORK,*110)
C
C?              build link table in work bank
C?              IW(KNS+1)= generation #
C?              IW(KNS+2)= nb of links for storey NS
C?              IW(KNS+3,4,..) = # of connected storeys
C
      DO 10 NS1=1,NHVST-1
         KHSDA=JHSDA+2+LHSDAA*(NS1-1)
         IT1=IW(KHSDA+JHSDTI)
         JF1=IW(KHSDA+JHSDPI)
         NR1=IW(KHSDA+JHSDRN)
         KNS1=IFCLHW+NLNMX*(NS1-1)
         DO 20 NS2=NS1+1,NHVST
            KHSDA=JHSDA+2+LHSDAA*(NS2-1)
            IT2=IW(KHSDA+JHSDTI)
            JF2=IW(KHSDA+JHSDPI)
            NR2=IW(KHSDA+JHSDRN)
            KNS2=IFCLHW+NLNMX*(NS2-1)
C?             test for storey connection
            IF(HCLTST(IT1,JF1,NR1,IT2,JF2,NR2).GE.0.) THEN
               IF(IW(KNS1+2).EQ.NLNMX-2.OR.IW(KNS2+2).EQ.NLNMX-2) THEN
                  CALL RERROR('HCFCLU',5,'HCAL tower index repeated')
                  GO TO 10
               ENDIF
               IW(KNS1+2)=IW(KNS1+2)+1
               IW(KNS2+2)=IW(KNS2+2)+1
               KW1=KNS1+2+IW(KNS1+2)
               KW2=KNS2+2+IW(KNS2+2)
               IW(KW1)=NS2
               IW(KW2)=NS1
            ENDIF
   20    CONTINUE
   10 CONTINUE
C
C?           create HCSR bank for storey/cluster relation
C?           bank HCSR is temporary, dropped in routine Hclufi
C
      LNHCS=2*NHVST
      CALL AUBOS('HCSR',0,LNHCS,JHCSR,IGARB)
      IF(IGARB.EQ.1)JHSDA=IW(NAHSDA)
      IF(IGARB.EQ.2) THEN
         CALL RERROR('HCFCLU',2,'no space for bank HCSR')
         CALL WDROP(IW,IFCLHW)
         GO TO 99
      ENDIF
      KHCSR=JHCSR
C
C?              form Hcal clusters
C?              IW(JHCSR+1)=number of storeys in cluster 1 (=N-1)
C?              IW(JHCSR+2,+3,.+N)=storey # in cluster 1
C?              IW(JHCSR+N+1)=numb of storeys in clust 2, and so on
C
      DO 30 NS=1,NHVST
         KNS=IFCLHW+NLNMX*(NS-1)
         IF(IW(KNS+1).NE.0) GO TO 30
         IW(KNS+1)=1
         NHCLU=NHCLU+1
         IS=1
         IW(KHCSR+2)=NS
         KHSDA=JHSDA+2+LHSDAA*(NS-1)
         IW(KHSDA+JHSDCN)=NHCLU
         IF(IW(KNS+2).EQ.0) GO TO 34
         DO 31 NGEN=1,99
            NEXTG=0
            DO 32 NP=1,NHVST
               KNP=IFCLHW+NLNMX*(NP-1)
               IF(IW(KNP+1).EQ.NGEN)THEN
                  IW(KNP+1)=-1
                  DO 33 K=1,IW(KNP+2)
                     NF=IW(KNP+2+K)
                     KNF=IFCLHW+NLNMX*(NF-1)
                     IF(IW(KNF+1).EQ.0)THEN
                        IW(KNF+1)=NGEN+1
                        NEXTG=1
                        IS=IS+1
                        IW(KHCSR+1+IS)=NF
                        IW(KHSDA+JHSDNS)=NF
                        KHSDA=JHSDA+2+LHSDAA*(NF-1)
                        IW(KHSDA+JHSDCN)=NHCLU
                     ENDIF
   33             CONTINUE
               ENDIF
   32       CONTINUE
            IF(NEXTG.EQ.0) GO TO 34
   31    CONTINUE
 34      IW(KHCSR+1)=IS
         KHCSR=KHCSR+1+IS
         IW(KNS+1)=-1
   30 CONTINUE
C
C?          adjust HCSR bank length, drop work bank
C
      LNHCS=KHCSR-JHCSR
      CALL AUBOS('HCSR',0,LNHCS,JHCSR,IGARB)
      CALL WDROP(IW,IFCLHW)
C?             fill bank HCLU
      IF(NHCLU.GT.0) CALL HCLUFI(NHCLU)
      IF(NHCLU.GT.0) CALL HCLCRA
C                        debug printing
      IF (IHPRIN.GE.1) WRITE(LDEBRL,77) NHCLU
 77   FORMAT(I5,' HCAL CLUSTERS IN THIS EVENT')
      IF(IHPRIN.GE.3) CALL PRTABL('HCLU',0)
C
   99 CONTINUE
      RETURN
C
  110 CALL RERROR('HCFCLU',3,'no space for work bank')
      RETURN
      END
#endif
