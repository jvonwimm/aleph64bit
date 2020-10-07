      SUBROUTINE ECGMIX
C***********************************************************************
C!Search for embedded gammas in charged eclusters                      *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  901206                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ECLU,ECT1,ERL3,EIDT                                    *
C!    OUTPUT  : EGMX                                                   *
C!    CREATED : EGMX                                                   *
C!                                                                     *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "ecnamc.h"
#include "eclujj.h"
#include "ect1jj.h"
#include "erl3jj.h"
#include "eidtjj.h"
#include "etp1jj.h"
#include "egmajj.h"
#include "bcs.h"
      DIMENSION XTR(3),PX(3,2),THP(2),PHP(2),ECP(2),PSC(2)
      DIMENSION PEL(3)
      DIMENSION IOPT(6),CORE(6),IFOT(19),PHOT(20)
C
C These are the options for EBNEUT : do not apply any crack correction
C
      DATA IOPT /0, 1, 4*0/
C
#include "bmacro.h"
      KECLU=IW(NAECLU)
      KECT1=IW(NAECT1)
      KERL3=IW(NAERL3)
      KEIDT=IW(NAEIDT)
      KETP1=IW(NAETP1)
      IF(KECLU.EQ.0.OR.KECT1.EQ.0.OR.KERL3.EQ.0.OR.KETP1.EQ.0)
     +       GO TO 998
      NECLU=LROWS(KECLU)
C
C? Create EGMA bank, number of lines=number of eclu (max)
C
      NDATA=NECLU*LEGMAA+LMHLEN
      CALL AUBOS('EGMA',0,NDATA,KEGMA,IGARB)
      IF(IGARB.EQ.2)GO TO 997
      IW(KEGMA+LMHROW)=0
      IW(KEGMA+LMHCOL)=LEGMAA
C
C? Put bank in list
C
      CALL BLIST(IW,'T+','EGMA')
C
C? Relink banks, in case of garbage collection
C
      KECT1=IW(NAECT1)
      KERL3=IW(NAERL3)
      KEIDT=IW(NAEIDT)
      KETP1=IW(NAETP1)
C
      IF(KEIDT.EQ.0)THEN
         NEIDT=0
      ELSE
         NEIDT=LROWS(KEIDT)
      ENDIF
      DO 10 IECLU=1,NECLU
C
C? Get number of tracks
C
      NTRK=0
      PCHT=0.
      IRL3=ITABL(KECT1,IECLU,JECTER)
   12 IF(IRL3.NE.0)THEN
         IETP1=ITABL(KERL3,IRL3,JERLET)
         CALL ESLAS(IECLU,IETP1,IKEPT)
         IF(IKEPT.NE.0) THEN
            NTRK=NTRK+1
            PCHT=PCHT+RTABL(KETP1,IETP1,JETPPT)
         ENDIF
         IRL3=ITABL(KERL3,IRL3,JERLER)
         GO TO 12
      ENDIF
C
C? Reject neutral clusters
C
      IF(NTRK.EQ.0)GO TO 10
C
C? Verify that no electron is associated to this ECLU
C
   50 CONTINUE
      DO 51 IEL=1,3
         PEL(IEL)=0.
   51 CONTINUE
      EEL=0.
      DO 20 ITR=1,NEIDT
      ICLL=ITABL(KEIDT,ITR,JEIDPE)
      IF(ICLL.EQ.IECLU)THEN
         NATU=ITABL(KEIDT,ITR,JEIDIP)
C
C?  If track is an electron, sum it
C
         IF(NATU.NE.0)THEN
            ITKE=ITABL(KEIDT,ITR,JEIDFR)
            PE=RTABL(KETP1,ITKE,JETPPT)
            XC=RTABL(KETP1,ITKE,JETPX3)
            YC=RTABL(KETP1,ITKE,JETPX3+1)
            ZC=RTABL(KETP1,ITKE,JETPX3+2)
            RC=SQRT(XC**2+YC**2+ZC**2)
            PEL(1)=PEL(1)+PE*XC/RC
            PEL(2)=PEL(2)+PE*YC/RC
            PEL(3)=PEL(3)+PE*ZC/RC
            EEL=EEL+PE
         ENDIF
      ENDIF
   20 CONTINUE
      CALL EBNEUT(IECLU,IOPT,IFOT,PHOT,CORE,JER)
      IF(JER.GT.0)GO TO 10
C  Janot's cuts
C
C  No electron and EBNEUT error :
C
      IF (EEL.EQ.0.AND.JER.NE.0)GO TO 10
C
C  Electromagnetic for sure, but e14+e17 < 1.
C
      ELEPH=PHOT(14)+PHOT(17)
      IF(IFOT(16).EQ.0.AND.ELEPH.LT.1.) GO TO 10
C
C  Ambiguous, but e14+e17 < 1.
C
      IF(IFOT(16).EQ.2.AND.ELEPH.LT.2.) GO TO 10
C
C? The cluster is not certainly hadronic. Let us consider it
C?  electromagnetic
C
       IF(IFOT(16).NE.1)THEN
C
C?  Get the 2 particles from ebneut
C
         DO 30 IPA=1,2
C   Theta.
        THP(IPA) = PHOT( 12 + 3 * IPA )
C   Phi
        PHP(IPA) = PHOT( 13 + 3 * IPA )
C   Corrected energy
        ECP(IPA) = PHOT( 11 + 3 * IPA )
        SINTH = SIN(THP(IPA))
        PX(1,IPA) = SINTH*COS(PHP(IPA))
        PX(2,IPA) = SINTH*SIN(PHP(IPA))
        PX(3,IPA) = COS(THP(IPA))
   30   CONTINUE
C
C? Let us see if there is a second electromagnetic particle
C
        IF(PHOT(17).NE.0)THEN
           IF(IFOT(19).NE.17)THEN
              DO 40 KC=1,3
   40         PX(KC,1)=(PX(KC,1)*ECP(1)+PX(KC,2)*ECP(2))
              ETOT=0.
              DO 41 IEL=1,3
                 ETOT=ETOT+PX(IEL,1)**2
   41         CONTINUE
              ETOT=SQRT(ETOT)
              DO 42 IEL=1,3
                 PX(IEL,1)= PX(IEL,1)/ETOT
   42            CONTINUE
              ECP(1)=ETOT
           ENDIF
        ENDIF
        DO 43 IEL=1,3
           PX(IEL,1)=(PX(IEL,1)*ECP(1)-PEL(IEL))
   43   CONTINUE
        ETOT=0.
        DO 44 IEL=1,3
           ETOT=ETOT+PX(IEL,1)**2
   44   CONTINUE
        ETOT=SQRT(ETOT)
        DO 45 IEL=1,3
           PX(IEL,1)= PX(IEL,1)/ETOT
   45   CONTINUE
        ECP(1)=ETOT
        THP(1)=ACOS(PX(3,1))
        PHP(1)=ATAN2(PX(2,1),PX(1,1))
        IBEST=1
C
C? When there are electrons, look if the excess of electromagnetic energ
C? is more than 20% root E of the electron, then we take it
C
      IF(EEL.NE.0.)THEN
         EJAN=((PHOT(14)+PHOT(17))/CORE(6)-EEL)/CORE(5)
         IF(EJAN.LE.0.2*SQRT(PCHT))GO TO 10
      ENDIF
C
C
C? Fill bank EGMA (next line)
C
         NEGMA=LROWS(KEGMA)
         NEGMA=NEGMA+1
         IW(KEGMA+LMHROW)=NEGMA
         KLINE=KROW(KEGMA,NEGMA)
         RW(KLINE+JEGMEN)=ECP(IBEST)
         RW(KLINE+JEGMTE)=THP(IBEST)
         RW(KLINE+JEGMPH)=PHP(IBEST)
         IW(KLINE+JEGMPE)=IECLU
      ENDIF
   10 CONTINUE
C
C? Remove useless lines
C
      CALL AUBPRS('EGMA')
  999 CONTINUE
      RETURN
  998 CONTINUE
      CALL RERROR('ECGMIX',1,' Missing bank')
      RETURN
  997 CONTINUE
      CALL RERROR('ECGMIX',2,' Too little space, EGMA not treated')
      IND=NDROP('EGMA',0)
      RETURN
      END
#endif
