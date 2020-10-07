      SUBROUTINE ECST (IER)
C=======================================================================
C!     Steering for mask analysis
C!     Creates POT banks PMSK and PRTM
C
C-
C-     Authors : M.N. Minard , M. Pepe       26/02/89
C-     Modified: M.N  Minard                 04/01/91
C-     Modified: F.Ranjard                   06/06/91
C      to add some protections when creating banks
C
C-     Creates temporary bank EMSK
C-     Creates temporary bank ECTY to tag electromagnetic cluster type
C-     For each track  calculates energy in road
C-     Fills  EMSK , PRTM  banks
C-     Output : IER = 0  all banks available for analysis
C-              ELSE     some banks are missing
C?
C!======================================================================
#ifndef DOC
#include "etp1jj.h"
#include "esdajj.h"
#include "ectyjj.h"
#include "eclujj.h"
#include "ect1jj.h"
#include "erl3jj.h"
#include "emskjj.h"
#include "pmskjj.h"
#include "prtmjj.h"
#include "ecnmsk.h"
#include "bcs.h"
      DIMENSION XXX ( 7 ), EMIP(5) ,XV(3)
      DIMENSION XTRAP (3,5) , ITRAP (5,5) , XCOS(3,5)
      DIMENSION DUMMY(4)
      DATA NAECTY /0/
#include "bmacro.h"
C
      IF (NAECTY.EQ.0) THEN
        NAECTY = NAMIND('ECTY')
        NAEMSK = NAMIND('EMSK')
      ENDIF

      IER = 0
C
C-    Define cluster type (summary of identification analysis)
C
      CALL ECLTYP
C
C     Create temporary bank to store cell content
C
      CALL ECTEMP
C
C-    Book EMSK bank ( output E-FLOW information )
C
      KETP1 = IW (NAETP1)
      IF (KETP1.EQ.0) THEN
        GO TO 998
      ENDIF
C
      NTRAK = LROWS ( KETP1 )
      NLENG = LMHLEN + NTRAK*LEMSKA
      CALL AUBOS ( 'EMSK',0,NLENG,KEMSK,IGAR1)
      IF ( KEMSK.EQ.0 ) GOTO 999
      CALL BLIST(IW,'T+','EMSK')
      IW ( KEMSK + 1 ) = LEMSKA
      IW ( KEMSK + 2 ) = 0
      CALL VZERO ( RW(KEMSK+3),NLENG-LMHLEN)
C
C-    Create relation bank EMSK - ETP1
C
      NLENG = LMHLEN + NTRAK*LPRTMA
      CALL AUBOS ( 'PRTM',0,NLENG,KPRTM,IGAR2)
      IF ( KPRTM.EQ.0 ) GOTO 999
      CALL BLIST(IW,'E+','PRTM')
      IW ( KPRTM + 1 ) = LPRTMA
      IW ( KPRTM + 2 ) = NTRAK
      IF (IGAR1.EQ.1 .OR. IGAR2.EQ.1) THEN
        KETP1  = IW(NAETP1)
        KEMSK  = IW(NAEMSK)
      ENDIF
      ITMSK = 0
C
C-    Define pointers
C
C-    Check that banks needed for analysis exist
C
      KECLU = IW (NAECLU)
      IF ( KECLU.EQ.0 ) GOTO 999
      NECLU = LROWS(KECLU)
      IF ( NECLU.EQ.0 ) GOTO 999
C
      KESDA = IW ( NAESDA )
      IF ( KESDA.EQ.0 ) GOTO 999
C
      KETP1 = IW ( NAETP1 )
C
      KERL3 = IW ( NAERL3 )
C
C-    Set pointers to ECTY bank
C
      KECTY  = IW(NAECTY)
C
C-    Loop on each cluster
C
C
      KECT1 = IW ( NAECT1 )
      IF( KECT1 .EQ. 0 )   GOTO 200
C
      DO 100 ICL = 1,NECLU
        JECLU = KROW (KECLU,ICL)
C
C-    Check if it is a charged cluster
C     then
C
C-        find cluster - track relation
C         if it is not an electron then
C
C-        initialize local buffer
C
C         extrapolate track in 5 stack (3 EM, 2Hadron )
C         calculate energy in road for charged clusters
C         Search for neutral (non.EM) cluster in 1.5 radius from road
C         if any then
C              estimate energy in the initial road
C         update EMSK , PRTM bank
C
C
C-    Select charged cluster
C
        ITYPC = ITABL(KECTY, ICL,JECTCT)
        IF ( ITYPC.GT.1) THEN
C
C-    Look for all tracks related to this cluster
C
          JECT1 = KROW ( KECT1 , ICL )
          NERL3 = IW ( JECT1+JECTER)
   12     IF ( NERL3.NE.0 ) THEN
C
C-    Track loop in a cluster
C
            JERL3 = KROW ( KERL3,NERL3)
            NETP1 = IW   ( JERL3 + JERLET)
            JETP1 = KROW ( KETP1,NETP1)
C
C-     Verify track extrapolation
C
            IF (IW(JETP1+JETPEF).EQ.0.OR.RW(JETP1+JETPCH).EQ.0) THEN
              GO TO 80
            ENDIF
C
C-     Check track quality
C
            CALL ECHTAG(NETP1,ITAG,DUMMY)
            IF (MOD(ITAG,100).NE.1.AND.MOD(ITAG,100).NE.3) THEN
              GO  TO 80
            ENDIF
            JPRTM = KROW (KPRTM,NETP1)
C
C-    Extrapolate track
C
            CALL ECTRAP ( NETP1 , XTRAP , ITRAP , XCOS )
C
C-    Pointer to EMSK bank
C
            IF ( IW(JPRTM+JPRTMA).EQ.0) THEN
              ITMSK = ITMSK +1
              IW ( JPRTM+JPRTMA) = ITMSK
              IW ( KEMSK + LMHROW ) = IW (KEMSK+LMHROW)+1
            ENDIF
            IPMSK = IW ( JPRTM + JPRTMA )
            JEMSK = KROW(KEMSK,IPMSK)
C
C-    In case of low momentum track takes track
C
            IF(ITAG.EQ.101.OR.ITAG.EQ.103) GO TO 80
C
C-    Try to identify electron
C
            IF ( ITYPC.EQ.2.OR.ITYPC.EQ.3) THEN
              CALL ECTREA(NETP1,ICL,IJ)
              IW(JPRTM+JPRTCT) = IJ
              GO TO 80
            ENDIF
C
C-    Try to identify MUON
C
C
            CALL EMSKMU( NETP1, XTRAP, EMIP, IMUON)
            IF ( IMUON.NE.0) THEN
C
C-    Muon candidate
C-    store energy along track
C
              IW ( JPRTM+JPRTCT) = 2
              CALL EMFILL(IPMSK,EMIP,XCOS,ITRAP)
              GO TO 80
            ENDIF
C
C-    Loop on cluster storeys
C
            NESDA = IW ( JECLU +JECLES )
            PMON = RW ( JETP1+JETPPT )
   22       JESDA = KROW (KESDA,NESDA)
            IS   = IW ( JESDA + 3 )
            CALL EROAD ( NESDA,PMON,XTRAP(1,IS),ITRAP(1,IS),
     +     XXX(1))
C
C-    Store energy in road for charged cluster
C
C-    Loop on regions
C
            DO 65 KK = 1,2
              INCL = (KK-1) * 15 + (IS-1) * 3
              KI = ( KK-1) * 3
              CALL UCOPY ( RW(JEMSK+INCL+1),XV(1),3)
C
C-    Add to EMSK what has been found inside the mask
              CALL EMSKF ( XV,XXX(KI+1))
C-    Copy it back to EMSK
              CALL UCOPY ( XV(1),RW(JEMSK+INCL+1),3)
   65       CONTINUE
C
C-    End loop on storeys of charged cluster
C
            NESDA = IW ( JESDA+JESDES )
            IF ( NESDA.NE.0 ) GO TO 22
C
C-    Search neutral cluster nearby
C
            DO 70 INC = 1,NECLU
              JECTY1 = KROW(KECTY,INC)
              IF(IW(JECTY1+JECTCT).EQ.0 ) GO TO 70
              IF ( ICL.EQ.INC ) GO TO 70
              JNCLU = KROW(KECLU,INC)
              JNCT1 = KROW(KECT1,INC)
              IGOO = 0
              IF ( ITRAP(1,2).NE.0) THEN
                CALL ENPROX(INC,XTRAP(1,2),IGOO)
              ENDIF
              IF (IGOO.EQ.0) GO TO 70
C
C-    Neutral cluster nearby , select storey in road
C
              NESDA = IW ( JNCLU +JECLES )
              PMON = RW ( JETP1+JETPPT )
   71         JESDA = KROW (KESDA,NESDA)
              IS   = IW ( JESDA + 3 )
              CALL VZERO (XXX(1),7)
              CALL EROAD ( NESDA,PMON,XTRAP(1,IS),ITRAP(1,IS),
     +      XXX(1))
C
C-    Update EMSK bank for current track
C
              DO 72 KK = 1,2
                INCL = (KK-1) * 15 + (IS-1) * 3
                KI = ( KK-1) * 3
                CALL UCOPY ( RW(JEMSK+INCL+1),XV(1),3)
                CALL EMSKF(XV,XXX(KI+1))
                CALL UCOPY ( XV(1),RW(JEMSK+INCL+1),3)
   72         CONTINUE
C
C-    End loop on neutral cluster storeys
C
              NESDA = IW ( JESDA+JESDES )
              IF ( NESDA.NE.0 ) GO TO 71
   70       CONTINUE
C
C
C-    Calculate hadronic calorimeter energy in road
C
            CALL HROAD(PMON,XTRAP(1,4),ITRAP (1,4),NETP1,XCOS(1,4))
C
C-    Loop on next track in the same cluster
C
   80       CONTINUE
          ENDIF
          NERL3 = IW ( JERL3 + 2 )
          IF ( NERL3 . NE. 0 ) GO TO 12
        ENDIF
C
C-    End loop on clusters
C
  100 CONTINUE
  200 CONTINUE
C
C-    Build & fill PMSK bank
C
      CALL PMSKFI
      CALL AUBPRS('ECTEHCTE')
C
  998 CONTINUE
      RETURN
  999 CONTINUE
      IER = 1
      RETURN
      END
#endif
