      SUBROUTINE HDGDEB
C*************************************************************
C! Digital Pattern debugging                                 *
C!                                                           *
C! AUTHOR :  G. Bagliesi      880511                         *
C!                                                           *
C! VERSION 1.1  890126                                       *
C!                                                           *
C! INPUT   HPCO HTUB                                         *
C!                                                           *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "alcons.h"
#include "hpcojj.h"
#include "hpdsjj.h"
#include "htpajj.h"
#include "htubjj.h"
#include "rlunit.h"
#include "bmacro.h"
C
      IHPCO=NLINK('HPCO',0)
      IF(IHPCO.EQ.0) THEN
         WRITE(LDEBRL,*) ' HDGDEB : HPCO BANK NOT FOUND'
         GOTO 999
      ENDIF
      IHTUB=NLINK('HTUB',0)
      IF(IHTUB.EQ.0) THEN
         WRITE(LDEBRL,*) ' HDGDEB : HTUB BANK NOT FOUND'
         GOTO 999
      ENDIF
      CALL PRTABL('HTUB',0)
      NHTUB = LROWS(IHTUB)
      NPATT = LROWS(IHPCO)
C
      WRITE (LDEBRL,*)
      WRITE (LDEBRL,*) ' HDGDEB : NPATT = ',NPATT
      DO 100 I=1,NPATT
         LHTPA  = ITABL(IHPCO,I,JHPCHT)
         LHPDS  = ITABL(IHPCO,I,JHPCHP)
         IHPDS  = NLINK('HPDS',LHPDS)
         IF(IHPDS.EQ.0 ) THEN
            WRITE(LDEBRL,*) ' HDGDEB : HPDS ',LHPDS,' NOT FOUND '
         ENDIF
      WRITE (LDEBRL,*) '   '
      WRITE (LDEBRL,*) ' HDGDEB : -------- PATTERN # ',I,' --------'
      WRITE (LDEBRL,*) ' HDGDEB : LHTPA = ',LHTPA
      WRITE (LDEBRL,*)
      WRITE (LDEBRL,*) ' HDGDEB : ITUB        ISUBC       MODBA     ',
     &                  '  ILAY        XHIT        CLUW        JHWDI '
      DO 109 JI=1,NHTUB
        IPAT  = ITABL(IHTUB,JI,JHTUPP)
        IF(IPAT.EQ.I) THEN
           ISUBC = ITABL(IHTUB,JI,JHTUSN)
           MODBA = ITABL(IHTUB,JI,JHTUMN)
           ILAY  = ITABL(IHTUB,JI,JHTULN)
           XHIT  = RTABL(IHTUB,JI,JHTULC)
           CLUW  = RTABL(IHTUB,JI,JHTUCW)
           JHWDI = ITABL(IHTUB,JI,JHTUPR)
           WRITE (LDEBRL,*) JI  ,ISUBC,MODBA,ILAY,XHIT,CLUW,JHWDI
        ENDIF
 109  CONTINUE
      NHPDS = LROWS(IHPDS)
      WRITE(LDEBRL,*) ' HDGDEB : LHPDS, # OF LAYERS ',LHPDS,NHPDS
      DO 119 III=1,NHPDS
        KLAYE = ITABL(IHPDS,III,JHPDNL)
        FLENG = RTABL(IHPDS,III,JHPDFL)
        DMAXX = RTABL(IHPDS,III,JHPDMD)
        WRITE(LDEBRL,*) ' LAYER, FIRED LENGTH, MAXIMUM DISTANCE'
     &                   ,KLAYE,FLENG,DMAXX
 119  CONTINUE
         IDIR   = ITABL(IHPCO,I,JHPCDI)
         EDIG   = RTABL(IHPCO,I,JHPCDE)
         COR1   = RTABL(IHPCO,I,JHPCC1)
         COR2   = RTABL(IHPCO,I,JHPCC2)
         NHIT   = ITABL(IHPCO,I,JHPCND)
         INTP   = ITABL(IHPCO,I,JHPCIP)
         IPL    = ITABL(IHPCO,I,JHPCFP)
         LPL    = ITABL(IHPCO,I,JHPCLP)
         DMAX   = RTABL(IHPCO,I,JHPCMD)
         DEN    = RTABL(IHPCO,I,JHPCPD)
         CENX   = RTABL(IHPCO,I,JHPCCX)
         CENY   = RTABL(IHPCO,I,JHPCCY)
         IBORD  = ITABL(IHPCO,I,JHPCBF)
         LHTPA  = ITABL(IHPCO,I,JHPCHT)
         LHPDS  = ITABL(IHPCO,I,JHPCHP)
         WRITE (LDEBRL,*) ' HDGDEB : IDIR, IBORD    = ',IDIR,IBORD
         WRITE (LDEBRL,*) ' HDGDEB : EDIG           = ',EDIG
         WRITE (LDEBRL,*) ' HDGDEB : COR1           = ',COR1
         IF(IDIR.EQ.1) THEN
           COR2= COR2 * 180/PI
           SCOR2 = SCOR2 * 180/PI
         ENDIF
         WRITE (LDEBRL,*) ' HDGDEB : COR2           = ',COR2
         WRITE (LDEBRL,*) ' HDGDEB : NHIT           = ',NHIT
         WRITE (LDEBRL,*) ' HDGDEB : INTP, IPL, LPL = ',INTP,IPL,LPL
         WRITE (LDEBRL,*) ' HDGDEB : DMAX, DEN      = ',DMAX,DEN
         WRITE (LDEBRL,*) ' HDGDEB : CENX           = ',CENX
         WRITE (LDEBRL,*) ' HDGDEB : CENY           = ',CENY
 100  CONTINUE
 999  RETURN
      END
#endif
