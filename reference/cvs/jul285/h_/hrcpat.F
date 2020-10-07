      SUBROUTINE HRCPAT(NPAT0)
C*************************************************************
C! Finds digital patterns                                    *
C!                                                           *
C! Author  : G. BAGLIESI   880119                            *
C!                                                           *
C! VERSION 1.0                                               *
C!                                                           *
C! Input   HTUB BANK                                         *
C! Output  HTPA FAMILY OF BANKS                              *
C!                                                           *
C! NPAT0=NUMBER  OF PATTERNS                                 *
C!                                                           *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "hwbnkw.h"
      LOGICAL HPLINK
#include "rlunit.h"
#include "htpajj.h"
#include "htubjj.h"
      LOGICAL FIRST
      DATA FIRST/.TRUE./
#include "bmacro.h"
C
C  CLUSTERS RECONSTRUCTION
C
      IF(FIRST) THEN
         FIRST=.FALSE.
         ICLIHW=0
         IPLIHW=0
      ENDIF
      NPAT0 = 0
      IHTUB=NLINK('HTUB',0)
      IF(IHTUB.EQ.0) THEN
         GOTO 80
      ENDIF
      NFIRE=LROWS(IHTUB)
      IF(NFIRE.EQ.0) GOTO 80
      IF(NFIRE.GE.1000) THEN
         CALL REPORT('HRCPAT','TOO MANY HITS '//
     &       'NO DIGITAL PATTERN RECONSTRUCTION',0)
         GOTO 80
      ENDIF
C
C   THE PATTERN WORKING BANK ICLIHW IS CREATED
C
      CALL WBANK(IW,ICLIHW,NFIRE,*90)
      DO 17 IX=1,NFIRE
        IW(ICLIHW+IX) = 0
  17  CONTINUE
C  ----
C  ----  Loop on different modules
C  ----
      NPATT=0
      I1LIM=1
      I2LIM=NFIRE
      DO 30 JJS=I1LIM,I2LIM
         ISUB1=ITABL(IHTUB,JJS,JHTUSN)
         IMOD1=ITABL(IHTUB,JJS,JHTUMN)
         ILAY1=ITABL(IHTUB,JJS,JHTULN)
         XCLU1=RTABL(IHTUB,JJS,JHTULC)
         WCLU1=RTABL(IHTUB,JJS,JHTUCW)
         IF((ISUB1.EQ.1.OR.ISUB1.EQ.3).AND.ILAY1.GT.22) GOTO 30
         IF(IW(ICLIHW+JJS).EQ.0) THEN
            NPATT=NPATT+1
            IW(ICLIHW+JJS)=NPATT
         ENDIF
C        protection added by J.Knobloch
          IF(JJS.EQ.I2LIM)GOTO 30
         DO 20 JJSS=JJS+1,I2LIM
            ISUB2=ITABL(IHTUB,JJSS,JHTUSN)
            IMOD2=ITABL(IHTUB,JJSS,JHTUMN)
            ILAY2=ITABL(IHTUB,JJSS,JHTULN)
            XCLU2=RTABL(IHTUB,JJSS,JHTULC)
            WCLU2=RTABL(IHTUB,JJSS,JHTUCW)
            IF((ISUB2.EQ.1.OR.ISUB2.EQ.3).AND.ILAY2.GT.22) GOTO 20
            IF(HPLINK(ISUB1,IMOD1,ILAY1,XCLU1,WCLU1, ISUB2,IMOD2,ILAY2,
     +      XCLU2,WCLU2)) THEN
               IF(IW(ICLIHW+JJSS).EQ.0.OR. IW(ICLIHW+JJSS).EQ.
     +         IW(ICLIHW+JJS)) THEN
                  IW(ICLIHW+JJSS)=IW(ICLIHW+JJS)
               ELSE
                  ITEMP=IW(ICLIHW+JJS)
                  DO 10 KPI=I1LIM,I2LIM
                     IF(IW(ICLIHW+KPI).EQ.ITEMP) IW(ICLIHW+KPI)=
     +               IW(ICLIHW+JJSS)
   10             CONTINUE
               ENDIF
            ENDIF
   20    CONTINUE
   30 CONTINUE
      IF(NPATT.EQ.0) GOTO 80
C -----
C -----  IPLIHW is the pointers bank
C -----
C  IW(IPLIHW+I)= IPATT * 1000 + # TUBESI LINKED TO IPATT
C
C
      CALL WBANK(IW,IPLIHW,NPATT,*90)
      DO 18 IX=1,NPATT
        IW(IPLIHW+IX) = 0
   18 CONTINUE
      DO 40 JPIP=1,NFIRE
C        protection added by J.Knobloch
         IF(IW(ICLIHW+JPIP).EQ.0)GOTO40
         IW(IPLIHW+IW(ICLIHW+JPIP))=IW(IPLIHW+IW(ICLIHW+JPIP)) + 1
   40 CONTINUE
      NPAT0=0
      DO 50 JPIP=1,NPATT
         IF(IW(IPLIHW+JPIP).NE.0) THEN
            NPAT0 = NPAT0 + 1
            IW(IPLIHW+JPIP) = JPIP*1000+IW(IPLIHW+JPIP)
         ELSE
            IW(IPLIHW+JPIP) = 1000000
         ENDIF
   50 CONTINUE
      CALL INTSOR(IW(IPLIHW+1),NPATT)
C
C  The HTPA banks are created
C
      CALL BLIST(IW,'E+','HTPA')
      DO 70 JPIP=1,NPAT0
         NR=JPIP
         ND=MOD(IW(IPLIHW+JPIP),1000) + LMHLEN
         ITEMP=IW(IPLIHW+JPIP)/1000
         CALL AUBOS('HTPA',NR,ND,IHTPA,IGARB)
         IF(IGARB.EQ.2) THEN
            CALL RERROR('HRCPAT',1,'INSUFF. SPACE TO CREATE HTPA')
            GOTO 80
         ENDIF
         IW(IHTPA+LMHCOL) = LHTPAA
         IW(IHTPA+LMHROW) = 0
         DO 60 JPIP1=1,NFIRE
            IF(IW(ICLIHW+JPIP1).EQ.ITEMP) THEN
               IW(KNEXT(IHTPA)+1) = JPIP1
               IW(IHTPA+LMHROW) = IW(IHTPA+LMHROW) + 1
            ENDIF
   60    CONTINUE
   70 CONTINUE
   80 CONTINUE
      CALL WDROP(IW,ICLIHW)
      CALL WDROP(IW,IPLIHW)
      RETURN
   90       CALL RERROR('HRCPAT',2,'INSUFF.SPACE TO CREATE WORK BANKS')
      CALL WDROP(IW,ICLIHW)
      CALL WDROP(IW,IPLIHW)
      RETURN
      END
#endif
