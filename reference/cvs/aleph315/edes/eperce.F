         SUBROUTINE EPERCE(P,V,TYPE,N,IMPACT,INSIDE)
C ------------------------------------------------------------
CKEY ECALDES IMPACT VOLUME TYPE / USER
C     H Videau    decembre 88
C! Impacts on an Ecal volume type
C  This routine computes the N impacts in the assigned volume
C  type of a straight line coming from point P along vector V.
C  Input :
C          P   point               REAL(3)
C          V   vector              REAL(3)
C          TYPE volume type        CHARACTER*16
C                TYPE= 'sensitive','stack1','stack2','stack3',
C                      'external'
C  Output:
C          N number of impacts  <5   INTEGER
C          IMPACT points of impact   REAL(3,N)
C          INSIDE flag               LOGICAL
C               .TRUE. if P inside volume
C   Calls ECYLND, EFNDSC, EFNDMD, ETOUCH
C   Called by USER.
C------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C  Input
         REAL P(3),V(3)
         CHARACTER*(*) TYPE
C  Output
         REAL IMPACT(3,*)
         INTEGER N
         LOGICAL INSIDE
C Locales
         INTEGER SC,MD,CMD,I,J,K,LEPLAN,ITYPE
         REAL PLANES(4,10),PIN(3)
         LOGICAL RIEN,DEDANS
         CHARACTER*16 VOLNAM(ECALSC,5),VOLTYP(5)
C external
         INTEGER EFNDSC,EFNDMD
C initialisation
         DATA VOLTYP/
     & 'sensitive','stack1','stack2','stack3','external'/
         DATA VOLNAM/
     & 'E sensitive','B sensitive','E sensitive',
     & 'E stack1','B stack1','E stack1',
     & 'E stack2','B stack2','E stack2',
     & 'E stack3','B stack3','E stack3',
     & 'E external','B external','E external'/
C  execution
          DO 3 I=1,5
          IF(TYPE.EQ.VOLTYP(I))THEN
                ITYPE=I
                GO TO 4
                END IF
 3       CONTINUE
 4       CONTINUE
C
         INSIDE=.FALSE.
         CALL ECYLND(200.,270.,P,V,IMPACT)
         SC=EFNDSC(IMPACT)
         MD=EFNDMD(SC,IMPACT)
         N=0
         IF(IMPACT(3,1).GT.0) THEN
                          SC=1
                              ELSE
                          SC=2
                              END IF
         DO 1 I=SC,SC+1
         DO 2 J=MD-1,MD+1
         CMD=J
         IF(J.EQ.0)CMD=12
         IF(J.EQ.13)CMD=1
         CALL EVOLPL(VOLNAM(SC,ITYPE),I,CMD,LEPLAN,PLANES)
         CALL ETOUCH(P,V,LEPLAN,PLANES,PIN,RIEN,DEDANS)
         INSIDE=INSIDE.OR.DEDANS
         IF(RIEN) GOTO 2
         IF(N.LT.5)N=N+1
         IMPACT(1,N)=PIN(1)
         IMPACT(2,N)=PIN(2)
         IMPACT(3,N)=PIN(3)
 2       CONTINUE
 1       CONTINUE
 999     CONTINUE
         END
#endif
