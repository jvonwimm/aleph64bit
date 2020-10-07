      SUBROUTINE QUCLUS(ityp)
C--------------------------------------------------------------------
C! Clustering based on the angular distance
C
C  Input : ITYP     : passed, =1 for photons, =2 for hadrons
C          VHN, VPH : /PARCUT/ the four momenta
C          ICLU     : /HADPHO/ the pre-cluster numbering
C                              as done in CLUSTER
C
C  Output: NCLU     : /HADPHO/ the number of clusters
C          VCLU     : /HADPHO/ the four-momenta
C
C  Patrick Janot -- 23 Nov 1993
C
C  Modified: Marumi Kado -- October 1997   
C
C--------------------------------------------------------------------
#include "parcut.h"
#include "fsimid.h"
#include "hadpho.h"
#include "resolu.h"


      DIMENSION ecef(maxclu),ecal(maxclu),
     .          ent(maxclu),eef(maxclu)

      INTEGER indx(maxclu)
      REAL vobj(4,maxclu)

      IF ( ityp .EQ. 1 ) THEN
        nobj = nph
        IF ( nobj .GT. maxclu ) GOTO 998
        CALL ucopy(vph(1,1),vobj(1,1),4*nobj)
      ELSE
        nobj = nhn
        IF ( nobj .GT. maxclu ) GOTO 998
        CALL ucopy(vhn(1,1),vobj(1,1),4*nobj)
      ENDIF
C
C Assign a cluster number to each object
C
      DO iobj = 1, nobj
        CALL ucopy(vobj(1,iobj),vec(1),4)
        CALL cluster(vec,iobj,agcut(ityp))
      ENDDO
C
C Initialize output variables
C
      nclu = 0
      CALL vzero(vclu(1,1),4*maxclu)
      CALL vzero(echa(1)  ,  maxclu)
      call vzero(nmoclu, maxclu)
      call vzero(moclu, maxclu*maxclu)
      call vzero(fclu,maxclu*maxclu)
      call vzero(ecal(1),maxclu)
      call vzero(ent(1),maxclu)
      call vzero(eef(1),maxclu)
C      call vzero(propec(1),maxclu)
C
C Classify the cluster numbers in decreasing order
C
      CALL sortzv(iclu,indx,nobj,-1,1,0)

      IF ( idbg .GE. 10 ) THEN
        IF ( ityp .EQ. 1 ) THEN
          WRITE(6,*) 'Liste des photons a clusteriser : '
        ELSE
          WRITE(6,*) 'Liste des hadrons a clusteriser : '
        ENDIF
        DO iobj = 1, nobj
          jobj = indx(iobj)
          WRITE(6,*) iobj,iclu(jobj),(vobj(i,jobj),i=1,4)
        ENDDO
      ENDIF
      mclu = 0
      DO 1 iobj = 1, nobj
        jobj = indx(iobj)
        lclu = iclu(jobj)
        IF ( lclu .EQ. 0 ) GOTO 2
        IF ( lclu .NE. mclu ) THEN
          IF ( nclu .NE. 0 ) THEN
          IF ( (vclu(4,nclu)) .GT. 
     .       ((1.*(1.-eef(nclu))+.5*eef(nclu))*
     .       SQRT(echa(nclu))) .and. echa(nclu) .ne.0) THEN
             iflufs = iflufs + 1
          ENDIF

          IF ( (vclu(4,nclu)) .LE. 
     .       ((1.*(1.-eef(nclu))+.5*eef(nclu))*
     .       SQRT(echa(nclu)))) THEN
              CALL vzero(vclu(1,nclu),4)
              call vzero(moclu(1,nclu),maxclu)
              call vzero(fclu(1,nclu),maxclu)
              nmoclu(nclu) = 0
              echa(nclu) = 0.
              eef(nclu) = 0.
              ent(nclu) = 0.
              ecal(nclu) = 0.
              nclu = nclu - 1
           ENDIF
          ENDIF
          nclu = nclu + 1
          mclu = lclu
        ENDIF

        IF ( vobj(4,jobj) .LT. 0. ) THEN
           echa(nclu) = echa(nclu) - vobj(4,jobj)
        ELSEIF (ityp .NE. 1) THEN
           ecal(nclu)=ecal(nclu)+propec(jobj)*vobj(4,jobj)
           ent(nclu)=ent(nclu)+vobj(4,jobj)
           eef(nclu)=ecal(nclu)/ent(nclu)
        ENDIF

        DO 3 i = 1, 4
 3        vclu(i,nclu) = vclu(i,nclu) + vobj(i,jobj)
        DO IMO = 1, NMOCLU(NCLU)
            IF (MOCLU(IMO,NCLU).EQ.MOBJ(JOBJ))THEN
                FCLU(IMO,NCLU) = FCLU(IMO,NCLU)+VOBJ(4,JOBJ)
                GO TO 35
            ENDIF
        ENDDO
        nmoclu(nclu) = nmoclu(nclu) + 1
        IMO = NMOCLU(NCLU)
        moclu(nmoclu(nclu),nclu) = mobj(jobj)
        fclu(nmoclu(nclu),nclu) = vobj(4,jobj)
  35    CONTINUE
        IF ( idbg .GE. 15 ) THEN
          WRITE(6,*) 'Cluster # ',lclu,vclu(4,nclu),echa(nclu),
     &      imo,moclu(imo,nclu),fclu(imo,nclu), ' %ECAL:',eef(nclu)
        ENDIF
 1    CONTINUE

          IF ( (vclu(4,nclu)) .GT.
     .       ((1.*(1.-eef(nclu))+.5*eef(nclu))*
     .       SQRT(echa(nclu))) .and. echa(nclu) .ne.0) THEN
             iflufs = iflufs + 1
          ENDIF

 2    IF ( (vclu(4,nclu)) .LE.
     .     ((1.*(1.-eef(nclu))+.5*eef(nclu))
     .     *SQRT(echa(nclu))) ) THEN
        CALL vzero(vclu(1,nclu),4)
        echa(nclu) = 0.
        call vzero(moclu(1,nclu),maxclu)
        call vzero(fclu(1,nclu),maxclu)
        nmoclu(nclu) = 0
        eef(nclu) = 0.
        ecal(nclu) = 0.
        ent(nclu) = 0.
        nclu = nclu - 1
      ENDIF

C
C Scale the cluster momenta to a zero mass
C
      DO 4 jclu = 1, nclu
       scale = vclu(4,jclu)
     .       / SQRT ( vclu(1,jclu)**2 +
     .                vclu(2,jclu)**2 +
     .                vclu(3,jclu)**2 )
        DO 5 i = 1, 3
    5   vclu(i,jclu)= scale * vclu(i,jclu)
    4 CONTINUE
      IF ( idbg .GE. 10 ) THEN
        IF ( ityp .EQ. 1 ) THEN
          WRITE(6,*) 'Liste des photons   clusterises : '
        ELSE
          WRITE(6,*) 'Liste des hadrons   clusterises : '
        ENDIF
        DO jclu = 1, nclu
          WRITE(6,*) jclu,(vclu(i,jclu),i=1,4),echa(jclu)
          do imo = 1, nmoclu(jclu)
            write(6,*) '   Mc particle:',moclu(imo,jclu),
     &          ' Energy:',fclu(imo,jclu), ' %ECAL:',eef(jclu)
          enddo
        ENDDO
      ENDIF
C
      GOTO 999
  998 WRITE(6,*) '+++ QUCLUS +++ Too many objects : ',nobj
      CALL qmterm('Stop execution - Please enlarge dimensions')
C
  999 RETURN
      END








