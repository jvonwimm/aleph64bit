C This set of routines replace EBRID.FOR which is now obsolete
      SUBROUTINE ELCOR(IFRFT,PX,PY,PZ,NGOOD)
C Author         A. Bonissent  1991
C Modification   P. Schwemling 1993
C Modification   D. Rousseau   1995
C INPUT IFRFT FRFT track number
C INPUT/OUTPUT  PX,PY,PZ the 3-momentum of the Bremsstrahlung photon(s) is
C                         added to PX,PY,PZ
C         NGOOD    number of photon found
C For more information on the photon
C EBRID should be called directly  (see below)
      LOGICAL FIRST                                                     ELCOR  3
      DATA FIRST /.TRUE./                                               ELCOR  4
CDR
      DIMENSION PGAM(3)
      INTEGER NGOOD
      REAL BRELE(3),BRPHO(10,2)

      logical mini
C*CD BCS                                                                ELCOR  6
      INTEGER LMHLEN, LMHCOL, LMHROW                                    ELCOR  7
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          ELCOR  8
C                                                                       ELCOR  9
      COMMON /BCS/   IW(1000)                                           ELCOR 10
      INTEGER IW                                                        ELCOR 11
      REAL RW(1000)                                                     ELCOR 12
      EQUIVALENCE (RW(1),IW(1))                                         ELCOR 13
C                                                                       ELCOR 14
      IF(FIRST)THEN                                                     ELCOR 15
         FIRST=.FALSE.                                                  ELCOR 16
CSC        NAPEST=NAMIND('PEST')                                        ELCOR 17
CSC        CALL EBINIT(IER)                                             ELCOR 18
CSC         IF(IER.NE.0)THEN                                            ELCOR 19
CSC            WRITE(6,*)' EBINIT IER =',IER                            ELCOR 20
CSC            CALL EXIT                                                ELCOR 21
CSC         ENDIF                                                       ELCOR 22
      ENDIF                                                             ELCOR 23
C Check that we work on a DST file                                      ELCOR 24
CSC      KPEST=IW(NAPEST)                                               ELCOR 25
CSC      IF(KPEST.EQ.0)THEN                                             ELCOR 26
CSC         WRITE(6,*)' EBRID working on MINI '                         ELCOR 27
CSC        GO TO 999                                                    ELCOR 28
CSC      ENDIF                                                          ELCOR 29
        CALL EBRID(IFRFT,PGAM,NGOOD,BRPHO,BRELE)                        ELCOR 30
      PXG=0.
      PYG=0.
      PZG=0.
      IF(NGOOD.GT.0)THEN                                                ELCOR 31
         PXG=PGAM(1)                                                    ELCOR 32
         PYG=PGAM(2)                                                    ELCOR 33
         PZG=PGAM(3)                                                    ELCOR 34
      ENDIF                                                             ELCOR 38
         PX=PX+PXG                                                      ELCOR 35
         PY=PY+PYG                                                      ELCOR 36
         PZ=PZ+PZG                                                      ELCOR 37
  999 CONTINUE                                                          ELCOR 39
      RETURN                                                            ELCOR 40
      END                                                               ELCOR 41
C*DK EBRID
      SUBROUTINE EBRID(IFRFT,PGAM,NGOOD,BRPHO,BRELE)                    EBRID  2
C******************************************************************     EBRID  3
C Find bremstrahlung Photons                                            EBRID  4
C  INPUT     IFRFT      Julia track number
C  OUTPUT
C            PGAM(3) 3-MOMENTUM OF PHOTON(S) FOUND
C            NGOOD     NUMBER OF PHOTON FOUND
C                                                                       EBRID  7
C    BRELE(3) : Array of internal quantities for the electron           EBRID  8
C            1    AMIN  COS(min. angle between gamma and peco           EBRID 13
C                            (seen from origin))                        EBRID 14
C            2    AMAX  COS(max. angle between gamma and peco           EBRID 15
C                            (seen from origin))                        EBRID 16
C            3    Maximal Change in impact parameter due to photon emission
C
C    BRPHO(10,2)
C
C            1    AVAL  COS(measured angle between found gamma and peco)EBRID 17
C                            (seen from origin))                        EBRID 18
C            2    GAMPEC PHOTON number
C                                                                       EBRID 19
C******************************************************************     EBRID 20
C      INCLUDE 'PHYINC:QCDE.INC'
      REAL PXG(10),PYG(10),PZG(10)
      DIMENSION PGAM(3),PTR(8,3),D0Z0(2),BRPHO(10,2),BRELE(3)           EBRID 21
      REAL MAINV(3)
      REAL EGAMM,RADIUS,RADMAX
      LOGICAL HIST
      DATA HIST/.FALSE./
      INTEGER KRUNOLD/-10/,KEVTOLD/-10/
C      INCLUDE 'PHYINC:QMACRO.INC'

      IGTYP=0                                                           EBRID 22
Cif first time in the event  get main vertex
      CALL EVNUM(KEVT,KRUN)
      IF (KEVTOLD.NE.KEVT.OR.KRUNOLD.NE.KRUN) THEN
         KRUNOLD=KRUN
         KEVTOLD=KEVT
         CALL EVTVER(MAINV(1),MAINV(2),MAINV(3))
      ENDIF

C                                                                       EBRID 25
C PTR = track coordinates : X,Y,Z,U,V,W,P,CHARGE                        EBRID 26
C   at 3 points : 1 origin, 2 TPC entry (not used), 3 TPC exit          EBRID 27
C                                                                       EBRID 28
      CALL EPR1(IFRFT,PTR,D0Z0,IER)                                     EBRID 29
      IF(IER.NE.0)GO TO 999                                             EBRID 30
      CALL CMPNG(PTR,AMIN,AMAX,MAINV)
      BRELE(1)=AMIN                                                     EBRID 32
      BRELE(2)=AMAX                                                     EBRID 33
      IGTYP=0                                                           EBRID 47
      PXG(1)=0.
      PYG(1)=0.
      PZG(1)=0.
C DR MOVE ANGOOD INTO PCPABR
      CALL PCPABR(PTR,PXG,PYG,PZG,NGOOD,BRPHO,BRELE,MAINV)
      IF (NGOOD.EQ.0) THEN
         DO I=1,2
           BRPHO(1,I)=0.
         ENDDO
         CALL VZERO (BRELE,3)
                      ELSE

C calculate the deviation due to the photons
      RADMAX=-1E6
      EELE=PTR(7,1)
C find the radius of emission

        AMIN=ACOS(BRELE(1))
        AMAX=ACOS(BRELE(2))
       RADMAX=-1E5
       EGAMM=0.
       CALL VZERO (PGAM,3)
       DO IGOOD=1,NGOOD
          EGAMM=EGAMM+SQRT(ABS(PXG(IGOOD)**2+PYG(IGOOD)**2
     &                                      +PZG(IGOOD)**2))
          AVAL=ACOS(BRPHO(IGOOD,1))
          RADIUS=31.* (AMAX-AVAL)/(AMAX-AMIN)
          RADMAX=MAX(RADIUS,RADMAX)
         IF (HIST) CALL HFILL(550,RADIUS,0.0,1.0)
        PGAM(1)=PGAM(1)+PXG(IGOOD)
        PGAM(2)=PGAM(2)+PYG(IGOOD)
        PGAM(3)=PGAM(3)+PZG(IGOOD)
       ENDDO
C decide if radiation in vdet or ITC/TPC
        IF (RADMAX.GE.20) THEN
                          RADIUS=31.
                         ELSE
                          RADIUS=13.
                         ENDIF
C calculate change in impact parameter

        DEVIA=0.5*0.0045*RADIUS**2*EGAMM
     &    /((EGAMM+EELE)*EELE*SQRT(1.-PTR(6,1)**2) )
        BRELE(3)=DEVIA

                      ENDIF    !NGOOD


  999 CONTINUE                                                          EBRID 71
      RETURN                                                            EBRID 72
      END                                                               EBRID 73
C*HE 01/21/93 17:48:22 S
*DK PCPABR
CDR
      SUBROUTINE PCPABR(PTR,PXGF,PYGF,PZGF,NGOOD,BRPHO,BRELE,MAINV)
      DATA TMNIS /0.95/
      DATA CUMIN /0.0016/,CUMAX /-0.002/, CUTST /-1.0/

      LOGICAL FIRST
      DATA FIRST /.TRUE./
      LOGICAL HIST
      DATA HIST /.FALSE./

      DIMENSION PTR(8,*)
      REAL TETHAG, PHIG
      REAL MAINV(3)
      REAL PXGF(10),PYGF(10),PZGF(10)
      DIMENSION BRPHO(10,2),BRELE(3)
      REAL TRUC, TRUC2,DELTA
C
C*CD PECOJJ
      PARAMETER(JPECER=1,JPECE1=2,JPECE2=3,JPECTH=4,JPECPH=5,JPECEC=6,
     +          JPECKD=7,JPECCC=8,JPECRB=9,JPECPC=10,LPECOA=10)
C*CD PCPAJJ
      PARAMETER(JPCPNA=1,JPCPEN=2,JPCPTE=3,JPCPFI=4,JPCPR1=5,JPCPR2=6,
     +          JPCPPC=7,LPCPAA=7)
C*CD ETP1JJ
      PARAMETER(JETPX3=1,JETPP3=4,JETPPT=7,JETPCH=8,JETPEF=9,LETP1A=9)
C*CD BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C*CD BMACRO        SET OF INTRINSIC FUNCTIONS TO HANDLE BOS BANKS
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
      IF(FIRST)THEN
         FIRST=.FALSE.
         NAPGAC=NAMIND('PGAC')
         NAPGPC=NAMIND('PGPC')
         NAEGPC=NAMIND('EGPC')
         KBRCU=IW(NAMIND('BRCU'))
         IF(KBRCU.NE.0)THEN
              TMNIS=RW(KBRCU+2)
              CUMIN=RW(KBRCU+3)
              CUMAX=RW(KBRCU+4)
              CUTST=RW(KBRCU+5)
         ENDIF
         KEGPC=IW(NAEGPC)
         NEGPC=LROWS(KEGPC)
         KPGPC=IW(NAPGPC)
         NPGPC=LROWS(KPGPC)
         KPGAC=IW(NAPGAC)
         NPGAC=LROWS(KPGAC)
         IF (KPGAC.ne.0) THEN
                WRITE(*,*) 'EBRID initialisation : use PGAC'
         ELSEIF (KPGPC.ne.0) THEN
                WRITE(*,*) 'EBRID initialisation : use PGPC'
         ELSEIF (KPGAC.NE.0) THEN
                WRITE(*,*) 'EBRID initialisation : use EGPC'
         ENDIF

         IF (HIST) THEN
       CALL HBOOK1(500,' TEST ',200,0.96,1.,0.,0)
       CALL HBOOK1(510,' AVAL ',200,0.96,1.,0.,0)
       CALL HBOOK1(540,' AVAL APRES TEST',200,0.96,1.,0.,0)
       CALL HBOOK2(520,' TEST VS AVAL ',200,0.96,1.,200,0.96,1.,0.,0)
       CALL HBOOK1(530,' AVAL TRUC',200,-10.,10.,0.,0)
       CALL HBOOK1(531,' AVAL TRUC AVEC MARGE',200,-10.,10.,0.,0)
       CALL HBOOK1(550,' RADIUS ',100, -50.,50.,0.,0.)
         ENDIF

      ENDIF
      NEGPC=0
      NPGPC=0
      NPGAC=0

      KEGPC=IW(NAEGPC)
      IF (KEGPC.NE.0) NEGPC=LROWS(KEGPC)

      KPGPC=IW(NAPGPC)
      IF (KPGPC.NE.0) NPGPC=LROWS(KPGPC)

      KPGAC=IW(NAPGAC)
      IF (KPGAC.NE.0) NPGAC=LROWS(KPGAC)

      NPHOT=0
      IF (KPGAC.ne.0) THEN
               NPHOT=NPGAC
      ELSEIF (KPGPC.ne.0) THEN
               NPHOT=NPGPC
      ELSEIF (KEGPC.NE.0) THEN
               NPHOT=NEGPC
      ENDIF

C

      TESTM=0.

CDR
Cuse PGAC if it exit, otherwise PGPC if it exist, otherwise use EGPC

      AMIN=BRELE(1)
      AMAX=BRELE(2)
      NGOOD=0

      DO 10 IPHOT=1,NPHOT

CDR now we use PGAC if it is available ("new" MINI10 (January 1995))
      IF (KPGAC.NE.0) THEN
            eg    =rtabl(kpgac,iphot,1)
            thetag=rtabl(kpgac,iphot,2)
            phig  =rtabl(kpgac,iphot,3)

CDR now we use PGPC if it is available ("new" MINI (Aug 1993))
      ELSEIF (KPGPC.NE.0) THEN
            eg    =rtabl(kpgpc,iphot,1)
            thetag=rtabl(kpgpc,iphot,2)
            phig  =rtabl(kpgpc,iphot,3)
      ENDIF
            ug=sin(thetag)*cos(phig)
            vg=sin(thetag)*sin(phig)
            wg=cos(thetag)
            pxg=ug*eg
            pyg=vg*eg
            pzg=wg*eg

      IF (KPGPC+KPGAC.EQ.0) THEN      !  "old" MINI
            pxg=rtabl(kegpc,iphot,1)
            pyg=rtabl(kegpc,iphot,2)
            pzg=rtabl(kegpc,iphot,3)
C We need ug,wg,vg
            eg=sqrt(pxg*pxg+pyg*pyg+pzg*pzg)
            ug=pxg/eg
            vg=pyg/eg
            wg=pzg/eg
             ENDIF

      UTR=PTR(4,1)
      VTR=PTR(5,1)
      WTR=PTR(6,1)
      PSC=UG*UTR+VG*VTR+WG*WTR
      IF(PSC.LT.0.97)GO TO 10

      RTR=SQRT(PTR(1,3)**2+PTR(2,3)**2+PTR(3,3)**2)
CDR  add MAINV
      XFG=UG*RTR+MAINV(1)
      YFG=VG*RTR+MAINV(2)
      ZFG=WG*RTR+MAINV(3)


C      XFG=UG*RTR
C      YFG=VG*RTR
C      ZFG=WG*RTR
      CALL FNDACC(PTR,ACCX,ACCY,ACCZ)
      DX=PTR(1,3)-XFG
      DY=PTR(2,3)-YFG
      DZ=PTR(3,3)-ZFG
      DMOD2=DX**2+DY**2+DZ**2
      DPERP=DX*PTR(4,3)+DY*PTR(5,3)+DZ*PTR(6,3)
      DPAR=SQRT(DMOD2-DPERP**2)
      COEF=1./DPAR
      DX=DX*COEF
      DY=DY*COEF
      DZ=DZ*COEF
      TEST=DX*ACCX+DY*ACCY+DZ*ACCZ

      AMOD=SQRT(PTR(1,3)**2+PTR(2,3)**2+PTR(3,3)**2)*
     $  SQRT(PXG**2+PYG**2+PZG**2)
      AVAL=(PTR(1,3)*PXG+PTR(2,3)*PYG+PTR(3,3)*PZG)/AMOD
      IF (AMAX.GT.1.) THEN
                       CALL QWMESE ('WARNING EBRID big AMAX')
                       WRITE(*,*) AMAX
                       AMAX=1.
                      ENDIF
      AMTST=(AVAL-AMAX)/SQRT(1.-AMAX**2)

Csome histogram
      IF (HIST) CALL HFILL(500,TEST,0.0,1.0)
      IF (HIST) CALL HFILL(510,AVAL,0.0,1.0)
      IF (HIST) CALL HFILL(520,AVAL,TEST,1.0)
      IF(TEST.LT.TMNIS)GO TO 10
      IF (HIST) CALL HFILL(540,AVAL,0.0,1.0)

      IF (AMIN-AMAX.NE.0.) THEN
        TRUC=(AVAL-AMAX)/(AMIN-AMAX)
                      ELSE
        TRUC=-8.
                      ENDIF
      DELTA=AMIN+CUMIN-(AMAX+CUMAX)
      IF (DELTA.NE.0.) THEN
        TRUC2=(AVAL-(AMAX+CUMAX))/DELTA
                      ELSE
        TRUC2=-8.
                      ENDIF
      IF (HIST) CALL HFILL(530,TRUC,0.0,1.0)
      IF (HIST) CALL HFILL(531,TRUC2,0.0,1.0)




      IF (AVAL-AMAX.LT.CUMAX.OR.AVAL-AMIN.GT.CUMIN) GO TO 10

      IF(AMTST.LT.CUTST)GO TO 10
      NGOOD=NGOOD+1
      IF (NGOOD.GT.10) THEN
         WRITE(*,*)'WARNING EBRID more than 10 brems photon'
         NGOOD=10
                       ENDIF
      BRPHO(NGOOD,1)=AVAL
      BRPHO(NGOOD,2)=IPHOT

      PXGF(NGOOD)=PXG
      PYGF(NGOOD)=PYG
      PZGF(NGOOD)=PZG
   10 CONTINUE

C      IF (NGOOD.GE.2) THEN
C                       CALL QWMESE
C     &            ('WARNING EBRID MORE THAN 1 PHOTON')
C                      ENDIF

  999 CONTINUE
      RETURN
      END




      SUBROUTINE FNDACC(PTR,ACCX,ACCY,ACCZ)
      DIMENSION PTR(8,*)
C
C PTR = track coordinates : X,Y,Z,U,V,W,P,CHARGE
C   at 3 points : 1 origin, 2 TPC entry (not used), 3 TPC exit
C
      DPL=PTR(1,3)*PTR(4,1)+PTR(2,3)*PTR(5,1)+PTR(3,3)*PTR(6,1)
      COSA=PTR(4,1)*PTR(4,3)+PTR(5,1)*PTR(5,3)+PTR(6,1)*PTR(6,3)
      DIST=DPL/COSA
      XG=DIST*PTR(4,1)
      YG=DIST*PTR(5,1)
      ZG=DIST*PTR(6,1)
      ACCX=PTR(1,3)-XG
      ACCY=PTR(2,3)-YG
      ACCZ=PTR(3,3)-ZG
      AMOD=SQRT(ACCX**2+ACCY**2+ACCZ**2)
      ACCX=ACCX/AMOD
      ACCY=ACCY/AMOD
      ACCZ=ACCZ/AMOD
      RETURN
      END
      SUBROUTINE CMPNG(XTR,AMIN,AMAX,MAINV)
      DIMENSION XTR(8,*)
      DIMENSION XLOC(8)
      DIMENSION DELTA(3)
      REAL MAINV(3)

C
*CD BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
*CD BMACRO        SET OF INTRINSIC FUNCTIONS TO HANDLE BOS BANKS
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C
C
C? XTR(1:8,1) = Track at origin
C? XTR(1:8,3) = Track at TPC entry
C? XTR(1:8,3) = Track at TPC exit
C
C
C AMAX : between tpc exit and direction at origin
C

      XEXIT=XTR(1,3)-MAINV(1)
      YEXIT=XTR(2,3)-MAINV(2)
      ZEXIT=XTR(3,3)-MAINV(3)

      AMOD=SQRT(XEXIT**2+YEXIT**2+ZEXIT**2)
      AMAX=(XEXIT*XTR(4,1)+YEXIT*XTR(5,1)+ZEXIT*XTR(6,1))/AMOD
C
C AMIN : between tpc exit and direction at TPC entry
C


      AMIN=(XEXIT*XTR(4,2)+YEXIT*XTR(5,2)+ZEXIT*XTR(6,2))/AMOD



CDR take into account the fact that the photon angle is calculated from the
Cmain vertex, not from the origin

Ccalculate the position of a photon which would come from the itc wall

      CALL INTERCYL(XTR(1,2),XTR(2,2),XTR(3,2),XTR(4,2),
     &              XTR(5,2),XTR(6,2),XPH,YPH,ZPH)

Ccalculate the APPARENT direction of this photon from the origin
      PHDIST=SQRT((XPH-MAINV(1))**2+
     &       (YPH-MAINV(2))**2+(ZPH-MAINV(3))**2)
      UPH=(XPH-MAINV(1))/PHDIST
      VPH=(YPH-MAINV(2))/PHDIST
      WPH=(ZPH-MAINV(3))/PHDIST
C calculate the angle between this direction and the reference direction
      AMIN=(XEXIT*UPH+YEXIT*VPH+ZEXIT*WPH)/AMOD

  999 CONTINUE
      RETURN
      END






      SUBROUTINE EPR1(ITK,PTR,D0Z0,IER)
C                                                                       EPRTPC 3
C***********************************************************************EPRTPC 4
C!PREPARE ONE TPC TRACK FOR ECAL EXTRAP.                               *EPRTPC 5
C!                                                                     *EPRTPC 6
C!  AUTHOR   : A. BONISSENT  870125                                    *EPRTPC 7
C!                                                                     *EPRTPC 8
C!                                                                     *EPRTPC 9
C!  BANKS :                                                            *EPRTPC10
C!    INPUT   : FRFT                                                   *EPRTPC11
C!                                                                     *EPRTPC14
C! CHANGES HELIX PARAMETRISATION FROM TPC TO ECAL DESCRIPTION          *EPRTPC15
C!                                                                     *EPRTPC16
C!                                                                      EPRTPC17
C!       IER = return code ( 0 if normal)                              *EPRTPC18
C!                                                                      EPRTPC19
C***********************************************************************EPRTPC20
      DIMENSION PTR(8,3),D0Z0(2)
      DIMENSION RCYL(3),ZCYL(3)
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
C*CC BCS
C*CA FRFTJJ
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,  FRFTJJ 2
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)       FRFTJJ 3
C*CC FRFTJJ
C*CA TARCJJ
      PARAMETER(JTARIR=1,JTARTL=2,JTARP0=3,JTARD0=4,JTARZ0=5,JTARC1=6,  TARCJJ 2
     +          JTARC2=7,JTARER=8,JTARPI=13,JTAROF=14,JTARNC=15,        TARCJJ 3
     +          JTARAN=16,JTARTN=17,LTARCA=17)                          TARCJJ 4
C*CC TARCJJ
C*CA ALCONS
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12         ALCONS 2
      REAL RADEG, DEGRA                                                 ALCONS 3
      REAL CLGHT, ALDEDX                                                ALCONS 4
      INTEGER NBITW, NBYTW, LCHAR                                       ALCONS 5
      PARAMETER (PI=3.141592653589)                                     ALCONS 6
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)                          ALCONS 7
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          ALCONS 8
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         ALCONS 9
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         ALCONS10
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)                 ALCONS11
C*IF1 CRAY.OR.ETA
C                                                                       ALCONS17
C*IF DOC
C*CC ALCONS
      LOGICAL FIRST                                                     EPRTPC31
      SAVE FIRST,NAFRFT,NATARC                                          EPRTPC32
      DATA FIRST /.TRUE./                                               EPRTPC33
      DATA RECMN/186.2/,ZECMN/254.2/,THRTR/5.E-05/,NTURN/5/             EPRTPC34
C                                                                       EPRTPC35
C?  NTURN = MAX NUMBER OF TURNS                                         EPRTPC36
C                                                                       EPRTPC37
      INTEGER TTLOOP                                                    EPRTPC38
C*CA BMACRO
C - # of words/row in bank with index ID                                BMACRO 2
      LCOLS(ID) = IW(ID+1)                                              BMACRO 3
C - # of rows in bank with index ID                                     BMACRO 4
      LROWS(ID) = IW(ID+2)                                              BMACRO 5
C - index of next row in the bank with index ID                         BMACRO 6
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)                       BMACRO 7
C - index of row # NRBOS in the bank with index ID                      BMACRO 8
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)                 BMACRO 9
C - # of free words in the bank with index ID                           BMACRO10
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)                              BMACRO11
C - # of free rows in the bank with index ID                            BMACRO12
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)                               BMACRO13
C - Lth integer element of the NRBOSth row of the bank with index ID    BMACRO14
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO15
C - Lth real element of the NRBOSth row of the bank with index ID       BMACRO16
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO17
C                                                                       BMACRO18
C*CC BMACRO
      IF(FIRST)THEN                                                     EPRTPC40
         FIRST=.FALSE.                                                  EPRTPC41
C? Get speed of light in adequate units                                 ECBLDA43
         AAAKEC = CLGHT * 1.E-5                                            ECBLD
         NAFRFT=NAMIND('FRFT')                                          EPRTPC42
         NATARC=NAMIND('TARC')                                          EPRTPC43
      ENDIF                                                             EPRTPC44
      FILDEC=ALFIEL(DUMMY)
      IER=0                                                             EPRTPC45
      KFRFT  = IW(NAFRFT)                                               EPRTPC46
      IF(KFRFT.EQ.0)THEN                                                EPRTPC47
         WRITE(6,*)' epr1 -> frft bank missing, exit'
         IER=2
         GO TO 999
      ENDIF
C                                                                       EPRTPC48
C? GET NUMBER OF TRACKS                                                 EPRTPC49
C                                                                       EPRTPC50
      NTRKS=LROWS(KFRFT)
      IF(ITK.GT.NTRKS)THEN
         WRITE(6,*)' EPR1-> ITK too large ITK, NTRKS =',ITK,NTRKS
         IER=1
         GO TO 999
      ENDIF
C      KTARC  = IW(NATARC)
C      IARC=TTLOOP(ITK)
C      IF(IARC.NE.0)THEN
CC
CC? Abnormal tracks (more than 1 arc)
CC
C         IER=2
C         GO TO 999
C      ENDIF
      RECIP=-RTABL(KFRFT,ITK,JFRFIR)
      TLAM=RTABL(KFRFT,ITK,JFRFTL)
      COTH=TLAM/SQRT(1.+TLAM**2)
      PHI0=RTABL(KFRFT,ITK,JFRFP0)
      Z0  =RTABL(KFRFT,ITK,JFRFZ0)
      D0  =RTABL(KFRFT,ITK,JFRFD0)
      D0Z0(1)=D0
      D0Z0(2)=Z0
C
C?      SENSE OF TURN (SIGN OF RADIUS)
C
      SIGRA=SIGN(1.,RECIP)
C
C? SIGN OF COSTH
C
      SIGCO=SIGN(1.,COTH)
      SITH=SQRT(1.-COTH**2)
      THETA=ACOS(COTH)
      RCYL(1)=ABS(D0)
CDR      RCYL(2)=30.
      RCYL(2)=31.
      RCYL(3)=RECMN
      ZCYL(1)=Z0*SIGCO
      ZCYL(2)=ZECMN
      ZCYL(3)=ZECMN
      IFRFT=KROW(KFRFT,ITK)
      DO 200 ILOOP=1,3
C
C  LOOP 1 : ORIGIN
C  LOOP 2 : TPC ENTRY
C  LOOP 3 : TPC EXIT
C
      IF(ILOOP.NE.1)THEN
         CALL UNEWDP(RW(IFRFT+1),D0P,PHI0P)
CDR         D0  =   D0P * SIGN(1.,-RECIP)
         D0  =   D0P
         PHI0=   PHI0P
      ENDIF

C
C?      SENSE OF TURN (SIGN OF RADIUS)
C
      SIGRA=SIGN(1.,RECIP)
C
C? SIGN OF COSTH
C
      SIGCO=SIGN(1.,COTH)
      SITH=SQRT(1.-COTH**2)
      THETA=ACOS(COTH)
C
C
C?  TO GO TO BARREL
C
         SIPH0=SIN(PHI0)
         COPH0=COS(PHI0)
C
C?   CHECK THAT CURVATURE IS FINITE; ELSE, MAKE LINEAR EXTRAPOLATION,
C?    AND GIVE STANDARD MAX VALUE TO MOMENTUM
C
      IF(ABS(RECIP).LT.THRTR)THEN
         CCNST=0.
         PXY=90.
         IF(COTH.NE.0)THEN
           DSZ=(ZCYL(ILOOP)-Z0*SIGCO)/ABS(COTH)
           DSXY=SQRT(RCYL(ILOOP)**2-D0**2)/SIN(THETA)
           DS=AMIN1(DSZ,DSXY)
         ELSE
           DS=RCYL(ILOOP)-ABS(D0)
         ENDIF
           SINBR=-DS*SIN(THETA)
         ELSE
C
C?  ANGLE BETWEEN TRACK AND X AXIS AT ENTRY POINT
C
         RADIU=1./RECIP
         RAD=ABS(RADIU)
CDR         DIST=RAD-D0
            DIST=RAD+D0*SIGRA
         COPHI=-(RCYL(ILOOP)**2-RAD**2-DIST**2)/(2.*RAD*DIST)
C
C?  GET MOMENTUM PROJECTION IN X-Y PLANE
C
         PXY=AAAKEC*RAD*ABS(FILDEC)
C
C?  SEE IF TRACK INTERSECTS ECAL
C
         IF(ABS(COPHI).LT.1.)THEN
            DSXY=RAD*ACOS(COPHI)/SIN(THETA)
C
C?  Check that costheta is finite (avoid divide by zero)
C
            IF(COTH.NE.0)THEN
               DSZ=(ZCYL(ILOOP)-Z0*SIGCO)/ABS(COTH)
               DS=AMIN1(DSZ,DSXY)
            ELSE
               DS=DSXY
            ENDIF
         ELSE
            IF(COTH.NE.0)THEN
               DS=(ZCYL(ILOOP)-Z0*SIGCO)/ABS(COTH)
            ELSE
C              WRITE(6,*)' EPR1-> IER=3, ITK=',ITK
              IER=3
              GO TO 999
            ENDIF
         ENDIF
C
C?    DO NOT EXTRAPOLATE TRACKS WHICH MAKE
C?   MORE THAN NTURN  LOOPS BEFORE THEY REACH ECAL
C
         IER=1
         IF(DS.GT.TWOPI*RAD*FLOAT(NTURN))GO TO 999
         IER=0
C
C? PHI CHANGE FROM POINT 0, TO ENTRY INTO ECAL
C
         DPHI=-DS*SITH/RADIU
C
         SINB=SIN(DPHI)
         COSB=COS(DPHI)
         IF(SINB.NE.0)THEN
            CCNST=(1.-COSB)/SINB
         ELSE
            CCNST=0.
         ENDIF
         SINBR=SINB*RADIU
      ENDIF
CDR      X=-D0*SIGRA*SIPH0+SINBR*(CCNST*SIPH0-COPH0)
CDR      Y= D0*SIGRA*COPH0-SINBR*(CCNST*COPH0+SIPH0)
      X=D0*SIPH0+SINBR*(CCNST*SIPH0-COPH0)
      Y=-D0*COPH0-SINBR*(CCNST*COPH0+SIPH0)
      Z=Z0+DS*COTH
      ALPHA=PHI0+DPHI
C
C?  MAKE PROJECTIONS OF P
C
      PX=PXY*COS(ALPHA)
      PY=PXY*SIN(ALPHA)
      PZ=PXY*COTH/SITH
      PTOT=SQRT(PX**2+PY**2+PZ**2)
      PX=PX/PTOT
      PY=PY/PTOT
      PZ=PZ/PTOT
C
C?  MAKE CHARGE
C
      CHRGE=SIGN(1.,FILDEC*SIGRA)
C
      PTR(1,ILOOP)=X
      PTR(2,ILOOP)=Y
      PTR(3,ILOOP)=Z
      PTR(4,ILOOP)=PX
      PTR(5,ILOOP)=PY
      PTR(6,ILOOP)=PZ
      PTR(7,ILOOP)=PTOT
      PTR(8,ILOOP)=CHRGE
  200 CONTINUE
  999 CONTINUE
      IF(IER.NE.0)THEN
C         WRITE(6,*)' EPR1-> IER, ITK :',IER,ITK
      ENDIF
      RETURN
      END


      SUBROUTINE INTERCYL(X0,Y0,Z0,PX0,PY0,PZ0,OUTX,OUTY,OUTZ)
C given a starting point(x0,y0,z0) and a direction (px0,py0,pz0)
C output intersection with cylinder
      IMPLICIT NONE

      REAL X0,Y0,Z0
      REAL PX0,PY0,PZ0
      REAL OUTX,OUTY,OUTZ
      REAL RECMN/186.2/,ZECMN/254.2/

      REAL A,A2,B,DELTA2,C,DIST,PR0,PRX0,PRY0

Cverify the direction is a direction
      IF (ABS(1.-PX0**2-PY0**2-PZ0**2).GT.0.001) THEN
             CALL QWMESE('WARNING INTERCYL : NOT A DIRECTION')
             WRITE(*,*)X0,Y0,Z0,PX0,PY0,PZ0,OUTX,OUTY,OUTZ
                    ENDIF


Cfirst suppose intersection is on the end faces

      IF (PZ0.GT.0.) THEN
        DIST=(ZECMN-Z0)/PZ0
      ELSE IF (PZ0.LT.0) THEN
        DIST=(-ZECMN-Z0)/PZ0
      ELSE
        DIST=10000.
C        CALL QWMESE('WARNING INTERCYL PZ0=0')
      ENDIF


      OUTX=X0+PX0*DIST
      OUTY=Y0+PY0*DIST

      IF (OUTX**2+OUTY**2.LE.RECMN**2) THEN
Cyes the intersection is on the end faces
          OUTZ=Z0+PZ0*DIST
         GOTO 999
                                    ENDIF

Cno the intersection is on the cylinder
      PR0=SQRT(PX0**2+PY0**2)
C get the unit vector in rphi plane
      PRX0=PX0/PR0
      PRY0=PY0/PR0

C first calculate some coefficient
      A=1.

      B=X0*PRX0+Y0*PRY0

      C=X0**2+Y0**2-RECMN**2

      DELTA2=B**2-A*C
      IF (DELTA2.LT.0.) THEN
             WRITE(*,*) 'WARNING INTERCYL : PROBLEM WITH DISCRIMINANT'
             DELTA2=0.
                    ENDIF

      DIST=(-B+SQRT(DELTA2))/A/PR0

      OUTX=X0+PX0*DIST
      OUTY=Y0+PY0*DIST
      OUTZ=Z0+PZ0*DIST
C consistency check
      IF (ABS(OUTZ).GT.ZECMN) THEN
             WRITE(*,*) 'WARNING INTERCYL : UNCONSISTENCY'
             WRITE(*,*)X0,Y0,Z0,PX0,PY0,PZ0,OUTX,OUTY,OUTZ
             DELTA2=0.
                    ENDIF
Cverify that the point is on the cylinder
      IF (ABS(SQRT(OUTX**2+OUTY**2)-RECMN).GT.0.01) THEN
         IF (ABS(ABS(OUTZ)-ZECMN).GT.0.01) THEN

             CALL QWMESE('WARNING INTERCYL : POINT NOT ON CYLINDER')
             WRITE(*,*)X0,Y0,Z0,PX0,PY0,PZ0,OUTX,OUTY,OUTZ
                    ENDIF
       ENDIF

 999  CONTINUE
      RETURN
      END



      SUBROUTINE EVNUM(IEVNT,IRUNT)
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    4
C
      KEVEH=IW(NAMIND('EVEH'))
      IEVNT=IW(KEVEH+6)
      IRUNT=IW(KEVEH+2)
      RETURN
      END
