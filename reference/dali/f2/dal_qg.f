CH
CH --------------------------------------------------------------------  DAR
CH
      SUBROUTINE DQPR0(HPR,VPR)
CH
CH --------------------------------------------------------------------
CH
C!:  CALCULATE PERSPECTIVE SCALING RATIO = PERS
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      IF(FPSQDQ) THEN
        H=ABS(HPR)
        V=ABS(VPR)
        PRSHDQ=PRS2DQ/(1.+PRS1DQ*H)
        PRSVDQ=PRS2DQ/(1.+PRV1DQ*V)
      ELSE
        PRSHDQ=PRS2DQ/(1.+PRS1DQ*SQRT(HPR*HPR+VPR*VPR))
        PRSVDQ=PRSHDQ
      END IF
      END
CH
CH --------------------------------------------------------------------  DAR
CH
      SUBROUTINE DQDRAW(H1,V1,H2,V2)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C
C!:  DRAW 1 LINE IN DISPLAY COORDINATES
C ---------------------------------------------------------------------
      DIMENSION H(2),V(2)
      H(1)=H1
      H(2)=H2
      V(1)=V1
      V(2)=V2
      CALL DGDRAW(2,H,V)
      END
CH
CH --------------------------------------------------------------------  DAR
CH
      SUBROUTINE DQDAR(H1,V1,H2,V2)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C
C
C!:  DRAW RECTANGLE IN DISPLAY COORDINATES
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(5),V(5)
      H(1)=H1
      H(3)=H2
      V(1)=V1
      V(3)=V2
      H(2)=H(3)
      H(4)=H(1)
      H(5)=H(1)
      V(2)=V(1)
      V(4)=V(3)
      V(5)=V(1)
      CALL DGDRAW(5,H,V)
      END
CH
CH --------------------------------------------------------------------  DAR
CH
      SUBROUTINE DQFAR(H1,V1,H2,V2)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C
C
C!:  DRAW AREA IN DISPLAY COORDINATES
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(5),V(5)
      H(1)=H1
      H(3)=H2
      V(1)=V1
      V(3)=V2
      H(2)=H(3)
      H(4)=H(1)
      H(5)=H(1)
      V(2)=V(1)
      V(4)=V(3)
      V(5)=V(1)
      CALL DGAREA(5,H,V)
      END
CH
CH --------------------------------------------------------------------  DQDWI
CH
      SUBROUTINE DQDWI
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C
C
C!:  DRAW RECTANGLE AROUND WINDOW
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(5),V(5)
      DATA D1/1./
      CALL DQLEVL(ICFRDD)
      H(1)=HLOWDG(IAREDO)
      H(3)=HHGHDG(IAREDO)
      V(1)=VLOWDG(IAREDO)
      V(3)=VHGHDG(IAREDO)
      H(2)=H(3)
      H(4)=H(1)
      H(5)=H(1)
      V(2)=V(1)
      V(4)=V(3)
      V(5)=V(1)
      DLIN=DLINDD
      DLINDD=D1
      CALL DGDRAW(5,H,V)
      DLINDD=DLIN
      END
CH
CH --------------------------------------------------------------------  DQDWI
CH
      SUBROUTINE DQFFWI(LCOL)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C
C
C!:  FILL AREA OF FULL WINDOW
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(5),V(5)
      CALL DGLEVL(LCOL)
      H(1)=HMINDG(IAREDO)
      H(3)=HHGHDG(IAREDO)
      V(1)=VMINDG(IAREDO)
      V(3)=VHGHDG(IAREDO)
      H(2)=H(3)
      H(4)=H(1)
      H(5)=H(1)
      V(2)=V(1)
      V(4)=V(3)
      V(5)=V(1)
      CALL DGAREA(5,H,V)
      END
CH
CH --------------------------------------------------------------------  DQDWI
CH
      SUBROUTINE DQFWIA(LCOL)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C
C
C!:  FILL AREA OF WINDOW
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(5),V(5)
      CALL DGLEVL(LCOL)
      H(1)=HLOWDG(IAREDO)
      H(3)=HHGHDG(IAREDO)
      V(1)=VLOWDG(IAREDO)
      V(3)=VHGHDG(IAREDO)
      H(2)=H(3)
      H(4)=H(1)
      H(5)=H(1)
      V(2)=V(1)
      V(4)=V(3)
      V(5)=V(1)
      CALL DGAREA(5,H,V)
      END
CH
CH --------------------------------------------------------------------  DQFWAF
CH
      SUBROUTINE DQFWAF(LCOL)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C
C
C!:  FILL WINDOW AREA AND FRAME
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(5),V(5)
      DATA D1/1./
      CALL DGLEVL(LCOL)
      H(1)=HMINDG(IAREDO)
      H(3)=HHGHDG(IAREDO)
      V(1)=VMINDG(IAREDO)
      V(3)=VHGHDG(IAREDO)
      H(2)=H(3)
      H(4)=H(1)
      H(5)=H(1)
      V(2)=V(1)
      V(4)=V(3)
      V(5)=V(1)
      CALL DGAREA(5,H,V)
      CALL DQLEVL(ICFRDD)
      DLIN=DLINDD
      DLINDD=D1
      CALL DGDRAW(5,H,V)
      DLINDD=DLIN
      END
CH
CH --------------------------------------------------------------------  DAR
CH
      SUBROUTINE DQCIRC(HC,VC,RC,LAR,LFR,PO)
CH
CH --------------------------------------------------------------------
CH
C     INPUT: draw circle with PO points
C            RC = radius > 0 user    coordinates
C            RC = radius < 0 display coordinates
C            H,V = position in user or display coordinates
C            PO can be non natural number: example:
C                    PO=4.  circle = square
C                    PO=4.5 circle = square rotated by 45 degrees
C
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(25),V(25)
      CHARACTER *4 T
      IF(LAR.GE.0) THEN
        IF(LFR.GE.0) THEN
          T='AR+L'
        ELSE
          T='AREA'
        END IF
      ELSE
        IF(LFR.GE.0) THEN
          T='LINE'
        ELSE
          RETURN
        END IF
      END IF
      NP=PO
      N=MIN(NP,24)+1
      DA=360./FLOAT(NP)
      A=DA*MOD(PO,1.)
      R=ABS(RC)
      DO K=1,N
        H(K)=R*COSD(A)+HC
        V(K)=R*SIND(A)+VC
        A=A+DA
      END DO
      IF(FPIKDP) THEN
        IF(FPIMDP.AND.NPIKDP.NE.KPIKDP) RETURN
        DO K=1,N
          CALL DQPIK(H(K),V(K))
        END DO
      ELSE
        CALL DQPO0(T,LAR,LFR,' ')
        IF(RC.GT.0.) THEN
          CALL DQPOL(N,H,V)
        ELSE
          CALL DQPOL1(N,H,V)
        END IF
      END IF
      END
CH
CH --------------------------------------------------------------------  DQWSU
CH
      SUBROUTINE DQWSU
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C
C
C!:  SWITCH WINDOW ARRANGEMENT
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      IF(WISUDW.LE.0.) THEN
        WISUDW=1.
        IAREDO=9
      ELSE
        WISUDW=0.
      END IF
      CALL DQWIS
      DO M=0,MPNWDW
        ISTODS(4,M,IWUSDO)=-1
      END DO
      END
CH
CH --------------------------------------------------------------------  DQCL
CH
      SUBROUTINE DQCL(NCLER)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C_CG 10-May-1989 C.Grab  Adapted to CERNVM, no char* (*) ref. allowed for
C_CG                     entry-ref. on IBM.
C
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      IF(JWUSDW.GT.0) THEN
        CALL DGQINF(MODW,IWIN)
        CALL DQSWIN(0,LWUS)
        IF(MODW.EQ.2) CALL DGPOP('HPOP')
      END IF
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DQCL0
CH
      ENTRY DQCL_US(NCLER)
CH
CH --------------------------------------------------------------------
      IF(FPIKDP) RETURN
      CALL DGSTR1(NCLER,NOCLDT)
      IF(NOCLDT.EQ.0) THEN
         IARE=IAREDO
         IAREDO=NCLER
         CALL DGCLER(HMINDG(NCLER),HHGHDG(NCLER),
     1   VMINDG(NCLER),VHGHDG(NCLER))
         IF(NCLER.EQ.13) THEN
           ICBG=ICBADD
         ELSE
           ICBG=ICBGDD
         END IF
         IF(PDCODD(2,ICBG).GE.0..AND.
     &      PDCODD(4,ICBG).GE.0.) THEN
           CALL DQLEVL(ICBG)
           CALL DGRAR(HMINDG(NCLER),VMINDG(NCLER),
     &                HHGHDG(NCLER),VHGHDG(NCLER))
           IF(PDCODD(4,ICBWDD).GT.0..AND.NCLER.LE.12) THEN
             CALL DQLEVL(ICBWDD)
             CALL DGRAR(HLOWDG(NCLER),VLOWDG(NCLER),
     &                HHGHDG(NCLER),VHGHDG(NCLER))
           END IF
         END IF
         IAREDO=IARE
      END IF
      END
*DK DQCLP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQCLP
CH
      SUBROUTINE DQCLP(H1,V1,H2,V2,FDPR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann/W.Krischer          28-JUL-1988
C
C!:Clip Line, call BEFORE DQCL0
C
C    Inputs    :H1,V1 and H2,V2 endpoints of line before clipping.
C    Outputs   :H1,V1 and H2,V2 endpoints of line after clipping.
C              :FDPR=.TRUE. if part of line inside window (dpr = display range).
C
C    Called by :TV-routines and DGZOOM.
C ---------------------------------------------------------------------
      COMMON /DLOCCL/HLOW,HHGH,VLOW,VHGH
      INTEGER DQIC
      LOGICAL FDPR
C      DATA DCL/1./
      FDPR=.FALSE.
      ICLIP2=DQIC(H2,V2)
   10 ICLIP1=DQIC(H1,V1)
      IF (IOR(ICLIP1,ICLIP2) .EQ. 0)        THEN
C      IF( (ICLIP1 .OR. ICLIP2) .EQ. 0)        THEN
         FDPR=.TRUE.
         GO TO 100
      END IF
      IF( IAND(ICLIP1,ICLIP2 ) .NE. 0) GO TO 100
C      IF( (ICLIP1 .AND. ICLIP2) .NE. 0) GO TO 100
      IF( ICLIP1 .EQ. 0) THEN
         CALL USWOP(H1,H2,1)
         CALL USWOP(V1,V2,1)
         CALL USWOP(ICLIP1,ICLIP2,1)
      END IF
      IF( IAND(ICLIP1,512) .NE. 0) THEN
C      IF( (ICLIP1 .AND. "1000) .NE. 0) THEN
         V1=((V2-V1)/(H2-H1))*(HLOW-H1) + V1
         H1=HLOW
         GO TO 10
      END IF
      IF( IAND(ICLIP1,8) .NE. 0) THEN
C      IF( (ICLIP1 .AND. "0010) .NE. 0) THEN
         V1=((V2-V1)/(H2-H1))*(HHGH-H1) + V1
         H1=HHGH
         GO TO 10
      END IF
      IF( IAND(ICLIP1,64) .NE. 0) THEN
C      IF( (ICLIP1 .AND. "0100) .NE. 0) THEN
         H1=((H2-H1)/(V2-V1))*(VHGH-V1) + H1
         V1=VHGH
         GO TO 10
      END IF
      IF( IAND(ICLIP1,1) .NE. 0) THEN
C      IF( (ICLIP1 .AND. "0001) .NE. 0) THEN
         H1=((H2-H1)/(V2-V1))*(VLOW-V1) + H1
         V1=VLOW
         GO TO 10
      END IF
  100 RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DQCL0
CH
      ENTRY DQCL0(HLO,VLO,HHG,VHG,DCL)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:cl  l before DQclp to define window size
C
C    Inputs    :first and last point of the line
C    Outputs   :no
C
C ---------------------------------------------------------------------
      HLOW=HLO+DCL
      HHGH=HHG-DCL
      VLOW=VLO+DCL
      VHGH=VHG-DCL
      END
*DK DQIC
C
C
C
C
C
C
C *************************************************************** DQIC
C
      INTEGER FUNCTION DQIC(H,V)
C
C ***************************************************************
C ---------------------------------------------------------------------
C
C    Created by W.Krischer (IFCLIP)
C
C!:calculate bit pattern fot DQCLP
C
C    Inputs    :H,V = point coordinates
C    Outputs   :DQIC = bit pattern.
C
C    Called by :DQCLP
      COMMON /DLOCCL/HLOW,HHGH,VLOW,VHGH
C      ICL=0
C      IF(H .LT. HUMIDT) ICL="1000
C      IF(H .GT. HUMADT) ICL="0010
C      IF(V .LT. VUMIDT) ICL=ICL .OR. "0001
C      IF(V .GT. VUMADT) ICL=ICL .OR. "0100
C  100 DQIC=ICL
      DQIC=0
      IF(H .LT. HLOW) DQIC=512
      IF(H .GT. HHGH) DQIC=8
      IF(V .LT. VLOW) DQIC=IOR(DQIC,1)
      IF(V .GT. VHGH) DQIC=IOR(DQIC,64)
  100 END
*DK DQLEVH
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DQLEVH
CH
      SUBROUTINE DQLEVH(N)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      IF(PDCODD(4,ICCNDD).LE.0.) THEN
        CALL DGLEVL(ABS(IFIX(PDCODD(2,N))))
      ELSE
        CALL DGLEVL(IFIX(PDCODD(2,ICCNDD)))
      END IF
      END
*DK DQLEVL
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DQLEVL
CH
      SUBROUTINE DQLEVL(N)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CALL DGLEVL(ABS(IFIX(PDCODD(2,N))))
      END
*DK DQESC
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DQESC
CH
      SUBROUTINE DQSC0(T)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) T
      CHARACTER *10 T10
      DATA D/10./,DV/-5./,DH/5./,DLET/9./,DD/3./
      DIMENSION H(2),V(2),HH(2),VV(2)
      DIMENSION DX(11)
      CHARACTER *4 TX(11)
      DATA QX/3./
      DATA DX/ 200.  ,100.  ,
     &   50.  , 20.  , 10.  ,
     &    5.  ,  2.  ,  1.  ,
     &    0.5 ,  0.2 ,  0.1 /
      DATA TX/ ' 2m ',' 1m ',
     &  '50cm','20cm','10cm',
     &  ' 5cm',' 2cm',' 1cm',
     &  ' 5mm',' 2mm ','1mm'/
      IF(FPIKDP) RETURN
      SCALDS(IAREDO,1)=0.
      SCALDS(IAREDO,2)=0.
      SCALDS(IAREDO,3)=0.
      V(1)=VHGHDG(IAREDO)
      I=IFIX(DFWIDU(IZOMDO))
      IF(T.NE.'1 SC'.AND.T.NE.'2 SC') RETURN
      IF(MOD(I,10).NE.2) RETURN
      DO 1 K=1,11
        CALL DPARGV(11,'PTO',2,PPTO)
        IF(PPTO.GT.QX*DX(K)) THEN
          CALL DQPOC(0.,0.,H0,V0,FDUM)
          CALL DQPOC(DX(K),0.,H1,V1,FDUM)
          S=SQRT((H1-H0)**2+(V1-V0)**2)
          IF(T.EQ.'2 SC') THEN
            CALL DQPOC(0.,DX(K),H1,V1,FDUM)
            P=SQRT((H1-H0)**2+(V1-V0)**2)
            A1=ABS(S-P)
            A2=S+P
            IF(A1.GT.A2) RETURN
          END IF
          N=4
          T10=TX(K)(1:N)
          CALL DGLEVL(8)
          GO TO 10
        END IF
    1 CONTINUE
      RETURN
CH
CH
CH
      ENTRY DQSCE(T,L,E)
      IF(FPIKDP) RETURN
      IF(MOD(I/10,10).EQ.0) RETURN
      T10=T
      S=E
      N=L
   10 CALL DQLEVL(ICTXDD)
      V(1)=V(1)-D
      H(1)=HHGHDG(IAREDO)-N*DLET-DH
      CALL DGTEXT(H(1),V(1)+DV,T10,N)
      H(2)=H(1)-S
      V(2)=V(1)
      CALL DGDRAW(2,H,V)
      DO 700 K=1,2
        HH(1)=H(K)
        HH(2)=H(K)
        VV(1)=V(K)-DD
        VV(2)=V(K)+DD
        CALL DGDRAW(2,HH,VV)
  700 CONTINUE
      END
*DK DQFR
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DQFR
CH
      SUBROUTINE DQFR(NAR)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION HPS(5),VPS(5)
      IF(FPIKDP) RETURN
      IF(PDCODD(4,ICFRDD).EQ.1.) THEN
        CALL DQLEVL(ICFRDD)
        DLINDD=1
        HPS(1)=HMINDG(NAR)
        VPS(1)=VMINDG(NAR)
        HPS(3)=HHGHDG(NAR)
        VPS(3)=VHGHDG(NAR)
        HPS(2)=HPS(3)
        VPS(2)=VPS(1)
        HPS(4)=HPS(1)
        VPS(4)=VPS(3)
        HPS(5)=HPS(1)
        VPS(5)=VPS(1)
        CALL DGDRAW(5,HPS,VPS)
        IF(MOD(DFWIDU(IZOMDO),10.).NE.0.) THEN
          HPS(1)=HLOWDG(NAR)
          VPS(1)=VHGHDG(NAR)
          HPS(3)=HHGHDG(NAR)
          VPS(3)=VLOWDG(NAR)
          HPS(2)=HPS(1)
          VPS(2)=VPS(3)
          CALL DGDRAW(3,HPS,VPS)
        END IF
        DLINDD=PDCODD(2,LITRDD)
      END IF
      NSQUDT=0
C     ............... DOFISH: forced draw: filling of complicated areas = off
      FFDRDP=.FALSE.
      DFLGDU=ABS(DFLGDU)
      WRITE(TXTADW,1000) TAREDO(NAR)
 1000 FORMAT(' ********** draw on ',A,' **********')
      CALL DWRT_SETUP('TERMINAL=OFF')
      CALL DWRC
      CALL DWRT_SETUP('TERMINAL=LAST')
      CALL DMPSWI(NAR)
      CALL DGEXEC
C  .  .   .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
      END
*DK DQIN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQIN
CH
      SUBROUTINE DQIN
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     TABCD
C     A=V NORMAL OR ONLY CALCULATION    A=C ONLY CALCULATION
C     B: P=POINT L=LINE A=DAREA V=VECTOR T=TEXT R=RECTANGLE
C     C: N=NOSCALE  S=SCALE
C     D: N=NOSKEW   S=SKEW
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
C     H=Horizontal  V=Vertical
C     INITIALISE AND GET RANGE OF DEVICE COORDINATES
      IF(NTVIDT.EQ.1) RETURN
      CALL DQRDP(H1NW,V1NW,H2NW,V2NW,DVCOM,
     &  H1CM,V1CM,H2CM,V2CM)
      V2NW=V2NW-V1NW+1.
      IF(NTVIDT.EQ.0) THEN
        V1=V2NW-(H2NW-H1NW)*2./3.
        V1=FLOAT(IFIX(MAX(1.,V1)))
        CALL DQWIN(H1NW,V1,H2NW,V2NW,DVCOM)
      END IF
      H2NW=H2NW-H1NW+1.
      CALL DGINIT(1.,1.,H2NW,V2NW+DVCOM,
     &  H1CM,V1CM,H2CM,V2CM)
      NTVIDT=1
      END
*DK DQMID
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQMID
CH
      SUBROUTINE DQMID
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DATA I8/8/,D1/1./
      IF(.NOT.FPIKDP) THEN
         CALL DGLEVL(8)
         CALL DQPD0(I8,D1,0.)
         CALL DQPD(0.,0.)
      END IF
      END
*DK DQRDP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQRDP
CH
      SUBROUTINE DQRDP(HN1,VN1,HN2,VN2,DVC,HCM1,VCM1,HCM2,VCM2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Updated by C.Grab      for IBM version    22-May-1989
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      IF(NTVIDT.LE.0) CALL DQRPO
C                                       ! HLOW VIEWPORT COORDINATES [CM].=0.
      HCM1=POSIDW(2)
C                                       ! VLOW =100.
      VCM1=POSIDW(3)
C                                       ! HHGH=32.50
      HCM2=POSIDW(4)
C                                       ! VHGH=122.8
      VCM2=POSIDW(5)
C                                       ! HLOW NORMALISED WORLD C.=1.
      HN1 =POSIDW(6)
C                                       ! VLOW=1.
      VN1 =POSIDW(7)
C                                       ! HHGH=991.
      HN2 =POSIDW(8)
C                                       ! VHGH=661.
      VN2 =POSIDW(9)
C                                       ! DH OF BORDER =35
      DVC =POSIDW(10)
      DO   700  N=0,12
         TAREDO(N)='W'//TWINDW(N)
  700 CONTINUE
      END

*DK DQRPO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQRPO
CH
      SUBROUTINE DQRPO
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CALL DGOPEN(NUNIDU,TFILDC//TGRADG,2,*4,ISTAT)
C                                       !TWINDW(0:MPNWDW) MPNWDW=12
C                                       !definition window (13)
    2 READ(NUNIDU,1011) TWINDW
 1011 FORMAT(15(1X,A))
C                                       !MPOSDW=30
      DO   710  N=1,MPOSDW
         READ(NUNIDU,1009) M,POSIDW(M)
 1009    FORMAT(I2,1X,F14.7)
  710 CONTINUE
      CLOSE(NUNIDU)
      RETURN
 4    CALL DWRT('File '//TFILDC//TGRADG//' not found.')
      END

*DK DQ24
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQR24
CH
      SUBROUTINE DQ24(HRB,VRB)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DIMENSION HRB(4),VRB(4)
      HRB(2)=HRB(3)
      VRB(2)=VRB(1)
      HRB(4)=HRB(1)
      VRB(4)=VRB(3)
      END
*DK DQ245
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQ245
CH
      SUBROUTINE DQ245(HRB,VRB)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DIMENSION HRB(5),VRB(5)
      HRB(2)=HRB(3)
      VRB(2)=VRB(1)
      HRB(4)=HRB(1)
      VRB(4)=VRB(3)
      HRB(5)=HRB(1)
      VRB(5)=VRB(1)
      END
*DK DQRER
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQRER
CH
      SUBROUTINE DQRER(MODE,H1,V1,H2,V2,HRB,VRB)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   1.11.88
C
C!:Calculate 4 CORNERS OF RECTANGLE
C
C    Inputs    :h1,v1,h2,v2 = lower left and upper right corner
C              :Mode=0 corners to corners
C              :    =1 reduce horizontally to get aspect ratio =1
C              :    =2 reduce horizontally, *2 vertical for RZ:PR
C              :    =3 enlarge horizontally to get aspect ratio =1
C    Outputs   :4 corners of rectangle:
C              :1=left down,2=right down,3=right up,4=left up
C ---------------------------------------------------------------------
      DIMENSION HRB(4),VRB(4)
      IF(MODE.EQ.1) THEN
        CALL DQRHV(SH,SV)
        D=H1*SH
        HRB(1)=-D
        HRB(3)= D
        D=H1*SV
        VRB(1)=-D
        VRB(3)= D
      ELSE IF(MODE.EQ.2) THEN
        CALL DQRHV(SH,SV)
        D=H1*SH
        HRB(1)=-D
        HRB(3)= D
        VRB(1)=0.
        VRB(3)=2.*H1*SV
      ELSE IF(MODE.EQ.3) THEN
        CALL DQRHV(SH,SV)
        SS=MIN(SH,SV)
        D=ABS(H1*SH/SS)
        HRB(1)=-D
        HRB(3)= D
        D=ABS(H1*SV/SS)
        VRB(1)=-D
        VRB(3)= D
      ELSE
        HRB(1)=H1
        VRB(1)=V1
        HRB(3)=H2
        VRB(3)=V2
      END IF
      HRB(2)=HRB(3)
      HRB(4)=HRB(1)
      VRB(2)=VRB(1)
      VRB(4)=VRB(3)
      END
*DK DQRHV
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQRHV
CH
      SUBROUTINE DQRHV(SH,SV)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   1.11.88
C
C!:Calculate SCALE FOR NON SQUARE WINDOWS
C
C    Inputs    :NO
C    Outputs   :SH,SV = HORIZONT AND VERTICAL SCALE
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DH=HHGHDG(IAREDO)-HLOWDG(IAREDO)
      DV=VHGHDG(IAREDO)-VLOWDG(IAREDO)
      IF(DH-DV) 1,2,3
    1 SV=1.
      SH=DH/DV
      RETURN
    2 SV=1.
      SH=1.
      RETURN
    3 SH=1.
      SV=DV/DH
      END
*DK DQROT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQROT
CH
      SUBROUTINE DQROT(H1,V1,H2,V2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:Calculate 2D rotation,call dqrot0 to set up angles
C
C    Inputs    :h1,v1
C    Outputs   :h2,v2
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      HH=H1
      H2= CF*HH+SF*V1
      V2=-SF*HH+CF*V1
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DQROT0
CH
      ENTRY DQROT0(F)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C    INputs   :ROTATION ANGLE F
C
C ---------------------------------------------------------------------
      SF=SIND(F)
      CF=COSD(F)
      END
*DK DQRU
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQRU
CH
      SUBROUTINE DQRU(HI0,VI0)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:Calculate general 2D transformation
C
C    Inputs    :HIO(4),VIO(4) point of parallegram, Base between 4 and 1
C              :dir = packed direction of base dependent of rubber-band
C              :mode : BMODE.
C    Outputs   :no
C
C    Called by :all projections [ Example TINE DYXD(..) ]
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION HW(200), VW(200),
     &          HH(4),VV(4)
      DIMENSION HW1(100),VW1(100)
      EQUIVALENCE (HW(101),HW1),(VW(101),VW1)
      DIMENSION AH(-1:12),BH(-1:12),CH(-1:12),
     &          AV(-1:12),BV(-1:12),CV(-1:12)
C     &          CH(-1:12),CV(-1:12)
      DATA AH(-1)/1./,BH(-1)/0./,CH(-1)/0./
      DATA AV(-1)/0./,BV(-1)/1./,CV(-1)/0./
      DIMENSION PRS1S(-1:12),PRS2S(-1:12)
      DATA PRS1S(-1)/0./,PRS2S(-1)/0./
C      DATA RODZ/0.96/,GRZ/0.98/
C     ... With 0.96 and GRZ=0.98, R/Z fits exact into a square window
C      DATA GRZ/.9/
C     DATA PRH1S,PRV1S/1.,1.02/,PRH2S,PRV2S/1.,1./
      DATA PRV1S/1.02/
      LOGICAL FIN,FO2,F19,FLAST
      CHARACTER *(*) T13,T21,T24,T25,T26
      CHARACTER *(*) TI22,TIS22
      CHARACTER *4 T22,TS22
      DIMENSION PRSP(4)
      DIMENSION HI0(4),VI0(4),HI3(2),VI3(2),HI10(4),VI10(4),
     &  HI12(2),VI12(2),HO12(2),VO12(2),
     &  DIS7(2),DIS19(2),HI22(*),VI22(*),HI26(*),VI26(*)
      DIMENSION D22H(4),D22V(4)
      DATA D22H/-1., 1., 0., 0./
      DATA D22V/ 0., 0.,-1., 1./
      DATA K22/4/
      DIMENSION NTAB7(8)
      DIMENSION ITO10(3)
      DATA ITO10/1,3,2/
      DATA NTAB7/1,5,2,5,5,3,4,5/
      DATA DV13/4./,DMX19/3./,SM19/0.5/
      DIMENSION EI21(*)
      DATA D21/4./,DM26/6./
      DIMENSION JROP(4)
      DATA JROP/1,2,3,4/
      DATA NS110/0/
      DIMENSION K11(0:3)
      DATA K11/3,3,3,2/
      DATA KDEB/0/
      HL(HSF,VSF)=CHW+AHW*HSF       +BHW*VSF
      VL(HSF,VSF)=CVW+AVW*HSF       +BVW*VSF
C     ............................................ Perspective transformation.
      HP(HSF,VSF)=CHW+AHW*HSF*PRSHDQ+BHW*VSF*PRSVDQ
      VP(HSF,VSF)=CVW+AVW*HSF*PRSHDQ+BVW*VSF*PRSVDQ
      PRS1S(IAREDO)=0.
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQRUPR
CH
      ENTRY DQRUPR(HI0,VI0)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  SAME AS DQRU BUT WITH PERSPECTIVE TRANSFORMATION, BUT PERSPECTIVE
C!:  IS NOT SET: DQPRS0 MUST BE CALLED BEFORE
C
C    Outputs   :no
C
C      4 +-----+ 3
C        |     |
C      1 +-----+ 2
C
C     X13=   X1 -   X3
      X13=HI0(JROP(1))-HI0(JROP(3))
C     X23=   X2 -   X3
      X23=HI0(JROP(2))-HI0(JROP(3))
C     Y13=   Y1 -   Y3
      Y13=VI0(JROP(1))-VI0(JROP(3))
C     Y23=   Y2 -   Y3
      Y23=VI0(JROP(2))-VI0(JROP(3))
C
      H1=HLOWDG(IAREDO)
      H2=HHGHDG(IAREDO)
      H3=HHGHDG(IAREDO)
      H13=H1-H3
      H23=H2-H3
      V1=VLOWDG(IAREDO)
      V2=VLOWDG(IAREDO)
      V3=VHGHDG(IAREDO)
      V13=V1-V3
      V23=V2-V3
      XY23=X23*Y13-X13*Y23
      IF(XY23.NE.0.)THEN
         QXY=1./XY23
      ELSE
         QXY=1.
      END IF
      BH(IAREDO)=(X23*H13-X13*H23)*QXY
      BV(IAREDO)=(X23*V13-X13*V23)*QXY
C??      IF(X13.NE.1.)THEN
      IF(X13.NE.0.)THEN
         QX13=1./X13
         AH(IAREDO)=(H13-BH(IAREDO)*Y13)*QX13
         AV(IAREDO)=(V13-BV(IAREDO)*Y13)*QX13
      ELSE
         QX23=1./X23
         AH(IAREDO)=(H23-BH(IAREDO)*Y23)*QX23
         AV(IAREDO)=(V23-BV(IAREDO)*Y23)*QX23
      END IF
      CH(IAREDO)=H1-AH(IAREDO)*HI0(JROP(1))-BH(IAREDO)*VI0(JROP(1))
      CV(IAREDO)=V1-AV(IAREDO)*HI0(JROP(1))-BV(IAREDO)*VI0(JROP(1))
      DHO14=0.
      DVO14=0.
      IARE=IAREDO
      DO K=1,4
        HUSRDQ(K,IARE)=HI0(K)
        VUSRDQ(K,IARE)=VI0(K)
      END DO
      GO TO 5
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQLAST
CH
      ENTRY DQLAST(T25)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C    RESTORE LAST SCALE PARAMETERS
C    USED AFTER GG:FT:RB:EN: DFTD IS CALLED WITH DIFFERENT USER RANGE BUT
C    THE PICTURE IS NOT CHANGED AND THEREFORE SCALE PARAMETERS MUST BE
C    RESTORED.
      IF(T25.EQ.'STORE') THEN
        AHOLD=AH(IAREDO)
        BHOLD=BH(IAREDO)
        CHOLD=CH(IAREDO)
        AVOLD=AV(IAREDO)
        BVOLD=BV(IAREDO)
        CVOLD=CV(IAREDO)
      ELSE
        AH(IAREDO)=AHOLD
        BH(IAREDO)=BHOLD
        CH(IAREDO)=CHOLD
        AV(IAREDO)=AVOLD
        BV(IAREDO)=BVOLD
        CV(IAREDO)=CVOLD
      END IF
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQSET
CH
      ENTRY DQSET1(NI14,DHI14,DVI14)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C    SCALE IS SET TO 1, SO THAT ONLY THE CLIPPING IS DONE
C!:to be called if DQRU is not called before to set up window
C
C    Inputs    :NI14 = window , window size reduced by DHI14,DVI14
C    Outputs   :no
C
C ---------------------------------------------------------------------
      DHO14=DHI14
      DVO14=DVI14
      HLOW=HLOWDG(NI14)+DHO14
      HHGH=HHGHDG(NI14)-DHO14
      VLOW=VLOWDG(NI14)+DVO14
      VHGH=VHGHDG(NI14)-DVO14
      IARE=-1
      GO TO 6
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQSET
CH
      ENTRY DQSET(NI14,DHI14,DVI14)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:to be called if DQRU is not called before to set up window
C
C    Inputs    :NI14 = window , window size reduced by DHI14,DVI14
C    Outputs   :no
C
C ---------------------------------------------------------------------
      DHO14=DHI14
      DVO14=DVI14
      IARE=NI14
    5 HLOW=HLOWDG(IARE)+DHO14
      HHGH=HHGHDG(IARE)-DHO14
      VLOW=VLOWDG(IARE)+DVO14
      VHGH=VHGHDG(IARE)-DVO14
    6 AHW=AH(IARE)
      BHW=BH(IARE)
      CHW=CH(IARE)
      AVW=AV(IARE)
      BVW=BV(IARE)
      CVW=CV(IARE)
      CALL DQCL0(HLOW,VLOW,HHGH,VHGH,1.)
      AHSCDQ=AHW
      BHSCDQ=BHW
      CHSCDQ=CHW
      AVSCDQ=AVW
      BVSCDQ=BVW
      CVSCDQ=CVW
      PRS1DQ=PRS1S(IARE)
      PRS2DQ=PRS2S(IARE)
      PRV1DQ=PRS1DQ*PRV1S
      IF(PRS1DQ.EQ.0.) THEN
        FPRSDQ=.FALSE.
        PRSHDQ=1.
        PRSVDQ=1.
      ELSE
        FPRSDQ=.TRUE.
      END IF
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQPRS0
CH
      ENTRY DQPRS0(PRSP)
CH
CH --------------------------------------------------------------------
CH
C     ..................................... set up perspective transformation
      IF(PRSP(4).EQ.1..AND.PRSP(2).GT.0.) THEN
C       ................................. 13: PRODUCE WONDERFUL TRACKPICTURES
        CALL DPARGV(11,'PTO',2,R2)
C       R2 = PARADA(2,J_PTO)
        R1 = 0.5*R2
        RM = R2 - R1/PRSP(2)
        PRS1S(IAREDO) = (RM - R1)/(R1*(R2-RM))
        PRS2S(IAREDO) = 1.+PRS1S(IAREDO)*R2
        FPRSDQ=.TRUE.
      ELSE
        PRS1S(IAREDO) = 0.
        FPRSDQ=.FALSE.
      END IF
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQPRS0
CH
      ENTRY DQPRSF(T24)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
      IF(T24(1:3).EQ.'OFF') THEN
        FLAST =FPRSDQ
        FPRSDQ=.FALSE.
        PRSHDQ=1.
        PRSVDQ=1.
      ELSE IF(T24.EQ.'LAST') THEN
        FPRSDQ=FLAST
      END IF
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQPRS0
CH
      ENTRY DQPRS(H24,V24)
CH
CH --------------------------------------------------------------------
CH
C     H24 and V24 are fransformed via pers.transf.
C     They should then be drawn with via HW=... ,VW=..... or HP=..., VP+...
C     For the latter case PRSHDQ ans PRSVDQ  are set to 1.
C ---------------------------------------------------------------------
C
C     PRSHDQ and PRSVDQ are calculated in DQPR0.
      CALL DQPR0(H24,V24)
      H24=H24*PRSHDQ
      V24=V24*PRSVDQ
      PRSHDQ=1.
      PRSVDQ=1.
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQROP
CH
      ENTRY DQROP(NI23)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!: ROTATE PICTURE
C
C    Inputs    :NI23  1 TO  4 ROTATE BY O,90,180,270 DEG
C              :NI23<0 MIRROR IMAGE
C    Outputs   :no
C
C ---------------------------------------------------------------------
CH..............--
      IF(NI23.GT.4.OR.NI23.LT.-4.OR.NI23.EQ.0) RETURN
      NO23=ABS(NI23)-1
      DO J=1,4
        IF(NI23.GT.0) THEN
          JROP(  J)=1+MOD(J-1+NO23,4)
        ELSE
          JROP(5-J)=1+MOD(J-1+NO23,4)
        END IF
      END DO
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQHIC
CH
      ENTRY DQHI2(HI21,VI21,EI21,NI21,DR21,A21,T21)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C
C    Inputs    :H,V = left edge of baseline of histogram icon
C              :NI21=# of channels in Ei21(NI21), A21 = angle, T21= ,+,_
C    Outputs   :no
C
C ---------------------------------------------------------------------
      IF(FPRSDQ) CALL DQPR0(HI21,VI21)
      HO21=HP(HI21,VI21)
      VO21=VP(HI21,VI21)
      NB21=DR21
      B21=NI21*NB21
      C21=COSD(A21)
      S21=SIND(A21)
      HW(1)=HO21
      VW(1)=VO21
      HW(2)=HO21+C21*B21
      VW(2)=VO21+S21*B21
C     .............................................. draw base line
      CALL DQCLP(HW(1),VW(1),HW(2),VW(2),FIN)
      IF(FIN) CALL DGDRAW(2,HW,VW)
      H21=HO21+0.4*C21*B21+S21*D21
      V21=VO21+0.4*S21*B21-C21*D21
      IF(T21.NE.' ') THEN
        HW(1)=H21-D21
        HW(2)=H21+D21
        VW(1)=V21
        VW(2)=V21
C       ............................................. draw -
        CALL DQCLP(HW(1),VW(1),HW(2),VW(2),FIN)
        IF(FIN) CALL DGDRAW(2,HW,VW)
        IF(T21.EQ.'+') THEN
          HW(1)=H21
          HW(2)=H21
          VW(1)=V21+D21
          VW(2)=V21-D21
C         ............................................. draw +
          CALL DQCLP(HW(1),VW(1),HW(2),VW(2),FIN)
          IF(FIN) CALL DGDRAW(2,HW,VW)
        END IF
      END IF
      R21=0.
      DO 821 K=1,NI21
        HW(1)=HO21+C21*R21
        VW(1)=VO21+S21*R21
        HW(2)=HW(1)-S21*EI21(K)
        VW(2)=VW(1)+C21*EI21(K)
        R21=R21+DR21
        CALL DQCLP(HW(1),VW(1),HW(2),VW(2),FIN)
        IF(FIN) CALL DGDRAW(2,HW,VW)
  821 CONTINUE
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQHIC
CH
      ENTRY DQHI2_PICK(HI21,VI21,NI21,DR21,A21)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C
C    Inputs    :H,V = left edge of baseline of histogram icon
C              :NI21=# of channels  A21 = angle
C    Outputs   :no
C
C ---------------------------------------------------------------------
      IF(FPRSDQ) CALL DQPR0(HI21,VI21)
      HO21=HP(HI21,VI21)
      VO21=VP(HI21,VI21)
      NB21=DR21
      B21=NI21*NB21
      C21=COSD(A21)
      S21=SIND(A21)
      HW(1)=HO21
      VW(1)=VO21
      HW(2)=HO21+C21*B21
      VW(2)=VO21+S21*B21
C     .............................................. draw base line
      CALL DQCLP(HW(1),VW(1),HW(2),VW(2),FIN)
      IF(FIN) THEN
        H21=HO21+0.4*C21*B21+S21*D21
        V21=VO21+0.4*S21*B21-C21*D21
        D=ABS(H21-HPIKDP)+ABS(V21-VPIKDP)
        IF(D.LE.DPIKDP) THEN
          DPIKDP=D
          NPIKDP=KPIKDP
          MDLPDP=MDLRDP
          HHPKDP=H21
          VVPKDP=V21
        END IF
      END IF
      RETURN
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DQL2E
CH
      ENTRY DQL2EP(HI4P1,VI4P1,HI4P2,VI4P2)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:transform and draw 1 line
C    SAME AS DQL2E. BUT ALLOWS FOR picking
C
C    Inputs    :first and last point of the line
C    Outputs   :no
C
C ---------------------------------------------------------------------
      IF(FPIKDP) THEN
        IF(FPRSDQ) CALL DQPR0(HI4P1,VI4P1)
        HW(1)=HP(HI4P1,VI4P1)
        VW(1)=VP(HI4P1,VI4P1)
        IF(FPRSDQ) CALL DQPR0(HI4P2,VI4P2)
        HW(2)=HP(HI4P2,VI4P2)
        VW(2)=VP(HI4P2,VI4P2)
        CALL DQCLP(HW(1),VW(1),HW(2),VW(2),FIN)
        IF(FIN) THEN
          D21H4=HW(2)-HW(1)
          D21V4=VW(2)-VW(1)
          D01H4=HPIKDP-HW(1)
          D01V4=VPIKDP-VW(1)
          Q4=D21H4*D21H4+D21V4*D21V4
          IF(Q4.NE.0.0) THEN
            U=(D21H4*D01H4+D21V4*D01V4)/Q4
          ELSE
            U=-99.
          END IF
          IF(U.LT.0.) THEN
            U=0
            D=SQRT(D01H4*D01H4+D01V4*D01V4)
          ELSE IF(U.GT.1.) THEN
            U=1
            D02H4=HPIKDP-HW(2)
            D02V4=VPIKDP-VW(2)
            D=SQRT(D01H4*D01H4+D02V4*D02V4)
          ELSE
            D=ABS( (D21V4*D01H4-D21H4*D01V4)/SQRT(Q4))
          END IF
          IF(D.LE.DPIKDP) THEN
            DPIKDP=D
            NPIKDP=KPIKDP
            FNTRDP=FKTRDP
            MDLPDP=MDLRDP
            HHPKDP=HW(1)+U*D21H4
            VVPKDP=VW(1)+U*D21V4
          END IF
        END IF
        RETURN
      END IF
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DQL2E
CH
      ENTRY DQL2E(HI4P1,VI4P1,HI4P2,VI4P2)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:transform and draw 1 line
C
C    Inputs    :first and last point of the line
C    Outputs   :no
C
C ---------------------------------------------------------------------
      IF(FPRSDQ) CALL DQPR0(HI4P1,VI4P1)
      HW(1)=HP(HI4P1,VI4P1)
      VW(1)=VP(HI4P1,VI4P1)
      IF(FPRSDQ) CALL DQPR0(HI4P2,VI4P2)
      HW(2)=HP(HI4P2,VI4P2)
      VW(2)=VP(HI4P2,VI4P2)
      CALL DQCLP(HW(1),VW(1),HW(2),VW(2),FIN)
      IF(FIN) CALL DGDRAW(2,HW,VW)
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQLHE
CH
      ENTRY DQLHE(HI15L,HI15R,VI15)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:transform and draw 1 horizontal line. No rotation assumed; skew possible.
C Used for grey pull lines.
C
C    Inputs    :Left and right horizontal position and vertical position.
C              :IF(FIO15>0) no clipping and IF(H.LT.FIO15) H=H+360
C              :FIO15 is set by DQPD0
C    Outputs   :no
C
C    CALLED BY :DFTPT
C ---------------------------------------------------------------------
      HW(1)=HL(HI15L,VI15)
      HW(2)=HL(HI15R,VI15)
      VW(1)=VL(0.,VI15)
      VW(2)=VW(1)
      IF(FIO15.EQ.0) THEN
         CALL DQCLP(HW(1),VW(1),HW(2),VW(2),FIN)
         IF(FIN) CALL DGDRAW(2,HW,VW)
      ELSE
         CALL DGDRAW(2,HW,VW)
         IF(VI15.LT.FIO15) THEN
            VW(1)=VL(0.,VI15+360.)
            VW(2)=VW(1)
            CALL DGDRAW(2,HW,VW)
         END IF
      END IF
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQLIC
CH
      ENTRY DQLIC(HI12,VI12,HO12,VO12,FO12)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:transform and clip 1 line. Return result. No drawing!
C
C    Inputs    :HI12(2),VI12(2) = first and last point of the line.
C    Outputs   :HO12(2),VO12(2) = result of transformation and clipping.
C              :FO12 = .TRUE. if inside window.
C
C ---------------------------------------------------------------------
      IF(FPRSDQ) CALL DQPR0(HI12(1),VI12(1))
      HO12(1)=HP(HI12(1),VI12(1))
      VO12(1)=VP(HI12(1),VI12(1))
      IF(FPRSDQ) CALL DQPR0(HI12(2),VI12(2))
      HO12(2)=HP(HI12(2),VI12(2))
      VO12(2)=VP(HI12(2),VI12(2))
      CALL DQCLP(HO12(1),VO12(1),HO12(2),VO12(2),FO12)
      RETURN
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DQLIEP
CH
      ENTRY DQLIEP(HI3,VI3)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C    SAME AS DQLIE. BUT ALLOWS FOR PICKING
C
C ---------------------------------------------------------------------
      IF(FPIKDP) THEN
        IF(FPRSDQ) CALL DQPR0(HI3(1),VI3(1))
        HW(1)=HP(HI3(1),VI3(1))
        VW(1)=VP(HI3(1),VI3(1))
        IF(FPRSDQ) CALL DQPR0(HI3(2),VI3(2))
        HW(2)=HP(HI3(2),VI3(2))
        VW(2)=VP(HI3(2),VI3(2))
        CALL DQCLP(HW(1),VW(1),HW(2),VW(2),FIN)
        IF(FIN) THEN
          D21H4=HW(2)-HW(1)
          D21V4=VW(2)-VW(1)
          D01H4=HPIKDP-HW(1)
          D01V4=VPIKDP-VW(1)
          Q4=D21H4*D21H4+D21V4*D21V4
          IF(Q4.NE.0.0) THEN
            U=(D21H4*D01H4+D21V4*D01V4)/Q4
          ELSE
            U=-99.
          END IF
          IF(U.LT.0.) THEN
            U=0
            D=SQRT(D01H4*D01H4+D01V4*D01V4)
          ELSE IF(U.GT.1.) THEN
            U=1
            D02H4=HPIKDP-HW(2)
            D02V4=VPIKDP-VW(2)
            D=SQRT(D01H4*D01H4+D02V4*D02V4)
          ELSE
            D=ABS( (D21V4*D01H4-D21H4*D01V4)/SQRT(Q4))
          END IF
          IF(D.LE.DPIKDP) THEN
            DPIKDP=D
            NPIKDP=KPIKDP
            FNTRDP=FKTRDP
            MDLPDP=MDLRDP
            HHPKDP=HW(1)+U*D21H4
            VVPKDP=VW(1)+U*D21V4
          END IF
        END IF
        RETURN
      END IF
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQLIE
CH
      ENTRY DQLIE(HI3,VI3)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:transform and draw 1 line.
C
C    Inputs    :HI3(2),VI3(2) = first and last point of the line.
C    Outputs   :no
C
C ---------------------------------------------------------------------
      IF(FPRSDQ) CALL DQPR0(HI3(1),VI3(1))
      HW(1)=HP(HI3(1),VI3(1))
      VW(1)=VP(HI3(1),VI3(1))
      IF(FPRSDQ) CALL DQPR0(HI3(2),VI3(2))
      HW(2)=HP(HI3(2),VI3(2))
      VW(2)=VP(HI3(2),VI3(2))
      CALL DQCLP(HW(1),VW(1),HW(2),VW(2),FIN)
      IF(FIN) CALL DGDRAW(2,HW,VW)
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQLIE
CH
      ENTRY DQLIE1(HI3,VI3)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  Clip and draw 1 line.
C
C    Inputs    :HI3(2),VI3(2) = first and last point of the line.
C    Outputs   :no
C
C ---------------------------------------------------------------------
      HW(1)=HI3(1)
      VW(1)=VI3(1)
      HW(2)=HI3(2)
      VW(2)=VI3(2)
      CALL DQCLP(HW(1),VW(1),HW(2),VW(2),FIN)
      IF(FIN) CALL DGDRAW(2,HW,VW)
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQINV
CH
      ENTRY DQINV(NW,HI1,VI1,HO1,VO1)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:Inverse transformation from display to user space.
C    Inputs    :NW = number of window for transformation
C              :HI1,VI1 input in display space.
C    Outputs   :HO1,VO1 output in user space.
C
C    Called by : DQZO2,DQZO2
C ---------------------------------------------------------------------
      FINV=AH(NW)*BV(NW)-AV(NW)*BH(NW)
      IF(FINV.NE.0.) THEN
         QINV=1./FINV
      ELSE
         QINV=1.
      END IF
      HM1=HI1-CH(NW)
      VM1=VI1-CV(NW)
      HO1=(-BH(NW)*VM1+BV(NW)*HM1)*QINV
      VO1=( AH(NW)*VM1-AV(NW)*HM1)*QINV
C
C
C      IF(AH(NW).NE.0.) THEN
C         QAH=1./AH(NW)
C      ELSE
C         QAH=1.
C      END IF
C      HCH=HI1-CH(NW)
C      VO1=(AH(NW)*(VI1-CV(NW))-AV(NW)*HCH)*QINV
C      HO1=(HCH-BH(NW)*VO1)*QAH
C
      IF(PRS1S(NW).NE.0.) THEN
        IF(TPICDP(ISTODS(5,NW,IWUSDO)).NE.'RZ') THEN
          QPINV=1./(PRS2S(NW)-PRS1S(NW)*SQRT(HO1*HO1+VO1*VO1))
          HO1=HO1*QPINV
          VO1=VO1*QPINV
        ELSE
C         ......................................................... 25 = R/Z
          QPINV=1./(PRS2S(NW)-PRS1S(NW)*ABS(HO1))
          HO1=HO1*QPINV
          QPINV=1./(PRS2S(NW)-PRS1S(NW)*ABS(VO1)*PRV1S)
          VO1=VO1*QPINV
        END IF
      END IF
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQCH0
CH
      ENTRY DQCH0(SCI11,WI11)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
      SI11=SCI11
      IW11=WI11-1.
      W11 =0.5*FLOAT(IW11)
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQCH
CH
      ENTRY DQCH(HI11,VI11,CI11)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  Draw  chimney defined in display space. Call DQRCH0 before.
C
C    Inputs    :HI11,VI11 = chimney foot, CI11=chimney hight
C    Outputs   :NO
C
C    Called by :DFTCH
C ---------------------------------------------------------------------
      HW(2)=HL(HI11,VI11)
      VW(2)=VL(HI11,VI11)
      IF(HW(2).LT.HLOW.OR.HW(2).GT.HHGH.OR.
     &   VW(2).LT.VLOW.OR.VW(2).GT.VHGH) RETURN
      VW(3)=MIN(VHGH,VW(2)+CI11*SI11)
      IF(W11.LE.0.) THEN
        HW(3)=HW(2)
        CALL DGDRAW(2,HW(2),VW(2))
      ELSE
        HW(1)=HW(2)-W11
        HW(2)=HW(2)+W11
        HW(3)=HW(2)
        HW(4)=HW(1)
        VW(1)=VW(2)
        VW(4)=VW(3)
        CALL DGAREA(4,HW,VW)
      END IF
      RETURN
CH..............---
CH
CH
CH
CH -----------------------------------------------------------  DQCH_PI
CH
      ENTRY DQCH_PI(HI11,VI11,CI11)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  Draw  chimney defined in display space. Call DQRCH0 before.
C
C    Inputs    :HI11,VI11 = chimney foot, CI11=chimney hight
C    Outputs   :NO
C
C ---------------------------------------------------------------------
      HW(2)=HL(HI11,VI11)
      VW(2)=VL(HI11,VI11)
      IF(HW(2).LT.HLOW.OR.HW(2).GT.HHGH.OR.
     &   VW(2).LT.VLOW.OR.VW(2).GT.VHGH) RETURN
      VW(3)=MIN(VHGH,VW(2)+CI11*SI11)
      HW(3)=HW(2)
      IF(FPIMDP.AND.KPIKDP.NE.NPIKDP) RETURN
      DO K=2,K11(IPTNDP)
        D=ABS(HW(K)-HPIKDP)+ABS(VW(K)-VPIKDP)
        IF(D.LE.DPIKDP) THEN
          DPIKDP=D
          NPIKDP=KPIKDP
          MDLPDP=MDLRDP
          HHPKDP=HW(K)
          VVPKDP=VW(K)
        END IF
      END DO
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQREC
CH
      ENTRY DQREC(HI16L,VI16L,HI16H,VI16H)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C       DRAW RECTANGLE
C    Inputs    :HI16,VI16 = 2 corners of rectangle
C    Outputs   :no
C
C ---------------------------------------------------------------------
      IF(FPRSDQ) CALL DQPR0(HI16L,VI16L)
      HW(1)=HP(HI16L,VI16L)
      VW(1)=VP(HI16L,VI16L)
      IF(FPRSDQ) CALL DQPR0(HI16H,VI16L)
      HW(2)=HP(HI16H,VI16L)
      VW(2)=VP(HI16H,VI16L)
      IF(FPRSDQ) CALL DQPR0(HI16H,VI16H)
      HW(3)=HP(HI16H,VI16H)
      VW(3)=VP(HI16H,VI16H)
      IF(FPRSDQ) CALL DQPR0(HI16L,VI16H)
      HW(4)=HP(HI16L,VI16H)
      VW(4)=VP(HI16L,VI16H)
      HW(5)=HW(1)
      VW(5)=VW(1)
      DO 700 K=1,4
         HH(1)=HW(K)
         HH(2)=HW(K+1)
         VV(1)=VW(K)
         VV(2)=VW(K+1)
         CALL DQCLP(HH(1),VV(1),HH(2),VV(2),FIN)
         IF(FIN) CALL DGDRAW(2,HH,VV)
  700 CONTINUE
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQPIK
CH
      ENTRY DQPIK(HI5,VI5)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  find next point for picking
C    Inputs    :HI5,VI5 = point-coordinates
C    Outputs   :no
C
C    Called by :all projections
C ---------------------------------------------------------------------
      IF(FPRSDQ) CALL DQPR0(HI5,VI5)
      HO5=HP(HI5,VI5)
      VO5=VP(HI5,VI5)
      IF(HO5.LT.HLOW.OR.HO5.GT.HHGH.OR.
     &  VO5.LT.VLOW.OR.VO5.GT.VHGH) RETURN
      D=ABS(HO5-HPIKDP)+ABS(VO5-VPIKDP)
      IF(D.LE.DPIKDP) THEN
         DPIKDP=D
         NPIKDP=KPIKDP
         FNTRDP=FKTRDP
         MDLPDP=MDLRDP
         HHPKDP=HO5
         VVPKDP=VO5
      END IF
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQPIF
CH
      ENTRY DQPIF(HI18,VI18)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  find next point for picking
C    Inputs    :HI5,VI5 = point-coordinates
C    Outputs   :no
C
C    Called by :all projections
C ---------------------------------------------------------------------
      VO18=VI18
      DO 718 K=1,2
        HS=HL(HI18,VO18)
        VS=VL(HI18,VO18)
        IF(HS.LT.HLOW.OR.HS.GT.HHGH.OR.
     &    VS.LT.VLOW.OR.VS.GT.VHGH) GO TO 18
        D=ABS(HS-HPIKDP)+ABS(VS-VPIKDP)
        IF(D.LT.DPIKDP) THEN
           DPIKDP=D
           NPIKDP=KPIKDP
           FNTRDP=FKTRDP
           MDLPDP=MDLRDP
           HHPKDP=HS
           VVPKDP=VS
        END IF
   18   VO18=VO18+360.
  718 CONTINUE
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQPD0
CH
      ENTRY DQPD0(NSI6,DI6,FII6)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:set up parameters for DQPD.
C
C    Inputs    :NSI6 # of symbol :
C              : Symbols    1   2   3   4   5   6   7   8
C              :            .   /   X   _   |   R   C   A
C              :DI6 symbol size in display coordinates
C    Outputs   :no
C
C ---------------------------------------------------------------------
      NSO6=NSI6
      IO6=MAX(1.,DI6)
      DHO6=0.5*IO6
      DVO6=DHO6
      FIO6 =FII6
      FIO15=FII6
      IF(NSO6.NE.8.AND.NSO6.NE.1) CALL DPARGV(71,'SLW',2,DLINDD)
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQPD0
CH
      ENTRY DQPD1(NGRUP,FII6)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
CJ_     ..................... 2 following lines not checked by J_DPAR_CHECK
      CALL DPARGV(NGRUP,'SSY',2,SYMB)
      CALL DPARGV(NGRUP,'SSZ',2,SYSZ)
      NSO6=SYMB
      IO6=MAX(1.,SYSZ)
      DHO6=0.5*IO6
      DVO6=DHO6
      FIO6 =FII6
      FIO15=FII6
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQPD
CH
      ENTRY DQPD_DC(HI6,VI6)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:draw symbol defined by DQPD0
C    Inputs    :HI6,VI6 symbol-position in display coordinates
C    Outputs   :no
C
      HO6=HI6
      VO6=VI6
      GO TO 60
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQPD
CH
      ENTRY DQPD(HI6,VI6)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:draw symbol defined by DQPD0
C    Inputs    :HI6,VI6 symbol-position
C    Outputs   :no
C
C ---------------------------------------------------------------------
      IF(FPRSDQ) CALL DQPR0(HI6,VI6)
      HO6=HP(HI6,VI6)
      VO6=VP(HI6,VI6)
      IF(HO6.LT.HLOW.OR.HO6.GT.HHGH.OR.
     &  VO6.LT.VLOW.OR.VO6.GT.VHGH) RETURN
   60 GO TO (61,62,63,64,65,66,67,68,69),NSO6
C                                               POINT
   61 CALL DGPOIN(HO6,VO6)
      RETURN
C                                               LINE
   62 GO TO 64
C                                               X
   63 HW(1)=HO6-DHO6
      HW(2)=HO6+DHO6
      VW(1)=VO6-DVO6
      VW(3)=VW(1)
      VW(2)=VO6+DVO6
      CALL DGDRAW(2,HW,VW)
      CALL DGDRAW(2,HW,VW(2))
      RETURN
C                                               HORIZONTAL LINE
   64 HW(1)=HO6-DHO6
      HW(2)=HO6+DHO6
      VW(1)=VO6
      VW(2)=VO6
      CALL DGDRAW(2,HW,VW)
      RETURN
C                                               VERTICAL LINE
   65 HW(1)=HO6
      HW(2)=HO6
      VW(1)=VO6-DVO6
      VW(2)=VO6+DVO6
      CALL DGDRAW(2,HW,VW)
      RETURN
C                                               RECTANGLE
   66 HW(1)=HO6-DHO6
      HW(4)=HW(1)
      HW(5)=HW(1)
      HW(3)=HO6+DHO6
      HW(2)=HW(3)
      VW(1)=VO6-DVO6
      VW(2)=VW(1)
      VW(5)=VW(1)
      VW(3)=VO6+DVO6
      VW(4)=VW(3)
      CALL DGDRAW(5,HW,VW)
      RETURN
C                                               COFFIN
   67 HW(1)=HO6-DHO6
      HW(4)=HW(1)
      HW(5)=HW(1)
      HW(8)=HW(1)
      HW(3)=HO6+DHO6
      HW(2)=HW(3)
      HW(7)=HW(3)
      VW(1)=VO6-DVO6
      VW(2)=VW(1)
      VW(5)=VW(1)
      VW(7)=VW(1)
      VW(3)=VO6+DVO6
      VW(4)=VW(3)
      HW(6)=HW(3)
      VW(6)=VW(3)
      VW(8)=VW(3)
      CALL DGDRAW(8,HW,VW)
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQREC
CH
      ENTRY DQAR0(DHI20,DVI20)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C       DRAW RECTANGLE
C    Inputs    :MINIMUM SICE FOR DQRAR
C    Outputs   :no
C
C ---------------------------------------------------------------------
      DH20=MAX(1.,DHI20)
      DV20=MAX(1.,DVI20)
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQREC
CH
      ENTRY DQAR(HI20L,VI20L,HI20H,VI20H)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C       DRAW UNROTATED RECTANGLE AREA SET UP MINIMUM SIZE (>=1.)BEFORE
C
C    ----------H
C    |         |   L < H
C    |         |
C    L----------   IS ASSUMED.
C
C    Inputs    :HI20,VI20 = 2 corners of rectangle
C    Outputs   :no
C
C ---------------------------------------------------------------------
      HW(1)=HL(HI20L,VI20L)
      IF(HW(1).GT.HHGHDG(IAREDO)) RETURN
      VW(1)=VL(HI20L,VI20L)
      IF(VW(1).GT.VHGHDG(IAREDO)) RETURN
      HW(3)=HL(HI20H,VI20H)
      IF(HW(3).LT.HLOWDG(IAREDO)) RETURN
      VW(3)=VL(HI20H,VI20H)
      IF(VW(3).LT.VLOWDG(IAREDO)) RETURN
      HW(1)=MAX(HW(1),HLOWDG(IAREDO))
      VW(1)=MAX(VW(1),VLOWDG(IAREDO))
      HW(3)=MIN(HW(3),HHGHDG(IAREDO))
      VW(3)=MIN(VW(3),VHGHDG(IAREDO))
      HW(3)=MAX(HW(3),HW(1)+DH20)
      VW(3)=MAX(VW(3),VW(1)+DV20)
      HW(2)=HW(3)
      VW(2)=VW(1)
      HW(4)=HW(1)
      VW(4)=VW(3)
C      DO K=1,4
C        IF(VW(K).LT.VLOWDG(IAREDO).OR.VW(K).GT.VHGHDG(IAREDO).OR.
C     &     HW(K).LT.HLOWDG(IAREDO).OR.HW(K).GT.HHGHDG(IAREDO)) THEN
C          TYPE *,'======'
C        END IF
C      END DO
      CALL DGAREA(4,HW,VW)
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQARD
CH
      ENTRY DQARD(HI6,VI6)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:draw area fast. Area defined in display space.
C
C    Inputs    :HI6,VI6 = AREA CENTER POSITION
C    Outputs   :NO
C
C    Called by :?
C ---------------------------------------------------------------------
      HO6=HL(HI6,VI6)
      VO6=VL(HI6,VI6)
      IF(HO6.LT.HLOW.OR.HO6.GT.HHGH.OR.
     &  VO6.LT.VLOW.OR.VO6.GT.VHGH) RETURN
C
C                                                 AREA
   68 HW(1)=HO6-DHO6
      HW(4)=HW(1)
      HW(3)=HO6+DHO6
      HW(2)=HW(3)
      VW(1)=VO6-DVO6
      VW(2)=VW(1)
      VW(3)=VO6+DVO6
      VW(4)=VW(3)
      CALL DGAREA(4,HW,VW)
      RETURN
C                                       CROSSED AREA
   69 HW(1)=HO6-DHO6
      HW(4)=HW(1)
      HW(3)=HO6+DHO6
      HW(2)=HW(3)
      VW(1)=VO6-DVO6
      VW(2)=VW(1)
      VW(3)=VO6+DVO6
      VW(4)=VW(3)
      CALL DGAREA(4,HW,VW)
      CALL DGLEVS(0)
      CALL DGDRAW(2,HW,VW(2))
      CALL DGDRAW(2,HW,VW(3))
      CALL DGLEVS(-1)
      RETURN
C
C ---------------------------------------------------------------------
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQPO0
CH
      ENTRY DQPO0(TI22,J22A,J22L,TIS22)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C ---------------------------------------------------------------------
      T22=TI22
      TS22 = TIS22
      L22A=J22A
      L22L=J22L
      IF(L22L.GE.0) THEN
        CALL DGLEVL(L22L)
      ELSE IF(KDEB.EQ.1) THEN
  444   CALL DWRT('DQPO0: Negative color? #')
      END IF
      IF(T22.NE.'LINE') THEN
        CALL DGCLPA(L22A,HLOWDG(IAREDO),VLOWDG(IAREDO),
     &                   HHGHDG(IAREDO),VHGHDG(IAREDO))
      END IF
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQPO0
CH
      ENTRY DQPO1(TI22,J22A,J22L,TIS22,H1D22,V1D22,H2D22,V2D22)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C ---------------------------------------------------------------------
      T22=TI22
      TS22 = TIS22
      L22A=J22A
      L22L=J22L
      CALL DGLEVL(L22L)
      IF(T22.NE.'LINE') CALL DGCLPA(L22A,H1D22,V1D22,H2D22,V2D22)
C     IF(FUISDU) SET DRSTDU TO LINE
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQPOL
CH
      ENTRY DQPOL1(NI22,HI22,VI22)
CH
CH --------------------------------------------------------------------
CH
      DO 622 N=1,NI22
        HW(N)=HI22(N)
        VW(N)=VI22(N)
  622 CONTINUE
      GO TO 623
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQPOL
CH
      ENTRY DQPOL(NI22,HI22,VI22)
CH
CH --------------------------------------------------------------------
CH
      DO 722 N=1,NI22
        IF(FPRSDQ) CALL DQPR0(HI22(N),VI22(N))
        HW(N)=HP(HI22(N),VI22(N))
        VW(N)=VP(HI22(N),VI22(N))
  722 CONTINUE
  623 IF(T22(1:3).NE.'LIN') THEN
        IF(TS22.EQ.'NOCP') THEN
          CALL DGAREA(NI22,HW,VW)
        ELSE
          CALL DGARCL(NI22,HW,VW)
        END IF
      END IF
      IF(T22.NE.'AREA') THEN
        DO 922 N=1,NI22-1
          HH(1)=HW(N)
          VV(1)=VW(N)
          HH(2)=HW(N+1)
          VV(2)=VW(N+1)
          CALL DQCLP(HH(1),VV(1),HH(2),VV(2),FIN)
          IF (TS22.NE.'SKIP') THEN
            IF (FIN) CALL DGDRAW(2,HH,VV)
          ELSE
            IF ((FIN).AND.(N.NE.1.AND.N.NE.3)) CALL DGDRAW(2,HH,VV)
          END IF
  922   CONTINUE
      END IF
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQPOL
CH
      ENTRY DQPOLF(HI22,VI22)
CH
CH --------------------------------------------------------------------
CH
C     ................. DRAW AREA OF 4 CORNERS ENLARGED BY 1.
      DO N=1,4
        IF(FPRSDQ) CALL DQPR0(HI22(N),VI22(N))
        HW(N)=HP(HI22(N),VI22(N))
        VW(N)=VP(HI22(N),VI22(N))
      END DO
      IF(IZOMDO.NE.0) THEN
        DO N=1,4
          IF(HW(N).GT.HLOW.AND.HW(N).LT.HHGH.AND.
     &       VW(N).GT.VLOW.AND.VW(N).LT.VHGH) GO TO 738
        END DO
        RETURN
      END IF
  738 DO K=1,K22
        DO N=1,4
          HH(N)=HW(N)+D22H(K)
          VV(N)=VW(N)+D22V(K)
        END DO
        CALL DGARCL(4,HH,VV)
C       ................ Replacing DGARCL by DGAREA does NOT increas speed!
C        IF(TS22.EQ.'NOCP') THEN
C          CALL DGAREA(4,HH,VV)
C        ELSE
C          CALL DGARCL(4,HH,VV)
C        END IF
      END DO
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQSHA0
CH
      ENTRY DQSHA0(D26I,C26I,C26O,C26L)
CH
CH --------------------------------------------------------------------
CH
      L26I=C26I
      L26O=C26O
      L26L=C26L
      D26 =D26I
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQSHA
CH
      ENTRY DQSHA(Q26,NI26,HI26,VI26,T26)
CH
CH --------------------------------------------------------------------
CH
      IF(L26I.LT.0.AND.L26O.LT.0.) RETURN
      IF(L26I.LT.0.AND.T26.EQ.'I') RETURN
      IF(L26O.LT.0.AND.T26.EQ.'O') RETURN
      DO N=1,NI26
        IF(FPRSDQ) CALL DQPR0(HI26(N),VI26(N))
        HW(N)=HP(HI26(N),VI26(N))
        VW(N)=VP(HI26(N),VI26(N))
      END DO
      HM26=0.5*(HW(1)+HW(NI26))-CHW
C     IF(T26.EQ.'I'.AND.HM26*D26.GE.0.) RETURN
C     IF(T26.EQ.'O'.AND.HM26*D26.LE.0.) RETURN
      IF(T26.EQ.'I') THEN
        CALL DGCLPA(L26I,HLOWDG(IAREDO),VLOWDG(IAREDO),
     &                   HHGHDG(IAREDO),VHGHDG(IAREDO))
      ELSE
        CALL DGCLPA(L26O,HLOWDG(IAREDO),VLOWDG(IAREDO),
     &                   HHGHDG(IAREDO),VHGHDG(IAREDO))
      END IF
      I=2*NI26+1
      HW(I)=HW(1)
      VW(I)=VW(1)
      DQ26=Q26*D26
      DO N=1,NI26
        I=I-1
        HW(I)=HW(N)+DQ26
        VW(I)=VW(N)
      END DO
      N26=NI26*2
      CALL DGARCL(N26+1,HW,VW)
      IF(L26L.GE.0) THEN
        IF(ABS(VW(NI26)-VW(1)).GE.DM26) THEN
          CALL DGLEVL(L26L)
          DO N=NI26,N26
            HH(1)=HW(N)
            VV(1)=VW(N)
            HH(2)=HW(N+1)
            VV(2)=VW(N+1)
            CALL DQCLP(HH(1),VV(1),HH(2),VV(2),FIN)
            IF(FIN) CALL DGDRAW(2,HH,VV)
          END DO
          CALL DGLEVL(L22L)
        END IF
      END IF
      CALL DGCLPA(L22A,HLOWDG(IAREDO),VLOWDG(IAREDO),
     &                 HHGHDG(IAREDO),VHGHDG(IAREDO))
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQPU0
CH
      ENTRY DQPU0(NSI7,DIS7)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:Set up of symbol for DQPU
C
C    Inputs    :NSI7 symbol
C              : Symbols    1   2   3   4   5   6   7   8
C              :            .   A   X   A   A   R   C   A
C    Outputs   :no
C
C    Called by :
C ---------------------------------------------------------------------
      NSO7=NTAB7(NSI7)
      AH7=AHW*DIS7(1)
      BV7=BVW*DIS7(1)
      DH7=DIS7(2)
      DV7=DIS7(2)
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQPU
CH
      ENTRY DQPU(HI7,VI7,DHI7,DVI7)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:draw symbol (defined in DQPU0) with size defined in user space.
C
C    Inputs    :HI7,VI7 = symbol position
C              :DHI7,DVI7 horizontal and vertical symbol size in
C              :user coordinates.
C    Outputs   :no
C
C    Called by : DFTPT,DFTPE
C ---------------------------------------------------------------------
      HO7=HL(HI7,VI7)
      VO7=VL(HI7,VI7)
      IF(HO7.LT.HLOW.OR.HO7.GT.HHGH.OR.
     &  VO7.LT.VLOW.OR.VO7.GT.VHGH) RETURN
      DAH=AH7*DHI7-DH7
      DAV=AVW*DHI7
      DBH=BHW*DVI7
      DBV=BV7*DVI7-DV7
      GO TO (71,72,73,74,75),NSO7
C                                               POINT
   71 CALL DGPOIN(HO7,VO7)
      RETURN
C                                               X
   72 HW(1)=MAX(HLOW,HO7-DAH-DBH)
      HW(2)=MIN(HHGH,HO7+DAH+DBH)
      VW(1)=MAX(VLOW,VO7-DAV-DBV)
      VW(2)=MIN(VHGH,VO7+DAV+DBV)
      CALL DGDRAW(2,HW,VW)
      HW(1)=MAX(HLOW,HO7-DAH+DBH)
      HW(2)=MIN(HHGH,HO7+DAH-DBH)
      VW(1)=MIN(VHGH,VO7-DAV+DBV)
      VW(2)=MAX(VLOW,VO7+DAV-DBV)
      CALL DGDRAW(2,HW,VW)
      RETURN
C                                               RECTANGLE
   73 HW(1)=MAX(HLOW,HO7-DAH-DBH)
      HW(5)=HW(1)
      HW(2)=MIN(HHGH,HO7+DAH-DBH)
      HW(3)=MIN(HHGH,HO7+DAH+DBH)
      HW(4)=MAX(HLOW,HO7-DAH+DBH)
      VW(1)=MAX(VLOW,VO7-DAV-DBV)
      VW(5)=VW(1)
      VW(2)=MAX(VLOW,VO7+DAV-DBV)
      VW(3)=MIN(VHGH,VO7+DAV+DBV)
      VW(4)=MIN(VHGH,VO7-DAV+DBV)
      CALL DGDRAW(5,HW,VW)
      RETURN
C                                               COFFIN
   74 HW(1)=MAX(HLOW,HO7-DAH-DBH)
      HW(5)=HW(1)
      HW(2)=MIN(HHGH,HO7+DAH-DBH)
      HW(7)=HW(2)
      HW(3)=MIN(HHGH,HO7+DAH+DBH)
      HW(6)=HW(3)
      HW(4)=MAX(HLOW,HO7-DAH+DBH)
      HW(8)=HW(4)
      VW(1)=MAX(VLOW,VO7-DAV-DBV)
      VW(5)=VW(1)
      VW(2)=MAX(VLOW,VO7+DAV-DBV)
      VW(7)=VW(2)
      VW(3)=MIN(VHGH,VO7+DAV+DBV)
      VW(6)=VW(3)
      VW(4)=MIN(VHGH,VO7-DAV+DBV)
      VW(8)=VW(4)
      CALL DGDRAW(8,HW,VW)
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQARU
CH
      ENTRY DQARU(HI7,VI7,DHI7,DVI7)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:draw area fast. Area size defined in user space.
C
C    Inputs    :HI7,VI7 = AREA CENTER POSITION
C              :DHI7,DVI7 horizontal and vertical symbol size in
C              :user coordinates.
C    Outputs   :NO
C
C    Called by :?
C ---------------------------------------------------------------------
      HO7=HL(HI7,VI7)
      VO7=VL(HI7,VI7)
      IF(HO7.LT.HLOW.OR.HO7.GT.HHGH.OR.
     &  VO7.LT.VLOW.OR.VO7.GT.VHGH) RETURN
      DAH=AHW*DHI7
      DAV=AVW*DHI7
      DBH=BHW*DVI7
      DBV=BVW*DVI7
C                                                 AREA
   75 HW(1)=MAX(HLOW,HO7-DAH-DBH)
      HW(2)=MIN(HHGH,HO7+DAH-DBH)
      HW(3)=MIN(HHGH,HO7+DAH+DBH)
      HW(4)=MAX(HLOW,HO7-DAH+DBH)
      VW(1)=MAX(VLOW,VO7-DAV-DBV)
      VW(2)=MAX(VLOW,VO7+DAV-DBV)
      VW(3)=MIN(VHGH,VO7+DAV+DBV)
      VW(4)=MIN(VHGH,VO7-DAV+DBV)
      CALL DGAREA(4,HW,VW)
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQPOC
CH
      ENTRY DQPOC(HI2,VI2,HO2,VO2,FO2)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:calculate display coordinates.
C    Inputs    :HI2,VI2 = point coordinates in user space.
C    Outputs   :HO2,VO2 = point coordinates in display space.
C
C ---------------------------------------------------------------------
      IF(FPRSDQ) CALL DQPR0(HI2,VI2)
      HO2=HP(HI2,VI2)
      VO2=VP(HI2,VI2)
      IF(HO2.LT.HLOW.OR.HO2.GT.HHGH.OR.
     &  VO2.LT.VLOW.OR.VO2.GT.VHGH) THEN
         FO2=.FALSE.
      ELSE
         FO2=.TRUE.
      END IF
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQRA0
CH
      ENTRY DQRA0(NSI10,LSI10)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:set up symbol and colour for DQRAR
C
C    Inputs    :NSI10=1 rectangle , =2 area. , 3=BOTH
C               lsi10 = col.POSITION in PDCODD = col of area if 3
C
C    Outputs   :no
C
C    Called by :rotated histograms
C ---------------------------------------------------------------------
C     NSI10 -> NSO10  : 1->1  2->3  3->2
      NSO10=ITO10(NSI10)
      IF(NSI10.EQ.1) THEN
        CALL DQLEVL(LSI10)
      ELSE
        LSO10=ABS(PDCODD(2,LSI10))
        CALL DGCLPA(LSO10,HLOWDG(IAREDO),VLOWDG(IAREDO),
     &                    HHGHDG(IAREDO),VHGHDG(IAREDO))
      END IF
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQRA0
CH
      ENTRY DQRA1(NSI10,LSI10)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:set up symbol for DQRAR
C
C    Inputs    :NSI10=1 rectangle , =2 area. 3 = both
C               lsi10= COLOUR = colour of area if both
C    Outputs   :no
C
C    Called by :rotated histograms
C ---------------------------------------------------------------------
C     NSI10 -> NSO10  : 1->1  2->3  3->2
      NSO10=ITO10(NSI10)
      IF(NS110.EQ.1) THEN
        CALL DGLEVL(LSI10)
      ELSE
        CALL DGCLPA(LSI10,HLOWDG(IAREDO),VLOWDG(IAREDO),
     &                    HHGHDG(IAREDO),VHGHDG(IAREDO))
      END IF
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQRAR
CH
      ENTRY DQRAR(HI10,VI10)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:Draw 4 corner rectangle or area (defined by DQRA0) rotated.
C
C    Inputs    :HI10(4),VI10(4) corners of polyline.
C    Outputs   :no
C
C    Called by :rotated histograms
C ---------------------------------------------------------------------
      DO 610 K=1,4
        IF(FPRSDQ) CALL DQPR0(HI10(K),VI10(K))
        HW(K)=HP(HI10(K),VI10(K))
        VW(K)=VP(HI10(K),VI10(K))
  610 CONTINUE
      IF(NSO10.GE.2) CALL DGARCL(4,HW,VW)
      IF(NSO10.LE.2) THEN
        HW(5)=HW(1)
        VW(5)=VW(1)
        DO 611 K=1,4
          HH(1)=HW(K)
          VV(1)=VW(K)
          HH(2)=HW(K+1)
          VV(2)=VW(K+1)
          CALL DQCLP(HH(1),VV(1),HH(2),VV(2),FIN)
          IF(.NOT.FIN) GO TO 611
          CALL DGDRAW(2,HH,VV)
  611   CONTINUE
      END IF
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQSQ0
CH
      ENTRY DQSQ0(QI19,DIS19)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:SET UP VARIABLE SQUARE SIZE
C    Inputs    : QI19 = SCALE FOR VARIBLE SQUARE
C    Outputs   :no
C
C    Called by :DFTPE AND DFTPH
C ---------------------------------------------------------------------
      QO19=QI19
      DHS19=DIS19(2)
      DVS19=DIS19(2)
C     ........................................... ABS INSTALLED 18.2.92
      AH19=ABS(AHW*DIS19(1))
      BV19=ABS(BVW*DIS19(1))
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQSQ1
CH
      ENTRY DQSQ1(DHI19,DVI19,SO19)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:SET UP VARIABLE SQUARE SIZE
C    Inputs    : QI19 = SCALE FOR VARIBLE SQUARE
C    Outputs   :no
C
C    Called by :DFTPE AND DFTPH
C ---------------------------------------------------------------------
      DH19=AH19*DHI19-DHS19
      DV19=BV19*DVI19-DVS19
      SO19=DH19*DV19
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQSQ
CH
      ENTRY DQSQ(HI19,VI19,SI19,DHI19,DVI19)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:draw square of variable size, CALL DQSQ0 before
C    Inputs    :HI19,VI19 = area position , SI19 = area-size
C              :DHI19,DVI19 = maximum dimensions of area
C    Outputs   :no
C
C    Called by :DFTPE AND DFTPH
C ---------------------------------------------------------------------
      HO19=HL(HI19,VI19)
      VO19=VL(HI19,VI19)
      IF(HO19.LT.HLOW.OR.HO19.GT.HHGH.OR.
     &  VO19.LT.VLOW.OR.VO19.GT.VHGH) RETURN
      F19=.TRUE.
      S19=SI19*QO19

c      DH19=AH19*DHI19-DHS19
c      DV19=BV19*DVI19-DVS19                    9-mar-1999

      DH19=ABS(AH19*DHI19-DHS19)
      DV19=ABS(BV19*DVI19-DVS19)

      DHO19=MAX(SM19,SQRT(S19))
      DVO19=DHO19
      IF(DHO19.GT.DH19) THEN
         DHO19=DH19
         DVO19=S19/DHO19
         IF(DVO19.GT.DV19) THEN
            DVO19=DV19
            F19=.FALSE.
         END IF
      ELSE IF(DVO19.GT.DV19) THEN
         DVO19=DV19
         DHO19=S19/DVO19
         IF(DHO19.GT.DH19) THEN
            DHO19=DH19
            F19=.FALSE.
         END IF
      END IF
      HW(1)=MAX(HLOW,HO19-DHO19)
      HW(3)=MIN(HHGH,HO19+DHO19)
      HW(2)=HW(3)
      HW(4)=HW(1)
      VW(1)=MAX(VLOW,VO19-DVO19)
      VW(3)=MIN(VHGH,VO19+DVO19)
      VW(2)=VW(1)
      VW(4)=VW(3)
      CALL DGAREA(4,HW,VW)
      IF(F19) RETURN
      IF(DHO19.LT.DMX19.OR.DVO19.LT.DMX19) RETURN
      CALL DGLEVS(0)
      CALL DGDRAW(2,HW,VW(2))
C      CALL DGDRAW(2,HW,VW(3))
      CALL DGLEVS(-1)
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQSR
CH
      ENTRY DQSR(HI19,VI19,SI19)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:draw square OUTLINE of variable size, CALL DQSQ0 before
C    Inputs    :HI19,VI19 = area position , SI19 = area-size
C    Outputs   :no
C
C    Called by :DAPEO
C ---------------------------------------------------------------------
      HO19=HL(HI19,VI19)
      VO19=VL(HI19,VI19)
      IF(HO19.LT.HLOW.OR.HO19.GT.HHGH.OR.
     &  VO19.LT.VLOW.OR.VO19.GT.VHGH) RETURN
      F19=.TRUE.
      S19=SI19*QO19
      DHO19=MAX(SM19,SQRT(S19))
      DVO19=DHO19
      HW(1)=MAX(HLOW,HO19-DHO19)
      HW(3)=MIN(HHGH,HO19+DHO19)
      HW(2)=HW(3)
      HW(4)=HW(1)
      HW(5)=HW(1)
      VW(1)=MAX(VLOW,VO19-DVO19)
      VW(3)=MIN(VHGH,VO19+DVO19)
      VW(2)=VW(1)
      VW(4)=VW(3)
      VW(5)=VW(1)
      DLINDD=PDCODD(2,LIGLDD)
      CALL DGDRAW(5,HW,VW)
      DLINDD=PDCODD(2,LITRDD)
      RETURN
CH..............---
CH
      ENTRY DQTXT(HI13,VI13,T13,N13)
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C_CG 10-May-1989 C.Grab  Adapted to CERNVM
C
C
C
C Draw 4 corner rectangle or area (defined by DQRA0) rotated.
C
C    Inputs    :HI13,VI13 text position. T13=text . N13=#of letters.
C    Outputs   :no
C
C ---------------------------------------------------------------------
      HO13=HL(HI13,VI13)
      VO13=VL(HI13,VI13)-DV13
      IF (HO13.GE.HLOW .AND. HO13.LE.HHGH .AND.
     1    VO13.GE.VLOW .AND. VO13.LE.VHGH) THEN
         CALL DGTEXT(HO13,VO13,T13,N13)
      ENDIF
      RETURN
CH..............---
CH
      ENTRY DQTXT0(DHI13,DVI13)
CH
C ---------------------------------------------------------------------
C
      DHO13=DHI13
      DVO13=DVI13
      RETURN
CH..............---
CH
      ENTRY DQTXTS(HI13,VI13,T13,N13)
CH
C ---------------------------------------------------------------------
C
      HO13=HL(HI13,VI13)
      VO13=VL(HI13,VI13)-DV13
      IF (HO13.GE.HLOW .AND. HO13.LE.HHGH .AND.
     1    VO13.GE.VLOW .AND. VO13.LE.VHGH) THEN
         CALL DGTEXT(HO13+DHO13,VO13+DVO13,T13,N13)
      ENDIF
      END
C
*DK DQVRT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQVRT
CH
      SUBROUTINE DQVRT(H,V)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DATA SIZ/30./
      IF(.NOT.FPIKDP.AND.BNUMDB(4,PYERDB).LE.0.) THEN
         CALL DGLEVL(12)
         CALL DQPD0(3,SIZ,0.)
         CALL DQPD(H,V)
      END IF
      END
*DK DQWIN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQWIN
CH
      SUBROUTINE DQWIN(HI1,VI1,HI2,VI2,DIV13)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION NHM(0:12),NHH(0:12),NVM(0:12),NVH(0:12),H(4),V(3)
C              W 1 2 3 4 5 6 U D L M R S
      DATA NHM/1,1,1,2,2,3,3,1,1,1,2,3,1/
      DATA NHH/4,2,2,3,3,4,4,3,3,2,3,4,3/
      DATA NVM/1,2,1,2,1,2,1,2,1,1,1,1,1/
      DATA NVH/3,3,2,3,2,3,2,3,2,3,3,3,3/
      V1=VI1
      V2=VI2
      H1=HI1
      H2=HI2
      DV13=DIV13
      HMINDG(14)=1.
      VMINDG(14)=1.
      HHGHDG(14)=H2
      VHGHDG(14)=MAX(2.,V1)
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQWIS
CH
      ENTRY DQWIS
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
      IF(WISUDW.LE.0.) THEN
        H(1)=H1
        H(4)=H2
        IF(TWINDW(5).EQ.'*') THEN
           H(2)=0.5*(H1+H2)
           H(3)=H2
        ELSE
           DH=(H2-H1)/3.
           H(2)=H1+DH
           H(3)=H(2)+DH
        END IF
        V(1)=V1
        V(2)=0.5*(V1+V2)
        V(3)=V2
        DO   700  K=0,12
          HMINDG(K)=H(NHM(K))
          HHGHDG(K)=H(NHH(K))
          VMINDG(K)=V(NVM(K))
          VHGHDG(K)=V(NVH(K))
  700   CONTINUE
C        TWINDW( 7)='U'
C        TAREDO( 7)='WU'
C        TWINDW( 8)='D'
C        TAREDO( 8)='WD'
C        TWINDW(10)='M'
C        TAREDO(10)='WM'
C        TWINDW(12)='S'
C        TAREDO(12)='WS'
      ELSE
        HM=0.5*(H1+H2)
        VL=V2-HM
C       .............................. W
        HMINDG( 0)=H1
        HHGHDG( 0)=H2
        VMINDG( 0)=V1
        VHGHDG( 0)=V2+DV13
C       .............................. U
        HMINDG( 7)=H1
        HHGHDG( 7)=H2
        VMINDG( 7)=VL
        VHGHDG( 7)=V2
C       .............................. D
        HMINDG( 8)=H1
        HHGHDG( 8)=H2
        VMINDG( 8)=V1
        VHGHDG( 8)=VL
C       .............................. L
        HMINDG( 9)=H1
        HHGHDG( 9)=HM
        VMINDG( 9)=VL
        VHGHDG( 9)=V2
C       .............................. M
        HMINDG(10)=H(NHM(9))
        HHGHDG(10)=H(NHH(9))
        VMINDG(10)=V(NVM(9))
        VHGHDG(10)=V(NVH(9))
C       .............................. R
        HMINDG(11)=HM
        HHGHDG(11)=H2
        VMINDG(11)=VL
        VHGHDG(11)=V2
C       .............................. S
        HMINDG(12)=H(NHM(10))
        HHGHDG(12)=H(NHH(11))
        VMINDG(12)=V(NVM(12))
        VHGHDG(12)=V(NVH(12))
        HH=H1
        DH=(H2-H1)/6.
        DO 710 I=1,6
          HMINDG(I)=HH
          HHGHDG(I)=HH+DH
          VMINDG(I)=V1
          VHGHDG(I)=VL
          HH=HH+DH
  710   CONTINUE
C       TWINDW( 7)='*'
C       TAREDO( 7)='**'
C       TWINDW( 8)='*'
C       TAREDO( 8)='**'
C       TWINDW(10)='*'
C       TAREDO(10)='**'
C       TWINDW(12)='*'
C       TAREDO(12)='**'
      END IF
      IF(DV13.EQ.0.) THEN
        VHGHDG(13)=0.
      ELSE
        HMINDG(13)=H1
        HLOWDG(13)=H1
        HHGHDG(13)=H2
        VMINDG(13)=V2
        VLOWDG(13)=V2
        VHGHDG(13)=V2+DV13
      END IF
      BORD=1.
      GO TO 1
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DQWIL
CH
      ENTRY DQWIL(BO)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     CALCULATE LOW WINDOW
      BORD=BO
    1 IF(BORD.NE.1..OR.
     &  (WISUDW.GT.0..AND.IAREDO.GE.1..AND.IAREDO.LE.6) ) THEN
         DH=0.
         DV=0.
      ELSE
         DH=POSIDW(10)
         DV=POSIDW(11)
      END IF
      DO   770  K=0,12
         HLOWDG(K)=HMINDG(K)+DH
         VLOWDG(K)=VMINDG(K)+DV
  770 CONTINUE
      END
*DK DQWRT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQWRT
CH
      SUBROUTINE DQWRV(NPOS,TV3)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *3 TV3
      DIMENSION HH(4),VV(4)
      H=HHGHDG(IAREDO)-POSIDW(21)*2.
      V=VHGHDG(IAREDO)-NPOS*POSIDW(21)
      IF(TV3.EQ.'BLK') THEN
        HH(1)=H
        HH(3)=HHGHDG(IAREDO)
        HH(2)=HH(1)
        HH(4)=HH(3)
        VV(1)=VHGHDG(IAREDO)
        VV(3)=V-0.5*POSIDW(21)
        VV(2)=VV(3)
        VV(4)=VV(1)
        CALL DGLEVS(0)
        CALL DGAREA(4,HH,VV)
        CALL DQLEVL(ICFRDD)
        CALL DGDRAW(3,HH,VV)
      ELSE
        CALL DGTEXT(H,V,TV3,3)
      END IF
      END
*DK DQSYMB
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQSYMB
CH
      SUBROUTINE DQSYMB(HS,VS,DS,TS)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:   DRAW + OR -
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      CHARACTER *(*) TS
      DIMENSION H(2),V(2)
      LOGICAL FIN
      CALL DQPOC(HS,VS,HH,VV,FIN)
      IF(FIN) THEN
        H(1)=HH-DS
        H(2)=HH+DS
        V(1)=VV
        V(2)=VV
        CALL DGDRAW(2,H,V)
        IF(TS.EQ.'+') THEN
          H(1)=HH
          H(2)=HH
          V(1)=VV+DS
          V(2)=VV-DS
          CALL DGDRAW(2,H,V)
        END IF
      END IF
      END
*DK DQCWIN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DQCWIN
CH
      SUBROUTINE DQCWIN(NWUS,H1,V1,TITLE,TNAME)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TITLE,TNAME
      DIMENSION IX(4),IY(4),HW(4),VW(4)
      DATA IWUS/0/,HVMIN/40./HMAX/1000./,VMAX/700./
      CHARACTER *1 TWUS(4)
      DATA TWUS /'A','B','C','D'/
      LOGICAL FWUS(4),FOUT
      DATA FWUS/4*.FALSE./
      IF(NWUS.NE.0) THEN
        IF(NWUS.EQ.IWUSDO) GO TO 1
        IF(FWUS(NWUS)) THEN
          CALL DGSWIN(NWUS,KWUS,0)
          IF(NWUS.NE.KWUS) CALL DWRT('DQCWIN error.##')
          IWUSDO=KWUS
          CALL DGIFOC(0,IWUSDO)
        ELSE
          CALL DPAR_GET_ARRAY(101,'UX1',4,IX)
          CALL DPAR_GET_ARRAY(101,'UY1',4,IY)
          HH=MIN(HMAX,MAX(H1,HVMIN))
          VV=MIN(VMAX,MAX(H1,HVMIN))
          CALL DGCWIN(1.,1.,H1,V1,IX(NWUS),IY(NWUS),NWUS,TITLE,TNAME)
          HW(NWUS)=H1
          VW(NWUS)=V1
          IWUSDO=NWUS
        END IF
        FWUS(NWUS)=.TRUE.
      ELSE
        GO TO 1
      END IF
      JWUSDW=IWUSDO
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------  DQSWIN
CH
      ENTRY DQSWIN(NWUS,LWUS)
CH
CH --------------------------------------------------------------------
CH
      LWUS=IWUSDO
    1 IF(NWUS.EQ.0) THEN
        CALL DGSWIN(NWUS,KWUS,0)
        IWUSDO=0
        IWARDO=0
      ELSE
        IF(FWUS(NWUS)) THEN
          CALL DGSWIN(NWUS,KWUS,0)
          IF(NWUS.NE.KWUS) CALL DWRT('DQSWIN error.##')
          IWUSDO=KWUS
        ELSE
          TXTADW='DALI-Window'//TWUS(NWUS)//' does not exist.#'
          CALL DWRC
          IWUSDO=0
        END IF
      END IF
      CALL DGIFOC(0,IWUSDO)
      JWUSDW=IWUSDO
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------  
CH
      ENTRY DQDWIN(NWUS)
CH
CH --------------------------------------------------------------------
CH
      IF(NWUS.GT.0.AND.FWUS(NWUS)) THEN
        FWUS(NWUS)=.FALSE.
        CALL DGDWIN(NWUS,KWUS)
        IWUSDO=KWUS
        CALL DGIFOC(0,IWUSDO)
        IWARDO=-1
        HW(NWUS)=0.
        VW(NWUS)=0.
      END IF
      JWUSDW=IWUSDO
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------  DQSWIN
CH
      ENTRY DQPWIN(NWUS,HIN,VIN,FOUT)
CH
CH --------------------------------------------------------------------
CH
      FOUT=.TRUE.
      IF(NWUS.EQ.0) THEN
        IF(HIN.LE.0..OR.
     &     VIN.LE.0..OR.
     &     HIN.GT.POSIDW(8).OR.
     &     VIN.GT.POSIDW(9)) RETURN
      ELSE
        IF(HIN.LE.0..OR.
     &     VIN.LE.0..OR.
     &     HIN.GT.HW(NWUS).OR.
     &     VIN.GT.VW(NWUS)) RETURN
      END IF
      FOUT=.FALSE.
      END
*DK DQ_TOGGLE_HELP_DALI
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++ DQ_TOGGLE_HELP_DALI
CH
      SUBROUTINE DQ_TOGGLE_HELP_DALI
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DATA IDALI/0/,IHLP/2/,JHL/0/
C     DATA ITERM/1/
      CALL DGQINF(MODW,IWIN)
      IF(     MODW.EQ.IDALI) THEN
        CALL DGPOP('HPOP')
        CALL DGIFOC(IHLP,JHL)
      ELSE
        CALL DGSWIN(0,KWUS,0)
        CALL DGIFOC(IDALI,0)
      END IF
      END
*DK DQ_CHANGE_WINDOW
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DQ_CHANGE_WINDOW
CH
      SUBROUTINE DQ_CHANGE_WINDOW(IAR,TSTOR,H1,V1,H2,V2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TSTOR
      IF(TSTOR.EQ.'STORE') THEN
        HS1=HMINDG(IAR)
        HS2=HHGHDG(IAR)
        VS1=VMINDG(IAR)
        VS2=VHGHDG(IAR)
        NAR=IAREDO
        IAREDO=IAR
        LAR=IAR
      END IF
      HMINDG(IAR)=H1
      HHGHDG(IAR)=H2
      VMINDG(IAR)=V1
      VHGHDG(IAR)=V2
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------  
CH
      ENTRY DQ_CHANGE_WINDOW_BACK
CH
CH --------------------------------------------------------------------
CH
      HMINDG(LAR)=HS1
      HHGHDG(LAR)=HS2
      VMINDG(LAR)=VS1
      VHGHDG(LAR)=VS2
      IAREDO=NAR
      END

*DK DQCHKX
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DQCHKX
CH
      SUBROUTINE DQCHKX
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      IF(.NOT.FPIKDP) CALL DGCHKX
      END

