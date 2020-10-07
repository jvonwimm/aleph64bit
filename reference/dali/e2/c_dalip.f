*DK DPA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPA
CH
      SUBROUTINE DPA
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
      CHARACTER *3 TP
      CHARACTER *8 TCOM
      CHARACTER *28 TMAC
      CHARACTER *49 T1
C
C              123456789 123456789 123456789 123456789 123456789
      DATA T1/'P?: Type 3 letter pagename ,    GB=<CR>'/
      CALL DTYPT('TYPE',TPICDO, 0    ,  0   ,  0   ,  ' ' ,T1)
      CALL DQHL_MENU('PA0')
      CALL DGETLN(TP,LN,3)
      IF(     LN.LE.1) THEN
        TPICDO='GB'
        RETURN
      END IF
      IF(LN.EQ.2) TP(3:3)='0'
      CALL DQH_PAGE(TP,TCOM)
      IF(TCOM.EQ.' ') THEN
        TPICDO=TP(1:2)
        IF(TP(1:2).EQ.'GT') THEN
          IF(TP(3:3).EQ.'0') CALL DWRT(
     &      'Second part of page is found in DALI with "GTC".#')
          TPRGDH=TP(3:3)
        ELSE
          TAN1DH=TP(1:3)
        END IF
      ELSE
        TPICDO=TCOM(1:2)
        CALL DGINMA(TCOM(3:8))
      END IF
      END
*DK DPCEWI
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPCEAR
CH
      SUBROUTINE DPCEWI(NAR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:Parameter Copy: Execute all pictures on ARea "nar"
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DATA NOCL/0/
      LOGICAL F0
      CALL DPCGAR(NAR,F0)
      IF(F0) THEN
        CALL DWRT(' No picture on '//TAREDO(NAR))
      ELSE
        NOCLDT=NOCL
        CALL DBR2
      END IF
      END
*DK DPIBEG
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPIBEG
CH
      SUBROUTINE DPIBEG(NAR,H,V,FOUT)
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
      LOGICAL FOUT
      CALL DPCGAR_NEW(IWUSDO,NAR,FOUT)
      DPIKDP=999999.
      NPIKDP=0.
      HPIKDP=H
      VPIKDP=V
      RETURN
      END
*DK DPIFT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPIFT
CH
      SUBROUTINE DPIFT(NUMT,H,V,NHIT,MODUL,FI,TE)
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
      LOGICAL FOUT,FIN
      CALL DPOAR(H,V,NAR)
      IF(NAR.LT.0) RETURN
      CALL DPIBEG(NAR,H,V,FOUT)
      IF(FOUT) RETURN
      IARE=IAREDO
      IAREDO=NAR
      CALL DBR2
      CALL DPIGET(NHIT,MODUL,H,V,FIN)
      IF(FIN) THEN
         FFI=DVCO(IVFIDV,NHIT,MODUL)
         IF(FFI.NE.-999999.) FI=FFI
         TTE=DVCO(IVTEDV,NHIT,MODUL)
         IF(TTE.NE.-999999.) TE=TTE
         FPOSDT=FI
         TPOSDT=TE
      ELSE
         MODUL=0
         CALL DWRT('Neither Phi nor Theta are defined.')
         NUMT=0
         IAREDO=IARE
         RETURN
      END IF
      IAREDO=IARE
      RETURN
      END
*DK DPIGET
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPIGET
CH
      SUBROUTINE DPIGET(NHIT,MODUL,H,V,FIN)
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
      LOGICAL FIN
      IF(DPIKDP.LT.999999.) THEN
         NHIT=NPIKDP
C        12,1989:It is now unclear to me why MODUL=MDLPDP and not =MDLRDP.
C        Check later!
         MODUL=MDLPDP
         FIN=.TRUE.
         H=HHPKDP
         V=VVPKDP
      ELSE
         FIN=.FALSE.
      END IF
      RETURN
      END
*DK DPIMOV
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPIMOV
CH
      SUBROUTINE DPIMOV(NUMT,NHIT,MODUL,NDIR,H,V,FIN)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modified by B.S. Nilsson                  June 1989
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C   Return coordinates (H,V) for the hit NHIT,MODUL in previous, present
C   or next window (NDIR=-1,0,+1)
C
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DATA FP/0./,TP/0./
      DIMENSION NLST(5)
      DATA MEF/7000/,NLST/5,0,0,0,1/
      LOGICAL FIN,FOUT
      IARE=IAREDO
      IF(NDIR.EQ.0) THEN
         FPOSDT=FP
         TPOSDT=TP
         CALL DPOAR(H,V,NAR)
         IF(NAR.LT.0) GO TO 5
         IAR=NAR
         GO TO 3
      ELSE
         CALL DPIFT(NUMT,H,V,NHIT,MODUL,FPOSDT,TPOSDT)
         CALL DPOAR(H,V,NAR)
         IF(NAR.LT.0) GO TO 5
         IAR=NAR
    1    IF(NDIR.EQ.1) THEN
            DO K=IAR+1,MPARDS
               IF(ISTODS(4,K,IWUSDO).GT.0) GO TO 2
            END DO
            DO K=0,IAR
               IF(ISTODS(4,K,IWUSDO).GT.0) GO TO 2
            END DO
         ELSE
            DO K=IAR-1,0,-1
               IF(ISTODS(4,K,IWUSDO).GT.0) GO TO 2
            END DO
            DO K=MPARDS,IAR,-1
               IF(ISTODS(4,K,IWUSDO).GT.0) GO TO 2
            END DO
         END IF
         GO TO 5
      END IF
    2 IAR=K
C   3 CALL DPCGVA(IAR,FOUT)
    3 CALL DPCGAR_NEW(IWUSDO,IAR,FOUT)
      IF(FOUT) GO TO 4
      CALL DPIBEG(IAR,H,V,FOUT)
      IF(FOUT) GO TO 4
      FPIMDP=.TRUE.
      NPIKDP=NHIT
      MDLPDP=MODUL
      JPIK=NHIT
      JDLP=MODUL
      CALL DBR2
C     .......... Assumption: no ALPHA tracks and FRFT tracks on 1 window.
      IF(DPIKDP.EQ.999999.) THEN
C       .... Move between FRFT and ALPHA EF and ALPHA CH
        IF(     MODUL.EQ.FRFTDB) THEN
C         ............................. search in ALPHA CH,EF list
          DO LIST=5,1,-4
            CALL DV_QVEC_GET_INDEX(LIST,NHIT,NPIKDP)
            IF(NPIKDP.GT.0) THEN
              MDLPDP=MEF
              CALL DBR2
              IF(DPIKDP.NE.999999.) GO TO 10
            END IF
          END DO
        ELSE IF(MODUL.EQ.MEF) THEN
          CALL DV_QVEC_GET_TRACK(NHIT,LIST,NTRAK)
          IF(NTRAK.GT.0) THEN
            LIST=NLST(LIST)
            CALL DV_QVEC_GET_INDEX(LIST,NTRAK,NPIKDP)
            IF(NPIKDP.GT.0) CALL DBR2
            IF(DPIKDP.EQ.999999.) THEN
              MDLPDP=FRFTDB
              NPIKDP=NTRAK
              IPTNDP=1
              CALL DBR2
              IPTNDP=0
            END IF
          END IF
        END IF
      END IF
   10 FPIMDP=.FALSE.
      NPIKDP=JPIK
      MDLPDP=JDLP
      CALL DPIGET(NHIT,MODUL,H,V,FIN)
      IF(FIN) GO TO 6
      CALL DWRT('Selected object not found. Select right one or')
    4 IF(IAR.NE.NAR) GO TO 1
    5 FIN=.FALSE.
      CALL DWRT('position cursor to right window.')
    6 IAREDO=IARE
      NUMT=NUMT+1
      END
*DK DPIPOS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPIPOS
CH
      SUBROUTINE DPIPOS(ANFI,ANTE,NEXEC,CHG,TANSW)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modified by B.S. Nilsson                  June 1989.
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C INVOKED BY TANSW.EQ.'PI'  (DPIPOS)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      LOGICAL CHG,FIN
      LOGICAL NYES
      CHARACTER *2 TANSW
      DIMENSION PLAB(4,2)
      DATA PLAB/-9.,0.,99.,0.,   -1.,0.,15.,0./
      CHARACTER *2 TPR2(3)
      DATA TPR2/'RO','DZ','TR'/
      DIMENSION PR(4,3)
      DATA PR/ 1.,1.,9999.,0.,  -99.,0.,99.,0.,   1.,1.,99.,-1./
      DATA NHIT/1/,MODUL/6/
      EQUIVALENCE (H,HPOSDT),(V,VPOSDT)
      CHARACTER TT1*1
      CHARACTER *2 DT2,TLAB(2)
      CHARACTER *3 DT3
      CHARACTER *5 DT5
      DATA TLAB/'LA','CO'/
      DATA DHD/0./,DVD/0./
      INTEGER BUTSTA,KEYBF1
      LOGICAL FOUT
      LOGICAL FBOX
      REAL LOWX,LOWY,PXX,PYY,HPOPOS,VPOPOS
      COMMON /ASTCM1/ BUTSTA,KEYBF1(4),FBOX,LOWX,LOWY,PXX(5),PYY(5),
     & HPOPOS,VPOPOS

      CHARACTER ICHSTR*2
      INTEGER LCHSTR
CVV      LOGICAL FFIX
      REAL HOLD,VOLD,HCU,VCU
      IF(NTVIDT.EQ.0) RETURN
      CALL DAC_PICK_0
      NUMPDP=0
      NXYZDP=0
      TRK=-99999.
      CALL DCOPTL
      FPIKDP=.TRUE.
      CALL DWRT('>PIck')
      CALL DQHLP('PI ')
      NUMT=0
      NOARDO=1
      AFI=ANFI
      ATE=ANTE
      FIOLD=AFI
      TEOLD=ATE
      FI=AFI
      TE=ATE
      CHG=.FALSE.
      H=HOLD
      V=VOLD
  936 D=(VHGHDG(1)-VLOWDG(1))/200.
      HOLD=H
      VOLD=V
      NEXEC=2
      HPI=H
      VPI=V
      IF(TANSW.EQ.'PF') THEN
CVV         CALL DGGCUR(H,V)
         CALL DGCURG(H,V)
         TANSW='ST'
         GO TO 920
      END IF
      CALL DGSCUR(H,V)
C
C   Change cursor and then wait for either a click on the middle button
C   or for input.
C
CVV      CALL DGIAST(3,1)
CVV      FFIX=.FALSE.
  937 CALL DWRT_END(2)
      FPROD0=.FALSE.
CVV      IF(.NOT.FMACDM) CALL DGETST(ICHSTR,LCHSTR,1,1)

CVV      IF(BUTSTA.EQ.3) THEN
CVVC
CVVC   Mouse button M2 was pushed.
CVVC
CVV         HCU=HPOPOS
CVV         VCU=VPOPOS
CVV         CALL DGSCUR(HCU,VCU)
CVVC  Fix cursor and wait for more input.
CVV         CALL DGFXCU(3,1)
CVV         CALL DGSPPT(3,2)
CVV         FFIX=.TRUE.
CVV         BUTSTA=1
CVV         GO TO 937
CVV      ELSE
CVV         IF(FFIX) CALL DGEAST(3,2)
CVV         IF(.NOT.FFIX) CALL DGEAST(3,1)
CVV      END IF
CVV  938 BUTSTA=0
  938 PR(2,1)=NHIT
      PR(2,2)=VRDZDV
      CHG=.FALSE.
      TAN1DH='PI'
      IF(TRK.NE.-99999..AND.TRK.NE.0.) THEN
         PLAB(4,1)=-1.
         PLAB(4,2)=-1.
         NLAB=2
      ELSE
         NLAB=0
      END IF
      CALL DOPER(NTVIDT,0,
     &  1,NLAB,TLAB,PLAB,
     &  1,3,TPR2,PR,
     &  NEXEC,CHG,TANSW)
      IF(CHG) THEN
         IF(TRK.NE.-99999..AND.TRK.NE.0.) THEN
            NTRK=ABS(TRK)
            IF(PLAB(4,1).EQ.1.) THEN
               CALL DWRT('Label of track '//DT3(FLOAT(NTRK))//
     &          ' changed from '//DT2(ULABDT(NTRK))//' to '//
     &          DT2(PLAB(2,1)))
               ULABDT(NTRK)=PLAB(2,1)
               IF(NEXEC.EQ.3) GO TO 936
            END IF
            IF(PLAB(4,2).EQ.1.) THEN
               CALL DWRT('Color of track '//DT3(FLOAT(NTRK))//
     &          ' changed from '//DT2(COLRDT(NTRK))//' to '//
     &          DT2(PLAB(2,2)))
               COLRDT(NTRK)=PLAB(2,2)
               CALL DSC0
               IF(NEXEC.EQ.3) GO TO 936
            END IF
         END IF
         IF(PR(2,2).NE.VRDZDV) THEN
            VRDZDV=PR(2,2)
            CALL DWRT('DZ of vertex = '//DT5(VRDZDV))
            CALL=DVTPCL(IVTEDV)
         END IF
         NUMT=0
         IF(PR(4,3).LE.0.) THEN
           NHIT=MIN(BNUMDB(2,MODUL),PR(2,1))
         ELSE
           PR(4,3)=-1.
           MODUL=FRFTDB
           NHIT=MIN(BNUMDB(2,MODUL),PR(2,3))
         END IF
      END IF
CVV      CALL DGUXCU(3,1)
CVV      IF(FFIX) THEN
CVV        H=HCU
CVV        V=VCU
CVV      ELSE
CVV      CALL DGGCUR(H,V)
CVV      END IF
      FFI=DVCO(IVFIDV,NHIT,MODUL)
      TTE=DVCO(IVTEDV,NHIT,MODUL)
      IF(FFI.NE.-999999.) FI=FFI
      IF(TTE.NE.-999999.) TE=TTE
      NUMT=NUMT+1
      IF(NUMT.GE.9) THEN
         CALL DWRT(' ')
         NUMT=0
      END IF
      IF(TANSW.EQ.'GB') THEN
         NEXEC=3
         GO TO 910
      END IF
      GO TO (910,920,930,920),NEXEC
  910 IF(TANSW.NE.'GW') THEN
        CALL DCOPFL
        FPIKDP=.FALSE.
      END IF
      IF(TANSW.EQ.'ST') THEN
        ANFI=FI
        ANTE=TE
      END IF
CVV      BUTSTA=0
      CALL DGSPPT(1,1)
      TAN1DH='  '
      FPIKDP=.FALSE.
      RETURN
  920 CALL DGQINF(MODWI,NUMWI)
      IF(MODWI.EQ.0) CALL DQSWIN(NUMWI,LDUM)
      CALL DGCURG(H,V)
      CALL DQPWIN(NUMWI,H,V,FOUT)
      IF(FOUT)THEN
        CALL DWRT('Cursor outside active window! Pop up.#')
        GO TO 936
      END IF
      IF(ABS(HOLD-H).LT.D.AND.ABS(VOLD-V).LT.D) THEN
         H=HOLD
         V=VOLD
      END IF
      IF(NEXEC.EQ.4.OR.TANSW.EQ.'  ') GO TO 940
      CALL DO_STR('ST')
      IF(TANSW.EQ.'ST') THEN
        CALL DPIFT(NUMT,H,V,NHIT,MODUL,FI,TE)
        NEXEC=3
        GO TO 910
      END IF
      IF(TANSW.EQ.'GB') THEN
         NEXEC=3
         GO TO 910
      END IF
      IF(TANSW.EQ.'PI') GO TO 941
      CALL DO_STR('DR')
      IF(TANSW.EQ.'DR') THEN
        CALL DQLEVL(ICTXDD)
        CALL DGTEXT(H+DHD,V+DVD,DT5(FLOAT(NHIT)),5)
        GO TO 936
      END IF
      CALL DO_STR('TY')
      IF(TANSW.EQ.'TY') THEN
        CALL DPITYP(NHIT,MODUL,'NORMAL')
        GO TO 936
      END IF
C      CALL DO_STR('TV')
C      IF(TANSW.EQ.'TV') THEN
C        CALL DPITYP(NHIT,MODUL,'VIDEAU')
C        GO TO 936
C      END IF
      CALL DO_STR('MO"MM')
      IF(TANSW.EQ.'MO'.OR.TANSW.EQ.'MM') THEN
         CALL DPIMOV(NUMT,NHIT,MODUL,0,H,V,FIN)
         GO TO 936
      END IF
      CALL DO_STR('MT: move to track')
      IF(TANSW.EQ.'MT') THEN
        IPTNDP=1
        CALL DPIMOV(NUMT,NHIT,MODUL,0,H,V,FIN)
        IPTNDP=0
        GO TO 936
      END IF
      CALL DO_STR('M0: move to neutral EF track')
      IF(TANSW.EQ.'M0') THEN
        IPTNDP=2
        CALL DPIMOV(NUMT,NHIT,MODUL,0,H,V,FIN)
        IPTNDP=0
        GO TO 936
      END IF
      CALL DO_STR('ZP')
      IF(TANSW.EQ.'ZP') THEN
C Sleep and wait for a carriage return.
         CALL DWRT('Pause in PIck processor. Hit Return to continue:')
CX       READ(*,'(A)',END=936,ERR=936)
         CALL DGETLN(TT1,LTT1,LEN(TT1))
CX
CVV         CALL DGGCUR(H,V)
         CALL DGCURG(H,V)
         GO TO 938
      END IF
      NOARDO=0
      IARE=IAREDO
C      CALL DAREA('M',TANSW,1,12,JARE,NYES)
C      IF(NYES) THEN
C         CALL DPIFT(NUMT,H,V,NHIT,MODUL,FI,TE)
C         HA=(HHGHDG(JARE)+HLOWDG(JARE))*0.5
C         VA=(VHGHDG(JARE)+VLOWDG(JARE))*0.5
C         IAREDO=IARE
C         CALL DPIMOV(NUMT,NHIT,MODUL,0,HA,VA,FIN)
C         IF(FIN) THEN
C            H=HA
C            V=VA
C         END IF
C         GO TO 936
C      END IF
      CALL DO_STR('Z0')
      IF(TANSW.EQ.'Z0') THEN
         VRDZDV=0.
         CALL DWRT('DZ of vertex = '//DT5(VRDZDV))
         CALL=DVTPCL(IVTEDV)
         GO TO 936
      END IF
      CALL DO_STR('ZB')
      IF(TANSW.EQ.'ZB') THEN
         VRDZDV=VRTZDV
         CALL DWRT('DZ of vertex = '//DT5(VRDZDV))
         CALL=DVTPCL(IVTEDV)
         GO TO 936
      END IF
      CALL DO_STR('CZ')
      IF(TANSW.EQ.'CZ') THEN
         R1=EVARDP(IVRODV,1)
         R2=EVARDP(IVRODV,2)
         DRHO=R2-R1
         IF(ABS(DRHO).GT.10.) THEN
            DZZ=(R1*EVARDP(IVZZDV,2)-R2*EVARDP(IVZZDV,1))/DRHO
            IF(ABS(DZZ).LT.30.) THEN
               VRDZDV=-DZZ
               CALL DWRT('DZ of vertex = '//DT5(VRDZDV))
               CALL=DVTPCL(IVTEDV)
               GO TO 936
            END IF
         END IF
      END IF
      CALL DO_STR('SZ')
      IF(TANSW.EQ.'SZ') THEN
         CALL DWRT('DZ of vertex = '//DT5(VRDZDV))
         GO TO 936
      END IF
      CALL DO_STR('PT')
      IF(TANSW.EQ.'PT') THEN
        IPTNDP=1
        GO TO 941
      END IF
      CALL DO_STR('P0')
      IF(TANSW.EQ.'P0') THEN
        IPTNDP=2
        GO TO 941
      END IF
      CALL DO_STR('PH"PP')
      IF(TANSW.EQ.'PH'.OR.TANSW.EQ.'PP') THEN
        LORPDP=2
        BNUMDB(1,FRFTDB)=BNUMDB(4,FRFTDB)
        BNUMDB(4,FRFTDB)=-1.
        GO TO 941
      END IF
      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('DPIPOS')
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  930 H=HPI
      V=VPI
      NUMT=0
      CALL DV0TYP(NHIT,MODUL,TRK)
      GO TO 936
  940 IF(TANSW.EQ.'++'.OR.TANSW.EQ.';;'.
     1  OR.TANSW.EQ.'--'.OR.TANSW.EQ.'__') THEN
         CALL DPIMOV(NUMT,NHIT,MODUL,0,H,V,FIN)
         GO TO 936
      END IF
  941 CALL DPIFT(NUMT,H,V,NHIT,MODUL,FI,TE)
      IPTNDP=0
      CALL DV0TYP(NHIT,MODUL,TRK)
      LTRK=TRK
      IF(LTRK.GT.0) THEN
        DO K=7,1,-1
          LTRKDP(K+1)=LTRKDP(K)
        END DO
        LTRKDP(1)=NTRKDP
        NTRKDP=LTRK
        NUMPDP=MIN(9,NUMPDP+1)
      END IF
      IF(MDLPDP.EQ.TPCODB.OR.MDLPDP.EQ.TPADDB) THEN
        CALL UCOPY(EVARDP(1,2),EVARDP(1,3),20)
        CALL UCOPY(EVARDP(1,1),EVARDP(1,2),20)
        IF(MDLPDP.EQ.TPCODB) THEN
          DO K=1,20
            EVARDP(K,1)=DVTP(K,NPIKDP)
          END DO
        ELSE
          DO K=1,20
            EVARDP(K,1)=DVPADA(K,NPIKDP)
          END DO
        END IF
        DO K=MXYZDP,2,-1
          XYZPDP(1,K)=XYZPDP(1,K-1)
          XYZPDP(2,K)=XYZPDP(2,K-1)
          XYZPDP(3,K)=XYZPDP(3,K-1)
        END DO
        XYZPDP(1,1)=EVARDP(1,1)
        XYZPDP(2,1)=EVARDP(2,1)
        XYZPDP(3,1)=EVARDP(3,1)
        NXYZDP=NXYZDP+1
      END IF
      IF(LORPDP.EQ.2) THEN
        BNUMDB(4,FRFTDB)=BNUMDB(1,FRFTDB)
        BNUMDB(1,FRFTDB)=1.
      END IF
      LORPDP=0
      IF(MODUL.EQ.VDXYDB) NPXYDV=NPIKDP
      IF(MODUL.EQ.VDZTDB) NPZTDV=NPIKDP
      GO TO 936
      END
*DK DPICK_TR_EF_1
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DPICK_TR_EF_1
CH
      SUBROUTINE DPICK_TR_EF_1
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Switch off all banks except FRFT and ALPHA tracks.
C     FRFT from 0 to M2, neutral energy flow tracks from 0 to end
C
C     IPTNDP = 0 no pick of invisible tracks and neutral energy flow tracks
C              1 pick of invisible tracks
C              2 pick neutral energy flow tracks, in TF both sides of chimney
C              3 pick neutral energy flow tracks, in TF bottem of chimney
C
      INCLUDE 'DALI_CF.INC'
      DIMENSION B(MBNCDB)
      LOGICAL FYES
      IF(IPTNDP.NE.0) THEN
        DO K=1,MBNCDB
          B(K)=BNUMDB(4,K)
          BNUMDB(4,K)=-1.
        END DO
        IF(IPTNDP.EQ.1) THEN
          BNUMDB(4,FRFTDB)=1.
          TPARDA=
     &      'J_HIN,J_HCA,J_HNT'
          CALL DPARAM(20
     &      ,J_HIN,J_HCA,J_HNT)
          CALL DHTMOD('TR',FYES)
          CALL DHTMOD('JU',FYES)
C         ...................... from VX to T3 or further up
          IHTRDO(3)=2
          IHTRDO(4)=MAX(IHTRDO(4),5)
          CALL DHTMO2
          PARADA(4,J_HNT)=1.
          PARADA(2,J_HNT)=1.
          IF(PARADA(2,J_HCA).EQ.0.) THEN
            CALL DPARGG(20,N1,N2)
            DO N=N1,N2
              PARADA(4,N)=-1.
            END DO
            K2=BNUMDB(2,FRFTDB)
            DO K=1,K2
              IF(COLRDT(K).EQ.-1.) COLRDT(K)=0.1
            END DO
          END IF
        ELSE
          TPARDA=
     &      'J_HEO'
          CALL DPARAM(20
     &      ,J_HEO)
          IF(PARADO(4,J_HEO).NE.2.) IPTNDP=3
          PARADA(4,J_HEO)=2.
          PARADA(2,J_HEO)=3.
        END IF
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------  DPICK_TR_EF_2
CH
      ENTRY DPICK_TR_EF_2
CH
CH --------------------------------------------------------------------
CH
C     Reset all to last values. PARADA is replaced before each pick or move
C     by PSTODS, i.e old values and it is not necessary to reset it. Current
C     parameters are reset when exiting for DPIPOS.
C
      IF(IPTNDP.NE.0) THEN
        DO K=1,MBNCDB
          BNUMDB(4,K)=B(K)
        END DO
        IF(IPTNDP.EQ.1) THEN
          IF(PARADA(2,J_HCA).EQ.0.) THEN
            DO K=1,K2
              IF(COLRDT(K).EQ.0.1) COLRDT(K)=-1.
            END DO
          END IF
        END IF
      END IF
      END
*DK DPITRK
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPITRK
CH
      SUBROUTINE DPITRK(TA,TRNEW,NUM,TRAK,FOUND,ETRAK)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    : ID=PROCESSOR IDENTIFICATION, TCOM = ARRAY OF ALLOWED COMMANDS
C                TA=ANSWER, NUM = LENGTH OF TRACK ARRAY
C    Outputs   :  TRAK(*)=TRACK ARRAY, FOUND = TRUE IF TA=TCOM(1:13)
C
C ---------------------------------------------------------------------
C     ........................................................ TA=
C     ....................................................... 1=EL: ERASE LIST
C     ....................................................... 2=PT: PICK TRACK
C     ...................................................... 3=ST: STORE TRACK
C     ...................................................... 4=ET: ERASE TRACK
C     ............................... 5=DN: reDisplay tracks in Normal colours
C     .............................. 6=DG: Display non selected tracks to Grey
C     ........................... 7=DE: Display non selected tracks Eliminated
C     .............. 8=DH: Display non selected tracks as Hits
C     .............................................. 9=TL: LIST SELECTED TRACK
C     ............................ 10=MT,11=MN: MOVE TO SELECTED OR NEXT TRACK
C     ............................................................ 12=DR: DRAW
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TCOM(12),TCOMD(12),TC(*)
      DATA TCOMD/'EL','PT','ST','ET','DN','DG','DE','DH','TL','MT',
     $           'MN','DR'/
C     EQUIVALENCE (TRNEW,PARADO(2,IPSTDO))
C     EQUIVALENCE (TRNEW,PARADO(2,32))
      DATA G7/7./
      CHARACTER *2 DT3
      CHARACTER *3 DT2
      CHARACTER *1 T1
      CHARACTER *50 T
C     .. IF CALLED BY DIFFERENT PROCESSORS GIVE EACH PROCESSOR A DIFFERENT ID
C     .. AND SET FCLER(3) HIGHER IF NECESSARY
      PARAMETER (NID=3)
      LOGICAL FIN,FCLER(NID),FOUND
      DIMENSION TRAK(*)
      CHARACTER *2 TA
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HIN,J_HCA'
      CALL DPARAM(20
     &  ,J_HIN,J_HCA)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(FCLER(IDC)) THEN
        CALL VZERO(TRAK,NUM)
        FCLER(IDC)=.FALSE.
      END IF
      FOUND=.TRUE.
      ETRAK=0.
C     ....................................................... 1=EL: ERASE LIST
      IF(TA.EQ.TCOM(1)) THEN
        CALL VZERO(TRAK,NUM)
        GO TO 95
      END IF
      IF(TA.EQ.TCOM(2).OR.TA.EQ.TCOM(3)) THEN
C       ..................................................... 2=PT: PICK TRACK
        IF(TA.EQ.TCOM(2)) THEN
          MODUL=0
          CALL DCOPTL
          FPIKDP=.TRUE.
          CALL DGGCUR(H,V)
          CALL DPIFT(NUMT,H,V,NHIT,MODUL,FI,TE)
          CALL DCOPFL
          FPIKDP=.FALSE.
          IF(MODUL.EQ.0) GO TO 2
          CALL DV0TYP(NHIT,MODUL,TRNEW)
          IF(TRNEW.EQ.-99999.) THEN
    2       CALL DWRT(' No track found.')
            RETURN
          END IF
        END IF
C       .................................................... 3=ST: STORE TRACK
        IF(TRNEW.EQ.0.) GO TO 90
        DO N=1,NUM
          IF(TRAK(N).EQ.TRNEW) THEN
    3       CALL DWRT('Track '//DT3(TRNEW)//
     &        ' exists already. Type Ignore=<CR> or Delet')
            CALL DGETLN(T1,L1,LEN(T1))
            IF(L1.LE.0.OR.T1.EQ.'I'.OR.T1.EQ.'i') GO TO 90
            IF(T1.EQ.'D'.OR.T1.EQ.'d') GO TO 21
            GO TO 3
          END IF
        END DO
    5   DO N=NUM,2,-1
          TRAK(N)=TRAK(N-1)
        END DO
        TRAK(1)=TRNEW
        GO TO 90
      END IF
C     ...................................................... 4=ET: ERASE TRACK
      IF(TA.EQ.TCOM(4)) THEN
        IF(TRAK(1).EQ.0.) GO TO 95
        GO TO 20
      END IF
C     ............................... 5=DN: REDISPLAY TRACKS IN NORMAL COLOURS
      IF(TA.EQ.TCOM(5)) THEN
        COL=0.
        PARADA(2,J_HCA)=PCUT
        PARADA(2,J_HIN)=PINH
        GO TO 30
      END IF
C     ....................... 6=DG: SET COLOURS OF NON SELECTED TRACKS TO GREY
      IF(TA.EQ.TCOM(6)) THEN
        IF(TRAK(1).EQ.0.) GO TO 95
        COL=G7
        GO TO 30
      END IF
C     ............................... 7=D0: DO NOT DISPLAY NON SELECTED TRACKS
      IF(TA.EQ.TCOM(7)) THEN
        IF(TRAK(1).EQ.0.) GO TO 95
        COL=-1.
        GO TO 30
      END IF
C     .............. 8=DE: DO NOT DISPLAY NON SELECTED TRACKS BUT DISPLAY HITS
      IF(TA.EQ.TCOM(8)) THEN
        IF(TRAK(1).EQ.0.) GO TO 95
        COL=-1.
        PARADA(2,J_HCA)=0.
        CALL DHTMOD('HT',FDUM)
        GO TO 30
      END IF
C     .............................................. 9=LT: LIST SELECTED TRACK
      IF(TA.EQ.TCOM(9)) THEN
        IF(TRAK(1).EQ.0.) GO TO 95
        DO N=1,NUM
          IF(TRAK(N).EQ.TRNEW) GO TO 91
        END DO
        N=1
        TRNEW=TRAK(1)
        GO TO 91
      END IF
C     ............................ 10=MT,11=MN: MOVE TO SELECTED OR NEXT TRACK
      IF(TA.EQ.TCOM(10).OR.TA.EQ.TCOM(11)) THEN
        IF(TRAK(1).EQ.0.) GO TO 95
        IF(TRNEW.EQ.0.) GO TO 10
        DO N=1,NUM
          IF(TRAK(N).EQ.TRNEW) GO TO 11
        END DO
   10   N=1
        GO TO 12
C       ................................................... MOVE TO NEXT TRACK
   11   IF(TA.EQ.TCOM(11)) N=N+1
   12   IF(TRAK(N).EQ.0..OR.N.GT.NUM) N=1
        TRNEW=TRAK(N)
        CALL DCOPTL
        FPIKDP=.TRUE.
        CALL DGGCUR(H,V)
        CALL DPIMOV(NUMT,IFIX(TRAK(N)),FRFTDB,0,H,V,FIN)
        CALL DCOPFL
        FPIKDP=.FALSE.
        IF(FIN) THEN
          CALL DGSCUR(H,V)
          GO TO 91
        END IF
        RETURN
      END IF
   20 DO N=1,NUM
        IF(TRAK(N).EQ.TRNEW) GO TO 21
      END DO
      CALL DWRT(' Track '//DT3(TRNEW)//' does not exist.')
      RETURN
   21 ETRAK=TRAK(N)
      DO I=N+1,NUM
        TRAK(I-1)=TRAK(I)
      END DO
      TRAK(NUM)=0.
   90 N=1
   91 IF(N.EQ.0) THEN
        L1=0
      ELSE
        L1=1+3*N
      END IF
      T='Tr:'
      L=4
      DO N=1,NUM
        T(L:L+1)=DT2(TRAK(N))
        L=L+3
      END DO
      CALL DWRT(T)
      RETURN
   95 CALL DWRT('TR:0 ...  No track selected.')
      RETURN
   30 NUMTR=BNUMDB(2,FRFTDB)
      CALL UFILL(COLRDT,1,NUMTR,COL)
      DO N=1,NUM
        J=TRAK(N)
        IF(J.GT.0) COLRDT(J)=0.
      END DO
  940 CALL DSC0
      NAR=IAREDO
      DO M=0,MPNWDW
        IF(NWINDW(M,NAR).EQ.-2.OR.NWINDW(M,NAR).EQ.1) THEN
          PSTODS(1,J_HCA,M,IWUSDO)=PARADA(2,J_HCA)
          PSTODS(2,J_HCA,M,IWUSDO)=PARADA(4,J_HCA)
          PSTODS(1,J_HIN,M,IWUSDO)=PARADA(2,J_HIN)
          PSTODS(2,J_HIN,M,IWUSDO)=PARADA(4,J_HIN)
        END IF
      END DO
      CALL DCOPTL
      CALL DPCEAR(NAR)
      CALL DCOPFL
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DPITR1
CH
      ENTRY DPITR0(ID,TC)
CH
CH --------------------------------------------------------------------
CH
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!: STORE INITIAL VALUES
C       INPUT: TC(12)=LIST OF COMMANDS, IF TC(1)='df' USE INTERNAL LIST
C            : ID = IDENTIFICATION OF PROCESSOR 1, .. 3
C!:          : ID=0 : SET CLEAR FLAGS TRUE
C ---------------------------------------------------------------------
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_RST'
      CALL DPARAM(19
     &  ,J_RST)
      TPARDA=
     &  'J_HIN,J_HCA'
      CALL DPARAM(20
     &  ,J_HIN,J_HCA)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(ID.EQ.0) THEN
        DO I=1,NID
          FCLER(I)=.TRUE.
        END DO
        PARADA(2,J_RST)=0.
        RETURN
      END IF
      IDC=ID
      IF(TC(1).EQ.'df') THEN
        DO K=1,12
          TCOM(K)=TCOMD(K)
        END DO
      ELSE
        DO K=1,12
          TCOM(K)=TC(K)
        END DO
      END IF
      PCUT=PARADA(2,J_HCA)
      PINH=PARADA(2,J_HIN)
      IF(ID.GT.NID) THEN
        CALL DWRT('Call H.Drevermann : NID too small!')
        RETURN
      END IF
      END
*DK DPITYP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPITYP
CH
      SUBROUTINE DPITYP(NHIT,MODUL,TMOD)
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
      INCLUDE 'DALI_EX.INC'
      DATA PIDEG/57.29577951/
      PARAMETER (MESDA=13)
      DIMENSION NESDA(MESDA)
      DIMENSION TP(6)
      CHARACTER *2 TESDA(MESDA)
      CHARACTER *3 DT3
      CHARACTER *4 DT4
      CHARACTER *5 DT5,TV(MESDA,2)
      CHARACTER *6 DT6
      DATA NESDA/
     &    1 ,  2 ,  3 ,  4 ,  5 ,  6 , 10 , 11 ,
     &   12 , 13 , 14 , 15 , 19 /
      DATA TESDA/
     &  ' X',' Y',' Z','RO','FI','TE','DF','DT',
     &  ' E',' I',' J',' K','CL'/
      PARAMETER (MPNT=9)
      DIMENSION NPNT(MPNT)
      CHARACTER *2 TPNT(MPNT)
      DATA NPNT/
     &    1 ,  2 ,  3 ,  4 ,  5 ,  6 ,  7 ,  8 , 18 /
      DATA TPNT/
     &  ' X',' Y',' Z','RO','FI','TE',' R',' B','NT'/
      CHARACTER * 49 TVDCO
C                 123456789 123456789 123456789 123456789 123456789
      DATA TVDCO/'from   VDXY # 12345 row 12   VDZT # 12345 row 12'/
      PARAMETER (MMHT=10)
      DIMENSION NMHT(MMHT)
      CHARACTER *2 TMHT(MMHT)
      DATA NMHT/
     &    1 ,  2 ,  3 ,  4 ,  5 ,  6 ,  7 ,  9 , 16 , 13 /
      DATA TMHT/
     &  ' X',' Y',' Z','RO','FI','TE',' R','YL','SC','SL'/
      PARAMETER (MVDX=6)
      DIMENSION NVDX(MVDX)
      CHARACTER *2 TVDX(MVDX)
      DATA NVDX/
     &    1 ,  2  , 4 ,  5 , 10 , 12 /
      DATA TVDX/
     &  ' X',' Y','RO','FI','UC','PH'/
      PARAMETER (MVDR=7)
      DIMENSION NVDR(MVDR)
      CHARACTER *2 TVDR(MVDR)
      DATA NVDR/
     &    3 ,  4 ,  9  , 5 ,  6 , 10 , 12 /
      DATA TVDR/
     &  ' Z','R1','R2','FI','TE','WC','PH'/
      CHARACTER *6 TMOD
      LOGICAL FOUT
      IF(MODUL.EQ.ESDADB) THEN
        CALL DV0(ESDADB,NUM1,NUM2,FOUT)
        IF(FOUT) RETURN
        IF(NHIT.LT.NUM1.OR.NHIT.GT.NUM2) RETURN
        DO NV=1,MESDA
          TV(NV,1)=DT5(DVEC(NESDA(NV),NHIT))
        END DO
        IF(TMOD.EQ.'NORMAL') THEN
          WRITE(TXTADW,1001) (TESDA(K),'=',TV(K,1),K=1,4)
          CALL DWRC
          WRITE(TXTADW,1001) (TESDA(K),'=',TV(K,1),K=5,8)
          CALL DWRC
          WRITE(TXTADW,1001) (TESDA(K),'=',TV(K,1),K=9,13)
          CALL DWRC
 1001     FORMAT(5(1X,3A))
        ELSE
          CALL=DVVEC0(NUM)
          DO NV=1,4
            TV(NV,2)=DT5(DVVEC(NESDA(NV),NHIT))
          END DO
          DO NV=5,6
            TV(NV,2)=DT5(DVVEC(NESDA(NV),NHIT)*PIDEG)
          END DO
          DO NV=7,8
            TV(NV,2)=DT5(DVVEC(NESDA(NV),NHIT)*PIDEG*0.5)
          END DO
          WRITE(TXTADW,1002) (TESDA(K),'=',TV(K,1),
     &                                 '=',TV(K,2),K=1,8)
 1002     FORMAT(3(1X,5A))
          CALL DWRC
        END IF
      ELSE IF (MODUL.EQ.TPCODB) THEN
        CALL DPITBK(TPCODB,DVTP  ,MPNT,NPNT,TPNT,NHIT,'TPCO')
      ELSE IF (MODUL.EQ.TBCODB) THEN
        CALL DPITBK(TBCODB,DVTP  ,MPNT,NPNT,TPNT,NHIT,'TBCO')
      ELSE IF (MODUL.EQ.MHITDB) THEN
        CALL DPITBK(MHITDB,DVMD  ,MMHT,NMHT,TMHT,NHIT,'MHIT')
      ELSE IF (MODUL.EQ.VDZTDB) THEN
        CALL DPITBK(VDZTDB,DVVDRZ,MVDR,NVDR,TVDR,NHIT,'VDZT')
      ELSE IF (MODUL.EQ.VDCODB) THEN
        CALL=DVVDC1(NHIT,IWFXY,IHTXY,IWFZT,IHTZT)
        IF(IHTXY.GT.0) THEN
          CALL DTINT(IWFXY,15,19,TVDCO)
          CALL DTINT(IHTXY,25,26,TVDCO)
        ELSE
          TVDCO(15:19)=' '
          TVDCO(25:26)=' '
        END IF
        IF(IHTZT.GT.0) THEN
          CALL DTINT(IWFZT,37,41,TVDCO)
          CALL DTINT(IHTZT,47,48,TVDCO)
        ELSE
          TVDCO(37:41)=' '
          TVDCO(47:48)=' '
        END IF
        CALL DWRT(TVDCO)
      ELSE IF (MODUL.EQ.VDXYDB) THEN
        CALL DPITBK(VDXYDB,DVVDXY,MVDX,NVDX,TVDX,NHIT,'VDXY')
      ELSE IF (MODUL.EQ.FRFTDB) THEN
        CALL DVTRSX(NHIT,TP(1),TP(2),TP(3),TP(4),TP(5),TP(6))
        WRITE(TXTADW,1008) 
     &    'IR=',DT6(TP(1)),
     &    'TL=',DT4(TP(2)),
     &    'P0=',DT3(TP(3)),
     &    'D0=',DT4(TP(4)),
     &    'Z0=',DT4(TP(5)),
     &    'AL=',DT5(TP(6))
 1008   FORMAT(2A,5(1X,2A))
        CALL DWRC
      ELSE
        CALL DWRT('No further output for this bank.')
      END IF
      END
*DK DPITBK
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPITBK
CH
      SUBROUTINE DPITBK(MB,DFU,M,N,T,K,TBNK)
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
      CHARACTER *2 T(*)
      CHARACTER *(*) TBNK
      DIMENSION N(*)
      CHARACTER *5 DT5,TP(20)
      LOGICAL FOUT
      EXTERNAL DFU
      CALL DV0(MB,NUM1,NUM2,FOUT)
      IF(FOUT.OR.K.LT.NUM1.OR.K.GT.NUM2) THEN
        CALL DWRT('Hit not found or bank not available')
      ELSE
        IF(MB.NE.VDZTDB) THEN
          DO L=1,M
            TP(L)=DT5(DFU(N(L),K))
          END DO
        ELSE
          DO L=1,M
            TP(L)=DT5(DFU(N(L),K,1))
          END DO
        END IF
        DO I=1,M,5
          I4=MIN(M,I+4)
          WRITE(TXTADW,1000) (T(L),TP(L),L=I,I4)
 1000     FORMAT(5(1X,A,'=',A))
          CALL DWRC
        END DO
        TXTADW=TBNK//': # '//DT5(FLOAT(K))
        CALL DWRC
      END IF
      END
*DK DPLANE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPLANE
CH
      SUBROUTINE DPLANE(NP,TA,NYES,NEXEC)
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
      LOGICAL NYES
      CHARACTER *2 TA
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PLA,J_PP1,J_PP2,J_PTO'
      CALL DPARAM(11
     &  ,J_PLA,J_PP1,J_PP2,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(TA.EQ.'EO') THEN
        IF(NP.EQ.J_PP1) THEN
          PARADA(2,J_PP1)=1011.
          NYES=.TRUE.
          RETURN
        ELSE IF(NP.EQ.J_PLA) THEN
          PARADA(2,J_PP1)=1011.
          PARADA(2,J_PP2)=1011.
          NYES=.TRUE.
          RETURN
        END IF
      END IF
      IF(NP.GE.J_PLA.AND.NP.LE.J_PTO) THEN
         IF(TA.EQ.'TP') THEN
            NYES=.TRUE.
            PARADA(2,NP)=1004.
            IF(NP.EQ.J_PLA) THEN
               PARADA(2,J_PP1)=1001.
               PARADA(2,J_PP2)=1004.
            END IF
            RETURN
         END IF
         DO M=-2,11
            IF(TA.EQ.TPLNDO(M)) THEN
               NYES=.TRUE.
               PARADA(2,NP)=1000.+M
               IF(NP.EQ.J_PLA) THEN
                  PARADA(2,J_PP1)=PARADA(2,J_PLA)
                  PARADA(2,J_PP2)=PARADA(2,J_PLA)
               END IF
               RETURN
            END IF
         END DO
      END IF
      NYES=.FALSE.
      RETURN
      END
*DK DPOAR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPOAR
CH
      SUBROUTINE DPOAR(H,V,NAR)
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
C   Return window number in NAR for present coordinates (H,V).
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DATA IW12/12/
      NAR=-1
C     IF(IWARDO.LE.0) THEN
      IF(     JWUSDW.EQ.0) THEN
        DO NAR=0,MPNWDW
          IF(H.GE.HMINDG(NAR).AND.H.LE.HHGHDG(NAR).AND.
     &        V.GE.VMINDG(NAR).AND.V.LE.VHGHDG(NAR).AND.
     &        ISTODS(4,NAR,IWUSDO).GT.0) RETURN
        END DO
      ELSE IF(JWUSDW.EQ.1) THEN
        NAR=IW12
      END IF
      END
*DK DPOFT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPOFT
CH
      SUBROUTINE DPOFT(NUMT,H,V,FI,TE)
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
      CHARACTER *2 TPIC
      LOGICAL FMES
      FMES=.FALSE.
      CALL DPOAR(H,V,NAR)
      IF(NAR.LT.0) GO TO 9
      IARE=IAREDO
      IAREDO=NAR
      TPIC=TPICDP(ISTODS(5,NAR,IWUSDO))
      IF(TPIC.EQ.'YX'.OR.TPIC.EQ.'XY') THEN
         CALL DYXPO(NAR,H,V,FI,FMES)
         IF(.NOT.FMES) GO TO 9
         FPOSDT=FI
         GO TO 99
      ELSE IF(TPIC.EQ.'FT'.OR.TPIC.EQ.'TF') THEN
         CALL DFTPO(NAR,H,V,FI,TE)
         FPOSDT=FI
         TPOSDT=TE
         GO TO 99
      ELSE IF(TPIC.EQ.'RZ'.OR.TPIC.EQ.'ZR'.OR.TPIC.EQ.'HZ') THEN
         CALL DRZPO(NAR,H,V,TE)
         TPOSDT=TE
         GO TO 99
      ELSE IF(TPIC.EQ.'FZ'.OR.TPIC.EQ.'ZF'.OR.
     1        TPIC.EQ.'FR'.OR.TPIC.EQ.'RF') THEN
         CALL DFZPO(NAR,V,FI,FMES)
         FPOSDT=FI
         GO TO 99
      END IF
    9 CALL DWRT('Neither Phi nor Theta are defined.')
      NUMT=0
   99 IAREDO=IARE
      RETURN
      END
*DK DPOLCO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPOLCO
CH
      SUBROUTINE DPOLCO(X,Y,Z,RO,T,F)
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
      RO=SQRT(X*X+Y*Y)
      T=DATN2D(RO,Z)
      F=DATN2D(Y ,X)
      END
*DK DPOLCR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPOLCR
CH
      SUBROUTINE DPOLCR(X,Y,Z,R,T,F)
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
      R2=X*X+Y*Y
      R=SQRT(R2+Z*Z)
      IF(R.EQ.0.) THEN
        F=0.
        T=0.
      ELSE
        RO=SQRT(R2)
        T=DATN2D(RO,Z)
        IF(RO.EQ.0.) THEN
          F=0.
        ELSE
          F=DATN2D(Y,X)
        END IF
      END IF
      END
*DK DPOLCRV
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPOLCRV
CH
      SUBROUTINE DPOLCRV(XYZ,RFT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C ---------------------------------------------------------------------
      DIMENSION XYZ(3),RFT(3)
      R2=XYZ(1)*XYZ(1)+XYZ(2)*XYZ(2)
      RFT(1)=SQRT(R2+XYZ(3)*XYZ(3))
      IF(RFT(1).EQ.0.) THEN
        RFT(2)=0.
        RFT(3)=0.
      ELSE
        RO=SQRT(R2)
        RFT(3)=DATN2D(RO,XYZ(3))
        IF(RO.EQ.0.) THEN
          RFT(2)=0.
        ELSE
          RFT(2)=DATN2D(XYZ(2),XYZ(1))
        END IF
      END IF
      END
*DK DPOMOV
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPOMOV
CH
      SUBROUTINE DPOMOV(NUMT,FP,TP,NDIR,H,V,FIN)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modified by B.S. Nilsson                  June 1989
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C   Return coordinates (H,V) for the point FP,TP in previous, present or
C   next window (NDIR=-1,0,+1)
C
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      LOGICAL FIN
      IARE=IAREDO
      IF(NDIR.EQ.0) THEN
         FPOSDT=FP
         TPOSDT=TP
         CALL DPOAR(H,V,NAR)
         IF(NAR.LT.0) GO TO 3
         IAR=NAR
         GO TO 2
      ELSE
         CALL DPOFT(NUMT,H,V,FPOSDT,TPOSDT)
         CALL DPOAR(H,V,NAR)
         IF(NAR.LT.0) GO TO 3
         IAR=NAR
         IF(NDIR.EQ.1) THEN
            DO K=IAR+1,MPARDS
               IF(ISTODS(4,K,IWUSDO).GT.0) GO TO 1
            END DO
            DO K=0,IAR
               IF(ISTODS(4,K,IWUSDO).GT.0) GO TO 1
            END DO
         ELSE
            DO K=IAR-1,0,-1
               IF(ISTODS(4,K,IWUSDO).GT.0) GO TO 1
            END DO
            DO K=MPARDS,IAR,-1
               IF(ISTODS(4,K,IWUSDO).GT.0) GO TO 1
            END DO
         END IF
         GO TO 3
      END IF
    1 IAR=K
    2 IPICDO=ISTODS(5,IAR,IWUSDO)
      IAREDO=IAR
      CALL DBR3(H,V,FIN)
      IAREDO=IARE
      RETURN
    3 FIN=.FALSE.
      END
*DK DPONXT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPONXT
CH
      SUBROUTINE DPONXT(H,V)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Rewritten by B.S. Nilsson                 June 1989
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C   Find the next active subwindow in the standard window list. Then
C   return coordinates of the centre of this window.
C   One could set up a list here defining another priority order bet-
C   ween the windows.
C
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
C   Determine subwindow for input coordinates.
      DO K=0,MPARDS
         IF(ISTODS(4,K,IWUSDO).GT.0) THEN
            IF(H.GE.HMINDG(K).AND.H.LE.HHGHDG(K).AND.
     &         V.GE.VMINDG(K).AND.V.LE.VHGHDG(K)) GO TO 2
         END IF
      END DO
      RETURN
    2 IAR=K
      DO K=IAR+1,MPARDS
         IF(ISTODS(4,K,IWUSDO).GT.0) GO TO 4
      END DO
      DO K=0,IAR
         IF(ISTODS(4,K,IWUSDO).GT.0) GO TO 4
      END DO
      RETURN
    4 IAR=K
      H=0.5*(HLOWDG(IAR)+HHGHDG(IAR))
      V=0.5*(VLOWDG(IAR)+VHGHDG(IAR))
      RETURN
      END
*DK DPOS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPOS
CH
      SUBROUTINE DPOS(TANSW,FI,TE,NEXEC,CHG)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modified by B.S. Nilsson                  16-Feb-1990
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TPIC,TANSW
      LOGICAL CHG
      CALL DO_STR('PI')
      IF(TANSW.NE.'PI') RETURN
      TPIC=TPICDO
      IARE=IAREDO
      CALL DPIPOS(FI,TE,NEXEC,CHG,TANSW)
      IF(TPICDO.EQ.'GB') NEXEC=3
      IF(NEXEC.EQ.3) TPICDO=TPIC
      IF(NEXEC.NE.1) IAREDO=IARE
      END
*DK DPT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DPT
CH
      SUBROUTINE DPT
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
      LOGICAL FCHG,FOUND
      CHARACTER *2 TANSW,TPR
      DATA TPR/'TR'/
      DIMENSION TRAK(15)
      DATA NTRK/15/
      CALL DPITR0(3,'df')
      CALL DCOPTL
C        123456789 123456789 123456789 123456789 123456789
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_RST'
      CALL DPARAM(19
     &  ,J_RST)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
  930 CALL DPITRK('TL',PARADA(2,J_RST),NTRK,TRAK,FOUND,ETRAK)
      CALL DWR_HL_AR('PT:WS:    Pick tracks')
  936 FCHG=.FALSE.
      CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(1,0,
     &  1,1,TPR,PARADA(1,J_RST),
     &  1,0,' ',0,
     1  NEXEC,CHG,TANSW)
      CALL DGZOOM(6,-1,0,0)
      GO TO (910,920,930,940),NEXEC
  910 IF(TANSW.NE.'GW') CALL DCOPFL
      CALL DSC0
      RETURN
  940 TANSW='PT'
  920 CALL DPITRK(TANSW,PARADA(2,J_RST),NTRK,TRAK,FOUND,ETRAK)
      IF(FOUND) GO TO 936
      CALL DAREA('D',TANSW,0,12,IAREDO,FOUND)
      IF(FOUND) THEN
        CALL DSC0
        NAR=IAREDO
        CALL DPCEAR(NAR)
        IAREDO=NAR
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
      END
