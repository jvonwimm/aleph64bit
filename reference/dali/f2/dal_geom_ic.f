*DK DGDITC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DGDITC( TPROJ, TUNIT , RR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DI
      COMMON /DITGEO/ RMAXDI,RMINDI,ZMAXDI
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *3 TPROJ
      CHARACTER *6 TUNIT
      DIMENSION RR(3)
      DATA QSHA/0.186/
C
C  ITC has 12 corners on the inside and on the outside
C
      DIMENSION H(10),V(10)
      PARAMETER ( NPTS = 36 )
C
C  statement-functions
C
C      HPER(X,Y,R) = X*(1.+E*R2)/(1.+E*R)
C      VPER(X,Y,R) = Y*(1.+E*R2)/(1.+E*R)
C
C      R1 = RR(1)
C      R2 = RR(2)
C      R1P= RR(3)

      IF(TPROJ(2:3).EQ.'YX') THEN
        IF(TUNIT.EQ.'BARREL' .OR. TUNIT.EQ.'BAR+MA' .OR.
     &     TUNIT.EQ.'MIDANG' ) THEN

          FI = 0.
          FIINC = 360./NPTS
          DO I=1,NPTS
            C1 = COSD( FI )
            S1 = SIND( FI )
            C2 = COSD( FI+FIINC )
            S2 = SIND( FI+FIINC )
            H(1) = RMAXDI*C1
            V(1) = RMAXDI*S1
            H(2) = RMINDI*C1
            V(2) = RMINDI*S1
            H(3) = RMINDI*C2
            V(3) = RMINDI*S2
            H(4) = RMAXDI*C2
            V(4) = RMAXDI*S2
            H(5) = H(1)
            V(5) = V(1)
            FI = FI + FIINC
            IF(TPROJ(1:1).EQ.' ') THEN
              CALL DQPOL(5,H,V)
            ELSE
              CALL DQSHA(QSHA,2,H(4),V(4),'O')
              CALL DQSHA(QSHA,2,H(2),V(2),'I')
            END IF
          END DO

        END IF
      ELSE IF(TPROJ(2:3).EQ.'PP') THEN

C        E = (R1P - R1)/(R1*(R2-R1P))

        FI = 0.
        FIINC = 360./NPTS
        DO I=1,NPTS
          C1 = COSD( FI )
          S1 = SIND( FI )
          C2 = COSD( FI+FIINC )
          S2 = SIND( FI+FIINC )
          H(1)= RMAXDI*C1
          V(1)= RMAXDI*S1
          H(2)= RMINDI*C1
          V(2)= RMINDI*S1
          H(3)= RMINDI*C2
          V(3)= RMINDI*S2
          H(4)= RMAXDI*C2
          V(4)= RMAXDI*S2
          H(5)= H(1)
          V(5)= V(1)
          FI = FI + FIINC
          IF(TPROJ(1:1).EQ.' ') THEN
            CALL DQPOL(5,H,V)
          ELSE
            CALL DQSHA(QSHA,2,H(4),V(4),'O')
            CALL DQSHA(QSHA,2,H(2),V(2),'I')
          END IF
        END DO
      ELSE IF(TPROJ(2:3).EQ.'FR') THEN
C
C    the 12-corner ITC is approximated as a circular cylinder and there-
C    fore becomes a rectangle in the phi/theta projection.
C    phi ranges here from -360 to 720 deg, is clipped to (0,400) deg in
C    the display routines
C
        H(1) = RMINDI
        H(2) = RMAXDI
        H(3) = RMAXDI
        H(4) = RMINDI
        H(5) = H(1)

        V(1) = -60.
        V(2) = -60.
        V(3) =  540.
        V(4) =  540.
        V(5) = V(1)

        CALL DQPOL( 5 , H, V )

      ELSE IF(TPROJ(2:3).EQ.'RZ') THEN

        H(1) = -ZMAXDI
        H(2) =  ZMAXDI
        H(3) =  ZMAXDI
        H(4) = -ZMAXDI
        H(5) = H(1)
        V(1) = RMINDI
        V(2) = RMINDI
        V(3) = RMAXDI
        V(4) = RMAXDI
        V(5) = V(1)
        CALL DQPOL( 5 , H, V )

        IF(TPROJ(1:1).EQ.'S') THEN

          DO I=1 , 5
            V(I) = -V(I)
          END DO
          CALL DQPOL( 5, H, V )

        END IF
      END IF
      RETURN
      END
*DK DGDVDT
C----------------------------------------------------------------------
C!  -
      SUBROUTINE DGDVDT( TPROJ, TDRMD, TUNIT)
C!
C!   Author   :- R.Vogl                12-SEP-1989
C!
C!   Inputs: TPROJ : projection to be drawn
C!        -
C!
C!   Outputs:
C!        -
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!
C?
C!======================================================================


C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DV
      COMMON /DVDGEO/ NSLIDV,NSLODV,NWAFDV,PALWDV,ZALWDV,
     &                TIILDV(15),TIOLDV(15),
     &                RMNIDV,RMXIDV,RMNODV,RMXODV,
     &                DELXDV,DELYDV,DELZDV,DELPDV,
     &                ZOFWDV(6), NWTSDV(15,2),
     &                RAILDV,RAOLDV,DFILDV(12),DFOLDV(15),
     &                CRILDV(2,2,15),CROLDV(2,2,15),
     &                NFRSDV(2),NZRSDV(2),
     &                PFRSDV(2),PZRSDV(2)
C     HARD WIRED IN P_DALB_ALEPH.FOR
      COMMON /DVDGE1/ FEVDDV
      LOGICAL FEVDDV
C------------------------------------------------------------------- DV
      COMMON /DVDDFZ/ DLSTDV,NTOTDV(2),
     &  FIMDDV(-2:18,2),DLFIDV(-2:18,2),
     &  ZLFTDV(6,2),ZRGTDV(6,2)
      COMMON /DVDLTF/ FISCDV
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *3 TPROJ
      CHARACTER *6 TDRMD
      CHARACTER *6 TUNIT
C      INTEGER GTSTUP

      PARAMETER( NPTS = 36 )
      DIMENSION H(NPTS+1),V(NPTS+1)

C
C  it is not sure in what direction the VDET-modules are tilted
C  so it might be possible that FAC has to be changed to -1 !!
C  ( especially one has to make sure from what direction the VDET
C   is viewed ( from positive or negative z !!)
C
CCC      DATA FAC / 1. /

C?      DATA SCALI /0.7/
C?      DATA SCALO /1.1/
      DATA SCALI /1./
      DATA SCALO /1./

C
C  statement-functions
C
C      HPER(X,Y,R) = X*(1+E*R2)/(1+E*R)
C      VPER(X,Y,R) = Y*(1+E*R2)/(1+E*R)
C

C  IF VDET GEOMETRY DOES NOT EXIST FOR GIVEN RUN: DON'T DRAW IT!

C      IRUN = IRUNDE(1)
C      ISTUP = GTSTUP('VD',IRUN)
C      IF (ISTUP.EQ.1) RETURN

      IF (.NOT. FEVDDV) RETURN

C
C draw the different projections
C

      IF ((TPROJ(2:3).EQ.'YX') .AND. (TUNIT.EQ.'BARREL' .OR.
     &    TUNIT.EQ.'MIDANG' .OR. TUNIT.EQ.'BAR+MA')) THEN

C  NWTSDV (I , J) contains wafer type of I-th Module in J-th slot
C     0 .... nothing in slot
C     1 .... Munich type wafer
C  2,3,4 ... Pisa type wafer
C
C  inner layer
        IF (TDRMD.EQ.'DETAIL') THEN
          DO I=1 , NSLIDV
            IF (NWTSDV(I,1).NE.0) THEN
               CALL DQL2E ( CRILDV(1,1,I),CRILDV(2,1,I),
     &                       CRILDV(1,2,I),CRILDV(2,2,I) )
            END IF
          END DO
C  outer layer
          DO I=1 , NSLODV
            IF (NWTSDV(I,2).NE.0) THEN
               CALL DQL2E ( CROLDV(1,1,I),CROLDV(2,1,I),
     &                       CROLDV(1,2,I),CROLDV(2,2,I) )
            END IF
          END DO
        ELSE IF (TDRMD.EQ.'SIMPLE') THEN
C
C  simple means that only two polygon lines that fully include the
C  VDET on the inside and on the outside are drawn
C
          FI = 0.
          FIINC = 360./NPTS
          DO I=1,NPTS
            C1 = COSD( FI )
            S1 = SIND( FI )
            C2 = COSD( FI+FIINC )
            S2 = SIND( FI+FIINC )
            H(1) = RAOLDV*C1*SCALO
            V(1) = RAOLDV*S1*SCALO
            H(2) = RAILDV*C1*SCALI
            V(2) = RAILDV*S1*SCALI
            H(3) = RAILDV*C2*SCALI
            V(3) = RAILDV*S2*SCALI
            H(4) = RAOLDV*C2*SCALO
            V(4) = RAOLDV*S2*SCALO
            H(5) = H(1)
            V(5) = V(1)
            FI = FI + FIINC
            CALL DQPOL(5,H,V)
          END DO

        END IF

      ELSE IF (TPROJ(2:3).EQ.'FR') THEN

        IF (TDRMD.EQ.'SIMPLE') THEN

          H(1) = RMNIDV*SCALI
          H(2) = RMXODV*SCALO
          H(3) = RMXODV*SCALO
          H(4) = RMNIDV*SCALI
          H(5) = H(1)
          V(1) = -60.
          V(2) = -60.
          V(3) =  540.
          V(4) =  540.
          V(5) = V(1)
          CALL DQPOL( 5 , H , V )
C
C          H(1) = RMNODV
C          H(2) = RMXODV
C          H(3) = RMXODV
C          H(4) = RMNODV
C          H(5) = H(1)
C          CALL DQPOL( 5 , H , V )

        ELSE IF (TDRMD.EQ.'DETAIL') THEN

          NPR=5

          DO I=1 , NSLIDV
            IF (NWTSDV(I,1).NE.0) THEN
              DX = CRILDV(1,2,I) - CRILDV(1,1,I)
              DY = CRILDV(2,2,I) - CRILDV(2,1,I)
              DO J = 0 , NPR+1
                ZPX = CRILDV(1,1,I) + ( J * DX / (NPR+1))
                ZPY = CRILDV(2,1,I) + ( J * DY / (NPR+1))
                H(J+1) = SQRT( ZPX**2 + ZPY**2 )
                IF (J.NE.0) THEN
                  V(J+1) = DFINXT(V(J),DATN2D(ZPY,ZPX))
                ELSE
                  V(J+1) = DATN2D(ZPY,ZPX)
                END IF
              END DO

              CALL DQPOL(NPR+2,H,V)

C
C  draw module shifted by +360 deg. in phi
C
              DO II=1 , NPR+2
                V(II) = V(II)+360.
              END DO
              CALL DQPOL(NPR+2,H,V)
C
C  draw module shifted by -360 deg. in phi
C
              DO II=1 , NPR+2
                V(II) = V(II)-720.
              END DO
              CALL DQPOL(NPR+2,H,V)
            END IF
          END DO

          DO I=1 , NSLODV
            IF (NWTSDV(I,2).NE.0) THEN
              DX = CROLDV(1,2,I) - CROLDV(1,1,I)
              DY = CROLDV(2,2,I) - CROLDV(2,1,I)
              DO J = 0 , NPR+1
                ZPX = CROLDV(1,1,I) + ( J * DX / (NPR+1))
                ZPY = CROLDV(2,1,I) + ( J * DY / (NPR+1))
                H(J+1) = SQRT( ZPX**2 + ZPY**2 )
                IF (J.NE.0) THEN
                  V(J+1) = DFINXT(V(J),DATN2D(ZPY,ZPX))
                ELSE
                  V(J+1) = DATN2D(ZPY,ZPX)
                END IF
              END DO

              CALL DQPOL(NPR+2,H,V)

C
C  draw module shifted by +360 deg. in phi
C
              DO II=1 , NPR+2
                V(II) = V(II)+360.
              END DO
              CALL DQPOL(NPR+2,H,V)
C
C  draw module shifted by -360 deg. in phi
C
              DO II=1 , NPR+2
                V(II) = V(II)-720.
              END DO
              CALL DQPOL(NPR+2,H,V)
            END IF
          END DO

        END IF

      ELSE IF (TPROJ(2:3).EQ.'RZ') THEN
        IF (TDRMD.EQ.'SIMPLE') THEN
          V(1) = RMNIDV*SCALI
          V(2) = V(1)
          V(3) = RMXODV*SCALO
          V(4) = V(3)
          V(5) = V(1)
C          H(1) = ZOFWDV(1)      - ZALWDV/2
C          H(2) = ZOFWDV(NWAFDV) + ZALWDV/2
          H(1) = ZLFTDV(1,1)
          H(2) = ZRGTDV(NWAFDV,1)
          H(3) = H(2)
          H(4) = H(1)
          H(5) = H(1)
          CALL DQPOL( 5 , H, V )

          IF (TPROJ(1:1).EQ.'S') THEN
            DO I=1, 5
              V(I) = -V(I)
            END DO
            CALL DQPOL( 5 , H, V )
          END IF

        ELSE IF (TDRMD.EQ.'DETAIL') THEN

          DO J=1,NWAFDV

C            H(1) = ZOFWDV(J) - ZALWDV/2
C            H(2) = ZOFWDV(J) + ZALWDV/2
C            H(3) = ZOFWDV(J) + ZALWDV/2
C            H(4) = ZOFWDV(J) - ZALWDV/2

            H(1) = ZLFTDV(J,1)
            H(2) = ZRGTDV(J,1)
            H(3) = ZRGTDV(J,1)
            H(4) = ZLFTDV(J,1)
            H(5) =  H(1)
C           ............................... INNER LAYRE
            V(1) = RMNIDV
            V(2) = RMNIDV
            V(3) = RMXIDV
            V(4) = RMXIDV
            V(5) = V(1)
            CALL DQPOL( 5 , H, V )

            IF (TPROJ(1:1).EQ.'S') THEN
              DO I=1, 5
                V(I) = -V(I)
              END DO
              CALL DQPOL( 5 , H, V )
            END IF

C           ............................... OUTER LAYRE
            V(1) = RMNODV
            V(2) = RMNODV
            V(3) = RMXODV
            V(4) = RMXODV
            V(5) = V(1)
            CALL DQPOL( 5 , H, V )

            IF (TPROJ(1:1).EQ.'S') THEN
              DO I=1, 5
                V(I) = -V(I)
              END DO
              CALL DQPOL( 5 , H, V )
            END IF

          END DO
        END IF
      ELSE IF (TPROJ(2:3).EQ.'FT') THEN

        RM = 0.5*(RMNIDV+RMXODV)
        ZZ = -ZOFWDV(1) + 0.5 * ZALWDV
        TMAX = - DATN2D(RM,ZZ)
C       TMAX = - DATN2D(RM,2.*PALWDV)
        TMIN = - 180. - TMAX

        H(1) = TMIN
        H(2) = TMAX
        H(3) = TMAX
        H(4) = TMIN
        H(5) = H(1)
        V(1) = -60.
        V(2) = -60.
        V(3) =  540.
        V(4) =  540.
        V(5) = V(1)
        CALL DQPOL( 5 , H , V )

      END IF
      END
C
C  end of DGDVDT
C





C######################################################################
C
*DK DGDTPC
      SUBROUTINE DGDTPC(TPROJ,TDRMD,TUNIT,ROT,RR)
C
C-----------------------------------------
C
C   Author   :- R.Vogl                   3-SEP-1989
C
C=========================================
C
C   Purpose   : draw TPC geometry in various projections
C   Inputs    : TPROJ : projection to be drawn
C   Outputs   : draws TPC geometry
C
C   called by : DALI3 (DGLD)
C
C   Comment   : based on a routine by R.F.Xu
C=========================================
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DT
      COMMON /DTPGEO/ FISTDT(3),
     &       CORNDT (2,10,6,3), POLCDT ( 2,10,3 ),
     &       CRNGDT (2,10,6,3),
     &       NTYPDT,NCORDT(3),NSLTDT,
     &       RMAXDT , RMINDT , ZMAXDT
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *3 TPROJ
      CHARACTER *6 TDRMD
      CHARACTER *6 TUNIT
      DIMENSION RR(3)

      DIMENSION H(50),V(50), HH(50),VV(50)

      DIMENSION ROT(6)
C     DIMENSION HRI(13),VRI(13),HLI(13),VLI(13)
      DIMENSION HRO(25),VRO(25),HLO(25),VLO(25)
      DIMENSION HSID(5),VSID(5)
      DIMENSION ISHAD(3)
      DATA ISHAD/3,4,4/,NSHAD/4/,QSHAD/0.38/
C     ...................................................... QSHAD=220/538

C
C  STATEMENT FUNCTIONS :
C
      HCOR(X,Y,Z) = (ROT(2)*X+ROT(1)*Y)*ROT(4) + ROT(3)*Z
      VCOR(X,Y,Z) = ROT(6)*( ROT(2)*Y - ROT(1)*X ) +
     &              ROT(5)*( ROT(4)*Z - ROT(3)*(ROT(2)*X + ROT(1)*Y) )
C
C  statement-functions
C
C      HPER(X,Y,R) = X*(1+E*R2)/(1+E*R)
C      VPER(X,Y,R) = Y*(1+E*R2)/(1+E*R)
C
C      R1 = RR(1)
C      R2 = RR(2)
C      R1P= RR(3)

C
C  ACTUAL CODE
C
      IF ((TPROJ(2:3).EQ.'YX') .AND. (TUNIT.EQ.'BARREL' .OR.
     &     TUNIT.EQ.'MIDANG' .OR. TUNIT.EQ.'BAR+MA' )) THEN
        IF (TDRMD.EQ.'DETAIL') THEN
          DO ITYPE = 1 , NTYPDT
            DO J = 1 , NSLTDT
              DO K = 1 , NCORDT(ITYPE)
                H(K)= CORNDT(1,K,J,ITYPE)
                V(K)= CORNDT(2,K,J,ITYPE)
              END DO
              H(NCORDT(ITYPE)+1) = H(1)
              V(NCORDT(ITYPE)+1) = V(1)
              IF(TPROJ(1:1).EQ.' ') THEN
                CALL DQPOL(NCORDT(ITYPE)+1,H,V)
              ELSE
                CALL DQSHA(QSHAD,NSHAD,H(ISHAD(ITYPE))
     &                                ,V(ISHAD(ITYPE)),'O')
                IF(ITYPE.EQ.1) THEN
                  H(9)=H(1)
                  V(9)=V(1)
                  CALL DQSHA(QSHAD,2,H(7),V(7),'I')
                  CALL DQSHA(QSHAD,2,H(8),V(8),'I')
                  CALL DQSHA(QSHAD,2,H(1),V(1),'I')
                END IF
              END IF
            END DO
          END DO
        ELSE IF (TDRMD.EQ.'NOGAPS') THEN
          DO ITYPE = 1 , NTYPDT
            DO J = 1 , NSLTDT
              DO K = 1 , NCORDT(ITYPE)
                H(K)= CRNGDT(1,K,J,ITYPE)
                V(K)= CRNGDT(2,K,J,ITYPE)
              END DO
              H(NCORDT(ITYPE)+1) = H(1)
              V(NCORDT(ITYPE)+1) = V(1)
              IF(TPROJ(1:1).EQ.' ') THEN
                CALL DQPOL(NCORDT(ITYPE)+1,H,V)
              ELSE
                CALL DQSHA(QSHAD,NSHAD,H(ISHAD(ITYPE))
     &                                ,V(ISHAD(ITYPE)),'O')
                IF(ITYPE.EQ.1) THEN
                  H(9)=H(1)
                  V(9)=V(1)
                  CALL DQSHA(QSHAD,2,H(7),V(7),'I')
                  CALL DQSHA(QSHAD,2,H(8),V(8),'I')
                  CALL DQSHA(QSHAD,2,H(1),V(1),'I')
                END IF
              END IF
            END DO
          END DO
        ELSE IF (TDRMD.EQ.'SIMPLE') THEN
          J1 = 1
          J2 = 1
          DO J = 1 , NSLTDT

            H(J1  ) = CRNGDT(1,8,J,1)
            V(J1  ) = CRNGDT(2,8,J,1)
            H(J1+1) = CRNGDT(1,1,J,1)
            V(J1+1) = CRNGDT(2,1,J,1)
            J1 = J1+2

            HH(J2  ) = CRNGDT(1,6,J,2)
            VV(J2  ) = CRNGDT(2,6,J,2)
            HH(J2+1) = CRNGDT(1,5,J,2)
            VV(J2+1) = CRNGDT(2,5,J,2)
            HH(J2+2) = CRNGDT(1,4,J,2)
            VV(J2+2) = CRNGDT(2,4,J,2)

            HH(J2+3) = CRNGDT(1,6,J,3)
            VV(J2+3) = CRNGDT(2,6,J,3)
            HH(J2+4) = CRNGDT(1,5,J,3)
            VV(J2+4) = CRNGDT(2,5,J,3)
            HH(J2+5) = CRNGDT(1,4,J,3)
            VV(J2+5) = CRNGDT(2,4,J,3)

            J2 = J2 + 6

          END DO

          H (J1) = H (1)
          V (J1) = V (1)
          HH(J2) = HH(1)
          VV(J2) = VV(1)

          CALL DQPOL( J1, H, V )
          CALL DQPOL( J2, HH, VV )

        END IF
      ELSE IF (TPROJ(2:3).EQ.'PP') THEN

C        E = (R1P - R1)/(R1*(R2-R1P))

        IF (TDRMD.EQ.'DETAIL') THEN

          DO ITYPE = 1 , NTYPDT
            DO J = 1 , NSLTDT
              DO K = 1 , NCORDT(ITYPE)
                H(K)= CORNDT(1,K,J,ITYPE)
                V(K)= CORNDT(2,K,J,ITYPE)
              END DO
              H(NCORDT(ITYPE)+1) = H(1)
              V(NCORDT(ITYPE)+1) = V(1)
              IF(TPROJ(1:1).EQ.' ') THEN
                CALL DQPOL(NCORDT(ITYPE)+1,H,V)
              ELSE
                CALL DQSHA(QSHAD,NSHAD,H(ISHAD(ITYPE))
     &                                ,V(ISHAD(ITYPE)),'O')
                IF(ITYPE.EQ.1) THEN
                  H(9)=H(1)
                  V(9)=V(1)
                  CALL DQSHA(QSHAD,2,H(7),V(7),'I')
                  CALL DQSHA(QSHAD,2,H(8),V(8),'I')
                  CALL DQSHA(QSHAD,2,H(1),V(1),'I')
                END IF
              END IF
            END DO
          END DO

        ELSE IF (TDRMD.EQ.'NOGAPS') THEN

          DO ITYPE = 1 , NTYPDT
            DO J = 1 , NSLTDT
              DO K = 1 , NCORDT(ITYPE)
                H(K)= CRNGDT(1,K,J,ITYPE)
                V(K)= CRNGDT(2,K,J,ITYPE)
              END DO
              H(NCORDT(ITYPE)+1) = H(1)
              V(NCORDT(ITYPE)+1) = V(1)
              IF(TPROJ(1:1).EQ.' ') THEN
                CALL DQPOL(NCORDT(ITYPE)+1,H,V)
              ELSE
                CALL DQSHA(QSHAD,NSHAD,H(ISHAD(ITYPE))
     &                                ,V(ISHAD(ITYPE)),'O')
                IF(ITYPE.EQ.1) THEN
                  H(9)=H(1)
                  V(9)=V(1)
                  CALL DQSHA(QSHAD,2,H(7),V(7),'I')
                  CALL DQSHA(QSHAD,2,H(8),V(8),'I')
                  CALL DQSHA(QSHAD,2,H(1),V(1),'I')
                END IF
              END IF
            END DO
          END DO

        ELSE IF (TDRMD.EQ.'SIMPLE') THEN

          J1 = 1
          J2 = 1
          DO J = 1 , NSLTDT

C INNER EDGES, TYPE 1 MODULES
            H(J1  ) = CRNGDT(1,8,J,1)
            V(J1  ) = CRNGDT(2,8,J,1)
            H(J1+1) = CRNGDT(1,1,J,1)
            V(J1+1) = CRNGDT(2,1,J,1)
            J1 = J1+2

C  OUTER EDGES, TYPE 2 MODULES
            HH(J2  ) = CRNGDT(1,6,J,2)
            VV(J2  ) = CRNGDT(2,6,J,2)
            HH(J2+1) = CRNGDT(1,5,J,2)
            VV(J2+1) = CRNGDT(2,5,J,2)
            HH(J2+2) = CRNGDT(1,4,J,2)
            VV(J2+2) = CRNGDT(2,4,J,2)

C OUTER EDGES , TYPE 3 MODULES
            HH(J2+3) = CRNGDT(1,6,J,3)
            VV(J2+3) = CRNGDT(2,6,J,3)
            HH(J2+4) = CRNGDT(1,5,J,3)
            VV(J2+4) = CRNGDT(2,5,J,3)
            HH(J2+5) = CRNGDT(1,4,J,3)
            VV(J2+5) = CRNGDT(2,4,J,3)

            J2 = J2 + 6

          END DO

          H (J1) = H (1)
          V (J1) = V (1)
          HH(J2) = HH(1)
          VV(J2) = VV(1)

          CALL DQPOL( J1, H, V )
          CALL DQPOL( J2, HH, VV )
        END IF
      ELSE IF (TPROJ(2:3).EQ.'FR') THEN
        IF (TDRMD.EQ.'DETAIL') THEN

          DX = CORNDT(1,1,1,2)-CORNDT(1,10,1,2)
          DY = CORNDT(2,1,1,2)-CORNDT(2,10,1,2)
          DISSQ = DX**2 + DY**2
          RMIDL = SQRT( POLCDT(1,1,2)**2 - DISSQ / 4 )

          FMIDL = (POLCDT(2,10,2) - POLCDT(2,1,2))/2

          DO ITYPE = 1 , NTYPDT
            IF (ITYPE.NE.2) THEN

              DO J = 1 , NSLTDT

                DO K = 1 , NCORDT(ITYPE)
                  H(K)=POLCDT(1,K,ITYPE)
                  V(K)=POLCDT(2,K,ITYPE)+60.*FLOAT(J-1)
                END DO
                H(NCORDT(ITYPE)+1) = H(1)
                V(NCORDT(ITYPE)+1) = V(1)
                CALL DQPOL(NCORDT(ITYPE)+1,H,V)
C  shift by +360. deg
                DO K = 1 , NCORDT(ITYPE)+1
                  V(K)=V(K) + 360.
                END DO
                CALL DQPOL(NCORDT(ITYPE)+1,H,V)
C  shift by -360. deg
                DO K = 1 , NCORDT(ITYPE)+1
                  V(K)=V(K) - 720.
                END DO
                CALL DQPOL(NCORDT(ITYPE)+1,H,V)

              END DO

            ELSE

              DO J = 1 , NSLTDT

                DO K = 1 , NCORDT(ITYPE)
                  H(K)=POLCDT(1,K,ITYPE)
                  V(K)=POLCDT(2,K,ITYPE)+60.*FLOAT(J-1)
                END DO

C  insert point halfway between corners 10 and 1 in order to account
C  for the differences of radii

                H(NCORDT(ITYPE)+1) = RMIDL
                V(NCORDT(ITYPE)+1) = POLCDT(2,1,2)+60.*FLOAT(J-1)+FMIDL

                H(NCORDT(ITYPE)+2) = H(1)
                V(NCORDT(ITYPE)+2) = V(1)

                CALL DQPOL(NCORDT(ITYPE)+2,H,V)
C  shift by +360. deg
                DO K = 1 , NCORDT(ITYPE)+2
                  V(K)=V(K) + 360.
                END DO
                CALL DQPOL(NCORDT(ITYPE)+2,H,V)
C  shift by -360. deg
                DO K = 1 , NCORDT(ITYPE)+2
                  V(K)=V(K) - 720.
                END DO
                CALL DQPOL(NCORDT(ITYPE)+2,H,V)

              END DO
            END IF
          END DO
        ELSE IF(TDRMD.EQ.'SIMPLE') THEN

          H(1) = RMINDT
          H(2) = RMAXDT
          H(3) = RMAXDT
          H(4) = RMINDT
          H(5) = H(1)
          V(1) = -60.
          V(2) = -60.
          V(3) =  540.
          V(4) =  540.
          V(5) = V(1)
          CALL DQPOL( 5, H , V )

        END IF
      ELSE IF(TPROJ(2:3).EQ.'RZ') THEN

        H(1) = -ZMAXDT
        H(2) =  ZMAXDT
        H(3) =  ZMAXDT
        H(4) = -ZMAXDT
        H(5) = H(1)
        V(1) = RMINDT
        V(2) = RMINDT
        V(3) = RMAXDT
        V(4) = RMAXDT
        V(5) = V(1)
        CALL DQPOL( 5, H , V )
        CALL DQL2E(0.,RMINDT,0.,RMAXDT)

        IF(TPROJ(1:1).EQ.'S') THEN

          DO I=1,5
            V(I) = -V(I)
          END DO
          CALL DQPOL( 5, H , V )
          CALL DQL2E(0.,-RMINDT,0.,-RMAXDT)

        END IF
      ELSE IF(TPROJ(2:3).EQ.'FZ') THEN
        IF (TDRMD.EQ.'DETAIL') THEN

          H(1) = -ZMAXDT
          H(2) =  ZMAXDT
          H(3) =  ZMAXDT
          H(4) = -ZMAXDT
          H(5) = H(1)
          V(1) = -60.
          V(2) = -60.
          V(3) =  540.
          V(4) =  540.
          V(5) = V(1)
          CALL DQPOL( 5, H , V )
C          DO FF = -360. , 720. , 360.
C            CALL DQL2E(-ZMAXDT,FF,ZMAXDT,FF)
C          END DO
        ELSE IF(TDRMD.EQ.'SIMPLE') THEN

          H(1) = -ZMAXDT
          H(2) =  ZMAXDT
          H(3) =  ZMAXDT
          H(4) = -ZMAXDT
          H(5) = H(1)
          V(1) = -60.
          V(2) = -60.
          V(3) =  540.
          V(4) =  540.
          V(5) = V(1)
          CALL DQPOL( 5, H , V )

        END IF
      ELSE IF(TPROJ(2:3).EQ.'YZ') THEN

        H(1) = -ZMAXDT
        H(2) =  ZMAXDT
        H(3) =  ZMAXDT
        H(4) = -ZMAXDT
        H(5) = H(1)
        V(1) = -RMAXDT
        V(2) = -RMAXDT
        V(3) =  RMAXDT
        V(4) =  RMAXDT
        V(5) = V(1)
        CALL DQPOL( 5, H , V )
        CALL DQL2E(0.,-RMAXDT,0.,RMAXDT)

      ELSE IF(TPROJ(2:3).EQ.'FT') THEN
        T11=-DATN2D(RMINDT,ZMAXDT)
        T12=-DATN2D(RMAXDT,ZMAXDT)
        T21=-180.-T11
        T22=-180.-T12

        H(1) = T21
        H(2) = T11
        H(3) = T11
        H(4) = T21
        H(5) = H(1)
        V(1) = -60.
        V(2) = -60.
        V(3) =  540.
        V(4) =  540.
        V(5) = V(1)
        CALL DQPOL( 5 , H, V )

        IF(TDRMD.EQ.'SIMPLE') RETURN
        CALL DQREC(T12,-60.,T11,540.)
        CALL DQREC(T21,-60.,T22,540.)
C
C  draw indicators for gaps between sectors in theta/phi projection
C
        TSCAL = T21 + 180.
        RSCAL = POLCDT(1,4,3) - POLCDT(1,2,1)
CZZZZZ

        DIS1 = (POLCDT(1,3,1) - POLCDT(1,2,1)) * TSCAL / RSCAL
        DIS2 = (POLCDT(1,3,3) - POLCDT(1,2,1)) * TSCAL / RSCAL
        DIS3 = (POLCDT(1,4,3) - POLCDT(1,2,1)) * TSCAL / RSCAL

        CALL DQL2E(T21-DIS1,-360.,T21-DIS1,720.)
        DO FI = -360.,720.,60.
         CALL DQL2E(T21     ,POLCDT(2,3,1)+FI,
     &               T21-DIS1,POLCDT(2,3,1)+FI)
         CALL DQL2E(T21-DIS1,POLCDT(2,1,2)+FI,
     &               T21-DIS2,POLCDT(2,1,2)+FI)
         CALL DQL2E(T21-DIS1,POLCDT(2,1,3)+FI,
     &               T21-DIS2,POLCDT(2,1,3)+FI)
         CALL DQL2E(T21-DIS2,POLCDT(2,3,2)+FI,
     &               T21-DIS3,POLCDT(2,3,2)+FI)
         CALL DQL2E(T21-DIS2,POLCDT(2,3,3)+FI,
     &               T21-DIS3,POLCDT(2,3,3)+FI)
         CALL DQL2E(T21-DIS2,POLCDT(2,2,2)+FI,
     &               T21-DIS2,POLCDT(2,3,2)+FI)
         CALL DQL2E(T21-DIS2,POLCDT(2,2,3)+FI,
     &               T21-DIS2,POLCDT(2,3,3)+FI)
        END DO
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C  draw complete TPC-Endcap structure in theta/phi projection
C  NOGAPS is used!
C  it is possible that FAC has to changed to +1 in order to get a correct view
C  of the TPC endcaps in FT
C        FAC =-1.
C        NIP = 3
C        FILAS = 0.
C        DO ITYPE = 1 , NTYPDT
C          DO J = 1 , NSLTDT
C            KK = 0
C            DO K = 1 , NCORDT(ITYPE)
C              IF(K.LT.NCORDT(ITYPE)) THEN
C                DX = CRNGDT(1,K+1,J,ITYPE)-CRNGDT(1,K,J,ITYPE)
C                DY = CRNGDT(2,K+1,J,ITYPE)-CRNGDT(2,K,J,ITYPE)
C              ELSE
C                DX = CRNGDT(1,  1,J,ITYPE)-CRNGDT(1,K,J,ITYPE)
C                DY = CRNGDT(2,  1,J,ITYPE)-CRNGDT(2,K,J,ITYPE)
C              END IF
C              DO L=0,NIP
C                KK    = KK + 1
C                XX    = CRNGDT(1,K,J,ITYPE)+(L*DX/(NIP+1))
C                YY    = CRNGDT(2,K,J,ITYPE)+(L*DY/(NIP+1))
C                RHO   = SQRT(XX*XX + YY*YY)
C                THETA = FAC*DATN2D(RHO,ZMAXDT)
C                FIRAW = DATN2D(YY,XX)
C                FI    = DFINXT(FIRAW,FILAS)
C                FILAS = FI
C                H(KK) = THETA
C                V(KK) = FI
C              END DO
C            END DO
C            H(KK+1) = H(1)
CC            V(KK+1) = V(1)
CC----------------------------------------------------draw left hand side first
C            CALL DQPOL(KK+1,H,V)
CC  shift by +360. deg
C            DO K = 1 , KK+1
C              V(K)=V(K) + 360.
C            END DO
C            CALL DQPOL(KK+1,H,V)
CC  shift by -360. deg
C            DO K = 1 , KK+1
C              V(K)=V(K) - 720.
C            END DO
C            CALL DQPOL(KK+1,H,V)
CC----------------------------------------------------now draw right hand side
C            DO K = 1 , KK+1
C              H(K) = FAC*180. - H(K)
C              V(K) = V(K) + 360.
C            END DO
C            CALL DQPOL(KK+1,H,V)
CC  shift by +360. deg
C            DO K = 1 , KK+1
C              V(K)=V(K) + 360.
C            END DO
C            CALL DQPOL(KK+1,H,V)
CC  shift by -360. deg
C            DO K = 1 , KK+1
C              V(K)=V(K) - 720.
C            END DO
C            CALL DQPOL(KK+1,H,V)
C          END DO
C        END DO
CC----------------------------------------------------------------------
C----------------------------------------------------------------------
      ELSE IF (TPROJ.EQ.'3-D') THEN
        IF (TDRMD.EQ.'SIMPLE') THEN
          J1 = 1
          J2 = 1
          DO J = 1 , NSLTDT
C
CC   right side ( z>0 ) inner corners
C
C            HRI(J1  )=HCOR(CRNGDT(1,8,J,1),CRNGDT(2,8,J,1), ZMAXDT)
C            VRI(J1  )=VCOR(CRNGDT(1,8,J,1),CRNGDT(2,8,J,1), ZMAXDT)
C            HRI(J1+1)=HCOR(CRNGDT(1,1,J,1),CRNGDT(2,1,J,1), ZMAXDT)
C            VRI(J1+1)=VCOR(CRNGDT(1,1,J,1),CRNGDT(2,1,J,1), ZMAXDT)
C
CC   left side ( z<0 ) inner corners
C
C            HLI(J1  )=HCOR(CRNGDT(1,8,J,1),CRNGDT(2,8,J,1),-ZMAXDT)
C            VLI(J1  )=VCOR(CRNGDT(1,8,J,1),CRNGDT(2,8,J,1),-ZMAXDT)
C            HLI(J1+1)=HCOR(CRNGDT(1,1,J,1),CRNGDT(2,1,J,1),-ZMAXDT)
C            VLI(J1+1)=VCOR(CRNGDT(1,1,J,1),CRNGDT(2,1,J,1),-ZMAXDT)
C
CC    draw inner TPC edges connecting the left and right endplates
C
C            CALL DQL2E( HRI(J1  ),VRI(J1  ),HLI(J1  ),VLI(J1  ) )
C            CALL DQL2E( HRI(J1+1),VRI(J1+1),HLI(J1+1),VLI(J1+1) )
C
C            J1 = J1 + 2
C
C     right side ( z>0 ) outer corners

            HRO(J2  )=HCOR(CRNGDT(1,6,J,2),CRNGDT(2,6,J,2), ZMAXDT)
            VRO(J2  )=VCOR(CRNGDT(1,6,J,2),CRNGDT(2,6,J,2), ZMAXDT)
            HRO(J2+1)=HCOR(CRNGDT(1,5,J,2),CRNGDT(2,5,J,2), ZMAXDT)
            VRO(J2+1)=VCOR(CRNGDT(1,5,J,2),CRNGDT(2,5,J,2), ZMAXDT)

            HRO(J2+2)=HCOR(CRNGDT(1,6,J,3),CRNGDT(2,6,J,3), ZMAXDT)
            VRO(J2+2)=VCOR(CRNGDT(1,6,J,3),CRNGDT(2,6,J,3), ZMAXDT)
            HRO(J2+3)=HCOR(CRNGDT(1,5,J,3),CRNGDT(2,5,J,3), ZMAXDT)
            VRO(J2+3)=VCOR(CRNGDT(1,5,J,3),CRNGDT(2,5,J,3), ZMAXDT)

C   left side ( z<0 ) outer corners

            HLO(J2  )=HCOR(CRNGDT(1,6,J,2),CRNGDT(2,6,J,2),-ZMAXDT)
            VLO(J2  )=VCOR(CRNGDT(1,6,J,2),CRNGDT(2,6,J,2),-ZMAXDT)
            HLO(J2+1)=HCOR(CRNGDT(1,5,J,2),CRNGDT(2,5,J,2),-ZMAXDT)
            VLO(J2+1)=VCOR(CRNGDT(1,5,J,2),CRNGDT(2,5,J,2),-ZMAXDT)

            HLO(J2+2)=HCOR(CRNGDT(1,6,J,3),CRNGDT(2,6,J,3),-ZMAXDT)
            VLO(J2+2)=VCOR(CRNGDT(1,6,J,3),CRNGDT(2,6,J,3),-ZMAXDT)
            HLO(J2+3)=HCOR(CRNGDT(1,5,J,3),CRNGDT(2,5,J,3),-ZMAXDT)
            VLO(J2+3)=VCOR(CRNGDT(1,5,J,3),CRNGDT(2,5,J,3),-ZMAXDT)

            IF (J.LT.NSLTDT) THEN
              H1R=HCOR(CRNGDT(1,6,J+1,2),CRNGDT(2,6,J+1,2), ZMAXDT)
              V1R=VCOR(CRNGDT(1,6,J+1,2),CRNGDT(2,6,J+1,2), ZMAXDT)
              H1L=HCOR(CRNGDT(1,6,J+1,2),CRNGDT(2,6,J+1,2),-ZMAXDT)
              V1L=VCOR(CRNGDT(1,6,J+1,2),CRNGDT(2,6,J+1,2),-ZMAXDT)
            ELSE
              H1R=HCOR(CRNGDT(1,6,  1,2),CRNGDT(2,6,  1,2), ZMAXDT)
              V1R=VCOR(CRNGDT(1,6,  1,2),CRNGDT(2,6,  1,2), ZMAXDT)
              H1L=HCOR(CRNGDT(1,6,  1,2),CRNGDT(2,6,  1,2),-ZMAXDT)
              V1L=VCOR(CRNGDT(1,6,  1,2),CRNGDT(2,6,  1,2),-ZMAXDT)
            END IF

C   draw outer TPC edges connecting the left and right endplates

            HSID(1) = HRO(J2  )
            HSID(2) = HRO(J2+1)
            HSID(3) = HLO(J2+1)
            HSID(4) = HLO(J2  )
            HSID(5) = HSID(1)
            VSID(1) = VRO(J2  )
            VSID(2) = VRO(J2+1)
            VSID(3) = VLO(J2+1)
            VSID(4) = VLO(J2  )
            VSID(5) = VSID(1)
            CALL DQPOL(5,HSID,VSID)

C
C consecutive modules are drawn in alternating orientation in order to
C avoid drawing the same line twice in two different directions
C
            HSID(1) = HRO(J2+2)
            HSID(2) = HRO(J2+1)
            HSID(3) = HLO(J2+1)
            HSID(4) = HLO(J2+2)
            HSID(5) = HSID(1)
            VSID(1) = VRO(J2+2)
            VSID(2) = VRO(J2+1)
            VSID(3) = VLO(J2+1)
            VSID(4) = VLO(J2+2)
            VSID(5) = VSID(1)
            CALL DQPOL(5,HSID,VSID)

            HSID(1) = HRO(J2+2)
            HSID(2) = HRO(J2+3)
            HSID(3) = HLO(J2+3)
            HSID(4) = HLO(J2+2)
            HSID(5) = HSID(1)
            VSID(1) = VRO(J2+2)
            VSID(2) = VRO(J2+3)
            VSID(3) = VLO(J2+3)
            VSID(4) = VLO(J2+2)
            VSID(5) = VSID(1)
            CALL DQPOL(5,HSID,VSID)

            HSID(1) = H1R
            HSID(2) = HRO(J2+3)
            HSID(3) = HLO(J2+3)
            HSID(4) = H1L
            HSID(5) = HSID(1)
            VSID(1) = V1R
            VSID(2) = VRO(J2+3)
            VSID(3) = VLO(J2+3)
            VSID(4) = V1L
            VSID(5) = VSID(1)

            CALL DQPOL(5,HSID,VSID)

C            CALL DQL2E( HRO(J2  ),VRO(J2  ),HLO(J2  ),VLO(J2  ) )
C            CALL DQL2E( HRO(J2+1),VRO(J2+1),HLO(J2+1),VLO(J2+1) )
C            CALL DQL2E( HRO(J2+2),VRO(J2+2),HLO(J2+2),VLO(J2+2) )
C            CALL DQL2E( HRO(J2+3),VRO(J2+3),HLO(J2+3),VLO(J2+3) )

            J2 = J2 + 4

          END DO

C         HRI(J1) = HRI(1)
C         VRI(J1) = VRI(1)
C         HLI(J1) = HLI(1)
C         VLI(J1) = VLI(1)
          HRO(J2) = HRO(1)
          VRO(J2) = VRO(1)
          HLO(J2) = HLO(1)
          VLO(J2) = VLO(1)

C   draw inner and outer edges of left and right endplates

C         CALL DQPOL( J1, HRI, VRI )
C         CALL DQPOL( J1, HLI, VLI )
          CALL DQPOL( J2, HRO, VRO )
          CALL DQPOL( J2, HLO, VLO )
        END IF
      END IF
      END
C
C  end of DGDTPC
C


C
C
*DK DGDCOS
      SUBROUTINE DGDCOS(PHI,AL1,AM1,AL2,AM2)
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann ,R.F.XU               18-AUGUST-1988
C
C!:Calculate new coordinate system direction cosine.
C    Inputs    : PHI is rotation angle of new X axis in phi direction
C    Outputs   : AL1,AM1 are direction cosine of new X axis
C                AL1,AM1 are direction cosine of new y axis
C    Called by : DGMBXY,DGMMXY
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
C      PHI=30.*FLOAT(J-1)
      AL1 =COSD(PHI)
      AM1 =COSD(PHI-90.)
      AL2 =COSD(PHI+90.)
      AM2 =AL1
      RETURN
      END
C
C






C
C
*DK DGDLCA
      SUBROUTINE DGDLCA(TPROJ,TUNIT)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- R.Vogl                   2-SEP-1989
C!
C!   Inputs:  TPROJ: projection to be drawn
C!        -
C!
C!   Outputs:
C!        -
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!
C?
C!======================================================================


C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DL
      COMMON /DLCGEO/ ZDISDL,RINNDL,ROUTDL,ZLENDL
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *3 TPROJ
      CHARACTER *6 TUNIT

      DIMENSION HCRL(5),HCRR(5),VCRU(5),VCRD(5),PMLCR(0:4)
      DATA HCRL/-309.4,-307.3,-307.3,-309.4,-309.4/
      DATA HCRR/ 309.4, 307.3, 307.3, 309.4, 309.4/
      DATA VCRU/  17.3,  17.3,  43.9,  43.9,  17.3/
      DATA VCRD/ -17.3, -17.3, -43.9, -43.9, -17.3/

      PARAMETER ( NPTS=36)
      DIMENSION HR(NPTS+3),VR(NPTS+3),HL(NPTS+3),VL(NPTS+3)

      IF ((TPROJ(2:3).EQ.'YX') .AND. (TUNIT.EQ.'ENDCAP' )) THEN
        FI = 270.
        FIINC = 360./ NPTS
        DO I = 1, NPTS/2+1
          C = COSD(FI)
          S = SIND(FI)
          HR(I)        = RINNDL*C
          VR(I)        = RINNDL*S
          HR(NPTS-I+3) = ROUTDL*C
          VR(NPTS-I+3) = ROUTDL*S
          HL(I)        = -HR(I)
          VL(I)        =  VR(I)
          HL(NPTS-I+3) = -HR(NPTS-I+3)
          VL(NPTS-I+3) =  VR(NPTS-I+3)
          FI = FI + FIINC
        END DO
        HR(NPTS+3) = HR(1)
        VR(NPTS+3) = VR(1)
        HL(NPTS+3) = HL(1)
        VL(NPTS+3) = VL(1)
        CALL DQPOL( NPTS+3, HR, VR )
        CALL DQPOL( NPTS+3, HL, VL )
      ELSE IF (TPROJ(2:3).EQ.'RZ') THEN

        HR(1) = ZDISDL
        HR(2) = ZDISDL+ZLENDL
        HR(3) = ZDISDL+ZLENDL
        HR(4) = ZDISDL
        HR(5) = HR(1)
        VR(1) = RINNDL
        VR(2) = RINNDL
        VR(3) = ROUTDL
        VR(4) = ROUTDL
        VR(5) = VR(1)
        DO I=1 , 5
          HL(I) = -HR(I)
          VL(I) =  VR(I)
        END DO
        CALL DQPOL(5,HR,VR)
        CALL DQPOL(5,HL,VL)
        CALL DPARGV(56,'CLC',4,CLC)
        IF(CLC.GT.0.) THEN
          CALL DV_LC_CRACK(PMLCR)
          IF(PMLCR(0).GT.0.) THEN
            CALL DPARGV(10,'PFI',2,FIMID)
            FIMIN=FIMID-90.
            FIMAX=FIMID+90.
            IF(FIMID-90.LE.90..AND.90.LT.FIMID+90.) THEN
              IF(PMLCR(1).GT.0.) CALL DQPOL(5,HCRL,VCRU)
              IF(PMLCR(4).GT.0.) CALL DQPOL(5,HCRR,VCRU)
            ELSE
              IF(PMLCR(2).GT.0.) CALL DQPOL(5,HCRL,VCRU)
              IF(PMLCR(3).GT.0.) CALL DQPOL(5,HCRR,VCRU)
            END IF
          END IF
        ELSE
          PMLCR(0)=0.
        END IF

        IF (TPROJ(1:1).EQ.'S') THEN

          DO I=1 , 5
            VR(I) = -VR(I)
            VL(I) = -VL(I)
          END DO
          CALL DQPOL(5,HR,VR)
          CALL DQPOL(5,HL,VL)
          IF(PMLCR(0).GT.0.) THEN
            IF(FIMID-90.LE.270..AND.270.LT.FIMID+90.) THEN
              IF(PMLCR(1).GT.0.) CALL DQPOL(5,HCRL,VCRD)
              IF(PMLCR(4).GT.0.) CALL DQPOL(5,HCRR,VCRD)
            ELSE
              IF(PMLCR(2).GT.0.) CALL DQPOL(5,HCRL,VCRD)
              IF(PMLCR(3).GT.0.) CALL DQPOL(5,HCRR,VCRD)
            END IF
          END IF

        END IF
      ELSE IF (TPROJ(2:3).EQ.'FT') THEN
        TMINA = - DATN2D(RINNDL , ZDISDL+ZLENDL )
        TMAXA = - DATN2D(ROUTDL , ZDISDL )
        TMINB = - ( 180. + TMINA )
        TMAXB = - ( 180. + TMAXA )

        HR(1) = TMAXA
        HR(2) = TMINA
        HR(3) = TMINA
        HR(4) = TMAXA
        HR(5) = HR(1)
        VR(1) = -60.
        VR(2) = -60.
        VR(3) =  540.
        VR(4) =  540.
        VR(5) = VR(1)

        HL(1) = TMAXB
        HL(2) = TMINB
        HL(3) = TMINB
        HL(4) = TMAXB
        HL(5) = HL(1)
        VL(1) = -60.
        VL(2) = -60.
        VL(3) =  540.
        VL(4) =  540.
        VL(5) = VL(1)

        CALL DQPOL(5,HR,VR)
        CALL DQPOL(5,HL,VL)

C
C  draw also the LCAL-intersecting line at 90deg and 270deg
C
        CALL DQL2E(TMAXA,  90.,TMINA,  90.)
        CALL DQL2E(TMAXA, 270.,TMINA, 270.)
        CALL DQL2E(TMAXA, 450.,TMINA, 450.)
        CALL DQL2E(TMAXA, 630.,TMINA, 630.)
        CALL DQL2E(TMAXA, -90.,TMINA, -90.)
        CALL DQL2E(TMAXA,-270.,TMINA,-270.)
        CALL DQL2E(TMAXB,  90.,TMINB,  90.)
        CALL DQL2E(TMAXB, 270.,TMINB, 270.)
        CALL DQL2E(TMAXB, 450.,TMINB, 450.)
        CALL DQL2E(TMAXB, 630.,TMINB, 630.)
        CALL DQL2E(TMAXB, -90.,TMINB, -90.)
        CALL DQL2E(TMAXB,-270.,TMINB,-270.)
      END IF
      END
CC
CC
CC
C*DK DGDSAT
C      SUBROUTINE DGDSAT (TPROJ,TUNIT)
CC----------------------------------------------------------------------
CC!  -
CC!
CC!   Author   :- R.Vogl                11-SEP-1989
CC!
CC!   Inputs: TPROJ : projection to be drawn
CC!        -
CC!
CC!   Outputs:
CC!        -
CC!
CC!   Libraries required:
CC!
CC!   Description draw the SATR geometry
CC!   ===========
CC!
CC?
CC!======================================================================
C
C      INCLUDE 'DALI_CF.INC'
C      CHARACTER *3 TPROJ
C      CHARACTER *6 TUNIT
C      INTEGER GTSTUP
C
C      PARAMETER ( NPTS=36)
C      DIMENSION HR(NPTS+3),VR(NPTS+3),HL(NPTS+3),VL(NPTS+3)
C
CC  IF SATR GEOMETRY COULD NOT BE READ FOR GIVEN RUN: DON'T DRAW IT!
C
C      IRUN = IRUNDE(1)
C      ISTUP = GTSTUP('PM',IRUN)
CC for SATR it's strange! ISTUP=0 means SATR is present for IRUN!
CC      IF(ISTUP.NE.0) RETURN
CC      IF (.NOT. FESADS) RETURN
C
C      IF ((TPROJ(2:3).EQ.'YX').AND.(TUNIT.EQ.'ENDCAP')) THEN
C        FI = 270.
C        FIINC = 360./ NPTS
C        DO I = 1, NPTS/2+1
C          C = COSD(FI)
C          S = SIND(FI)
C          HR(I)        = RINNDS*C
C          VR(I)        = RINNDS*S
C          HR(NPTS-I+3) = ROUTDS*C
C          VR(NPTS-I+3) = ROUTDS*S
C          HL(I)        = -HR(I)
C          VL(I)        =  VR(I)
C          HL(NPTS-I+3) = -HR(NPTS-I+3)
C          VL(NPTS-I+3) =  VR(NPTS-I+3)
C          FI = FI + FIINC
C        END DO
C        HR(NPTS+3) = HR(1)
C        VR(NPTS+3) = VR(1)
C        HL(NPTS+3) = HL(1)
C        VL(NPTS+3) = VL(1)
C        CALL DQPOL( NPTS+3, HR, VR )
C        CALL DQPOL( NPTS+3, HL, VL )
C      ELSE IF (TPROJ(2:3).EQ.'RZ') THEN
C        HR(1) = ZDISDS-ZLENDS
C        HR(2) = ZDISDS
C        HR(3) = ZDISDS
C        HR(4) = ZDISDS-ZLENDS
C        HR(5) = HR(1)
C        VR(1) = RINNDS
C        VR(2) = RINNDS
C        VR(3) = ROUTDS
C        VR(4) = ROUTDS
C        VR(5) = VR(1)
C        DO I=1 , 5
C          HL(I) = -HR(I)
C          VL(I) =  VR(I)
C        END DO
C        CALL DQPOL(5,HR,VR)
C        CALL DQPOL(5,HL,VL)
C        IF (TPROJ(1:1).EQ.'S') THEN
C          DO I=1 , 5
C            VR(I) = -VR(I)
C            VL(I) = -VL(I)
C          END DO
C          CALL DQPOL(5,HR,VR)
C          CALL DQPOL(5,HL,VL)
C        END IF
C      END IF
C      END
CC
CC
*DK DGDSIC
      SUBROUTINE DGDSIC (TPROJ,TUNIT)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- R.Vogl                26-NOV-1991
C!       modified 18/05/92, R.Vogl, Proj 'ZRZ'
C!
C!   Inputs: TPROJ : projection to be drawn
C!        -
C!
C!   Outputs:
C!        -
C!
C!   Libraries required:
C!
C!   Description draw the SICAL geometry
C!   ===========
C!
C?
C!======================================================================

C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DE
C     ++++++++++++++++++++++++++++++ used in ATLANTIS , NRECDE not used here
      COMMON /DEVTIC/NFILDE,IRUNDE(2),IEVTDE(2),LNINDE(2),LCLSDE,NRECDE
      COMMON /DEVTIT/TFINDE(2)
      CHARACTER *80 TFINDE
C------------------------------------------------------------------- DS
      COMMON /DSIGEO/ ZAMIDS,ZAMADS,RAMIDS,RAMADS,
     &                ZBMIDS,ZBMADS,RBMIDS,RBMADS,FESIDS
      LOGICAL FESIDS
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *3 TPROJ
      CHARACTER *6 TUNIT
      INTEGER GTSTUP

      DATA GAP2 / 0.1 /
      PARAMETER ( NPTS=36)
      DIMENSION HR(NPTS+3),VR(NPTS+3),HL(NPTS+3),VL(NPTS+3)
C  IF SICAL GEOMETRY COULD NOT BE READ FOR GIVEN RUN: DON'T DRAW IT!
      IRUN = IRUNDE(1)
      ISTUP = GTSTUP('SI',IRUN)
C      IF(ISTUP.EQ.0) RETURN
C      IT (.NOT. FESIDS) RETURN

      IF ((TPROJ(2:3).EQ.'YX').AND.(TUNIT.EQ.'ENDCAP')) THEN
        FI = 270.
        FIINC = 360./ NPTS
        DO I = 1, NPTS/2+1
          C = COSD(FI)
          S = SIND(FI)
          HR(I)        = RAMIDS*C
          VR(I)        = RAMIDS*S
          HR(NPTS-I+3) = RAMADS*C
          VR(NPTS-I+3) = RAMADS*S
          HL(I)        = -HR(I)
          VL(I)        =  VR(I)
          HL(NPTS-I+3) = -HR(NPTS-I+3)
          VL(NPTS-I+3) =  VR(NPTS-I+3)
          FI = FI + FIINC
        END DO
        HR(NPTS+3) = HR(1)
        VR(NPTS+3) = VR(1)
        HL(NPTS+3) = HL(1)
        VL(NPTS+3) = VL(1)
        CALL DQPOL( NPTS+3, HR, VR )
        CALL DQPOL( NPTS+3, HL, VL )
      ELSE IF (TPROJ.EQ.'ZRZ') THEN

        DELZA = ZAMADS - ZAMIDS

        HR(1) = GAP2
        HR(2) = GAP2
        HR(3) = GAP2 + DELZA
        HR(4) = GAP2 + DELZA
        HR(5) = HR(1)
        VR(1) = RAMIDS
        VR(2) = RAMADS
        VR(3) = RAMADS
        VR(4) = RAMIDS
        VR(5) = VR(1)

        DELZB = ZBMADS - ZBMIDS

        HL(1) = - GAP2
        HL(2) = - GAP2
        HL(3) = - GAP2 + DELZB
        HL(4) = - GAP2 + DELZB
        HL(5) = HL(1)
        VL(1) = RBMIDS
        VL(2) = RBMADS
        VL(3) = RBMADS
        VL(4) = RBMIDS
        VL(5) = VL(1)

        CALL DQPOL(5,HR,VR)
        CALL DQPOL(5,HL,VL)

      ELSE IF (TPROJ.EQ.'ZFZ') THEN

        DELZA = ZAMADS - ZAMIDS

        HR(1) = GAP2
        HR(2) = GAP2
        HR(3) = GAP2 + DELZA
        HR(4) = GAP2 + DELZA
        HR(5) = HR(1)
        CALL DQINV(IAREDO,0.,VLOWDG(IAREDO),HDUM,VR(1))
        CALL DQINV(IAREDO,0.,VHGHDG(IAREDO),HDUM,VR(3))
        VR(2)=VR(3)
        VR(4)=VR(1)
C       Code below does not work for small ares because of UIS bug.
C       VR(1) = 0.
C       VR(2) = 720.
C       VR(3) = 720.
C       VR(4) = 0.
        VR(5) = VR(1)

        DELZB = ZBMADS - ZBMIDS

        HL(1) = - GAP2
        HL(2) = - GAP2
        HL(3) = - GAP2 + DELZB
        HL(4) = - GAP2 + DELZB
        HL(5) = HL(1)
C       VL(1) = 0.
C       VL(2) = 720.
C       VL(3) = 720.
C       VL(4) = 0.
C       VL(5) = VL(1)

        CALL DQPOL(5,HR,VR)
        CALL DQPOL(5,HL,VR)

      ELSE IF (TPROJ.EQ.'ZFR') THEN

        DELR = RAMADS - RAMIDS

        HR(1) = GAP2
        HR(2) = GAP2
        HR(3) = GAP2 + DELR
        HR(4) = GAP2 + DELR
        HR(5) = HR(1)
        CALL DQINV(IAREDO,0.,VLOWDG(IAREDO),HDUM,VR(1))
        CALL DQINV(IAREDO,0.,VHGHDG(IAREDO),HDUM,VR(3))
        VR(2)=VR(3)
        VR(4)=VR(1)
C       Code below does not work for small ares because of UIS bug.
C       VR(1) = 0.
C       VR(2) = 720.
C       VR(3) = 720.
C       VR(4) = 0.
        VR(5) = VR(1)

        HL(1) = - GAP2
        HL(2) = - GAP2
        HL(3) = - GAP2 - DELR
        HL(4) = - GAP2 - DELR
        HL(5) = HL(1)
C       VL(1) = 0.
C       VL(2) = 720.
C       VL(3) = 720.
C       VL(4) = 0.
C       VL(5) = VL(1)

        CALL DQPOL(5,HR,VR)
        CALL DQPOL(5,HL,VR)

      ELSE IF (TPROJ(2:3).EQ.'RZ') THEN

        HR(1) = ZAMIDS
        HR(2) = ZAMADS
        HR(3) = ZAMADS
        HR(4) = ZAMIDS
        HR(5) = HR(1)
        VR(1) = RAMIDS
        VR(2) = RAMIDS
        VR(3) = RAMADS
        VR(4) = RAMADS
        VR(5) = VR(1)

        HL(1) = ZBMIDS
        HL(2) = ZBMADS
        HL(3) = ZBMADS
        HL(4) = ZBMIDS
        HL(5) = HL(1)
        VL(1) = RBMIDS
        VL(2) = RBMIDS
        VL(3) = RBMADS
        VL(4) = RBMADS
        VL(5) = VL(1)

        CALL DQPOL(5,HR,VR)
        CALL DQPOL(5,HL,VL)
        IF (TPROJ(1:1).EQ.'S') THEN
          DO I=1 , 5
            VR(I) = -VR(I)
            VL(I) = -VL(I)
          END DO
          CALL DQPOL(5,HR,VR)
          CALL DQPOL(5,HL,VL)
        END IF
      END IF
      END
C
C


C
C
*DK DGDECA
      SUBROUTINE DGDECA(TPROJ,TDRMD,TUNIT,RNP,RR)
C
C-----------------------------------------
C
C   Author   :- R.Vogl                  25-AUG-1989
C
C=========================================
C
C   Purpose   : draw ECAL geometry in different projections
C   Inputs    : TPROJ : the projection to be draw
C               TDRMD : mode of TF-projection
C               RNP   : number of points to be interpolated in FR-proj.
C               RR    : perspective parameters
C   Outputs   : graphic display of ECAL geometry
C
C   called by : DALI
C=========================================
C +

C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DE
      PARAMETER (NEBAM=12,NEECM=12)
      COMMON /DECGEO/ CORADE(3,12,NEECM),CORBDE(3,8,NEBAM),
     &                CORCDE(3,12,NEECM),ETILDE ,
     &                POLADE(2,12,NEECM),POLBDE(2,8,NEBAM),
     &                POLCDE(2,12,NEECM),
     &                RMNBDE ,RMXBDE ,RICBDE ,ROCBDE,
     &                RMNEDE ,RMXEDE ,RICEDE ,RMCEDE ,ROCEDE
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *3 TPROJ
      CHARACTER *6 TDRMD
      CHARACTER *6 TUNIT
      DIMENSION RR(3)

      DIMENSION H(49),V(49),H1(10),V1(10),H2(10),V2(10),H3(10),V3(10)
      DIMENSION VE1(5),VE2(5),VE3(5),HW(4),VW(4)
      DATA DSEC/10./
      DIMENSION TBXI(12),TBYI(12)
      DATA N3/3/,QSHAD/0.41/
C     ...................................................... QSHAD=238/538
C
C
C  statement-functions
C
C      HPER(X,Y,R) = X*(1+E*R2)/(1+E*R)
C      VPER(X,Y,R) = Y*(1+E*R2)/(1+E*R)
C
C      R1 = RR(1)
C      R2 = RR(2)
C      R1P= RR(3)

C
C  # of points to be drawn on lines between points 1 and 3 ( resp. 5
C  and 7 ) in FR projection. is passed as a REAL argument and has to
C  be converted first. max is 20 interpolated points. RNP is later
C  to be included in a common-block
C
      NPR = IDMIX(0,IFIX(RNP),20)
C
C  for detailed information on the way the geometry data for the ECAL
C  is represented see  ECAL GEOMETRY PACKAGE (ALEPH 88-177) by M.Rumpf
C  and H.Videau
C

      IF (TPROJ(2:3).EQ.'YX') THEN
        IF (TUNIT.EQ.'BARREL' .OR. TUNIT.EQ.'MIDANG' .OR.
     &      TUNIT.EQ.'BAR+MA') THEN
          DO MD = 1 , NEBAM
            H(1) = CORBDE(1 ,1 , MD )
            V(1) = CORBDE(2 ,1 , MD )
            H(2) = CORBDE(1 ,3 , MD )
            V(2) = CORBDE(2 ,3 , MD )
            H(3) = CORBDE(1 ,7 , MD )
            V(3) = CORBDE(2 ,7 , MD )
            H(4) = CORBDE(1 ,5 , MD )
            V(4) = CORBDE(2 ,5 , MD )
            H(5) = CORBDE(1 ,1 , MD )
            V(5) = CORBDE(2 ,1 , MD )
C
C  corner# for XYview : outer corners : 5,7 (6,8)
C                       inner corners : 1,3 (2,4)
C  draw the quadrangle of the ECALbarrel, points ordered for polygon-line
C
            IF(TPROJ(1:1).EQ.' ') THEN
              CALL DQPOL(5,H,V)
            ELSE
              CALL DQSHA(QSHAD,2,H(3),V(3),'O')
              CALL DQSHA(QSHAD,2,H(1),V(1),'I')
            END IF
          END DO
        ELSE IF (TUNIT.EQ.'ENDCAP') THEN
          DO MD = 1 , NEECM
            H(1) = CORADE(1,1,MD)
            V(1) = CORADE(2,1,MD)
            H(2) = CORADE(1,3,MD)
            V(2) = CORADE(2,3,MD)
            H(3) = CORADE(1,7,MD)
            V(3) = CORADE(2,7,MD)
            H(4) = CORADE(1,11,MD)
            V(4) = CORADE(2,11,MD)
            H(5) = CORADE(1,9,MD)
            V(5) = CORADE(2,9,MD)
            H(6) = CORADE(1,5,MD)
            V(6) = CORADE(2,5,MD)
            H(7) = CORADE(1,1,MD)
            V(7) = CORADE(2,1,MD)
C
C   corner# for XYview : outer corners : 7,11,9,5 (8,12,10,6)
C                        inner corners : 1,3 (1,4)
C   draw the 6-cornered endcap modules
C
            CALL DQPOL(7,H,V)
          END DO
        END IF
      ELSE IF (TPROJ(2:3).EQ.'PP') THEN

C        E = (R1P - R1)/(R1*(R2-R1P))

          DO MD = 1 , NEBAM

            DHH  = CORBDE(1,3,MD) - CORBDE(1,1,MD)
            DVV  = CORBDE(2,3,MD) - CORBDE(2,1,MD)

            H(1)  = CORBDE(1 ,1 , MD )
            V(1)  = CORBDE(2 ,1 , MD )

            DO K=1, NPR
              H(1+K) = CORBDE(1,1,MD) + K*DHH/(NPR+1)
              V(1+K) = CORBDE(2,1,MD) + K*DVV/(NPR+1)
            END DO

            H(2+NPR) = CORBDE(1 ,3 , MD )
            V(2+NPR) = CORBDE(2 ,3 , MD )
            H(3+NPR) = CORBDE(1 ,7 , MD )
            V(3+NPR) = CORBDE(2 ,7 , MD )

            DHH  = CORBDE(1,5,MD) - CORBDE(1,7,MD)
            DVV  = CORBDE(2,5,MD) - CORBDE(2,7,MD)

            DO K=1, NPR
              H(3+NPR+K) = CORBDE(1,7,MD) + K*DHH/(NPR+1)
              V(3+NPR+K) = CORBDE(2,7,MD) + K*DVV/(NPR+1)
            END DO


            H(4+NPR+NPR) = CORBDE(1 ,5 , MD )
            V(4+NPR+NPR) = CORBDE(2 ,5 , MD )
            H(5+NPR+NPR) = H(1)
            V(5+NPR+NPR) = V(1)
C
C  corner# for XYview : outer corners : 5,7 (6,8)
C                       inner corners : 1,3 (2,4)
C  draw the quadrangle of the ECALbarrel, points ordered for polygon-line
C
            IF(TPROJ(1:1).EQ.' ') THEN
              CALL DQPOL(5+NPR+NPR,H,V)
            ELSE
              CALL DQSHA(QSHAD,2+NPR,H(N3+NPR),V(N3+NPR),'O')
              CALL DQSHA(QSHAD,2+NPR,H(1     ),V(1     ),'I')
            END IF
          END DO

      ELSE IF (TPROJ(2:3).EQ.'YZ') THEN
C
C  module 4 (corners 11 and 12 ) has maximum Y coordinates,
C  module 10(corners 11 and 12 ) has minimum Y coordinates
C
        H(1) = CORCDE(3,1,1)
        H(2) = CORCDE(3,2,1)
        H(3) = CORCDE(3,2,1)
        H(4) = CORCDE(3,1,1)
        H(5) = H(1)
        V(1) = -ROCEDE
        V(2) = -ROCEDE
        V(3) =  ROCEDE
        V(4) =  ROCEDE
        V(5) =  V(1)
        CALL DQPOL(5,H,V)
        H(1) = CORADE(3,1,1)
        H(2) = CORADE(3,2,1)
        H(3) = CORADE(3,2,1)
        H(4) = CORADE(3,1,1)
        H(5) = H(1)
        V(1) = -ROCEDE
        V(2) = -ROCEDE
        V(3) =  ROCEDE
        V(4) =  ROCEDE
        V(5) =  V(1)
        CALL DQPOL(5,H,V)

      ELSE IF (TPROJ(2:3).EQ.'RZ') THEN

C       ..................................................... BARREL
        H1(1) = CORBDE(3,1,1)
        H1(2) = CORBDE(3,2,1)
        H1(3) = CORBDE(3,2,1)
        H1(4) = CORBDE(3,1,1)
        H1(5) = H1(1)
        V1(1) = RMNBDE
        V1(2) = RMNBDE
        V1(3) = ROCBDE
        V1(4) = ROCBDE
        V1(5) = V1(1)

C       ..................................................... END CAP Z>0
        H2(1) = CORADE(3,1,1)
        H2(2) = CORADE(3,2,1)
        H2(3) = CORADE(3,2,1)
        H2(4) = CORADE(3,1,1)
        H2(5) = H2(1)
        V2(1) = RMNEDE
        V2(2) = RMNEDE
        V2(3) = ROCEDE
        V2(4) = ROCEDE
        V2(5) = V2(1)

C       ..................................................... END CAP Z<0
        H3(1) = CORCDE(3,1,1)
        H3(2) = CORCDE(3,2,1)
        H3(3) = CORCDE(3,2,1)
        H3(4) = CORCDE(3,1,1)
        H3(5) = H3(1)
        V3(1) = RMNEDE
        V3(2) = RMNEDE
        V3(3) = ROCEDE
        V3(4) = ROCEDE
        V3(5) = V3(1)

        CALL DQPOL( 5, H1, V1 )
        CALL DQPOL( 5, H2, V2 )
        CALL DQPOL( 5, H3, V3 )

        IF (TPROJ(1:1).EQ.'S') THEN
          DO I=1,5
            VE1(I) = -V1(I)
            VE2(I) = -V2(I)
            VE3(I) = -V3(I)
          END DO
          CALL DQPOL( 5, H1, VE1 )
          CALL DQPOL( 5, H2, VE2 )
          CALL DQPOL( 5, H3, VE3 )
        END IF

      ELSE IF (TPROJ(2:3).EQ.'FR') THEN

C
C  draw sensitive barrel module parts in phi vs. rho projection
C  rho ranges from Radius_of_INner_Corner to Radius_of_OUTer_Corner
C
C  path for drawing : 7->3->{interpolated points}->1->5->{i.p.}->7
C
C
C  draw lager range of phi due to possible shifts for track-viewing
C  always cipped to the current window-setting
C

        DX13 = CORBDE(1,1,1) - CORBDE(1,3,1)
        DY13 = CORBDE(2,1,1) - CORBDE(2,3,1)
        DX75 = CORBDE(1,7,1) - CORBDE(1,5,1)
        DY75 = CORBDE(2,7,1) - CORBDE(2,5,1)

        DO I = 1 , NPR
          ZPX = CORBDE(1,3,1) + ( I * DX13 / (NPR+1))
          ZPY = CORBDE(2,3,1) + ( I * DY13 / (NPR+1))
          H(2+I) = SQRT( ZPX**2 + ZPY**2 )
          V(2+I) = DATN2D( ZPY , ZPX )
          V(2+I) = DFINXT(POLBDE(1,3,1),V(2+I))
        END DO
        DO J = 1 , NPR
          ZPX = CORBDE(1,5,1) + ( J * DX75 / (NPR+1))
          ZPY = CORBDE(2,5,1) + ( J * DY75 / (NPR+1))
          H(4+NPR+J) = SQRT( ZPX**2 + ZPY**2 )
          V(4+NPR+J) = DATN2D( ZPY , ZPX )
          V(4+NPR+J) = DFINXT(POLBDE(1,3,1),V(4+NPR+J))
        END DO

        FIOFF = 360. / NEBAM

        DO MD = 1 , NEBAM

          H(1) = ROCBDE
          V(1) = POLBDE(1,7,MD)
          H(2) = RICBDE
          V(2) = POLBDE(1,3,MD)

          H(3+NPR) = RICBDE
          V(3+NPR) = POLBDE(1,1,MD)
          H(4+NPR) = ROCBDE
          V(4+NPR) = POLBDE(1,5,MD)

          H(5+NPR+NPR) = H(1)
          V(5+NPR+NPR) = V(1)

          CALL DQPOL(5+NPR+NPR,H,V)
C
C  draw module shifted by +360 deg. in phi
C
          DO II=1 , 5+NPR+NPR
            V(II) = V(II)+360.
          END DO
          CALL DQPOL(5+NPR+NPR,H,V)
C
C  draw module shifted by -360 deg. in phi
C
          DO II=1 , 5+NPR+NPR
            V(II) = V(II)-720.
          END DO
          CALL DQPOL(5+NPR+NPR,H,V)

          DO I=1 , NPR
            V(2+I)     = 360. + V(2+I) + FIOFF
            V(4+NPR+I) = 360. + V(4+NPR+I) + FIOFF
          END DO
        END DO
      ELSE IF (TPROJ(2:3).EQ.'FZ') THEN

C
C  corners 5 and 7 have largest phi ! -> therefore they are used in this
C  projection ( 6 and 8 correspond to 5 and 7 !)
C

C
C  draw ECAL-endcap  z>0
C  path of drawing : 8 -> 6 -> 5 -> 7 -> 8
C
        DO MD = 1 , NEECM
          H(1) = CORADE(3,8,MD)
          H(2) = CORADE(3,6,MD)
          H(3) = CORADE(3,5,MD)
          H(4) = CORADE(3,7,MD)
          H(5) = H(1)

          V(1) = POLADE(1,8,MD)
          V(2) = POLADE(1,6,MD)
          V(3) = POLADE(1,5,MD)
          V(4) = POLADE(1,7,MD)
          V(5) = V(1)

          CALL DQPOL(5,H,V)
C
C  draw module shifted by +360 deg. in phi
C
          DO I=1 , 5
            V(I) = V(I)+360.
          END DO
          CALL DQPOL(5,H,V)
C
C  draw module shifted by -360 deg. in phi
C
          DO I=1 , 5
            V(I) = V(I)-720.
          END DO
          CALL DQPOL(5,H,V)
        END DO
C
C  draw ECAL-endcap  z<0
C  path of drawing : 5 -> 7 -> 8 -> 6 -> 5
C
        DO MD = 1 , NEECM
          H(1) = CORCDE(3,5,MD)
          H(2) = CORCDE(3,7,MD)
          H(3) = CORCDE(3,8,MD)
          H(4) = CORCDE(3,6,MD)
          H(5) = H(1)

          V(1) = POLCDE(1,5,MD)
          V(2) = POLCDE(1,7,MD)
          V(3) = POLCDE(1,8,MD)
          V(4) = POLCDE(1,6,MD)
          V(5) = V(1)

          CALL DQPOL(5,H,V)
C
C  draw module shifted by +360 deg. in phi
C
          DO I=1 , 5
            V(I) = V(I)+360.
          END DO
          CALL DQPOL(5,H,V)
C
C  draw module shifted by -360 deg. in phi
C
          DO I=1 , 5
            V(I) = V(I)-720.
          END DO
          CALL DQPOL(5,H,V)
        END DO

      ELSE IF (TPROJ(2:3).EQ.'FT') THEN
C       draw either detailed or simple picture of ECAL in phi/theta projection
        IF (TDRMD.EQ.'DETAIL') THEN
C         ....................................................... BARREL first
C         ... inner corners of barrel modules have larger theta-range than the
C         .......................... outer corners and are therefore preferred
C         corners of inner plane are:1,2,4,3 in ECAL geometry package notation
C         ............................ remember :  theta = atan( rho/z )  !!!!
C         ........................................ phi   = atan( y/x   )  !!!!
C         .......................... path for drawing :  3 -> 4 -> 2 -> 1 -> 3
          DO MD = 1 , NEBAM
            H(1) = POLBDE(2,3,MD)
            H(2) = POLBDE(2,4,MD)
            H(3) = POLBDE(2,2,MD)
            H(4) = POLBDE(2,1,MD)
            H(5) = H(1)
            V(1) = POLBDE(1,3,MD)
            V(2) = POLBDE(1,4,MD)
            V(3) = POLBDE(1,2,MD)
            V(4) = POLBDE(1,1,MD)
            V(5) = V(1)
            CALL DQPOL(5,H,V)
C           .......................... draw module shifted by +360 deg. in phi
            DO I=1 , 5
              V(I) = V(I)+360.
            END DO
            CALL DQPOL(5,H,V)
C           .......................... draw module shifted by -360 deg. in phi
            DO I=1 , 5
              V(I) = V(I)-720.
            END DO
            CALL DQPOL(5,H,V)
          END DO
C         and now for the ENDCAPs : 8 points on the endcaps (1,3,7,8,12,10,6,5)
C         ..are (in the above order) connected to a polygon which gives in the
C         ..phi/theta projection the largest solid angle of the endcap modules
C         ......................................................... ENDCAP z>0
C         ..... path for drawing : 3 -> 7 -> 8 -> 12 -> 10 -> 6 -> 5 -> 1 -> 3
          DO MD = 1 , NEECM
            H(1) = POLADE(2,3 ,MD)
            H(2) = POLADE(2,7 ,MD)
            H(3) = POLADE(2,8 ,MD)
            H(4) = POLADE(2,12,MD)
            H(5) = POLADE(2,10,MD)
            H(6) = POLADE(2,6 ,MD)
            H(7) = POLADE(2,5 ,MD)
            H(8) = POLADE(2,1 ,MD)
            H(9) = H(1)
            V(1) = POLADE(1,3 ,MD)
            V(2) = POLADE(1,7 ,MD)
            V(3) = POLADE(1,8 ,MD)
            V(4) = POLADE(1,12,MD)
            V(5) = POLADE(1,10,MD)
            V(6) = POLADE(1,6 ,MD)
            V(7) = POLADE(1,5 ,MD)
            V(8) = POLADE(1,1 ,MD)
            V(9) = V(1)
            CALL DQPOL(9,H,V)
C           .......................... draw module shifted by +360 deg. in phi
            DO I=1 , 9
              V(I) = V(I)+360.
            END DO
            CALL DQPOL(9,H,V)
C           .......................... draw module shifted by -360 deg. in phi
            DO I=1 , 9
              V(I) = V(I)-720.
            END DO
            CALL DQPOL(9,H,V)
          END DO
C         ......................................................... ENDCAP z<0
C         ..... path for drawing : 1 -> 3 -> 7 -> 8 -> 12 -> 10 -> 6 -> 5 -> 1
          DO MD = 1 , NEECM
            H(1) = POLCDE(2,1 ,MD)
            H(2) = POLCDE(2,3 ,MD)
            H(3) = POLCDE(2,7 ,MD)
            H(4) = POLCDE(2,8 ,MD)
            H(5) = POLCDE(2,12,MD)
            H(6) = POLCDE(2,10,MD)
            H(7) = POLCDE(2,6 ,MD)
            H(8) = POLCDE(2,5 ,MD)
            H(9) = H(1)
            V(1) = POLCDE(1,1 ,MD)
            V(2) = POLCDE(1,3 ,MD)
            V(3) = POLCDE(1,7 ,MD)
            V(4) = POLCDE(1,8 ,MD)
            V(5) = POLCDE(1,12,MD)
            V(6) = POLCDE(1,10,MD)
            V(7) = POLCDE(1,6 ,MD)
            V(8) = POLCDE(1,5 ,MD)
            V(9) = V(1)
            CALL DQPOL(9,H,V)
C           .......................... draw module shifted by +360 deg. in phi
            DO I=1 , 9
              V(I) = V(I)+360.
            END DO
            CALL DQPOL(9,H,V)
C           .......................... draw module shifted by -360 deg. in phi
            DO I=1 , 9
              V(I) = V(I)-720.
            END DO
            CALL DQPOL(9,H,V)
          END DO
        ELSE IF (TDRMD.EQ.'SIMPLE') THEN
          H1(1)=POLADE(2,1,1)
          H1(3)=POLCDE(2,1,1)
          V1(1)=-180.
          V1(3)= 540.
          H1(2)=H1(3)
          V1(2)=V1(1)
          H1(4)=H1(1)
          V1(4)=V1(3)
          H1(5)=H1(1)
          V1(5)=V1(1)
          CALL DQPOL( 5, H1, V1 )
C          TMNB  = POLBDE(2,1,1)
C          TMXB  = POLBDE(2,2,1)
C          FIB = ETILDE
C          TMNEA = POLADE(2,1,1)
C          TMXEA = POLADE(2,12,1)
C          TMXEC = POLCDE(2,1,1)
C          TMNEC = POLCDE(2,12,1)
C          FIE = ETILDE - 15.
C          H1(1) = TMNB
C          H1(2) = TMXB
C          H1(3) = TMXB
C          H1(4) = TMNB
C          H1(5) = H1(1)
C          V1(1) = -60.
C          V1(2) = -60.
C          V1(3) =  540.
C          V1(4) =  540.
C          V1(5) = V1(1)
C          H2(1) = TMNEA
C          H2(2) = TMXEA
C          H2(3) = TMXEA
C          H2(4) = TMNEA
C          H2(5) = H2(1)
C          V2(1) = -60.
C          V2(2) = -60.
C          V2(3) =  540.
C          V2(4) =  540.
C          V2(5) = V2(1)
C          H3(1) = TMNEC
C          H3(2) = TMXEC
C          H3(3) = TMXEC
C          H3(4) = TMNEC
C          H3(5) = H3(1)
C          V3(1) = -60.
C          V3(2) = -60.
C          V3(3) =  540.
C          V3(4) =  540.
C          V3(5) = V3(1)
C          CALL DQPOL( 5, H1, V1 )
C          CALL DQPOL( 5, H2, V2 )
C          CALL DQPOL( 5, H3, V3 )
C          DO MD = 1 , 12
C            CALL DQL2E ( TMXB ,FIB,TMNB ,FIB)
C            CALL DQL2E ( TMXEA,FIE,TMNEA,FIE)
C            CALL DQL2E ( TMXEC,FIE,TMNEC,FIE)
C            CALL DQL2E ( TMXB ,FIB-360.,TMNB ,FIB-360.)
C            CALL DQL2E ( TMXEA,FIE-360.,TMNEA,FIE-360.)
C            CALL DQL2E ( TMXEC,FIE-360.,TMNEC,FIE-360.)
C            CALL DQL2E ( TMXB ,FIB+360.,TMNB ,FIB+360.)
C            CALL DQL2E ( TMXEA,FIE+360.,TMNEA,FIE+360.)
C            CALL DQL2E ( TMXEC,FIE+360.,TMNEC,FIE+360.)
C            FIB = FIB + 30.
C            FIE = FIE + 30.
C          END DO
        END IF
      END IF
      RETURN
C------------------------------------------------------------------------
C
C
      ENTRY DGTECA( TBXI,TBYI,Q12,ZINN,ZOUT)
C
C
C       written by R.Vogl                       2.11.1989
C
C------------------------------------------------------------------------
      ZINN = MIN( CORADE(3,1,1),CORADE(3,2,1))
      ZOUT = MAX( CORADE(3,1,1),CORADE(3,2,1))
      Q12 = ROCBDE/RICBDE
      FI = ETILDE
      DO I=1 , 12
        TBXI(I) = RICBDE*COSD(FI)
        TBYI(I) = RICBDE*SIND(FI)
        FI = FI + 30.
      END DO
      RETURN
C------------------------------------------------------------------------
C
C
      ENTRY DGDERZ(AT4)
C
C
C       written by H.DREVERMANN                       11.10.1993
C
C   AT4=PARADO(4,IPATDO) = SIGNED OR POSITIVE RHO
C
C   In order not to clip the areas of the ecal hits, the background
C   at the critical places close to the ecal is redrawn as well as the frame.
C------------------------------------------------------------------------
      LCAR=PDCODD(2,ICBGDD)
      CALL DQPO0('AREA',LCAR,0,' ')
      DO SV=1.,-1.,-2.
        DO SH=1.,-1.,-2.
          HW(1)=SH*H1(1)
          HW(3)=SH*H2(3)
          VW(1)=SV*V1(1)
          VW(3)=SV*V1(4)
          HW(2)=HW(3)
          VW(2)=VW(1)
          HW(4)=HW(1)
          VW(4)=VW(3)
          CALL DQPOL(4,HW,VW)
C         HW(1)=SH*H1(1)
          HW(3)=SH*H2(4)
          VW(1)=SV*V2(3)
          VW(3)=SV*(V2(3)+DSEC)
          HW(2)=HW(3)
          VW(2)=VW(1)
C         HW(4)=HW(1)
          VW(4)=VW(3)
          CALL DQPOL(4,HW,VW)
        END DO
        IF(AT4.EQ.0.) GO TO 832
      END DO
  832 IF(PDCODD(4,KCECDD).GT.0.) THEN
        DLINDD=PDCODD(2,LIDTDD)
        LCLIN=PDCODD(2,KCECDD)
        CALL DQPO0('LINE',0,LCLIN,' ')
        CALL DQPOL( 5, H1, V1 )
        CALL DQPOL( 5, H2, V2 )
        CALL DQPOL( 5, H3, V3 )
        IF(AT4.NE.0.) THEN
          CALL DQPOL( 5, H1, VE1 )
          CALL DQPOL( 5, H2, VE2 )
          CALL DQPOL( 5, H3, VE3 )
        END IF
        DLINDD=PDCODD(2,LITRDD)
      END IF
      END
C
C  end of DGDECA
C



*DK DGDHCA
      SUBROUTINE DGDHCA(TPROJ,TDRMD,TUNIT,RNP,RR)
C
C-----------------------------------------
C
C   Author   :- R.Vogl                  25-AUG-1989
C
C=========================================
C
C   Purpose   : draw HCAL geometry in different projections
C   Inputs    : TPROJ : the projection to be draw
C               TDRMD : mode of TF-projection
C               RNP   : number of points to be interpolated in FR-proj.
C               RR    : perspective parameters
C   Outputs   : graphic display of HCAL geometry
C
C   called by : DALI
C=========================================
C +

C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DH
      PARAMETER (NHBAM=12,NHECM=6)
      COMMON /DHCGEO/ CORADH(3,16,NHECM),CORBDH(3,8,NHBAM),
     &                CORCDH(3,16,NHECM),
     &                POLADH(2,16,NHECM),POLBDH(2,8,NHBAM),
     &                POLCDH(2,16,NHECM),
     &                RMNBDH,RMXBDH,RICBDH,ROCBDH,
     &                RMNEDH,RMXEDH,RMXIDH,RICEDH,RMCEDH,ROCEDH
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *3 TPROJ
      CHARACTER *6 TDRMD
      CHARACTER *6 TUNIT
      DIMENSION RR(3)
      DATA N3/3/

      DIMENSION H(49),V(49),H1(10),V1(10),H2(10),V2(10),H3(10),V3(10)

C
C  statement-functions
C
C      HPER(X,Y,R) = X*(1+E*R2)/(1+E*R)
C      VPER(X,Y,R) = Y*(1+E*R2)/(1+E*R)
C
C      R1 = RR(1)
C      R2 = RR(2)
C      R1P= RR(3)

C
C  # of points to be drawn on lines between points 1 and 3 ( resp. 5
C  and 7 ) in FR projection. is passed as a REAL argument and has to
C  be converted first. max is 20 interpolated points. RNP is later
C  to be included in a common-block
C
      NPR = IDMIX(0,IFIX(RNP),20)
C
C  for detailed information on the way the geometry data for the HCAL
C  is represented see  ECAL GEOMETRY PACKAGE (ALEPH 88-177) by M.Rumpf
C  and H.Videau
C

      IF (TPROJ(2:3).EQ.'YX') THEN
        IF (TUNIT.EQ.'BARREL' .OR. TUNIT.EQ.'MIDANG' .OR.
     &      TUNIT.EQ.'BAR+MA' ) THEN
          DO MD = 1 , NHBAM
            H(1) = CORBDH(1 ,1 , MD )
            V(1) = CORBDH(2 ,1 , MD )
            H(2) = CORBDH(1 ,3 , MD )
            V(2) = CORBDH(2 ,3 , MD )
            H(3) = CORBDH(1 ,7 , MD )
            V(3) = CORBDH(2 ,7 , MD )
            H(4) = CORBDH(1 ,5 , MD )
            V(4) = CORBDH(2 ,5 , MD )
            H(5) = CORBDH(1 ,1 , MD )
            V(5) = CORBDH(2 ,1 , MD )
C
C  corner# for XYview : outer corners : 5,7 (6,8)
C                       inner corners : 1,3 (2,4)
C  draw the quadrangle of the HCALbarrel, points ordered for polygon-line
C
            IF(TPROJ(1:1).EQ.' ') THEN
              CALL DQPOL(5,H,V)
            ELSE
              CALL DQSHA(1.,2,H(3),V(3),'O')
              CALL DQSHA(1.,2,H(1),V(1),'I')
            END IF
            IF(TDRMD.EQ.'DETAIL') CALL DGDHYX(H,V)
          END DO
        ELSE IF (TUNIT.EQ.'ENDCAP') THEN
          FIINC = (360./NHECM)/(NPR+1)
          FI = 0.
          DO MD = 1 , NHECM
            FI = FI + FIINC
            H(1) = CORADH(1,1,MD)
            V(1) = CORADH(2,1,MD)
            DO I=1,NPR
              H(1+I) = RMNEDH * COSD(FI)
              V(1+I) = RMNEDH * SIND(FI)
              FI = FI + FIINC
            END DO
            H(2+NPR) = CORADH(1,3,MD)
            V(2+NPR) = CORADH(2,3,MD)
            H(3+NPR) = CORADH(1,7,MD)
            V(3+NPR) = CORADH(2,7,MD)
            H(4+NPR) = CORADH(1,11,MD)
            V(4+NPR) = CORADH(2,11,MD)
            H(5+NPR) = CORADH(1,9,MD)
            V(5+NPR) = CORADH(2,9,MD)
            H(6+NPR) = CORADH(1,5,MD)
            V(6+NPR) = CORADH(2,5,MD)
            H(7+NPR) = CORADH(1,1,MD)
            V(7+NPR) = CORADH(2,1,MD)
C
C   corner# for XYview : outer corners : 7,11,9,5 (8,12,10,6)
C                        inner corners : 1,3 (1,4)
C   draw the 6-cornered endcap modules
C
            CALL DQPOL(7+NPR,H,V)
          END DO
        END IF
      ELSE IF (TPROJ(2:3).EQ.'PP') THEN

C        E = (R1P - R1)/(R1*(R2-R1P))

          DO MD = 1 , NHBAM

            DHH  = CORBDH(1,3,MD) - CORBDH(1,1,MD)
            DVV  = CORBDH(2,3,MD) - CORBDH(2,1,MD)

            H(1) = CORBDH(1 ,1 , MD )
            V(1) = CORBDH(2 ,1 , MD )

            DO K=1, NPR
              H(1+K) = CORBDH(1,1,MD) + K*DHH/(NPR+1)
              V(1+K) = CORBDH(2,1,MD) + K*DVV/(NPR+1)
            END DO

            H(2+NPR) = CORBDH(1 ,3 , MD )
            V(2+NPR) = CORBDH(2 ,3 , MD )
            H(3+NPR) = CORBDH(1 ,7 , MD )
            V(3+NPR) = CORBDH(2 ,7 , MD )

            DHH  = CORBDH(1,5,MD) - CORBDH(1,7,MD)
            DVV  = CORBDH(2,5,MD) - CORBDH(2,7,MD)

            DO K=1, NPR
              H(3+NPR+K) = CORBDH(1,7,MD) + K*DHH/(NPR+1)
              V(3+NPR+K) = CORBDH(2,7,MD) + K*DVV/(NPR+1)
            END DO

            H(4+NPR+NPR) = CORBDH(1 ,5 , MD )
            V(4+NPR+NPR) = CORBDH(2 ,5 , MD )
            H(5+NPR+NPR) = H(1)
            V(5+NPR+NPR) = V(1)
C
C  corner# for XYview : outer corners : 5,7 (6,8)
C                       inner corners : 1,3 (2,4)
C  draw the quadrangle of the HCALbarrel, points ordered for polygon-line
C
            IF(TPROJ(1:1).EQ.' ') THEN
              CALL DQPOL(5+NPR+NPR,H,V)
            ELSE
              CALL DQSHA(1.,2+NPR,H(N3+NPR),V(N3+NPR),'O')
              CALL DQSHA(1.,2+NPR,H(1     ),V(1     ),'I')
            END IF
C           IF(TDRMD.EQ.'DETAIL') CALL DGDHYX(H,V)
          END DO

      ELSE IF (TPROJ.EQ.'BYZ') THEN

        H1(1) = CORBDH(3,2,1)
        H1(2) = CORBDH(3,1,1)
        H1(3) = CORBDH(3,1,1)
        H1(4) = CORBDH(3,2,1)
        H1(5) = H1(1)
        V1(1) = -RMXBDH
        V1(2) = -RMXBDH
        V1(3) =  RMXBDH
        V1(4) =  RMXBDH
        V1(5) = V1(1)
        CALL DQPOL(5,H1,V1)

      ELSE IF (TPROJ(2:3).EQ.'YZ') THEN
C
C  module 2 (corners 11 and 12 ) has maximum Y coordinates,
C  module 5 (corners 11 and 12 ) has minimum Y coordinates
C

        H(1) = CORCDH(3,11,2)
        H(2) = CORCDH(3,12,2)
        H(3) = CORCDH(3,15,2)
        H(4) = CORCDH(3,16,2)
        H(5) = CORCDH(3,16,2)
        H(6) = CORCDH(3,15,2)
        H(7) = CORCDH(3,12,2)
        H(8) = CORCDH(3,11,2)
        H(9) = H(1)
        V(1) =  ROCEDH
        V(2) =  ROCEDH
        V(3) =  RMXIDH
        V(4) =  RMXIDH
        V(5) = -RMXIDH
        V(6) = -RMXIDH
        V(7) = -ROCEDH
        V(8) = -ROCEDH
        V(9) =  V(1)
        CALL DQPOL(9,H,V)

        DO I=1,9
          H(I) = -H(I)
        END DO
        CALL DQPOL(9,H,V)

      ELSE IF (TPROJ(2:3).EQ.'RZ') THEN

        H1(1) = CORBDH(3,2,1)
        H1(2) = CORBDH(3,1,1)
        H1(3) = CORBDH(3,1,1)
        H1(4) = CORBDH(3,2,1)
        H1(5) = H1(1)
        V1(1) = RMNBDH
        V1(2) = RMNBDH
        V1(3) = RMXBDH
        V1(4) = RMXBDH
        V1(5) = V1(1)

        H2(1) = CORADH(3,1 ,1)
        H2(2) = CORADH(3,11,1)
        H2(3) = CORADH(3,12,1)
        H2(4) = CORADH(3,13,1)
        H2(5) = CORADH(3,14,1)
        H2(6) = CORADH(3,2,1)
        H2(7) = H2(1)
        V2(1) = RMNEDH
        V2(2) = RMXEDH
        V2(3) = RMXEDH
        V2(4) = RMXIDH
        V2(5) = RMXIDH
        V2(6) = RMNEDH
        V2(7) = V2(1)

        DO I=1,7
          H3(I) = -H2(I)
          V3(I) =  V2(I)
        END DO

        CALL DQPOL(5,H1,V1)
        CALL DQPOL(7,H2,V2)
        CALL DQPOL(7,H3,V3)

        IF(TDRMD.EQ.'DETAIL') THEN
          CALL DGDHRB(H1,V1)
          CALL DGDHRE(H2,V2)
          CALL DGDHRE(H3,V3)
        END IF

        IF (TPROJ(1:1).EQ.'S') THEN
          DO I=1,7
            V1(I) = - V1(I)
            V2(I) = - V2(I)
            V3(I) = - V3(I)
          END DO
          CALL DQPOL(5,H1,V1)
          CALL DQPOL(7,H2,V2)
          CALL DQPOL(7,H3,V3)

          IF(TDRMD.EQ.'DETAIL') THEN
            CALL DGDHRB(H1,V1)
            CALL DGDHRE(H2,V2)
            CALL DGDHRE(H3,V3)
          END IF

        END IF

      ELSE IF (TPROJ(2:3).EQ.'FR') THEN

C
C  path for drawing : 7->3->{interpolated points}->1->5->{i.p.}->7
C
C
C  draw lager range of phi due to possible shifts for track-viewing
C  always cipped to the current window-setting
C

        DX13 = CORBDH(1,1,1) - CORBDH(1,3,1)
        DY13 = CORBDH(2,1,1) - CORBDH(2,3,1)
        DX75 = CORBDH(1,7,1) - CORBDH(1,5,1)
        DY75 = CORBDH(2,7,1) - CORBDH(2,5,1)

        DO I = 1 , NPR
          ZPX = CORBDH(1,3,1) + ( I * DX13 / (NPR+1))
          ZPY = CORBDH(2,3,1) + ( I * DY13 / (NPR+1))
          H(2+I) = SQRT( ZPX**2 + ZPY**2 )
          V(2+I) = DATN2D( ZPY , ZPX )
          V(2+I) = DFINXT(POLBDH(1,3,1),V(2+I))
        END DO
        DO J = 1 , NPR
          ZPX = CORBDH(1,5,1) + ( J * DX75 / (NPR+1))
          ZPY = CORBDH(2,5,1) + ( J * DY75 / (NPR+1))
          H(4+NPR+J) = SQRT( ZPX**2 + ZPY**2 )
          V(4+NPR+J) = DATN2D( ZPY , ZPX )
          V(4+NPR+J) = DFINXT(POLBDH(1,3,1),V(4+NPR+J))
        END DO

        FIOFF = 360. / NHBAM

        DO MD = 1 , NHBAM

          H(1) = ROCBDH
          V(1) = POLBDH(1,7,MD)
          H(2) = RICBDH
          V(2) = POLBDH(1,3,MD)

          H(3+NPR) = RICBDH
          V(3+NPR) = POLBDH(1,1,MD)
          H(4+NPR) = ROCBDH
          V(4+NPR) = POLBDH(1,5,MD)

          H(5+NPR+NPR) = H(1)
          V(5+NPR+NPR) = V(1)

          CALL DQPOL(5+NPR+NPR,H,V)
C
C  draw module shifted by +360 deg. in phi
C
          DO II=1 , 5+NPR+NPR
            V(II) = V(II)+360.
          END DO
          CALL DQPOL(5+NPR+NPR,H,V)
C
C  draw module shifted by -360 deg. in phi
C
          DO II=1 , 5+NPR+NPR
            V(II) = V(II)-720.
          END DO
          CALL DQPOL(5+NPR+NPR,H,V)

          DO I=1 , NPR
            V(2+I)     = 360. + V(2+I) + FIOFF
            V(4+NPR+I) = 360. + V(4+NPR+I) + FIOFF
          END DO
        END DO

      ELSE IF (TPROJ(2:3).EQ.'FZ') THEN

C
C  corners 5 and 7 have largest phi ! -> therefore they are used in this
C  projection ( 6 and 8 correspond to 5 and 7 !)
C

C
C  draw HCAL-endcap  z>0
C  path of drawing : 8 -> 6 -> 5 -> 7 -> 8
C
        DO MD = 1 , NHECM
          H(1) = CORADH(3,8,MD)
          H(2) = CORADH(3,6,MD)
          H(3) = CORADH(3,5,MD)
          H(4) = CORADH(3,7,MD)
          H(5) = H(1)

          V(1) = POLADH(1,8,MD)
          V(2) = POLADH(1,6,MD)
          V(3) = POLADH(1,5,MD)
          V(4) = POLADH(1,7,MD)
          V(5) = V(1)

          CALL DQPOL(5,H,V)
C
C  draw module shifted by +360 deg. in phi
C
          DO I=1 , 5
            V(I) = V(I)+360.
          END DO
          CALL DQPOL(5,H,V)
C
C  draw module shifted by -360 deg. in phi
C
          DO I=1 , 5
            V(I) = V(I)-720.
          END DO
          CALL DQPOL(5,H,V)
        END DO
C
C  draw HCAL-endcap  z<0
C  path of drawing : 5 -> 7 -> 8 -> 6 -> 5
C
        DO MD = 1 , NHECM
          H(1) = CORCDH(3,5,MD)
          H(2) = CORCDH(3,7,MD)
          H(3) = CORCDH(3,8,MD)
          H(4) = CORCDH(3,6,MD)
          H(5) = H(1)

          V(1) = POLCDH(1,5,MD)
          V(2) = POLCDH(1,7,MD)
          V(3) = POLCDH(1,8,MD)
          V(4) = POLCDH(1,6,MD)
          V(5) = V(1)

          CALL DQPOL(5,H,V)
C
C  draw module shifted by +360 deg. in phi
C
          DO I=1 , 5
            V(I) = V(I)+360.
          END DO
          CALL DQPOL(5,H,V)
C
C  draw module shifted by -360 deg. in phi
C
          DO I=1 , 5
            V(I) = V(I)-720.
          END DO
          CALL DQPOL(5,H,V)
        END DO

      ELSE IF (TPROJ(2:3).EQ.'FT') THEN

C
C  draw either detailed or simple picture of HCAL in phi/theta projection
C
        IF (TDRMD.EQ.'DETAIL') THEN

C
C BARREL first
C
C  inner corners of barrel modules have larger theta-range than the
C  outer corners and are therefore preferred
C  corners of inner plane are : 1,2,4,3 in ECAL geometry package notation
C
C   remember :  theta = atan( rho/z )  !!!!
C
C               phi   = atan( y/x   )  !!!!
C
C  path for drawing :  3 -> 4 -> 2 -> 1 -> 3
C

          DO MD = 1 , NHBAM

            H(1) = POLBDH(2,3,MD)
            H(2) = POLBDH(2,4,MD)
            H(3) = POLBDH(2,2,MD)
            H(4) = POLBDH(2,1,MD)
            H(5) = H(1)

            V(1) = POLBDH(1,3,MD)
            V(2) = POLBDH(1,4,MD)
            V(3) = POLBDH(1,2,MD)
            V(4) = POLBDH(1,1,MD)
            V(5) = V(1)

            CALL DQPOL(5,H,V)
C
C  draw module shifted by +360 deg. in phi
C
            DO I=1 , 5
              V(I) = V(I)+360.
            END DO
            CALL DQPOL(5,H,V)
C
C  draw module shifted by -360 deg. in phi
C
            DO I=1 , 5
              V(I) = V(I)-720.
            END DO
            CALL DQPOL(5,H,V)
          END DO
C
C and now for the ENDCAPs : 8 points on the endcaps (1,3,7,8,12,10,6,5)
C are (in the above order) connected to a polygon which gives in the
C phi/theta projection the largest solid angle of the endcap modules
C

C
C  ENDCAP z>0
C  path for drawing : 3 -> 7 -> 8 -> 12 -> 10 -> 6 -> 5 -> 1 -> 3
C

          DO MD = 1 , NHECM

            H(1) = POLADH(2,3 ,MD)
            H(2) = POLADH(2,7 ,MD)
            H(3) = POLADH(2,8 ,MD)
            H(4) = POLADH(2,12,MD)
            H(5) = POLADH(2,10,MD)
            H(6) = POLADH(2,6 ,MD)
            H(7) = POLADH(2,5 ,MD)
            H(8) = POLADH(2,1 ,MD)
            H(9) = H(1)


            V(1) = POLADH(1,3 ,MD)
            V(2) = POLADH(1,7 ,MD)
            V(3) = POLADH(1,8 ,MD)
            V(4) = POLADH(1,12,MD)
            V(5) = POLADH(1,10,MD)
            V(6) = POLADH(1,6 ,MD)
            V(7) = POLADH(1,5 ,MD)
            V(8) = POLADH(1,1 ,MD)
            V(9) = V(1)

            CALL DQPOL(9,H,V)
C
C  draw module shifted by +360 deg. in phi
C
            DO I=1 , 9
              V(I) = V(I)+360.
            END DO
            CALL DQPOL(9,H,V)
C
C  draw module shifted by -360 deg. in phi
C
            DO I=1 , 9
              V(I) = V(I)-720.
            END DO
            CALL DQPOL(9,H,V)
          END DO

C
C  ENDCAP z<0
C  path for drawing : 1 -> 3 -> 7 -> 8 -> 12 -> 10 -> 6 -> 5 -> 1
C

          DO MD = 1 , NHECM

            H(1) = POLCDH(2,1 ,MD)
            H(2) = POLCDH(2,3 ,MD)
            H(3) = POLCDH(2,7 ,MD)
            H(4) = POLCDH(2,8 ,MD)
            H(5) = POLCDH(2,12,MD)
            H(6) = POLCDH(2,10,MD)
            H(7) = POLCDH(2,6 ,MD)
            H(8) = POLCDH(2,5 ,MD)
            H(9) = H(1)

            V(1) = POLCDH(1,1 ,MD)
            V(2) = POLCDH(1,3 ,MD)
            V(3) = POLCDH(1,7 ,MD)
            V(4) = POLCDH(1,8 ,MD)
            V(5) = POLCDH(1,12,MD)
            V(6) = POLCDH(1,10,MD)
            V(7) = POLCDH(1,6 ,MD)
            V(8) = POLCDH(1,5 ,MD)
            V(9) = V(1)

            CALL DQPOL(9,H,V)
C
C  draw module shifted by +360 deg. in phi
C
            DO I=1 , 9
              V(I) = V(I)+360.
            END DO
            CALL DQPOL(9,H,V)
C
C  draw module shifted by -360 deg. in phi
C
            DO I=1 , 9
              V(I) = V(I)-720.
            END DO
            CALL DQPOL(9,H,V)
          END DO

        ELSE IF (TDRMD.EQ.'SIMPLE') THEN

          TMNB  = POLBDH(2,1,1)
          TMXB  = POLBDH(2,2,1)

          TMNEA = POLADH(2,1,1)
          TMXEA = POLADH(2,12,1)
          TMXEC = POLCDH(2,1,1)
          TMNEC = POLCDH(2,12,1)
          FIE = 0.
          FIB = 15.

          H1(1) = TMNB
          H1(2) = TMXB
          H1(3) = TMXB
          H1(4) = TMNB
          H1(5) = H1(1)
          V1(1) = -60.
          V1(2) = -60.
          V1(3) =  540.
          V1(4) =  540.
          V1(5) = V1(1)

          H2(1) = TMNEA
          H2(2) = TMXEA
          H2(3) = TMXEA
          H2(4) = TMNEA
          H2(5) = H2(1)
          V2(1) = -60.
          V2(2) = -60.
          V2(3) =  540.
          V2(4) =  540.
          V2(5) = V2(1)

          H3(1) = TMNEC
          H3(2) = TMXEC
          H3(3) = TMXEC
          H3(4) = TMNEC
          H3(5) = H3(1)
          V3(1) = -60.
          V3(2) = -60.
          V3(3) =  540.
          V3(4) =  540.
          V3(5) = V3(1)

          CALL DQPOL( 5, H1, V1 )
          CALL DQPOL( 5, H2, V2 )
          CALL DQPOL( 5, H3, V3 )

C          DO MD=1,NHBAM
C            CALL DQL2E ( TMXB ,FIB,TMNB ,FIB)
C            CALL DQL2E ( TMXB ,FIB-360.,TMNB ,FIB-360.)
C            CALL DQL2E ( TMXB ,FIB+360.,TMNB ,FIB+360.)
C            FIB = FIB + 30.
C          END DO
C          DO MD=1,NHECM
C            CALL DQL2E ( TMXEA,FIE,TMNEA,FIE)
C            CALL DQL2E ( TMXEC,FIE,TMNEC,FIE)
C            CALL DQL2E ( TMXEA,FIE-360.,TMNEA,FIE-360.)
C            CALL DQL2E ( TMXEC,FIE-360.,TMNEC,FIE-360.)
C            CALL DQL2E ( TMXEA,FIE+360.,TMNEA,FIE+360.)
C            CALL DQL2E ( TMXEC,FIE+360.,TMNEC,FIE+360.)
C            FIE = FIE + 60.
C          END DO

        END IF

      END IF
      RETURN
      END
C
C  end of DGDHCA
C



*DK DGDINT
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------
CH
      SUBROUTINE DGDINT(IRUN)
C ---------------------------------------------------------------------
C
C    Created by R.F.XU                   20-OCT-1988
C
C!   Modified :- R.Vogl                  19-AUG-1989
C
C!:This is the main control-routine needed to get the ALEPH geometry
C  data from the bos database
C
C    Inputs    :none
C    Outputs   :none
C
C    Called by :DALI
C ---------------------------------------------------------------------
C*************************************************************************
C
C  The former DGCOMN.INC include file for the detector geometry has been
C  added to DALI_CF.INC . All comments have been deleted.
C  They are kept here for documentation.
C  PARAMETER indentifiers have been shortened to 5 characters in order to
C  make IC-treatment possible!
C                 Raimund Vogl  22.07.91
C
C**************************************************************************
C
C     the input variable LDBAS indicating the logical unit of the database
C     to be accessed has to be initialized by the JUNIDB function from
C     ALEPHLIB and is passed on to the geometry read-in subroutines (which
C     require that argument for the calls of ERDDAF and ALGTDB) via the
C     following common block Dali_Graphics_Data_Base_Logical_Unit
C
C      COMMON / DGDBLU / LDBSDG,IRUNDG
C#######################################################################
C
C     This is the common declaration for passing the geometry data
C     from the read-in routines to the drawing rountines
C
C#######################################################################
C
C  VDET
C
C----------------------------------------------------------------------
C
C   IMPORTANT: there is a further include file ( DGCOMN_VRDDAF.INC )
C              that contains the COMMON-blocks needed for accessing
C              the results computed by VRDDAF!
C              Should these commons be changed, the COMMON-DECK
C              VDGEOS has to be extracted from ALEPHLIB anew.
C
C------------------------------------------------------------------------
C
C  VDET-geometry common block Dali_VDet_GEOmetry
C
C  VDET has 2 layers
C
C  CRILDV   corner-coordinates of inner layer ( wafer considered as flat !)
C  CROLDV   corner-coordinates of outer layer
C  NSLIDV   number of slots of inner layer
C  NSLODV   number of slots of outer layer
C  NWAFDV   number of silicon-chip-wafers per slot ( 4 ! )
C  TIILDV   tilts of inner-layer modules ( degree ! )
C  TIOLDV   tilts of outer-layer modules ( degree ! )
C  RAILDV   radius of inner-layer
C  RAOLDV   radius of outer-layer
C  DFILDV   displacement in phi for inner-layer-modules
C  DFOLDV   displacement in phi for outer-layer-modules
C  PALWDV   r-phi active length (=strip-length) of wafer ( type 1)
C  ZALWDV   z active length (=strip-length) of wafer ( type 1)
C  ZOFWDV   z-offset of wafers ( 4 wafer placed in a module )
C  NWTSDV   wafer type in slot ( 0 = not occupied, 1 = Munich, 2-4 = Pisa)
C  RMNIDV   minimum radius of inner-layer active-area wafer corner
C  RMXIDV
C  RMNODV
c  RMXODV
C  DELXDV,DELYDV,DELZDV,DELPDV .. alignment parameters
C
C      COMMON /DVDGEO/ NSLIDV,NSLODV,PALWDV,ZALWDV,
C     &                TIILDV(15),TIOLDV(15),
C     &                RMNIDV,RMXIDV,RMNODV,RMXODV,
C     &                DELXDV,DELYDV,DELZDV,DELPDV,
C     &                ZOFWDV(4), NWTSDV(15,2),
C     &                RAILDV,RAOLDV,DFILDV(12),DFOLDV(15),
C     &                CRILDV(2,2,15),CROLDV(2,2,15)
C#######################################################################
C
C  ITC
C
C----------------------------------------------------------------------
C
C  ITC-geometry common block Dali_ITc_GEOmetry
C
C  RMAXDI   outer radius
C  RMINDI   innner radius
C  ZMAXDI   half of z-length
C
C      COMMON /DITGEO/ RMAXDI,RMINDI,ZMAXDI
C#######################################################################
C
C  ECAL
C
C----------------------------------------------------------------------
C
C  ECAL-geometry common block Dali_ECal_GEOmetry
C
C  CORADE   corner-coordinates of endcap z>0
C  CORBDE   corner-coordinates of barrel
C  CORCDE   corner-coordinates of endcap z<0
C  ETILDE   tilt of ECAL
C  POLADE   polar-angles phi&theta of endcap(z>0)corners
C  POLBDE   polar-angles phi&theta of barrel corners
C  POLCDE   polar-angles phi&theta of endcap(z<0)corners
C  RMNBDE   radius-minimum of barrel
C  RMXBDE   radius-maximum of barrel
C  RICBDE   radius of inner corner of barrel modules
C  ROCBDE   radius of outer corner of barrel modules
C  NEBAM    number of barrel moduls
C  NEECM    number of end-cap moduls
C
C      PARAMETER (NEBAM=12,NEECM=12)
C      COMMON /DECGEO/ CORADE(3,12,NEECM),CORBDE(3,8,NEBAM),
C     &                CORCDE(3,12,NEECM),ETILDE ,
C     &                POLADE(2,12,NEECM),POLBDE(2,8,NEBAM),
C     &                POLCDE(2,12,NEECM),
C     &                RMNBDE ,RMXBDE ,RICBDE ,ROCBDE,
C     &                RMNEDE ,RMXEDE ,RICEDE ,RMCEDE ,ROCEDE
C#######################################################################
C
C  LCAL
C
C-----------------------------------------------------------------------
C
C  LCAL-geometry common block Dali_LCal_GEOmetry
C
C  ZDISDL   z-distance from 0
C  RINNDL   inner radius of LCAL
C  ROUTDL   outer radius of LCAL
C  ZLENDL   z-length of LCAL
C
C      COMMON /DLCGEO/ ZDISDL,RINNDL,ROUTDL,ZLENDL
C#######################################################################
C
C  SATR
C
C-----------------------------------------------------------------------
C
C  SATR-GEOMETRY common block Dali_SAtr_GEOmetry
C
C  ZDISDS   z-distance from 0 ( explanation see DGRSAT )
C  RINNDS   inner radius of SATR (sensitive !)
C  ROUTDS   outer radius of SATR (sensitive !)
C  ZLENDS   z-length of SATR
C
C      COMMON /DSAGEO/ ZDISDS,RINNDS,ROUTDS,ZLENDS
C#######################################################################
C
C  TPC
C
C-----------------------------------------------------------------------
C
C  TPC-geometry common block     Dali_TPc_GEOmetry
C
C  CORNDT   coordinates of cornerpoints of of all sectors of
C           the 3 sector-types
C  POLCDT   polar coordinates (radius,phi) of the 3 sector-types
C           ( these are the same for all sectors of one type ! )
C  FISTDT   starting-value for phi for the 3 sector-types
C  RMAXDT   maximum radius of TPC sensitive volume
C  RMINDT   minimum radius of TPC
C  ZMAXDT   maximum z-length of TPC
C
C  NTYPDT   max number of types (3)
C  NSLTDT   number of slots for each type (6)
C  NCORDT   number of corners for each of the types
C
C  remember:
C    max number of corners    : 10 (2*LTCORN - 2)
C    max number of slots      :  6 (LTSLOT/2 )  ( 1 dummy slot )
C    max number of slot-types :  3 ( M, W, K )
C
C      COMMON /DTPGEO/ FISTDT(3),
C     &       CORNDT (2,10,6,3), POLCDT ( 2,10,3 ),
C     &       CRNGDT (2,10,6,3),
C     &       NTYPDT,NCORDT(3),NSLTDT,
C     &       RMAXDT , RMINDT , ZMAXDT
C#######################################################################
C
C  HCAL
C
C-----------------------------------------------------------------------
C
C  HCAL-geometry common block Dali_HCal_GEOmetry
C
C  CORADH   corner-coordinates of endcap z>0
C  CORBDH   corner-coordinates of barrel
C  CORCDH   corner-coordinates of endcap z<0
C
C  NHBAM    number of barrel moduls
C  NHECM    number of endcap moduls
C
C      PARAMETER (NHBAM=12,NHECM=6)
C      COMMON /DHCGEO/ CORADH(3,16,NHECM),CORBDH(3,8,NHBAM),
C     &                CORCDH(3,16,NHECM),
C     &                POLADH(2,16,NHECM),POLBDH(2,8,NHBAM),
C     &                POLCDH(2,16,NHECM),
C     &                RMNBDH,RMXBDH,RICBDH,ROCBDH,
C     &                RMNEDH,RMXEDH,RMXIDH,RICEDH,RMCEDH,ROCEDH
C######################################################################
C
C  MUON-DEDECTOR-geometry common blocks
C
C==============================================
C
C  MUON-BARREL-geometry common block Dali_Barrel_Muon_GEOmetry
C
C       NSLTB   number of slots in barrel
C       NSLIB   number of slots in inner layer
C       NSLOB   number of slots in outer layer
C       NMTPB   number of different barrel-module types
C       NMTIB   number of different inner layer barrel-module types
C       NMTOB   number of different outer layer barrel-module types
C       NCMXB   maximum number of corners of those types
C
C      PARAMETER ( NSLTB = 34 , NSLIB = 17 , NSLOB = 17 )
C      PARAMETER ( NMTPB = 10 , NMTIB = 4  , NMTOB = 6  )
C      PARAMETER ( NCMXB = 16 )
C
C       ITYPDB    module TYPe in given slot
C       NCORDB    number of corners for given type
C       CORNDB    xyz-corner coordinates for modules in slots
C       RMNIDB    Radius MiNimum of Inner barrel layer
C       RMXIDB    Radius MaXimum of Inner barrel layer
C       RMNODB    Radius MiNimum of Outer barrel layer
C       RMXODB    Radius MaXimum of Outer barrel layer
C
C
C      COMMON /DBMGEO/  ITYPDB ( NSLTB ) , NCORDB ( NMTPB ) ,
C     &                 CORNDB ( 3, NCMXB, NSLTB ),
C     &                 RMNIDB, RMXIDB, RMNODB, RMXODB
C===============================================
C
C  MUON-MIDDEL-ANGLE-geometry common block Dali_Middle-angle_Muon_GEOmetry
C
C  ATTENTION : NCMXM has to be changed due to notch in mod-type 4
C              which is not yet accounted for !
C
C       NSLTM  number of slots in middle-angle
C       NSLHM  number of slots of subcomp. ( z>0 or z<0 )
C       NSLIM  number of slots in inner layer
C       NSLOM  number of slots in outer layer
C       NMTPM  number of module types in middle-angle
C       NCMXM  maximum number of corners of those types
C
C      PARAMETER ( NSLTM = 38 )
C      PARAMETER ( NSLHM = 19 )
C      PARAMETER ( NSLIM = 10 , NSLOM = 9 )
C      PARAMETER ( NMTPM = 9  )
C      PARAMETER ( NCMXM = 8  )
C
C       ITYPDM    module TYPe in given slot
C       NCORDM    number of corners for given type
C       CORNDM    xyz-corner coordinates for modules in slots
C       RMNIDM    Radius MiNimum of Inner middle angle layer
C       RMXIDM    Radius MaXimum of Inner middle angle layer
C       RMNODM    Radius MiNimum of Outer middle angle layer
C       RMXODM    Radius MaXimum of Outer middle angle layer
C
C
C      COMMON /DMMGEO/  ITYPDM ( NSLTM ) , NCORDM ( NMTPM ) ,
C     &                 CORNDM ( 3, NCMXM , NSLTM ),
C     &                 RMNIDM, RMXIDM, RMNODM, RMXODM
C================================================
C
C  MUON-END-CAP-geometry common block Dali_End-cap_Muon_GEOmetry
C
C  ATTENTION : the muon end-caps are here considered to be rectangles!
C              Later on, the irregular shapes of the end-caps should be
C              taken into account!
C
C       NSLTE  number of slots in end-caps
C       NSLHE  number of slots of subcomp. ( z>0 or z<0 )
C       NSLIE  number of slots in inner layer
C       NSLOE  number of slots in outer layer
C       NMTPE  number of module types in end-cap
C       NCMXE  maximum number of corners of those types
C
C      PARAMETER ( NSLTE = 16 )
C      PARAMETER ( NSLHE =  8 )
C      PARAMETER ( NSLIE =  4 , NSLOE =  4 )
C      PARAMETER ( NMTPE =  8 )
C      PARAMETER ( NCMXE =  8 )
C
C       ITYPDE    module TYPe in given slot
C       NCORDE    number of corners for given type
C       CORNDE    xyz-corner coordinates for modules in slots
C       RMNIDE    Radius MiNimum of Inner end cap layer
C       RMXIDE    Radius MaXimum of Inner end cap layer
C       RMNODE    Radius MiNimum of Outer end cap layer
C       RMXODE    Radius MaXimum of Outer end cap layer
C
C      COMMON /DEMGEO/  ITYPDE ( NSLTE ) , NCORDE ( NMTPE ) ,
C     &                 CORNDE ( 3, NCMXE , NSLTE ),
C     &                 RMNIDE, RMXIDE, RMNODE, RMXODE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C!Initialize ALEPH geometry
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DG
      COMMON / DGDBLU / LDBSDG,IRUNDG
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
C
C   for testing-purposes the run-number IRUN is set to 1
C
C    IMPORTANT : later on it will be desirable to extract the the run-number
C                for a given event ( to be displayed by DALI ) from the
C                BOS-bank EVEH ( EVEnt Header ) and therefore to read in
C                the geometry data for the dedectors anew each time the
C                run-number changes for a new event to be displayed !
C
C      IRUN=1
C
C   define logical unit number
C
C     When reading in a run around 8000, DGIVDT ... reads the old database,
C     which leads to an error in VRDDAF:
C     KRL = MDARD (IW,LUN,'VDRL',IVDSTP) -> krl=0 because VDRL is missing.
C     More serious: the tpc recalculation of TPCO via AUNPCK is wrond, due
C     to wrong TPC constants in the old DATABASE. Therefore we decide, not to
C     display VDET data befor run 10000. In that case the VDET plnes can always
C     be drawn in their actual form.
CCC      DATA IRUNV/998899/
      IRUNDG = IRUN
      LDBSDG = JUNIDB(0)
C
C   initialize the single dedector-geometry-data blocks
C
      CALL DPARGV(81,'OVD',2,OVD)
      IF(OVD.EQ.0.) THEN
        CALL DGIVDT_OLD(IRUN)
      ELSE
        CALL DGIVDT(IRUN)
      END IF
      CALL DGIITC(IRUN)
      CALL DGITPC
      CALL DGIECA
      CALL DGILCA
C     CALL DGISAT
      CALL DGISIC(IRUN)
      CALL DGIHCA
      CALL DGIMUD
      END
*DK DGIVDT
CH
CH
CH
CH
CH
CH
CH
CH
CH---------------------------------------------------------------------
C
      SUBROUTINE DGIVDT(IRUN)
C
CH----------------------------------------------------------------------
C!  -
C!
C!   Author   :- R.Vogl                12-SEP-1989
C!  **************************************************************
C!
C!   updated to the new structure of VDET on ADBSCONS.DAF with
C!   code out the JULIA-SR 'VRDDAF' (G.Triggiani & P.Cattaneo)
C!   G.Waltermann    June 90
C!
C!   modified for new structure of VDET banks: R.Vogl 22/03/91
C!
C!   Modified :- R.Vogl                14-MAR-1995 (new VRDDAF)
C!
c!  **************************************************************
C!
C!   Inputs:
C!        -
C!
C!   Outputs: fills geometry commons for Vertex-DeTector
C!        -
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!
C?
C!======================================================================
C
C   get common block for BOS vector IW defined in BCS.INC
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DG
      COMMON / DGDBLU / LDBSDG,IRUNDG
C------------------------------------------------------------------- DV
      COMMON /DVDGEO/ NSLIDV,NSLODV,NWAFDV,PALWDV,ZALWDV,
     &                TIILDV(15),TIOLDV(15),
     &                RMNIDV,RMXIDV,RMNODV,RMXODV,
     &                DELXDV,DELYDV,DELZDV,DELPDV,
     &                ZOFWDV(6), NWTSDV(15,2),
     &                RAILDV,RAOLDV,DFILDV(12),DFOLDV(15),
     &                CRILDV(2,2,15),CROLDV(2,2,15),
     &                NFRSDV(2),NZRSDV(2),
     &                PFRSDV(2),PZRSDV(2)
C     HARD WIRED IN P_DALB_ALEPH.FOR
      COMMON /DVDGE1/ FEVDDV
      LOGICAL FEVDDV
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
C
      INTEGER VNRFAC,VNRWAF,VWADIM,VJWFFW,VVUWXY,VJFACI,VROSTM
      LOGICAL VSLOTF
      DIMENSION RO(2,2)

      DIMENSION DVUW(3),VUW(3),XYZ(3)


C
      CALL VRDDAF(LDBSDG,IRUN,IFLAG)
      IF (IFLAG.EQ.0) THEN
        CALL DWRT('DGIVDT: Error reading VDET geometry using VRDDAF.')
        FEVDDV=.FALSE.
        GO TO 990
      ELSE
        FEVDDV=.TRUE.
      END IF
C      IF(IRUN.LT.IRNEW) THEN
C        FNVDDV=.FALSE.
C      ELSE
C        FNVDDV=.TRUE.
C      END IF
C   ============================================================
C**
C**
C
C
C  copy into DALI-VDET-local-arrays:
C-----------------------------------
C

      NSLIDV = VNRFAC(1)
      NSLODV = VNRFAC(2)
      NWAFDV = VNRWAF()*2

C     ......................... # and pitch of read out strips in fi and z
      DO I=1,2
        IRET = VROSTM(1,NZRSDV(I),PZRSDV(I),IDUM)
        IRET = VROSTM(2,NFRSDV(I),PFRSDV(I),IDUM)
      END DO

C
C get size of wafer

      IRET = VWADIM(DVUW)

      PALWDV = DVUW(2)
      ZALWDV = DVUW(3)

C we now assume that the centers of all wafers in a given face lie along a line
C which is parallel to the z axis (same R from z axis for all of them)
C take IFAC=1 to stand for all other faces in a layer
      DO I=1,3
        VUW(I) = 0.
      END DO
      ILAY = 1
      IFAC = 1
      IWFF = 1
      IRET = VJWFFW(ilay,ifac,iwff,JWAF)
      IRET = VVUWXY(VUW,JWAF,XYZ)
      RAILDV = SQRT(XYZ(1)**2+XYZ(2)**2)

      DO I=1,3
        VUW(I) = 0.
      END DO
      ILAY = 2
      IFAC = 1
      IWFF = 1
      IRET = VJWFFW(ilay,ifac,iwff,JWAF)
      IRET = VVUWXY(VUW,JWAF,XYZ)
      RAOLDV = SQRT(XYZ(1)**2+XYZ(2)**2)

C inner layer  occupation
      DO ISLO=1,NSLIDV
        ILAY = 1
        IFAC = ISLO
        IRET = VJFACI(ilay,ifac,JFAC)
        IF (VSLOTF(JFAC)) THEN
          NWTSDV(ISLO,1) = 1
        ELSE
          NWTSDV(ISLO,1) = 0
        END IF
      END DO
C outer layer  occupation
      DO ISLO=1,NSLODV
        ILAY = 2
        IFAC = ISLO
        IRET = VJFACI(ilay,ifac,JFAC)
        IF (VSLOTF(JFAC)) THEN
          NWTSDV(ISLO,2) = 1
        ELSE
          NWTSDV(ISLO,2) = 0
        END IF
      END DO
C all faces have the same set of wafer center z coordinates.
C just look for the first non-empty slot in the inner layer
      DO I=1,3
        VUW(I) = 0.
      END DO
      ILAY = 1
      DO ISLO=1, NSLIDV
        IF(NWTSDV(ISLO,1).NE.0) THEN
          DO IWFF=1,NWAFDV    ! running for IWFF
            IFAC = ISLO
            IRET = VJWFFW(ILAY,IFAC,IWFF,JWAF)
            IRET = VVUWXY(VUW,JWAF,XYZ)
            ZOFWDV(IWFF) = XYZ(3)
          END DO
          GO TO 100
        END IF
      END DO
  100 CONTINUE

C
C  real DALI-VDET-code:
C----------------------

      RMNIDV = 10000.
      RMXIDV = 0.
      RMNODV = 10000.
      RMXODV = 0.

      IRET = VWADIM(DVUW)

C always take IWFF=1 wafer in every face
      IWFF = 1

C inner layer
      ILAY = 1
      DO ISLO=1,NSLIDV

        VUW(1) = 0.
        VUW(2) =-0.5*DVUW(2)
        VUW(3) = 0.
        IFAC = ISLO
        IRET = VJWFFW(ilay,ifac,iwff,JWAF)
        IRET = VVUWXY(VUW,JWAF,XYZ)
        CRILDV(1,1,ISLO) = XYZ(1)
        CRILDV(2,1,ISLO) = XYZ(2)

        VUW(1) = 0.
        VUW(2) = 0.5*DVUW(2)
        VUW(3) = 0.
        IFAC = ISLO
        IRET = VJWFFW(ilay,ifac,iwff,JWAF)
        IRET = VVUWXY(VUW,JWAF,XYZ)
        CRILDV(1,2,ISLO) = XYZ(1)
        CRILDV(2,2,ISLO) = XYZ(2)

        IF (NWTSDV(ISLO,1).NE.0) THEN

          A =  ( CRILDV(2,1,ISLO) - CRILDV(2,2,ISLO)) /
     &         ( CRILDV(1,1,ISLO) - CRILDV(1,2,ISLO) )
          B =  CRILDV(2,2,ISLO) - A*CRILDV(1,2,ISLO)
          D =  ABS( B / SQRT(1. + A*A) )

          RMNIDV=MIN(RMNIDV,D)

          IF (D.LT.RMNIDV) THEN
            RMNIDV = D
          END IF

          RR1 = SQRT(CRILDV(1,1,ISLO)**2 + CRILDV(2,1,ISLO)**2)
          RR2 = SQRT(CRILDV(1,2,ISLO)**2 + CRILDV(2,2,ISLO)**2)
          RMXIDV=MAX(RMXIDV,RR1,RR2)

        END IF
      END DO
C outer layer
      ILAY = 2
      DO ISLO=1,NSLODV

        VUW(1) = 0.
        VUW(2) = - 0.5*DVUW(2)
        VUW(3) = 0.
        IFAC = ISLO
        IRET = VJWFFW(ilay,ifac,iwff,JWAF)
        IRET = VVUWXY(VUW,JWAF,XYZ)
        CROLDV(1,1,ISLO) = XYZ(1)
        CROLDV(2,1,ISLO) = XYZ(2)

        VUW(1) = 0.
        VUW(2) = 0.5*DVUW(2)
        VUW(3) = 0.
        IFAC = ISLO
        IRET = VJWFFW(ilay,ifac,iwff,JWAF)
        IRET = VVUWXY(VUW,JWAF,XYZ)
        CROLDV(1,2,ISLO) = XYZ(1)
        CROLDV(2,2,ISLO) = XYZ(2)

        IF (NWTSDV(ISLO,2).NE.0) THEN

          A =  ( CROLDV(2,1,ISLO) - CROLDV(2,2,ISLO)) /
     &         ( CROLDV(1,1,ISLO) - CROLDV(1,2,ISLO) )
          B =  CROLDV(2,2,ISLO) - A*CROLDV(1,2,ISLO)
          D =  ABS( B / SQRT(1. + A*A) )
          RMNODV=MIN(RMNODV,D)

          IF (D.LT.RMNODV) THEN
            RMNODV = D
          END IF

          RR1 = SQRT(CROLDV(1,1,ISLO)**2 + CROLDV(2,1,ISLO)**2)
          RR2 = SQRT(CROLDV(1,2,ISLO)**2 + CROLDV(2,2,ISLO)**2)
          RMXODV=MAX(RMXODV,RR1,RR2)

        END IF
      END DO
C
      CALL DGVDLF
C
  990 RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DGIVDR
CH
      ENTRY DGIVDR_NEW(RO)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by R.VOGL
C
C!:
C    Outputs   :INNER (1) AND OUTER (2) RADII.
C
C    Called by :DVVD
C ---------------------------------------------------------------------
      RO(1,1)=RMNIDV
      RO(2,1)=RMXIDV
      RO(1,2)=RMNODV
      RO(2,2)=RMXODV
      END

C
C  end of DGIVDT
C
CH
CH
CH---------------------------------------------------------------------
CH
CH
CH
CH
CH
CH
*DK DGIVDT
      SUBROUTINE DGIVDT_OLD(IRUN)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- R.Vogl                12-SEP-1989
C!  **************************************************************
C!
C!   updated to the new structure of VDET on ADBSCONS.DAF with
C!   code out the JULIA-SR 'VRDDAF' (G.Triggiani & P.Cattaneo)
C!   G.Waltermann    June 90
C!
C!   modified for new structure of VDET banks: R.Vogl 22/03/91
C!
c!  **************************************************************
C!
C!   Inputs:
C!        -
C!
C!   Outputs: fills geometry commons for Vertex-DeTector
C!        -
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!
C?
C!======================================================================
C
C   get common block for BOS vector IW defined in BCS.INC
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DG
      COMMON / DGDBLU / LDBSDG,IRUNDG
C------------------------------------------------------------------- DV
      COMMON /DVDGEO/ NSLIDV,NSLODV,NWAFDV,PALWDV,ZALWDV,
     &                TIILDV(15),TIOLDV(15),
     &                RMNIDV,RMXIDV,RMNODV,RMXODV,
     &                DELXDV,DELYDV,DELZDV,DELPDV,
     &                ZOFWDV(6), NWTSDV(15,2),
     &                RAILDV,RAOLDV,DFILDV(12),DFOLDV(15),
     &                CRILDV(2,2,15),CROLDV(2,2,15),
     &                NFRSDV(2),NZRSDV(2),
     &                PFRSDV(2),PZRSDV(2)
C     HARD WIRED IN P_DALB_ALEPH.FOR
      COMMON /DVDGE1/ FEVDDV
      LOGICAL FEVDDV
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
c
c**  ========================
c**
C      INCLUDE 'DGCOMN_VRDDAF.INC'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  CRILDV   corner-coordinates of inner layer ( wafer considered as flat !)
C  CROLDV   corner-coordinates of outer layer
C  NSLIDV   number of slots of inner layer
C  NSLODV   number of slots of outer layer
C  NWAFDV   number of silicon-chip-wafers per slot ( 4 ! )
C  TIILDV   tilts of inner-layer modules ( degree ! )
C  TIOLDV   tilts of outer-layer modules ( degree ! )
C  RAILDV   radius of inner-layer
C  RAOLDV   radius of outer-layer
C  DFILDV   displacement in phi for inner-layer-modules
C  DFOLDV   displacement in phi for outer-layer-modules
C  PALWDV   r-phi active length (=strip-length) of wafer ( type 1)
C  ZALWDV   z active length (=strip-length) of wafer ( type 1)
C  ZOFWDV   z-offset of wafers ( 4 wafer placed in a module )
C  NWTSDV   wafer type in slot ( 0 = not occupied, 1 = Munich, 2-4 = Pisa)
C  RMNIDV   minimum radius of inner-layer active-area wafer corner
C  RMXIDV
C  RMNODV
c  RMXODV
C
C  The following COMMON-blocks are needed by routine DGIVDT in file DALBG.FOR
C  for communication with the ALEPHLIB-routine VRDDAF.
C  Should there be any changes, these common-declarations have to be updated
C
C
C*CD VDGEOS
      PARAMETER (LVDL=2, LVDCC=7, LVDNC=3)
      PARAMETER (NSLOM=15, NSLOI=12, NWAFN=4,NWAFM=4, NGEOM=4)
      COMMON /VDGEOS/ NDIVZV(NGEOM) , NDIVPV(LVDL) , NCERVZ(NGEOM),
     &                VDWLEN(2,NWAFM), VDPPIT(NWAFM) , VDZPIT(NWAFM),
     &                VDDIPP(NWAFM) , VDDIPZ(NWAFM) , VDDIZP(NWAFM),
     &                VDDIZZ(NWAFM), VDCRHO(NSLOM,LVDL), NSTPVD(NWAFM),
     &                NSTZVD(NWAFM) , ACTPVD(NWAFM) , ACTZVD(NWAFM) ,
     &                VDTHCK(NWAFM) , VDPSDM(NGEOM) ,  VDBXTH(NGEOM) ,
     &                VDZOFF(NWAFN,NGEOM) , VDPOFF(NSLOM,LVDL) ,
     &                VDCPHI(NSLOM,LVDL) , VDTILT(NSLOM,LVDL) ,
     &                ZWAFVD(NWAFN,NSLOM,LVDL), VDAPPL(NWAFM),
     &                VDDEPL(NWAFM) , VDLESP(NWAFM) , VDLESZ (NWAFM),
     &                VDSTPH(NWAFM) , VDSTZE(NWAFM) , NVDPPI(NWAFM),
     &                NVDZPI(NWAFM), VDCEZO(NWAFM,NGEOM), IOPZVD(LVDL),
     &                VDCCCP(NWAFM,0:LVDCC), VDCCCZ(NWAFM,0:LVDCC),
     &                VDNCCP(NWAFM,0:LVDNC), VDNCCZ(NWAFM,0:LVDNC),
     &                NSLOCO(NSLOM,LVDL) , NSLOGM(NSLOM,LVDL) ,
     &                NSLOME(NSLOM,LVDL) , NSLOWA(NSLOM,LVDL) ,
     &                NSLOEL(NSLOM,LVDL) , NGEOWA(NGEOM) ,
     &                NVDLAY, NVDSLG, NVDMEC, NVDWAF ,
     &                IPSIGN(4,2) , IZSIGN(4,2) , NSLOAD(NSLOM,LVDL)
c**
c**  ===========================

      DIMENSION RO(2,2)

      DIMENSION VUW(3),XYZ(3)

C      INTEGER GTSTUP

CCC      DATA IVIEW / 1 /,IGET2/2/
      DATA IVIEW / 1 /
      DATA IDEB/0/
C
C  fill VDGEOS common blocks (basic VDET geometry)
C
C      BNUMDB(4,VDZTDB)=-1.
C      BNUMDB(4,VDXYDB)=-1.
C      FEVDDV = .FALSE.
C      IGET=GTSTUP('VD',IRUN)
C      IF(IGET.LE.IGET2) RETURN
C      CALL VRDDAF(LDBSDG,IRUN,IFLAG)
C      IF (IFLAG.EQ.0) THEN
C        CALL DT021A('Error reading VDET geom. DATABASE. Draw VDET? Y/N')
C        CALL DGSWPT( 5)
C        CALL DGETLN(TA,LA,1)
C        CALL DGSWPT(-5)
C        IF(LA.NE.1.OR.(TA.NE.'Y'.AND.TA.NE.'y')) GO TO 990
C      END IF
C
C  fill VGPAAL common blocks (alignment parameters)
C
C     ................................ IRUN is set to 998899 at start of DALI.
      CALL DWRT('Old VDET code is used.###')
      IF(IRUN.EQ.998899) THEN
        CALL VRDDAF(LDBSDG,IRUN,IFLAG)
C       11.6.96                       CALL V89RDDAF(LDBSDG,IRUN,IFLAG)
C       CALL VDET_CREATE_GDB
      END IF
      CALL VGRDAL(LDBSDG,IRUN,IFLAG)
      IF (IFLAG.EQ.0) THEN
        CALL DWRT('Error reading VDET algnment par.. VDET not drawn.')
        FEVDDV=.FALSE.
        GO TO 990
      ELSE
        FEVDDV=.TRUE.
      END IF
C   ============================================================
C**
C**
C
C
C  copy into DALI-VDET-local-arrays:
C-----------------------------------
C


      NWAFDV=4

      NSLIDV = NSLOI
      NSLODV = NSLOM
      IF(IDEB.EQ.1) CALL DG_VD_DEB_I('NSLIDV',-1,-1,-1,NSLIDV)
      IF(IDEB.EQ.1) CALL DG_VD_DEB_I('NSLODV',-1,-1,-1,NSLODV)
C
C  determine the active r-phi respectively z dimension of wafers as
C  number of strips times strip-pitch
C  (ONLY WAFER TYPE 1 IS CONSIDERED HERE)
      XXX=PALWDV
      XXX=ZALWDV

C     ................................ NSTPVD,VDPPIT,VDZPIT are not more
C     ................................ filled in VRDDAF. Why ??
C     ................................ Therefore we set it for the old code:
C
      PALWDV=5.12
      ZALWDV=5.12
C     PALWDV = NSTPVD(1)*VDPPIT(1)
C     ZALWDV = NSTZVD(1)*VDZPIT(1)
      IF(IDEB.EQ.1) CALL DG_VD_DEB_F('PALWDV',-1,-1,-1,XXX,PALWVD)
      IF(IDEB.EQ.1) CALL DG_VD_DEB_F('ZALWDV',-1,-1,-1,XXX,ZALWDV)

      RAILDV = 0
      RAOLDV = 0
C
      XXX=RAILDV
      DO ISLO=1,NSLIDV
        RADI = VDCRHO(ISLO,1)
        IF (RADI.GT.RAILDV) THEN
          RAILDV = RADI
        END IF
        NWTSDV(ISLO,1) = NSLOWA(ISLO,1)
        IF(IDEB.EQ.1) CALL DG_VD_DEB_I('NWTSDV',ISLO,1,-1,
     &    NWTSDV(ISLO,1))
      END DO
      IF(IDEB.EQ.1) CALL DG_VD_DEB_F('RAILDV',-1,-1,-1,XXX,RAILDV)
C
      XXX=RAOLDV
      DO ISLO=1,NSLODV
        RADI = VDCRHO(ISLO,2)
        IF (RADI.GT.RAOLDV) THEN
          RAOLDV = RADI
        END IF
        NWTSDV(ISLO,2) = NSLOWA(ISLO,2)
        IF(IDEB.EQ.1) CALL DG_VD_DEB_I('NWTSDV',ISLO,2,-1,
     &    NWTSDV(ISLO,2))
      END DO
      IF(IDEB.EQ.1) CALL DG_VD_DEB_F('RAOLDV',-1,-1,-1,XXX,RAOLDV)
C

      DO ISLO=1, NSLIDV
        IF(NWTSDV(ISLO,1).NE.0) THEN
          DO I=1,4
            CALL VAENWA(NWAF,1,I,ISLO,IVIEW)
            VUW(1) = 0.
            VUW(2) = 0.
            VUW(3) = 0.
            CALL VGWFXY(NWAF,VUW,XYZ)
            IF(IDEB.EQ.1) CALL DG_VD_DEB_F('ZOFWDV',I,-1,-1,ZOFWDV(I),
     &        XYZ(3))
            ZOFWDV(I) = XYZ(3)
          END DO
          GO TO 100
        END IF
      END DO
  100 CONTINUE



C
C  real DALI-VDET-code:
C----------------------
C  inner layer

      RMNIDV = 10000.
      RMXIDV = 0.
      RMNODV = 10000.
      RMXODV = 0.

      DO I=1,NSLIDV

        CALL VAENWA(NWAF,1,1,I,IVIEW)
        VUW(1) = 0.
        VUW(2) = - 0.5*PALWDV
        VUW(3) = 0.
        CALL VGWFXY(NWAF,VUW,XYZ)
        IF(IDEB.EQ.1) CALL DG_VD_DEB_F('CRILDV',1,1,I,CRILDV(1,1,I),
     &    XYZ(1))
        IF(IDEB.EQ.1) CALL DG_VD_DEB_F('CRILDV',2,1,I,CRILDV(2,1,I),
     &    XYZ(2))
        CRILDV(1,1,I) = XYZ(1)
        CRILDV(2,1,I) = XYZ(2)
C       VUW(1) = 0.
        VUW(2) =  0.5*PALWDV
C       VUW(3) = 0.
        CALL VGWFXY(NWAF,VUW,XYZ)
        IF(IDEB.EQ.1) CALL DG_VD_DEB_F('CRILDV',1,2,I,CRILDV(1,2,I),
     &    XYZ(1))
        IF(IDEB.EQ.1) CALL DG_VD_DEB_F('CRILDV',2,2,I,CRILDV(2,2,I),
     &    XYZ(2))
        CRILDV(1,2,I) = XYZ(1)
        CRILDV(2,2,I) = XYZ(2)

        IF (NWTSDV(I,1).NE.0) THEN

          A =  ( CRILDV(2,1,I) - CRILDV(2,2,I)) /
     &         ( CRILDV(1,1,I) - CRILDV(1,2,I) )
          B =  CRILDV(2,2,I) - A*CRILDV(1,2,I)
          D =  ABS( B / SQRT(1. + A*A) )

          IF (D.LT.RMNIDV) THEN
            RMNIDV = D
          END IF

          RR1 = SQRT(CRILDV(1,1,I)**2 + CRILDV(2,1,I)**2)
          RR2 = SQRT(CRILDV(1,2,I)**2 + CRILDV(2,2,I)**2)

          IF (RR1.LT.RR2) THEN
            RR3 = RR1
            RR1 = RR2
            RR2 = RR3
          END IF

          IF (RR1.GT.RMXIDV) THEN
            RMXIDV = RR1
          END IF

        END IF

      END DO
      DO I=1,NSLODV

        CALL VAENWA(NWAF,2,1,I,IVIEW)
        VUW(1) = 0.
        VUW(2) = - 0.5*PALWDV
        VUW(3) = 0.
        CALL VGWFXY(NWAF,VUW,XYZ)
        IF(IDEB.EQ.1) CALL DG_VD_DEB_F('CROLDV',1,1,I,CROLDV(1,1,I),
     &    XYZ(1))
        IF(IDEB.EQ.1) CALL DG_VD_DEB_F('CROLDV',2,1,I,CROLDV(2,1,I),
     &    XYZ(2))
        CROLDV(1,1,I) = XYZ(1)
        CROLDV(2,1,I) = XYZ(2)

        VUW(2) =  0.5*PALWDV
        CALL VGWFXY(NWAF,VUW,XYZ)
        IF(IDEB.EQ.1) CALL DG_VD_DEB_F('CROLDV',1,2,I,CROLDV(1,2,I),
     &    XYZ(1))
        IF(IDEB.EQ.1) CALL DG_VD_DEB_F('CROLDV',2,2,I,CROLDV(2,2,I),
     &    XYZ(2))
        CROLDV(1,2,I) = XYZ(1)
        CROLDV(2,2,I) = XYZ(2)

        IF (NWTSDV(I,2).NE.0) THEN

          A =  ( CROLDV(2,1,I) - CROLDV(2,2,I)) /
     &         ( CROLDV(1,1,I) - CROLDV(1,2,I) )
          B =  CROLDV(2,2,I) - A*CROLDV(1,2,I)
          D =  ABS( B / SQRT(1. + A*A) )

          IF (D.LT.RMNODV) THEN
            RMNODV = D
          END IF

          RR1 = SQRT(CROLDV(1,1,I)**2 + CROLDV(2,1,I)**2)
          RR2 = SQRT(CROLDV(1,2,I)**2 + CROLDV(2,2,I)**2)

          IF (RR1.LT.RR2) THEN
            RR3 = RR1
            RR1 = RR2
            RR2 = RR3
          END IF

          IF (RR1.GT.RMXODV) THEN
            RMXODV = RR1
          END IF

        END IF
      END DO
      IF(IDEB.EQ.1) CLOSE(UNIT=79)
      CALL DGVDLF
  990 RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DGIVDR
CH
      ENTRY DGIVDR(RO)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by R.VOGL
C
C!:
C    Outputs   :INNER (1) AND OUTER (2) RADII.
C
C    Called by :DVVD
C ---------------------------------------------------------------------
      RO(1,1)=RMNIDV
      RO(2,1)=RMXIDV
      RO(1,2)=RMNODV
      RO(2,2)=RMXODV
      END

C
C  end of DGIVDT
C


      SUBROUTINE DG_VD_DEB_F(TVAR,I,J,K,V1,V2)
C     INCLUDE 'DALI_CF.INC'
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *(*) TVAR
      WRITE(79,1000) TVAR,I,J,K,V1,V2
 1000 FORMAT(1X,A,'(',3I3,' ) = ',2F10.3)
      RETURN
      ENTRY DG_VD_DEB_I(TVAR,I,J,K,IV)
      WRITE(79,1001) TVAR,I,J,K,IV
 1001 FORMAT(1X,A,'(',3I3,' ) = ',I8)
      END



C
C
C
*DK DGIITC
      SUBROUTINE DGIITC(IRUN)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- R.Vogl                  19-AUG-1989
C!
C!   Inputs:none
C!        -
C!
C!   Outputs: fills common-block /DITGEO/ in DALI_CF.INC with ITC-geometry
C!            from ALEPH database
C!        -
C!
C!   called by : DGEINT
C!   Libraries required:
C!
C!   Description  based on a routine by R.F.XU
C!   ===========  initializes ITC-geometry
C!
C?
C!======================================================================
C ---------------------------------------------------------------------
C

C
C   get common block for BOS vector IW defined in BCS.INC
C
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_IGEOJJ.INC'
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DG
      COMMON / DGDBLU / LDBSDG,IRUNDG
C------------------------------------------------------------------- DI
      COMMON /DITGEO/ RMAXDI,RMINDI,ZMAXDI
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
C
C   due to the naming-conventions, the INTEGER FUNCTION used below is
C   called ALGTDB and therefore not implicitly declared as INTEGER
C
      INTEGER ALGTDB
C
C   due to the fact that BMACRO.INC contains statement-functions
C   it has always to be the last to be INCLUDed right before the start
C   of the FORTRAN statement block
C
      INCLUDE 'A_BMACRO.INC'
C
C   start of statement block
C
C   IMPORTANT : note that the geometry data for the ITC has to be read in
C               in this very primitive way because there exists no special
C               routine for data-extraction from the BOS-bank for the ITC
C               due to the simple geometry of this dedector
C               for ECAL and TPC such routines (e.g. ERDDAF ) are
C               supplied !
C
      IFLAG = ALGTDB (LDBSDG,'IGEO',IRUN)
      IF (IFLAG.EQ.0) THEN
        CALL DWRT('ERROR: Unable to read ITC geometry database !')
        RETURN
      END IF

      KIGEO = IW(NAMIND('IGEO'))

      IF (KIGEO .GT. 0) THEN
        ZMAXDI = RTABL(KIGEO,1,JIGEZX)
        RMINDI = RTABL(KIGEO,1,JIGERI)
        RMAXDI = RTABL(KIGEO,2,JIGERO)
      END IF

      END
C
CH..............---
CH
C
C
C######################################################################
C
*DK DGIECA
      SUBROUTINE DGIECA
C
C-----------------------------------------
C
C   Author   :- R.Vogl                  25-AUG-1989
C
C=========================================
C
C   Purpose   : read the ECAL geometry data
C   Inputs    :
C   Outputs   : fills DECGEO common block
C
C   called by : DGEINT
C
C   comment   : based on a routine by R.F.Xu
C=========================================
C +
      INCLUDE 'A_BCS.INC'
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DG
      COMMON / DGDBLU / LDBSDG,IRUNDG
C------------------------------------------------------------------- DE
      PARAMETER (NEBAM=12,NEECM=12)
      COMMON /DECGEO/ CORADE(3,12,NEECM),CORBDE(3,8,NEBAM),
     &                CORCDE(3,12,NEECM),ETILDE ,
     &                POLADE(2,12,NEECM),POLBDE(2,8,NEBAM),
     &                POLCDE(2,12,NEECM),
     &                RMNBDE ,RMXBDE ,RICBDE ,ROCBDE,
     &                RMNEDE ,RMXEDE ,RICEDE ,RMCEDE ,ROCEDE
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
C
C   arrays to be used as output-arguments for calls of EVOLPL and EVOLCR
C   some of the polyhedrons the ECAL is composed of have 6, some have 8
C   faces, so the array for storing the coefficients of the faces( these
C   are naturaly 4 ( a*x+b*y+c*z=d !!)) is dimensioned to 8
C   the same for the 3 coordinates for the 8 respectively 12 corners
C   of the polyhedrons
C
      DIMENSION PLANE(4,8),CORNR(3,12)
      PARAMETER ( PI = 3.141592653589 )
      PARAMETER ( RADDEG = 180./PI , DEGRAD = PI/180. )

C
C   use ERDDAF and ECDFRD to expand ECAL geometry data in order to be
C   accessed more easily
C
      CALL ERDDAF(LDBSDG,IRUNDG,IGOOD)
      IF( IGOOD.EQ.0)  THEN
        CALL DWRT(' ERROR: unable to read ECAL geometry data !')
        CALL DWRT('        source of error : ERDDAF.')
        RETURN
      END IF
      CALL ECDFRD
C
C   initialisation completed !
C

C
C   read in corners of all barrel and endcap modules and fill
C   the DECGEO common-block with them.
C   only the sensitive parts are taken into acount!
C

C  get tilt of ECAL
      ETILDE = RADDEG*ECTILT(0)

C  BARREL-module corners, SubComponent# is 2 for barrel
      ISC = 2
      DO MD = 1 , NEBAM
C        CALL EVOLPL('B sensitive     ',ISC,MD,NPLAN,PLANE)
C        CALL EVOLCR('B sensitive     ',NPLAN,PLANE,NCORN,CORNR)

        CALL EVOLPL('B external      ',ISC,MD,NPLAN,PLANE)
        CALL EVOLCR('B external      ',NPLAN,PLANE,NCORN,CORNR)

        DO I = 1 , 8
          DO J = 1 , 3
            CORBDE(J,I,MD) = CORNR(J,I)
          END DO
        END DO
      END DO

      RICBDE = SQRT(CORBDE(1,1,1)**2 + CORBDE(2,1,1)**2)
      ROCBDE = SQRT(CORBDE(1,5,1)**2 + CORBDE(2,5,1)**2)
      D13    = (CORBDE(1,1,1) - CORBDE(1,3,1))**2 +
     &         (CORBDE(2,1,1) - CORBDE(2,3,1))**2
      RMNBDE = SQRT( RICBDE**2 - D13/4 )
      RMXBDE = ROCBDE

      DO MD = 1 , NEBAM
        DO I = 1 , 8
          POLBDE(1,I,MD) = DATN2D(CORBDE(2,I,MD),CORBDE(1,I,MD))
          IF (I.LE.4) THEN
            POLBDE(2,I,MD) = - DATN2D(RICBDE,CORBDE(3,I,MD))
          ELSE
            POLBDE(2,I,MD) = - DATN2D(ROCBDE,CORBDE(3,I,MD))
          END IF
        END DO
      END DO
C
C  module 1 is situated on the x-axis ( ambiguity of atan results !)
C  point 3 is in the 1st quadrant and therefore taken as referense
C
      DO I = 1 , 8
        POLBDE(1,I,1) = DFINXT( POLBDE(1,3,1) , POLBDE(1,I,1))
      END DO


C  ENDCAP-module corners,SC# is 1 for endcap z>0
      ISC = 1
      DO MD = 1 , NEECM
C        CALL EVOLPL('E sensitive     ',ISC,MD,NPLAN,PLANE)
C        CALL EVOLCR('E sensitive     ',NPLAN,PLANE,NCORN,CORNR)

        CALL EVOLPL('E external      ',ISC,MD,NPLAN,PLANE)
        CALL EVOLCR('E external      ',NPLAN,PLANE,NCORN,CORNR)

        DO I = 1 , 12
          DO J = 1 , 3
            CORADE(J,I,MD) = CORNR(J,I)
          END DO
        END DO
      END DO

      RICEDE = SQRT(CORADE(1,1,1)**2 + CORADE(2,1,1)**2)
      RMCEDE = SQRT(CORADE(1,5,1)**2 + CORADE(2,5,1)**2)
      ROCEDE = SQRT(CORADE(1,9,1)**2 + CORADE(2,9,1)**2)
      D13    = (CORADE(1,1,1) - CORADE(1,3,1))**2 +
     &         (CORADE(2,1,1) - CORADE(2,3,1))**2
      RMNEDE = SQRT( RICEDE**2 - D13/4 )
      RMXEDE = ROCEDE

      DO MD = 1 , NEECM
        DO I = 1 , 12
          POLADE(1,I,MD) = DATN2D(CORADE(2,I,MD),CORADE(1,I,MD))
          IF (I.LE.4) THEN
            POLADE(2,I,MD) = - DATN2D(RICEDE,CORADE(3,I,MD))
          ELSE IF (I.LE.8) THEN
            POLADE(2,I,MD) = - DATN2D(RMCEDE,CORADE(3,I,MD))
          ELSE
            POLADE(2,I,MD) = - DATN2D(ROCEDE,CORADE(3,I,MD))
          END IF
        END DO
      END DO
C
C  module 1 is situated on the x-axis ( ambiguity of atan results !)
C  point 3 is in the 1st quadrant and therefore taken as referense
C
      DO I = 1 , 12
        POLADE(1,I,1) = DFINXT( POLADE(1,3,1) , POLADE(1,I,1))
      END DO

C  ENDCAP-module corners,SC# is 3 for endcap z<0
      ISC = 3
      DO MD = 1 , NEECM
C        CALL EVOLPL('E sensitive     ',ISC,MD,NPLAN,PLANE)
C        CALL EVOLCR('E sensitive     ',NPLAN,PLANE,NCORN,CORNR)

        CALL EVOLPL('E external      ',ISC,MD,NPLAN,PLANE)
        CALL EVOLCR('E external      ',NPLAN,PLANE,NCORN,CORNR)

        DO I = 1 , 12
          DO J = 1 , 3
            CORCDE(J,I,MD) = CORNR(J,I)
          END DO
        END DO
      END DO

      DO MD = 1 , NEECM
        DO I = 1 , 12
          POLCDE(1,I,MD) = DATN2D(CORCDE(2,I,MD),CORCDE(1,I,MD))
          IF (I.LE.4) THEN
            POLCDE(2,I,MD) = - DATN2D(RICEDE,CORCDE(3,I,MD))
          ELSE IF (I.LE.8) THEN
            POLCDE(2,I,MD) = - DATN2D(RMCEDE,CORCDE(3,I,MD))
          ELSE
            POLCDE(2,I,MD) = - DATN2D(ROCEDE,CORCDE(3,I,MD))
          END IF
        END DO
      END DO
C
C  module 1 is situated on the x-axis ( ambiguity of atan results !)
C  point 1 is in the 1st quadrant and therefore taken as referense
C
      DO I = 1 , 12
        POLCDE(1,I,1) = DFINXT( POLCDE(1,1,1) , POLCDE(1,I,1))
      END DO
C
C  NO MORE TO BE DONE
C
      END
C
C  end of DGIECA
C


C######################################################################
C
*DK DGIHCA
      SUBROUTINE DGIHCA
C
C-----------------------------------------
C
C   Author   :- R.Vogl                  25-AUG-1989
C
C=========================================
C
C   Purpose   : read the HCAL geometry data
C   Inputs    :
C   Outputs   : fills DHCGEO common block
C
C   called by : DGEINT
C
C   comment   : based on a routine by R.F.Xu
C=========================================
C +
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_HBGEJJ.INC'
      INCLUDE 'A_HEGEJJ.INC'
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DG
      COMMON / DGDBLU / LDBSDG,IRUNDG
C------------------------------------------------------------------- DH
      PARAMETER (NHBAM=12,NHECM=6)
      COMMON /DHCGEO/ CORADH(3,16,NHECM),CORBDH(3,8,NHBAM),
     &                CORCDH(3,16,NHECM),
     &                POLADH(2,16,NHECM),POLBDH(2,8,NHBAM),
     &                POLCDH(2,16,NHECM),
     &                RMNBDH,RMXBDH,RICBDH,ROCBDH,
     &                RMNEDH,RMXEDH,RMXIDH,RICEDH,RMCEDH,ROCEDH
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77

      INTEGER ALGTDB

      INCLUDE 'A_BMACRO.INC'


C
C  open BOS database
C
      IFLAG = ALGTDB(LDBSDG,'HBGEHEGE',IRUNDG)
      IF (IFLAG.EQ.0) THEN
        CALL DWRT(' ERROR: unable to read HCAL geometry data ! ')
        CALL DWRT('        source of error : ALGTDB.')
        RETURN
      END IF

C
C  read in BARREL constants
C

      KHBGE = IW( NAMIND ( 'HBGE' ))
      IF (KHBGE.NE.0) THEN

        RMNBDH = RTABL(KHBGE,1,JHBGYI)
        RMXBDH = RTABL(KHBGE,1,JHBGYX)
        ZMXBA  = RTABL(KHBGE,1,JHBGZX)
        FIOFDH = RTABL(KHBGE,1,JHBGPO)

      END IF

C
C  fill barrel commons
C
C  BAMDA   : barrel module angle
C

      BAMDA = 360. / NHBAM
      ROCBDH = RMXBDH / COSD( BAMDA / 2 )
      RICBDH = RMNBDH / COSD( BAMDA / 2 )
      FI = FIOFDH - BAMDA / 2

      DO MD = 1 , NHBAM
        POLBDH(1,1,MD) = FI
        POLBDH(1,2,MD) = FI
        POLBDH(1,5,MD) = FI
        POLBDH(1,6,MD) = FI
        POLBDH(1,3,MD) = FI + BAMDA
        POLBDH(1,4,MD) = FI + BAMDA
        POLBDH(1,7,MD) = FI + BAMDA
        POLBDH(1,8,MD) = FI + BAMDA

        CORBDH(3,1,MD) =   ZMXBA
        CORBDH(3,2,MD) = - ZMXBA
        CORBDH(3,3,MD) =   ZMXBA
        CORBDH(3,4,MD) = - ZMXBA
        CORBDH(3,5,MD) =   ZMXBA
        CORBDH(3,6,MD) = - ZMXBA
        CORBDH(3,7,MD) =   ZMXBA
        CORBDH(3,8,MD) = - ZMXBA

        A = COSD(FI)
        B = SIND(FI)
        C = COSD(FI+BAMDA)
        D = SIND(FI+BAMDA)

        CORBDH(1,1,MD) = RICBDH * A
        CORBDH(2,1,MD) = RICBDH * B
        CORBDH(1,2,MD) = RICBDH * A
        CORBDH(2,2,MD) = RICBDH * B
        CORBDH(1,3,MD) = RICBDH * C
        CORBDH(2,3,MD) = RICBDH * D
        CORBDH(1,4,MD) = RICBDH * C
        CORBDH(2,4,MD) = RICBDH * D

        CORBDH(1,5,MD) = ROCBDH * A
        CORBDH(2,5,MD) = ROCBDH * B
        CORBDH(1,6,MD) = ROCBDH * A
        CORBDH(2,6,MD) = ROCBDH * B
        CORBDH(1,7,MD) = ROCBDH * C
        CORBDH(2,7,MD) = ROCBDH * D
        CORBDH(1,8,MD) = ROCBDH * C
        CORBDH(2,8,MD) = ROCBDH * D

        DO I = 1 , 8
          IF (I.LE.4) THEN
            POLBDH(2,I,MD) = - DATN2D(RICBDH,CORBDH(3,I,MD))
          ELSE
            POLBDH(2,I,MD) = - DATN2D(ROCBDH,CORBDH(3,I,MD))
          END IF
        END DO

        FI = FI + BAMDA
      END DO

C
C  read in ENDCAP constants
C

      KHEGE = IW(NAMIND('HEGE'))
      IF (KHEGE.NE.0) THEN

        ZMNEI  = RTABL ( KHEGE,1,JHEGZI )
        ZMNEO  = RTABL ( KHEGE,1,JHEGZI+1)
        ZMXEO  = RTABL ( KHEGE,1,JHEGZX+1)
        RMNEDH = RTABL ( KHEGE,1,JHEGRI+1)
        RMXIDH = RTABL ( KHEGE,1,JHEGRX )
        RMXEDH = RTABL ( KHEGE,1,JHEGRX+1)

      END IF

C
C  fill ENDCAP commons
C
C  ECMDA   : Endcap module angle
C

      ECMDA = 360. / NHECM
      ROCEDH = RMXEDH
      RICEDH = RMNEDH
      RMCEDH = ROCEDH * COSD( ECMDA / 4 )

C
C  endcap z>0
C
      FI = 0.

      DO MD = 1 , NHECM

        POLADH(1,1 ,MD) = FI
        POLADH(1,2 ,MD) = FI
        POLADH(1,5 ,MD) = FI
        POLADH(1,6 ,MD) = FI
        POLADH(1,13,MD) = FI
        POLADH(1,14,MD) = FI
        POLADH(1,9 ,MD) = FI + ECMDA/4
        POLADH(1,10,MD) = FI + ECMDA/4
        POLADH(1,11,MD) = FI + ECMDA*3/4
        POLADH(1,12,MD) = FI + ECMDA*3/4
        POLADH(1,3 ,MD) = FI + ECMDA
        POLADH(1,4 ,MD) = FI + ECMDA
        POLADH(1,7 ,MD) = FI + ECMDA
        POLADH(1,8 ,MD) = FI + ECMDA
        POLADH(1,15,MD) = FI + ECMDA
        POLADH(1,16,MD) = FI + ECMDA

        CORADH(3,1 ,MD) = ZMXEO
        CORADH(3,2 ,MD) = ZMNEI
        CORADH(3,3 ,MD) = ZMXEO
        CORADH(3,4 ,MD) = ZMNEI
        CORADH(3,5 ,MD) = ZMXEO
        CORADH(3,6 ,MD) = ZMNEO
        CORADH(3,7 ,MD) = ZMXEO
        CORADH(3,8 ,MD) = ZMNEO
        CORADH(3,9 ,MD) = ZMXEO
        CORADH(3,10,MD) = ZMNEO
        CORADH(3,11,MD) = ZMXEO
        CORADH(3,12,MD) = ZMNEO
        CORADH(3,13,MD) = ZMNEO
        CORADH(3,14,MD) = ZMNEI
        CORADH(3,15,MD) = ZMNEO
        CORADH(3,16,MD) = ZMNEI

        A = COSD(FI)
        B = SIND(FI)
        C = COSD(FI+ ECMDA/4)
        D = SIND(FI+ ECMDA/4)
        E = COSD(FI+ ECMDA*3/4)
        F = SIND(FI+ ECMDA*3/4)
        G = COSD(FI+ ECMDA)
        H = SIND(FI+ ECMDA)

        CORADH(1,1 ,MD) = RICEDH * A
        CORADH(2,1 ,MD) = RICEDH * B
        CORADH(1,2 ,MD) = RICEDH * A
        CORADH(2,2 ,MD) = RICEDH * B
        CORADH(1,3 ,MD) = RICEDH * G
        CORADH(2,3 ,MD) = RICEDH * H
        CORADH(1,4 ,MD) = RICEDH * G
        CORADH(2,4 ,MD) = RICEDH * H
        CORADH(1,9 ,MD) = ROCEDH * C
        CORADH(2,9 ,MD) = ROCEDH * D
        CORADH(1,10,MD) = ROCEDH * C
        CORADH(2,10,MD) = ROCEDH * D
        CORADH(1,11,MD) = ROCEDH * E
        CORADH(2,11,MD) = ROCEDH * F
        CORADH(1,12,MD) = ROCEDH * E
        CORADH(2,12,MD) = ROCEDH * F
        CORADH(1,5 ,MD) = RMCEDH * A
        CORADH(2,5 ,MD) = RMCEDH * B
        CORADH(1,6 ,MD) = RMCEDH * A
        CORADH(2,6 ,MD) = RMCEDH * B
        CORADH(1,7 ,MD) = RMCEDH * G
        CORADH(2,7 ,MD) = RMCEDH * H
        CORADH(1,8 ,MD) = RMCEDH * G
        CORADH(2,8 ,MD) = RMCEDH * H
        CORADH(1,13,MD) = RMXIDH * A
        CORADH(2,13,MD) = RMXIDH * B
        CORADH(1,14,MD) = RMXIDH * A
        CORADH(2,14,MD) = RMXIDH * B
        CORADH(1,15,MD) = RMXIDH * G
        CORADH(2,15,MD) = RMXIDH * H
        CORADH(1,16,MD) = RMXIDH * G
        CORADH(2,16,MD) = RMXIDH * H

        DO I = 1 , 16
          IF (I.LE.4) THEN
            POLADH(2,I,MD) = - DATN2D(RICEDH,CORADH(3,I,MD))
          ELSE IF (I.LE.8) THEN
            POLADH(2,I,MD) = - DATN2D(RMCEDH,CORADH(3,I,MD))
          ELSE IF (I.LE.12) THEN
            POLADH(2,I,MD) = - DATN2D(ROCEDH,CORADH(3,I,MD))
          ELSE
            POLADH(2,I,MD) = - DATN2D(RMXIDH,CORADH(3,I,MD))
          END IF
        END DO

        FI = FI + ECMDA
      END DO

C
C  endcap z<0
C
      FI = 0

      DO MD = 1 , NHECM

        POLCDH(1,1 ,MD) = FI
        POLCDH(1,2 ,MD) = FI
        POLCDH(1,5 ,MD) = FI
        POLCDH(1,6 ,MD) = FI
        POLCDH(1,13,MD) = FI
        POLCDH(1,14,MD) = FI
        POLCDH(1,9 ,MD) = FI + ECMDA/4
        POLCDH(1,10,MD) = FI + ECMDA/4
        POLCDH(1,11,MD) = FI + ECMDA*3/4
        POLCDH(1,12,MD) = FI + ECMDA*3/4
        POLCDH(1,3 ,MD) = FI + ECMDA
        POLCDH(1,4 ,MD) = FI + ECMDA
        POLCDH(1,7 ,MD) = FI + ECMDA
        POLCDH(1,8 ,MD) = FI + ECMDA
        POLCDH(1,15,MD) = FI + ECMDA
        POLCDH(1,16,MD) = FI + ECMDA

        CORCDH(3,1 ,MD) = -ZMXEO
        CORCDH(3,2 ,MD) = -ZMNEI
        CORCDH(3,3 ,MD) = -ZMXEO
        CORCDH(3,4 ,MD) = -ZMNEI
        CORCDH(3,5 ,MD) = -ZMXEO
        CORCDH(3,6 ,MD) = -ZMNEO
        CORCDH(3,7 ,MD) = -ZMXEO
        CORCDH(3,8 ,MD) = -ZMNEO
        CORCDH(3,9 ,MD) = -ZMXEO
        CORCDH(3,10,MD) = -ZMNEO
        CORCDH(3,11,MD) = -ZMXEO
        CORCDH(3,12,MD) = -ZMNEO
        CORCDH(3,13,MD) = -ZMNEO
        CORCDH(3,14,MD) = -ZMNEI
        CORCDH(3,15,MD) = -ZMNEO
        CORCDH(3,16,MD) = -ZMNEI

        A = COSD(FI)
        B = SIND(FI)
        C = COSD(FI+ ECMDA/4)
        D = SIND(FI+ ECMDA/4)
        E = COSD(FI+ ECMDA*3/4)
        F = SIND(FI+ ECMDA*3/4)
        G = COSD(FI+ ECMDA)
        H = SIND(FI+ ECMDA)

        CORCDH(1,1 ,MD) = RICEDH * A
        CORCDH(2,1 ,MD) = RICEDH * B
        CORCDH(1,2 ,MD) = RICEDH * A
        CORCDH(2,2 ,MD) = RICEDH * B
        CORCDH(1,3 ,MD) = RICEDH * G
        CORCDH(2,3 ,MD) = RICEDH * H
        CORCDH(1,4 ,MD) = RICEDH * G
        CORCDH(2,4 ,MD) = RICEDH * H
        CORCDH(1,9 ,MD) = ROCEDH * C
        CORCDH(2,9 ,MD) = ROCEDH * D
        CORCDH(1,10,MD) = ROCEDH * C
        CORCDH(2,10,MD) = ROCEDH * D
        CORCDH(1,11,MD) = ROCEDH * E
        CORCDH(2,11,MD) = ROCEDH * F
        CORCDH(1,12,MD) = ROCEDH * E
        CORCDH(2,12,MD) = ROCEDH * F
        CORCDH(1,5 ,MD) = RMCEDH * A
        CORCDH(2,5 ,MD) = RMCEDH * B
        CORCDH(1,6 ,MD) = RMCEDH * A
        CORCDH(2,6 ,MD) = RMCEDH * B
        CORCDH(1,7 ,MD) = RMCEDH * G
        CORCDH(2,7 ,MD) = RMCEDH * H
        CORCDH(1,8 ,MD) = RMCEDH * G
        CORCDH(2,8 ,MD) = RMCEDH * H
        CORCDH(1,13,MD) = RMXIDH * A
        CORCDH(2,13,MD) = RMXIDH * B
        CORCDH(1,14,MD) = RMXIDH * A
        CORCDH(2,14,MD) = RMXIDH * B
        CORCDH(1,15,MD) = RMXIDH * G
        CORCDH(2,15,MD) = RMXIDH * H
        CORCDH(1,16,MD) = RMXIDH * G
        CORCDH(2,16,MD) = RMXIDH * H

        DO I = 1 , 16
          IF (I.LE.4) THEN
            POLCDH(2,I,MD) = - DATN2D(RICEDH,CORCDH(3,I,MD))
          ELSE IF (I.LE.8) THEN
            POLCDH(2,I,MD) = - DATN2D(RMCEDH,CORCDH(3,I,MD))
          ELSE IF (I.LE.12) THEN
            POLCDH(2,I,MD) = - DATN2D(ROCEDH,CORCDH(3,I,MD))
          ELSE
            POLCDH(2,I,MD) = - DATN2D(RMXIDH,CORCDH(3,I,MD))
          END IF
        END DO

        FI = FI + ECMDA
      END DO
C
C  end of dgrhcl
C
      END
C
C
C
*DK DGILCA
      SUBROUTINE DGILCA
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- R.Vogl                   2-SEP-1989
C!
C!   Inputs:none
C!        -
C!
C!   Outputs:fill LCAL geometry common-blocks
C!        -
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!
C?
C!======================================================================

      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_LCALJJ.INC'
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DG
      COMMON / DGDBLU / LDBSDG,IRUNDG
C------------------------------------------------------------------- DL
      COMMON /DLCGEO/ ZDISDL,RINNDL,ROUTDL,ZLENDL
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77

      INTEGER ALGTDB

      INCLUDE 'A_BMACRO.INC'
C
C  open BOS bank
C
      IFLAG = ALGTDB(LDBSDG,'LCAL',IRUNDG)
      IF (IFLAG.EQ.0) THEN
        CALL DWRT(' ERROR: unable to read LCAL geometry data ! ')
        CALL DWRT('        source of error : ALGTDB.')
        RETURN
      END IF
C
C  read in LCAL geometry constants
C
      KLCAL = IW ( NAMIND ( 'LCAL' ))
      IF (KLCAL.NE.0) THEN
        ZDISDL = RTABL(KLCAL,1,JLCAZD)
        RINNDL = RTABL(KLCAL,1,JLCARI)
        ROUTDL = RTABL(KLCAL,1,JLCARO)
        ZLENDL = RTABL(KLCAL,1,JLCAZL)
      END IF
      END
C
C  End of DGILCA
C

C#####################################################################
CC
C*DK DGISAT
C      SUBROUTINE DGISAT
CC----------------------------------------------------------------------
CC!  -
CC!
CC!   Author   :- R.Vogl                11-SEP-1989
CC!
CC!   Inputs:
CC!        -
CC!
CC!   Outputs: fills geometry common block DSAGEO
CC!        -
CC!
CC!   Libraries required:
CC!
CC!   Description
CC!   ===========
CC!
CC?
CC!======================================================================
C
C      INCLUDE 'BCS.INC'
C      INCLUDE 'SATRJJ.INC'
C      INCLUDE 'SPOSJJ.INC'
C      INCLUDE 'DALI_CF.INC'
C
C      INTEGER ALGTDB
C
C      INCLUDE 'BMACRO.INC'
CC
CC  open BOS bank
CC
C      IFLAG = ALGTDB(LDBSDG,'SATRSPOS',IRUNDG)
C      IF (IFLAG.EQ.0) THEN
C        CALL DWRT(' ERROR: unable to read SATR geometry data ! ')
C        CALL DWRT('        source of error : ALGTDB.')
C        FESADS = .FALSE.
C        RETURN
C      ELSE
C        FESADS = .TRUE.
C      END IF
CC
CC  read in SATR geometry constants
CC
C      KSATR = IW ( NAMIND ( 'SATR' ))
C      IF (KSATR.NE.0) THEN
CC
CC  take only sensitive volume into account
CC
C        RINNDS = RTABL(KSATR,1,JSATIM)
C        ROUTDS = RTABL(KSATR,1,JSATOM)
CC
CC  number of layers times thickness of layer equals z-length of dedector
CC
C        ZLENDS = ITABL(KSATR,1,JSATLA)*RTABL(KSATR,1,JSATLD)
C      END IF
C
C      KSPOS = IW( NAMIND ( 'SPOS'))
C      IF (KSPOS.NE.0) THEN
CC
CC  item ZP in BOS-bank SPOS contains z-coordinate of reference point for
CC  both LCAL and SATR ( on beam-axis between LCAL and SATR, i.e. at the
CC  outmost point of SATR and the innermost point of LCAL )
CC
C        ZDISDS = RTABL(KSPOS,1,JSPOZP)
C      END IF
C
C      END
CC
CC  End of DGISAT
C#####################################################################
C
*DK DGISIC
      SUBROUTINE DGISIC(IRUN)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- R.Vogl                26-NOV-1991
C!
C!   Inputs:
C!        -
C!
C!   Outputs: fills geometry common block DSIGEO
C!        -
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!
C?
C!======================================================================

C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DG
      COMMON / DGDBLU / LDBSDG,IRUNDG
C------------------------------------------------------------------- DS
      COMMON /DSIGEO/ ZAMIDS,ZAMADS,RAMIDS,RAMADS,
     &                ZBMIDS,ZBMADS,RBMIDS,RBMADS,FESIDS
      LOGICAL FESIDS
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DATA PIDEG/57.29577951/
      DIMENSION ICONS(5), RCONS(20)
C      INTEGER GTSTUP

C      ISTUP = GTSTUP('SI',IRUN)
C      IF(ISTUP.EQ.0) FESIDS=.FALSE.
      CALL SIRDAF(LDBSDG,IRUN,IFLAG)
      IF (IFLAG.NE.0) THEN
        CALL DWRT(' ERROR: Unable to read SICAL geometry database !')
        CALL DWRT('        source of error : SIRDAF .')
        FESIDS = .FALSE.
        RETURN
      ELSE
        FESIDS = .TRUE.
      END IF

      CALL SIGTGO(ICONS,RCONS)

C  set up geometry COMMON DSIGEO
C
C   RAMADS    Radius module A (z>0) MAximum of SICAL
C   RAMIDS    Radius module A (z>0) MInimum of SICAL
C
C   etc etc etc ...

        RAMIDS = RCONS(4)
        RBMIDS = RCONS(5)
        RAMADS = RAMIDS + ICONS(3)*RCONS(6)
        RBMADS = RBMIDS + ICONS(3)*RCONS(6)

        ZAMIDS = RCONS(1)
        ZBMIDS = RCONS(2)
        IF (ZBMIDS.GT.0.) ZBMIDS= - ZBMIDS
        ZAMADS = ZAMIDS + ICONS(2)*RCONS(3)
        ZBMADS = ZBMIDS - ICONS(2)*RCONS(3)

      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DGRSIC
CH
      ENTRY DGRSIC(DZ,DF,DR,R0)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.DREVERMANN
C
C!:
C    Outputs   :DF,DR,DZ size of sircal cells
C
C    Called by :DVSIC
      DF=0.5*RCONS(10)*PIDEG
      DR=0.5*RCONS(6)
      DZ=0.5*RCONS(3)
      R0=RCONS(4)
      END
C
C  End of DGISIC


C#####################################################################
C
*DK DGITPC
      SUBROUTINE DGITPC
C ---------------------------------------------------------------------
C
C    Created by R.F.XU                      2-DEC-1988
C    Modified :- R.Vogl                     3-SEP-1989
C
C!   Modified :- R.Vogl                    27-OCT-1989
C
C    Inputs    :none
C    Outputs   :fills TPC common-blocks
C
C    Called by : DGEINT
C ---------------------------------------------------------------------
C

      INCLUDE 'A_BCS.INC'
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DG
      COMMON / DGDBLU / LDBSDG,IRUNDG
C------------------------------------------------------------------- DT
      COMMON /DTPGEO/ FISTDT(3),
     &       CORNDT (2,10,6,3), POLCDT ( 2,10,3 ),
     &       CRNGDT (2,10,6,3),
     &       NTYPDT,NCORDT(3),NSLTDT,
     &       RMAXDT , RMINDT , ZMAXDT
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77

C
C  common block for data-transfer from DGITSA
C
      PARAMETER( LTCORN = 6, LTSTYP = 3 )
      COMMON /DGETPC/
     &            PX_DG (2*LTCORN-1,LTSTYP), PY_DG (2*LTCORN-1,LTSTYP),
     &            PF_DG (2*LTCORN-1,LTSTYP), PR_DG (2*LTCORN-1,LTSTYP),
     &            P_NG (2,2*LTCORN-2,LTSTYP),
     &            RMAX,RMIN,ZMAX,NSLT,NTYP,NCOR(3)

C
C  initialize TPC-geometry common-blocks
C
      CALL TRDDAF(LDBSDG,IRUNDG,IGOOD)
      IF (IGOOD.EQ.0) THEN
        CALL DWRT(' ERROR: unable to read TPC geometry data !')
        CALL DWRT('        source of error : TRDDAF.')
        RETURN
      END IF
C
C  initialisation complete !
C

C
C  the subroutine DGITSA by R.Richter computes the sensitive area of
C  the different TPC sectors
C
C  this subroutine is treated here as a 'BLACK BOX' !
C  it gives the required data for the corners of the sensitive volume
C  of the 3 different sector-types via the common block DGETPC and rids
C  us of bothering about reading in the data from the BOS-banks
C
      CALL DGITSA
C
      RMAXDT = RMAX
      RMINDT = RMIN
      ZMAXDT = ZMAX
      NSLTDT = NSLT
      NTYPDT = NTYP
      NCORDT(1) = NCOR(1)
      NCORDT(2) = NCOR(2)
      NCORDT(3) = NCOR(3)

      DO ITYPE = 1 , NTYPDT

        IF (ITYPE.EQ.2) THEN
          FISTDT(ITYPE) = -90.
        ELSE
          FISTDT(ITYPE) = -60.
        END IF

        DO J = 1 , NSLTDT
          PHI =FISTDT(ITYPE)+60.*FLOAT(J-1)
          CALL DGDCOS(PHI,AL1,AM1,AL2,AM2)
          DO K = 1 , NCORDT(ITYPE)
            CORNDT(1,K,J,ITYPE)=AL1*PX_DG(K,ITYPE)+AL2*PY_DG(K,ITYPE)
            CORNDT(2,K,J,ITYPE)=AM1*PX_DG(K,ITYPE)+AM2*PY_DG(K,ITYPE)
          END DO
          CALL DGDCOS(PHI+30.,AL1,AM1,AL2,AM2)
          DO K = 1 , NCORDT(ITYPE)
            CRNGDT(1,K,J,ITYPE)=AL1*P_NG(1,K,ITYPE)+AL2*P_NG(2,K,ITYPE)
            CRNGDT(2,K,J,ITYPE)=AM1*P_NG(1,K,ITYPE)+AM2*P_NG(2,K,ITYPE)
          END DO
        END DO

        DO I = 1 , NCORDT(ITYPE)
          POLCDT (1,I,ITYPE) = PR_DG (I,ITYPE)
          POLCDT (2,I,ITYPE) = PF_DG (I,ITYPE)
        END DO

      END DO
      END
C
C  end of DGITPC
C



*DK DGITSA
      SUBROUTINE DGITSA
C -------------------------------------------------------------------
C!  Compute sensitive area of the TPC-sectors
C!
C!  Author:   R.Richter   3-11-88
C!
C!  Modified :- R.Vogl                 3-SEP-1989
C!
C!  Description:
C!  ===========
C!  The sensitive area (where wires are surrounded by gas)
C!  is determined from the (outer) sector CORNERs, as given by
C!  data base (TSGM), and the wire-support-frame thickness.
C!  The output is the complete set of (inner) corner points,
C!  limiting the sensitive area. The last point is identical
C!  to the first point.
C!
C!-------------------------------------------------------------------
C
      IMPLICIT   REAL*8 (E)
C
C     Use standard JULIA/GALEPH common blocks
C         loaded by S.R. TRDDAF from the ADBSCONS-database
C
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
C
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,
     +           LMXPDR=150,LTTSRW=11)
C
      COMMON /TPGEOM/RTPCMN,RTPCMX,ZTPCMX,DRTPMN,DRTPMX,DZTPMX,
     &               TPFRDZ,TPFRDW,TPAVDZ,TPFOF1,TPFOF2,TPFOF3,
     &               TPPROW(LTPDRO),TPTROW(LTTROW),NTSECT,NTPROW,
     &               NTPCRN(LTSTYP),TPCORN(2,LTCORN,LTSTYP),
     &               TPPHI0(LTSECT),TPCPH0(LTSECT),TPSPH0(LTSECT),
     &               ITPTYP(LTSECT),ITPSEC(LTSECT),IENDTP(LTSECT)
C
      COMMON /TPGEOW/ TWSTEP(LTSTYP),TWIRE1(LTSTYP),NTWIRE(LTSTYP),
     &                TWIRMN(LTWIRE,LTSTYP),TWIRMX(LTWIRE,LTSTYP),
     &                TWIRLE(LTWIRE,LTSTYP),ITLWIF(LTSTYP),
     &                ITLWIL(LTSTYP),NTREG1(4,LTSTYP),TFRATH
C
      COMMON /DGETPC/
     &            PX_DG (2*LTCORN-1,LTSTYP), PY_DG (2*LTCORN-1,LTSTYP),
     &            PF_DG (2*LTCORN-1,LTSTYP), PR_DG (2*LTCORN-1,LTSTYP),
     &            P_NG (2,2*LTCORN-2,LTSTYP),
     &            RMAX,RMIN,ZMAX,NSLT,NTYP,NCOR(3)
C
      DIMENSION CORN(2,LTCORN), ANGREF(3), IANGPT(3), EPAR(3,0:LTCORN)
      DATA IANGPT / 1, 7, 8/
C
C  gain RMAX , RMIN and ZMAX in this subroutine in order to avoid
C  including all the TPCDES commons in DALIG !
C
      RMAX = RTPCMX
      RMIN = RTPCMN
      ZMAX = ZTPCMX
      NTYP = LTSTYP
      NSLT = LTSLOT/2
      NCOR(1) = 2*NTPCRN(1)
      NCOR(2) = 2*NTPCRN(2)
      NCOR(3) = 2*NTPCRN(3)

      DO ITYPE = 1, LTSTYP
        NC = NTPCRN(ITYPE)
        DO J = 1 , NC
          P_NG(1,J       ,ITYPE) =  TPCORN(1,J,ITYPE)
          P_NG(2,J       ,ITYPE) =  TPCORN(2,J,ITYPE)
          P_NG(1,2*NC-J+1,ITYPE) =  TPCORN(1,J,ITYPE)
          P_NG(2,2*NC-J+1,ITYPE) = -TPCORN(2,J,ITYPE)
        END DO
      END DO

C
C  LOOP OVER SECTOR TYPES.
C
      DO 500 ITYPE = 1,LTSTYP
C
        ANGREF (ITYPE) = TPPHI0 (IANGPT(ITYPE))
C
C  Get the number of corners and their coordinates for this sector type
C
        NCORN = NTPCRN (ITYPE)
        NCOR21 = 2 * NCORN + 1
C
        CALL UCOPY (TPCORN(1,1,ITYPE), CORN(1,1), 2*NCORN)
C
C  Definition of the 0-th and NCORN-th inner boundary.
C  (There are no wiring frames on top and bottom of the sector).
C
C
        EPAR (1,0)     =  0.D0
        EPAR (2,0)     = -1.D0
        EPAR (3,0)     =  CORN (1,1)
C
        EPAR (1,NCORN) =  0.D0
        EPAR (2,NCORN) = -1.D0
        EPAR (3,NCORN) =  CORN (1,NCORN)
C
C  Loop over the CORNERs of the sector at X > 0
C
        DO 100 ICOR  = 1,NCORN
C
C  Treate last point differently, as boundery already defined:
C
          IF (ICOR .EQ. NCORN) GOTO 50
C
C  Determine slope m and intercept b with the y-axis
C     of the line which forms the outer boundary of this region.
C     Generate the coeff.of its general equation  A x + B y + C = 0.
C     In the Hesse Normal form, where C is the distance to the origin,
C     we have A = m/d, B = -1/d and C = b/d, with d = SQRT (m*m +1).
C
          EY1        = CORN (1,ICOR)
          EY2        = CORN (1,ICOR+1)
          EDY        = EY2 - EY1
          EX1        = CORN (2,ICOR)
          EX2        = CORN (2,ICOR+1)
          EDX        = EX2 - EX1
          ESM        = EDY / EDX
          ESIG       = EDX / DABS (EDX)
          ESB        = - ESM * EX1 + EY1
          EDD        = DSQRT (1.D0 + ESM*ESM)
C
C
C  Find the line which forms the inner boundary of this region,
C  i.e. the parallel to the outer boundery at distance TFRATH.
C
          EPAR (1,ICOR) =  ESM / EDD
          EPAR (2,ICOR) = -1D0 / EDD
C
C        Shift outer boundary parallel in distance TFRATH
C
          EADD          =  ESIG * TFRATH
C        Exception for M-Sector, between Point 2 and 3 (no frame):
          IF (ITYPE .EQ. 2 .AND. ICOR .EQ. 2) EADD = 0.D0
          EPAR (3,ICOR) =  ESB / EDD + EADD
C
C  Find the intersection between inner boundery ICOR and ICOR-1,
C  giving the inner corner point ICOR.
C
   50     E1  =  EPAR(3,ICOR)/EPAR(2,ICOR) - EPAR(3,ICOR-1)/EPAR(2,ICOR-
     &      1)
          E2  =  EPAR(1,ICOR)/EPAR(2,ICOR) - EPAR(1,ICOR-1)/EPAR(2,ICOR-
     &      1)
          EPP =  E1 / E2
          PX_DG (ICOR,ITYPE) = - EPP
          PY_DG (ICOR,ITYPE) = (EPP*EPAR(1,ICOR)-EPAR(3,ICOR))/EPAR(2,
     &      ICOR)
C
C  Create the symmetric point at X < 0.
C
          ISYM       =   NCOR21 - ICOR
          PX_DG (ISYM,ITYPE) = - PX_DG (ICOR,ITYPE)
          PY_DG (ISYM,ITYPE) =   PY_DG (ICOR,ITYPE)
C
C  Next outer corner point
C
  100   CONTINUE
C
C  Create the last (dummy) inner corner point (= to the first one).
C
        PX_DG (NCOR21,ITYPE) =  PX_DG (1,ITYPE)
        PY_DG (NCOR21,ITYPE) =  PY_DG (1,ITYPE)
C
C  Change the coordinates of the inner points from cartesian to radial
C   (Take into account the different definition of a positive angle
C    in the Aleph global system and in the local x,y-system)
C
        DO 820 ICOR = 1,NCOR21
          PF_DG (ICOR,ITYPE) = (ATAN2 (-PX_DG(ICOR,ITYPE),PY_DG(ICOR,
     &      ITYPE))
     *    + ANGREF (ITYPE))*RADEG
          PR_DG (ICOR,ITYPE) =
     *   SQRT  (PY_DG(ICOR,ITYPE)**2 + PX_DG(ICOR,ITYPE)**2)
  820   CONTINUE
C
C  Next sector type
C
  500 CONTINUE

      RETURN
      END



*DK DGDMUD
C######################################################################
C
      SUBROUTINE DGDMUD( TPROJ, TUNIT, RNP, LAYER, FSLCT, FSLOT, RR )
C
C-----------------------------------------
C
C   Author   :- R.Vogl                21-SEP-1989
C
C=========================================
C
C   Purpose   : draw the muon-dedector in various projections
C   Inputs    : TPROJ : projection to be drawn
C               TUNIT : name of unit to be drawn ( BARREL, MIDANG,ENDCAP )
C               LAYER : number of the layer to be drawn ( 1,2 )
C               FSLCT : flag indicating if all or only selected slots
C                       are to be drawn
C               FSLOT : selected slots to be drawn
C               RR    : parameters for perspective display
C
C   Outputs   : none
C
C   Called by : DGLD
C
C=========================================
C +
C Declarations.
C -

C
C  FSLOT( I , J ) is true if slot had a hit
C
C       J   subcomponent number : 1 .. endcap
C                                 2 .. middleangle
C                                 3 .. barrel
C

C     INCLUDE 'DALI_CF.INC'
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *3 TPROJ
      CHARACTER *6 TUNIT
      DIMENSION RR(3)
      LOGICAL FSLCT, FSLOT(38,3)

      NPR = IDMIX(0,IFIX(RNP),15)

      IF      (TUNIT.EQ.'BARREL') THEN
        IF (LAYER.EQ.1) THEN
          CALL DGDMBI( TPROJ ,NPR, FSLCT, FSLOT, RR)
        ELSE IF (LAYER.EQ.2) THEN
          CALL DGDMBO( TPROJ ,NPR, FSLCT, FSLOT, RR)
        ELSE IF (LAYER.EQ.3) THEN
          CALL DGDMBI( TPROJ ,NPR, FSLCT, FSLOT, RR)
          CALL DGDMBO( TPROJ ,NPR, FSLCT, FSLOT, RR)
        END IF
      ELSE IF (TUNIT.EQ.'MIDANG') THEN
        IF (LAYER.EQ.1) THEN
          CALL DGDMMI( TPROJ ,NPR, FSLCT, FSLOT, RR)
        ELSE IF (LAYER.EQ.2) THEN
          CALL DGDMMO( TPROJ ,NPR, FSLCT, FSLOT, RR)
        ELSE IF (LAYER.EQ.3) THEN
          CALL DGDMMI( TPROJ ,NPR, FSLCT, FSLOT, RR)
          CALL DGDMMO( TPROJ ,NPR, FSLCT, FSLOT, RR)
        END IF
      ELSE IF (TUNIT.EQ.'ENDCAP') THEN
        IF (LAYER.EQ.1) THEN
          CALL DGDMEI( TPROJ ,NPR, FSLCT, FSLOT, RR)
        ELSE IF (LAYER.EQ.2) THEN
          CALL DGDMEO( TPROJ ,NPR, FSLCT, FSLOT, RR)
        ELSE IF (LAYER.EQ.3) THEN
          CALL DGDMEI( TPROJ ,NPR, FSLCT, FSLOT, RR)
          CALL DGDMEO( TPROJ ,NPR, FSLCT, FSLOT, RR)
        END IF
      ELSE IF (TUNIT.EQ.'BAR+MA') THEN
        IF (LAYER.EQ.1) THEN
          CALL DGDMBI( TPROJ ,NPR, FSLCT, FSLOT, RR)
          CALL DGDMMI( TPROJ ,NPR, FSLCT, FSLOT, RR)
        ELSE IF (LAYER.EQ.2) THEN
          CALL DGDMBO( TPROJ ,NPR, FSLCT, FSLOT, RR)
          CALL DGDMMO( TPROJ ,NPR, FSLCT, FSLOT, RR)
        ELSE IF (LAYER.EQ.3) THEN
          CALL DGDMBI( TPROJ ,NPR, FSLCT, FSLOT, RR)
          CALL DGDMBO( TPROJ ,NPR, FSLCT, FSLOT, RR)
          CALL DGDMMI( TPROJ ,NPR, FSLCT, FSLOT, RR)
          CALL DGDMMO( TPROJ ,NPR, FSLCT, FSLOT, RR)
        END IF
      ELSE IF (TUNIT.EQ.'B+MA+E') THEN
        IF (LAYER.EQ.1) THEN
          CALL DGDMBI( TPROJ ,NPR, FSLCT, FSLOT, RR)
          CALL DGDMMI( TPROJ ,NPR, FSLCT, FSLOT, RR)
          CALL DGDMEI( TPROJ ,NPR, FSLCT, FSLOT, RR)
        ELSE IF (LAYER.EQ.2) THEN
          CALL DGDMBO( TPROJ ,NPR, FSLCT, FSLOT, RR)
          CALL DGDMMO( TPROJ ,NPR, FSLCT, FSLOT, RR)
          CALL DGDMEO( TPROJ ,NPR, FSLCT, FSLOT, RR)
        ELSE IF (LAYER.EQ.3) THEN
          CALL DGDMBI( TPROJ ,NPR, FSLCT, FSLOT, RR)
          CALL DGDMBO( TPROJ ,NPR, FSLCT, FSLOT, RR)
          CALL DGDMMI( TPROJ ,NPR, FSLCT, FSLOT, RR)
          CALL DGDMMO( TPROJ ,NPR, FSLCT, FSLOT, RR)
          CALL DGDMEI( TPROJ ,NPR, FSLCT, FSLOT, RR)
          CALL DGDMEO( TPROJ ,NPR, FSLCT, FSLOT, RR)
        END IF
      END IF

      END
C
C  end of DGDMUD
C

*DK DGDMBI
C######################################################################
C
      SUBROUTINE DGDMBI ( TPROJ , NPR ,FSLCT, FSLOT, RR )
C
C-----------------------------------------
C
C   Author   :- R.Vogl                21-SEP-1989
C
C=========================================
C
C   Purpose   : draw inner layer of muon-barrel
C   Inputs    : TPROJ : projection to be drawn
C               NINTP : number of interpol. points
C   Outputs   :
C
C=========================================

C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DB
      PARAMETER ( NSLTB = 34 , NSLIB = 17 , NSLOB = 17 )
      PARAMETER ( NMTPB = 10 , NMTIB = 4  , NMTOB = 6  )
      PARAMETER ( NCMXB = 16 )
      COMMON /DBMGEO/  ITYPDB ( NSLTB ) , NCORDB ( NMTPB ) ,
     &                 CORNDB ( 3, NCMXB, NSLTB ),
     &                 RMNIDB, RMXIDB, RMNODB, RMXODB
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77

      CHARACTER *3 TPROJ
      DIMENSION RR(3)
      LOGICAL  FSLCT, FSLOT(38,3)

C
C  pathes for the poly-lines to be drawn ( xx ... projection )
C
C       LPAxx   length of path
C       IPAxx   corner-point number
C       INSxx   corner-points on poly-line after which interp. points are
C               to be inserted
C
      DIMENSION LPAYX ( NMTPB )
      DATA LPAYX / 5,5,5,7,5,5,5,7,5,5 /
      DIMENSION IPAYX ( 7 , NMTPB )
      DATA IPAYX / 1,2,6,5,1,0,0,
     &             1,2,8,7,1,0,0,
     &             1,2,6,5,1,0,0,
     &             2,1,9,12,4,3,11,
     &             1,2,6,5,1,0,0,
     &             1,2,8,7,1,0,0,
     &             1,2,6,5,1,0,0,
     &             2,1,9,12,4,3,11,
     &             1,2,6,5,1,0,0,
     &             1,2,6,5,1,0,0 /
      DIMENSION INSYX ( 2, NMTPB )
      DATA INSYX / 1,6, 1,8, 1,6, 1,12, 1,6, 1,8, 1,6, 1,12, 1,6, 1,6 /

      DIMENSION LPAFT ( NMTPB )
      DATA LPAFT / 5,7,5,9,5,7,5,9,5,5 /
      DIMENSION IPAFT ( 9 , NMTPB )
      DATA IPAFT /  1, 2, 3, 4, 1, 0, 0, 0, 0,
     &              1, 2, 3, 4, 5, 6, 1, 0, 0,
     &              1, 2, 3, 4, 1, 0, 0, 0, 0,
     &              1, 2, 3, 4, 5, 6, 7, 8, 1,
     &              1, 2, 3, 4, 1, 0, 0, 0, 0,
     &              1, 2, 3, 4, 5, 6, 1, 0, 0,
     &              1, 2, 3, 4, 1, 0, 0, 0, 0,
     &              1, 2, 3, 4, 5, 6, 7, 8, 1,
     &              1, 2, 3, 4, 1, 0, 0, 0, 0,
     &              1, 2, 3, 4, 1, 0, 0, 0, 0 /
      DIMENSION INSFT ( 2, NMTPB )
      DATA INSFT / 1,3, 1,0, 1,3, 0,0, 1,3, 1,0, 1,3, 0,0, 1,3, 1,3 /

      DIMENSION LPAFR ( NMTPB )
      DATA LPAFR / 5,5,5,5,5,5,5,5,5,5 /
      DIMENSION IPAFR ( 5 , NMTPB )
      DATA IPAFR / 1,2,6,5,1,
     &             1,2,8,7,1,
     &             1,2,6,5,1,
     &             1,4,12,9,1,
     &             1,2,6,5,1,
     &             1,2,8,7,1,
     &             1,2,6,5,1,
     &             1,4,12,9,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1  /
      DIMENSION INSFR ( 2, NMTPB )
      DATA INSFR / 1,6, 1,8, 1,6, 1,12, 1,6, 1,8, 1,6, 1,12, 1,6, 1,6 /

      DIMENSION H(50),V(50)

C
C  statement-functions
C
C      HPER(X,Y,R) = X*(1+E*R2)/(1+E*R)
C      VPER(X,Y,R) = Y*(1+E*R2)/(1+E*R)
C  perspective parameters
C      R1 = RR(1)
C      R2 = RR(2)
C      R1P= RR(3)
C

      NINTP = NPR

      IF (TPROJ(2:3).EQ.'YX') THEN

        DO I=1 , NSLIB
          DO J=1 , LPAYX( ITYPDB(I))
            H(J) = CORNDB( 1, IPAYX(J,ITYPDB(I) ), I)
            V(J) = CORNDB( 2, IPAYX(J,ITYPDB(I) ), I)
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,3 ) ) THEN
              CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
            END IF
          ELSE
            CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
          END IF
        END DO
      ELSE IF (TPROJ(2:3).EQ.'PP') THEN

C        E = (R1P - R1)/(R1*(R2-R1P))

        DO I=1 , NSLIB
          K = 1
          JJ = 0
          DO J=1 , LPAYX( ITYPDB(I))
            JJ = JJ + 1
            H(JJ) = CORNDB( 1, IPAYX(J,ITYPDB(I) ), I)
            V(JJ) = CORNDB( 2, IPAYX(J,ITYPDB(I) ), I)

            IF (K.LE.2) THEN
              IF (IPAYX(J,ITYPDB(I)).EQ.INSYX(K,ITYPDB(I))) THEN
                DX = CORNDB(1,IPAYX(J+1,ITYPDB(I)),I) -
     &               CORNDB(1,IPAYX(J  ,ITYPDB(I)),I)
                DY = CORNDB(2,IPAYX(J+1,ITYPDB(I)),I) -
     &               CORNDB(2,IPAYX(J  ,ITYPDB(I)),I)
                DO L=1 , NINTP
                  JJ =JJ + 1
                  H(JJ)=CORNDB(1,IPAYX(J,ITYPDB(I)),I)+(L*DX/(NINTP+1))
                  V(JJ)=CORNDB(2,IPAYX(J,ITYPDB(I)),I)+(L*DY/(NINTP+1))
                END DO
                K = K + 1
              END IF
            END IF
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,3 ) ) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF
C          IF (FSLCT) THEN
C            IF (FSLOT( I,3 ) ) THEN
C              CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
C            END IF
C          ELSE
C            CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
C          END IF
        END DO

      ELSE IF (TPROJ(2:3).EQ.'FR') THEN

        DO I=1 , NSLIB
          K = 1
          JJ = 0
          FIREF = 5.
          DO J=1 , LPAFR( ITYPDB(I))
            JJ = JJ + 1
            R = SQRT(CORNDB(1,IPAFR(J,ITYPDB(I)),I)**2 +
     &               CORNDB(2,IPAFR(J,ITYPDB(I)),I)**2 )
            H(JJ)=  R
            V(JJ)=  DATN2D( CORNDB(2,IPAFR(J,ITYPDB(I)),I),
     &                      CORNDB(1,IPAFR(J,ITYPDB(I)),I) )
            V(JJ)=  DFINXT ( FIREF, V(JJ) )
            FIREF=  V(JJ)

            IF (K.LE.2) THEN
              IF (IPAFR(J,ITYPDB(I)).EQ.INSFR(K,ITYPDB(I))) THEN
                DX = CORNDB(1,IPAFR(J+1,ITYPDB(I)),I) -
     &               CORNDB(1,IPAFR(J  ,ITYPDB(I)),I)
                DY = CORNDB(2,IPAFR(J+1,ITYPDB(I)),I) -
     &               CORNDB(2,IPAFR(J  ,ITYPDB(I)),I)
                DO L=1 , NINTP
                  JJ =JJ + 1
                  XIP=CORNDB(1,IPAFR(J,ITYPDB(I)),I)+(L*DX/(NINTP+1))
                  YIP=CORNDB(2,IPAFR(J,ITYPDB(I)),I)+(L*DY/(NINTP+1))
                  R  = SQRT( XIP**2 + YIP**2 )
                  H(JJ) =  R
                  V(JJ) =  DFINXT( FIREF, DATN2D( YIP, XIP))
                  FIREF =  V(JJ)
                END DO
                K = K + 1
              END IF
            END IF
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,3 ) ) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) + 360.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,3 ) ) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) - 720.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,3 ) ) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF
        END DO

      ELSE IF (TPROJ(2:3).EQ.'RZ') THEN

        H(1) = CORNDB(3,3,1)
        H(2) = CORNDB(3,1,1)
        H(3) = CORNDB(3,1,1)
        H(4) = CORNDB(3,3,1)
        H(5) = H(1)
        V(1) = RMNIDB
        V(2) = RMNIDB
        V(3) = RMXIDB
        V(4) = RMXIDB
        V(5) = V(1)
        CALL DQPOL( 5, H, V )

        IF (TPROJ(1:1).EQ.'S') THEN
          DO I=1,5
            V(I) = -V(I)
          END DO
          CALL DQPOL( 5, H, V )
        END IF

      ELSE IF (TPROJ(2:3).EQ.'FT') THEN

        DO I=1 , NSLIB
          K = 1
          JJ = 0
          FIREF = 5.
          DO J=1 , LPAFT( ITYPDB(I))
            JJ = JJ + 1
            R = SQRT(CORNDB(1,IPAFT(J,ITYPDB(I)),I)**2 +
     &               CORNDB(2,IPAFT(J,ITYPDB(I)),I)**2 )
            H(JJ)= -DATN2D( R, CORNDB(3,IPAFT(J,ITYPDB(I)),I) )
            V(JJ)=  DATN2D( CORNDB(2,IPAFT(J,ITYPDB(I)),I),
     &                      CORNDB(1,IPAFT(J,ITYPDB(I)),I) )
            V(JJ)=  DFINXT ( FIREF, V(JJ) )
            FIREF=  V(JJ)

            IF (K.LE.2) THEN
              IF (IPAFT(J,ITYPDB(I)).EQ.INSFT(K,ITYPDB(I))) THEN
                DX = CORNDB(1,IPAFT(J+1,ITYPDB(I)),I) -
     &               CORNDB(1,IPAFT(J  ,ITYPDB(I)),I)
                DY = CORNDB(2,IPAFT(J+1,ITYPDB(I)),I) -
     &               CORNDB(2,IPAFT(J  ,ITYPDB(I)),I)
                DO L=1 , NINTP
                  JJ =JJ + 1
                  XIP=CORNDB(1,IPAFT(J,ITYPDB(I)),I)+(L*DX/(NINTP+1))
                  YIP=CORNDB(2,IPAFT(J,ITYPDB(I)),I)+(L*DY/(NINTP+1))
                  ZIP=CORNDB(3,IPAFT(J,ITYPDB(I)),I)
                  R  = SQRT( XIP**2 + YIP**2 )
                  H(JJ) = -DATN2D( R, ZIP )
                  V(JJ) =  DFINXT( FIREF, DATN2D( YIP, XIP))
                  FIREF =  V(JJ)
                END DO
                K = K + 1
              END IF
            END IF
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,3 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) + 360.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,3 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) - 720.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,3 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF
        END DO

      END IF

      END
C
C  end of DGDMBI
C


*DK DGDMBO
C######################################################################
C
      SUBROUTINE DGDMBO ( TPROJ , NPR, FSLCT, FSLOT, RR )
C
C-----------------------------------------
C
C   Author   :- R.Vogl                21-SEP-1989
C
C=========================================
C
C   Purpose   : draw outer layer of muon-barrel
C   Inputs    : TPROJ : projection to be drawn
C               NINTP : number of interpol. points
C   Outputs   :
C
C=========================================
C +
C Declarations.
C -
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DB
      PARAMETER ( NSLTB = 34 , NSLIB = 17 , NSLOB = 17 )
      PARAMETER ( NMTPB = 10 , NMTIB = 4  , NMTOB = 6  )
      PARAMETER ( NCMXB = 16 )
      COMMON /DBMGEO/  ITYPDB ( NSLTB ) , NCORDB ( NMTPB ) ,
     &                 CORNDB ( 3, NCMXB, NSLTB ),
     &                 RMNIDB, RMXIDB, RMNODB, RMXODB
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77

      CHARACTER *3 TPROJ
      DIMENSION RR(3)
      LOGICAL  FSLCT, FSLOT(38,3)

C
C  pathes for the poly-lines to be drawn
C
C       LPAxx   length of path
C       IPAxx   corner-point number
C       INSxx   corner-points on poly-line after which interp. points are
C               to be inserted
C
      DIMENSION LPAYX ( NMTPB )
      DATA LPAYX / 5,5,5,7,5,5,5,7,5,5 /
      DIMENSION IPAYX ( 7 , NMTPB )
      DATA IPAYX / 1,2,6,5,1,0,0,
     &             1,2,8,7,1,0,0,
     &             1,2,6,5,1,0,0,
     &             2,1,9,12,4,3,11,
     &             1,2,6,5,1,0,0,
     &             1,2,8,7,1,0,0,
     &             1,2,6,5,1,0,0,
     &             2,1,9,12,4,3,11,
     &             1,2,6,5,1,0,0,
     &             1,2,6,5,1,0,0 /
      DIMENSION INSYX ( 2, NMTPB )
      DATA INSYX / 1,6, 1,8, 1,6, 1,12, 1,6, 1,8, 1,6, 1,12, 1,6, 1,6 /

      DIMENSION LPAFT ( NMTPB )
      DATA LPAFT / 5,7,5,9,5,7,5,9,5,5 /
      DIMENSION IPAFT ( 9 , NMTPB )
      DATA IPAFT /  1, 2, 3, 4, 1, 0, 0, 0, 0,
     &              1, 2, 3, 4, 5, 6, 1, 0, 0,
     &              1, 2, 3, 4, 1, 0, 0, 0, 0,
     &              1, 2, 3, 4, 5, 6, 7, 8, 1,
     &              1, 2, 3, 4, 1, 0, 0, 0, 0,
     &              1, 2, 3, 4, 5, 6, 1, 0, 0,
     &              1, 2, 3, 4, 1, 0, 0, 0, 0,
     &              1, 2, 3, 4, 5, 6, 7, 8, 1,
     &              1, 2, 3, 4, 1, 0, 0, 0, 0,
     &              1, 2, 3, 4, 1, 0, 0, 0, 0 /
      DIMENSION INSFT ( 2, NMTPB )
      DATA INSFT / 1,3, 1,0, 1,3, 0,0, 1,3, 1,0, 1,3, 0,0, 1,3, 1,3 /

      DIMENSION LPAFR ( NMTPB )
      DATA LPAFR / 5,5,5,5,5,5,5,5,5,5 /
      DIMENSION IPAFR ( 5 , NMTPB )
      DATA IPAFR / 1,2,6,5,1,
     &             1,2,8,7,1,
     &             1,2,6,5,1,
     &             1,4,12,9,1,
     &             1,2,6,5,1,
     &             1,2,8,7,1,
     &             1,2,6,5,1,
     &             1,4,12,9,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1  /
      DIMENSION INSFR ( 2, NMTPB )
      DATA INSFR / 1,6, 1,8, 1,6, 1,12, 1,6, 1,8, 1,6, 1,12, 1,6, 1,6 /

      DIMENSION H(50),V(50)
C
C  statement-functions
C
C      HPER(X,Y,R) = X*(1+E*R2)/(1+E*R)
C      VPER(X,Y,R) = Y*(1+E*R2)/(1+E*R)
C  perspective parameters
C      R1 = RR(1)
C      R2 = RR(2)
C      R1P= RR(3)
C

      NINTP = NPR

      IF (TPROJ(2:3).EQ.'YX') THEN

        DO I=1+NSLIB , NSLTB
          DO J=1 , LPAYX( ITYPDB(I) )
            H(J) = CORNDB( 1, IPAYX(J,ITYPDB(I) ), I)
            V(J) = CORNDB( 2, IPAYX(J,ITYPDB(I) ), I)
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,3 ) ) THEN
              CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
            END IF
          ELSE
            CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
          END IF
        END DO
      ELSE IF (TPROJ(2:3).EQ.'PP') THEN

C        E = (R1P - R1)/(R1*(R2-R1P))

        DO I=1+NSLIB , NSLTB
          K = 1
          JJ = 0
          DO J=1 , LPAYX( ITYPDB(I))
            JJ = JJ + 1
            H(JJ) = CORNDB( 1, IPAYX(J,ITYPDB(I) ), I)
            V(JJ) = CORNDB( 2, IPAYX(J,ITYPDB(I) ), I)

            IF (K.LE.2) THEN
              IF (IPAYX(J,ITYPDB(I)).EQ.INSYX(K,ITYPDB(I))) THEN
                DX = CORNDB(1,IPAYX(J+1,ITYPDB(I)),I) -
     &               CORNDB(1,IPAYX(J  ,ITYPDB(I)),I)
                DY = CORNDB(2,IPAYX(J+1,ITYPDB(I)),I) -
     &               CORNDB(2,IPAYX(J  ,ITYPDB(I)),I)
                DO L=1 , NINTP
                  JJ =JJ + 1
                  H(JJ)=CORNDB(1,IPAYX(J,ITYPDB(I)),I)+(L*DX/(NINTP+1))
                  V(JJ)=CORNDB(2,IPAYX(J,ITYPDB(I)),I)+(L*DY/(NINTP+1))
                END DO
                K = K + 1
              END IF
            END IF
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,3 ) ) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF
C          IF (FSLCT) THEN
C            IF (FSLOT( I,3 ) ) THEN
C              CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
C            END IF
C          ELSE
C            CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
C          END IF
        END DO
      ELSE IF (TPROJ(2:3).EQ.'FR') THEN

        DO I=1+NSLIB , NSLTB
          K = 1
          JJ = 0
          FIREF = 5.
          DO J=1 , LPAFR( ITYPDB(I))
            JJ = JJ + 1
            R = SQRT(CORNDB(1,IPAFR(J,ITYPDB(I)),I)**2 +
     &               CORNDB(2,IPAFR(J,ITYPDB(I)),I)**2 )
            H(JJ)=  R
            V(JJ)=  DATN2D( CORNDB(2,IPAFR(J,ITYPDB(I)),I),
     &                      CORNDB(1,IPAFR(J,ITYPDB(I)),I) )
            V(JJ)=  DFINXT ( FIREF, V(JJ) )
            FIREF=  V(JJ)

            IF (K.LE.2) THEN
              IF (IPAFR(J,ITYPDB(I)).EQ.INSFR(K,ITYPDB(I))) THEN
                DX = CORNDB(1,IPAFR(J+1,ITYPDB(I)),I) -
     &               CORNDB(1,IPAFR(J  ,ITYPDB(I)),I)
                DY = CORNDB(2,IPAFR(J+1,ITYPDB(I)),I) -
     &               CORNDB(2,IPAFR(J  ,ITYPDB(I)),I)
                DO L=1 , NINTP
                  JJ =JJ + 1
                  XIP=CORNDB(1,IPAFR(J,ITYPDB(I)),I)+(L*DX/(NINTP+1))
                  YIP=CORNDB(2,IPAFR(J,ITYPDB(I)),I)+(L*DY/(NINTP+1))
                  ZIP=CORNDB(3,IPAFR(J,ITYPDB(I)),I)
                  R  = SQRT( XIP**2 + YIP**2 )
                  H(JJ) =  R
                  V(JJ) =  DFINXT( FIREF, DATN2D( YIP, XIP))
                  FIREF =  V(JJ)
                END DO
                K = K + 1
              END IF
            END IF
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,3 ) ) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) + 360.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,3 ) ) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) - 720.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,3 ) ) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF
        END DO

      ELSE IF (TPROJ(2:3).EQ.'RZ') THEN


        H(1) = CORNDB(3,3,19)
        H(2) = CORNDB(3,1,19)
        H(3) = CORNDB(3,1,19)
        H(4) = CORNDB(3,3,19)
        H(5) = H(1)
        V(1) = RMNODB
        V(2) = RMNODB
        V(3) = RMXODB
        V(4) = RMXODB
        V(5) = V(1)
        CALL DQPOL( 5, H, V )

        IF (TPROJ(1:1).EQ.'S') THEN
          DO I=1,5
            V(I) = -V(I)
          END DO
          CALL DQPOL( 5, H, V )
        END IF

      ELSE IF (TPROJ(2:3).EQ.'FT') THEN

        DO I=1+NSLIB , NSLTB
          K = 1
          JJ = 0
          FIREF = 5.
          DO J=1 , LPAFT( ITYPDB(I))
            JJ = JJ + 1
            R = SQRT(CORNDB(1,IPAFT(J,ITYPDB(I)),I)**2 +
     &               CORNDB(2,IPAFT(J,ITYPDB(I)),I)**2 )
            H(JJ)= -DATN2D( R, CORNDB(3,IPAFT(J,ITYPDB(I)),I) )
            V(JJ)=  DATN2D( CORNDB(2,IPAFT(J,ITYPDB(I)),I),
     &                      CORNDB(1,IPAFT(J,ITYPDB(I)),I) )
            V(JJ)=  DFINXT ( FIREF, V(JJ) )
            FIREF=  V(JJ)

            IF (K.LE.2) THEN
              IF (IPAFT(J,ITYPDB(I)).EQ.INSFT(K,ITYPDB(I))) THEN
                DX = CORNDB(1,IPAFT(J+1,ITYPDB(I)),I) -
     &               CORNDB(1,IPAFT(J  ,ITYPDB(I)),I)
                DY = CORNDB(2,IPAFT(J+1,ITYPDB(I)),I) -
     &               CORNDB(2,IPAFT(J  ,ITYPDB(I)),I)
                DO L=1 , NINTP
                  JJ =JJ + 1
                  XIP=CORNDB(1,IPAFT(J,ITYPDB(I)),I)+(L*DX/(NINTP+1))
                  YIP=CORNDB(2,IPAFT(J,ITYPDB(I)),I)+(L*DY/(NINTP+1))
                  ZIP=CORNDB(3,IPAFT(J,ITYPDB(I)),I)
                  R  = SQRT( XIP**2 + YIP**2 )
                  H(JJ) = -DATN2D( R, ZIP )
                  V(JJ) =  DFINXT( FIREF, DATN2D( YIP, XIP))
                  FIREF =  V(JJ)
                END DO
                K = K + 1
              END IF
            END IF
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,3 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) + 360.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,3 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) - 720.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,3 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF
        END DO

      END IF


      END
C
C   end of DGDMBO
C


*DK DGDMMI
C######################################################################
C
      SUBROUTINE DGDMMI ( TPROJ , NPR, FSLCT, FSLOT, RR )
C
C-----------------------------------------
C
C   Author   :- R.Vogl                21-SEP-1989
C
C=========================================
C
C   Purpose   : draw inner layer of muon-middle-angle
C   Inputs    : TPROJ : projection to be drawn
C               NINTP : number of interpol. points
C   Outputs   :
C
C=========================================
C +
C Declarations.
C -
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DM
      PARAMETER ( NSLTM = 38 )
      PARAMETER ( NSLHM = 19 )
      PARAMETER ( NSLIM = 10 , NSLOM = 9 )
      PARAMETER ( NMTPM = 9  )
      PARAMETER ( NCMXM = 8  )
      COMMON /DMMGEO/  ITYPDM ( NSLTM ) , NCORDM ( NMTPM ) ,
     &                 CORNDM ( 3, NCMXM , NSLTM ),
     &                 RMNIDM, RMXIDM, RMNODM, RMXODM
C------------------------------------------------------------------- DB
      PARAMETER ( NSLTB = 34 , NSLIB = 17 , NSLOB = 17 )
      PARAMETER ( NMTPB = 10 , NMTIB = 4  , NMTOB = 6  )
      PARAMETER ( NCMXB = 16 )
      COMMON /DBMGEO/  ITYPDB ( NSLTB ) , NCORDB ( NMTPB ) ,
     &                 CORNDB ( 3, NCMXB, NSLTB ),
     &                 RMNIDB, RMXIDB, RMNODB, RMXODB
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77


      CHARACTER *3 TPROJ
      DIMENSION RR(3)
      LOGICAL  FSLCT, FSLOT(38,3)

C
C  pathes for the poly-lines to be drawn
C
C       LPAxx   length of path
C       IPAxx   corner-point number
C       INSxx   corner-points on poly-line after which interp. points are
C               to be inserted
C
      DIMENSION LPAYX ( NMTPM )
      DATA LPAYX / 5,5,5,5,5,5,5,5,5 /
      DIMENSION IPAYX ( 5 , NMTPM )
      DATA IPAYX / 1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1 /
      DIMENSION INSYX ( 2, NMTPM )
      DATA INSYX / 1,6, 1,6, 1,6, 1,6, 1,6, 1,6, 1,6, 1,6, 1,6 /

      DIMENSION LPAFT ( NMTPM )
      DATA LPAFT / 5,5,5,5,5,5,5,5,5 /
      DIMENSION IPAFT ( 5 , NMTPM )
      DATA IPAFT /  1, 2, 3, 4, 1,
     &              1, 2, 3, 4, 1,
     &              1, 2, 3, 4, 1,
     &              1, 2, 3, 4, 1,
     &              1, 2, 3, 4, 1,
     &              1, 2, 3, 4, 1,
     &              1, 2, 3, 4, 1,
     &              1, 2, 3, 4, 1,
     &              1, 2, 3, 4, 1 /
      DIMENSION INSFT ( 2, NMTPM )
      DATA INSFT / 1,3, 1,3, 1,3, 1,3, 1,3, 1,3, 1,3, 1,3, 1,3 /

      DIMENSION LPAFR ( NMTPM )
      DATA LPAFR / 5,5,5,5,5,5,5,5,5 /
      DIMENSION IPAFR ( 5 , NMTPM )
      DATA IPAFR / 1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1 /
      DIMENSION INSFR ( 2, NMTPM )
      DATA INSFR / 1,6, 1,6, 1,6, 1,6, 1,6, 1,6, 1,6, 1,6, 1,6 /

      DIMENSION H(50),V(50)
C
C  statement-functions
C
C      HPER(X,Y,R) = X*(1+E*R2)/(1+E*R)
C      VPER(X,Y,R) = Y*(1+E*R2)/(1+E*R)
C  perspective parameters
C      R1 = RR(1)
C      R2 = RR(2)
C      R1P= RR(3)
C

      NINTP = NPR

      IF (TPROJ(2:3).EQ.'YX') THEN

        DO I=1 , NSLIM
          DO J=1 , LPAYX( ITYPDM(I) )
            H(J) = CORNDM( 1, IPAYX(J,ITYPDM(I) ), I)
            V(J) = CORNDM( 2, IPAYX(J,ITYPDM(I) ), I)
          END DO
          IF (FSLCT) THEN
            IF (FSLOT(I,2).OR.FSLOT(I+NSLHM,2)) THEN
              CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
            END IF
          ELSE
            CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
          END IF
        END DO
      ELSE IF (TPROJ(2:3).EQ.'PP') THEN

C        E = (R1P - R1)/(R1*(R2-R1P))

        DO I=1 , NSLIM
          K = 1
          JJ = 0
          DO J=1 , LPAYX( ITYPDM(I))
            JJ = JJ + 1
            H(JJ) = CORNDM( 1, IPAYX(J,ITYPDM(I) ), I)
            V(JJ) = CORNDM( 2, IPAYX(J,ITYPDM(I) ), I)

            IF (K.LE.2) THEN
              IF (IPAYX(J,ITYPDM(I)).EQ.INSYX(K,ITYPDM(I))) THEN
                DX = CORNDM(1,IPAYX(J+1,ITYPDM(I)),I) -
     &               CORNDM(1,IPAYX(J  ,ITYPDM(I)),I)
                DY = CORNDM(2,IPAYX(J+1,ITYPDM(I)),I) -
     &               CORNDM(2,IPAYX(J  ,ITYPDM(I)),I)
                DO L=1 , NINTP
                  JJ =JJ + 1
                  H(JJ)=CORNDM(1,IPAYX(J,ITYPDM(I)),I)+(L*DX/(NINTP+1))
                  V(JJ)=CORNDM(2,IPAYX(J,ITYPDM(I)),I)+(L*DY/(NINTP+1))
                END DO
                K = K + 1
              END IF
            END IF
          END DO
          IF (FSLCT) THEN
            IF (FSLOT(I,2).OR.FSLOT(I+NSLHM,2)) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF
C          IF (FSLCT) THEN
C            IF (FSLOT(I,2).OR.FSLOT(I+NSLHM,2)) THEN
C              CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
C            END IF
C          ELSE
C            CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
C          END IF
        END DO
      ELSE IF (TPROJ(2:3).EQ.'FR') THEN

        DO I=1 , NSLIM
          K = 1
          JJ = 0
          FIREF = 5.
          DO J=1 , LPAFR( ITYPDM(I))
            JJ = JJ + 1
            R = SQRT(CORNDM(1,IPAFR(J,ITYPDM(I)),I)**2 +
     &               CORNDM(2,IPAFR(J,ITYPDM(I)),I)**2 )
            H(JJ)=  R
            V(JJ)=  DATN2D( CORNDM(2,IPAFR(J,ITYPDM(I)),I),
     &                      CORNDM(1,IPAFR(J,ITYPDM(I)),I) )
            V(JJ)=  DFINXT ( FIREF, V(JJ) )
            FIREF=  V(JJ)

            IF (K.LE.2) THEN
              IF (IPAFR(J,ITYPDM(I)).EQ.INSFR(K,ITYPDM(I))) THEN
                DX = CORNDM(1,IPAFR(J+1,ITYPDM(I)),I) -
     &               CORNDM(1,IPAFR(J  ,ITYPDM(I)),I)
                DY = CORNDM(2,IPAFR(J+1,ITYPDM(I)),I) -
     &               CORNDM(2,IPAFR(J  ,ITYPDM(I)),I)
                DO L=1 , NINTP
                  JJ =JJ + 1
                  XIP=CORNDM(1,IPAFR(J,ITYPDM(I)),I)+(L*DX/(NINTP+1))
                  YIP=CORNDM(2,IPAFR(J,ITYPDM(I)),I)+(L*DY/(NINTP+1))
                  ZIP=CORNDM(3,IPAFR(J,ITYPDM(I)),I)
                  R  = SQRT( XIP**2 + YIP**2 )
                  H(JJ) =  R
                  V(JJ) =  DFINXT( FIREF, DATN2D( YIP, XIP))
                  FIREF =  V(JJ)
                END DO
                K = K + 1
              END IF
            END IF
          END DO
          IF (FSLCT) THEN
            IF (FSLOT(I,2).OR.FSLOT(I+NSLHM,2)) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) + 360.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT(I,2).OR.FSLOT(I+NSLHM,2)) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) - 720.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT(I,2).OR.FSLOT(I+NSLHM,2)) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF
        END DO

      ELSE IF (TPROJ(2:3).EQ.'RZ') THEN

        H(1) = CORNDM(3,3,2)
        H(2) = CORNDM(3,1,2)
        H(3) = CORNDM(3,1,2)
        H(4) = CORNDM(3,3,2)
        H(5) = H(1)
        V(1) = RMNIDM
        V(2) = RMNIDM
        V(3) = RMXIDM
        V(4) = RMXIDM
        V(5) = V(1)
        CALL DQPOL( 5, H, V )
        IF (TPROJ(1:1).EQ.'S') THEN
          DO I=1,5
            V(I) = -V(I)
          END DO
          CALL DQPOL( 5, H, V )
        END IF

        H(1) = CORNDM(3,3,21)
        H(2) = CORNDM(3,1,21)
        H(3) = CORNDM(3,1,21)
        H(4) = CORNDM(3,3,21)
        H(5) = H(1)
        V(1) = RMNIDM
        V(2) = RMNIDM
        V(3) = RMXIDM
        V(4) = RMXIDM
        V(5) = V(1)
        CALL DQPOL( 5, H, V )
        IF (TPROJ(1:1).EQ.'S') THEN
          DO I=1,5
            V(I) = -V(I)
          END DO
          CALL DQPOL( 5, H, V )
        END IF

      ELSE IF (TPROJ(2:3).EQ.'FT') THEN
C  middle-angle slots z>0
        DO I=1 , NSLIM
          K = 1
          JJ = 0
          FIREF = 5.
          DO J=1 , LPAFT( ITYPDM(I))
            JJ = JJ + 1
            R = SQRT(CORNDM(1,IPAFT(J,ITYPDM(I)),I)**2 +
     &               CORNDM(2,IPAFT(J,ITYPDM(I)),I)**2 )
            H(JJ)= -DATN2D( R, CORNDM(3,IPAFT(J,ITYPDM(I)),I) )
            V(JJ)=  DATN2D( CORNDM(2,IPAFT(J,ITYPDM(I)),I),
     &                      CORNDM(1,IPAFT(J,ITYPDM(I)),I) )
            V(JJ)=  DFINXT ( FIREF, V(JJ) )
            FIREF=  V(JJ)

            IF (K.LE.2) THEN
              IF (IPAFT(J,ITYPDM(I)).EQ.INSFT(K,ITYPDM(I))) THEN
                DX = CORNDM(1,IPAFT(J+1,ITYPDM(I)),I) -
     &               CORNDM(1,IPAFT(J  ,ITYPDM(I)),I)
                DY = CORNDM(2,IPAFT(J+1,ITYPDM(I)),I) -
     &               CORNDM(2,IPAFT(J  ,ITYPDM(I)),I)
                DO L=1 , NINTP
                  JJ =JJ + 1
                  XIP=CORNDM(1,IPAFT(J,ITYPDM(I)),I)+(L*DX/(NINTP+1))
                  YIP=CORNDM(2,IPAFT(J,ITYPDM(I)),I)+(L*DY/(NINTP+1))
                  ZIP=CORNDM(3,IPAFT(J,ITYPDM(I)),I)
                  R  = SQRT( XIP**2 + YIP**2 )
                  H(JJ) = -DATN2D( R, ZIP )
                  V(JJ) =  DFINXT( FIREF, DATN2D( YIP, XIP))
                  FIREF =  V(JJ)
                END DO
                K = K + 1
              END IF
            END IF
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,2 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) + 360.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,2 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) - 720.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,2 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF
        END DO
C  middle-angle slots z<0
        DO I=1+NSLHM , NSLHM+NSLIM
          K = 1
          JJ = 0
          FIREF = 5.
          DO J=1 , LPAFT( ITYPDM(I))
            JJ = JJ + 1
            R = SQRT(CORNDM(1,IPAFT(J,ITYPDM(I)),I)**2 +
     &               CORNDM(2,IPAFT(J,ITYPDM(I)),I)**2 )
            H(JJ)= -DATN2D( R, CORNDM(3,IPAFT(J,ITYPDM(I)),I) )
            V(JJ)=  DATN2D( CORNDM(2,IPAFT(J,ITYPDM(I)),I),
     &                      CORNDM(1,IPAFT(J,ITYPDM(I)),I) )
            V(JJ)=  DFINXT ( FIREF, V(JJ) )
            FIREF=  V(JJ)

            IF (K.LE.2) THEN
              IF (IPAFT(J,ITYPDM(I)).EQ.INSFT(K,ITYPDM(I))) THEN
                DX = CORNDM(1,IPAFT(J+1,ITYPDM(I)),I) -
     &               CORNDM(1,IPAFT(J  ,ITYPDM(I)),I)
                DY = CORNDM(2,IPAFT(J+1,ITYPDM(I)),I) -
     &               CORNDM(2,IPAFT(J  ,ITYPDM(I)),I)
                DO L=1 , NINTP
                  JJ =JJ + 1
                  XIP=CORNDM(1,IPAFT(J,ITYPDM(I)),I)+(L*DX/(NINTP+1))
                  YIP=CORNDM(2,IPAFT(J,ITYPDM(I)),I)+(L*DY/(NINTP+1))
                  ZIP=CORNDM(3,IPAFT(J,ITYPDM(I)),I)
                  R  = SQRT( XIP**2 + YIP**2 )
                  H(JJ) = -DATN2D( R, ZIP )
                  V(JJ) =  DFINXT( FIREF, DATN2D( YIP, XIP))
                  FIREF =  V(JJ)
                END DO
                K = K + 1
              END IF
            END IF
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,2 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) + 360.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,2 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) - 720.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,2 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF
        END DO

      END IF


      END
C
C  end of DGDMMI
C


*DK DGDMMO
C######################################################################
C
      SUBROUTINE DGDMMO ( TPROJ , NPR, FSLCT, FSLOT, RR )
C
C-----------------------------------------
C
C   Author   :- R.Vogl                21-SEP-1989
C
C=========================================
C
C   Purpose   : draw inner layer of muon-middle-angle
C   Inputs    : TPROJ : projection to be drawn
C               NINTP : number of interpol. points
C   Outputs   :
C
C=========================================
C +
C Declarations.
C -
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DM
      PARAMETER ( NSLTM = 38 )
      PARAMETER ( NSLHM = 19 )
      PARAMETER ( NSLIM = 10 , NSLOM = 9 )
      PARAMETER ( NMTPM = 9  )
      PARAMETER ( NCMXM = 8  )
      COMMON /DMMGEO/  ITYPDM ( NSLTM ) , NCORDM ( NMTPM ) ,
     &                 CORNDM ( 3, NCMXM , NSLTM ),
     &                 RMNIDM, RMXIDM, RMNODM, RMXODM
C------------------------------------------------------------------- DB
      PARAMETER ( NSLTB = 34 , NSLIB = 17 , NSLOB = 17 )
      PARAMETER ( NMTPB = 10 , NMTIB = 4  , NMTOB = 6  )
      PARAMETER ( NCMXB = 16 )
      COMMON /DBMGEO/  ITYPDB ( NSLTB ) , NCORDB ( NMTPB ) ,
     &                 CORNDB ( 3, NCMXB, NSLTB ),
     &                 RMNIDB, RMXIDB, RMNODB, RMXODB
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77


      CHARACTER *3 TPROJ
      DIMENSION RR(3)
      LOGICAL  FSLCT, FSLOT(38,3)

C
C  pathes for the poly-lines to be drawn
C
C       LPAxx   length of path
C       IPAxx   corner-point number
C       INSxx   corner-points on poly-line after which interp. points are
C               to be inserted
C
      DIMENSION LPAYX ( NMTPM )
      DATA LPAYX / 5,5,5,5,5,5,5,5,5 /
      DIMENSION IPAYX ( 5 , NMTPM )
      DATA IPAYX / 1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1 /
      DIMENSION INSYX ( 2, NMTPM )
      DATA INSYX / 1,6, 1,6, 1,6, 1,6, 1,6, 1,6, 1,6, 1,6, 1,6 /

      DIMENSION LPAFT ( NMTPM )
      DATA LPAFT / 5,5,5,5,5,5,5,5,5 /
      DIMENSION IPAFT ( 5 , NMTPM )
      DATA IPAFT /  1, 2, 3, 4, 1,
     &              1, 2, 3, 4, 1,
     &              1, 2, 3, 4, 1,
     &              1, 2, 3, 4, 1,
     &              1, 2, 3, 4, 1,
     &              1, 2, 3, 4, 1,
     &              1, 2, 3, 4, 1,
     &              1, 2, 3, 4, 1,
     &              1, 2, 3, 4, 1 /
      DIMENSION INSFT ( 2, NMTPM )
      DATA INSFT / 1,3, 1,3, 1,3, 1,3, 1,3, 1,3, 1,3, 1,3, 1,3 /

      DIMENSION LPAFR ( NMTPM )
      DATA LPAFR / 5,5,5,5,5,5,5,5,5 /
      DIMENSION IPAFR ( 5 , NMTPM )
      DATA IPAFR / 1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1,
     &             1,2,6,5,1 /
      DIMENSION INSFR ( 2, NMTPM )
      DATA INSFR / 1,6, 1,6, 1,6, 1,6, 1,6, 1,6, 1,6, 1,6, 1,6 /

      DIMENSION H(50),V(50)
C
C  statement-functions
C
C      HPER(X,Y,R) = X*(1+E*R2)/(1+E*R)
C      VPER(X,Y,R) = Y*(1+E*R2)/(1+E*R)
C  perspective parameters
C      R1 = RR(1)
C      R2 = RR(2)
C      R1P= RR(3)
C

      NINTP = NPR

      IF (TPROJ(2:3).EQ.'YX') THEN

        DO I=1+NSLIM , NSLHM
          DO J=1 , LPAYX( ITYPDM(I) )
            H(J) = CORNDM( 1, IPAYX(J,ITYPDM(I) ), I)
            V(J) = CORNDM( 2, IPAYX(J,ITYPDM(I) ), I)
          END DO
          IF (FSLCT) THEN
            IF (FSLOT(I,2).OR.FSLOT(I+NSLHM,2)) THEN
              CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
            END IF
          ELSE
            CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
          END IF
        END DO
      ELSE IF (TPROJ(2:3).EQ.'PP') THEN

C        E = (R1P - R1)/(R1*(R2-R1P))

        DO I=1+NSLIM , NSLHM
          K = 1
          JJ = 0
          DO J=1 , LPAYX( ITYPDM(I))
            JJ = JJ + 1
            H(JJ) = CORNDM( 1, IPAYX(J,ITYPDM(I) ), I)
            V(JJ) = CORNDM( 2, IPAYX(J,ITYPDM(I) ), I)

            IF (K.LE.2) THEN
              IF (IPAYX(J,ITYPDM(I)).EQ.INSYX(K,ITYPDM(I))) THEN
                DX = CORNDM(1,IPAYX(J+1,ITYPDM(I)),I) -
     &               CORNDM(1,IPAYX(J  ,ITYPDM(I)),I)
                DY = CORNDM(2,IPAYX(J+1,ITYPDM(I)),I) -
     &               CORNDM(2,IPAYX(J  ,ITYPDM(I)),I)
                DO L=1 , NINTP
                  JJ =JJ + 1
                  H(JJ)=CORNDM(1,IPAYX(J,ITYPDM(I)),I)+(L*DX/(NINTP+1))
                  V(JJ)=CORNDM(2,IPAYX(J,ITYPDM(I)),I)+(L*DY/(NINTP+1))
                END DO
                K = K + 1
              END IF
            END IF
          END DO
          IF (FSLCT) THEN
            IF (FSLOT(I,2).OR.FSLOT(I+NSLHM,2)) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF
C          IF (FSLCT) THEN
C            IF (FSLOT(I,2).OR.FSLOT(I+NSLHM,2)) THEN
C              CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
C            END IF
C          ELSE
C            CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
C          END IF
        END DO

      ELSE IF (TPROJ(2:3).EQ.'FR') THEN

        DO I=1+NSLIM , NSLHM
          K = 1
          JJ = 0
          FIREF = 5.
          DO J=1 , LPAFR( ITYPDM(I))
            JJ = JJ + 1
            R = SQRT(CORNDM(1,IPAFR(J,ITYPDM(I)),I)**2 +
     &               CORNDM(2,IPAFR(J,ITYPDM(I)),I)**2 )
            H(JJ)=  R
            V(JJ)=  DATN2D( CORNDM(2,IPAFR(J,ITYPDM(I)),I),
     &                      CORNDM(1,IPAFR(J,ITYPDM(I)),I) )
            V(JJ)=  DFINXT ( FIREF, V(JJ) )
            FIREF=  V(JJ)

            IF (K.LE.2) THEN
              IF (IPAFR(J,ITYPDM(I)).EQ.INSFR(K,ITYPDM(I))) THEN
                DX = CORNDM(1,IPAFR(J+1,ITYPDM(I)),I) -
     &               CORNDM(1,IPAFR(J  ,ITYPDM(I)),I)
                DY = CORNDM(2,IPAFR(J+1,ITYPDM(I)),I) -
     &               CORNDM(2,IPAFR(J  ,ITYPDM(I)),I)
                DO L=1 , NINTP
                  JJ =JJ + 1
                  XIP=CORNDM(1,IPAFR(J,ITYPDM(I)),I)+(L*DX/(NINTP+1))
                  YIP=CORNDM(2,IPAFR(J,ITYPDM(I)),I)+(L*DY/(NINTP+1))
                  ZIP=CORNDM(3,IPAFR(J,ITYPDM(I)),I)
                  R  = SQRT( XIP**2 + YIP**2 )
                  H(JJ) =  R
                  V(JJ) =  DFINXT( FIREF, DATN2D( YIP, XIP))
                  FIREF =  V(JJ)
                END DO
                K = K + 1
              END IF
            END IF
          END DO
          IF (FSLCT) THEN
            IF (FSLOT(I,2).OR.FSLOT(I+NSLHM,2)) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) + 360.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT(I,2).OR.FSLOT(I+NSLHM,2)) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) - 720.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT(I,2).OR.FSLOT(I+NSLHM,2)) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF
        END DO

      ELSE IF (TPROJ(2:3).EQ.'RZ') THEN

        H(1) = CORNDM(3,3,12)
        H(2) = CORNDM(3,1,12)
        H(3) = CORNDM(3,1,12)
        H(4) = CORNDM(3,3,12)
        H(5) = H(1)
        V(1) = RMNODM
        V(2) = RMNODM
        V(3) = RMXODM
        V(4) = RMXODM
        V(5) = V(1)
        CALL DQPOL( 5, H, V )
        IF (TPROJ(1:1).EQ.'S') THEN
          DO I=1,5
            V(I) = -V(I)
          END DO
          CALL DQPOL( 5, H, V )
        END IF

        H(1) = CORNDM(3,3,31)
        H(2) = CORNDM(3,1,31)
        H(3) = CORNDM(3,1,31)
        H(4) = CORNDM(3,3,31)
        H(5) = H(1)
        V(1) = RMNODM
        V(2) = RMNODM
        V(3) = RMXODM
        V(4) = RMXODM
        V(5) = V(1)
        CALL DQPOL( 5, H, V )
        IF (TPROJ(1:1).EQ.'S') THEN
          DO I=1,5
            V(I) = -V(I)
          END DO
          CALL DQPOL( 5, H, V )
        END IF

      ELSE IF (TPROJ(2:3).EQ.'FT') THEN
C   middle-angle slots with z>0
        DO I=1+NSLIM , NSLHM
          K = 1
          JJ = 0
          FIREF = 5.
          DO J=1 , LPAFT( ITYPDM(I))
            JJ = JJ + 1
            R = SQRT(CORNDM(1,IPAFT(J,ITYPDM(I)),I)**2 +
     &               CORNDM(2,IPAFT(J,ITYPDM(I)),I)**2 )
            H(JJ)= -DATN2D( R, CORNDM(3,IPAFT(J,ITYPDM(I)),I) )
            V(JJ)=  DATN2D( CORNDM(2,IPAFT(J,ITYPDM(I)),I),
     &                      CORNDM(1,IPAFT(J,ITYPDM(I)),I) )
            V(JJ)=  DFINXT ( FIREF, V(JJ) )
            FIREF=  V(JJ)

            IF (K.LE.2) THEN
              IF (IPAFT(J,ITYPDM(I)).EQ.INSFT(K,ITYPDM(I))) THEN
                DX = CORNDM(1,IPAFT(J+1,ITYPDM(I)),I) -
     &               CORNDM(1,IPAFT(J  ,ITYPDM(I)),I)
                DY = CORNDM(2,IPAFT(J+1,ITYPDM(I)),I) -
     &               CORNDM(2,IPAFT(J  ,ITYPDM(I)),I)
                DO L=1 , NINTP
                  JJ =JJ + 1
                  XIP=CORNDM(1,IPAFT(J,ITYPDM(I)),I)+(L*DX/(NINTP+1))
                  YIP=CORNDM(2,IPAFT(J,ITYPDM(I)),I)+(L*DY/(NINTP+1))
                  ZIP=CORNDM(3,IPAFT(J,ITYPDM(I)),I)
                  R  = SQRT( XIP**2 + YIP**2 )
                  H(JJ) = -DATN2D( R, ZIP )
                  V(JJ) =  DFINXT( FIREF, DATN2D( YIP, XIP))
                  FIREF =  V(JJ)
                END DO
                K = K + 1
              END IF
            END IF
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I ,2 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) + 360.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,2  )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) - 720.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I, 2 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF
        END DO
C  middle-angle slots with z<0
        DO I=1+NSLIM+NSLHM , NSLTM
          K = 1
          JJ = 0
          FIREF = 5.
          DO J=1 , LPAFT( ITYPDM(I))
            JJ = JJ + 1
            R = SQRT(CORNDM(1,IPAFT(J,ITYPDM(I)),I)**2 +
     &               CORNDM(2,IPAFT(J,ITYPDM(I)),I)**2 )
            H(JJ)= -DATN2D( R, CORNDM(3,IPAFT(J,ITYPDM(I)),I) )
            V(JJ)=  DATN2D( CORNDM(2,IPAFT(J,ITYPDM(I)),I),
     &                      CORNDM(1,IPAFT(J,ITYPDM(I)),I) )
            V(JJ)=  DFINXT ( FIREF, V(JJ) )
            FIREF=  V(JJ)

            IF (K.LE.2) THEN
              IF (IPAFT(J,ITYPDM(I)).EQ.INSFT(K,ITYPDM(I))) THEN
                DX = CORNDM(1,IPAFT(J+1,ITYPDM(I)),I) -
     &               CORNDM(1,IPAFT(J  ,ITYPDM(I)),I)
                DY = CORNDM(2,IPAFT(J+1,ITYPDM(I)),I) -
     &               CORNDM(2,IPAFT(J  ,ITYPDM(I)),I)
                DO L=1 , NINTP
                  JJ =JJ + 1
                  XIP=CORNDM(1,IPAFT(J,ITYPDM(I)),I)+(L*DX/(NINTP+1))
                  YIP=CORNDM(2,IPAFT(J,ITYPDM(I)),I)+(L*DY/(NINTP+1))
                  ZIP=CORNDM(3,IPAFT(J,ITYPDM(I)),I)
                  R  = SQRT( XIP**2 + YIP**2 )
                  H(JJ) = -DATN2D( R, ZIP )
                  V(JJ) =  DFINXT( FIREF, DATN2D( YIP, XIP))
                  FIREF =  V(JJ)
                END DO
                K = K + 1
              END IF
            END IF
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,2  )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) + 360.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,2  )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) - 720.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,2  )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF
        END DO

      END IF


      END
C
C  end of DGDMMO
C

*DK DGDMEI
C######################################################################
C
      SUBROUTINE DGDMEI ( TPROJ , NPR, FSLCT, FSLOT )
C
C-----------------------------------------
C
C   Author   :- R.Vogl                21-SEP-1989
C
C=========================================
C
C   Purpose   : draw inner layer of muon-end-caps
C   Inputs    : TPROJ : projection to be drawn
C               NINTP : number of interpol. points
C   Outputs   :
C
C=========================================
C +
C Declarations.
C -
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DE
      PARAMETER ( NSLTE = 16 )
      PARAMETER ( NSLHE =  8 )
      PARAMETER ( NSLIE =  4 , NSLOE =  4 )
      PARAMETER ( NMTPE =  8 )
      PARAMETER ( NCMXE =  8 )
      COMMON /DEMGEO/  ITYPDE ( NSLTE ) , NCORDE ( NMTPE ) ,
     &                 CORNDE ( 3, NCMXE , NSLTE ),
     &                 RMNIDE, RMXIDE, RMNODE, RMXODE
C------------------------------------------------------------------- DB
      PARAMETER ( NSLTB = 34 , NSLIB = 17 , NSLOB = 17 )
      PARAMETER ( NMTPB = 10 , NMTIB = 4  , NMTOB = 6  )
      PARAMETER ( NCMXB = 16 )
      COMMON /DBMGEO/  ITYPDB ( NSLTB ) , NCORDB ( NMTPB ) ,
     &                 CORNDB ( 3, NCMXB, NSLTB ),
     &                 RMNIDB, RMXIDB, RMNODB, RMXODB
C------------------------------------------------------------------- DM
      PARAMETER ( NSLTM = 38 )
      PARAMETER ( NSLHM = 19 )
      PARAMETER ( NSLIM = 10 , NSLOM = 9 )
      PARAMETER ( NMTPM = 9  )
      PARAMETER ( NCMXM = 8  )
      COMMON /DMMGEO/  ITYPDM ( NSLTM ) , NCORDM ( NMTPM ) ,
     &                 CORNDM ( 3, NCMXM , NSLTM ),
     &                 RMNIDM, RMXIDM, RMNODM, RMXODM
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77


      CHARACTER *3 TPROJ
      LOGICAL  FSLCT, FSLOT(38,3)

C
C  pathes for the poly-lines to be drawn
C
C       LPAxx   length of path
C       IPAxx   corner-point number
C       INSxx   corner-points on poly-line after which interp. points are
C               to be inserted
C
      DIMENSION LPAYX ( NMTPE )
      DATA LPAYX / 5,5,5,5,5,5,5,5 /
      DIMENSION IPAYX ( 5 , NMTPE )
      DATA IPAYX / 1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1 /

      DIMENSION LPAFT ( NMTPE )
      DATA LPAFT / 5,5,5,5,5,5,5,5 /
      DIMENSION IPAFT ( 5 , NMTPE )
      DATA IPAFT / 1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1 /
      DIMENSION INSFT ( 4, NMTPE )
      DATA INSFT / 1,2,3,4, 1,2,3,4, 1,2,3,4, 1,2,3,4,
     &             1,2,3,4, 1,2,3,4, 1,2,3,4, 1,2,3,4 /

      DIMENSION H(100),V(100)


      NINTP = IDMIX(10,5*NPR,17)

      IF (TPROJ(2:3).EQ.'YX') THEN

        DO I=1 , NSLIE
          DO J=1 , LPAYX( ITYPDE(I) )
            H(J) = CORNDE( 1, IPAYX(J,ITYPDE(I) ), I)
            V(J) = CORNDE( 2, IPAYX(J,ITYPDE(I) ), I)
          END DO
          IF (FSLCT) THEN
            IF (FSLOT(I,1).OR.FSLOT(I+NSLHE,1)) THEN
              CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
            END IF
          ELSE
            CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
          END IF
        END DO

      ELSE IF (TPROJ(2:3).EQ.'YZ') THEN

        H(1) = CORNDE(3,5,4)
        H(2) = CORNDE(3,3,1)
        H(3) = CORNDE(3,3,1)
        H(4) = CORNDE(3,5,4)
        H(5) = H(1)
        V(1) = -RMXIDM
        V(2) = -RMXIDM
        V(3) =  RMXIDM
        V(4) =  RMXIDM
        V(5) = V(1)
        CALL DQPOL( 5, H, V )

        H(1) = CORNDE(3,5,12)
        H(2) = CORNDE(3,3,9)
        H(3) = CORNDE(3,3,9)
        H(4) = CORNDE(3,5,12)
        H(5) = H(1)
        V(1) = -RMXIDM
        V(2) = -RMXIDM
        V(3) =  RMXIDM
        V(4) =  RMXIDM
        V(5) = V(1)
        CALL DQPOL( 5, H, V )

      ELSE IF (TPROJ(2:3).EQ.'FZ') THEN

        H(1) = CORNDE(3,5,1)
        H(2) = CORNDE(3,1,1)
        H(3) = CORNDE(3,1,1)
        H(4) = CORNDE(3,5,1)
        H(5) = H(1)
        V(1) = -60.
        V(2) = -60.
        V(3) =  540.
        V(4) =  540.
        V(5) = V(1)
        CALL DQPOL( 5, H, V )
        H(1) = CORNDE(3,5,9)
        H(2) = CORNDE(3,1,9)
        H(3) = CORNDE(3,1,9)
        H(4) = CORNDE(3,5,9)
        H(5) = H(1)
        V(1) = -60.
        V(2) = -60.
        V(3) =  540.
        V(4) =  540.
        V(5) = V(1)
        CALL DQPOL( 5, H, V )

      ELSE IF (TPROJ(2:3).EQ.'RZ') THEN

        H(1) = CORNDE(3,5,1)
        H(2) = CORNDE(3,1,1)
        H(3) = CORNDE(3,1,1)
        H(4) = CORNDE(3,5,1)
        H(5) = H(1)
        V(1) = RMNIDE
        V(2) = RMNIDE
        V(3) = RMXIDM
        V(4) = RMXIDM
        V(5) = V(1)
        CALL DQPOL( 5, H, V )
        IF (TPROJ(1:1).EQ.'S') THEN
          DO I=1,5
            V(I) = -V(I)
          END DO
          CALL DQPOL( 5, H, V )
        END IF

        H(1) = CORNDE(3,5,9)
        H(2) = CORNDE(3,1,9)
        H(3) = CORNDE(3,1,9)
        H(4) = CORNDE(3,5,9)
        H(5) = H(1)
        V(1) = RMNIDE
        V(2) = RMNIDE
        V(3) = RMXIDM
        V(4) = RMXIDM
        V(5) = V(1)
        CALL DQPOL( 5, H, V )
        IF (TPROJ(1:1).EQ.'S') THEN
          DO I=1,5
            V(I) = -V(I)
          END DO
          CALL DQPOL( 5, H, V )
        END IF

      ELSE IF (TPROJ(2:3).EQ.'FT') THEN
C   middle-angle slots with z>0
        DO I=1 , NSLIE
          K = 1
          JJ = 0
          FIREF = 5.
          DO J=1 , LPAFT( ITYPDE(I))
            JJ = JJ + 1
            R = SQRT(CORNDE(1,IPAFT(J,ITYPDE(I)),I)**2 +
     &               CORNDE(2,IPAFT(J,ITYPDE(I)),I)**2 )
            H(JJ)= -DATN2D( R, CORNDE(3,IPAFT(J,ITYPDE(I)),I) )
            V(JJ)=  DATN2D( CORNDE(2,IPAFT(J,ITYPDE(I)),I),
     &                      CORNDE(1,IPAFT(J,ITYPDE(I)),I) )
            V(JJ)=  DFINXT ( FIREF, V(JJ) )
            FIREF=  V(JJ)

            IF (K.LE.4) THEN
              IF (IPAFT(J,ITYPDE(I)).EQ.INSFT(K,ITYPDE(I))) THEN
                DX = CORNDE(1,IPAFT(J+1,ITYPDE(I)),I) -
     &               CORNDE(1,IPAFT(J  ,ITYPDE(I)),I)
                DY = CORNDE(2,IPAFT(J+1,ITYPDE(I)),I) -
     &               CORNDE(2,IPAFT(J  ,ITYPDE(I)),I)
                DO L=1 , NINTP
                  JJ =JJ + 1
                  XIP=CORNDE(1,IPAFT(J,ITYPDE(I)),I)+(L*DX/(NINTP+1))
                  YIP=CORNDE(2,IPAFT(J,ITYPDE(I)),I)+(L*DY/(NINTP+1))
                  ZIP=CORNDE(3,IPAFT(J,ITYPDE(I)),I)
                  R  = SQRT( XIP**2 + YIP**2 )
                  H(JJ) = -DATN2D( R, ZIP )
                  V(JJ) =  DFINXT( FIREF, DATN2D( YIP, XIP))
                  FIREF =  V(JJ)
                END DO
                K = K + 1
              END IF
            END IF
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,1 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) + 360.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,1 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) - 720.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,1 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF
        END DO
C  middle-angle slots with z<0
        DO I = 1+NSLHE , NSLHE+NSLIE
          K = 1
          JJ = 0
          FIREF = 5.
          DO J=1 , LPAFT( ITYPDE(I))
            JJ = JJ + 1
            R = SQRT(CORNDE(1,IPAFT(J,ITYPDE(I)),I)**2 +
     &               CORNDE(2,IPAFT(J,ITYPDE(I)),I)**2 )
            H(JJ)= -DATN2D( R, CORNDE(3,IPAFT(J,ITYPDE(I)),I) )
            V(JJ)=  DATN2D( CORNDE(2,IPAFT(J,ITYPDE(I)),I),
     &                      CORNDE(1,IPAFT(J,ITYPDE(I)),I) )
            V(JJ)=  DFINXT ( FIREF, V(JJ) )
            FIREF=  V(JJ)

            IF (K.LE.4) THEN
              IF (IPAFT(J,ITYPDE(I)).EQ.INSFT(K,ITYPDE(I))) THEN
                DX = CORNDE(1,IPAFT(J+1,ITYPDE(I)),I) -
     &               CORNDE(1,IPAFT(J  ,ITYPDE(I)),I)
                DY = CORNDE(2,IPAFT(J+1,ITYPDE(I)),I) -
     &               CORNDE(2,IPAFT(J  ,ITYPDE(I)),I)
                DO L=1 , NINTP
                  JJ =JJ + 1
                  XIP=CORNDE(1,IPAFT(J,ITYPDE(I)),I)+(L*DX/(NINTP+1))
                  YIP=CORNDE(2,IPAFT(J,ITYPDE(I)),I)+(L*DY/(NINTP+1))
                  ZIP=CORNDE(3,IPAFT(J,ITYPDE(I)),I)
                  R  = SQRT( XIP**2 + YIP**2 )
                  H(JJ) = -DATN2D( R, ZIP )
                  V(JJ) =  DFINXT( FIREF, DATN2D( YIP, XIP))
                  FIREF =  V(JJ)
                END DO
                K = K + 1
              END IF
            END IF
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,1 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) + 360.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,1 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) - 720.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,1 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF
        END DO

      END IF


      END
C
C  end of DGDMEI
C

*DK DGDMEO
C######################################################################
C
      SUBROUTINE DGDMEO ( TPROJ , NPR, FSLCT, FSLOT )
C
C-----------------------------------------
C
C   Author   :- R.Vogl                21-SEP-1989
C
C=========================================
C
C   Purpose   : draw outer layer of muon-end-caps
C   Inputs    : TPROJ : projection to be drawn
C               NINTP : number of interpol. points
C   Outputs   :
C
C=========================================
C +
C Declarations.
C -
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DE
      PARAMETER ( NSLTE = 16 )
      PARAMETER ( NSLHE =  8 )
      PARAMETER ( NSLIE =  4 , NSLOE =  4 )
      PARAMETER ( NMTPE =  8 )
      PARAMETER ( NCMXE =  8 )
      COMMON /DEMGEO/  ITYPDE ( NSLTE ) , NCORDE ( NMTPE ) ,
     &                 CORNDE ( 3, NCMXE , NSLTE ),
     &                 RMNIDE, RMXIDE, RMNODE, RMXODE
C------------------------------------------------------------------- DB
      PARAMETER ( NSLTB = 34 , NSLIB = 17 , NSLOB = 17 )
      PARAMETER ( NMTPB = 10 , NMTIB = 4  , NMTOB = 6  )
      PARAMETER ( NCMXB = 16 )
      COMMON /DBMGEO/  ITYPDB ( NSLTB ) , NCORDB ( NMTPB ) ,
     &                 CORNDB ( 3, NCMXB, NSLTB ),
     &                 RMNIDB, RMXIDB, RMNODB, RMXODB
C------------------------------------------------------------------- DM
      PARAMETER ( NSLTM = 38 )
      PARAMETER ( NSLHM = 19 )
      PARAMETER ( NSLIM = 10 , NSLOM = 9 )
      PARAMETER ( NMTPM = 9  )
      PARAMETER ( NCMXM = 8  )
      COMMON /DMMGEO/  ITYPDM ( NSLTM ) , NCORDM ( NMTPM ) ,
     &                 CORNDM ( 3, NCMXM , NSLTM ),
     &                 RMNIDM, RMXIDM, RMNODM, RMXODM
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77


      CHARACTER *3 TPROJ
      LOGICAL  FSLCT, FSLOT(38,3)

C
C  pathes for the poly-lines to be drawn
C
C       LPAxx   length of path
C       IPAxx   corner-point number
C       INSxx   corner-points on poly-line after which interp. points are
C               to be inserted
C
      DIMENSION LPAYX ( NMTPE )
      DATA LPAYX / 5,5,5,5,5,5,5,5 /
      DIMENSION IPAYX ( 5 , NMTPE )
      DATA IPAYX / 1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1 /

      DIMENSION LPAFT ( NMTPE )
      DATA LPAFT / 5,5,5,5,5,5,5,5 /
      DIMENSION IPAFT ( 5 , NMTPE )
      DATA IPAFT / 1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1,
     &             1,2,3,4,1 /
      DIMENSION INSFT ( 4, NMTPE )
      DATA INSFT / 1,2,3,4, 1,2,3,4, 1,2,3,4, 1,2,3,4,
     &             1,2,3,4, 1,2,3,4, 1,2,3,4, 1,2,3,4 /

      DIMENSION H(100),V(100)


      NINTP = IDMIX(10,5*NPR,17)

      IF (TPROJ(2:3).EQ.'YX') THEN

        DO I=1+NSLIE , NSLHE
          DO J=1 , LPAYX( ITYPDE(I) )
            H(J) = CORNDE( 1, IPAYX(J,ITYPDE(I) ), I)
            V(J) = CORNDE( 2, IPAYX(J,ITYPDE(I) ), I)
          END DO
          IF (FSLCT) THEN
            IF (FSLOT(I,1).OR.FSLOT(I+NSLHE,1)) THEN
              CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
            END IF
          ELSE
            CALL DQPOL( LPAYX( ITYPDB(I)), H, V )
          END IF
        END DO

      ELSE IF (TPROJ(2:3).EQ.'YZ') THEN

        H(1) = CORNDE(3,5,8)
        H(2) = CORNDE(3,3,5)
        H(3) = CORNDE(3,3,5)
        H(4) = CORNDE(3,5,8)
        H(5) = H(1)
        V(1) = -RMXODM
        V(2) = -RMXODM
        V(3) =  RMXODM
        V(4) =  RMXODM
        V(5) = V(1)
        CALL DQPOL( 5, H, V )

        H(1) = CORNDE(3,5,16)
        H(2) = CORNDE(3,3,13)
        H(3) = CORNDE(3,3,13)
        H(4) = CORNDE(3,5,16)
        H(5) = H(1)
        V(1) = -RMXODM
        V(2) = -RMXODM
        V(3) =  RMXODM
        V(4) =  RMXODM
        V(5) = V(1)
        CALL DQPOL( 5, H, V )

      ELSE IF (TPROJ(2:3).EQ.'FZ') THEN

        H(1) = CORNDE(3,5,13)
        H(2) = CORNDE(3,1,13)
        H(3) = CORNDE(3,1,13)
        H(4) = CORNDE(3,5,13)
        H(5) = H(1)
        V(1) = -60.
        V(2) = -60.
        V(3) =  540.
        V(4) =  540.
        V(5) = V(1)
        CALL DQPOL( 5, H, V )
        H(1) = CORNDE(3,5,5)
        H(2) = CORNDE(3,1,5)
        H(3) = CORNDE(3,1,5)
        H(4) = CORNDE(3,5,5)
        H(5) = H(1)
        V(1) = -60.
        V(2) = -60.
        V(3) =  540.
        V(4) =  540.
        V(5) = V(1)
        CALL DQPOL( 5, H, V )

      ELSE IF (TPROJ(2:3).EQ.'RZ') THEN

        H(1) = CORNDE(3,5,13)
        H(2) = CORNDE(3,1,13)
        H(3) = CORNDE(3,1,13)
        H(4) = CORNDE(3,5,13)
        H(5) = H(1)
        V(1) = RMNODE
        V(2) = RMNODE
        V(3) = RMXODM
        V(4) = RMXODM
        V(5) = V(1)
        CALL DQPOL( 5, H, V )
        IF (TPROJ(1:1).EQ.'S') THEN
          DO I=1,5
            V(I) = -V(I)
          END DO
          CALL DQPOL( 5, H, V )
        END IF

        H(1) = CORNDE(3,5,5)
        H(2) = CORNDE(3,1,5)
        H(3) = CORNDE(3,1,5)
        H(4) = CORNDE(3,5,5)
        H(5) = H(1)
        V(1) = RMNODE
        V(2) = RMNODE
        V(3) = RMXODM
        V(4) = RMXODM
        V(5) = V(1)
        CALL DQPOL( 5, H, V )
        IF (TPROJ(1:1).EQ.'S') THEN
          DO I=1,5
            V(I) = -V(I)
          END DO
          CALL DQPOL( 5, H, V )
        END IF

      ELSE IF (TPROJ(2:3).EQ.'FT') THEN
C   middle-angle slots with z>0
        DO I=1+NSLIE , NSLHE
          K = 1
          JJ = 0
          FIREF = 5.
          DO J=1 , LPAFT( ITYPDE(I))
            JJ = JJ + 1
            R = SQRT(CORNDE(1,IPAFT(J,ITYPDE(I)),I)**2 +
     &               CORNDE(2,IPAFT(J,ITYPDE(I)),I)**2 )
            H(JJ)= -DATN2D( R, CORNDE(3,IPAFT(J,ITYPDE(I)),I) )
            V(JJ)=  DATN2D( CORNDE(2,IPAFT(J,ITYPDE(I)),I),
     &                      CORNDE(1,IPAFT(J,ITYPDE(I)),I) )
            V(JJ)=  DFINXT ( FIREF, V(JJ) )
            FIREF=  V(JJ)

            IF (K.LE.4) THEN
              IF (IPAFT(J,ITYPDE(I)).EQ.INSFT(K,ITYPDE(I))) THEN
                DX = CORNDE(1,IPAFT(J+1,ITYPDE(I)),I) -
     &               CORNDE(1,IPAFT(J  ,ITYPDE(I)),I)
                DY = CORNDE(2,IPAFT(J+1,ITYPDE(I)),I) -
     &               CORNDE(2,IPAFT(J  ,ITYPDE(I)),I)
                DO L=1 , NINTP
                  JJ =JJ + 1
                  XIP=CORNDE(1,IPAFT(J,ITYPDE(I)),I)+(L*DX/(NINTP+1))
                  YIP=CORNDE(2,IPAFT(J,ITYPDE(I)),I)+(L*DY/(NINTP+1))
                  ZIP=CORNDE(3,IPAFT(J,ITYPDE(I)),I)
                  R  = SQRT( XIP**2 + YIP**2 )
                  H(JJ) = -DATN2D( R, ZIP )
                  V(JJ) =  DFINXT( FIREF, DATN2D( YIP, XIP))
                  FIREF =  V(JJ)
                END DO
                K = K + 1
              END IF
            END IF
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,1 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) + 360.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,1 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) - 720.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,1 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF
        END DO
C  middle-angle slots with z<0
        DO I = 1+NSLHE+NSLIE , NSLTE
          K = 1
          JJ = 0
          FIREF = 5.
          DO J=1 , LPAFT( ITYPDE(I))
            JJ = JJ + 1
            R = SQRT(CORNDE(1,IPAFT(J,ITYPDE(I)),I)**2 +
     &               CORNDE(2,IPAFT(J,ITYPDE(I)),I)**2 )
            H(JJ)= -DATN2D( R, CORNDE(3,IPAFT(J,ITYPDE(I)),I) )
            V(JJ)=  DATN2D( CORNDE(2,IPAFT(J,ITYPDE(I)),I),
     &                      CORNDE(1,IPAFT(J,ITYPDE(I)),I) )
            V(JJ)=  DFINXT ( FIREF, V(JJ) )
            FIREF=  V(JJ)

            IF (K.LE.4) THEN
              IF (IPAFT(J,ITYPDE(I)).EQ.INSFT(K,ITYPDE(I))) THEN
                DX = CORNDE(1,IPAFT(J+1,ITYPDE(I)),I) -
     &               CORNDE(1,IPAFT(J  ,ITYPDE(I)),I)
                DY = CORNDE(2,IPAFT(J+1,ITYPDE(I)),I) -
     &               CORNDE(2,IPAFT(J  ,ITYPDE(I)),I)
                DO L=1 , NINTP
                  JJ =JJ + 1
                  XIP=CORNDE(1,IPAFT(J,ITYPDE(I)),I)+(L*DX/(NINTP+1))
                  YIP=CORNDE(2,IPAFT(J,ITYPDE(I)),I)+(L*DY/(NINTP+1))
                  ZIP=CORNDE(3,IPAFT(J,ITYPDE(I)),I)
                  R  = SQRT( XIP**2 + YIP**2 )
                  H(JJ) = -DATN2D( R, ZIP )
                  V(JJ) =  DFINXT( FIREF, DATN2D( YIP, XIP))
                  FIREF =  V(JJ)
                END DO
                K = K + 1
              END IF
            END IF
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,1 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) + 360.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,1 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF

          DO J=1 , JJ
            V(J) = V(J) - 720.
          END DO
          IF (FSLCT) THEN
            IF (FSLOT( I,1 )) THEN
              CALL DQPOL( JJ, H, V )
            END IF
          ELSE
            CALL DQPOL( JJ, H, V )
          END IF
        END DO

      END IF


      END
C
C  end of DGDMEO
C


*DK DGIMUD
C######################################################################
C
      SUBROUTINE DGIMUD
C
C-----------------------------------------
C
C   Author   :- R.Vogl                19-SEP-1989
C
C=========================================
C
C   Purpose   : read in muon-dedector geometry data
C   Inputs    : none
C   Outputs   : fills muon-dedector geometry common-blocks
C
C=========================================
C +
C Declarations.
C -

      INCLUDE 'A_BCS.INC'
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DB
      PARAMETER ( NSLTB = 34 , NSLIB = 17 , NSLOB = 17 )
      PARAMETER ( NMTPB = 10 , NMTIB = 4  , NMTOB = 6  )
      PARAMETER ( NCMXB = 16 )
      COMMON /DBMGEO/  ITYPDB ( NSLTB ) , NCORDB ( NMTPB ) ,
     &                 CORNDB ( 3, NCMXB, NSLTB ),
     &                 RMNIDB, RMXIDB, RMNODB, RMXODB
C------------------------------------------------------------------- DM
      PARAMETER ( NSLTM = 38 )
      PARAMETER ( NSLHM = 19 )
      PARAMETER ( NSLIM = 10 , NSLOM = 9 )
      PARAMETER ( NMTPM = 9  )
      PARAMETER ( NCMXM = 8  )
      COMMON /DMMGEO/  ITYPDM ( NSLTM ) , NCORDM ( NMTPM ) ,
     &                 CORNDM ( 3, NCMXM , NSLTM ),
     &                 RMNIDM, RMXIDM, RMNODM, RMXODM
C------------------------------------------------------------------- DE
      PARAMETER ( NSLTE = 16 )
      PARAMETER ( NSLHE =  8 )
      PARAMETER ( NSLIE =  4 , NSLOE =  4 )
      PARAMETER ( NMTPE =  8 )
      PARAMETER ( NCMXE =  8 )
      COMMON /DEMGEO/  ITYPDE ( NSLTE ) , NCORDE ( NMTPE ) ,
     &                 CORNDE ( 3, NCMXE , NSLTE ),
     &                 RMNIDE, RMXIDE, RMNODE, RMXODE
C------------------------------------------------------------------- DG
      COMMON / DGDBLU / LDBSDG,IRUNDG
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77

      INCLUDE 'A_MBTGJJ.INC'
      INCLUDE 'A_MBAGJJ.INC'
      INCLUDE 'A_MBSGJJ.INC'

      INCLUDE 'A_MMTGJJ.INC'
      INCLUDE 'A_MMAGJJ.INC'
      INCLUDE 'A_MMSGJJ.INC'

      INCLUDE 'A_MECGJJ.INC'
      INCLUDE 'A_METGJJ.INC'
      INCLUDE 'A_MESGJJ.INC'

      INTEGER ALGTDB


C ideal phi of BARREL-slots : not in data-base!
C      DIMENSION FIIDB(NSLTB)
C      DATA FIIDB / 0.,30.,60.,90.,90.,120.,150.,180.,210.,240.,240.,
C     &             240.,270.,300.,300.,300.,330.,
C     &             0.,30.,60.,90.,90.,120.,150.,180.,210.,240.,240.,
C     &             240.,270.,300.,300.,300.,330. /


      INCLUDE 'A_BMACRO.INC'

C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  initialize arrays holding the number of corners for types
C
      NCORDB( 1) =  8
      NCORDB( 2) = 12
      NCORDB( 3) =  8
      NCORDB( 4) = 16
      NCORDB( 5) =  8
      NCORDB( 6) = 12
      NCORDB( 7) =  8
      NCORDB( 8) = 16
      NCORDB( 9) =  8
      NCORDB(10) =  8

      NCORDM( 1) =  8
      NCORDM( 2) =  8
      NCORDM( 3) =  8
      NCORDM( 4) =  8
      NCORDM( 5) =  8
      NCORDM( 6) =  8
      NCORDM( 7) =  8
      NCORDM( 8) =  8
      NCORDM( 9) =  8

      NCORDE( 1) =  8
      NCORDE( 2) =  8
      NCORDE( 3) =  8
      NCORDE( 4) =  8
      NCORDE( 5) =  8
      NCORDE( 6) =  8
      NCORDE( 7) =  8
      NCORDE( 8) =  8


C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  Barrel and special barrel modules first
C

      IFLAG = ALGTDB(LDBSDG,'MBTGMBAGMBSG',IRUNDG)
      IF (IFLAG.EQ.0) THEN
        CALL DWRT(' ERROR: unable to read MUON DET geometry data!')
        CALL DWRT('        source of error : ALGTDB.')
        RETURN
      END IF

      KMBTG = IW( NAMIND('MBTG'))
      KMBAG = IW( NAMIND('MBAG'))
      KMBSG = IW( NAMIND('MBSG'))

C
C  data readin
C
C  TH     THickness of muon-box
C  RICEI  Radius of Inner CEnter of Inner layer box
C  RICEO  Radius of Inner CEnter of Outer layer box
C  ROCEI  Radius of Outer CEnter of Inner layer box
C  ROCEO  Radius of Outer CEnter of Outer layer box
C

      DO I = 1 , NSLTB


        ITYPDB(I) = ITABL( KMBSG,I,JMBSK2 )
        K1        = ITABL( KMBSG,I,JMBSK1 )
        ZC        = RTABL( KMBSG,I,JMBSZC )
        DE        = RTABL( KMBSG,I,JMBSDE )
        VO        = ITABL( KMBSG,I,JMBSVO )
C###########################################################################
C###########################################################################
C
C  IMPORTANT : muon hits were found not to be in the drawn modules ( some
C              data from the data-base ( either geometry of muon-dedectors
C              or the reconstructed hits !!) must be wrong or is at least
C              miss-interpreted !). To have all ( or at least most of the hits)
C              in the drawn areas, these are increased in size ! The mu-barrel
C              modules are now drawn one and a half time as thick as they are
C              according to the database!
C              We have still to finde out, what radius is diven in MBAG item R1
C              and R2 !
C
C###########################################################################
C###########################################################################

        TH    = RTABL( KMBAG, K1, JMBATH )
        RICEI = RTABL( KMBAG, K1, JMBAR1 )-TH/2.
        RICEO = RTABL( KMBAG, K1, JMBAR2 )-TH/2.
        ROCEI = RICEI + TH
        ROCEO = RICEO + TH

        FI = 360./12 * (VO-1)

        IF (ITYPDB(I).EQ. 1) THEN
C--------------------------------------------------------------------------
C type 1:
C
          WIDE  = RTABL( KMBTG,1,JMBTRB )
          ZLONG = RTABL( KMBTG,1,JMBTZB )

          DO J=1,2
            CORNDB(3,J  ,I) =  ZLONG/2 + ZC
            CORNDB(3,4+J,I) =  ZLONG/2 + ZC
            CORNDB(3,2+J,I) = -ZLONG/2 + ZC
            CORNDB(3,6+J,I) = -ZLONG/2 + ZC
          END DO

          C = COSD(FI)
          S = SIND(FI)

          CORNDB(1,1,I)=RICEI*C+( WIDE/2-DE)*S
          CORNDB(2,1,I)=RICEI*S-( WIDE/2-DE)*C
          CORNDB(1,2,I)=RICEI*C+(-WIDE/2-DE)*S
          CORNDB(2,2,I)=RICEI*S-(-WIDE/2-DE)*C
          CORNDB(1,3,I)=RICEI*C+(-WIDE/2-DE)*S
          CORNDB(2,3,I)=RICEI*S-(-WIDE/2-DE)*C
          CORNDB(1,4,I)=RICEI*C+( WIDE/2-DE)*S
          CORNDB(2,4,I)=RICEI*S-( WIDE/2-DE)*C
          CORNDB(1,5,I)=ROCEI*C+( WIDE/2-DE)*S
          CORNDB(2,5,I)=ROCEI*S-( WIDE/2-DE)*C
          CORNDB(1,6,I)=ROCEI*C+(-WIDE/2-DE)*S
          CORNDB(2,6,I)=ROCEI*S-(-WIDE/2-DE)*C
          CORNDB(1,7,I)=ROCEI*C+(-WIDE/2-DE)*S
          CORNDB(2,7,I)=ROCEI*S-(-WIDE/2-DE)*C
          CORNDB(1,8,I)=ROCEI*C+( WIDE/2-DE)*S
          CORNDB(2,8,I)=ROCEI*S-( WIDE/2-DE)*C


        ELSE IF (ITYPDB(I).EQ.2) THEN
C--------------------------------------------------------------------------
C type 2:
C
          WIDE  = RTABL( KMBTG,2,JMBTRB )
          ZLONG = RTABL( KMBTG,2,JMBTZB )
          ZT    = RTABL( KMBTG,2,JMBTZT )
          RT    = RTABL( KMBTG,2,JMBTRT )

          IF (ZC.GT.0) THEN
            FAC = 1.
          ELSE
            FAC = -1.
          END IF

          DO J=1,2
            CORNDB(3,J+0 ,I) =  FAC*ZLONG/2 + ZC
            CORNDB(3,J+6 ,I) =  FAC*ZLONG/2 + ZC
            CORNDB(3,J+2 ,I) = -FAC*ZLONG/2 + ZC
            CORNDB(3,J+8 ,I) = -FAC*ZLONG/2 + ZC
            CORNDB(3,J+4 ,I) = -FAC*ZLONG/2 + ZC + FAC*ZT
            CORNDB(3,J+10,I) = -FAC*ZLONG/2 + ZC + FAC*ZT
          END DO

C slots 14 and 16 are  mirrorimages of 10 and 12
          IF (I.EQ.10 .OR. I.EQ.12) THEN
            FAC = 1.
          ELSE
           FAC = -1.
          END IF

C
C  remember :   cos(fi) = sin(90-fi)
C               sin(fi) = cos(90-fi)
C
          C = COSD(FI)
          S = SIND(FI)

          CORNDB(1,1 ,I)=RICEI*C+( FAC*WIDE/2-DE)       *S
          CORNDB(2,1 ,I)=RICEI*S-( FAC*WIDE/2-DE)       *C
          CORNDB(1,2 ,I)=RICEI*C+(-FAC*WIDE/2-DE)       *S
          CORNDB(2,2 ,I)=RICEI*S-(-FAC*WIDE/2-DE)       *C
          CORNDB(1,3 ,I)=RICEI*C+(-FAC*WIDE/2-DE)       *S
          CORNDB(2,3 ,I)=RICEI*S-(-FAC*WIDE/2-DE)       *C
          CORNDB(1,4 ,I)=RICEI*C+( FAC*WIDE/2-DE-FAC*RT)*S
          CORNDB(2,4 ,I)=RICEI*S-( FAC*WIDE/2-DE-FAC*RT)*C
          CORNDB(1,5 ,I)=RICEI*C+( FAC*WIDE/2-DE-FAC*RT)*S
          CORNDB(2,5 ,I)=RICEI*S-( FAC*WIDE/2-DE-FAC*RT)*C
          CORNDB(1,6 ,I)=RICEI*C+( FAC*WIDE/2-DE)       *S
          CORNDB(2,6 ,I)=RICEI*S-( FAC*WIDE/2-DE)       *C
          CORNDB(1,7 ,I)=ROCEI*C+( FAC*WIDE/2-DE)       *S
          CORNDB(2,7 ,I)=ROCEI*S-( FAC*WIDE/2-DE)       *C
          CORNDB(1,8 ,I)=ROCEI*C+(-FAC*WIDE/2-DE)       *S
          CORNDB(2,8 ,I)=ROCEI*S-(-FAC*WIDE/2-DE)       *C
          CORNDB(1,9 ,I)=ROCEI*C+(-FAC*WIDE/2-DE)       *S
          CORNDB(2,9 ,I)=ROCEI*S-(-FAC*WIDE/2-DE)       *C
          CORNDB(1,10,I)=ROCEI*C+( FAC*WIDE/2-DE-FAC*RT)*S
          CORNDB(2,10,I)=ROCEI*S-( FAC*WIDE/2-DE-FAC*RT)*C
          CORNDB(1,11,I)=ROCEI*C+( FAC*WIDE/2-DE-FAC*RT)*S
          CORNDB(2,11,I)=ROCEI*S-( FAC*WIDE/2-DE-FAC*RT)*C
          CORNDB(1,12,I)=ROCEI*C+( FAC*WIDE/2-DE)       *S
          CORNDB(2,12,I)=ROCEI*S-( FAC*WIDE/2-DE)       *C

        ELSE IF (ITYPDB(I).EQ. 3) THEN
C------------------------------------------------------------------------
C type 3:
C
          WIDE  = RTABL( KMBTG,3,JMBTRB )
          ZLONG = RTABL( KMBTG,3,JMBTZB )

          DO J=1,2
            CORNDB(3,J  ,I) =  ZLONG/2 + ZC
            CORNDB(3,4+J,I) =  ZLONG/2 + ZC
            CORNDB(3,2+J,I) = -ZLONG/2 + ZC
            CORNDB(3,6+J,I) = -ZLONG/2 + ZC
          END DO

          C = COSD(FI)
          S = SIND(FI)

          CORNDB(1,1,I)=RICEI*C+( WIDE/2-DE)*S
          CORNDB(2,1,I)=RICEI*S-( WIDE/2-DE)*C
          CORNDB(1,2,I)=RICEI*C+(-WIDE/2-DE)*S
          CORNDB(2,2,I)=RICEI*S-(-WIDE/2-DE)*C
          CORNDB(1,3,I)=RICEI*C+(-WIDE/2-DE)*S
          CORNDB(2,3,I)=RICEI*S-(-WIDE/2-DE)*C
          CORNDB(1,4,I)=RICEI*C+( WIDE/2-DE)*S
          CORNDB(2,4,I)=RICEI*S-( WIDE/2-DE)*C
          CORNDB(1,5,I)=ROCEI*C+( WIDE/2-DE)*S
          CORNDB(2,5,I)=ROCEI*S-( WIDE/2-DE)*C
          CORNDB(1,6,I)=ROCEI*C+(-WIDE/2-DE)*S
          CORNDB(2,6,I)=ROCEI*S-(-WIDE/2-DE)*C
          CORNDB(1,7,I)=ROCEI*C+(-WIDE/2-DE)*S
          CORNDB(2,7,I)=ROCEI*S-(-WIDE/2-DE)*C
          CORNDB(1,8,I)=ROCEI*C+( WIDE/2-DE)*S
          CORNDB(2,8,I)=ROCEI*S-( WIDE/2-DE)*C

        ELSE IF (ITYPDB(I).EQ. 4) THEN
C--------------------------------------------------------------------------
C type 4:
C
          WIDE  = RTABL( KMBTG,4,JMBTRB )
          ZLONG = RTABL( KMBTG,4,JMBTZB )
          ZT    = RTABL( KMBTG,4,JMBTZT )
          RT    = RTABL( KMBTG,4,JMBTRT )

          DO J=1,2
            CORNDB(3,J+0 ,I) =  ZLONG/2 + ZC
            CORNDB(3,J+2 ,I) =  ZLONG/2 + ZC - ZT
            CORNDB(3,J+4 ,I) = -ZLONG/2 + ZC + ZT
            CORNDB(3,J+6 ,I) = -ZLONG/2 + ZC
            CORNDB(3,J+8 ,I) =  ZLONG/2 + ZC
            CORNDB(3,J+10,I) =  ZLONG/2 + ZC - ZT
            CORNDB(3,J+12,I) = -ZLONG/2 + ZC + ZT
            CORNDB(3,J+14,I) = -ZLONG/2 + ZC
          END DO

C slots 4 and 5 are mirrorimages
          IF (I.EQ.4) THEN
            FAC = 1.
          ELSE
            FAC = -1.
          END IF

          C = COSD(FI)
          S = SIND(FI)

          CORNDB(1,1 ,I)=RICEI*C+( FAC*WIDE/2-DE)       *S
          CORNDB(2,1 ,I)=RICEI*S-( FAC*WIDE/2-DE)       *C
          CORNDB(1,2 ,I)=RICEI*C+(-FAC*WIDE/2-DE+FAC*RT)*S
          CORNDB(2,2 ,I)=RICEI*S-(-FAC*WIDE/2-DE+FAC*RT)*C
          CORNDB(1,3 ,I)=RICEI*C+(-FAC*WIDE/2-DE+FAC*RT)*S
          CORNDB(2,3 ,I)=RICEI*S-(-FAC*WIDE/2-DE+FAC*RT)*C
          CORNDB(1,4 ,I)=RICEI*C+(-FAC*WIDE/2-DE)       *S
          CORNDB(2,4 ,I)=RICEI*S-(-FAC*WIDE/2-DE)       *C
          CORNDB(1,5 ,I)=RICEI*C+(-FAC*WIDE/2-DE)       *S
          CORNDB(2,5 ,I)=RICEI*S-(-FAC*WIDE/2-DE)       *C
          CORNDB(1,6 ,I)=RICEI*C+(-FAC*WIDE/2-DE+FAC*RT)*S
          CORNDB(2,6 ,I)=RICEI*S-(-FAC*WIDE/2-DE+FAC*RT)*C
          CORNDB(1,7 ,I)=RICEI*C+(-FAC*WIDE/2-DE+FAC*RT)*S
          CORNDB(2,7 ,I)=RICEI*S-(-FAC*WIDE/2-DE+FAC*RT)*C
          CORNDB(1,8 ,I)=RICEI*C+( FAC*WIDE/2-DE)       *S
          CORNDB(2,8 ,I)=RICEI*S-( FAC*WIDE/2-DE)       *C
          CORNDB(1,9 ,I)=ROCEI*C+( FAC*WIDE/2-DE)       *S
          CORNDB(2,9 ,I)=ROCEI*S-( FAC*WIDE/2-DE)       *C
          CORNDB(1,10,I)=ROCEI*C+(-FAC*WIDE/2-DE+FAC*RT)*S
          CORNDB(2,10,I)=ROCEI*S-(-FAC*WIDE/2-DE+FAC*RT)*C
          CORNDB(1,11,I)=ROCEI*C+(-FAC*WIDE/2-DE+FAC*RT)*S
          CORNDB(2,11,I)=ROCEI*S-(-FAC*WIDE/2-DE+FAC*RT)*C
          CORNDB(1,12,I)=ROCEI*C+(-FAC*WIDE/2-DE)       *S
          CORNDB(2,12,I)=ROCEI*S-(-FAC*WIDE/2-DE)       *C
          CORNDB(1,13,I)=ROCEI*C+(-FAC*WIDE/2-DE)       *S
          CORNDB(2,13,I)=ROCEI*S-(-FAC*WIDE/2-DE)       *C
          CORNDB(1,14,I)=ROCEI*C+(-FAC*WIDE/2-DE+FAC*RT)*S
          CORNDB(2,14,I)=ROCEI*S-(-FAC*WIDE/2-DE+FAC*RT)*C
          CORNDB(1,15,I)=ROCEI*C+(-FAC*WIDE/2-DE+FAC*RT)*S
          CORNDB(2,15,I)=ROCEI*S-(-FAC*WIDE/2-DE+FAC*RT)*C
          CORNDB(1,16,I)=ROCEI*C+( FAC*WIDE/2-DE)       *S
          CORNDB(2,16,I)=ROCEI*S-( FAC*WIDE/2-DE)       *C

        ELSE IF (ITYPDB(I).EQ. 5) THEN
C--------------------------------------------------------------------------
C type 5:
C
          WIDE  = RTABL( KMBTG,5,JMBTRB )
          ZLONG = RTABL( KMBTG,5,JMBTZB )

          DO J=1,2
            CORNDB(3,J  ,I) =  ZLONG/2 + ZC
            CORNDB(3,4+J,I) =  ZLONG/2 + ZC
            CORNDB(3,2+J,I) = -ZLONG/2 + ZC
            CORNDB(3,6+J,I) = -ZLONG/2 + ZC
          END DO

          C = COSD(FI)
          S = SIND(FI)

          CORNDB(1,1,I)=RICEO*C+( WIDE/2-DE)*S
          CORNDB(2,1,I)=RICEO*S-( WIDE/2-DE)*C
          CORNDB(1,2,I)=RICEO*C+(-WIDE/2-DE)*S
          CORNDB(2,2,I)=RICEO*S-(-WIDE/2-DE)*C
          CORNDB(1,3,I)=RICEO*C+(-WIDE/2-DE)*S
          CORNDB(2,3,I)=RICEO*S-(-WIDE/2-DE)*C
          CORNDB(1,4,I)=RICEO*C+( WIDE/2-DE)*S
          CORNDB(2,4,I)=RICEO*S-( WIDE/2-DE)*C
          CORNDB(1,5,I)=ROCEO*C+( WIDE/2-DE)*S
          CORNDB(2,5,I)=ROCEO*S-( WIDE/2-DE)*C
          CORNDB(1,6,I)=ROCEO*C+(-WIDE/2-DE)*S
          CORNDB(2,6,I)=ROCEO*S-(-WIDE/2-DE)*C
          CORNDB(1,7,I)=ROCEO*C+(-WIDE/2-DE)*S
          CORNDB(2,7,I)=ROCEO*S-(-WIDE/2-DE)*C
          CORNDB(1,8,I)=ROCEO*C+( WIDE/2-DE)*S
          CORNDB(2,8,I)=ROCEO*S-( WIDE/2-DE)*C

        ELSE IF (ITYPDB(I).EQ. 6) THEN
C--------------------------------------------------------------------------
C type 6:
C
          WIDE  = RTABL( KMBTG,6,JMBTRB )
          ZLONG = RTABL( KMBTG,6,JMBTZB )
          ZT    = RTABL( KMBTG,6,JMBTZT )
          RT    = RTABL( KMBTG,6,JMBTRT )

          IF (ZC.GT.0) THEN
            FAC = 1.
          ELSE
            FAC = -1.
          END IF

          DO J=1,2
            CORNDB(3,J+0 ,I) =  FAC*ZLONG/2 + ZC
            CORNDB(3,J+6 ,I) =  FAC*ZLONG/2 + ZC
            CORNDB(3,J+2 ,I) = -FAC*ZLONG/2 + ZC
            CORNDB(3,J+8 ,I) = -FAC*ZLONG/2 + ZC
            CORNDB(3,J+4 ,I) = -FAC*ZLONG/2 + ZC + FAC*ZT
            CORNDB(3,J+10,I) = -FAC*ZLONG/2 + ZC + FAC*ZT
          END DO

C slots 31 and 33 are  mirrorimages of 27 and 29
          IF (I.EQ.27 .OR. I.EQ.29) THEN
            FAC = 1.
          ELSE
           FAC = -1.
          END IF

          C = COSD(FI)
          S = SIND(FI)

          CORNDB(1,1 ,I)=RICEO*C+( FAC*WIDE/2-DE)       *S
          CORNDB(2,1 ,I)=RICEO*S-( FAC*WIDE/2-DE)       *C
          CORNDB(1,2 ,I)=RICEO*C+(-FAC*WIDE/2-DE)       *S
          CORNDB(2,2 ,I)=RICEO*S-(-FAC*WIDE/2-DE)       *C
          CORNDB(1,3 ,I)=RICEO*C+(-FAC*WIDE/2-DE)       *S
          CORNDB(2,3 ,I)=RICEO*S-(-FAC*WIDE/2-DE)       *C
          CORNDB(1,4 ,I)=RICEO*C+( FAC*WIDE/2-DE-FAC*RT)*S
          CORNDB(2,4 ,I)=RICEO*S-( FAC*WIDE/2-DE-FAC*RT)*C
          CORNDB(1,5 ,I)=RICEO*C+( FAC*WIDE/2-DE-FAC*RT)*S
          CORNDB(2,5 ,I)=RICEO*S-( FAC*WIDE/2-DE-FAC*RT)*C
          CORNDB(1,6 ,I)=RICEO*C+( FAC*WIDE/2-DE)       *S
          CORNDB(2,6 ,I)=RICEO*S-( FAC*WIDE/2-DE)       *C
          CORNDB(1,7 ,I)=ROCEO*C+( FAC*WIDE/2-DE)       *S
          CORNDB(2,7 ,I)=ROCEO*S-( FAC*WIDE/2-DE)       *C
          CORNDB(1,8 ,I)=ROCEO*C+(-FAC*WIDE/2-DE)       *S
          CORNDB(2,8 ,I)=ROCEO*S-(-FAC*WIDE/2-DE)       *C
          CORNDB(1,9 ,I)=ROCEO*C+(-FAC*WIDE/2-DE)       *S
          CORNDB(2,9 ,I)=ROCEO*S-(-FAC*WIDE/2-DE)       *C
          CORNDB(1,10,I)=ROCEO*C+( FAC*WIDE/2-DE-FAC*RT)*S
          CORNDB(2,10,I)=ROCEO*S-( FAC*WIDE/2-DE-FAC*RT)*C
          CORNDB(1,11,I)=ROCEO*C+( FAC*WIDE/2-DE-FAC*RT)*S
          CORNDB(2,11,I)=ROCEO*S-( FAC*WIDE/2-DE-FAC*RT)*C
          CORNDB(1,12,I)=ROCEO*C+( FAC*WIDE/2-DE)       *S
          CORNDB(2,12,I)=ROCEO*S-( FAC*WIDE/2-DE)       *C

        ELSE IF (ITYPDB(I).EQ. 7) THEN
C--------------------------------------------------------------------------
C type 7:
C
          WIDE  = RTABL( KMBTG,7,JMBTRB )
          ZLONG = RTABL( KMBTG,7,JMBTZB )

          DO J=1,2
            CORNDB(3,J  ,I) =  ZLONG/2 + ZC
            CORNDB(3,4+J,I) =  ZLONG/2 + ZC
            CORNDB(3,2+J,I) = -ZLONG/2 + ZC
            CORNDB(3,6+J,I) = -ZLONG/2 + ZC
          END DO

          C = COSD(FI)
          S = SIND(FI)

          CORNDB(1,1,I)=RICEO*C+( WIDE/2-DE)*S
          CORNDB(2,1,I)=RICEO*S-( WIDE/2-DE)*C
          CORNDB(1,2,I)=RICEO*C+(-WIDE/2-DE)*S
          CORNDB(2,2,I)=RICEO*S-(-WIDE/2-DE)*C
          CORNDB(1,3,I)=RICEO*C+(-WIDE/2-DE)*S
          CORNDB(2,3,I)=RICEO*S-(-WIDE/2-DE)*C
          CORNDB(1,4,I)=RICEO*C+( WIDE/2-DE)*S
          CORNDB(2,4,I)=RICEO*S-( WIDE/2-DE)*C
          CORNDB(1,5,I)=ROCEO*C+( WIDE/2-DE)*S
          CORNDB(2,5,I)=ROCEO*S-( WIDE/2-DE)*C
          CORNDB(1,6,I)=ROCEO*C+(-WIDE/2-DE)*S
          CORNDB(2,6,I)=ROCEO*S-(-WIDE/2-DE)*C
          CORNDB(1,7,I)=ROCEO*C+(-WIDE/2-DE)*S
          CORNDB(2,7,I)=ROCEO*S-(-WIDE/2-DE)*C
          CORNDB(1,8,I)=ROCEO*C+( WIDE/2-DE)*S
          CORNDB(2,8,I)=ROCEO*S-( WIDE/2-DE)*C

        ELSE IF (ITYPDB(I).EQ. 8) THEN
C--------------------------------------------------------------------------
C type 8:
C
          WIDE  = RTABL( KMBTG,8,JMBTRB )
          ZLONG = RTABL( KMBTG,8,JMBTZB )
          ZT    = RTABL( KMBTG,8,JMBTZT )
          RT    = RTABL( KMBTG,8,JMBTRT )

          DO J=1,2
            CORNDB(3,J+0 ,I) =  ZLONG/2 + ZC
            CORNDB(3,J+2 ,I) =  ZLONG/2 + ZC - ZT
            CORNDB(3,J+4 ,I) = -ZLONG/2 + ZC + ZT
            CORNDB(3,J+6 ,I) = -ZLONG/2 + ZC
            CORNDB(3,J+8 ,I) =  ZLONG/2 + ZC
            CORNDB(3,J+10,I) =  ZLONG/2 + ZC - ZT
            CORNDB(3,J+12,I) = -ZLONG/2 + ZC + ZT
            CORNDB(3,J+14,I) = -ZLONG/2 + ZC
          END DO

C slots 21 and 22 are mirrorimages
          IF (I.EQ.21) THEN
            FAC = 1.
          ELSE
            FAC = -1.
          END IF

          C = COSD(FI)
          S = SIND(FI)

          CORNDB(1,1 ,I)=RICEO*C+( FAC*WIDE/2-DE)       *S
          CORNDB(2,1 ,I)=RICEO*S-( FAC*WIDE/2-DE)       *C
          CORNDB(1,2 ,I)=RICEO*C+(-FAC*WIDE/2-DE+FAC*RT)*S
          CORNDB(2,2 ,I)=RICEO*S-(-FAC*WIDE/2-DE+FAC*RT)*C
          CORNDB(1,3 ,I)=RICEO*C+(-FAC*WIDE/2-DE+FAC*RT)*S
          CORNDB(2,3 ,I)=RICEO*S-(-FAC*WIDE/2-DE+FAC*RT)*C
          CORNDB(1,4 ,I)=RICEO*C+(-FAC*WIDE/2-DE)       *S
          CORNDB(2,4 ,I)=RICEO*S-(-FAC*WIDE/2-DE)       *C
          CORNDB(1,5 ,I)=RICEO*C+(-FAC*WIDE/2-DE)       *S
          CORNDB(2,5 ,I)=RICEO*S-(-FAC*WIDE/2-DE)       *C
          CORNDB(1,6 ,I)=RICEO*C+(-FAC*WIDE/2-DE+FAC*RT)*S
          CORNDB(2,6 ,I)=RICEO*S-(-FAC*WIDE/2-DE+FAC*RT)*C
          CORNDB(1,7 ,I)=RICEO*C+(-FAC*WIDE/2-DE+FAC*RT)*S
          CORNDB(2,7 ,I)=RICEO*S-(-FAC*WIDE/2-DE+FAC*RT)*C
          CORNDB(1,8 ,I)=RICEO*C+( FAC*WIDE/2-DE)       *S
          CORNDB(2,8 ,I)=RICEO*S-( FAC*WIDE/2-DE)       *C
          CORNDB(1,9 ,I)=ROCEO*C+( FAC*WIDE/2-DE)       *S
          CORNDB(2,9 ,I)=ROCEO*S-( FAC*WIDE/2-DE)       *C
          CORNDB(1,10,I)=ROCEO*C+(-FAC*WIDE/2-DE+FAC*RT)*S
          CORNDB(2,10,I)=ROCEO*S-(-FAC*WIDE/2-DE+FAC*RT)*C
          CORNDB(1,11,I)=ROCEO*C+(-FAC*WIDE/2-DE+FAC*RT)*S
          CORNDB(2,11,I)=ROCEO*S-(-FAC*WIDE/2-DE+FAC*RT)*C
          CORNDB(1,12,I)=ROCEO*C+(-FAC*WIDE/2-DE)       *S
          CORNDB(2,12,I)=ROCEO*S-(-FAC*WIDE/2-DE)       *C
          CORNDB(1,13,I)=ROCEO*C+(-FAC*WIDE/2-DE)       *S
          CORNDB(2,13,I)=ROCEO*S-(-FAC*WIDE/2-DE)       *C
          CORNDB(1,14,I)=ROCEO*C+(-FAC*WIDE/2-DE+FAC*RT)*S
          CORNDB(2,14,I)=ROCEO*S-(-FAC*WIDE/2-DE+FAC*RT)*C
          CORNDB(1,15,I)=ROCEO*C+(-FAC*WIDE/2-DE+FAC*RT)*S
          CORNDB(2,15,I)=ROCEO*S-(-FAC*WIDE/2-DE+FAC*RT)*C
          CORNDB(1,16,I)=ROCEO*C+( FAC*WIDE/2-DE)       *S
          CORNDB(2,16,I)=ROCEO*S-( FAC*WIDE/2-DE)       *C

        ELSE IF (ITYPDB(I).EQ. 9) THEN
C--------------------------------------------------------------------------
C type 9:
C
          WIDE  = RTABL( KMBTG,9,JMBTRB )
          ZLONG = RTABL( KMBTG,9,JMBTZB )

          DO J=1,2
            CORNDB(3,J  ,I) =  ZLONG/2 + ZC
            CORNDB(3,4+J,I) =  ZLONG/2 + ZC
            CORNDB(3,2+J,I) = -ZLONG/2 + ZC
            CORNDB(3,6+J,I) = -ZLONG/2 + ZC
          END DO

          C = COSD(FI)
          S = SIND(FI)

          CORNDB(1,1,I)=RICEO*C+( WIDE/2-DE)*S
          CORNDB(2,1,I)=RICEO*S-( WIDE/2-DE)*C
          CORNDB(1,2,I)=RICEO*C+(-WIDE/2-DE)*S
          CORNDB(2,2,I)=RICEO*S-(-WIDE/2-DE)*C
          CORNDB(1,3,I)=RICEO*C+(-WIDE/2-DE)*S
          CORNDB(2,3,I)=RICEO*S-(-WIDE/2-DE)*C
          CORNDB(1,4,I)=RICEO*C+( WIDE/2-DE)*S
          CORNDB(2,4,I)=RICEO*S-( WIDE/2-DE)*C
          CORNDB(1,5,I)=ROCEO*C+( WIDE/2-DE)*S
          CORNDB(2,5,I)=ROCEO*S-( WIDE/2-DE)*C
          CORNDB(1,6,I)=ROCEO*C+(-WIDE/2-DE)*S
          CORNDB(2,6,I)=ROCEO*S-(-WIDE/2-DE)*C
          CORNDB(1,7,I)=ROCEO*C+(-WIDE/2-DE)*S
          CORNDB(2,7,I)=ROCEO*S-(-WIDE/2-DE)*C
          CORNDB(1,8,I)=ROCEO*C+( WIDE/2-DE)*S
          CORNDB(2,8,I)=ROCEO*S-( WIDE/2-DE)*C

        ELSE IF (ITYPDB(I).EQ.10) THEN
C--------------------------------------------------------------------------
C type 10:
C
          WIDE  = RTABL( KMBTG,10,JMBTRB )
          ZLONG = RTABL( KMBTG,10,JMBTZB )

          DO J=1,2
            CORNDB(3,J  ,I) =  ZLONG/2 + ZC
            CORNDB(3,4+J,I) =  ZLONG/2 + ZC
            CORNDB(3,2+J,I) = -ZLONG/2 + ZC
            CORNDB(3,6+J,I) = -ZLONG/2 + ZC
          END DO

          C = COSD(FI)
          S = SIND(FI)

          CORNDB(1,1,I)=RICEO*C+( WIDE/2-DE)*S
          CORNDB(2,1,I)=RICEO*S-( WIDE/2-DE)*C
          CORNDB(1,2,I)=RICEO*C+(-WIDE/2-DE)*S
          CORNDB(2,2,I)=RICEO*S-(-WIDE/2-DE)*C
          CORNDB(1,3,I)=RICEO*C+(-WIDE/2-DE)*S
          CORNDB(2,3,I)=RICEO*S-(-WIDE/2-DE)*C
          CORNDB(1,4,I)=RICEO*C+( WIDE/2-DE)*S
          CORNDB(2,4,I)=RICEO*S-( WIDE/2-DE)*C
          CORNDB(1,5,I)=ROCEO*C+( WIDE/2-DE)*S
          CORNDB(2,5,I)=ROCEO*S-( WIDE/2-DE)*C
          CORNDB(1,6,I)=ROCEO*C+(-WIDE/2-DE)*S
          CORNDB(2,6,I)=ROCEO*S-(-WIDE/2-DE)*C
          CORNDB(1,7,I)=ROCEO*C+(-WIDE/2-DE)*S
          CORNDB(2,7,I)=ROCEO*S-(-WIDE/2-DE)*C
          CORNDB(1,8,I)=ROCEO*C+( WIDE/2-DE)*S
          CORNDB(2,8,I)=ROCEO*S-( WIDE/2-DE)*C

        END IF

      END DO
C
C finally radius min 'n' max for barrel inner & outer layer
C
      RMNIDB = RICEI
      RMXIDB = SQRT( CORNDB(1,5,1)**2 + CORNDB(2,5,1)**2 )
      RMNODB = RICEO
      RMXODB = SQRT( CORNDB(1,5,19)**2 + CORNDB(2,5,19)**2 )

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  Middle-Angle modules second
C

      IFLAG = ALGTDB(LDBSDG,'MMTGMMAGMMSG',IRUNDG)
      IF (IFLAG.EQ.0) THEN
        CALL DWRT(' ERROR: unable to read MUON DET geometry data!')
        CALL DWRT('        source of error : ALGTDB.')
        RETURN
      END IF

      KMMTG = IW( NAMIND('MMTG'))
      KMMAG = IW( NAMIND('MMAG'))
      KMMSG = IW( NAMIND('MMSG'))

      DO I = 1 , NSLTM

        ITYPDM(I) = ITABL( KMMSG, I, JMMSK2 )
        K1        = ITABL( KMMSG, I, JMMSK1 )
        DE        = RTABL( KMMSG, I, JMMSDE )
        ZC        = RTABL( KMMSG, I, JMMSZC )
        VO        = ITABL( KMMSG, I, JMMSVO )
        RC        = RTABL( KMMSG, I, JMMSRC )

        Z0 = RTABL( KMMAG, K1, JMMAZ0 )
        Z1 = RTABL( KMMAG, K1, JMMAZ1 )
        TH = RTABL( KMMAG, K1, JMMATH )
        TB = RTABL( KMMAG, K1, JMMATB )

        FI = 360./12 * ( VO - 1 )

        IF (ITYPDM(I).EQ. 1) THEN
C--------------------------------------------------------------------------
C type 1:
C
          WIDE  = RTABL( KMMTG, 1,JMMTRB )
          ZLONG = RTABL( KMMTG, 1,JMMTZB )

          IF (VO.EQ.1 .OR. VO.EQ.7) THEN
            RIC = RC + Z0 - TB
            ROC = RC + Z0
          ELSE
            RIC = RC - Z0
            ROC = RC - Z0 + TB
          END IF

          IF (ZC.GE.0) THEN
            FAC = 1.
          ELSE
            FAC = -1.
          END IF

          DO J=1,2
            CORNDM(3,J  ,I) =  FAC*ZLONG/2 + ZC
            CORNDM(3,4+J,I) =  FAC*ZLONG/2 + ZC
            CORNDM(3,2+J,I) = -FAC*ZLONG/2 + ZC
            CORNDM(3,6+J,I) = -FAC*ZLONG/2 + ZC
          END DO

          C = COSD(FI)
          S = SIND(FI)

          CORNDM(1,1,I)=RIC*C+( WIDE/2-DE)*S
          CORNDM(2,1,I)=RIC*S-( WIDE/2-DE)*C
          CORNDM(1,2,I)=RIC*C+(-WIDE/2-DE)*S
          CORNDM(2,2,I)=RIC*S-(-WIDE/2-DE)*C
          CORNDM(1,3,I)=RIC*C+(-WIDE/2-DE)*S
          CORNDM(2,3,I)=RIC*S-(-WIDE/2-DE)*C
          CORNDM(1,4,I)=RIC*C+( WIDE/2-DE)*S
          CORNDM(2,4,I)=RIC*S-( WIDE/2-DE)*C
          CORNDM(1,5,I)=ROC*C+( WIDE/2-DE)*S
          CORNDM(2,5,I)=ROC*S-( WIDE/2-DE)*C
          CORNDM(1,6,I)=ROC*C+(-WIDE/2-DE)*S
          CORNDM(2,6,I)=ROC*S-(-WIDE/2-DE)*C
          CORNDM(1,7,I)=ROC*C+(-WIDE/2-DE)*S
          CORNDM(2,7,I)=ROC*S-(-WIDE/2-DE)*C
          CORNDM(1,8,I)=ROC*C+( WIDE/2-DE)*S
          CORNDM(2,8,I)=ROC*S-( WIDE/2-DE)*C

        ELSE IF (ITYPDM(I).EQ. 2) THEN
C--------------------------------------------------------------------------
C type 2:
C
          WIDE  = RTABL( KMMTG, 2,JMMTRB )
          ZLONG = RTABL( KMMTG, 2,JMMTZB )

          RIC = RC - Z0
          ROC = RC - Z0 + TB

          IF (ZC.GE.0) THEN
            FAC = 1.
          ELSE
            FAC = -1.
          END IF

          DO J=1,2
            CORNDM(3,J  ,I) =  FAC*ZLONG/2 + ZC
            CORNDM(3,4+J,I) =  FAC*ZLONG/2 + ZC
            CORNDM(3,2+J,I) = -FAC*ZLONG/2 + ZC
            CORNDM(3,6+J,I) = -FAC*ZLONG/2 + ZC
          END DO

          C = COSD(FI)
          S = SIND(FI)

          CORNDM(1,1,I)=RIC*C+( WIDE/2-DE)*S
          CORNDM(2,1,I)=RIC*S-( WIDE/2-DE)*C
          CORNDM(1,2,I)=RIC*C+(-WIDE/2-DE)*S
          CORNDM(2,2,I)=RIC*S-(-WIDE/2-DE)*C
          CORNDM(1,3,I)=RIC*C+(-WIDE/2-DE)*S
          CORNDM(2,3,I)=RIC*S-(-WIDE/2-DE)*C
          CORNDM(1,4,I)=RIC*C+( WIDE/2-DE)*S
          CORNDM(2,4,I)=RIC*S-( WIDE/2-DE)*C
          CORNDM(1,5,I)=ROC*C+( WIDE/2-DE)*S
          CORNDM(2,5,I)=ROC*S-( WIDE/2-DE)*C
          CORNDM(1,6,I)=ROC*C+(-WIDE/2-DE)*S
          CORNDM(2,6,I)=ROC*S-(-WIDE/2-DE)*C
          CORNDM(1,7,I)=ROC*C+(-WIDE/2-DE)*S
          CORNDM(2,7,I)=ROC*S-(-WIDE/2-DE)*C
          CORNDM(1,8,I)=ROC*C+( WIDE/2-DE)*S
          CORNDM(2,8,I)=ROC*S-( WIDE/2-DE)*C

        ELSE IF (ITYPDM(I).EQ. 3) THEN
C--------------------------------------------------------------------------
C type 3:
C
          WIDE  = RTABL( KMMTG, 3,JMMTRB )
          ZLONG = RTABL( KMMTG, 3,JMMTZB )

          RIC = RC + Z0 - TB
          ROC = RC + Z0

          IF (ZC.GE.0) THEN
            FAC = 1.
          ELSE
            FAC = -1.
          END IF

          DO J=1,2
            CORNDM(3,J  ,I) =  FAC*ZLONG/2 + ZC
            CORNDM(3,4+J,I) =  FAC*ZLONG/2 + ZC
            CORNDM(3,2+J,I) = -FAC*ZLONG/2 + ZC
            CORNDM(3,6+J,I) = -FAC*ZLONG/2 + ZC
          END DO

          C = COSD(FI)
          S = SIND(FI)

          CORNDM(1,1,I)=RIC*C+( WIDE/2-DE)*S
          CORNDM(2,1,I)=RIC*S-( WIDE/2-DE)*C
          CORNDM(1,2,I)=RIC*C+(-WIDE/2-DE)*S
          CORNDM(2,2,I)=RIC*S-(-WIDE/2-DE)*C
          CORNDM(1,3,I)=RIC*C+(-WIDE/2-DE)*S
          CORNDM(2,3,I)=RIC*S-(-WIDE/2-DE)*C
          CORNDM(1,4,I)=RIC*C+( WIDE/2-DE)*S
          CORNDM(2,4,I)=RIC*S-( WIDE/2-DE)*C
          CORNDM(1,5,I)=ROC*C+( WIDE/2-DE)*S
          CORNDM(2,5,I)=ROC*S-( WIDE/2-DE)*C
          CORNDM(1,6,I)=ROC*C+(-WIDE/2-DE)*S
          CORNDM(2,6,I)=ROC*S-(-WIDE/2-DE)*C
          CORNDM(1,7,I)=ROC*C+(-WIDE/2-DE)*S
          CORNDM(2,7,I)=ROC*S-(-WIDE/2-DE)*C
          CORNDM(1,8,I)=ROC*C+( WIDE/2-DE)*S
          CORNDM(2,8,I)=ROC*S-( WIDE/2-DE)*C

          RMINI = RIC
          RMAXI = SQRT(CORNDM(1,5,I)**2 + CORNDM(2,5,I)**2)

        ELSE IF (ITYPDM(I).EQ. 4) THEN
C--------------------------------------------------------------------------
C type 4:
C
          WIDE  = RTABL( KMMTG, 4,JMMTRB )
          ZLONG = RTABL( KMMTG, 4,JMMTZB )

          RIC = RC - Z0
          ROC = RC - Z0 + TB

          IF (ZC.GE.0) THEN
            FAC = 1.
          ELSE
            FAC = -1.
          END IF

          DO J=1,2
            CORNDM(3,J  ,I) =  FAC*ZLONG/2 + ZC
            CORNDM(3,4+J,I) =  FAC*ZLONG/2 + ZC
            CORNDM(3,2+J,I) = -FAC*ZLONG/2 + ZC
            CORNDM(3,6+J,I) = -FAC*ZLONG/2 + ZC
          END DO

          C = COSD(FI)
          S = SIND(FI)

          CORNDM(1,1,I)=RIC*C+( WIDE/2-DE)*S
          CORNDM(2,1,I)=RIC*S-( WIDE/2-DE)*C
          CORNDM(1,2,I)=RIC*C+(-WIDE/2-DE)*S
          CORNDM(2,2,I)=RIC*S-(-WIDE/2-DE)*C
          CORNDM(1,3,I)=RIC*C+(-WIDE/2-DE)*S
          CORNDM(2,3,I)=RIC*S-(-WIDE/2-DE)*C
          CORNDM(1,4,I)=RIC*C+( WIDE/2-DE)*S
          CORNDM(2,4,I)=RIC*S-( WIDE/2-DE)*C
          CORNDM(1,5,I)=ROC*C+( WIDE/2-DE)*S
          CORNDM(2,5,I)=ROC*S-( WIDE/2-DE)*C
          CORNDM(1,6,I)=ROC*C+(-WIDE/2-DE)*S
          CORNDM(2,6,I)=ROC*S-(-WIDE/2-DE)*C
          CORNDM(1,7,I)=ROC*C+(-WIDE/2-DE)*S
          CORNDM(2,7,I)=ROC*S-(-WIDE/2-DE)*C
          CORNDM(1,8,I)=ROC*C+( WIDE/2-DE)*S
          CORNDM(2,8,I)=ROC*S-( WIDE/2-DE)*C

        ELSE IF (ITYPDM(I).EQ. 5) THEN
C--------------------------------------------------------------------------
C type 5:
C
          WIDE  = RTABL( KMMTG, 5,JMMTRB )
          ZLONG = RTABL( KMMTG, 5,JMMTZB )

          RIC = RC + Z1 - TH
          ROC = RC + Z1

          IF (ZC.GE.0) THEN
            FAC = 1.
          ELSE
            FAC = -1.
          END IF

          DO J=1,2
            CORNDM(3,J  ,I) =  FAC*ZLONG/2 + ZC
            CORNDM(3,4+J,I) =  FAC*ZLONG/2 + ZC
            CORNDM(3,2+J,I) = -FAC*ZLONG/2 + ZC
            CORNDM(3,6+J,I) = -FAC*ZLONG/2 + ZC
          END DO

          C = COSD(FI)
          S = SIND(FI)

          CORNDM(1,1,I)=RIC*C+( WIDE/2-DE)*S
          CORNDM(2,1,I)=RIC*S-( WIDE/2-DE)*C
          CORNDM(1,2,I)=RIC*C+(-WIDE/2-DE)*S
          CORNDM(2,2,I)=RIC*S-(-WIDE/2-DE)*C
          CORNDM(1,3,I)=RIC*C+(-WIDE/2-DE)*S
          CORNDM(2,3,I)=RIC*S-(-WIDE/2-DE)*C
          CORNDM(1,4,I)=RIC*C+( WIDE/2-DE)*S
          CORNDM(2,4,I)=RIC*S-( WIDE/2-DE)*C
          CORNDM(1,5,I)=ROC*C+( WIDE/2-DE)*S
          CORNDM(2,5,I)=ROC*S-( WIDE/2-DE)*C
          CORNDM(1,6,I)=ROC*C+(-WIDE/2-DE)*S
          CORNDM(2,6,I)=ROC*S-(-WIDE/2-DE)*C
          CORNDM(1,7,I)=ROC*C+(-WIDE/2-DE)*S
          CORNDM(2,7,I)=ROC*S-(-WIDE/2-DE)*C
          CORNDM(1,8,I)=ROC*C+( WIDE/2-DE)*S
          CORNDM(2,8,I)=ROC*S-( WIDE/2-DE)*C

        ELSE IF (ITYPDM(I).EQ. 6) THEN
C--------------------------------------------------------------------------
C type 6:
C
          WIDE  = RTABL( KMMTG, 6,JMMTRB )
          ZLONG = RTABL( KMMTG, 6,JMMTZB )

          IF (VO.EQ.1 .OR. VO.EQ.7) THEN
            RIC = RC + Z0 - TB
            ROC = RC + Z0
          ELSE
            RIC = RC - Z0
            ROC = RC - Z0 + TB
          END IF

          IF (ZC.GE.0) THEN
            FAC = 1.
          ELSE
            FAC = -1.
          END IF

          DO J=1,2
            CORNDM(3,J  ,I) =  FAC*ZLONG/2 + ZC
            CORNDM(3,4+J,I) =  FAC*ZLONG/2 + ZC
            CORNDM(3,2+J,I) = -FAC*ZLONG/2 + ZC
            CORNDM(3,6+J,I) = -FAC*ZLONG/2 + ZC
          END DO

          C = COSD(FI)
          S = SIND(FI)

          CORNDM(1,1,I)=RIC*C+( WIDE/2-DE)*S
          CORNDM(2,1,I)=RIC*S-( WIDE/2-DE)*C
          CORNDM(1,2,I)=RIC*C+(-WIDE/2-DE)*S
          CORNDM(2,2,I)=RIC*S-(-WIDE/2-DE)*C
          CORNDM(1,3,I)=RIC*C+(-WIDE/2-DE)*S
          CORNDM(2,3,I)=RIC*S-(-WIDE/2-DE)*C
          CORNDM(1,4,I)=RIC*C+( WIDE/2-DE)*S
          CORNDM(2,4,I)=RIC*S-( WIDE/2-DE)*C
          CORNDM(1,5,I)=ROC*C+( WIDE/2-DE)*S
          CORNDM(2,5,I)=ROC*S-( WIDE/2-DE)*C
          CORNDM(1,6,I)=ROC*C+(-WIDE/2-DE)*S
          CORNDM(2,6,I)=ROC*S-(-WIDE/2-DE)*C
          CORNDM(1,7,I)=ROC*C+(-WIDE/2-DE)*S
          CORNDM(2,7,I)=ROC*S-(-WIDE/2-DE)*C
          CORNDM(1,8,I)=ROC*C+( WIDE/2-DE)*S
          CORNDM(2,8,I)=ROC*S-( WIDE/2-DE)*C

        ELSE IF (ITYPDM(I).EQ. 7) THEN
C--------------------------------------------------------------------------
C type 7:
C
          WIDE  = RTABL( KMMTG, 7,JMMTRB )
          ZLONG = RTABL( KMMTG, 7,JMMTZB )

          RIC = RC - Z0
          ROC = RC - Z0 + TB

          IF (ZC.GE.0) THEN
            FAC = 1.
          ELSE
            FAC = -1.
          END IF

          DO J=1,2
            CORNDM(3,J  ,I) =  FAC*ZLONG/2 + ZC
            CORNDM(3,4+J,I) =  FAC*ZLONG/2 + ZC
            CORNDM(3,2+J,I) = -FAC*ZLONG/2 + ZC
            CORNDM(3,6+J,I) = -FAC*ZLONG/2 + ZC
          END DO

          C = COSD(FI)
          S = SIND(FI)

          CORNDM(1,1,I)=RIC*C+( WIDE/2-DE)*S
          CORNDM(2,1,I)=RIC*S-( WIDE/2-DE)*C
          CORNDM(1,2,I)=RIC*C+(-WIDE/2-DE)*S
          CORNDM(2,2,I)=RIC*S-(-WIDE/2-DE)*C
          CORNDM(1,3,I)=RIC*C+(-WIDE/2-DE)*S
          CORNDM(2,3,I)=RIC*S-(-WIDE/2-DE)*C
          CORNDM(1,4,I)=RIC*C+( WIDE/2-DE)*S
          CORNDM(2,4,I)=RIC*S-( WIDE/2-DE)*C
          CORNDM(1,5,I)=ROC*C+( WIDE/2-DE)*S
          CORNDM(2,5,I)=ROC*S-( WIDE/2-DE)*C
          CORNDM(1,6,I)=ROC*C+(-WIDE/2-DE)*S
          CORNDM(2,6,I)=ROC*S-(-WIDE/2-DE)*C
          CORNDM(1,7,I)=ROC*C+(-WIDE/2-DE)*S
          CORNDM(2,7,I)=ROC*S-(-WIDE/2-DE)*C
          CORNDM(1,8,I)=ROC*C+( WIDE/2-DE)*S
          CORNDM(2,8,I)=ROC*S-( WIDE/2-DE)*C

        ELSE IF (ITYPDM(I).EQ. 8) THEN
C--------------------------------------------------------------------------
C type 8:
C
          WIDE  = RTABL( KMMTG, 8,JMMTRB )
          ZLONG = RTABL( KMMTG, 8,JMMTZB )

          RIC = RC + Z0 - TB
          ROC = RC + Z0

          IF (ZC.GE.0) THEN
            FAC = 1.
          ELSE
            FAC = -1.
          END IF

          DO J=1,2
            CORNDM(3,J  ,I) =  FAC*ZLONG/2 + ZC
            CORNDM(3,4+J,I) =  FAC*ZLONG/2 + ZC
            CORNDM(3,2+J,I) = -FAC*ZLONG/2 + ZC
            CORNDM(3,6+J,I) = -FAC*ZLONG/2 + ZC
          END DO

          C = COSD(FI)
          S = SIND(FI)

          CORNDM(1,1,I)=RIC*C+( WIDE/2-DE)*S
          CORNDM(2,1,I)=RIC*S-( WIDE/2-DE)*C
          CORNDM(1,2,I)=RIC*C+(-WIDE/2-DE)*S
          CORNDM(2,2,I)=RIC*S-(-WIDE/2-DE)*C
          CORNDM(1,3,I)=RIC*C+(-WIDE/2-DE)*S
          CORNDM(2,3,I)=RIC*S-(-WIDE/2-DE)*C
          CORNDM(1,4,I)=RIC*C+( WIDE/2-DE)*S
          CORNDM(2,4,I)=RIC*S-( WIDE/2-DE)*C
          CORNDM(1,5,I)=ROC*C+( WIDE/2-DE)*S
          CORNDM(2,5,I)=ROC*S-( WIDE/2-DE)*C
          CORNDM(1,6,I)=ROC*C+(-WIDE/2-DE)*S
          CORNDM(2,6,I)=ROC*S-(-WIDE/2-DE)*C
          CORNDM(1,7,I)=ROC*C+(-WIDE/2-DE)*S
          CORNDM(2,7,I)=ROC*S-(-WIDE/2-DE)*C
          CORNDM(1,8,I)=ROC*C+( WIDE/2-DE)*S
          CORNDM(2,8,I)=ROC*S-( WIDE/2-DE)*C

          RMINO = RIC
          RMAXO = SQRT(CORNDM(1,5,I)**2 + CORNDM(2,5,I)**2)

        ELSE IF (ITYPDM(I).EQ. 9) THEN
C--------------------------------------------------------------------------
C type 9:
C
          WIDE  = RTABL( KMMTG, 9,JMMTRB )
          ZLONG = RTABL( KMMTG, 9,JMMTZB )

          RIC = RC - Z0
          ROC = RC - Z0 + TB

          IF (ZC.GE.0) THEN
            FAC = 1.
          ELSE
            FAC = -1.
          END IF

          DO J=1,2
            CORNDM(3,J  ,I) =  FAC*ZLONG/2 + ZC
            CORNDM(3,4+J,I) =  FAC*ZLONG/2 + ZC
            CORNDM(3,2+J,I) = -FAC*ZLONG/2 + ZC
            CORNDM(3,6+J,I) = -FAC*ZLONG/2 + ZC
          END DO

          C = COSD(FI)
          S = SIND(FI)

          CORNDM(1,1,I)=RIC*C+( WIDE/2-DE)*S
          CORNDM(2,1,I)=RIC*S-( WIDE/2-DE)*C
          CORNDM(1,2,I)=RIC*C+(-WIDE/2-DE)*S
          CORNDM(2,2,I)=RIC*S-(-WIDE/2-DE)*C
          CORNDM(1,3,I)=RIC*C+(-WIDE/2-DE)*S
          CORNDM(2,3,I)=RIC*S-(-WIDE/2-DE)*C
          CORNDM(1,4,I)=RIC*C+( WIDE/2-DE)*S
          CORNDM(2,4,I)=RIC*S-( WIDE/2-DE)*C
          CORNDM(1,5,I)=ROC*C+( WIDE/2-DE)*S
          CORNDM(2,5,I)=ROC*S-( WIDE/2-DE)*C
          CORNDM(1,6,I)=ROC*C+(-WIDE/2-DE)*S
          CORNDM(2,6,I)=ROC*S-(-WIDE/2-DE)*C
          CORNDM(1,7,I)=ROC*C+(-WIDE/2-DE)*S
          CORNDM(2,7,I)=ROC*S-(-WIDE/2-DE)*C
          CORNDM(1,8,I)=ROC*C+( WIDE/2-DE)*S
          CORNDM(2,8,I)=ROC*S-( WIDE/2-DE)*C

        END IF

      END DO
C
C  max & min radii are calculated for module-type 3 ( for inner layer )
C  and module-type 8 ( for outer layer ) due to their large r-phi dimension
C  ( module-type 5 is not taken into account !)
C
      RMNIDM = RMINI
      RMXIDM = RMAXI
      RMNODM = RMINO
      RMXODM = RMAXO


C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  and now , last but not least , the end-caps
C

      IFLAG = ALGTDB(LDBSDG,'METGMECGMESG',IRUNDG)
      IF (IFLAG.EQ.0) THEN
        CALL DWRT(' ERROR: unable to read MUON DET geometry data!')
        CALL DWRT('        source of error : ALGTDB.')
        RETURN
      END IF

      KMETG = IW( NAMIND('METG'))
      KMECG = IW( NAMIND('MECG'))
      KMESG = IW( NAMIND('MESG'))

      RMNIDE = 40.
      RMXIDE = 0.
      RMNODE = 40.
      RMXODE = 0.

      DO I = 1 , NSLTE

        ITYPDE(I) = ITABL( KMESG, I, JMESK2 )
        K1        = ITABL( KMESG, I, JMESK1 )
        XC        = RTABL( KMESG, I, JMESXC )
        YC        = RTABL( KMESG, I, JMESYC )
        ZC        = RTABL( KMESG, I, JMESZC )

        ZI = RTABL( KMECG, K1, JMECZI )
        TH = RTABL( KMECG, K1, JMECTH )

        XBOX = RTABL( KMETG, ITYPDE(I), JMETXB )
        YBOX = RTABL( KMETG, ITYPDE(I), JMETYB )

        IF (ZC.GE.0) THEN
          FAC = 1.
        ELSE
          FAC = -1.
        END IF

        DO J=1 , 4
          CORNDE(3,J  ,I) = ZC + FAC*( 0-ZI)
          CORNDE(3,J+4,I) = ZC + FAC*(TH-ZI)
        END DO

        CORNDE(1,1,I) = XC + XBOX/2
        CORNDE(2,1,I) = YC - YBOX/2
        CORNDE(1,2,I) = XC - XBOX/2
        CORNDE(2,2,I) = YC - YBOX/2
        CORNDE(1,3,I) = XC - XBOX/2
        CORNDE(2,3,I) = YC + YBOX/2
        CORNDE(1,4,I) = XC + XBOX/2
        CORNDE(2,4,I) = YC + YBOX/2
        CORNDE(1,5,I) = XC + XBOX/2
        CORNDE(2,5,I) = YC - YBOX/2
        CORNDE(1,6,I) = XC - XBOX/2
        CORNDE(2,6,I) = YC - YBOX/2
        CORNDE(1,7,I) = XC - XBOX/2
        CORNDE(2,7,I) = YC + YBOX/2
        CORNDE(1,8,I) = XC + XBOX/2
        CORNDE(2,8,I) = YC + YBOX/2

        IF ( MOD(I,NSLHE).LE.NSLIE ) THEN
          DO K=1,4
            R = SQRT(CORNDE(1,K,I)**2 + CORNDE(2,K,I)**2)
            IF (R.GT.RMXIDE)  RMXIDE=R
            IF (R.LT.RMNIDE)  RMNIDE=R
          END DO
        ELSE
          DO K=1,4
            R = SQRT(CORNDE(1,K,I)**2 + CORNDE(2,K,I)**2)
            IF (R.GT.RMXODE)  RMXODE=R
            IF (R.LT.RMNODE)  RMNODE=R
          END DO
        END IF

      END DO
      END
*DK DGDHYX
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DGDHYX(H,V)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION H(5),V(5),Q(23)
      LOGICAL FSTRT
      DATA FSTRT/.TRUE./
      IF(FSTRT) THEN
        DRTOT=SQRT((H(3)-H(2))**2+(V(3)-V(2))**2)
        DR1=0.5
        DR23=DRTOT-10.
        D=(DR23-DR1)/22.
        DR=DR1
        R1=SQRT(H(2)**2+V(2)**2)
        DO 700 L=1,23
          Q(L)=1.+DR/R1
          DR=DR+D
  700   CONTINUE
        FSTRT=.FALSE.
      END IF
      CALL DQLEVL(KCHCDD)
      DLINDD=PDCODD(2,LIGLDD)
      DO 710 L=1,23
        H1=H(2)*Q(L)
        V1=V(2)*Q(L)
        H2=H(1)*Q(L)
        V2=V(1)*Q(L)
        CALL DQL2E(H1,V1,H2,V2)
  710 CONTINUE
      DLINDD=PDCODD(2,LIDTDD)
      END
*DK DGDHRB
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DGDHRB(H,V)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION H(5),V(5),Q(23)
      LOGICAL FSTRT
      DATA FSTRT/.TRUE./
      IF(FSTRT) THEN
        DRTOT=V(3)-V(1)
        DR1=0.5
        DR23=DRTOT-10.
        D=(DR23-DR1)/22.
        DR=DR1
        R1=V(1)
        DO 700 L=1,23
          Q(L)=1.+DR/R1
          DR=DR+D
  700   CONTINUE
        FSTRT=.FALSE.
      END IF
      CALL DQLEVL(KCHCDD)
      DLINDD=PDCODD(2,LIGLDD)
      H1=H(1)
      H2=H(2)
      DO 710 L=1,23
        V1=V(2)*Q(L)
        V2=V(1)*Q(L)
        CALL DQL2E(H1,V1,H2,V2)
  710 CONTINUE
      DLINDD=PDCODD(2,LIDTDD)
      END
*DK DGDHRE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DGDHRE(H,V)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION H(7),V(7),Q(22)
      LOGICAL FSTRT
      DATA FSTRT/.TRUE./
      DATA L1/7/
      IF(FSTRT) THEN
        DZTOT=H(1)-H(5)
        DZ1=0.5
        DZ22=DZTOT-10.
        D=(DZ22-DZ1)/21.
        DZ=DZ1
        Z1=H(5)
        DO 700 L=1,22
          Q(L)=1.+DZ/Z1
          DZ=DZ+D
  700   CONTINUE
        FSTRT=.FALSE.
      END IF
      CALL DQLEVL(KCHCDD)
      DLINDD=PDCODD(2,LIGLDD)
      V1=V(6)
      V2=V(4)
      DO 710 L=1,L1
        H1=H(5)*Q(L)
        H2=H(5)*Q(L)
        CALL DQL2E(H1,V1,H2,V2)
  710 CONTINUE
      V2=V(2)
      DO 720 L=L1+1,22
        H1=H(5)*Q(L)
        H2=H(5)*Q(L)
        CALL DQL2E(H1,V1,H2,V2)
  720 CONTINUE
      DLINDD=PDCODD(2,LIDTDD)
      END
