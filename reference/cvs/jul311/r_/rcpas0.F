      SUBROUTINE RCPAS0
C----------------------------------------------------------------------
C!  - Write TDPV bank in PASS0 summary file
C!
C!   Author   :- E. Lancon             25-FEB-1992
C!   Modified  :- I. Tomalin           7-NOV-1995
C!               Use TLAS bank correctly.
C!   Modified  :- A. Bonissent         10-NOV-1995
C!               For VDET95, avoid messing around with VHOT and TOHV.
C!   Modified  :- I. Tomalin           26-JUN-1996
C!               If using new VDET and requested by PVGG card,
C!               PASS0 will take V_d from VDET/gamma-gamma.
C!   Modified  :- M. Cattaneo           8-DEC-1999
C!               Fix IDAT and format statement for y2k
C!
C!   Description
C!   ===========
C!      Open The PASS0 summary file, defined by the PAS0 data card.
C!      Check that TDPV exists on input.
C!        Determine V_d using laser, Delta(t0) and VDET/gamma-gamma.
C!        Find hot channels in VDET.
C!        Write PASS0 conditions and results to JPAS bank.
C!        If at high energy and requested by PVGG card then
C!          Create TDPV using VDET/gamma-gamma events.
C!        Else
C!          Create TDPV using Delta(t0).
C!        Endif
C!        If too few events for above, then create TDPV from TLAS.
C!        If laser shot too old, then just fill TDPV from reference run.
C!      Write TDPV on PAS0 summary file.
C!      If a new TPDV bank is created the previous one is dropped.
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "tpgpar.h"
#include "jsumjj.h"
#include "tdpvjj.h"
#include "tlasjj.h"
#include "tvvdjj.h"
#include "rcurnt.h"
#include "rlunit.h"
#include "rpass0.h"
#include "jpasjj.h"
#include "rjulver.h"
C +y2k
      COMMON /SLATE/ ISL(40)
C -y2k
      PARAMETER( NECL =  30 )
      DIMENSION IECLS(NECL)
      EXTERNAL NDROP
      INTEGER  NDROP
      LOGICAL USEVGG, LASOK, DT0OK, VGGOK, UPDAT
      EXTERNAL GTSTUP,VDYEAR
      INTEGER  GTSTUP,VDYEAR
C Min. number of VDET coords in each side required to trust V_d from TVVD.
C Also min. clearance in sigma from edge of window boundary.
      PARAMETER(MINOBS=50,CLRCUT=2.0)
#include "bmacro.h"
C----------------------------------------------------------------------
C
C?   Get Edir statistics
C
      CALL ALSUMGT (NECL,IECLS)
C
      CALL BDROP (IW,'JPAS')
      CALL BLIST (IW,'T=','0')
C
C?   Get alephlib and data base version
C?   And date and time
C
      CALL ALVERS (ALVER)
      CALL ADBVER (IDBVE, IDBDA)
C +y2k
      CALL DATIME(JD,ITIM)
      IDAY = ISL(1)*10000 + MOD(JD,10000)
C -y2k
C
C?   Get PAS0 output file, open it with 72 characters
C
      LREC  = 72
      LDUM = MALREC ('CARD',LREC)
      CALL AGTFIL ('PAS0', 'WRIT', LPAS0L, IER)
      IF (IER.NE.0) GOTO 998
C
C? Should TPC drift velocity be obtained from VDET & gamma/gamma events ?
      IVSET = GTSTUP('VD',IRUNRC)
      USEVGG = IW(NAMIND('PVGG')).GT.0.AND.IVSET.GE.8
C
      WRITE (LPAS0L,'(A)')
     &  'PON     '
      WRITE (LPAS0L,'(A,I6,A,I5,A)')
     &  '*PASS0 --- Summary for Run ', IRUNRC, ' -', IECLS(16),
     &  ' Z read '
C +y2k
      WRITE (LPAS0L,'(A,I9,A,I7)')
     &  '*PASS0 Date ', IDAY, ' Time ', ITIM
      WRITE (LPAS0L,'(A,F6.2,A,F6.2,A,I7,A,I7)')
     &  '*PASS0 Julia ', RJVERS, ' Alephlib ', ALVER, ' Daf ',
     &  IDBVE, ' from ', IDBDA
C -y2k
      WRITE (LPAS0L,'(A,I6,A,F7.2,A)')
     &  '*PASS0 Cuts: Min. nb of evts :', NPEVMI,
     &  ' - TLAS max. elapsed time : ', TMTLAS, ' h.'
      WRITE (LPAS0L,'(A,L4)')
     &  '*PASS0 Cuts: Request V_d from VDET/gamma-gamma ? ',USEVGG
C
C?   Get old V_d from reference run.
      KTDPV = IW(NAMIND('TDPV'))
      IF ( KTDPV.GT.0 ) THEN
        NTDPV = IW(KTDPV-2)
        VOLD = 0.5*(VIPAS0(3,1) + VIPAS0(3,2))
      ELSE
        WRITE (LPAS0L,'(A)') '*PASS0 ERROR: no TDPV found on input.'
        CALL RERROR ('RCPAS0', -1, 'No TDPV found on input.')
        GOTO 997
      ENDIF
C
C?   Compute new TPC drift velocity from Delta(t0).
C
      DT0OK = .FALSE.
      KJSUM = IW(NAMIND('JSUM'))
      IF (KJSUM.GT.0) THEN
        NEVT = ITABL (KJSUM,1,JJSUTN)
      ELSE
        WRITE (LPAS0L,'(A)') '*PASS0 t0 diff.: no JSUM bank found.'
        NEVT = 0
      ENDIF
      IF ( NEVT.GT.0 ) THEN
        T0    = RTABL(KJSUM,1,JJSUTS)/FLOAT(NEVT)
        ST0   = SQRT(ABS(RTABL(KJSUM,1,JJSUTV)/FLOAT(NEVT) - T0**2))
        ERR   = ST0/SQRT(FLOAT(NEVT))
        VNEW  = VOLD + T0*VOLD*VOLD/220.
        EVNEW = ERR*VOLD*VOLD/220.
        WRITE (LPAS0L,'(A,F8.5,A,F8.5)')
     &      '*PASS0 t0 diff.: V_d = ', VNEW, '+-', EVNEW
        WRITE(LPAS0L,'(A,I5,A)')
     &      '*PASS0 t0 diff.: 0.5*(TPC end-A - TPC end-B) for ',
     &      NEVT,' events'
        WRITE(LPAS0L,'(A,F10.5,A,F7.5,A,F7.5,A)')
     &      '*PASS0 t0 diff.=', T0, ' +- ', ERR,
     &      ' mus; rms= ', ST0, ' mus'
      ELSE
        T0    = -9999.
        ST0   = -9999.
        ERR   = -9999.
        VNEW  = -9999.
        EVNEW = -9999.
      ENDIF
      IF ( NEVT.GE.NPEVMI ) THEN
        DT0OK = .TRUE.
      ELSE
        WRITE (LPAS0L,'(A)') '*PASS0 t0 diff.: Too few events.'
      ENDIF
C
C?   Compute new TPC drift velocity from VDET/gamma-gamma.
C
      VGGOK = .FALSE.
      IF (USEVGG) THEN
        KTVVD = NLINK('TVVD',IRUNRC)
        IF ( KTVVD.LE.0 ) THEN
          WRITE (LPAS0L,'(A)') '*PASS0 TVVD: Too few VDET coords.'     
        ELSE 
          VNEWGG = RW(KTVVD + LMHLEN + JTVVVZ)
          ERRVGG = RW(KTVVD + LMHLEN + JTVVER)
          DELVGG = VNEWGG - VOLD
          T0GG   = DELVGG*220./VOLD**2
          NVCOGG = IW(KTVVD + LMHLEN + JTVVNU)
          NEXPGG = IW(KTVVD + LMHLEN + JTVVEX)
          CHI2GG = RW(KTVVD + LMHLEN + JTVVC2)
          CLRGG  = RW(KTVVD + LMHLEN + JTVVCL)
          WRITE (LPAS0L,'(A,F8.5,A,F8.5)')
     &      '*PASS0 TVVD: V_d = ', VNEWGG, '+-', ERRVGG
          WRITE (LPAS0L,'(A,I6,A1,I6,A,F6.1,A,F6.1)')
     &      '*PASS0 TVVD: Obs./Exp. VDET coor ',NVCOGG,'/',NEXPGG,
     &      ' CHI**2 ',CHI2GG,' Clearance ',CLRGG
C?   Apply cuts to ensure that this drift velocity is reliable.
          IF (NVCOGG.GT.MINOBS.AND.CLRGG.GT.CLRCUT) THEN
            VGGOK = .TRUE. 
          ELSE
            WRITE (LPAS0L,'(A)') '*PASS0 TVVD: Too few VDET coords.'
          ENDIF
        ENDIF
      ENDIF
C
C?   Compute new TPC drift velocity from TPC laser.
C
      LASOK = .FALSE.
      KTLAS = IW(NAMIND('TLAS'))
      IF (KTLAS.GT.0 .AND. ABS(RTABL(KTLAS,1,JTLANV)).GT.1.0E-3) THEN
        VTLAS  = RTABL (KTLAS,1,JTLANV)
        EVTLAS = RTABL (KTLAS,1,JTLANE)
        ELAPS  = RTABL (KTLAS,1,JTLAET)
        WRITE (LPAS0L,'(A,F8.5,A,F8.5)')
     &    '*PASS0 Laser: V_d = ', VTLAS, '+-', EVTLAS
        IF ( ELAPS.LT.TMTLAS ) THEN
          LASOK = .TRUE.
          WRITE (LPAS0L,'(A,F6.0,A)')
     &      '*PASS0 Laser: Last measurement taken : ', ELAPS,
     &      ' hours ago '
        ELSE
          WRITE (LPAS0L,'(A,F6.0,A)')
     &      '*PASS0 Laser: TLAS too OLD : ', ELAPS, ' hours'
        ENDIF
      ELSE
        VTLAS  = 9999.
        EVTLAS = 9999.
        ELAPS  = 9999.
        WRITE(LPAS0L,'(A)') '*PASS0 Laser: No good TLAS found on input.'
      ENDIF
C
C?   Get VHOT statistics
C?   If VHOT is Empty (Not enough events drop it and restore previous one)
C
      KVHOT   = IW(NAMIND('VHOT'))
      NHOTCU  = LROWS(KVHOT)
      WRITE(LPAS0L,'(A,I6,A,I6)')
     &  '*PASS0 VDET VHOT: No. of channels in this run = ', NHOTCU,
     &  ' in ref. run = ', NHOTBF
      IF (VDYEAR().LT.95) THEN
        KTOHV = IW(NAMIND ('TOHV'))
        NTOHV = IW(KTOHV-2)
        IF ( NHOTCU.LE.0 ) THEN
          CALL BDROP (IW,'VHOT')
          CALL BSWAP (IW,'TOHV', 'VHOT')
          WRITE(LPAS0L,'(A,I6)')
     &      '*PASS0 VDET VHOT taken from reference run ',NTOHV
        ENDIF
      ENDIF
C
C?   Write PASS0 Summary bank
C
      CALL AUBOS ('JPAS', IRUNRC, LJPASA+LMHLEN, KJPAS, IGARB1)
      IF ( IGARB1.EQ.2 ) THEN
        WRITE (LPAS0L,'(A)')
     &        '*PASS0 Unable to create JPAS '
      ELSE
        IW (KJPAS+LMHCOL) = LJPASA
        IW (KJPAS+LMHROW) = 1
        KS = KJPAS+LMHLEN
        IW (KS+JJPAZO)   = IECLS(16)
        IW (KS+JJPANU)   = NEVT
        IW (KS+JJPANC)   = NPEVMI
        RW (KS+JJPADT)   = T0
        RW (KS+JJPADT+1) = ERR
        RW (KS+JJPADT+2) = ST0
        RW (KS+JJPAVT)   = VTLAS
        RW (KS+JJPAVO)   = VOLD
        RW (KS+JJPAVN)   = VNEW
        RW (KS+JJPAVN+1) = EVNEW
        RW (KS+JJPATC)   = TMTLAS
        RW (KS+JJPAET)   = ELAPS
        IW (KS+JJPADA)   = IDAY
        IW (KS+JJPATI)   = ITIM
        IW (KS+JJPANM)   = NPEVMA
        IW (KS+JJPAHP)   = NHOTBF
        IW (KS+JJPAHC)   = NHOTCU
      ENDIF
C
C?  Decide which V_d measurement to use.
C
      UPDAT = .TRUE.
      IF (USEVGG.AND.VGGOK) THEN
        WRITE (LPAS0L,'(A)') '*PASS0 V_d taken from VDET/gamma-gamma.'
        VNEW  = VNEWGG
        EVNEW = ERRVGG
        T0    = T0GG
      ELSE IF (DT0OK) THEN
        WRITE (LPAS0L,'(A)') '*PASS0 V_d taken from Delta(t0).'
      ELSE IF (LASOK) THEN
        WRITE (LPAS0L,'(A)') '*PASS0 V_d taken from TPC laser.'
        VNEW  = VTLAS
        EVNEW = EVTLAS
        T0    = 0.0
      ELSE
        UPDAT = .FALSE.
      ENDIF
C
C Check that change in V_d was not suspiciously large.
C
      IF (UPDAT) THEN
        IF ( ABS(T0).GT.DT0CUT ) THEN
          WRITE (LPAS0L,'(A)')
     &      '*PASS0 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          WRITE (LPAS0L,'(A,F7.2,A)')
     &      '*PASS0 !!!!! t0 diff. too large > ', DT0CUT,' musec !!!!!'
          WRITE (LPAS0L,'(A)')
     &      '*PASS0 !!!!!    SOMETHING IS WRONG                  !!!!!'
          WRITE (LPAS0L,'(A)')
     &      '*PASS0 !!!!! Drift velocity not updated             !!!!!'
          WRITE (LPAS0L,'(A)')
     &      '*PASS0 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          UPDAT = .FALSE. 
        ENDIF
      ENDIF
C
      IF (UPDAT) THEN
        WRITE (LPAS0L,'(A,F8.5,A,F8.5,A,F8.5,A,F8.5)')
     &    '*PASS0: Vold', VOLD, ' Vnew', VNEW, '+-', EVNEW,
     &    ' Dv/v ', (VOLD-VNEW)/VOLD
      ELSE 
        WRITE (LPAS0L,'(A,I6)')
     &    '*PASS0 TPC Drift Velocity taken from reference run ',NTDPV 
      ENDIF
C
C?   create a new TDPV if there is enough events or
C?   if the TPAS is not too old
C?   Write a message if there is enough events to update the reference
C?   Do not drop TDPV if input run number = output one!
C
      IF ( UPDAT ) THEN
        CALL AUBOS ('TDPV', IRUNRC, 2*LTDPVA+LMHLEN, KTDPV, IGARB)
        IF ( IGARB.EQ.2 ) THEN
          WRITE (LPAS0L,'(A)')
     &        '*PASS0 Unable to create TPDV '
          WRITE (LPAS0L,'(A,I6)')
     &        'TPC Drift Velocity taken from reference run : ', NTDPV
        ELSE
          IF ( NTDPV.NE.IRUNRC) IND = NDROP ('TDPV', NTDPV)
          IW (KTDPV+LMHCOL) = LTDPVA
          IW (KTDPV+LMHROW) = 2
          KS = KTDPV+LMHLEN
          RW (KS+JTDPDV)   = VIPAS0(1,1)
          RW (KS+JTDPDV+1) = VIPAS0(2,1)
          RW (KS+JTDPDV+2) = VNEW
          RW (KS+JTDPEZ)   = EVNEW
          KS = KS + LTDPVA
          RW (KS+JTDPDV)   = VIPAS0(1,2)
          RW (KS+JTDPDV+1) = VIPAS0(2,2)
          RW (KS+JTDPDV+2) = VNEW
          RW (KS+JTDPEZ)   = EVNEW
          IF ( NEVT.GE.NPEVMI ) WRITE (LPAS0L,'(A)')
     &        '*PASS0 ----> Reference Drift Velocity updated <---- '
        ENDIF
      ENDIF
C
C For VDET95, make sure that we do not mess around with VHOT and TOHV !
C
      IF (VDYEAR().EQ.95) PASLIS='JPASTDPV'
C
C?   Close the PAS0 file
C
      CALL BLIST(IW,'T+',PASLIS (1:LNBLNK(PASLIS)) )
      CALL BWRITE (IW,LPAS0L,'T')
      CALL BWRITE (IW,LPAS0L,'0')
  997 CONTINUE
      CALL ACLOSE (LPAS0L,IER)
      IF ( IER.NE.0 )
     &  CALL RERROR ('RCPAS0', -2, 'Unable to close PASS0 summary file')
      GOTO 999
C
  998 CONTINUE
      CALL RERROR ('RCPAS0', -3, 'Unable to open PASS0 summary file')
C
  999 RETURN
      END
#endif
