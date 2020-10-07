      SUBROUTINE VGCOMP(CompOffset, SelFlag)
C ---------------------------------------------------------------------
C! Create a list of components for the global VDet pat. rec.
CKEY VGLOB VDET TRACK
C
C      Paul Rensing   - 1-11-94
C
C Starting from the VM5C and VHMP banks, determine the lists of which
C     tracks belong in each component. The VCTL banks holds the list of
C     tracks (FRFT #) that are part of the component, while VCHx holds
C     the unique list of hits in layer x in the component.
C
C  INPUT:
C    integer CompOffset  - offset for numbering the components
C    integer SelFlag     - track selection flag
C    VM5C banks
C OUTPUT:
C    VCTL, VCHx banks; x=1-4
C ------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C  Function parameters
C
      INTEGER CompOffset, SelFlag
C
C  Global includes
C
#include "bcs.h"
#include "vglbcm.h"
C
#include "vm5cjj.h"
#include "fxtrjj.h"
#include "vctljj.h"
#include "vchljj.h"
#include "vhmpjj.h"
C
C  Function calls
C
      INTEGER NLINK,NBANK, NAMIND
      EXTERNAL NLINK,NBANK, NAMIND
C
C  Local variables
C
      integer namvm5c, NamFXTR
      INTEGER indvm5c, INDC, INDT, INDH, indFXTR
      INTEGER I, J, K, M, HIT
      INTEGER NTRK, ITRK
      INTEGER NCOMP
      integer indHList(NLAYER*NVIEW)
      integer layer, view, grk
      LOGICAL ADDED, repeat
      integer nremove, indrm
      integer no, trkflag
      Character*4 HitBnk(NLAYER*NVIEW)
      INTEGER NVCH1,NVCH2,NVCH3,NVCH4,KVCH1,KVCH2,KVCH3,KVCH4,JT
      INTEGER IRUN,IEVT,KVCTL,ITRKDP,JTRKDP,ITCDP,ICDP,KVTDP,ITC
      LOGICAL DOIT
C
      SAVE HitBnk, namvm5c, NamFXTR
C
#include "bmacrod.h"
C
      INTEGER JVIND, IL, IV
C
C Data Statements
C
      DATA NamVM5C/0/
      DATA HitBnk/'VCH1','VCH2','VCH3','VCH4'/
C
C  Inline functions
C
#include "bmacro.h"
C
      JVIND(IL, IV) = NVIEW*(IV-1) + IL
C
      if (NamVM5C .eq. 0) then
         NamVM5C = NAMIND('VM5C')
         NamFXTR = NAMIND('FXTR')
      end if
C
C we come here if some tracks were dropped.
C
 10   continue
C
C Clean out old banks
C
      CALL BDROP(IW, 'VGXX')
      CALL BDROP(IW, 'VHMP')
      CALL BDROP(IW, 'VCTL')
      CALL BDROP(IW, 'VCH1VCH2VCH3VCH4')
C
      indFXTR = IW(NamFXTR)
      if (indFXTR .le. 0) then
         call RERROR('VGCOMP', -1, ' Missing FXTR bank')
         goto 999
      end if
C
C Create the hit->track mapping
C
      repeat = .false.
      NTRK = 0
      CALL VHMAP(SelFlag)
C
C Loop through all the VM[UW]C banks and store the indices
C 
C This is a temporary bank! 
      INDT = NBANK('VGXX', 0, LROWS(indFXTR))
      if (indt .eq. 0) then
         call rerror('VGCOMP', -2, ' Out of memory')
         goto 999
      end if
      call VZERO(IW(indt+1), LROWS(indFXTR) )
C
C Start chaining through the VM5C banks
C
      indvm5c = IW(NamVM5C)
      do while (indvm5c .gt. 0)
         itrk = IW(indvm5c - 2)
         If (ITABL(indFXTR, itrk, JFXTVD) .eq. SelFlag) then
            IW(INDT + itrk) = indvm5c
            IF (itrk .GT. NTRK) NTRK = itrk
         End if
C
         indvm5c = iw(indvm5c-1)
      END DO
C
C Build the new component table
C
      NCOMP = 0
      DO I = 1, NTRK
         IF (IW(INDT + I) .eq. 0) goto 100
C 
C Found a track not yet in a component. Seed the component with it.
C
         NCOMP = NCOMP + 1
         INDC = NBANK('VCTL', CompOffset + NCOMP, LMHLEN + 10*LVCTLA)
         if (indc .eq. 0) then
            call rerror('VGCOMP', -3,' Out of memory')
            goto 999
         end if
         IW(INDC+LMHROW) = 1
         IW(INDC+LMHCOL) = LVCTLA
         IW(INDC+LMHLEN+1) = I
         do layer = 1, 2
            do view = 1, 2
               INDHList(JVIND(layer,view)) =
     $              NBANK(HitBnk(JVIND(layer,view)),
     $              CompOffset + NCOMP, LMHLEN + 10*LVCHLA)
               if (INDHList(JVIND(layer,view)) .eq. 0) then
                  call RERROR('VGCOMP', -4, ' Out of memory')
                  goto 999
               end if
               IW(INDHList(JVIND(layer,view))+LMHCOL) = 1
               IW(INDHList(JVIND(layer,view))+LMHROW) = 0
            end do
         end do
C
C Now, follow indices until we are done
C
         ITRK = 1
         do while (itrk .le. lrows(indc))
            indvm5c = IW(INDT + ITABL(INDC,ITRK,JVCTTR))
            DO J = 1, LROWS(indvm5c)
               do layer = 1, NLAYER
                  do grk = 1, NGRK
                     do view = 1, NVIEW
                        k = (layer-1)*4 + (grk-1)*2 + view-1
                        hit = ITABL(indvm5c, J, JVM5HW+K)
                        IF (HIT .GT. 0) THEN
                           CALL VHitID(Hit, VIEW, HIT)
                           call vaddia(INDHList(JVIND(layer,view)),
     $                          HIT, ADDED)
                           IF (ADDED) THEN
                              INDH = NLINK('VHMP', HIT)
                              if (indh .eq. 0) then
                                 call rerror('VGCOMP', -5,
     $                                ' Missing hit bank')
                                 goto 999
                              end if
                              DO M=1,LROWS(INDH)
                                 CALL VADDIA(INDC, ITABL(INDH,M,JVHMTR),
     $                                ADDED)
                              END DO
                           END IF
                        END IF
                     end do
                  end do
               END DO
            END DO
            ITRK = ITRK + 1
         end do
C
C End of the loop for all tracks in the component. Zero the indices
C  in VGC1 corresponding to these tracks and go on to the next component
C 
C
         DO ITRK = 1, LROWS(INDC)
            IW(INDT + ITABL(INDC,ITRK,1)) = 0
         END DO
C
C Run an algorithm to remove tracks which have too big an extrapolation
C     footprint compared to the others.
C
C list to hold tracks to "throw out"
         indrm = NBANK('VGXX', 2, LROWS(INDC))
         if (indrm .eq. 0) then
            call rerror('VGCOMP',-6,' Out of memory')
            goto 999
         end if
C
         nremove = 0
         CALL VGTCUT(LROWS(indc), IW(indc+LMHLEN+1), MOMCUT,
     $        nremove, IW(indrm+1))
         if (nremove .gt. 0) then
            repeat = .true.
C
C for all removed tracks, change the flag in the FRFT bank by one.
C
            do itrk = 1, nremove
               IW(KROW(indFXTR,IW(indrm+ITRK))+JFXTVD) =
     $              IW(KROW(indFXTR,IW(indrm+ITRK))+JFXTVD) + 1
            end do
         end if
C
 100     continue
      END DO
C
C We have formed all the components. However, if some tracks where
C     removed by the algorithm to kill big footprint tracks, we need to
C     start all over again.
C
      if (repeat) goto 10
C
C Clean up
C
 999  continue
      CALL ABRUEV(IRUN,IEVT)
      KVTDP = NLINK('VTDP',IEVT)
      IF (KVTDP.GT.0) THEN
        NVCH1 = NAMIND('VCH1')
        NVCH2 = NAMIND('VCH2')
        NVCH3 = NAMIND('VCH3')
        NVCH4 = NAMIND('VCH4')
        DO ITRKDP = 1, IW(KVTDP)
          JTRKDP = IW(KVTDP+ITRKDP)
          write(iw(6),1) jtrkdp
 1        format(1X,'VGCOMP - Track: ',I6)
          DO ICDP = COMPOFFSET+1,COMPOFFSET+NCOMP
            KVCTL = NLINK('VCTL',ICDP)
            IF (KVCTL.GT.0) THEN
                DOIT = .FALSE.
                DO ITC = 1, LROWS(KVCTL)
                    ITCDP = IW(KVCTL+LMHLEN+ITC)
                    IF (ITCDP.EQ.JTRKDP) DOIT = .TRUE.
                ENDDO
                IF (DOIT) THEN
                    write(iw(6),2) ICDP
 2                  format(1X,'VGCOMP - Component: ',I6)
                    write(iw(6),3)
     $                 (IW(KVCTL+LMHLEN+JT),JT=1,LROWS(KVCTL))
 3                  format(1X,'VGCOMP - Tracks in component:',(I6))
                    KVCH1 = NLINK('VCH1',ICDP)
                    IF (KVCH1.GT.0)THEN
                        write(iw(6),*) 'VCH1 Dump:'
                        CALL BPRTAB(NVCH1,KVCH1,1,LROWS(KVCH1))
                    ELSE
                        write(iw(6),4)
 4                      format(1X,'VGCOMP - No VCH1 bank')
                    ENDIF
                    KVCH2 = NLINK('VCH2',ICDP)
                    IF (KVCH2.GT.0)THEN
                        write(iw(6),*) 'VCH2 Dump:'
                        CALL BPRTAB(NVCH2,KVCH2,1,LROWS(KVCH2))
                    ELSE
                        write(iw(6),5)
 5                      format(1X,'VGCOMP - No VCH2 bank')
                    ENDIF
                    KVCH3 = NLINK('VCH3',ICDP)
                    IF (KVCH3.GT.0)THEN
                        write(iw(6),*) 'VCH3 Dump:'
                        CALL BPRTAB(NVCH3,KVCH3,1,LROWS(KVCH3))
                    ELSE
                        write(iw(6),6)
 6                      format(1X,'VGCOMP - No VCH3 bank')
                    ENDIF
                    KVCH4 = NLINK('VCH4',ICDP)
                    IF (KVCH4.GT.0)THEN
                        write(iw(6),*) 'VCH4 Dump:'
                        CALL BPRTAB(NVCH4,KVCH4,1,LROWS(KVCH4))
                    ELSE
                        write(iw(6),7)
 7                      format(1X,'VGCOMP - No VCH4 bank')
                    ENDIF
                ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDIF
      CALL BDROP(IW, 'VHMP')
      CALL BDROP(IW, 'VGXX')
      RETURN
      END
#endif
