      SUBROUTINE HCNAMI
C--------------------------------------------------------------------
C
C!   Define formats and name indices of HC Bos Banks
C!
C!                                Author:G.Catanesi 87/09/15
C!          Called from :HCIRUN
C!          Calls       :NAMIND,BFKFMT from BOS77.hlb
C --------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(NHTTR=192,NHWTR=36)
      PARAMETER (MHCSE=200)
      PARAMETER(MHCTH=300)
      COMMON /HCNAMC/NAHTHT,NAHWHT,NAHTTR,NAHWTR,NAHTDI,NAHWDI, NAHTDT,
     &               NAHTTD,NAHWDT,NAHWTD,NAHPDI,NAHPHT,NAHLWD,
     &               JDHCSE,JDHCHI,JDHCTH
C         define name-index
C
      NAHTHT = NAMIND('HTHT')
      NAHWHT = NAMIND('HWHT')
      NAHTTR = NAMIND('HTTR')
      NAHWTR = NAMIND('HWTR')
      NAHTDI = NAMIND('HTDI')
      NAHWDI = NAMIND('HWDI')
      NAHTDT = NAMIND('HTDT')
      NAHTTD = NAMIND('HTTD')
      NAHWDT = NAMIND('HWDT')
      NAHWTD = NAMIND('HWTD')
      NAHPDI = NAMIND('HPDI')
      NAHPHT = NAMIND('HPHT')
      NAHLWD = NAMIND('HLWD')
C
C       define formats
C
      CALL BKFMT ('HTHT','2I,(3I,F)')
      CALL BKFMT ('HWHT','(I)')
      CALL BKFMT ('HTTR','(I)')
      CALL BKFMT ('HWTR','(I)')
      CALL BKFMT ('HTDI','(I)')
      CALL BKFMT ('HWDI','(I)')
      CALL BKFMT ('HTDT','(I)')
      CALL BKFMT ('HWDT','(I)')
      CALL BKFMT ('HTTD','(I)')
      CALL BKFMT ('HWTD','(I)')
      CALL BKFMT ('HPDI','(I)')
      CALL BKFMT ('HPHT','2I,(I,F)')
      CALL BKFMT ('HLWD','2I,(I,F)')
C
C      Set to zero pointers to Work banks
C
      JDHCSE = 0
      JDHCHI = 0
      JDHCTH = 0
C
      END
