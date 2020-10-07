      COMMON/TMONIT/ NEVTTM,NTKSTM(LTSECT),NUUCTM(LTSECT),
     &               DEDXTM(LTSECT),DEX2TM(LTSECT),NDEXTM(LTSECT),
     &               NUSCTM(LTSECT),TDEXTM,TDX2TM,NTDXTM,T0SHTM,
     &               T0S2TM,NT0STM,NWTKTM(LTSECT),NPTKTM(LTSECT),
     &               NMISTM(LTSECT),NWSYTM(LTSECT),NPSYTM(LTSECT),
     &               NBADTM(LTSECT),NDRPTM(LTSECT),NHORTM(LTSECT),
     &               NROWTM(LTSECT),NBDHTM(LTSECT),NTPDTM(LTSECT),
     &               NCPDTM(LTSECT),GAINTM(LTSECT),NDPXTM(LTSECT),
     &               DPDXTM(LTSECT),DPX2TM(LTSECT),NTPXTM,TDPXTM,
     &               TPX2TM
#if defined(DOC)
C
C! TPC monitoring.  Information to be printed at end of run or job.
C
C NEVTTM = number of events included in sums
C NTKSTM = number of tracks per sector
C NUSCTM = number of used coordinates per sector
C NUUCTM = number of unused good coordinates per sector
C NBADTM = number of bad coordinates per sector
C DEDXTM = sum of dedx per sector
C DEX2TM = sum of square of dedx per sector
C NDEXTM = number of entries per sector in DEDXTM and DEX2TM
C TDEXTM = sum of overall dE/dx
C TDX2TM = sum of squares of overall dE/dx
C NTDXTM = number of entries in TDEXTM and TDX2TM
C T0SHTM = sum of measured T0 shift per event
C T0S2TM = sum of squares of measured T0 shift per event
C NT0STM = number of entries in T0SHTM and T0S2TM
C NWTKTM = number of times wire data are truncated for each sector
C NPTKTM = number of times pad data are truncated for each sector
C NMISTM = number of times data banks are missing for each sector
C NWSYTM = number of times each wire TPP is out of sequence in evt cnt
C NPSYTM = number of times each pad TPP is out of sequence in evt cnt
C NDRPTM = number of times each sector is dropped because of data error
C NHORTM = number of times each sector has number of hits out of range
C NROWTM = number of times each sector has a bad padrow number
C NBDHTM = number of times each sector has a bad hit word
C NTPDTM = number of times each sector has a TPAD, TPDI mismatch
C NCPDTM = number of times pad data are truncated due to format error
C GAINTM = Sector to sector gain map from TC2X or TCSX
C DPDXTM = sum of pad dedx per sector
C DPX2TM = sum of square of pad dedx per sector
C NDPXTM = number of entries per sector in DPDXTM and DPX2TM
C TDPXTM = sum of overall pad dE/dx
C TPX2TM = sum of squares of overall pad dE/dx
C NTPXTM = number of entries in TDPXTM and TPX2TM
C
#endif
