SUBSCHEMA X1RUNTriggerType
    : 'A database for storing trigger setup information.'
 
 AUTHOR   'M.Wunsch, A.Stahl, B.Rensch' 
 REVIEWER 'F.Ranjard'
 VERSION  '6.2'
 DATE     '27/06/94'
 
 
 DEFINE ESET
 
 
 XTOP
    : 'List of Trigger OPtions which can be selected for a
       trigger type. NR=run number. (RUN) \
       **** OBSOLETE, will be dropped; BR-94/04/28 **** \
       **** Equivalent Information in X1TT since 1994 ****\ '
 
        SIZE 1,20
 
        = ( ValRng(2),
            Name        = CHA8 : 'Name of the trigger option',
            Trgmask     = INTE  [-999999999,999999999]
                                : 'Bit mask for level 1/2 trigger',
            GbxScale    = INTE  [-16777215,16777215] :
                                  'Used to define random trigger. \
                                   If negative the TMT signal is
                                   expected to come from the TPC.
                                   A positive value is taken as
                                   scale factor to downscale GBX as
                                   random trigger.',
            DwnScale(8) = INTE [1,65535] :
                                  'Scale factors for the trigger
                                   bits 1 - 8',
            TrgType    = INTE [0,10] : 'Number of links to XTYP');
 

 END ESET
 
 END SUBSCHEMA

 
 SUBSCHEMA TRIGGERRDOUTLVL1
 :'Event record from the Trigger Level 1/2 of the Aleph detector.'
 
 
 AUTHOR   'M.Wunsch, R.Geiges, B.Rensch'
 REVIEWER 'A.Putzer'
 VERSION  '13.5'
 DATE     '28/03/95'
 
 
 DEFINE ESET
 
 X1IP
        : 'Trigger level 1 Input for Physics triggers NR=0 (MC)\
         Number of words / Row \
         Number of Rows'
 
      STATIC
      SIZE 1,3
 
      = (InpType       = CHA4 'HCW '|'ECW '|'MISC' :
                              'Trigger input source name.',
         Cont(36)      = BITP [0,*] : 'the content depends on the trigger source.
                                HCW : Number of double planes (2 per word)
                                ECW : Energy (MeV) (2 per word)
                                MISC: LC-Wires (8 words) +
                                    : Ec-Wires total energy (8 words) +
                                    : ITC bits (4 words)'         )
      ;
 
 XTRB
      :      'Trigger level 1 + 2 Register Bit pattern
              Replaces obsolete bank XTEB! NR=0. (RAW)
              **** OBSOLETE, will be dropped; BR-94/04/28    ****
              **** Equivalent Information in X1RG since 1994 ****\
              bit  pattern of trigger sources \   
              number of trigger sources '
      STATIC
      SIZE 1,26

   =    (RegPat(3)   = BITP [0,*] :  'Bit pattern of 72 contiguous bits
                       from the readout of one register.
                       The definition of the pattern depends on the \
                       trigger source. Bits set to 1 if threshold
                       in the corresponding trigger segment is exceeded ',
         RegNam      = CHA4  'TRB1'|'HCW1'|'HCW2'|'HCW3'|'HCW4'|'ECW1'|'ECW2'|
                             'ECW3'|'ECW4'|'LCT1'|'LCT2'|'LCT3'|'LCT4'|'LCW1'|
                             'LCW2'|'LCW3'|'LCW4'|'ETT1'|'ETT2'|'ETT3'|'ITC1'|
                             'TPC1'|'PLU1'|'PLU2'|'PLU3'|'PLU4'
                            : 'Name of the bit pattern. \ 
                       TRB1 = Trigger bits (trigger mask register). \
                              Word 1: Level 1 decision, Word 2: Level 2 decision,
                              Word 3: final trigger decision) \
                       HCW1 = Hcal Wires threshold 1. 24 modules mapped to 72 
                              trigger segments \ 
                       HCW2,HCW3,HCW4 same as HCW1 but for thresholds 2,3 and 4.
                       ECW1 = Ecal Wires information for threshold 1. \
                              Bit pattern of the odd/even planes of ECAL 
                       ECW2,ECW3,ECW4 same as ECW1 but for threshold 2,3 and 4.
                       LCT1 = LCAL tower bit pattern of supersegments. 
                              Word 1 bits 0-23 are filled \  
                              Word 2 and 3 are empty.\
                       LCT2,LCT3,LCT4 same as LCT1 but for thresholds 2,3 and 4.,
                       LCW1 = Lcal Wires pattern for threshold 1. 
                              The even/odd bits pairs 0/1,2/3,6/7,8/9 correspond to
                              energy sum of odd/even planes of modules 1,2,3,4 \ 
                              Word 2 and 3 are empty.\  
                       LCW2,LCW3,LCW4 same as LCW1 but for thresholds 2,3 and 4.
                       ETT1 = TOTAL ENERGY: Bit 0-3 of word 1 
                              correspond to the total energies deposited in HCT
                              (Endcap A,endcap B,barrel,total).\
                              Bit 16-19 of word 1 equivalent info for ECT \
                              Bit  0- 3 of word 2 equivalent info for ECW odd \
                              Bit 16-19 of word 2 equivalent info for ECW even \ 
                              Bit  0- 3 of word 3 correspond  to odd/even planes of LCW. \
                       ETT2,ETT3 same as ETT1 but for threshold 2 and 3 \    
                       ITC1 = ITC track mask mapped onto the 72 trigger segments \ 
                       TPC1 = TPC track mask mapped onto the 72 trigger segments \
                       PLU1 = PLU input bit pattern of the ITC special bits. \
                              Word 1: bit 0-7 PLU sector 0 (Veto bits)
                              Word 2: bit 0-7 PLU sector 1 (Track count) \
                       PLU2 = PLU input bit pattern of the TPC special bits\
                              Word 1: bit 0-3 PLU sector 0 (inner TPC ring Track count) 
                              Word 1: bit 4-7 PLU sector 1 (outer TPC ring Track count) 
                              Word 2: special bits \
                       PLU3 = PLU input bit pattern of the LCW triggers\
                              Word 1: bit 0-7 PLU sector 0 
                              Word 1: bit 0-7 PLU sector 1 \
                       PLU4 = PLU input pattern of SATR triggers\
                              Word 1: bit 0-7 PLU sector 0 
                              Word 1: bit 0-7 PLU sector 1') ;
 
  X1AD
      : 'Trigger level 1 ADc values NR=0. (RAW) 
         **** OBSOLETE, will be dropped; BR-94/04/28    ****
         **** Equivalent Information in X1DI since 1994 ****\
         Number of words / trigger input detector \
         Number of trigger input detectors'
 
      STATIC
      SIZE 1,5
 
      = (InpType       = CHA4 'HCT '|'HCW '|'ECT '|'ECW '|'LCT ' :
                              'Trigger input source name.
                               The sequence of the sources depends on
                               the running conditions.',
         AdcVal(36)    = BITP [0,*] : '72 ADC values from analog
                               signal digitizings. The mapping of the 72
                               signals is the same as for the corresponding
                               fastbus register bit pattern (see desc. of
                               XTRB). The values of the ADC (12 bits) readings
                               are written into an array 16 bit words.');
 
  X1SC
      : 'Trigger level 1 SCaler values. NR=0. (RAW)
         **** OBSOLETE, will be dropped; BR-94/04/28    ****
         **** Equivalent Information in X1HI since 1994 **** \
         Number of words/trigger source \
         Number of trigger sources '
 
      STATIC
      SIZE 1,23
 
      = (InpType = CHA4 'HCT '|'HCW '|'ECT '|'ECW '|'LCT '|'LCW'|
                        'ETT '|'ITC '|'TPC '|'TRB ' :
                               'Trigger input source name.
                                The sequence of the sources depends on
                                the running conditions.',
         ThrSet       = INTE [1,4]   : 'Threshold set number',
         ScaVal(30)   = BITP [0,*]   : 'Number of hits on the segments\
                                since last random trigger event,\
                                array of 60 packed 16bit words. \
                                For each of the four thresholds there
                                is one row of scaler values from HCT,
                                HCW, ECT, ECW. There exist 2 scalers
                                for LCT signals and for LCW, ETT, ITC, TPC
                                and TMR there is one scaler. \
                                From HCT, HCW, ECT, ITC and TPC the hits
                                are counted after the central OR in
                                the barrel region.\
                                From ECW the hits are counted after the
                                odd - even coincidence. \
                                The ETT scaler readout contains 3*4 HCT,
                                3*4ECT, 3*4ECW (after coinc.) counts,
                                and 3*4LCW counts,
                                for each subdetector the four groups are
                                the endcap A, the endcap B, the barrel
                                and the total total energy rate. \
                                The rate from the fourth threshold is
                                unavailable (see description of XTRB). \
                                The TMR scaler contains the unscaled rate
                                of the 32 trigger bits on level 1. ');
 
 XTCN
      : 'Trigger level 1/2 CoNtrol information NR=0. (RAW)
         **** OBSOLETE, will be dropped; BR-94/04/28    ****
         **** Equivalent Information in X1RG since 1994 ****\
         Number of words per row \
         Number of rows '
 
      STATIC
      SIZE 1,1
 
   = (TrgTime(2)   = BITP[0,*]    : 'Date and Time at readout of trigger
                                     information \
                                     word 1 byte 1: day   \
                                     word 1 byte 2: month \
                                     word 1 byte 3/4: year\
                                     word 2 byte 1: 1/100 second \
                                     word 2 byte 2: second \
                                     word 2 byte 3: minute \
                                     word 2 byte 4: hour ',
      GbxCount     = INTE[0,16777215] :
                                    'Number of GBX counted by the
                                     trigger since last event readout',
      LvL1count    = INTE[0,16777215] :
                                    'Number of level 1 YES since last
                                     event readout',
      BunNum       = INTE  1|2|4|8 :
                                    'Bunch number of colliding e-beam
                                     only one single bit must be set. \
                                     Bit 0: Bunch number 1; \
                                     Bit 1: Bunch number 1 + 1BX; \
                                     Bit 2: Bunch number 1 + 2BX; \
                                     Bit 3: Bunch number 1 + 3BX.' ,
      CntrlL1      = BITP [0,*]   : 'Control flags from trigger Level 1',
      TrgRes(10)   = INTE [0,*]   : 'Reserved space for trigger info. \
                                     1 word : HV status bits,
                                     Bit  0: ECAL Endcap A; \
                                     Bit  1: ECAL Endcap B; \
                                     Bit  2: ECAL Barrel; \
                                     Bit  3: LCAL; \
                                     Bit  4: TPC dE/dxA; \
                                     Bit  5: ITC ;\
                                     Bit  6: SATR ; \
                                     Bit  7: HCAL Endcap A; \
                                     Bit  8: HCAL Endcap B; \
                                     Bit  9: HCAL Barrel; \
                                     Bit 10: MUON chambers not conn. ; \
                                     Bit 11: BCAL          not conn. ; \
                                     Bit 12: VDET          not conn. ; \
                                     Bit 13: Trigger analog crates; \
                                     Bit 14: Trigger CAMAC crates; \
                                     Bit 15: TPC ; \
                                     Bit 16 - 22: not allocated; \
                                     Bit 23: Beam pick up.\
                                     Word 2: Current trigger mask used
                                     in this event. \
                                     Word 3: number of fastbus errors
                                     during readout. \
                                     Word 4: register error flags,
                                     a bit set indicates a fastbus error
                                     caused by the corresponding TSR. \
                                     Word 5: scaler error flags, same
                                     as word 3 for TSS modules. \
                                     Word 6: ADC error flags.
                                     Word 7-10 empty' );
 
 
XCNT
      : 'Trigger Supervisor Scalers\
         Number of words per row \
         Number of rows '
 
      STATIC
 
      = (BxonPa    = INTE : 'BX during pause counter',
         BxonBu    = INTE : 'BX during busy counter',
         Gbxcnt    = INTE : 'GBX counter',
         Ly1cnt    = INTE : 'Level 1 yes  counter',
         Ly2cnt    = INTE : 'Level 2 yes  counter',
         BLast     = INTE : 'Busy time of last event (1 = 10 usecs)',
         TdcVal    = INTE : 'TDC value',
         GbxAck    = INTE : 'GBX to GBXACK timeout counter',
         EgbxBx    = INTE : 'EGBX to BX timeout counter',
         NobL1n    = INTE : 'L1NO to NOBUSY timeout counter',
         GbxL1s    = INTE : 'GBX to L1STR timeout counter',
         NoBL2n    = INTE : 'L2NO to NOBUSY timeout counter',
         LY1L2s    = INTE : 'L1YES to L2STR timeout counter',
         Busy      = INTE : 'Busy timeout counter',
         ProVio    = INTE : 'Protocol violation counter',
         Reserv(5) = INTE : 'Reserved');
 
XTEB
      :      'Trigger level 1 + 2 Event Bit pattern
 
              **** OBSOLETE, WILL BE PURGED ****
              !!! Will be replaced by bank XTRB !!!! 
              Replaces bank XTBP! NR=0. (RAW) \
              number of words per row \
              number of rows '
      STATIC
      SIZE 1,1
 
   =    (Tbitlvl1    = BITP [0,*] : 'Trigger bit pattern at time
                       of level 1 decision    \
                       The assignment of the different trigger bits
                       may be subject to changes. The actual definition
                       can be found by decoding bank XTBN in the start
                       of run records.',
         Tbitlvl2    = BITP [0,*] : 'Trigger bit pattern at time
                       of level two decision. The definition of the bits
                       is the same as for the Tbitlvl1 pattern.',
         Lvl2Bits    = BITP [0,*] : 'Level 2 trigger bit pattern
                       after application of the trigger mask. This is
                       the final trigger bit pattern which is send to
                       the trigger supervisor and distributed on the
                       level 2 broadcast. The definition
                       of the bits is the same as above.',
         HcTrb(12)   = BITP [0,*] : 'Fastbus register bit pattern from
                       discriminators on HCAL tower signals, one bit for
                       each of the 72 trigger segments. \
                       word  1- 3 bits (1:72) threshold 1     \
                       word  4- 6 bits (1:72) threshold 2     \
                       word  7- 9 bits (1:72) threshold 3     \
                       word 10-12 bits (1:72) threshold 4',
         HcWrb(12)   = BITP [0,*] : 'Fastbus register bit pattern from
                       discriminators on HCAL wire signals. \
                       Definition of the 12 words is the same as for
                       HcTrb.',
         LcWrb(12)   = BITP [0,*] : 'Fastbus register bit pattern from
                       discriminators on LCAL wire signals \
                       The first four words contain the bit pattern
                       from the LCW register. For each threshold
                       there are 8 bits for the discriminated sum from
                       the odd and even planes of the four LCAL
                       modules. The fifth word contains the total
                       energy bits from the four thresholds. The
                       significance of the bits is :
                       bit 0 : Wire energy sum side A, \
                       bit 1 : Wire energy sum side B, \
                       bit 3 : unused, \
                       bit 4 : Total wire energy sum. \
                       This sequence is repeated for the four the
                       four thresholds.
                       The remaining words are empty, this is a
                       temporary measure, this bank will be revised in
                       the future. ',
         EcWrb(12)   = BITP [0,*] : 'Fastbus register bit pattern from
                       discriminators on ECAL wire signals \
                       The 72 bits from the different thresholds
                       correspond to the 36 wire
                       modules, where each module is discriminated on
                       signals from odd and even planes separately.
                       The sequence is odd,even,odd .... planes.',
         LcTrb(4)    = BITP [0,*] : 'Fastbus register bit pattern from
                       discriminators on LCAL tower signals
                       4*24 bits from four different thresholds
                       one word for each threshold.',
         ToErb(2)    = BITP [0,*] : 'Fastbus register bit pattern from
                       discriminators on total energy sums, one 16 bit
                       word for each total energy signal type. \
                       Bits  0 - 11: 3*4 bit for HCT endcap A, \
                       endcap B, barrel, total ; thresholds 1-3.  \
                       Bits 16 - 27: 3*4 bit for LCW total sum odd, \
                       total sum even, empty, empty; thresholds 1-3.\
                       Bits 32 - 43: 3*4 bit for ECW odd endcap A, \
                       endcap B, barrel, total ; thresholds 1-3.  \
                       Bits 48 - 59: 3*4 bit for ECW even endcap A, \
                       endcap B, barrel, total ; thresholds 1-3. \
                       Remark:                                    \
                       For the time beeing the 4. threshold is not \
                       available due to hardware reasons. ',
         ITcrb(4)    = BITP [0,*] : 'Fastbus register bit pattern from
                       ITC track mask and special bits.
                       bytes 1-9: track mask string of
                       72 contigous bits; note that the ITC
                       delivers only 60 bits (central or already
                       done), therefore the bits 37-48 are dummy
                       and are always 0; \
                       Word 4: special bits. \
                       Bits 0-7 : Veto Bits; Bits 8-12 : Track count.',
         TPcrb(4)    = BITP [0,*] : 'Fastbus register bit pattern from
                       TPC track mask and special bits.
                       bytes 1-9: track mask string of
                       72 contigous bits; note that the TPC
                       delivers only 60 bits (central or already
                       done), therefore the bits 37-48 are dummy
                       and are always 0; \
                       Word 4: special bits. \
                       Bits 0-3 : Track count inner sectors; \
                       Bits 4-7 : Track count outer sectors.');
 

END ESET
 
END SUBSCHEMA
 
 
 SUBSCHEMA TRIGGERSUMMARY
 :'Contains trigger run summary banks'
 
 
 AUTHOR   'Y. Maumary,B.Rensch'
 REVIEWER 'A. Putzer'
 VERSION  '3.0'
 DATE     '28/04/94'
 
 DEFINE ESET
 
 XSGE
      :      'Trigger run Summary: GEneral information
              **** OBSOLETE, will be dropped; BR-94/04/28 **** \
              Number of Columns\
              Number of rows'
        STATIC
 
        SIZE 1,1
 
      = (GbxSum       = REAL [0.0000,*] : 'Sum of GBX counts',
         TimeReal     = REAL [0.0000,*] : 'Total time in seconds since start of run',
         TimeOpen     = REAL [0.0000,*] : 'Open time in seconds',
         NbrEvents    = INTE [0,*]      : 'Number of events',
         nbEvtxtrB    = INTE [0,*]      : 'Nb of events with XTRB filled',
         nbevtx1Ad    = INTE [0,*]      : 'Nb of events with X1AD filled',
         nbevtx1Sc    = INTE [0,*]      : 'Nb of events with X1SC filled',
         nbevtxtCN    = INTE [0,*]      : 'Nb of events with XTCN filled',
         nbEventRec   = INTE [0,*]      : 'Nb of event records read',
         ReserVe(6)   = INTE [*,*]      : 'Reserve');
 
 XSHI
      :      'Trigger run Summary: sum of HIts (bit patterns from XTRB)
              **** OBSOLETE, will be dropped; BR-94/04/28 **** \
              Number of Columns\
              Number of rows'
        STATIC
 
        SIZE 36,36
 
      = (NAme         = CHA4            : 'Name of trigger signal\
                                           TRG1,TRG2,TRGA,\
                                           HWT1,HWT2,HWT3,HWT4,\
                                           LCW1,LCW2,LCW3,LCW4,\
                                           EWT1,EWT2,EWT3,EWT4,\
                                           ITCT,TPCT,\
                                           LCT1,LCT2,LCT3,LCT4,\
                                           ETT1,ETT2,ETT3,\
                                           HCW1,HCW2,HCW3,HCW4,\
                                           EWU1,EWU2,EWU3,EWU4,\
                                           PLU1,PLU2,PLU3,PLU4',
         SegHits(72)  = INTE [0,*]      : 'Sum of segment hits');
 
 XSSC
      :      'Trigger run Summary: sum of SCalers (from X1SC)
              **** OBSOLETE, will be dropped; BR-94/04/28 **** \
              Number of Columns\
              Number of rows'
        STATIC
 
        SIZE 23,23
 
      = (NAme         = CHA4            : 'Name of trigger signal\
                                           HCT1,HCT2,HCT3,HCT4,\
                                           HCW1,HCW2,HCW3,HCW4,\
                                           ECT1,ECT2,ECT3,ECT4,\
                                           ECW1,ECW2,ECW3,ECW4,\
                                           LCT1,LCT2,LCW1,\
                                           ETT1,\
                                           ITC ,TPC ,TRB ',
         SCaler(60)   = INTE [0,*]      : 'Sum of scaler values');
 
 XTDI
      :      'All mapped and unmapped trigger bit patterns from XTRB
              **** OBSOLETE, will be dropped; BR-94/04/28 **** \
              Number of Columns\
              Number of rows'
        STATIC
 
        SIZE 36,36
 
      = (NAme         = CHA4            : 'Name of trigger signal\
                                           TRG1,TRG2,TRGA,\
                                           HWT1,HWT2,HWT3,HWT4,\
                                           LCW1,LCW2,LCW3,LCW4,\
                                           EWT1,EWT2,EWT3,EWT4,\
                                           ITCT,TPCT,\
                                           LCT1,LCT2,LCT3,LCT4,\
                                           ETT1,ETT2,ETT3,\
                                           HCW1,HCW2,HCW3,HCW4,\
                                           EWU1,EWU2,EWU3,EWU4,\
                                           PLU1,PLU2,PLU3,PLU4',
         FIlled       = INTE [0,1]      : 'Fill tag (1=filled)',
         BitPattrn(3) = BITP [0,*]      : 'Register bit pattern\
                                           from discriminators');
 
 END ESET
 
 END SUBSCHEMA
 
 
SUBSCHEMA XLV1SorRdout
    : 'Trigger Readout and SOR banks in the VME-era'
 AUTHOR     'B.Rensch'
 REVIEWER   'F.Loverre'
 VERSION    '1.2'
 DATE       '01/02/95'
 
 
 DEFINE ESET
   X1RG
      : 'XLV1: ReadOut: Registers and related stuff \
         Number of Words/Row\
         Number of Rows'
         STATIC
      = (Name          = CHA4    : 'Name of Entry \
                                    TIM  2*Time+TPR_EnabMask \
                                    TSR  GBXcount,{L1Ys,BunchNum},HV_bits \
                                    TPR  L1rslt,L2rslt,EvtMask \
                                    PLU  One Byte/Sector; Four Sect/Word \
                                    ITC  Bits {24-35}->{36-47} \
                                    TPC  Bits {24-35}->{36-47} \
                                    LCW  squeezed; 4*{2*4}Bits \
                                    ETT  3*12 Bits {ECW_o,ECW_e,LCW} \
                                    HCWn no manipulation wrt TSR \
                                    ECWn no manipulation wrt TSR ',
         Cont(3)       = BITP[0,*]: 'Data' );
 
   X1ER
      : 'XLV1: ReadOut: Hardware Readout Errors \
         Number of Words/Row\
         Number of Rows'
         STATIC
      = (PortName(3)   = CHA4      : '12 Bytes DeviceName',
         ErrCode       = BITP[0,*] : 'Normally FBrc; \
                                    -1 if R/O permanently disabled \
                                    1,2,3 for PLU only: \
                                    (No X-rsp,No Q-rsp,NoneAtAll)' );
 
   X1HI
      : 'XLV1: ReadOut: TSS_Scalers; HistoryBank; selective R/O \
         Number of Words/Row\
         Number of Rows'
         STATIC
      = (Name          = CHA4     : 'Name; see X1RG w/o PLU,TIM,TSR',
         Cont(30)      = BITP[0,*]: '2 scalers packed in 1 word' );
 
   X1DI
      : 'XLV1: ReadOut: ADCs; selective R/O \
         Number of Words/Row\
         Number of Rows'
         STATIC
      = (AdcName       = CHA4     : 'Name; HCW ECW LCW',
         Cont(36)      = BITP[0,*]: '2 values(12 bits) in 1 word' );
 
   X1TD
      : 'XLV1: ReadOut: TDC-info formatted; selective R/O \
         Number of Words/Row\
         Number of Rows'
         STATIC
      = (Cont          = BITP[0,*]: '7 bits channel  [0,95] \
                                    9 bits StrtTime [0,511] \
                                    9 bits StopTime [0,511] \
                                    2 bits ClkSelec [0,3]=[2,16]ns \
                                    5 bits ErrCode  [0,2] \
                                      0=ok, Matching Strt/Stop \
                                      1=STRTcha w/o StopCha \
                                      2=STOPcha w/o StrtCha' );
 
   X1TT
      : 'XLV1: SOR: Current TrigType expanded \
         Number of Words/Row\
         Number of Rows'
         STATIC
      = (TrigType(2)   = CHA4       : 'Current TrigType',
         OrigMask      = BITP[0,*]  : 'Original TrigEnableMask',
         EnabMask      = BITP[0,*]  : 'Current  TrigEnableMask',
         GBXscale      = BITP[0,*]  : 'DwnScale for RandomTrigger',
         DwnScale(8)   = BITP[0,*]  : 'DwnScale for first 8 Trigs',
         x1HImsk       = BITP[0,*]  : 'ReadoutMask for TSS Scalers',
         x1ADmsk       = BITP[0,*]  : 'ReadoutMask for ADC',
         x1TDmsk       = BITP[0,*]  : 'ReadoutMask for TDC' );
 
   X1DA
      : 'XLV1: SOR: This TrigType Threshold Setting in DAC_counts \
         Number of Words/Row\
         Number of Rows'
         STATIC
      = (Name          = CHA4         : 'Name HCWn,ECWn,LCW /n=1..4',
         DACount(80)   = INTE [0,255] : 'DACsettting for one Thrs \
                                         HCW 72 values \
                                         ECW 72+2*4 {Seg+Etot} \
                                         LCW 4*(4+4+4) all 4 Thrs' );
 
   X1CB
      : 'XLV1: SOR: Calibration Constants DACount-->Energy=DAC*val+off \
         Number of Words/Row\
         Number of Rows=1'
         STATIC
      = (CalEWval(5)  = INTE    : '[Mev/cnt]: Segment,EA,EB,BA,TOT',
         cAlEWoff(5)  = INTE    : '[Mev]:     Segment,EA,EB,BA,TOT',
         calLWval(5)  = INTE    : '[Mev/cnt]: Segment,EA,EB,TH,TOT',
         callWOff(5)  = INTE    : '[Mev]:     Segment,EA,EB,TH,TOT' );
 
 END ESET
 
END SUBSCHEMA
 
 
SUBSCHEMA XLV1eorREC
    : 'Trigger EOR banks from X1ver and X1mon in the VME-era'
 AUTHOR     'B.Rensch'
 REVIEWER   'F.Loverre'
 VERSION    '1.1'
 DATE       '01/02/95'
 
 
 DEFINE ESET
   X1SV
      : 'XLV1: EOR: Summary from X1ver_Online_Monitoring \
         Number of Words/Row\
         Number of Rows'
         STATIC
      = (VerDat        = BITP[0,*]  : 'Data' );
 
   X1SM
      : 'XLV1: EOR: Summary from X1mon_Online_Monitoring \
         Number of Words/Row\
         Number of Rows'
         STATIC
      = (MonDat        = BITP[0,*]   : 'Data' );
 
 END ESET
 
END SUBSCHEMA
