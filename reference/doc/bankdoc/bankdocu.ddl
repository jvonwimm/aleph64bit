 SUBSCHEMA AdbsDOCU
 :'Description of Adbs documentation tables containing information  used
   for documentation purpose only'
 
 
 AUTHOR   'F.Ranjard'
 VERSION  '1.0'
 DATE     '02/05/90'
 
 DEFINE ESET
 
  CADB
      :       ' The table contains for each ADBS bank the description
                of each column.(DOC)\
                number of words/column\
                total number of columns'
          STATIC
      = (NAmetab      = CHA4                  : 'Name of a  table',
         NameCol      = CH16                  : 'Name of a column',
         NumbofIdent  = INTE                  : 'Number of identical column',
         Format       = CHA8                  : 'Format of a column')
      
      ;

  CPRO
      :       ' The table contains for each PROgram bank the description
                of each column.(DOC)\
                number of words/column\
                total number of columns'
          STATIC
      = (NAmetab      = CHA4                  : 'Name of a  table',
         NameCol      = CH16                  : 'Name of a column',
         NumbofIdent  = INTE                  : 'Number of identical column',
         Format       = CHA8                  : 'Format of a column')
      
      ;

  TPRO
      :       'list of PROGRAM table names in integer alphabetic
               order (means alphabetic order on IBM and CRAY and
               alphabetic order from the last character on VAX).
               (DOC)\
               number of words/table\
               number of tables'
           STATIC
      = (NAmetab      = CHA4                : 'Name of a table',
         RowinCPRO    = INTE                : 'Row number where it starts
                                               in CPRO')
      ;
    
  TADB
      :       'list of ADBSCONS table names in integer alphabetic
               order (means alphabetic order on IBM and CRAY and
               alphabetic order from the last character on VAX).
               (DOC)\
               number of words/table\
               number of tables'
            STATIC
      = (NAmetab      = CHA4                : 'Name of a table',
         RowinCADB    = INTE                : 'Row number where it starts
                                               in CADB')
      ;
    
 
 
 END ESET
 
 END SUBSCHEMA

 SUBSCHEMA ProgDOCU
 :'Description of PROGram documentation tables containing information  used
   for documentation purpose only'
 
 
 AUTHOR   'F.Ranjard'
 VERSION  '1.0'
 DATE     '02/05/90'
 
 DEFINE ESET
 
  CLTC
      :       ' The table contains for each LTC bank the description
                of each column.(DOC)\
                number of words/column\
                total number of columns'
          STATIC
      = (NAmetab      = CHA4                  : 'Name of a  table',
         NameCol      = CH16                  : 'Name of a column',
         NumbofIdent  = INTE                  : 'Number of identical column',
         Format       = CHA8                  : 'Format of a column')
      
      ;

 COBS
      :      'ADAMO .BS file table of all columns of all tables
              (DOC)'
 
      = (NameCol      = CH16                  : 'Name of a column',
         Type         = CHA4                  : 'Type of a column',
         Format       = CHA8                  : 'Format for a column',
         OffSet       = INTE                  : 'Offset',
         WiDth        = INTE                  : 'Width',  
         TableInd     = INTE                  : 'Index to the TAB2 table')
      ;
 
  CSTC
      :       ' The table contains for each STC bank the description
                of each column.(DOC)\
                number of words/column\
                total number of columns'
          STATIC
      = (NAmetab      = CHA4                  : 'Name of a  table',
         NameCol      = CH16                  : 'Name of a column',
         NumbofIdent  = INTE                  : 'Number of identical column',
         Format       = CHA8                  : 'Format of a column')
      
      ;

 
     
  TCOL
      :       ' The table contains for each bank the description
                of each column.(DOC)\
                number of words/column\
                total number of columns'
          STATIC
      = (NAmetab      = CHA4                  : 'Name of a  table',
         NameCol      = CH16                  : 'Name of a column',
         NumbofIdent  = INTE                  : 'Number of identical column',
         Format       = CHA8                  : 'Format of a column')
      
      ;

  
  TDOC
      :       'ALEPH bank documentation.(DOC)\
               number of words/bank\
               number of ALEPH banks'
           STATIC
      = (NAmetab      = CHA4                  : 'Name of a table',   
         UnitName     = CHA4                  : 'dataflow name',
         BosFormat(9) = CHA4                  : 'BOS format',
         NumberofCol  = INTE                  : 'Number of columns') 
 
      ;

  TNOG
      :       'list of STC banks to be dropped by GALEPH.(DOC)\
               number of words/bank\
               number of NOGaleph banks'
           STATIC
      = (NAmetab      = CHA4                  : 'Name of a STC table')

      ;
 
  TNOJ
      :       'list of STC banks to be dropped by JULIA dataflow.(DOC)\
               number of words/bank\
               number of NOJulia banks'
           STATIC
      = (NAmetab      = CHA4                  : 'Name of a STC table')

      ;
 
  TPOT
      :       'list of POT banks .(DOC)\
               number of words/bank\
               number of POT banks'
           STATIC
      = (NAmetab      = CHA4                  : 'Name of a POT bank')

      ;
 
  TSTC
      :       'list of banks belonging to STC dataflow.(DOC)\
               number of words/bank\
               number of STC banks'
           STATIC
      = (NAmetab      = CHA4                  : 'Name of a STC table')

      ;
 
 
 TABS
      :      'ADAMO .BS file table containing all table names
             . (DOC)'
 
      = (NAmetab      = CH16                  : 'Name of a table',
         ADdress      = INTE                  : 'Address'
         )
      ;
 
 
 END ESET
 
 END SUBSCHEMA
