# massUnitsCheck.r
#
# Runs missing values tests on all tbl* tables in NRSA.  A simple tweak would
# run missing and range tests.
#  2/25/2010 cws moved source() calls to NRSAvalidation.r
# 09/16/2010 cws Removing hardcoding of NRSA database name, using NRSAdbName
#            instead.
#
require(RODBC)

chan <- odbcConnect(NRSAdbName)
meta <- fetchNRSATable(chan, 'tblPARAMETERDESCRIPTIONS2')

tables <- subset(sqlTables(chan)
                ,substr(TABLE_NAME,1,3)=='tbl' &
                 !(TABLE_NAME %in% c('tblCOMMENTS2','tblFISHPHOTO2'
                                    ,'tblLABRESULTS2','tblPARAMETERDESCRIPTIONS2'
                                    ,'tblVISITS2','tblSAMPLECHARACTERISTICS2'
                                    )
                  )
                )$TABLE_NAME


for(t in tables) {
  # Let us know what's happening
  print(sprintf("Working on %s", t))

  # Run a test
  df <- fetchNRSATable(chan, t)
  rr <- quickUnitsCheck(df, meta)
  print(rr)
}
