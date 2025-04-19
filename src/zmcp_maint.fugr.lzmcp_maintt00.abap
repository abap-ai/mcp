*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMCP_CONFIG.....................................*
DATA:  BEGIN OF STATUS_ZMCP_CONFIG                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMCP_CONFIG                   .
*...processing: ZMCP_ORIGINS....................................*
DATA:  BEGIN OF STATUS_ZMCP_ORIGINS                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMCP_ORIGINS                  .
CONTROLS: TCTRL_ZMCP_ORIGINS
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZMCP_SERVERS....................................*
DATA:  BEGIN OF STATUS_ZMCP_SERVERS                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMCP_SERVERS                  .
CONTROLS: TCTRL_ZMCP_SERVERS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMCP_CONFIG                   .
TABLES: *ZMCP_ORIGINS                  .
TABLES: *ZMCP_SERVERS                  .
TABLES: ZMCP_CONFIG                    .
TABLES: ZMCP_ORIGINS                   .
TABLES: ZMCP_SERVERS                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
