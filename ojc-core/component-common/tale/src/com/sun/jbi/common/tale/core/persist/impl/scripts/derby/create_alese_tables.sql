--/**************************************** 
--           Common Tables               *
--*****************************************/

-- Configuration Tables --
CREATE TABLE CSF_REP_USERS (
	USER_LOGICAL_ID VARCHAR ( 128 ) NOT NULL,
	USER_DESCRIPTION VARCHAR ( 128 ),
	USER_NAME VARCHAR ( 128 ),
	USER_PASSWORD VARCHAR ( 128 ),
	ACTIVE_FLAG CHAR ( 1 ) NOT NULL,
	CREATE_ID VARCHAR ( 128 ),
	CREATE_DATE_TIME TIMESTAMP,
	LAST_MOD_ID VARCHAR ( 128 ),
	LAST_MOD_DATE_TIME TIMESTAMP,
	CONSTRAINT PK_CSF_REP_USERS PRIMARY KEY (USER_LOGICAL_ID)
	);
CREATE TABLE CSF_JMS_CHANNEL (
	CHANNEL_CODE INTEGER NOT NULL,
	HOST_IP VARCHAR ( 15 ) NOT NULL,
	PORT SMALLINT NOT NULL,
	NAME VARCHAR ( 64 ) NOT NULL,
	CHANNEL_TYPE CHAR ( 1 ) NOT NULL,
	ACTIVE_FLAG CHAR ( 1 ) NOT NULL,
	DESCRIPTION VARCHAR ( 256 ),
	CREATE_ID VARCHAR ( 128 ),
	CREATE_DATE_TIME TIMESTAMP NOT NULL,
	LAST_MOD_ID VARCHAR ( 128 ),
	LAST_MOD_DATE_TIME TIMESTAMP NOT NULL,
	CONSTRAINT PK_CSF_JMS_CHNL2 PRIMARY KEY (CHANNEL_CODE)
	);

-- /* Run-time Tables */
CREATE TABLE CSF_PAYLOAD_LOG (
	MESG_ID VARCHAR ( 128 ) NOT NULL,
	ENCODE_MODE VARCHAR ( 32 ),
	CONSTRAINT PK_CSF_PYLD_LOG6 PRIMARY KEY (MESG_ID)
	);
CREATE TABLE CSF_PAYLOAD_STORE (
	MESG_ID VARCHAR ( 128 ) NOT NULL,
	VERSION SMALLINT DEFAULT 0 NOT NULL,
	PAYLOAD_TYPE VARCHAR ( 32 ) NOT NULL,
	PAYLOAD_MESG CLOB,
	CREATE_ID VARCHAR ( 128 ),
	CREATE_DATE_TIME TIMESTAMP,
	CONSTRAINT PK_CSF_PYLD_STORE PRIMARY KEY (MESG_ID, VERSION, PAYLOAD_TYPE)
	);
CREATE TABLE CSF_CME_LOG (
	MESG_ID VARCHAR ( 128 ) NOT NULL,
	MESG_DATESTAMP TIMESTAMP NOT NULL,
	APP_MESG_ID VARCHAR ( 128 ),
	APP_DATESTAMP TIMESTAMP,
	PROJECT_NAME VARCHAR ( 128 ),
	APP_TYPE VARCHAR ( 128 ) NOT NULL,
	APP_NAME VARCHAR ( 128 ),
	SERVICE_NAME VARCHAR ( 128 ) NOT NULL,
	MODULE_NAME VARCHAR ( 128 ) NOT NULL,
	UNIT_NAME VARCHAR ( 128 ) NOT NULL,
	INSERT_DATE_TIME TIMESTAMP,
	CONSTRAINT PK_CSF_CME_LOG5 PRIMARY KEY (MESG_ID)
	);
CREATE TABLE CSF_USER_FIELDS (
	MESG_ID VARCHAR ( 128 ) NOT NULL,
	SEQUENCE_ID SMALLINT NOT NULL,
	FIELD_NAME VARCHAR ( 512 ),
	FIELD_VALUE VARCHAR ( 512 ),
	CONSTRAINT PK_CSF_USR_FLDS PRIMARY KEY (MESG_ID, SEQUENCE_ID)
	);

--/* Constraints */
ALTER TABLE CSF_REP_USERS ADD CONSTRAINT FK_CSF_REP_USERS38 FOREIGN KEY (CREATE_ID) REFERENCES CSF_REP_USERS (USER_LOGICAL_ID);
ALTER TABLE CSF_REP_USERS ADD CONSTRAINT FK_CSF_REP_USERS37 FOREIGN KEY (LAST_MOD_ID) REFERENCES CSF_REP_USERS (USER_LOGICAL_ID);

ALTER TABLE CSF_JMS_CHANNEL ADD CONSTRAINT FK_CSF_JMS_CHNL36 FOREIGN KEY (LAST_MOD_ID) REFERENCES CSF_REP_USERS (USER_LOGICAL_ID);
ALTER TABLE CSF_JMS_CHANNEL ADD CONSTRAINT FK_CSF_JMS_CHNL35 FOREIGN KEY (CREATE_ID) REFERENCES CSF_REP_USERS (USER_LOGICAL_ID);

ALTER TABLE CSF_PAYLOAD_LOG ADD CONSTRAINT FK_CSF_PYLD_LOG10 FOREIGN KEY (MESG_ID) REFERENCES CSF_CME_LOG (MESG_ID);

ALTER TABLE CSF_PAYLOAD_STORE ADD CONSTRAINT FK_CSF_PYLD_STORE9 FOREIGN KEY (MESG_ID) REFERENCES CSF_PAYLOAD_LOG (MESG_ID);

ALTER TABLE CSF_USER_FIELDS ADD CONSTRAINT FK_CSF_USR_FLDS11 FOREIGN KEY (MESG_ID) REFERENCES CSF_CME_LOG (MESG_ID);

--/**************************************** 
--*                ALE                    *
--*****************************************/

--/* Configuration Tables */
CREATE TABLE CSF_ALERTER_GROUPS (
	ALERTER_GROUP VARCHAR ( 256 ) NOT NULL,
	ALERTER_FROM VARCHAR ( 256 ),
	ALERTER_TO_PRIMARY VARCHAR ( 256 ),
	ALERTER_TO_SECONDARY VARCHAR ( 256 ),
	ACTIVE_FLAG CHAR ( 1 ),
	CREATE_ID VARCHAR ( 128 ),
	CREATE_DATE_TIME TIMESTAMP,
	LAST_MOD_ID VARCHAR ( 128 ),
	LAST_MOD_DATE_TIME TIMESTAMP,
	CONSTRAINT PK_CSF_ALRT_GRPS PRIMARY KEY (ALERTER_GROUP)
	);
CREATE TABLE CSF_ALERTER_CHANNELS (
	ALERTER_CHANNEL_CODE INTEGER NOT NULL,
	CHANNEL_TYPE VARCHAR ( 128 ),
	HOST_NAME VARCHAR ( 256 ),
	COMMUNITY VARCHAR ( 32 ),
	TRAP_PORT VARCHAR ( 32 ),
	LISTENER_PORT VARCHAR ( 32 ),
	ACTIVE_FLAG CHAR ( 1 ),
	CREATE_ID VARCHAR ( 128 ),
	CREATE_DATE_TIME TIMESTAMP,
	LAST_MOD_ID VARCHAR ( 128 ),
	LAST_MOD_DATE_TIME TIMESTAMP,
	CONSTRAINT PK_CSF_ALRT_CHNL PRIMARY KEY (ALERTER_CHANNEL_CODE)
	);
CREATE TABLE CSF_ALERTER_CODES (
	ALERTER_CODE INTEGER NOT NULL,
	ALERTER_LABEL VARCHAR ( 128 ),
	ALERTER_CATEGORY VARCHAR ( 32 ),
	ALERTER_LEVEL VARCHAR ( 32 ),
	ALERTER_DESCRIPTION VARCHAR ( 256 ),
	ALERTER_GROUP VARCHAR ( 256 ),
	ALERTER_CHANNEL_CODE INTEGER,
	ACTIVE_FLAG CHAR ( 1 ),
	CREATE_ID VARCHAR ( 128 ),
	CREATE_DATE_TIME TIMESTAMP,
	LAST_MOD_ID VARCHAR ( 128 ),
	LAST_MOD_DATE_TIME TIMESTAMP,
	CONSTRAINT PK_CSF_ALRT_CDS PRIMARY KEY (ALERTER_CODE)
	);
CREATE TABLE CSF_LOGGER_CHANNELS (
	LOGGER_CHANNEL_CODE INTEGER NOT NULL,
	CHANNEL_TYPE VARCHAR ( 128 ),
	FILE_NAME VARCHAR ( 256 ),
	ACTIVE_FLAG CHAR ( 1 ),
	CREATE_ID VARCHAR ( 128 ),
	CREATE_DATE_TIME TIMESTAMP,
	LAST_MOD_ID VARCHAR ( 128 ),
	LAST_MOD_DATE_TIME TIMESTAMP,
	CONSTRAINT PK_CSF_LGR_CHNL PRIMARY KEY (LOGGER_CHANNEL_CODE)
	);
CREATE TABLE CSF_LOGGER_CODES (
	LOGGER_CODE INTEGER NOT NULL,
	LOGGER_LABEL VARCHAR ( 128 ),
	LOGGER_CATEGORY VARCHAR ( 32 ) DEFAULT 'APPLICATION',
	LOGGER_LEVEL VARCHAR ( 32 ),
	LOGGER_DESCRIPTION VARCHAR ( 256 ),
	LOGGER_CHANNEL_CODE INTEGER,
	ACTIVE_FLAG CHAR ( 1 ),
	CREATE_ID VARCHAR ( 128 ),
	CREATE_DATE_TIME TIMESTAMP,
	LAST_MOD_ID VARCHAR ( 128 ),
	LAST_MOD_DATE_TIME TIMESTAMP,
	CONSTRAINT PK_CSF_LGR_CDS PRIMARY KEY (LOGGER_CODE)
	);
CREATE TABLE CSF_ERROR_CODES (
	ERROR_CODE INTEGER NOT NULL,
	ERROR_LABEL VARCHAR ( 128 ),
	ERROR_CATEGORY VARCHAR ( 32 ),
	ERROR_LEVEL VARCHAR ( 32 ),
	ERROR_DESCRIPTION VARCHAR ( 256 ),
	AUTHORIZE_FLAG CHAR ( 1 ),
	LOGGER_FLAG CHAR ( 1 ),
	ALERTER_FLAG CHAR ( 1 ),
	REPLAY_FLAG CHAR ( 1 ),
	PERSIST_FLAG CHAR ( 1 ),
	ENCODE_FLAG CHAR ( 1 ),
	ALERTER_CODE INTEGER NOT NULL,
	LOGGER_CODE INTEGER,
	PERSIST_MODE VARCHAR ( 32 ),
	ENCODE_MODE VARCHAR ( 32 ),
	ACTIVE_FLAG CHAR ( 1 ),
	CREATE_ID VARCHAR ( 128 ),
	CREATE_DATE_TIME TIMESTAMP,
	LAST_MOD_ID VARCHAR ( 128 ),
	LAST_MOD_DATE_TIME TIMESTAMP,
	CONSTRAINT PK_CSF_ERR_CDS PRIMARY KEY (ERROR_CODE)
	);

--/* Run-time Tables */
CREATE TABLE CSF_ALERTER_LOG (
	MESG_ID VARCHAR ( 128 ) NOT NULL,
	ALERTER_CODE INTEGER NOT NULL,
	ALERT_DETAILS VARCHAR ( 512 ),
	DISPLAY_MESG VARCHAR ( 512 ),
	CONSTRAINT PK_CSF_ALRT_LOG7 PRIMARY KEY (MESG_ID)
	);
CREATE TABLE CSF_ALERT_RESOLUTION (
	MESG_ID VARCHAR ( 128 ) NOT NULL,
	RESOLUTION_STATUS VARCHAR ( 32 ),
	RESOLUTION_BY VARCHAR ( 32 ),
	RESOLUTION_DETAILS VARCHAR ( 128 ),
	RESOLUTION_DT TIMESTAMP,
	CONSTRAINT PK_CSF_ALRT_RESL10 PRIMARY KEY (MESG_ID)
	);
CREATE TABLE CSF_LOGGER_LOG (
	MESG_ID VARCHAR ( 128 ) NOT NULL,
	LOGGER_CODE INTEGER NOT NULL,
	LOG_DETAILS VARCHAR ( 512 ),
	DISPLAY_MESG VARCHAR ( 512 ),
	CONSTRAINT PK_CSF_LGR_LOG8 PRIMARY KEY (MESG_ID)
	);
CREATE TABLE CSF_ERROR_LOG (
	MESG_ID VARCHAR ( 128 ) NOT NULL,
	ERROR_CODE INTEGER NOT NULL,
	ERROR_DETAILS VARCHAR ( 512 ),
	DISPLAY_MESG VARCHAR ( 512 ),
	CONSTRAINT PK_CSF_ERR_LOG9 PRIMARY KEY (MESG_ID)
	);
CREATE TABLE CSF_ERROR_RESOLUTION (
	MESG_ID VARCHAR ( 128 ) NOT NULL,
	RESOLUTION_STATUS VARCHAR ( 32 ),
	RESOLUTION_BY VARCHAR ( 32 ),
	RESOLUTION_DETAILS VARCHAR ( 128 ),
	RESOLUTION_DT TIMESTAMP,
	CONSTRAINT PK_CSF_ERR_RESL11 PRIMARY KEY (MESG_ID)
	);

--/* Views */	
CREATE VIEW CSF_UNRESOLVED_ALERTS_V
AS SELECT
   C.MESG_ID, C.MESG_DATESTAMP, C.PROJECT_NAME, C.MODULE_NAME,
   R.RESOLUTION_STATUS,
   A.DISPLAY_MESG,
   D.ALERTER_GROUP
FROM 
  CSF_ALERTER_LOG A LEFT OUTER JOIN CSF_ALERT_RESOLUTION R
    ON A.MESG_ID = R.MESG_ID
  JOIN CSF_CME_LOG C
    ON C.MESG_ID = A.MESG_ID
  JOIN CSF_ALERTER_CODES D
    ON A.ALERTER_CODE = D.ALERTER_CODE
WHERE 
  R.MESG_ID IS NULL;
  
CREATE VIEW ALE_SEVERITY_LEVELS
AS select distinct(alerter.alerter_level) SEVERITY_LEVEL from csf_alerter_codes alerter
union
select distinct(logger.LOGGER_LEVEL) SEVERITY_LEVEL from csf_logger_codes logger
union
select distinct(error.ERROR_LEVEL)  SEVERITY_LEVEL from csf_error_codes error;

--/* Constraints */
ALTER TABLE CSF_ALERTER_GROUPS ADD CONSTRAINT FK_CSF_ALRT_GRPS28 FOREIGN KEY (LAST_MOD_ID) REFERENCES CSF_REP_USERS (USER_LOGICAL_ID);
ALTER TABLE CSF_ALERTER_GROUPS ADD CONSTRAINT FK_CSF_ALRT_GRPS27 FOREIGN KEY (CREATE_ID) REFERENCES CSF_REP_USERS (USER_LOGICAL_ID);

ALTER TABLE CSF_ALERTER_CHANNELS ADD CONSTRAINT FK_CSF_ALRT_CHNL26 FOREIGN KEY (CREATE_ID) REFERENCES CSF_REP_USERS (USER_LOGICAL_ID);
ALTER TABLE CSF_ALERTER_CHANNELS ADD CONSTRAINT FK_CSF_ALRT_CHNL25 FOREIGN KEY (LAST_MOD_ID) REFERENCES CSF_REP_USERS (USER_LOGICAL_ID);

ALTER TABLE CSF_ALERTER_CODES ADD CONSTRAINT FK_CSF_ALRT_CDS24 FOREIGN KEY (ALERTER_GROUP) REFERENCES CSF_ALERTER_GROUPS (ALERTER_GROUP);
ALTER TABLE CSF_ALERTER_CODES ADD CONSTRAINT FK_CSF_ALRT_CDS23 FOREIGN KEY (ALERTER_CHANNEL_CODE) REFERENCES CSF_ALERTER_CHANNELS (ALERTER_CHANNEL_CODE);
ALTER TABLE CSF_ALERTER_CODES ADD CONSTRAINT FK_CSF_ALRT_CDS21 FOREIGN KEY (CREATE_ID) REFERENCES CSF_REP_USERS (USER_LOGICAL_ID);
ALTER TABLE CSF_ALERTER_CODES ADD CONSTRAINT FK_CSF_ALRT_CDS22 FOREIGN KEY (LAST_MOD_ID) REFERENCES CSF_REP_USERS (USER_LOGICAL_ID);

ALTER TABLE CSF_LOGGER_CHANNELS ADD CONSTRAINT FK_CSF_LGR_CHNL34 FOREIGN KEY (LAST_MOD_ID) REFERENCES CSF_REP_USERS (USER_LOGICAL_ID);
ALTER TABLE CSF_LOGGER_CHANNELS ADD CONSTRAINT FK_CSF_LGR_CHNL33 FOREIGN KEY (CREATE_ID) REFERENCES CSF_REP_USERS (USER_LOGICAL_ID);

ALTER TABLE CSF_LOGGER_CODES ADD CONSTRAINT FK_CSF_LGR_CDS32 FOREIGN KEY (LOGGER_CHANNEL_CODE) REFERENCES CSF_LOGGER_CHANNELS (LOGGER_CHANNEL_CODE);
ALTER TABLE CSF_LOGGER_CODES ADD CONSTRAINT FK_CSF_LGR_CDS31 FOREIGN KEY (LAST_MOD_ID) REFERENCES CSF_REP_USERS (USER_LOGICAL_ID);
ALTER TABLE CSF_LOGGER_CODES ADD CONSTRAINT FK_CSF_LGR_CDS30 FOREIGN KEY (CREATE_ID) REFERENCES CSF_REP_USERS (USER_LOGICAL_ID);

ALTER TABLE CSF_ERROR_CODES ADD CONSTRAINT FK_CSF_ERR_CDS29 FOREIGN KEY (LOGGER_CODE) REFERENCES CSF_LOGGER_CODES (LOGGER_CODE);
ALTER TABLE CSF_ERROR_CODES ADD CONSTRAINT FK_CSF_ERR_CDS20 FOREIGN KEY (LAST_MOD_ID) REFERENCES CSF_REP_USERS (USER_LOGICAL_ID);
ALTER TABLE CSF_ERROR_CODES ADD CONSTRAINT FK_CSF_ERR_CDS18 FOREIGN KEY (ALERTER_CODE) REFERENCES CSF_ALERTER_CODES (ALERTER_CODE);
ALTER TABLE CSF_ERROR_CODES ADD CONSTRAINT FK_CSF_ERR_CDS19 FOREIGN KEY (CREATE_ID) REFERENCES CSF_REP_USERS (USER_LOGICAL_ID);

ALTER TABLE CSF_ALERTER_LOG ADD CONSTRAINT FK_CSF_ALRT_LOG12 FOREIGN KEY (MESG_ID) REFERENCES CSF_CME_LOG (MESG_ID);
ALTER TABLE CSF_ALERTER_LOG ADD CONSTRAINT FK_CSF_ALRT_LOG7 FOREIGN KEY (ALERTER_CODE) REFERENCES CSF_ALERTER_CODES (ALERTER_CODE);

ALTER TABLE CSF_ALERT_RESOLUTION ADD CONSTRAINT FK_CSF_ALR_RESL16 FOREIGN KEY (MESG_ID) REFERENCES CSF_ALERTER_LOG (MESG_ID);

ALTER TABLE CSF_LOGGER_LOG ADD CONSTRAINT FK_CSF_LGR_LOG13 FOREIGN KEY (MESG_ID) REFERENCES CSF_CME_LOG (MESG_ID);
ALTER TABLE CSF_LOGGER_LOG ADD CONSTRAINT FK_CSF_LGR_LOG8 FOREIGN KEY (LOGGER_CODE) REFERENCES CSF_LOGGER_CODES (LOGGER_CODE);

ALTER TABLE CSF_ERROR_LOG ADD CONSTRAINT FK_CSF_ERR_LOG15 FOREIGN KEY (MESG_ID) REFERENCES CSF_CME_LOG (MESG_ID);
ALTER TABLE CSF_ERROR_LOG ADD CONSTRAINT FK_CSF_ERR_LOG14 FOREIGN KEY (ERROR_CODE) REFERENCES CSF_ERROR_CODES (ERROR_CODE);

ALTER TABLE CSF_ERROR_RESOLUTION ADD CONSTRAINT FK_CSF_ERR_RESL17 FOREIGN KEY (MESG_ID) REFERENCES CSF_ERROR_LOG (MESG_ID);

--/**************************************** 
--*           RECONCILIATION              *
--*****************************************/

--/* Configuration Tables */
CREATE TABLE CSF_RECONCILIATION_APPS (
	APP_NAME VARCHAR ( 128 ) NOT NULL,
	PERSISTENT CHAR ( 1 ) NOT NULL,
	INTERVAL INTEGER,
	CREATE_ID VARCHAR ( 128 ),
	CREATE_DATE_TIME TIMESTAMP,
	LAST_MOD_ID VARCHAR ( 128 ),
	LAST_MOD_DATE_TIME TIMESTAMP,
	CONSTRAINT PK_CSF_RECN_APPS PRIMARY KEY (APP_NAME)
	);

--/* Run-time Tables */
CREATE TABLE CSF_RECONCILIATION_LOG (
	APP_NAME VARCHAR ( 128 ) NOT NULL,
	APP_MESG_ID VARCHAR ( 128 ) NOT NULL,
	UNIT_NAME VARCHAR ( 128 ) NOT NULL,
	PROJECT_NAME VARCHAR ( 128 ),
	MODULE_NAME VARCHAR ( 128 ),
	APP_TYPE VARCHAR ( 128 ),
	SERVICE_NAME VARCHAR ( 128 ),
	IN_COUNT INTEGER,
	COPY_COUNT INTEGER,
	CREATE_COUNT INTEGER,
	OUT_COUNT INTEGER,
	FILTER_COUNT INTEGER,
	ERROR_COUNT INTEGER,
	MESG_DATESTAMP TIMESTAMP,
	APP_DATESTAMP TIMESTAMP,
	INSERT_DATE_TIME TIMESTAMP,
	CONSTRAINT PK_CSF_RECN_LOG PRIMARY KEY (APP_NAME, APP_MESG_ID, UNIT_NAME)
	);

--/* Constraints */
ALTER TABLE CSF_RECONCILIATION_APPS ADD CONSTRAINT FK_CSF_RECN_APPS40 FOREIGN KEY (LAST_MOD_ID) REFERENCES CSF_REP_USERS (USER_LOGICAL_ID);
ALTER TABLE CSF_RECONCILIATION_APPS ADD CONSTRAINT FK_CSF_RECN_APPS39 FOREIGN KEY (CREATE_ID) REFERENCES CSF_REP_USERS (USER_LOGICAL_ID);

CREATE INDEX SK_CSF_RECN_LOG ON CSF_RECONCILIATION_LOG (APP_NAME, UNIT_NAME, APP_DATESTAMP);

--/**************************************** 
--*          User Actions Audit           *
--*****************************************/

--/* Run-time Tables */
CREATE TABLE CSF_USER_AUDIT_LOG
(
  MESG_ID VARCHAR(128),
  USER_ID VARCHAR(128) NOT NULL,
  ACTION_TYPE VARCHAR(512),
  ACTION_RESULT VARCHAR(128),
  CONSTRAINT PK_CSF_USR_AUDT PRIMARY KEY (MESG_ID)
);

--/* Constraints */
ALTER TABLE CSF_USER_AUDIT_LOG ADD CONSTRAINT FK_CSF_USER_AUDT1 FOREIGN KEY (MESG_ID) REFERENCES CSF_CME_LOG (MESG_ID);
