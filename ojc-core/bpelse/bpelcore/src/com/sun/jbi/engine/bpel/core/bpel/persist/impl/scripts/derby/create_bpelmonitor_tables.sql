CREATE TABLE SERVICEUNIT (
    suname varchar(256),
    suziparchive blob (2 G) NOT NULL,
    lastupdatetime TIMESTAMP,
    primary key (suname)
);

CREATE TABLE MONITORBPELPROCESS (
    suname varchar(256),
    bpelid varchar(1028),
    genbpelevents char(1),
    primary key (bpelid)
);

CREATE TABLE MONITORBPELINSTANCE (
    engineid varchar(128),
    instanceid varchar(128),
    bpelid varchar(1028),
    status varchar(32) DEFAULT 'RUNNING',  
    starttime TIMESTAMP,
    endtime TIMESTAMP,
    updatedtime TIMESTAMP,    
    primary key (instanceid)
);

CREATE TABLE MONITORBPELACTIVITY (
    engineid varchar(128),
    instanceid varchar(128),
    activityid numeric(16,0),
    activityxpath varchar (2000),
    iteration  numeric(4,0) DEFAULT 0,
    status varchar(32) DEFAULT 'STARTED',
    hasfaulted char(1) DEFAULT 'N',
    crmpreceiveid varchar(128),    
	crmpinvokeid varchar(128),      
    starttime TIMESTAMP,
    endtime TIMESTAMP,
    primary key (instanceid, activityid, iteration)
);
	
CREATE TABLE MONITORBPELVARIABLE (
    instanceid varchar(128),
    scopeid numeric(16,0),
    varid numeric(16,0),
    varname varchar (255),
	isfault char(1) DEFAULT 'N',
    varvalue clob ( 2 G ),
    primary key (instanceid, scopeid, varname)
);

CREATE TABLE MONITORSIMPLEVARIABLE (
	instanceid varchar(128),
	scopeid numeric(16,0),
	varid numeric(16,0),
	varType char(1),
	varName varchar (255),
	strvalue varchar (4000),
	numvalue numeric(20,5),
	datevalue TIMESTAMP,
	primary key (instanceid, scopeid, varName)
);

CREATE TABLE MONITORNMPROPERTY (
	instanceid varchar(128),
	scopeid numeric(16,0),
	varid numeric(16,0),
	propname varchar(1028),
	propvalue clob (2 G),
	primary key (instanceid, scopeid, varid, propname)
);

CREATE TABLE MONITORNMSIMPLEPROPERTY (
	instanceid varchar(128),
	scopeid numeric(16,0),
	varid numeric(16,0),
	propname varchar(1028),
	propvalue varchar(4000),
	primary key (instanceid, scopeid, varid, propname)
);

CREATE TABLE MONITORVARIABLEATTACHMENT (
	instanceid varchar(128),
	scopeid numeric(16,0),
	varid numeric(16,0),
	name varchar(1028),
	attachment blob (2 G),
	primary key (instanceid, scopeid, varid, name)
);

CREATE TABLE MONITORBPELACTIVITYVARIABLE (
    instanceid varchar(128),
    varid numeric(16,0),
    activityid numeric(16,0),
    varname varchar(255),
    isfault char(1) DEFAULT 'N',
    varvalue clob (2 G),
    vardatatype  varchar(32) DEFAULT 'STRING',
    vartype varchar(32),
    primary key (instanceid, activityid, varname, vartype)
);

CREATE INDEX idx_BPELID ON MONITORBPELINSTANCE(BPELID);

CREATE INDEX idx_CRMPINVOKEID ON MONITORBPELACTIVITY(CRMPINVOKEID);

CREATE INDEX idx_CRMPRECEIVEID ON MONITORBPELACTIVITY(CRMPRECEIVEID);
