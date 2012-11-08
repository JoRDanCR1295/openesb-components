CREATE TABLE SERVICEUNIT (
    suname varchar(256),
    suziparchive longblob NOT NULL,
    lastupdatetime TIMESTAMP null,
    primary key (suname)
) ENGINE=InnoDB;

CREATE TABLE MONITORBPELPROCESS (
    suname varchar(256),
    bpelid varchar(744),
    genbpelevents char(1),
    primary key (bpelid)
) ENGINE=InnoDB;

CREATE TABLE MONITORBPELINSTANCE (
    engineid varchar(128),
    instanceid varchar(128),
    bpelid varchar(744),
    status varchar(32) DEFAULT 'RUNNING',  
    starttime TIMESTAMP null,
    endtime TIMESTAMP null,
    updatedtime TIMESTAMP null,    
    primary key (instanceid)
) ENGINE=InnoDB;

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
    starttime TIMESTAMP null,
    endtime TIMESTAMP null,
    primary key (instanceid, activityid, iteration)
) ENGINE=InnoDB;
	
CREATE TABLE MONITORBPELVARIABLE (
    instanceid varchar(128),
    scopeid numeric(16,0),
    varid numeric(16,0),
    varname varchar (255),
	isfault char(1) DEFAULT 'N',
    varvalue longtext,
    primary key (instanceid, scopeid, varname)
) ENGINE=InnoDB;

CREATE TABLE MONITORSIMPLEVARIABLE (
	instanceid varchar(128),
	scopeid numeric(16,0),
	varid numeric(16,0),
	varType char(1),
	varName varchar (255),
	strvalue varchar (4000),
	numvalue numeric(20,5),
	datevalue TIMESTAMP null,
	primary key (instanceid, scopeid, varName)
) ENGINE=InnoDB;

CREATE TABLE MONITORNMPROPERTY (
	instanceid varchar(128),
	scopeid numeric(16,0),
	varid numeric(16,0),
	propname varchar(736),
	propvalue longtext,
	primary key (instanceid, scopeid, varid, propname)
) ENGINE=InnoDB;

CREATE TABLE MONITORNMSIMPLEPROPERTY (
	instanceid varchar(128),
	scopeid numeric(16,0),
	varid numeric(16,0),
	propname varchar(736),
	propvalue varchar(4000),
	primary key (instanceid, scopeid, varid, propname)
) ENGINE=InnoDB;

CREATE TABLE MONITORVARIABLEATTACHMENT (
	instanceid varchar(128),
	scopeid numeric(16,0),
	varid numeric(16,0),
	name varchar(736),
	attachment longblob,
	primary key (instanceid, scopeid, varid, name)
) ENGINE=InnoDB;

CREATE TABLE MONITORBPELACTIVITYVARIABLE (
    instanceid varchar(128),
    varid numeric(16,0),
    activityid numeric(16,0),
    varname varchar(255),
    isfault char(1) DEFAULT 'N',
    varvalue longtext,
    vardatatype  varchar(32) DEFAULT 'STRING',
    vartype varchar(32),
    primary key (instanceid, activityid, varname, vartype)
) ENGINE=InnoDB;

CREATE INDEX idx_BPELID ON MONITORBPELINSTANCE(BPELID);

CREATE INDEX idx_CRMPINVOKEID ON MONITORBPELACTIVITY(CRMPINVOKEID);

CREATE INDEX idx_CRMPRECEIVEID ON MONITORBPELACTIVITY(CRMPRECEIVEID);
