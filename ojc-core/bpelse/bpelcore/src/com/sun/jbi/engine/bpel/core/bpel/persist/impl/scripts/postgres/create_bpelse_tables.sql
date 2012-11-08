CREATE TABLE ENGINE (
    engineid varchar(128),
    location varchar(1028),
    -- expiration number,
    lastupdatetime timestamp,
    primary key (engineid)
);
CREATE INDEX ENGINELASTUT on ENGINE (lastupdatetime);

CREATE TABLE STATE (
    stateid varchar(128),
    bpelid varchar(1028),
    engineid varchar(128),
    ownerlock char(1) DEFAULT 'Y',
    status varchar(32) DEFAULT 'RUNNING',
    primary key (stateid)
);
CREATE INDEX STATESTATUSINDEX on STATE (status);
CREATE INDEX STATEENGINEIDINDEX on STATE (engineid); 

CREATE TABLE WAITINGIMA (
    stateid varchar(128),
    partnerlink varchar(1028),
    operation varchar(1028),
    foreign key (stateid) references STATE (stateid)
);
CREATE INDEX WAITINGIMAINDEX on WAITINGIMA (stateid, partnerlink, operation);

CREATE TABLE VARIABLE (
    stateid varchar(128),
    varid decimal,
    scalabilitypassivated char(1) DEFAULT 'N',
    value text,
    scopeguid varchar(128),
    primary key (stateid, varid, scalabilitypassivated, scopeguid)
);
CREATE INDEX VARIABLEFKEY on VARIABLE (stateid);

CREATE TABLE NMPROPERTY (
	stateid varchar(128),
	scopeguid varchar(128),
	varid decimal,
	propname varchar(1028),
	propvalue text,
	primary key (stateid, scopeguid, varid, propname)
);
CREATE INDEX NMPROPERTYFKEY on NMPROPERTY (stateid);

CREATE TABLE NMSIMPLEPROPERTY (
	stateid varchar(128),
	scopeguid varchar(128),
	varid decimal,
	propname varchar(1028),
	propvalue varchar(4000),
	primary key (stateid, scopeguid, varid, propname)
);
CREATE INDEX NMSIMPLEPROPERTYFKEY on NMSIMPLEPROPERTY (stateid);

CREATE TABLE VARIABLEATTACHMENT (
	stateid varchar(128),
	scopeguid varchar(128),
	varid decimal,
	name varchar(1028),
	attachment text,
	primary key (stateid, scopeguid, varid, name)
);
CREATE INDEX VARIABLEATTACHMENTFKEY on VARIABLEATTACHMENT (stateid);

CREATE TABLE FOREACH (
	foreachid decimal,
	stateid varchar(128),
	counter integer,
	successes integer,
	startcount integer,
	finalcount integer,
	completioncount integer,
	primary key (foreachid, stateid)
);
CREATE INDEX FOREACHFKEY on FOREACH (stateid);

CREATE TABLE INSTANCECORRELATION (
    stateid varchar(128),
    inscorrid decimal,
    value varchar(4000),
    primary key (stateid, inscorrid)
);
CREATE INDEX INSTCORRFKEY on INSTANCECORRELATION (stateid);

CREATE TABLE ENGINECORRELATION (
    enginecorrid decimal,
    bpelid varchar(1028),
    engineid varchar(128),
    value varchar(4000),
    foreign key (engineid) references ENGINE(engineid)      
);
CREATE INDEX ENGINECORRIDINDEX ON ENGINECORRELATION (enginecorrid);

CREATE TABLE LASTCHECKPOINT (
    stateid varchar(128),
    activityid decimal,
    timeval TIMESTAMP,
    pickcompositeactid decimal,
    branchinvokecounter decimal DEFAULT 0,
    primary key (stateid, activityid)
);
CREATE INDEX LCPFKEY on LASTCHECKPOINT (stateid);

CREATE TABLE SCOPE (
    stateid varchar(128),
    scopeid decimal,
    scopeguid varchar(128),
    parentscopeguid varchar(128),
    scopestate varchar(3),
    compensateid decimal,
    completionorder decimal,
    faultname varchar(4000),
    faultdata text,
    faultactivityid decimal,
    primary key (stateid, scopeguid)
);
CREATE INDEX SCOPEFKEY on SCOPE (stateid);

CREATE TABLE CRMP (
	stateid varchar(128),
	crmpinvokeid varchar(128),
	partnerlink varchar(1028),
	operation varchar(1028),
	bpelmessageexchange varchar(4000),
	replyvariableid decimal DEFAULT -1,
	responseobj text,
	primary key (crmpinvokeid)
);
CREATE INDEX CRMPINDEX on CRMP (stateid, partnerlink, operation);
CREATE INDEX CRMPREPLYVARINDEX on CRMP (replyvariableid, stateid);
CREATE INDEX CRMPFKEY on CRMP (stateid);

CREATE TABLE SIMPLEVARIABLE (
    stateid varchar(128),
    varid decimal,
    stringvalue varchar(4000),
    scopeguid varchar(128),
    primary key (stateid, varid, scopeguid)
);
CREATE INDEX SIMPLEVARFKEY on SIMPLEVARIABLE (stateid);

CREATE TABLE EVENTHANDLER (
	stateid varchar(128), 
	ehid varchar(128), 
	scopeguid varchar(128),
	eventmodelid decimal,
	status char(1) DEFAULT 'I',
    timeval TIMESTAMP,
    repeatevery decimal,
    primary key (stateid, ehid), 
	foreign key (stateid) references STATE(stateid)
);

CREATE TABLE PARTNERLINK (
    stateid varchar(128),
    plinkid decimal,
    value text,
	scopeguid varchar(128),     
    primary key (stateid, plinkid, scopeguid)
); 
CREATE INDEX PARTNERLINKFKEY on PARTNERLINK (stateid);

CREATE TABLE OUTSTANDINGMSGEX (
    msgexid varchar(128),
    stateid varchar(128),
    bpelid varchar(1028),
	primary key (msgexid, bpelid)
); 