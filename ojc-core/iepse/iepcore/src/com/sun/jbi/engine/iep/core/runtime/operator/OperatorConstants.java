/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)OperatorConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.engine.iep.core.runtime.operator;

import com.sun.jbi.engine.iep.core.share.SharedConstants;

/*
 * OperatorConstants.java
 *
 * Created on July 26, 2005, 4:07 PM
 *
 * @author Bing Lu
 */
public interface OperatorConstants extends SharedConstants {
    // Global table names
    public static final String TABLE_EMS_TOKEN = "ems_token";

    public static final String TABLE_EMS_ENGINE = "ems_engine";

    public static final String TABLE_EMS_PLAN = "ems_plan";

    public static final String TABLE_EMS_PLAN_VERSIONS = "ems_plan_versions";
    
    public static final String TABLE_EMS_OUTPUT = "ems_output";

    public static final String TABLE_EMS_PROCESSING_STATE = "ems_processing_state";

    public static final String TABLE_EMS_TABLE_USAGE = "ems_table_usage";
    
    // Gobal sequence names
    public static final String SEQUENCE_EMS_ENGINE = "ems_engine_sequence";
    
    // Table column names
    public static final String COL_IDX = "idx";
    
    public static final String COL_INSTANCE_LOCATION = "instance_location";
    
    public static final String COL_LEASE_EXPIRATION = "lease_expiration";
    
    public static final String COL_ENGINE_ID = "engine_id";
    
    public static final String COL_ID = "id";
    
    public static final String COL_NAME = "name";
    
    public static final String COL_VERSION_ID = "version_id";
    
    public static final String COL_CONTENT = "content";
    
    public static final String COL_STATUS = "status";
    public static final String STATUS_SAVED = "saved";
    public static final String STATUS_DEPLOYED = "deployed";
    public static final String STATUS_STARTED = "started";
    public static final String STATUS_STOPPED = "stopped";
    public static final String STATUS_UNDEPLOYED = "undeployed";
    
    public static final String COL_PLAN_ID = "plan_id";
    
    public static final String COL_SCHEMA_ID = "schema_id";
    
    public static final String COL_INSTANCE_ID = "instance_id";

    public static final String COL_LAST_ACTIVATION = "last_activation";
    
    public static final String COL_LAST_MODIFIED = "last_modified";
    
    public static final String COL_PROCESSING_STATE = "processing_state"; 
    
    public static final String COL_PREV_TIMESTAMP_TO_CHECK = "prev_timestamp_to_check";
    
    public static final String COL_PREV_TIMESTAMP_TO_PROCESS = "prev_timestamp_to_process";

    public static final String COL_TABLE_NAME = "table_name";

    public static final String COL_USER_ID = "user_id";
    
    public static final String COL_TAG = "ems_tag";
    
    public static final String COL_PSEQID = "ems_pseqid";
    
    public static final String COL_INPUT_ID = "ems_input_id";
    
    public static final String COL_INPUT_SEQID = "ems_input_seqid";
    
    public static final String COL_PLAN_INSTANCE_ID = "plan_instance_id";
    
    
    public static final String COL_OUTPUT_NAME = "output_name";

    public static final String COL_OUTPUT_DESCRIPTION = "output_description";
    
    public static final String COL_OUTPUT_LAST_UPDATED= "last_updated";
    
    public static final String COL_MISS_SEQ = "ems_miss_seq";
    
    public static final String COL_MAX_SEQ = "ems_max_seq";
    
    public static final String COL_PROCESSING_TIME = "ems_processing_time";
    
    public static final String COL_UPDATE = "ems_udpate";

    // Property names
    // Configuration properties ================================================
    public static final String PROP_DB_JNDI_CONTEXT = "DbJndiContext";
    
    public static final String PROP_DB_NON_XA_JNDI_NAME = "DatabaseNonXaJndiName";
    
    public static final String PROP_DB_XA_JNDI_NAME = "DatabaseXaJndiName";
    
    public static final String PROP_DB_HOSTNAME = "DatabaseHostname";

    public static final String PROP_DB_PORT = "DatabasePort";

    public static final String PROP_DB_USERNAME = "DatabaseUsername";

    public static final String PROP_DB_PASSWORD = "DatabasePassword";

    public static final String PROP_DB_SCHEMA = "DatabaseSchemaName";

    public static final String PROP_DB_SID = "DatabaseSid";

    public static final String PROP_DB_TYPE = "DatabaseType";
    public static final String DB_TYPE_ORACLE = "oracle";
    public static final String DB_TYPE_DERBY = "derby";
    public static final String DB_TYPE_MYSQL = "mysql";

    // Fortent (jtaylor) 2008-11-21 DB2 constants
    public static final String DB_TYPE_DB2 = "db2";
    public static final String PROP_DB2_DATABASENAME = "dataBaseName";
    public static final String PROP_DB2_DRIVER_TYPE = "driverType";
    public static final String PROP_DB2_USER = "user";
    public static final String PROP_DB2_PASSWORD = "password";
    public static final String PROP_DB2_PORTNUMBER = "portNumber";
    public static final String PROP_DB2_SERVERNAME = "serverName";
    
    public static final String PROP_RUNTIME_STYLE = "RuntimeStyle";
    public static final String RUNTIME_STYLE_STAND_ALONE = "standAlone";
    public static final String RUNTIME_STYLE_EMBEDDED = "embedded";
    
    public static final String PROP_IEP_DERBY_JAR_PATH = "IepDerbyJarPath";
    
    public static final String PROP_GARBAGE_COLLECTION_ENABLED = "GarbageCollectionEnabled";
    
    public static final String PROP_ENGINE_ID = "EngineId";
    
    public static final String PROP_ENGINE_EXPIRY_INTERVAL = "EngineExpiryInterval";
    public static final int ENGINE_EXPIRY_INTERVAL_FACTORY_DEFAULT = 60; // seconds
    
    public static final String PROP_MAXIMUM_BATCH_SIZE = "MaximumBatchSize";
    public static final int MAXIMUM_BATCH_SIZE_FACTORY_DEFAULT = 2; // seconds
    
//    public static final String PROP_THREADS_COUNT = "NoOfThreads";
    
    public static final String PROP_TRANSACTED_OUTPUT = "TransactedOutput";
    
    
    //==========================================================================
     
    public static final String PROP_CONFIG_PROPERTIES = "configProperties";
    
    public static final String PROP_PLAN_ID = "planId";
    
    public static final String PROP_PLAN_NAME = "planName";

    public static final String PROP_PLAN_CONTENT = "planContent";

    public static final String PROP_QUERY_PLAN = "queryPlan";
    
    public static final String PROP_IS_CLUSTERED = "isClustered";
    
    public static final String PROP_INSTANCE_ID = "instanceId";
    
    public static final String PROP_TIME_UNIT = "timeUnit";
    
    public static final String PROP_OP_NAME = "opName";
    
    public static final String PROP_DB_SPECIAL = "DbSpecial";
    
    public static final String PROP_WEB_SERVICE_CONTEXT = "webServiceContext";
    
    // Processing state
    public static final String PS_PLAN_STATE = "psPlanState";

    public static final String PS_PREV_TIMESTAMP_TO_PROCESS = "psPrevTimestampToProcess";
    
    public static final String PS_OPERATOR_STATE = "psOperatorState";
    
    public static final String PS_PREV_SEQID_TO_PROCESS = "psPrevSeqidToProcess";
    
    public static final String PS_PREV_TIME_MARKER = "psPrevTimeMarker";
    
    public static final String PS_PREV_TIMESTAMP_TO_CHECK = "psPrevTimestampToCheck";
    
    // Table status
    public static final int TS_UNKNOWN = 0;

    public static final int TS_NAME_NOT_EXIST = 1;
    
    public static final int TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA = 2;
    
    public static final int TS_TABLE_EXIST = 3;
    
    public static final String INITIAL_PROCESSING_STATE = "initialProcessingState";
    
    public static final String DELIM = "|";
    
    public static final String SINGLE_QUOTE_SUB = "!";
    
}
