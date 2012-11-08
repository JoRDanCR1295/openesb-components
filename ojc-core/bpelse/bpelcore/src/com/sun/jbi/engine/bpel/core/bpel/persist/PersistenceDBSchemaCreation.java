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
 * @(#)$Id: PersistenceDBSchemaCreation.java,v 1.11 2008/10/08 23:02:49 vinayram Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.persist;

public class PersistenceDBSchemaCreation extends DBSchemaCreation {

    /** ENGINE table name */
    public static final String ENGINE = "ENGINE "; //$NON-NLS-1$

    /** STATE table name */
    public static final String STATE = "STATE "; //$NON-NLS-1$

    /** VARIABLE table name */
    public static final String VARIABLE = "VARIABLE "; //$NON-NLS-1$

    /** VARIABLE table name */
    public static final String SIMPLEVARIABLE = "SIMPLEVARIABLE "; //$NON-NLS-1$

    /** INSTANCECORRELATION table name */
    public static final String INSTANCECORRELATION = "INSTANCECORRELATION "; //$NON-NLS-1$

    /** ENGINECORRELATION table name */
    public static final String ENGINECORRELATION = "ENGINECORRELATION "; //$NON-NLS-1$

    /** LASTCHECKPOINT table name */
    public static final String LASTCHECKPOINT = "LASTCHECKPOINT "; //$NON-NLS-1$

    /** FOREACH table name */
    public static final String FOREACH = "FOREACH "; //$NON-NLS-1$

    public static final String WAITINGIMA = "WAITINGIMA "; //$NON-NLS-1$

    /** FAULTHANDLERDATA table name */
    public static final String SCOPE = "SCOPE "; //$NON-NLS-1$

    /** CRMP table name: table used to persist data for the recovery of Sub-BP
     * (InOut message invoked BP's
     */
    public static final String CRMP = "CRMP "; //$NON-NLS-1$

    /** EVENTHANDLER table name */
    public static final String EVENTHANDLER = "EVENTHANDLER "; //$NON-NLS-1$


    /** Partner Link table name */
    public static final String PARTNERLINK = "PARTNERLINK "; //$NON-NLS-1$

    public static final String OUTSTANDINGMSGEX = "OUTSTANDINGMSGEX "; //$NON-NLS-1$
    
    public static final String NMPROPERTY = "NMPROPERTY "; //$NON-NLS-1$
    
    public static final String NMSIMPLEPROPERTY = "NMSIMPLEPROPERTY "; //$NON-NLS-1$
    
    public static final String VARIABLEATTACHMENT = "VARIABLEATTACHMENT "; //$NON-NLS-1$
    
    private static String [] PERSISTENCE_TABLES = new String [] {ENGINE.trim(), STATE.trim(), 
    	WAITINGIMA.trim(), VARIABLE.trim(), INSTANCECORRELATION.trim(), ENGINECORRELATION.trim(), 
        LASTCHECKPOINT.trim(), SCOPE.trim(), CRMP.trim(), SIMPLEVARIABLE.trim(), EVENTHANDLER.trim(), 
        PARTNERLINK.trim(), FOREACH.trim(), OUTSTANDINGMSGEX.trim(), NMPROPERTY.trim(), 
        NMSIMPLEPROPERTY.trim(), VARIABLEATTACHMENT.trim()};

    private static PersistenceDBSchemaCreation mSingleton = new PersistenceDBSchemaCreation();
    private static String CREATE_TABLES_SCRIPT = "create_bpelse_tables.sql"; //$NON-NLS-1$
    private static String DROP_TABLES_SCRIPT = "drop_bpelse_tables.sql"; //$NON-NLS-1$
    private static String TRUNCATE_TABLES_SCRIPT = "truncate_bpelse_tables.sql"; //$NON-NLS-1$	

    /**
     * Get singleton instance of DBSchemaCreation
     * @return DBSchemaCreation DSchemaCreation
     */
    public static PersistenceDBSchemaCreation getInstance() {
        return mSingleton;
    }


    @Override
    protected String getCreateScriptName() {
        return CREATE_TABLES_SCRIPT;
    }


    @Override
    protected String getDropScriptName() {
        return DROP_TABLES_SCRIPT;
    }


    @Override
    protected String[] getTabels() {
        return PERSISTENCE_TABLES;
    }


    @Override
    protected String getTruncateScriptName() {
        return TRUNCATE_TABLES_SCRIPT;
    }


}
