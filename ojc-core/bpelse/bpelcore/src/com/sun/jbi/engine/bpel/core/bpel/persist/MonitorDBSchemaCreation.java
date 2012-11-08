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
 * @(#)$Id: MonitorDBSchemaCreation.java,v 1.7 2010/02/04 02:51:15 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.persist;

public class MonitorDBSchemaCreation extends DBSchemaCreation {	


	private static MonitorDBSchemaCreation mSingleton = new MonitorDBSchemaCreation();
	private static String CREATE_TABLES_SCRIPT = "create_bpelmonitor_tables.sql"; //$NON-NLS-1$
	private static String DROP_TABLES_SCRIPT = "drop_bpelmonitor_tables.sql"; //$NON-NLS-1$
	private static String TRUNCATE_TABLES_SCRIPT = "truncate_bpelmonitor_tables.sql"; //$NON-NLS-1$		

	/* Table Names */
	public static final String MONITOR_SIMPLE_VARIABLE = "MONITORSIMPLEVARIABLE";

	public static final String SERVICE_UNIT = "SERVICEUNIT";

	public static final String MONITOR_BPEL_VARIABLE = "MONITORBPELVARIABLE";

	public static final String MONITOR_BPEL_INSTANCE = "MONITORBPELINSTANCE";

	public static final String MONITOR_BPEL_ACTIVITY = "MONITORBPELACTIVITY";
	
	public static final String MONITOR_NM_PROPERTY = "MONITORNMPROPERTY";
	
	public static final String MONITOR_NM_SIMPLE_PROPERTY = "MONITORNMSIMPLEPROPERTY";
	
	public static final String MONITOR_VARIABLE_ATTACHMENT = "MONITORVARIABLEATTACHMENT";
	
	public static final String MONITOR_BPEL_ACTIVITY_VARIABLE = "MONITORBPELACTIVITYVARIABLE";
	
	public static final String MONITOR_BPEL_PROCESS = "MONITORBPELPROCESS";

	private static String [] MONITOR_TABLES = new String [] {SERVICE_UNIT, MONITOR_BPEL_INSTANCE, MONITOR_BPEL_ACTIVITY,
		MONITOR_BPEL_VARIABLE, MONITOR_SIMPLE_VARIABLE, MONITOR_NM_PROPERTY, MONITOR_NM_SIMPLE_PROPERTY, 
		MONITOR_VARIABLE_ATTACHMENT, MONITOR_BPEL_ACTIVITY_VARIABLE, MONITOR_BPEL_PROCESS};

	/**
	 * Get singleton instance of DBSchemaCreation
	 * @return DBSchemaCreation DSchemaCreation
	 */
	public static MonitorDBSchemaCreation getInstance() {
		return mSingleton;
	}


	@Override
	protected String getCreateScriptName() {
		// TODO Auto-generated method stub
		return CREATE_TABLES_SCRIPT;
	}


	@Override
	protected String getDropScriptName() {
		// TODO Auto-generated method stub
		return DROP_TABLES_SCRIPT;
	}


	@Override
	protected String[] getTabels() {
		// TODO Auto-generated method stub
		return MONITOR_TABLES;
	}


	@Override
	protected String getTruncateScriptName() {
		// TODO Auto-generated method stub
		return TRUNCATE_TABLES_SCRIPT;
	}


}
