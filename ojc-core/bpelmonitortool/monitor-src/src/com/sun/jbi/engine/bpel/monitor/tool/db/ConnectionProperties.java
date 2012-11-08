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
 * @(#)ConnectionProperties.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.monitor.tool.db;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public interface ConnectionProperties {
    /** persistence enabled tag */
    String PERSISTENCEENABLED = "PersistenceEnabled"; //$NON-NLS-1$

    /** database URL tag */
    String DB_URL = "DB_URL"; //$NON-NLS-1$

    /** database user name tag */
    String DB_USERNAME = "DB_UserName"; //$NON-NLS-1$

    /** database user password tag */
    String DB_PASSWORD = "DB_Password"; //$NON-NLS-1$

    /** database type tag */
    String DB_TYPE = "DB_Type"; //$NON-NLS-1$

    /** jndi name tag */
    String DatabaseXAJNDIName = "DatabaseXAJNDIName"; //$NON-NLS-1$

    /** jndi name tag */
    String DatabaseNonXAJNDIName = "DatabaseNonXAJNDIName"; //$NON-NLS-1$
    
    String DB_DRIVER_CLASS_NAME="DB_Driver_Class_Name";

    /** max thread count tag */
    String MAX_THREAD_COUNT = "MAX_THREAD_COUNT";
    
    // DB Type values.

     /** Oracle */
    Integer ORCL_DB = new Integer(0);

    /** DB2 */
    Integer DB2_DB = new Integer(1);

    /** sybase */
    Integer SYBS_DB = new Integer(2);

    /** point base */
    Integer PBASE_DB = new Integer(3);

    /** derby */
    Integer DERBY_DB = new Integer(4);
}
