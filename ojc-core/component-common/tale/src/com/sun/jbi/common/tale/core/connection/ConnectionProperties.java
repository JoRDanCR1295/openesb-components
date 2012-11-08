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

package com.sun.jbi.common.tale.core.connection;


/**
 * @author Sun Microsystems
 */
public interface ConnectionProperties {
	
    /** database URL tag */
    String DB_URL = "DB_URL"; //$NON-NLS-1$

    /** database user name tag */
    String DB_USERNAME = "DB_UserName"; //$NON-NLS-1$

    /** database user password tag */
    String DB_PASSWORD = "DB_Password"; //$NON-NLS-1$

    /** database type tag */
    String DB_TYPE = "DB_Type"; //$NON-NLS-1$

    /** jndi name tag */
    String DatabaseJNDIName = "DatabaseJNDIName"; //$NON-NLS-1$

    /** max thread count tag */
    String MAX_THREAD_COUNT = "MAX_THREAD_COUNT";
    
    /** database Host tag */
    String DB_HOST = "DB_Host"; //$NON-NLS-1$
    
    /** database Port tag */
    String DB_PORT = "DB_Port"; //$NON-NLS-1$
    
    /** database Instance/SID tag */
    String DB_INSTANCE = "DB_Instance"; //$NON-NLS-1$
    
}
