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
 * @(#)InstallerExtMBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc.bootstrap;

/**
 * MBean interface
 * 
 * @author aegloff, S. Nageswara Rao
 */
public interface InstallerExtMBean {

    /**
     *  MBean Getter for web page configuration
     *
     * @return number of threads
     */

    public String getThreads();

    /**
     *  MBean Setter for web page configuration
     *
     * @param val number of threads
     */
    public void setThreads(String val);

    /**
     * MBean Setter for web page configuration
     *
     * @param url database usl
     */
    public void setDB_URL(String url);

    /**
     * MBean Setter for web page configuration
     *
     * @param type database type
     */
    public void setDB_Type(String type);

    /**
     * MBean Setter for web page configuration
     *
     * @param username database username
     */
    public void setDB_UserName(String username);

    /**
     *  MBean Setter for web page configuration
     *
     * @param password database password
     */
    public void setDB_Password(String password);

    /**
     MBean Setter for web page configuration
     *
     * @param flag DB JNDI Name
     */
    public void setDB_JNDIName(String flag);

    /**
     * MBean Getter for web page configuration
     *
     * @return DB User Name
     */
    public String getDB_UserName();

    /**
     MBean Getter for web page configuration
     *
     * @return DB password
     */
    public String getDB_Password();

    /**
     MBean Getter for web page configuration
     *
     * @return JNDI Name
     */
    public String getDB_JNDIName();

    /**
     MBean Getter for web page configuration
     *
     * @return DB URL
     */
    public String getDB_URL();

    /**
     MBean Getter for web page configuration
     *
     * @return DB Type
     */
    public String getDB_Type();
}
