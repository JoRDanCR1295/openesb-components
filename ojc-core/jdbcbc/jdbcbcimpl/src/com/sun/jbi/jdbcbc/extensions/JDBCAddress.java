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
 * @(#)JDBCAddress.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc.extensions;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;

import javax.xml.namespace.QName;


/**
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class JDBCAddress implements ExtensibilityElement, Serializable {
    public static final String ATTR_JNDI_NAME = "jndiName";
	public static final String ATTR_DRIVER_NAME = "driverClassName";
	public static final String ATTR_URL_NAME = "dbURL";
	public static final String ATTR_USER_NAME = "userName";
	public static final String ATTR_PASSWORD_NAME = "password";

    private static final long serialVersionUID = 1L;
    private QName mFieldElementType = JDBCConstants.QNAME_ADDRESS;
    private Boolean mFieldRequired = null;
    private String mJndiName = null;

	private String mPassword = null;
	private String mUserName = null;
	private String mUrl = null;
	private String mDriver = null;


    public JDBCAddress() {
    }

    /**
     * @return mFieldElementType
     */
    //@Override
    public QName getElementType() {
        return mFieldElementType;
    }

    /**
     * @return mFieldRequired
     */
    //@Override
    public Boolean getRequired() {
        return mFieldRequired;
    }

    ///@Override
    public void setElementType(final QName elementType) {
        mFieldElementType = elementType;
    }

    //@Override
    public void setRequired(final Boolean required) {
        mFieldRequired = required;
    }

    protected void setJndiName(final String jndiName) {
        mJndiName = jndiName;
    }
    
    public String getJndiName() {
        return mJndiName;
    }

    protected void setPassword(final String password) {
        mPassword = password;
    }
    
    public String getPassword() {
        return mPassword;
    }

    protected void setUserName(final String username) {
        mUserName = username;
    }
    
    public String getUserName() {
        return mUserName;
    }

    protected void setDBUrl(final String url) {
        mUrl = url;
    }
    
    public String getDBUrl() {
        return mUrl;
    }

    protected void setDriverClass(final String driver) {
        mDriver = driver;
    }
    
    public String getDriverClass() {
        return mDriver;
    }
}
