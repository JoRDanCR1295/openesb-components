/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/

package com.sun.jbi.dcombc.extensions;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;

import javax.xml.namespace.QName;

/**
 *
 * @author Chandrakanth Belde
 */
public class DCOMAddress implements ExtensibilityElement, Serializable {
	/**
	 *
	 */    
    private final long serialVersionUID = 1L;

    private QName fieldElementType = DCOMConstants.QNAME_ADDRESS;

    private Boolean fieldRequired = null;
    
    private String fieldDomain ;
    
    private String fieldServer ;
    
    private String fieldUsername ;
    
    private String fieldPassword ;
    
    public static String ATTR_DOMAIN = "domain";
    
    public static String ATTR_SERVER = "server";    
    
    public static String ATTR_USERNAME = "username";
    
    public static String ATTR_PASSWORD = "password";

    public DCOMAddress() {
		//
    }

    /**
     * Get the extensibility element type
     * 
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return fieldElementType;
    }

    /**
     * Set the extensibility element type
     * 
     * @param elementType the type
     */
    public void setElementType(QName elementType) {
        fieldElementType = elementType;
    }

    /**
     * Get whether required (for wsdl:required)
     * 
	 * @return the extensibility element's required
     */
    public Boolean getRequired() {
        return fieldRequired;
    }

    /**
     * Set whether required (for wsdl:required)
	 *
	 * @param the extensibility element's required
     */
    public void setRequired(Boolean required) {
        fieldRequired = required;
    }

    /**
	 * Get domain of this address element
	 *
	 * @return the extensiblity element's domain
	 */
    public String getDomain() {
        return fieldDomain;
    }

	/**
	 * Set domain for this address element
	 *
	 * @param the extensibility element's domain
	 */
    public void setDomain(String domain) {
        this.fieldDomain = domain;
    }

	/**
	 * Get server of this address element
	 *
	 * @return the extensiblity element's server
	 */
    public String getServer() {
        return fieldServer;
    }

	/**
	 * Set server for this address element
	 *
	 * @param the extensibility element's server
	 */
    public void setServer(String server) {
        this.fieldServer = server;
    }

	/**
	 * Get username of this address element
	 *
	 * @return the extensiblity element's username
	 */
    public String getUsername() {
        return fieldUsername;
    }

	/**
	 * Set username for this address element
	 *
	 * @param the extensibility element's username
	 */
    public void setUsername(String username) {
        this.fieldUsername = username;
    }

	/**
	 * Get password of this address element
	 *
	 * @return the extensiblity element's password
	 */
    public String getPassword() {
        return fieldPassword;
    }

	/**
	 * Set password for this address element
	 *
	 * @param the extensibility element's password
	 */
    public void setPassword(String password) {
        this.fieldPassword = password;
    }
    
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nDCOM Address (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired );
        strBuf.append("\nDomain=" + fieldDomain);
        strBuf.append("\nServer=" + fieldServer);        
        strBuf.append("\nUsername=" + fieldUsername);
        strBuf.append("\nPassword=" + fieldPassword);
        return strBuf.toString();
    }

}
