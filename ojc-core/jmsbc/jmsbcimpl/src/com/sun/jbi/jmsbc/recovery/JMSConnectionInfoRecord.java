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
 * @(#)JMSConnectionInfoRecord.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.recovery;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Properties;

/**
 *
 * Class representing JMS connection related information
 */
public class JMSConnectionInfoRecord implements ConnectionInfoRecord {
    private String connectionURL;
    private String username;
    private String password;
    private Properties jmsjcaOptions;

    /** Creates a new instance of JMSConnectionInfoRecord */
    public JMSConnectionInfoRecord() {        
    }
    
    /** Creates a new instance of JMSConnectionInfoRecord */
    public JMSConnectionInfoRecord(String connectionURL,
                                   String username,
                                   String password,
                                   String jmsjcaOptions) {
        this.connectionURL = connectionURL;
        this.username = username;
        this.password = password;
        if(jmsjcaOptions != null){
        	try {
            	this.jmsjcaOptions = new Properties();
				this.jmsjcaOptions.load(new ByteArrayInputStream(jmsjcaOptions.getBytes()));
			} catch (IOException e) {
				this.jmsjcaOptions = null;
			}
        }
    }
    
    /**
     * Gets the connection URL
     * @returns The connection URL
     */
    public String getConnectionURL () {
        return this.connectionURL;
    }
    
    /**
     * Gets the connection username
     * @returns The connection username
     */
    public String getUsername() {
        return this.username;
    }
    
    /**
     * Gets the connection password
     * @returns The connection password
     */
    public String getPassword() {
        return this.password;
    }

    /**
     * Sets the connection URL
     * @param  connectionURL The connection URL
     */
    public void setConnectionURL (String connectionURL) {
        this.connectionURL = connectionURL;
    }
    
    /**
     * Sets the connection username
     * @param  username The connection username
     */
    public void setUsername(String username) {
        this.username = username;
    }
    
    /**
     * Sets the connection password
     * @param  password The connection password
     */
    public void setPassword(String password) {
        this.password = password;
    }
    
    public boolean equals(ConnectionInfoRecord other) {
        boolean ret = false;        
        if (other instanceof JMSConnectionInfoRecord) {
            JMSConnectionInfoRecord otherJMSRec = (JMSConnectionInfoRecord)other;
            ret = stringsAreEqual(this.connectionURL, otherJMSRec.getConnectionURL()) &&
                  stringsAreEqual(this.username, otherJMSRec.getUsername()) &&
                  stringsAreEqual(this.password, otherJMSRec.getPassword()) &&
                  (this.jmsjcaOptions != null && this.jmsjcaOptions.equals(otherJMSRec.getJmsjcaOptions()));
        }        
        return ret;
    }
    
    public int hashCode() {
        return this.connectionURL.hashCode() + 
               (this.username==null? "".hashCode() : this.username.hashCode()) +
               (this.password==null? "".hashCode() : this.password.hashCode()) +
               (this.jmsjcaOptions==null? "".hashCode() : this.jmsjcaOptions.hashCode());
    }
    
    protected boolean stringsAreEqual (String thisString, String otherString) {
        boolean equal = false;
        if (thisString != null && otherString != null) {
            equal = thisString.equals(otherString);
        } else if (thisString==null && otherString==null) {
            equal = true;
        }
        return equal;
    }

	public Properties getJmsjcaOptions() {
		return jmsjcaOptions;
	}

	public void setJmsjcaOptions(Properties jmsjcaOptions) {
		this.jmsjcaOptions = jmsjcaOptions;
	}
}
