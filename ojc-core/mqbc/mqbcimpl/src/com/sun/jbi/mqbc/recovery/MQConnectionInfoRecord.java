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
 * @(#)MQConnectionInfoRecord.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.mqbc.recovery;

import com.sun.jbi.mqbc.extservices.MQClientAgent;

/**
 * MQ Connection information.
 */
public class MQConnectionInfoRecord implements ConnectionInfoRecord {
   
    
    private String hostName;
    private int portNumber=1414;
    private String queueManagerName;
    private String channelName;
    private String userID;
    private String password;
    private String cipherSuite;
    private String sslPeerName;
    

    public MQConnectionInfoRecord() {        
    }
    
    public MQConnectionInfoRecord(String hostName,
                                  int portNumber,
                                   String queueManagerName,
                                   String channleName,
                                   String userID,
                                   String password,
                                   String cipherSuite,
                                   String sslPeerName) {
        this.hostName = hostName;
        this.portNumber = portNumber;
        this.queueManagerName  = queueManagerName;
        this.channelName = channleName;
        this.userID = userID;
        this.password = password;
        this.cipherSuite = cipherSuite;
        this.sslPeerName = sslPeerName;
        
    }
    
    public String getHostName () {
        return clean(hostName);
    }
    
    public int getPortNumber () {
        if (portNumber < 1) {
            return MQClientAgent.MQ_DEFAULT_PORT;
        } else {
            return portNumber;
        }
    }
    
    public String getQueueManagerName () {
        return clean(queueManagerName);
    }
    
    public String getChannelName () {
        return clean(channelName);
    }
    
    public String getUserName() {
        return clean(userID);
    }
    
    public String getPassword() {
        return clean(password);
    }

    public String getCipherSuite() {
        return clean(cipherSuite);
    }
    
    public String getSslPeerName() {
        return clean(sslPeerName);
    }
    
    public void setHostName(String hostname) {
        this.hostName = clean(hostname);
    }
    
    public void setPortNumber(int portNumber) {
        if (this.portNumber < 1) {
            this.portNumber = MQClientAgent.MQ_DEFAULT_PORT;
        } else {
            this.portNumber = portNumber;
        }
    }
    
    public void setQueueManagerName (String queueManagerName) {
        this.queueManagerName = clean(queueManagerName);
    }
    
    public void setChannelName (String channelName) {
        this.channelName = clean(channelName);
    }
    
    public void setUserName(String userID) {
        this.userID = clean(userID);
    }
    
    public void setPassword(String password) {
        this.password = clean(password);
    }
    
    public void setCipherSuite(String cipherSuite) {
        this.cipherSuite = clean(cipherSuite);
    }
    
    public void setSslPeerName(String sslPeerName) {
        this.sslPeerName = clean(sslPeerName);
    }
    
    public boolean equals(ConnectionInfoRecord that) {
        boolean ret = false;        
        if (that instanceof MQConnectionInfoRecord) {
            MQConnectionInfoRecord other = (MQConnectionInfoRecord)that;
            ret = getHostName().equals(other.getHostName())
                    && getPortNumber() == other.getPortNumber()
                    && getQueueManagerName().equals(other.getQueueManagerName())
                    && getChannelName().equals(other.getChannelName())
                    && getUserName().equals(other.getUserName())
                    && getPassword().equals(other.getPassword())
                    && getCipherSuite().equals(other.getCipherSuite())
                    && getSslPeerName().equals(other.getSslPeerName());
        }        
        return ret;
    }
    
    public int hashCode() {
        return getHostName().hashCode()
                + getPortNumber()
                + getQueueManagerName().hashCode()
                + getChannelName().hashCode()
                + getUserName().hashCode()
                + getPassword().hashCode()
                + getCipherSuite().hashCode()
                + getSslPeerName().hashCode();
    }
        
    private String clean(String value) {
        return (value != null ? value : "");
    }
}
