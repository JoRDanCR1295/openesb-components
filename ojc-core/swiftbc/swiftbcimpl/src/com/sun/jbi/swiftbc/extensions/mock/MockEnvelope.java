/*
 * MockEnvelope.java
 *
 * Created on April 7, 2007, 7:35 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.swiftbc.extensions.mock;
import com.sun.jbi.swiftbc.extensions.SwiftEnvelope;
import com.sun.jbi.swiftbc.extensions.SwiftNamedItemList;
import com.sun.jbi.swiftbc.extensions.SwiftRoutingList;

/**
 *
 * @author Sun Microsystems, Inc.
 */
public class MockEnvelope implements SwiftEnvelope {
    private String senderAuth;
    private String sender;
    private String receiver;
    private String messageFormat;
    private String localAuth;
    private String contextId;
    private String applicationStatus;
    private String applicationId;
    private String messageRef;
    private SwiftNamedItemList nilist;
    private SwiftRoutingList reqRoutingList;
    private SwiftRoutingList responseRoutingList;
    /** Creates a new instance of MockEnvelope */
    public MockEnvelope() {
    }

    public void setSenderAuth(String value) {
        senderAuth = value;
    }

    public void setSender(String value) {
        sender = value;
    }

    public void setReceiver(String value) {
        receiver = value;
    }

    public void setMsgFormat(String value) {
        messageFormat = value;
    }

    public void setLocalAuth(String value) {
        localAuth = value;
    }

    public void setContextId(String value) {
        contextId = value;
    }

    public void setApplicationStatus(String value) {
        applicationStatus = value;
    }

    public void setApplicationId(String value) {
        applicationId = value;
    }

    public String getApplicationId() {
        return applicationId;
    }

    public String getApplicationStatus() {
        return applicationStatus;
    }

    public String getContextId() {
        return contextId;
    }

    public String getLocalAuth() {
        return localAuth;
    }

    public String getMsgFormat() {
        return messageFormat;
    }

    public String getMsgRef() {
        return messageRef;
    }

    public SwiftNamedItemList getNamedItemList() {
        return nilist;
    }

    public String getReceiver() {
        return receiver;
    }

    public SwiftRoutingList getRoutingListRequest() {
        return reqRoutingList;
    }

    public SwiftRoutingList getRoutingListResponse() {
        return responseRoutingList;
    }

    public String getSender() {
        return sender;
    }

    public String getSenderAuth() {
        return senderAuth;
    }

    public long getPeer() {
        return 1;
    }
    
}
