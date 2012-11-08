/*
 * Envelope.java
 *
 * Created on April 7, 2007, 7:02 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.swiftbc.extensions;

/**
 *
 * @author Sun Microsystems, Inc.
 */
public interface SwiftEnvelope {
    
    public long getPeer();
        /**
     * Method setApplicationId.
     * @param value String
     */
    public void setApplicationId(String value);
    
    /**
     * Method setContextId.
     * @param value String
     */
    public void setContextId(String value);
    
    /**
     * Method setMsgFormat.
     * @param value String
     */
    public void setMsgFormat(String value);
    
    /**
     * Method setSender.
     * @param value String
     */
    public void setSender(String value);
    
    /**
     * Method setSenderAuth.
     * @param value String
     */
    public void setSenderAuth(String value);
    
    /**
     * Method setReceiver.
     * @param value String
     */
    public void setReceiver(String value);

    /**
     * Method setLocalAuth.
     * @param value String
     */
    public void setLocalAuth(String value);
    
    /**
     * Method setApplicationStatus.
     * @param value String
     */
    public void setApplicationStatus(String value);
    
    /**
     * Method getApplicationId.
     * @return String
     */
    public String getApplicationId();
    
    /**
     * Method getContextId.
     * @return String
     */
    public String getContextId();
    
    /**
     * Method getMsgFormat.
     * @return String
     */
    public String getMsgFormat();
    
    /**
     * Method getSender.
     * @return String
     */
    public String getSender();
    
    /**
     * Method getSenderAuth.
     * @return String
     */
    public String getSenderAuth();
    
    /**
     * Method getReceiver.
     * @return String
     */
    public String getReceiver();
    
    /**
     * Method getMsgRef.
     * @return String
     */
    public String getMsgRef();
    
    /**
     * Method getLocalAuth.
     * @return String
     */
    public String getLocalAuth();
    
    /**
     * Method getApplicationStatus.
     * @return String
     */
    public String getApplicationStatus();
    
    /**
     * Method getNamedItemList.
     * @return NamedItemList
     */
    public SwiftNamedItemList getNamedItemList();
    
    //newly-added for completion although deprecated
    public SwiftRoutingList getRoutingListRequest();
    public SwiftRoutingList getRoutingListResponse();

}
