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

package com.sun.jbi.dcombc.dcom;

import java.util.Map;

import com.sun.jbi.dcombc.Endpoint;
import com.sun.jbi.dcombc.DCOMException;
import com.sun.jbi.dcombc.extensions.DCOMAddress;
import com.sun.jbi.dcombc.extensions.DCOMOperation;
import com.sun.jbi.dcombc.extensions.DCOMInput;
import com.sun.jbi.dcombc.extensions.DCOMOutput;

/**
 * A channel allows for sending and receiving DCOM messages
 * It is associated with the web services endpoint and operation
 * 
 * @author Chandrakanth Belde
 */
public interface Channel {

    /**
     * Get the Endpoint associated with this channel
     *
     * @return assoicated endpoint
     */
    public Endpoint getEndpoint();

    /**
     * Get corresponding conn info for this channel
     * 
     * @return dcom connection information bean
     */
    public DCOMConnInfo getDCOMConnInfo();
    
    /**
     * Get corresponding operation for this channel
     * 
     * @return dcom operation bean
     */
    public DCOMOperation getDCOMOperation();
    
    /**
     * Get corresponding operation input for this channel
     * 
     * @return dcom operation input bean
     */
    public DCOMInput getDCOMOperationInput();
    
    /**
     * Get corresponding operation output for this channel
     * 
     * @return dcom operation output bean
     */
    public DCOMOutput getDCOMOperationOutput();
    
    /**
     * Opens the channel by doing the following:
     *
     * Creates connection to DCOm Queue for InOnly Inbound 
     *
     * @throws DCOMException upon error
     */
    public void open() throws DCOMException;
    
    /**
     * to do 
     */
    public String receive() throws DCOMException;
    

    /**
     * Closes the channel by doing the following:
     *
     * Closes and opened connection.
     * 
     */
    public void close() throws DCOMException;

    /**
     * 
     * @param val
     * @return
     * @throws DCOMException
     */
    public String invoke(String val) throws DCOMException;


}
