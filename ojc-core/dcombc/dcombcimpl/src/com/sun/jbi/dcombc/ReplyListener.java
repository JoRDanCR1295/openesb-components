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
package com.sun.jbi.dcombc;

import javax.jbi.messaging.MessageExchange;


/**
 * Listener interface for processing reply.
 *
 * @author Chandrakanth Belde
 *
 */
public interface ReplyListener {
    /**
     * Process reply
     * @param me MessageExchange to process.
     * @returns A boolean true if successful processing of the reply; false otherwise.
     */
    public boolean onReply (MessageExchange me);
    
    /**
     * Get the request received time
     * @return Request invocation time.
     */
    public long getRequestInvocationTime();
}
