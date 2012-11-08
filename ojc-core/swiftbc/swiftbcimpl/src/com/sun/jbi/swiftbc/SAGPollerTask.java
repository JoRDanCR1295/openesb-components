/************************************************************************************
*
*   Copyright © 2006 Sun Microsystems, Inc., 4150 Network Circle, Santa Clara, 
*   California 95054, U.S.A. All rights reserved. Sun Microsystems, Inc. has 
*   intellectual property rights relating to technology embodied in the product 
*   that is described in this document. In particular, and without limitation, 
*   these intellectual property rights may include one or more of the U.S. patents 
*   listed at http://www.sun.com/patents and one or more additional patents or 
*   pending patent applications in the U.S. and in other countries. THIS PRODUCT 
*   CONTAINS CONFIDENTIAL INFORMATION AND TRADE SECRETS OF SUN MICROSYSTEMS, INC. 
*   USE, DISCLOSURE OR REPRODUCTION IS PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN 
*   PERMISSION OF SUN MICROSYSTEMS, INC.U.S. Government Rights - Commercial 
*   software.  Government users are subject to the Sun Microsystems, Inc. standard 
*   license agreement and applicable provisions of the FAR and its supplements.  
*   Use is subject to license terms.  This distribution may include materials 
*   developed by third parties. Sun, Sun Microsystems, the Sun logo, Java 
*   Composite Application Platform Suite,  SeeBeyond, eGate, eInsight, eVision, eTL,
*   eXchange, eView, eIndex, eBAM and  eWay are trademarks or registered trademarks of
*   Sun Microsystems, Inc. in the U.S. and other countries. All SPARC trademarks are 
*   used under license and are trademarks or registered trademarks of SPARC 
*   International, Inc. in the U.S. and other countries. Products bearing SPARC 
*   trademarks are based upon architecture developed by Sun Microsystems, Inc. 
*   UNIX is a registered trademark in the U.S. and other countries, exclusively 
*   licensed through X/Open Company, Ltd. This product is covered and controlled by 
*   U.S. Export Control laws and may be subject to the export or import laws in 
*   other countries.  Nuclear, missile, chemical biological weapons or nuclear 
*   maritime end uses or end users, whether direct or indirect, are strictly 
*   prohibited.  Export or reexport to countries subject to U.S. embargo or to 
*   entities identified on U.S. export exclusion lists, including, but not limited 
*   to, the denied persons and specially designated nationals lists is strictly 
*   prohibited.
*
*************************************************************************************/
package com.sun.jbi.swiftbc;

import com.sun.jbi.swiftbc.extensions.SAGMessageImpl;
import javax.jbi.messaging.MessageExchangeFactory;
import com.sun.jbi.swiftbc.extensions.jni.HandleServer;
import com.sun.jbi.internationalization.Messages;
import java.util.logging.Level;
import java.util.logging.Logger;
import com.sun.jbi.swiftbc.extensions.SwiftMessage;
import com.sun.jbi.swiftbc.extensions.SAGMessageImpl;



/**
 * SAGPollerTask defines a unit of work for the SAGPoller.  A unit of
 * work is the processing of a message from the SAG EIS and passing it
 * to the inbound message-driven bean.
 *
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.1 $   Last Modified: $Date: 2007/04/20 05:37:50 $
 *
 */
public class SAGPollerTask implements Runnable {

    private static Logger logger =Messages.getLogger(SAGPollerTask.class);
    private SwiftMessage messageToBeProcessed;
    private long token;
    private HandleServer eisConnection;

    /**
     * Default constructor
     *
     * @param        message the message to be processed
     * @param        messageEndpointFactory the factory used to create
     * message endpoints.  These message endpoints will receive the
     * message.
     */
    public SAGPollerTask(SwiftMessage message,
 //                        MessageEndpointFactory messageEndpointFactory,
                         long token,
                         HandleServer eisConnection) {
        this.messageToBeProcessed = message;
//        this.mef = messageEndpointFactory;
        this.token = token;
        this.eisConnection = eisConnection;
    }

    /**
     * Performs the Poller task.  This method creates an message endpoint
     * and passes the message to the message endpoint.
     */
    public void run() {

        try {
            // Create message endpoint and send message to endpoint.
            //MessageEndpoint me = mef.createEndpoint(null);
            Object me = null;
            if (me == null) {
                logger.log(Level.SEVERE,"MessageEndpointFactory returned a null MessageEndpoint.");
                throw new Exception ("MessageEndpointFactory returned a null MessageEndpoint.");
            }
        
            if (! (me instanceof SAGListener)) {
                logger.log(Level.SEVERE,"MessageEndpoint is not an instance of SAGEwayListener.");
                throw new Exception ("MessageEndpoint is not an instance of SAGEwayListener.");
            }
        
            logger.log(Level.INFO,"MessageEndpointFactory returned a SAGEwayListener MessageEndpoint.");
        
            SAGListener sagListener = (SAGListener)me;
            SwiftMessage message = new SAGMessageImpl(messageToBeProcessed);
            try {
                SwiftMessage response = sagListener.onSAGMessage(message);
                if (response != null) {
                    SAGMessageImpl responseImpl = (SAGMessageImpl)response;
                    SwiftMessage msg = responseImpl.getSwiftMessage();
                    eisConnection.putResponse(token, msg);
                }
            } catch (ApplicationException ex) {
                // What to do here?
            }
//            me.release();
        } catch (Exception ex) {
            logger.log(Level.SEVERE,"Error processing SAG Message", ex);
        }
    }
}
