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

import com.sun.jbi.swiftbc.extensions.jni.SAGJNIMessage;
import com.sun.jbi.swiftbc.extensions.jni.ExcStatus;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.swiftbc.extensions.SwiftMessage;
import java.util.logging.Level;
import java.util.logging.Logger;
// import com.stc.connector.management.jca.system.mbeans.STCActivationSpecMonitorBean;
// import com.stc.connector.management.jca.system.mbeans.STCActivationSpecMonitorBean;
/*
import com.stc.connector.logging.LogFactory;
import com.stc.connector.logging.Logger;
*/
/**
 * SAGPoller listens for inbound events from the SAG External Application.
 * Objects of this class type are designed to run within a Resource Adapter,
 * using information from an ActivationSpec to initialize and control its
 * polling behavior.  Upon receiving an SAG event, the SAGPoller spawns off
 * a SAGPollerTask to accomplish the actual work of invoking a SAGJNIMessage-Driven
 * Bean and passing that bean the SAG inbound event.  All SAG inbound events
 * are represented as SAGJNIMessage objects.
 * 
 * 
 * 
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.1 $   Last Modified: $Date: 2007/04/20 05:37:51 $
 */
public class SAGPoller implements Runnable {

    private static Logger logger =Messages.getLogger(SAGPoller.class);
        
    private SAGServiceProvider activationSpec;
    private boolean keepRunning = false;

    /**
     * Constructor used to create a SAG Poller
     *
     * @param        activationSpec the activationSpec used to start
     * up this thread
     */
    public SAGPoller(SAGServiceProvider activationSpec) {
        this.activationSpec = activationSpec;
    }

    /**
     * Activates the SAG Poller to start waiting for events from the
     * SAG EIS
     *
     */
    public void run() {
        logger.log(Level.INFO,"Running SAGPoller.");
        keepRunning = true;
        while (keepRunning) {
            try {

//                 STCActivationSpecMonitorBean mBean = activationSpec.getMBean();
//                 if (mBean != null) {
//                     int counter = 0;
//                     while (mBean.isSuspended().booleanValue() && keepRunning) {

//                         if (counter % 10 == 0) {
//                             logger.info("SAG Inbound eWay SUSPENDED.");
//                         }
//                         counter++;

//                         try {
//                             Thread.sleep(1000);
//                         } catch (Exception ex) {
//                             // Ignore on purpose
//                         }

//                     }

//                     if (!keepRunning) continue;
//                 }

                SwiftMessage response = null;
                long token = -1;
                try {
                    logger.log(Level.INFO,"Waiting for a message from SAG");
                    token =
                        activationSpec.getSAGConnection().getRequest(5000,
                                                                     response);
                } catch (ExcStatus ex) {
                    if (ex.getCode().equals("Sag:APL-I.001.005")) {
                        logger.log(Level.INFO,"Timed out without getting any message from SAG");
                        continue;
                    } else {
                        throw ex;
                    }
                }

                logger.log(Level.INFO,"Got a message from SAG");
                SAGPollerTask task =
                    new SAGPollerTask(response,/*activationSpec.getMessageEndpointFactory()*/
                                      token,
                                      activationSpec.getSAGConnection());
                new Thread(task).start();

            } catch (Throwable th) {
                logger.log(Level.SEVERE,"Error processing SAGMessage", th);
            }
        }
        logger.info("SAGPoller shutting down");
    }

    public void end() {
        keepRunning = false;
    }
    
}

