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

import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.util.Random;
/*
import com.stc.configuration.IConfiguration;
import com.stc.connector.framework.eway.EwayEndpoint;
import com.stc.connector.framework.util.ConfigurationHelper;
import com.stc.connector.management.STCManagedSlave;
import com.stc.connector.management.util.ObjectReference;
 */
import com.sun.jbi.swiftbc.extensions.jni.HandleFactory;
import com.sun.jbi.swiftbc.extensions.jni.HandleServer;
import com.sun.jbi.internationalization.Messages;
import java.util.logging.Level;

import java.util.logging.Logger;

import javax.jbi.messaging.MessagingException;

/**
 *
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.2 $   Last Modified: $Date: 2007/05/15 23:54:06 $
 */
public class SAGServiceProvider {
    
    private static Logger logger = Messages.getLogger(SAGServiceProvider.class);
    
    private HandleServer eisConnection;
    private File cfgFile;
    private SAGPoller poller;    
    private String hostName;
    private long port = -1;
    private boolean useSSL = false;
    private String messagePartner;
    /**
     * Default constructor for the SAGEwayActivationSpec
     */
    public SAGServiceProvider() {
        if (logger.isLoggable(Level.INFO)) {
            logger.log(Level.INFO,"SAGEndpoint.SAGEndpoint(): Done [" + this + "].");
        }
    }
    
    /**
     * Activate eWay endpoint.
     *
     * @param mef The MessageEndpointFactory assoicated with the activation.
     * @param ari The instance of STCPropertiesInfo which contains the
     *        information for creating an endpoint activation.
     *
     * @throws NotSupportedException upon error.
     */
    public void activation()
    throws FaultException {
        
        logger.log(Level.INFO,"endpointActivation called.");
        
        
        String cfgFileName = Integer.toString(new Random().nextInt());
        
        
        
        try {
            // Set the path to the configuration file.  What should
            // our prefix and suffic for the directory name be?
            String cfgPath = HandleFactory.getEnvVar("SWNET_CFG_PATH");
            if (cfgPath == null || cfgPath.equals("")) {
                throw new Exception("SWNET_CFG_PATH environment variable is not set.  Please set this variable in the Integration Server properties");
            }
            
            File cfgDir = null;
            try {
                cfgDir = new File(cfgPath);
            } catch (Exception ex) {
                throw new Exception("The value set for SWNET_CFG_PATH is not a valid directory.");
            }
            
            if (!cfgDir.isDirectory()) {
                throw new Exception("The value set for SWNET_CFG_PATH is not a valid directory.");
            }
            
            cfgFile = new File(cfgDir, cfgFileName);
            PrintWriter writer = new PrintWriter(new FileWriter(cfgFile));
            writer.println("HostName = " +getHostName());
            writer.println("PortNumber = " + getPort());
            writer.println("SSLMode = " + isUseSSL());
            writer.flush();
            writer.close();
            
            HandleFactory obj = HandleFactory.newFactory(cfgFileName);
            setSAGConnection(obj.newHServer(messagePartner));
            
            poller = new SAGPoller(this);
            new Thread(poller).start();
            
        } catch (Exception ex) {
            MessagingException me =
                    new MessagingException(
                    "Failed to create a physical connection "
                    + "to EIS SAG."
                    + ex.toString());
            FaultException fe = new FaultException(me);
            throw fe;
        }
        
        logger.log(Level.INFO,"endpointActivation call completed.");
    }
    
    /**
     * Deactivate (terminate) eWay endpoint.
     *
     * @param mef The MessageEndpointFactory assoicated with the activation.
     */
    public void deactivation() {
        logger.info("endpointDeactivation called. ");
        poller.end();
        setSAGConnection(null);
        cfgFile.delete();
        cfgFile = null;
        logger.info("endpointDeactivation call completed.");
    }
    
    
    
    //
    // SAG specific calls
    //
    
    /**
     * Retrieves the JNI-based connection to the SAG Handle object
     *
     * @return       the JNI-based connection to an SAG Handle object
     */
    protected HandleServer getSAGConnection() {
        return eisConnection;
    }
    
    /**
     * Sets the JNI-based connection to the given SAG Handle object
     *
     * @param        eisConnection the JNI-based SAG Handle object
     */
    protected void setSAGConnection(HandleServer eisConnection) {
        this.eisConnection = eisConnection;
    }
            
    public String getHostName() {
        return hostName;
    }
    
    public void setHostName(String hostName) {
        this.hostName = hostName;
    }
    
    public long getPort() {
        return port;
    }
    
    public void setPort(long port) {
        this.port = port;
    }
    
    public boolean isUseSSL() {
        return useSSL;
    }
    
    public void setUseSSL(boolean useSSL) {
        this.useSSL = useSSL;
    }
    public void setMessagePartner(String s){
        messagePartner = s;
    }
    public String getMessagePartner(){
        return messagePartner;
    }

}
