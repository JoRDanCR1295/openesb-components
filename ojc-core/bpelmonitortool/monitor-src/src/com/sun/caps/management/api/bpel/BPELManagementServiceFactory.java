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
 * @(#)$Id: BPELManagementServiceFactory.java,v 1.1 2008/04/08 20:24:48 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.caps.management.api.bpel;

import javax.management.MBeanServerConnection;

import com.sun.caps.management.impl.bpel.BPELManagementServiceImpl;
import com.sun.jbi.engine.bpel.monitor.tool.util.CommandUtil;
import com.sun.jbi.engine.bpel.monitor.tool.util.I18n;

/**
 * The Factory class to create BPELManagementService
 *  
 */
public class BPELManagementServiceFactory {
    

    private static final String UNABLE_TO_GET_MBEANSERVERCONNECTION = "BPCOR-6005: Unable to get MbeanServerConnection -- host:{0}, port:{1}, user:{2}, password:{3}";
    private static final String MBEAN_SERVER_CAN_NOT_BE_NULL =  "BPCOR-6021: MBeanServerConnection can not be null";
    
    /**
     * Get the BPELManagementService by making remote JMX connection
     * @param jmxHostName  The host name
     * @param jmxPort  The portNumber, default is 8686
     * @param jmxUserName The admin userName
     * @param jmxPwd  The admin password
     * @return   BPELManagementService
     * @throws Exception
     * 
     */    
    public static BPELManagementService getBPELManagementServiceRemote (String jmxHostName,
            int jmxPort, String jmxUserName, String jmxPwd) throws Exception {
        MBeanServerConnection mbeanServerConn = null;        
        try {
            mbeanServerConn = CommandUtil.getMbeanServerConnection(jmxHostName, jmxPort, jmxUserName, jmxPwd);
            if (mbeanServerConn == null) {
                    String errorMsg = I18n.loc(UNABLE_TO_GET_MBEANSERVERCONNECTION, 
                    jmxHostName, jmxPort, jmxUserName, jmxPwd);              
                   throw new Exception (errorMsg); 
            }
            return new BPELManagementServiceImpl(mbeanServerConn, true);
        }catch (Exception e) {
                String errorMsg = I18n.loc(UNABLE_TO_GET_MBEANSERVERCONNECTION, jmxHostName, jmxPort, jmxUserName, jmxPwd);        
                throw  new Exception (errorMsg, e); 
            }
              
        }
    
    /**
     * Get the BPELManagementService by taking a local MBeanServerConnection. This is applicable when monitor client and
     * bpelse  co-locate on the same appserver and MbeanServerConnection can be obtained before calling this method
     * @param mbeanServerConn  The MbeanServerConnection to the AppServer
     * @return BPELManagementService
     * @throws Exception
     */
        public static BPELManagementService getBPELManagementServiceLocal (MBeanServerConnection mbeanServerConn) throws Exception {
            if (mbeanServerConn == null) {
                String errorMsg = I18n.loc(MBEAN_SERVER_CAN_NOT_BE_NULL); 
                throw  new Exception (errorMsg);                    
            }
            return new BPELManagementServiceImpl(mbeanServerConn, false);
        }

    
    }
    
    


