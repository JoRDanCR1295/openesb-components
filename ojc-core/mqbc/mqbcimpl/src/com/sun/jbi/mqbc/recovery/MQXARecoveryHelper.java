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
 * @(#)MQXARecoveryHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.mqbc.recovery;

import java.util.ArrayList;
import java.util.Iterator;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationListener;
import javax.transaction.xa.XAResource;

import com.sun.jbi.mqbc.extservices.MQClientAgent;
import com.sun.jbi.mqbc.extservices.MQClientConfiguration;
import com.sun.jbi.mqbc.mbeans.RuntimeConfiguration;

/**
 *
 * Helper class to deal with JMSJCA RA related wrapper classes
 */
public class MQXARecoveryHelper {
    
    private RuntimeConfiguration runtimeConfig = null;
    private ArrayList mqclntagent = new ArrayList();
    private ArrayList mqclntagents = new ArrayList();
    
    // Inner class to handle configuration change notifications
    private NotificationListener listener = new NotificationListener() {
        public void handleNotification(Notification notification, Object obj) {
            if (notification instanceof AttributeChangeNotification) {
                AttributeChangeNotification attrNotif =
                    (AttributeChangeNotification) notification;
                String attrName = attrNotif.getAttributeName();
                /*
                if (attrName.equals("AllowCrashOnCommit")) {  // todo: change this to use environment map
                    Boolean newVal = (Boolean) (attrNotif.getNewValue());
                    setAllowCrashOnCommit(newVal.booleanValue());
                } 
                 */
            }
        }
    };
    
    /** Creates a new instance of JMSSessionHelper */
    public MQXARecoveryHelper(RuntimeConfiguration runtimeConfig) {
        this.runtimeConfig = runtimeConfig;
    }    
        
    public XAResource getXAResource (MQConnectionInfoRecord rec) 
    throws Throwable {
        // Initialized  MQXAQueueManager  from first seen record
        
        MQClientConfiguration mqConfig = new MQClientConfiguration();
        mqConfig.setHost(rec.getHostName());
        mqConfig.setPort(rec.getPortNumber());
        mqConfig.setQueueManagerName(rec.getQueueManagerName());
        mqConfig.setXAMode(true);
        mqConfig.setChannelName(rec.getChannelName());
        mqConfig.setUser(rec.getUserName());
        mqConfig.setPassword(rec.getPassword());
        mqConfig.setCipherSuite(rec.getCipherSuite());
        mqConfig.setSslPeerName(rec.getSslPeerName());
        
        MQClientAgent   mClntAgnt = new MQClientAgent(mqConfig);
        
        
        mqclntagents.add(mClntAgnt);
        mClntAgnt.connect();
           
        LoggableXAResource xar = new LoggableXAResource(mClntAgnt.getXAResource());
        xar.logCalls(true);
        return xar;
    }
    
   
   
    
    public void closeOpenedResources() {
        Iterator mcIt =  this.mqclntagents.iterator();
        while (mcIt.hasNext()) {
            MQClientAgent mqclntagent = (MQClientAgent)mcIt.next();
            try {
                mqclntagent.invalidate();  
            } catch (Throwable t) {
                continue;
            }
        }
        this.mqclntagents.clear();
       
    }
}
