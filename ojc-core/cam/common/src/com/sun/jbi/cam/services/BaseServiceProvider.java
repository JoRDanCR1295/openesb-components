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
 * @(#)BaseServiceProvider.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.services;

import com.sun.jbi.cam.connectors.ServerConnector;
import com.sun.jbi.cam.services.administration.providers.glassfish.ServerInformation;
import com.sun.jbi.cam.services.administration.providers.glassfish.ServerInformationImpl;
import java.util.logging.Logger;
import javax.management.Attribute;
import javax.management.MBeanServerConnection;
import javax.management.ObjectName;

/**
 *
 * @author ylee
 */
public class BaseServiceProvider implements Service {
    
    protected ServerConnector serverConnector;    
    protected MBeanServerConnection serverConnection = null;
    protected String targetName;
    protected ServerInformation serverInfo;
    
    private Logger logger = Logger.getLogger(BaseServiceProvider.class.getName());
    
    
    /** Creates a new instance of BaseService */
    public BaseServiceProvider(ServerConnector connector,String targetName) {
        serverConnector = connector;
        serverConnection = serverConnector.getConnection();
        this.targetName = targetName;
        // do cluster setup
        setup();
    }
    
    protected void setup() {
        try {
            //  for cluster environment - use this class to get instance MBS
            serverInfo = new ServerInformationImpl(serverConnection);
            if ( serverInfo.isDAS() ) {
                 serverConnection = serverInfo.getMBeanServerConnection(targetName,false);
            }
        } catch(Exception e) {
            e.printStackTrace();
        }
        
        
    }
    
    
    protected String getTargetName() {
        return targetName;
    }
    
    
    public Object invoke(ObjectName objectName, String operationName) {
        Object resultObject = null;
        try {
            if( this.serverConnection != null && serverConnection.isRegistered(objectName) ) {
                    resultObject = (String) this.serverConnection.invoke(objectName,
                            operationName,
                            null,
                            null);
            }
        } catch(Exception e) {
           e.printStackTrace();
        }
        return resultObject;
    }    
    
    public Object invoke(ObjectName objectName, String operationName, Object[] parameters) {
        Object result = null;
        String[] signature = this.getSignatures(parameters);
        
        try {
            if (this.serverConnection != null && serverConnection.isRegistered(objectName) ) {
                result = serverConnection.invoke(objectName,
                        operationName,
                        parameters,
                        signature);
            }
        } catch(Exception e) {
            e.printStackTrace();
        }
        
        return result;
    }  

    public Object invoke(ObjectName objectName, String operationName, Object[] parameters, String[] signatures) {
        Object result = null;
        
        try {
            if (this.serverConnection != null && serverConnection.isRegistered(objectName) ) {
                result = serverConnection.invoke(objectName,
                        operationName,
                        parameters,
                        signatures);
            }
        } catch(Exception e) {
            e.printStackTrace();
        }
        
        return result;
    }  
    
    
    public long invokeLong(ObjectName objectName, String operationName, Object[] parameters) {
        Long result = (Long)invoke(objectName,operationName,parameters);
        long value = -1;
        if ( result!=null ) {
            value = result.longValue();
        }
        return value;
    }
    
    
    public Object getAttribute(ObjectName objectName, String attrName) {
        Object attrValue = null;
        try {
            
            if ( serverConnection!=null && serverConnection.isRegistered(objectName) ) {
                attrValue = serverConnection.getAttribute(objectName,attrName);
            }
            
        } catch(Exception e) {
            e.printStackTrace();
        }
        
        return attrValue;
    }
    
    
    protected String[] getSignatures(Object[] params) {
        if (params == null || params.length == 0) {
            return null;
        }
        String[] signatures = new String[params.length];
        for (int index = 0; index < params.length; index++) {
            if(params[index] == null) {
                signatures[index] = "java.lang.Object";
            } else {
                signatures[index] = params[index].getClass().getName();
            }
        }
        return signatures;
    }    
    
    
}
