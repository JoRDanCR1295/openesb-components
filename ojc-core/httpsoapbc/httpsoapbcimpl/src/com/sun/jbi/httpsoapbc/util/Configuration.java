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
 * @(#)Configuration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.util;

import java.util.ArrayList;
import java.util.List;

/**
 *
 */
public class Configuration {
    public static final String TARGETNAMESPACE = "targetNamespace";
    public static final String SERVICE = "service";
    public static final String END_POINT = "endPoint";
    public static final String DIRECTION = "direction";
    public static final String WSDL_FILE = "wsdlFile";
    public static final String PORT_MAP = "portmap";
    
    // use arraylist for now
    private ArrayList services;
    
    /** Creates a new instance of Configuration */
    public Configuration() {
        services = new ArrayList();
    }
    
    public void addPortMap(PortMap p) {
        services.add(p);
    }
    
    public List portMaps() {
        return services;
    }
    
    public PortMap newPortMap(String targetNameSpace, String service, String endPoint, String direction, String wsdlFile) {
        
        PortMap pm = new PortMap(targetNameSpace, service, endPoint, direction, wsdlFile);
        return pm;
    }
    
    public static class PortMap {
        private String mService;
        private String mEndPoint;
        private String mDirection;
        private String mWsdlFile;
        private String mTargetNameSpace;
        
        protected PortMap(String targetNameSpace, String service, String endPoint, String direction, String wsdlFile) {
            mTargetNameSpace = targetNameSpace;
            mService = service;
            mEndPoint = endPoint;
            mDirection = direction;
            mWsdlFile = wsdlFile;
        }
        
        public String getTargetNameSpace() {
            return mTargetNameSpace;
        }
        
        public String getService() {
            return mService;
        }
        
        public String getEndPoint() {
            return mEndPoint;
        }
        
        public String getDirection() {
            return mDirection;
        }
        
        public String getWsdlFile() {
            return mWsdlFile;
        }
    }    
           
}
