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
 * @(#)IEPConfig.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep;

import com.sun.jbi.engine.iep.core.runtime.operator.OperatorConstants;

/**
 * IEPConfig.java
 *
 * Created on May 27, 2005, 4:59 PM
 *
 * @author Bing Lu
 */
public interface IEPConfig extends OperatorConstants {
    public static final String PROP_CONFIG_FILE = "config.properties";
    public static final String PROP_UPDATED_CONFIG_FILE = "updatedConfig.properties";
    public static final String PROP_BOOTSTRAP_FILE = "bootstrap.properties";
    
    public static final String PROP_NEW_INSTALL = "NewInstall";
    
    public static final String PROP_AIE_TYPE = "AieType";
    public static final String PROP_AIE_ID = "AieId";
    public static final String PROP_AIE_HOSTNAME = "AieHostname";
    public static final String PROP_AIE_PORT = "AiePort";
    
    public static final String AIE_TYPE_NONE = "none";
    public static final String AIE_TYPE_MATLAB = "matlab";
    
    public static final String GF_IS_CLUSTERED = "com.sun.jbi.isClustered"; 
    public static final String GF_INSTANCE_NAME = "com.sun.jbi.instanceName";
    
    public static final String IEP_IS_CLUSTERED = "com.sun.jbi.iepse.isClustered"; 
    public static final String IEP_INSTANCE_NAME = "com.sun.jbi.iepse.instanceName"; 
}
