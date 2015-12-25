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
 * @(#)AdministrationServiceFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.test.framework.container;

import java.lang.reflect.Constructor;
import java.util.Properties;

/**
 *
 * Factory for various JBI runtime administration service
 */
public class AdministrationServiceFactory {
    public static String JBI_ID_OPENESB = "OpenESB";
    
    public static AdministrationService getAdminService (String jbiID, Properties connection) throws AdministrationServiceException {
        AdministrationService adminService = null;
        try {
            if (JBI_ID_OPENESB.equalsIgnoreCase(jbiID)) {
                Class connSpecCls = Class.forName("com.sun.jbi.component.test.framework.container.openesb.OpenESBAdminServiceConnectionSpec");
                Class partypes[] = new Class[1];
                partypes[0] = Properties.class;
                Constructor ct = connSpecCls.getConstructor(partypes);
                Object arglist[] = new Object[] {connection};
                Object connSpec = ct.newInstance(arglist);
                
                Class cls = Class.forName("com.sun.jbi.component.test.framework.container.openesb.OpenESBAdministrationService");
                partypes = new Class[2];
                partypes[0] = connSpecCls;
                partypes[1] = String.class;
                ct = cls.getConstructor(partypes);
                String targetName = null;
                if (connection.containsKey("targetName")) {
                    targetName = connection.getProperty("targetName");
                }                
                arglist = new Object[] {connSpec, targetName};                        
                adminService = (AdministrationService)ct.newInstance(arglist);
            }
            
            return adminService;
        } catch (Throwable t) {
            throw new AdministrationServiceException(t);
        }
    }
    
    public static boolean supportedJBIRuntime (String jbiID) {
        if (!JBI_ID_OPENESB.equalsIgnoreCase(jbiID)) {
            return false;
        } else {
            return true;
        }
    }
}
