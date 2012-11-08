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
 * @(#)QosServicesDescriptor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.descriptor;

import java.io.File;

import javax.jbi.management.DeploymentException;

import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.descriptor.model.Services;
import com.sun.jbi.common.descriptor.parsers.sax.StackableHandler;
import com.sun.jbi.common.qos.descriptor.parsers.sax.AppConfigHandler;
import com.sun.jbi.common.util.I18n;

/**
 * Utility to parse and model a service unit descriptor,
 * along with <code>ApplicationConfiguration</code>.
 * 
 * @author Kevan Simpson
 */
public class QosServicesDescriptor extends JbiDescriptor {
    public static final String APP_CONFIG_NS = "http://www.sun.com/jbi/descriptor/configuration";
    
    public static final String APPLICATION_CONFIGURATION_ELEM = "application-config";
    
    /**
     * Parses service endpoints and application configurations.
     * 
     * @param serviceUnitRootPath The root path of a deployed service unit.
     * @return Assembly and QoS configuration.
     * @throws DeploymentException If an error occurs while parsing.
     */
    public static QosServices parse(String serviceUnitRootPath) throws DeploymentException {
        File jbiDescriptorFile = 
            new File(serviceUnitRootPath + File.separator + META_INF_DIR, 
                     JBI_DESC_FILE_NAME);
    
        if (jbiDescriptorFile == null || !jbiDescriptorFile.exists()) {
            throw error(I18n.loc("QOS-6004: Descriptor file does not exist: {0}",
            		             jbiDescriptorFile.getAbsolutePath()), 
            		    null);
        }

        AppConfigHandler ac = new AppConfigHandler();
        StackableHandler<Services> stack = 
        		new StackableHandler<Services>(ac);
        Services srvcs = stack.parse(jbiDescriptorFile);

        return new QosServices(srvcs.getProvides(), 
                               srvcs.getConsumes(), 
                               ac.getApplicationConfigurations());
    }
    
}
