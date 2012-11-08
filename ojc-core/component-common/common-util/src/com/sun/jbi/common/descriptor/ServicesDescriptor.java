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
 * @(#)SUDescriptor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor;

import java.io.File;

import javax.jbi.management.DeploymentException;

import com.sun.jbi.common.descriptor.model.Services;
import com.sun.jbi.common.descriptor.parsers.sax.ServicesHandler;
import com.sun.jbi.common.descriptor.parsers.sax.StackableHandler;
import com.sun.jbi.common.util.I18n;

/**
 * Utility to parse and model a service unit descriptor.
 * 
 * @author Kevan Simpson
 */
public class ServicesDescriptor extends JbiDescriptor {
    /**
     * Parses a service unit descriptor and returns a list of endpoint definitions.
     * 
     * @param file The descriptor file to parse.
     * @return a list of endpoint definitions.
     * @throws DeploymentException if an error occurs parsing.
     */
    public static Services parse(File file) throws DeploymentException {
        try {
            StackableHandler<Services> stack = 
                    new StackableHandler<Services>(new ServicesHandler());
            return stack.parse(file);
        }
        catch (Exception e) {
            throw error(I18n.loc("UTIL-6002: Failed to parse service unit descriptor: {0}",
                                 e.getMessage()), 
                        e);
        }
    }

    /**
     * Parses a service unit descriptor with the given name and path.
     * 
     * @param serviceUnitName The name of the service unit.
     * @param serviceUnitRootPath The path where the service unit is deployed.
     * @return a service unit.
     * @throws DeploymentException if an error occurs parsing the descriptor.
     */
    public static ServiceUnit parse(String serviceUnitName, String serviceUnitRootPath) 
            throws DeploymentException {
        File jbiDescriptorFile = 
                new File(serviceUnitRootPath + File.separator + META_INF_DIR, 
                         JBI_DESC_FILE_NAME);
        
        if (jbiDescriptorFile == null || !jbiDescriptorFile.exists()) {
            throw error(I18n.loc("UTIL-6004: Descriptor file does not exist: {0}",
                                 jbiDescriptorFile.getAbsolutePath()), 
                        null);
        }
        
        return new ServiceUnit(serviceUnitName, serviceUnitRootPath,
                               parse(jbiDescriptorFile));
    }
}
