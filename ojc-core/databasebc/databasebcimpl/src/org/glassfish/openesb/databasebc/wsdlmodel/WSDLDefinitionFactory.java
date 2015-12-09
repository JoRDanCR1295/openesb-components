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
 * @(#)WSDLDefinitionFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc.wsdlmodel;

import com.sun.jbi.internationalization.Messages;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.wsdl.Definition;


/**
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public abstract class WSDLDefinitionFactory {
    
    private static WSDLDefinitionFactory mInstance;
    
    private static final Logger mLogger = Messages.getLogger(WSDLDefinitionFactory.class);
    
    private static WSDLDefinitionFactory getInstance() throws Exception {
        if (WSDLDefinitionFactory.mInstance == null) {
            final String factoryImplName = System.getProperty("org.glassfish.openesb.databasebc.wsdlmodel.WSDLDefinitionFactory",
                    "org.glassfish.openesb.databasebc.wsdlmodel.impl.WSDLDefinitionFactoryImpl");

            try {
                final Class cls = Class.forName(factoryImplName);
                WSDLDefinitionFactory.mInstance = (WSDLDefinitionFactory) cls.newInstance();
            } catch (final ClassNotFoundException ex) {
                mLogger.log(Level.SEVERE,"WSDL factory could not be found!!!",ex);
                throw new Exception("failed to create concrete factory class for org.glassfish.openesb.databasebc.wsdlmodel.WSDLDefinitionFactory",
                    ex);
            }
        }

        return WSDLDefinitionFactory.mInstance;
    }

    abstract WSDLDefinition newWSDLDefinition(Definition definition);
}
