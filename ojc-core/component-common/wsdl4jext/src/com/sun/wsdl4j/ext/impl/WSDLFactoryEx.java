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
 * @(#)WSDLFactoryEx.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext.impl;

import javax.wsdl.Definition;
import javax.wsdl.extensions.ExtensionRegistry;

import com.ibm.wsdl.factory.WSDLFactoryImpl;

/**
 * The class extends <code>com.ibm.wsdl.factory.WSDLFactoryImpl</code>
 * with facility to return a <code>WSDLReaderEx</code> instance when method
 * <code>newWSDLReaderEx</code> is called.  <code>WSDLReaderEx</code> instance
 * is entity resolver aware. 
 * 
 * @author Jun Xu
 * @version $Revision: 1.4 $
 */
public class WSDLFactoryEx extends WSDLFactoryImpl {
    
    /**
     * Instantiates a <code>WSDLReaderEx</code> instance.
     * 
     * @return a <code>WSDLReaderEx</code> instance
     */
    public WSDLReaderEx newWSDLReaderEx() {
        WSDLReaderEx reader = new WSDLReaderEx();
        reader.setExtensionRegistry(newPopulatedExtensionRegistry());
        reader.setFactoryImplName(this.getClass().getName());
        reader.setFactory(this);
        return reader;
    }

    @Override
    public ExtensionRegistry newPopulatedExtensionRegistry() {
        return new PopulatedExtensionRegistryEx();
    }

    @Override
    public Definition newDefinition() {
        DefinitionEx wsdlDef = new DefinitionEx();
        wsdlDef.setExtensionRegistry(newPopulatedExtensionRegistry());
        return wsdlDef;
    }
}
