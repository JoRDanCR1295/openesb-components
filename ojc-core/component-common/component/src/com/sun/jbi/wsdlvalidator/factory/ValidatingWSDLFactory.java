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
 * @(#)ValidatingWSDLFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.wsdlvalidator.factory;

import java.io.File;

import javax.wsdl.Definition;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.wsdl.xml.WSDLWriter;

import org.apache.xml.resolver.tools.CatalogResolver;
import org.apache.xml.resolver.CatalogManager;
import org.xml.sax.EntityResolver;

import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.jbi.wsdlvalidator.impl.ValidatingWSDLReaderImpl;

/**
 * ValidatingWSDLFactory is an implementation of the WSDLFactory interface
 * that provides ValidatingWSDLReader objects.  It adds an additional
 * method to retrieve a ValidatingWSDLReader using Apache's CatalogManager
 * and CatalogResolver classes.
 */
public class ValidatingWSDLFactory extends WSDLFactory {

    private WSDLFactory mProxy;

    public ValidatingWSDLFactory() {
        mProxy = new WSDLFactoryImpl();
    }

    /**
     * Create a new instance of a Definition.
     */
    public Definition newDefinition() {
        return mProxy.newDefinition();
    }
    
    /**
     * Create a new instance of a WSDLReader.
     */
    public WSDLReader newWSDLReader() {
        return new ValidatingWSDLReaderImpl();
    }

    /**
     * Create a new WSDLReader using the xml-catalog.xml
     * located at the directory, dir.  The WSDLReader object
     * is actually an instance of ValidatingWSDLReader.
     *
     * @param  dir the directory 
     * @return a WSDLReader object
     */
    public WSDLReader newWSDLReader(File dir) {
        File catalog = new File(dir.getAbsolutePath() +
            File.separator + "META-INF" + File.separator +
            "catalog.xml");
        if (catalog.exists()) {
            CatalogManager catalogManager = new CatalogManager();
            catalogManager.setCatalogFiles(catalog.getAbsolutePath());
            catalogManager.setRelativeCatalogs(true);
            catalogManager.setUseStaticCatalog(false);
            EntityResolver resolver = new CatalogResolver(catalogManager);
            return newWSDLReader(resolver);
        }
        return newWSDLReader();
    }

    /**
     * Create a new instance of a WSDLReader.
     */
    public WSDLReader newWSDLReader(EntityResolver resolver) {
        return new ValidatingWSDLReaderImpl(resolver);
    }

    /**
     * Create a new instance of a WSDLWriter.
     */
    public WSDLWriter newWSDLWriter() {
        return mProxy.newWSDLWriter();
    }
    
    /**
     * Create a new instance of an ExtensionRegistry with pre-registered
     * serializers/deserializers for the SOAP, HTTP and MIME
     * extensions. Java extensionTypes are also mapped for all
     * the SOAP, HTTP and MIME extensions.
     */
    public ExtensionRegistry newPopulatedExtensionRegistry() {
        return mProxy.newPopulatedExtensionRegistry();
    }
}
