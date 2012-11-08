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
 * @(#)WSDLFactoryImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.ibm.wsdl.factory;

import javax.wsdl.*;
import javax.wsdl.extensions.*;
import javax.wsdl.factory.*;
import javax.wsdl.xml.*;
import org.xml.sax.EntityResolver;
import com.ibm.wsdl.*;
import com.ibm.wsdl.extensions.*;
import com.ibm.wsdl.xml.*;

/**
 * This class is a concrete implementation of the abstract class
 * WSDLFactory. Some ideas used here have been shamelessly
 * copied from the wonderful JAXP and Xerces work.
 *
 * @author Matthew J. Duftler (duftler@us.ibm.com)
 */
public class WSDLFactoryImpl extends WSDLFactory
{
  /**
   * Create a new instance of a Definition, with an instance
   * of a PopulatedExtensionRegistry as its ExtensionRegistry.
   *
   * @see com.ibm.wsdl.extensions.PopulatedExtensionRegistry
   */
  public Definition newDefinition()
  {
    Definition def = new DefinitionImpl();
    ExtensionRegistry extReg = newPopulatedExtensionRegistry();

    def.setExtensionRegistry(extReg);

    return def;
  }

  /**
   * Create a new instance of a WSDLReader.
   */
  public WSDLReader newWSDLReader()
  {
    return new WSDLReaderImpl();
  }

  /** 
   * Creates a new instance of WSDLReader, setting the resolver to be used
   * temp solution so that WSDL4J package does not depend on apache resolver 
   * project.
   */
  public WSDLReader newWSDLReader(EntityResolver resolver) {
    WSDLReaderImpl reader = new WSDLReaderImpl();
    reader.setEntityResolver(resolver);
    return reader;
  }
  /**
   * Create a new instance of a WSDLWriter.
   */
  public WSDLWriter newWSDLWriter()
  {
    return new WSDLWriterImpl();
  }

  /**
   * Create a new instance of an ExtensionRegistry with pre-registered
   * serializers/deserializers for the SOAP, HTTP and MIME
   * extensions. Java extensionTypes are also mapped for all
   * the SOAP, HTTP and MIME extensions.
   */
  public ExtensionRegistry newPopulatedExtensionRegistry()
  {
    return new PopulatedExtensionRegistry();
  }
}
