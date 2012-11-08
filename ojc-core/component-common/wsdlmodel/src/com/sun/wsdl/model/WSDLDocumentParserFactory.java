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
 * @(#)WSDLDocumentParserFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import java.io.Reader;

import org.xml.sax.ErrorHandler;

import com.sun.wsdl.model.common.model.EInsightModelException;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public abstract class WSDLDocumentParserFactory {
	   
    /** Holds factory singleton. */
    private static WSDLDocumentParserFactory factory = null;
    
    /**
     * Gets the WSDL document factory singleton.
     * @return  a WSDL document factory.
     * @throws  EInsightModelException  When implementing factory class not found.
     */
    public static synchronized WSDLDocumentParserFactory getInstance() throws EInsightModelException {
        if (null == factory) {
            String wsdlFac = System.getProperty("com.sun.wsdl.model.WSDLDocumentParserFactory",
                                                "com.sun.wsdl.model.impl.WSDLDocumentParserFactoryImpl");
            try {
                factory = (WSDLDocumentParserFactory) Class.forName(wsdlFac).newInstance();
            } catch (Exception e) {
                throw new EInsightModelException(
                    "WSDLDocumentParserFactory.getInstance(): Cannot find class: " + wsdlFac, e);
            }
        }
        return factory;
    }    
    
    /**
     *
     * Creates a new WSDL document.
     * @return  New WSDL XML document.
     */
    public abstract WSDLDocument load(Reader reader, String baseURI);
    
    /**
    *
    * Creates a new WSDL document.
    * @return  New WSDL XML document.
    */
   public abstract void load(WSDLDocument doc,
   							 Reader reader,
							 WSDLParseContext context);
    
   
   /**
   *
   * parse specified element again. reader should have xml which starts with the element.
   * @return  New WSDL XML document.
   */
  public abstract void load(WSDLElement element,
  							 Reader reader,
							 WSDLParseContext context);

}
