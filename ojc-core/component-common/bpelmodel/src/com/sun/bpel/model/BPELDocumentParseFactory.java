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
 * @(#)BPELDocumentParseFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import java.io.Reader;

import com.sun.bpel.model.common.EInsightModelException;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public abstract class BPELDocumentParseFactory {
	   
    /** Holds factory singleton. */
    private static BPELDocumentParseFactory factory = null;
        
    /**
     * Gets the BPEL document parse factory singleton.
     * @return  a BPEL document parse factory.
     * @throws  EInsightModelException  When implementing factory cannot be found.
     */
    public static synchronized BPELDocumentParseFactory getInstance() throws EInsightModelException {
        if (null == factory) {
            String bpelFac = System.getProperty("com.sun.bpel.model.BPELDocumentParseFactory",
                                                "com.sun.bpel.model.impl.BPELDocumentParseFactoryImpl");
            try {
                factory = (BPELDocumentParseFactory) Class.forName(bpelFac).newInstance();
            } catch (Exception e) {
                throw new EInsightModelException(
                    "BPELDocumentParseFactory.getInstance(): Cannot find class: " + bpelFac, e);
            }
        }
        return factory;
    }
    
    
    /**
    *
    * load a new BPEL4WS document.
    * @return  a new BPEL4WS document.
    */
   public abstract BPELDocument load(Reader reader, BPELParseContext context);
   
   /**
    * parse bpel document from given reader and repopulate BPELDocument.
    * @param reader Reader
    * @param context BPELParseContext
    * @param document BPELDocument
    * @throws EInsightModelException if error occured
    */
   public abstract void load(Reader reader, BPELParseContext context, BPELDocument document);
}
