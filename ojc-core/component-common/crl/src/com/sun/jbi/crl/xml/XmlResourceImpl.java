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
 * @(#)XmlResourceImpl.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.xml;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;

/**
 * 
 * @author Kevan Simpson
 */
public class XmlResourceImpl implements XmlResource {
	private static TransformerFactory mFactory = 
			XmlUtil.newTransformerFactory();//TransformerFactory.newInstance();
	
    private DocumentBuilder mDocumentBuilder = null;
    private Transformer mTransformer = null;
    
    public XmlResourceImpl() {
    	try {
	        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
	        factory.setNamespaceAware(true);
	        mDocumentBuilder = factory.newDocumentBuilder();
	        mTransformer = mFactory.newTransformer();
    	}
    	catch (ParserConfigurationException pce) {
    		// TODO new doc bldr
    		throw new RuntimeException(pce);
    	}
    	catch (TransformerConfigurationException tce) {
    		// TODO new trans
    		throw new RuntimeException(tce);
    	}
    }
    
	/** @see com.sun.jbi.crl.xml.XmlResource#getDocumentBuilder() */
	public DocumentBuilder getDocumentBuilder() {
		return mDocumentBuilder;
	}

	/** @see com.sun.jbi.crl.xml.XmlResource#getTransformer() */
	public Transformer getTransformer() {
		return mTransformer;
	}
}
