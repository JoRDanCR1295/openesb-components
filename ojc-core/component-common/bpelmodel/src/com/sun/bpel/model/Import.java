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
 * @(#)Import.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

/**
 * @author Sun Microsystems
 *
 * import element of a bpel document.
 */
public interface Import extends BPELElement {
	
	
	public static final String WSDL_IMPORT_TYPE = "http://schemas.xmlsoap.org/wsdl/";
	
	public static final String XSD_IMPORT_TYPE = "http://www.w3.org/2001/XMLSchema";
	
	/** Tag for this element */
    public static final String TAG = "import";
    
    /** Describes the attributes for this element */
    public interface ATTR {
    	/** "namespace" attribute token */
    	public static final String NAMESPACE = "namespace";
    	/** "location" attribute token */
        public static final String LOCATION = "location";
        /** "importType" attribute token */
        public static final String IMPORT_TYPE = "importType";
        
    }
    
    /** Ordinal position of namespace attribute. */
    public static final int NAMESPACE = 0;
    
    /** Ordinal position of location attribute. */
    public static final int LOCATION = NAMESPACE + 1;
    
    /** Ordinal position of importType attribute. */
    public static final int IMPORT_TYPE = LOCATION + 1;
    
    /**
     * set the namespace attribute of this import.
     * @param targetNamespace
     */
    public void setNamespace(String targetNamespace);
    
    /**
     * set namespace attribute of this import.
     * @return
     */
    public String getNamespace();
    
    public void setLocation(String newLocation);
    
    public String getLocation();
    
    public void setImportType(String newImportType);
    
    /**
     * import type can be either xsd (@see XSD_IMPORT_TYPE)
     * or wsdl (@see WSDL_IMPORT_TYPE)
     * @return
     */
    public String getImportType();
    
    /**
     * set the top level object which this import represents.
     * this will be either WSDLDocument or XMLSchema
     * @param document top level document
     */
    void setImportedObject(Object document);

    /**
     * get the top level object which this import represents.
     * this will be either WSDLDocument or XMLSchema
     * @return WSDLDocument or XMLSchema
     */
    Object getImportedObject();
    
}
