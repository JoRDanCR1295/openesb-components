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
 * @(#)JbiDescriptor.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.management.DeploymentException;
import javax.xml.XMLConstants;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import com.sun.jbi.common.xml.XmlUtil;

/**
 * Base class, with useful constants, for JBI descriptor representations.
 * 
 * @author Kevan Simpson
 */
public abstract class JbiDescriptor {
	/** The JBI namespace uri. */
	public static final String JBI_NS = "http://java.sun.com/xml/ns/jbi";
	/** The WSDL 1.1 message wrapper namespace uri. */
	public static final String JBI_WSDL11_NS = "http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper";
	
	/** The directory in which JBI descriptors reside. */
    public static final String META_INF_DIR = "META-INF";
    /** The JBI descriptor file name.*/
    public static final String JBI_DESC_FILE_NAME	= "jbi.xml";
    
    /** JBI element name. */
    public static final String JBI_ELEM				= "jbi";
    /** JBI component element name. */
    public static final String COMPONENT_ELEM 		= "component";
    
    // service unit
    public static final String SERVICES_ELEM    = "services";
	public static final String PROVIDES_ELEM 	= "provides";
	public static final String CONSUMES_ELEM	= "consumes";
	public static final String INTERFACE_ATTR	= "interface-name";
	public static final String SERVICE_ATTR 	= "service-name";
	public static final String ENDPOINT_ATTR 	= "endpoint-name";
	public static final String LINK_TYPE_ATTR 	= "link-type";
	
	// service assembly
    public static final String SERVICE_ASSEMBLY_ELEM = "service-assembly";
	public static final String CONNECTION_LIST_ELEM  = "connections";
	public static final String CONNECTION_ELEM       = "connection";
	public static final String CONSUMER_ELEM	     = "consumer";
	public static final String PROVIDER_ELEM 	     = "provider";
	
	// identification
	public static final String IDENTIFICATION_ELEM	= "identification";
	public static final String NAME_ELEM			= "name";
	public static final String DESCRIPTION_ELEM		= "description";
    
    // assembly unit (i.e. service unit in an assembly descriptor)
    public static final String SERVICE_UNIT_ELEM    = "service-unit";
    public static final String TARGET_ELEM          = "target";
    public static final String ARTIFACT_ELEM        = "artifacts-zip";
    public static final String COMPONENT_NAME_ELEM  = "component-name";
    
    /*
     * private Logger and protected error-throwing utility method
     */
    private static Logger mLogger = 
    		Logger.getLogger(JbiDescriptor.class.getName());

    public static Document createJbiDescriptorRoot() {
        // build descriptor element
        Document doc = XmlUtil.newDocument();
        Element jbi = doc.createElementNS(
                JbiDescriptor.JBI_NS, JbiDescriptor.JBI_ELEM);
        doc.appendChild(jbi);
        // append default namespaces
        Attr schema = doc.createAttributeNS(
                XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI, "schemaLocation");
        schema.setPrefix("xsi");
        schema.setTextContent("http://java.sun.com/xml/ns/jbi jbi.xsd");
        jbi.setAttributeNodeNS(schema);
        jbi.setAttribute("version", "1.0");

        return doc;
    }
    
    /**
     * Utitity method for JBI descriptors to log and throw {@link DeploymentException}s.
     * @param message The error message.
     * @param thrown The error's cause or <code>null</code>.
     * @return A logged <code>DeploymentException</code>.
     */
    protected static DeploymentException error(String message, Exception thrown) {
        if (thrown == null) {
            mLogger.warning(message);
            return new DeploymentException(message);
        }
        else {
            mLogger.log(Level.WARNING, message, thrown);
            return new DeploymentException(message, thrown);
        }
    }
}
