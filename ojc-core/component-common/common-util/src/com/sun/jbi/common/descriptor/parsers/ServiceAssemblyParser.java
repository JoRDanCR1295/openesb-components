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
 * @(#)DescriptorHandler.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor.parsers;

import java.util.ArrayList;
import java.util.List;

import javax.jbi.management.DeploymentException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.descriptor.ServiceAssembly;
import com.sun.jbi.common.descriptor.model.AssemblyUnit;
import com.sun.jbi.common.descriptor.model.Connection;
import com.sun.jbi.common.descriptor.model.Identification;
import com.sun.jbi.common.descriptor.model.Target;
import com.sun.jbi.common.util.I18n;

/**
 * Parses JBI connections from a service assembly descriptor.
 * @author Kevan Simpson
 */
public class ServiceAssemblyParser extends AbstractJbiParser<ServiceAssembly> {
	private List<AssemblyUnit> mUnits = new ArrayList<AssemblyUnit>();
	private Identification mId;
	
	/**
	 * Default <code>ServiceAssemblyParser</code> constructor. 
	 */
	public ServiceAssemblyParser() {
	}

    /**
     * Constructs a <code>ServiceAssemblyParser</code> using the specified 
     * parser's {@link XPath} resouce and {@link JbiNamespaceContext}.
     * @param jp A <code>JbiParser</code> which must not be <code>null</code>.
     */
	public ServiceAssemblyParser(JbiParser jp) {
		super(jp);
	}
	
    /** @see com.sun.jbi.common.descriptor.parsers.JbiParser#parse(org.w3c.dom.Element) */
	public ServiceAssembly parse(Element elem) throws DeploymentException {
		try {
			if (elem == null) {
				return null;
			}

			Element sa = (Element) getXPath().evaluate(
					build("./", null, JbiDescriptor.SERVICE_ASSEMBLY_ELEM), 
					elem, XPathConstants.NODE);
			mId = parseIdentification(sa);
			parseAssemblyUnits(elem);
			
            ConnectionParser connParser = new ConnectionParser(this);
            Connection[] conn = connParser.parse(elem);

		    return new ServiceAssembly(getIdentification(),
                                       getServiceUnits(),
                                       conn);
		}
		catch (DeploymentException de) {
			throw de;
		}
		catch (Exception e) {
		    throw error(e,
		    			I18n.loc("UTIL-6001: Failed to parse service assembly descriptor: {0}",
		    				     e.getMessage()));
		}
	}

	/**
	 * Parses an {@link Identification} from the specified element, which is
	 * expected to be the parent of an <code>Identification</code> element.
	 * 
	 * @param elem The parent of an <code>Identification</code> element.
	 * @return an <code>Identification</code>.
	 * @throws DeploymentException if an error occurs parsing the specified element. 
	 */
    public Identification parseIdentification(Element elem) throws DeploymentException {
    	try {
	    	// expects the 'id' element parent
	    	String idExprBase = build("./", null, JbiDescriptor.IDENTIFICATION_ELEM, "/", null);
	    	String name = (String) getXPath().evaluate(
	    			build(idExprBase, JbiDescriptor.NAME_ELEM, "/text()"), 
	    			elem, XPathConstants.STRING);
	    	String desc = (String) getXPath().evaluate(
	    			build(idExprBase, JbiDescriptor.DESCRIPTION_ELEM, "/text()"), 
	    			elem, XPathConstants.STRING);
	    	
	    	return new Identification(name, desc);
    	}
    	catch (Exception e) {
		    throw error(e, I18n.loc(
		    		"UTIL-6005: Failed to parse Identification element: {0}",
	    			e.getMessage()));
    	}
    }

    /**
     * Fetches the parsed {@link Identification}.
     * @return the parsed <code>Identification</code>.
     */
	public Identification getIdentification() {
        return mId;
    }
    
	/**
	 * Fetches the assembly's service units.
	 * @return the assembly's service units.
	 */
    public AssemblyUnit[] getServiceUnits() {
        AssemblyUnit[] units = new AssemblyUnit[mUnits.size()];
        mUnits.toArray(units);
        return units;
    }
  
    /**
     * Parses all service unit descendants from an element. 
     * @param elem An element containing service unit descendants. 
     * @throws Exception if an error occurs parsing.
     */
    protected void parseAssemblyUnits(Element elem) throws Exception {
    	NodeList units = (NodeList) getXPath().evaluate(
    			build("//", null, JbiDescriptor.SERVICE_UNIT_ELEM),
    			elem, XPathConstants.NODESET);
    	for (int i = 0, n = units.getLength(); i < n; i++) {
    		Element su = (Element) units.item(i);
    		Identification id = parseIdentification(su);
    		Target trgt = parseTarget(su);
    		mUnits.add(new AssemblyUnit(trgt, id));
    	}
    }
    
    /**
     * Parses a {@link Target} from a 
     * {@link JbiDescriptor#SERVICE_UNIT_ELEM service unit element}.
     * @param elem A service unit element.
     * @return A <code>Target</code>.
     * @throws Exception if an error occurs parsing.
     */
    protected Target parseTarget(Element elem) throws Exception {
    	String exprBase = build("./", null, JbiDescriptor.TARGET_ELEM, "/", null);
    	String zip = (String) getXPath().evaluate(
    			build(exprBase, JbiDescriptor.ARTIFACT_ELEM, "/text()"), 
    			elem, XPathConstants.STRING);
    	String comp = (String) getXPath().evaluate(
    			build(exprBase, JbiDescriptor.COMPONENT_NAME_ELEM, "/text()"), 
    			elem, XPathConstants.STRING);
    	
    	return new Target(zip, comp);
    }
}
