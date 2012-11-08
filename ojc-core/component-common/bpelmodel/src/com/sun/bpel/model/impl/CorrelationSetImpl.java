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
 * @(#)CorrelationSetImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;
import javax.xml.namespace.QName;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.CorrelationSet;
import com.sun.bpel.model.CorrelationSet.ATTR;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.visitor.Visitor;
import com.sun.wsdl4j.ext.bpel.MessageProperty;


/**
 * Implements the &lt;correlationSet&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class CorrelationSetImpl extends BPELElementImpl
    implements CorrelationSet {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -6876375517512539389L;
    
    /** Creates a new instance of CorrelationSetImpl */
    public CorrelationSetImpl() {
        super();
        initCorrelationSet();
    }
    
    /** Constructs new instance of correlationSet element.
     * @param   d   Owner document.
     */
    public CorrelationSetImpl(XMLDocument d) {
        super(d);
        initCorrelationSet();
    }
    
    /** Initializes this class.
     */
    private void initCorrelationSet() {
        setLocalName(CorrelationSet.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.NAME, String.class, false, null),
            new XMLAttributeImpl(ATTR.PROPERTIES, String.class, false, null)
        };
    }
    
    /** Getter for property name.
     * @return Value of property name.
     *
     */
    public String getName() {
        return xmlAttrs[NAME].getValue();
    }
    
    /** Setter for property name.
     * @param name New value of property name.
     *
     */
    public void setName(String name) {
        setAttribute(NAME, name);
    }
    
    
    /** Getter for property properties.
     * @return Value of property properties.
     *
     */
    private String getPropertiesAsString() {
        return xmlAttrs[PROPERTIES].getValue();
    }
    
    /** Setter for property properties.
     * @param properties New value of property properties.
     *
     */
    private void setPropertiesAsString(String properties) {
        setAttribute(PROPERTIES, properties);
    }
   
    
    
    public Collection getBPELProperties() {
    	ArrayList properties = new ArrayList();
    	
		QName[] propertiesQNames = getProperties();
		if(propertiesQNames != null) {
			BPELDocument bpelDoc = (BPELDocument) this.getOwnerDocument();
			for(int i = 0; i < propertiesQNames.length; i++) {
				QName propertyQName = (QName) propertiesQNames[i];
				MessageProperty property =
				    bpelDoc.getDocumentProcess().getBPELProperty(propertyQName);
				if(property != null) {
					properties.add(property);
				}
			}
		}
		return properties;
	}

	public QName[] getProperties() {
		String lProperties = getPropertiesAsString();
        String lDelimiter = " ";
        StringTokenizer tokenizer = new StringTokenizer(lProperties, lDelimiter);
        QName propertiesQName[] = new QName[tokenizer.countTokens()];
        int i = 0;
        while(tokenizer.hasMoreElements()) {
                String lNextToken = (String) tokenizer.nextElement();
                propertiesQName[i] = com.sun.bpel.xml.NamespaceUtility.resolveAndGetQName(
                		lNextToken, this);
                i++;
        }
		return propertiesQName;
	}

	public void setBPELProperties(Collection properties) {
		if(properties != null) {
			Iterator it = properties.iterator();
			List<QName> propNames = new ArrayList<QName>();
			while(it.hasNext()) {
				MessageProperty property = (MessageProperty) it.next();
				propNames.add(property.getName());
			}
			setProperties(propNames.toArray(new QName[0]));
		}
	}

	public void setProperties(QName[] properties) {
		StringBuffer propBuf = new StringBuffer(50);
		if(properties != null) {
			for(int i =0; i < properties.length; i++) {
				propBuf.append(properties[i].toString());
				
				if(i < properties.length) {
					propBuf.append(" ");
				}
			}
			
			setPropertiesAsString(propBuf.toString());
		}
	}

	/** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean accept(Visitor w) {
        BPELVisitor v = morphVisitor(w);
        return v.visit(this);
    }
}
