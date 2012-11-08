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
 * @(#)PartImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;

import org.exolab.castor.xml.schema.ElementDecl;
import org.exolab.castor.xml.schema.Schema;
import org.exolab.castor.xml.schema.XMLType;

import com.sun.wsdl.model.NamespaceUtility;
import com.sun.wsdl.model.Part;
import com.sun.wsdl.model.WSDLHelper;
import javax.xml.namespace.QName;
import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLAttributeEvent;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLElementAdapter;
import com.sun.wsdl.model.common.model.impl.XMLAttributeImpl;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.visitor.WSDLVisitor;

/**
 * Implements the &lt;part&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class PartImpl extends NamedWSDLElementImpl implements Part {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -6590729964509647635L;
    
    private ElementDecl mElement;
    
    private XMLType mType;
    
    /** Creates a new instance of PartImpl */
    public PartImpl() {
        super();
        initPart();
    }
    
    /** Constructor for new part instance.
     * @param   d   Owner document.
     */
    public PartImpl(XMLDocument d) {
        super(d);
        initPart();
    }
    
    /** Initializes this class.
     */
    private void initPart() {
        setLocalName(Part.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(Part.ATTR.NAME, String.class, false, null),
            new XMLAttributeImpl(Part.ATTR.TYPE, String.class, true, null),
            new XMLAttributeImpl(Part.ATTR.ELEMENT, String.class, true, null),
        };
        
        this.addXMLElementListener(new PartElementAdapter());
    }
    
    
    /** @see com.sun.wsdl.model.Part#getType()
     */
    public QName getType() {
    	String typeQNameStr = xmlAttrs[TYPE].getValue();
    	if(typeQNameStr != null) {
    		return NamespaceUtility.resolveAndGetQName(typeQNameStr, this);
    	}
    	
    	return null;
    }
    
    /** @see com.sun.wsdl.model.Part#setType(java.lang.String)
     */
    public void setType(QName type) {
    	if(type != null) {
    		/*setAttribute(TYPE, type.toString());*/
    		setAttribute(TYPE, 
    				NamespaceUtility.getQNameAsString(type));
    		
    	} else {
    		setAttribute(TYPE, null);
    	}
    	
    }
    
    /** @see com.sun.wsdl.model.Part#getElement()
     */
    public QName getElement() {
    	String elementQNameStr = xmlAttrs[ELEMENT].getValue();
    	if(elementQNameStr != null) {
            return NamespaceUtility.resolveAndGetQName(elementQNameStr, this);
    	}
    	return null;
    }
    
    /** @see com.sun.wsdl.model.Part#setElement(java.lang.String)
     */
    public void setElement(QName element) {
    	if(element != null) {
    		/*setAttribute(ELEMENT, element.toString());*/
    		setAttribute(ELEMENT, 
    				NamespaceUtility.getQNameAsString(element));
    		
    	} else {
    		setAttribute(ELEMENT, null);
    	}
    }
  
    
    public ElementDecl getXSDElement() {
    	if(this.mElement != null) {
			return this.mElement;
		}
		
		if (getElement() == null) {
			return null;
		}
		
		this.mElement = WSDLHelper.getMatchingXSDElement(getElement(), this);
		return this.mElement;
	}

	public XMLType getXSDType() {
		if(this.mType != null) {
			return this.mType;
		}
		
		if (getType() == null) {
			return null;
		}
		
		this.mType = WSDLHelper.getMatchingXSDType(getType(), this);
		return this.mType;
	}

	public void setXSDElement(ElementDecl element) {
//		update the element attribute value
		if(element != null) {
			Schema xsdDocument = (Schema) element.getSchema();
			if(xsdDocument == null) {
				throw new IllegalArgumentException("can not set type, type "+ element.getName() + " does not have owner Schema");
			}
			
			String targetNamespace = xsdDocument.getTargetNamespace();
			if(targetNamespace != null) {
				String prefix = getNamespacePrefix(targetNamespace);
				if(prefix != null) {
					setType(NamespaceUtility.getQName(targetNamespace, 
									element.getName(), prefix));
				} else {
					setType(NamespaceUtility.getQName(targetNamespace, element.getName()));
				}
			} else {
				throw new IllegalArgumentException("can not set type, type "+ element.getName() + " 's owner Schema does not have targetNamespace attribute defined");
			}
			
		}
		
//		this.mElement = element;
		
	}

	public void setXSDType(XMLType type) {
//		update the type attribute value
		if(type != null) {
			Schema xsdDocument = (Schema) type.getSchema();
			if(xsdDocument == null) {
				throw new IllegalArgumentException("can not set type, type "+ type.getName() + " does not have owner Schema");
			}
			
			String targetNamespace = xsdDocument.getTargetNamespace();
			if(targetNamespace != null) {
				String prefix = getNamespacePrefix(targetNamespace);
				if(prefix != null) {
					setType(NamespaceUtility.getQName(targetNamespace, 
								type.getName(), prefix));
				} else {
					setType(NamespaceUtility.getQName(targetNamespace, type.getName()));
				}
			} else {
				throw new IllegalArgumentException("can not set type, type "+ type.getName() + " 's owner Schema does not have targetNamespace attribute defined");
			}
			
		}
		
//		this.mType = type;
		
	}

	/** @see com.sun.wsdl.model.common.model.XMLNode#accept(
     *  com.sun.wsdl.model.common.model.visitor.Visitor)
     */
    public boolean accept(Visitor w) {
    	WSDLVisitor v = morphVisitor(w);
        if (traverseParentFirst(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }
        
        if (!super.accept(v)) {
            return false;
        }

        if (traverseParentLast(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }
        return true;
    }
    
    
    class PartElementAdapter extends XMLElementAdapter {
    	public void attributeModified(XMLAttributeEvent evt) {
    		QName attrQName = evt.getAttributeName();
    		
    		if(attrQName != null) {
    			//String localName = attrQName.getLocalName();
    			String localName = attrQName.getLocalPart();
    			//if Type is changed we need to clear out cached XMLType object
    	        //so that next call to getXSDType can find new XMLType object
    	        if(localName.equals(Part.ATTR.TYPE)) {
    				mType = null;
    			} else if(localName.equals(Part.ATTR.ELEMENT)) {
    				mElement = null;
    			}
    		}
    	}
    }
}
