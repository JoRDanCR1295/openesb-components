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
 * @(#)FromImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.From;
import com.sun.bpel.model.Literal;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.model.wsdlmodel.impl.XMLTextImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.model.XMLText;
import com.sun.bpel.xml.common.visitor.Visitor;
import com.sun.wsdl4j.ext.bpel.MessageProperty;


/**
 * Implements the &lt;from&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class FromImpl extends BPELElementImpl implements From {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 3492731034037825454L;
    
    /** Holds expression text data for this element. */
    private XMLText expression;
    
    /** Holds literal sub-element */
    private Literal literal;
    
    private PartnerLink mPartnerLink;
    
    private Variable mVariable;
    
    //private List xpathVariablesList;
    
    /** Creates a new instance of FromImpl */
    public FromImpl() {
        super();
        initFrom();
    }
    
    /** Creates a new instance of FromImpl.
     * @param   d   Owner document.
     */
    public FromImpl(XMLDocument d) {
        super(d);
        initFrom();
    }
    
    /** Initializes this class.
     */
    private void initFrom() {
        setLocalName(From.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.VARIABLE, String.class, true, null),
            new XMLAttributeImpl(ATTR.PART, String.class, true, null),
            new XMLAttributeImpl(ATTR.PARTNERLINK, String.class, true, null),
            new XMLAttributeImpl(ATTR.ENDPOINT_REFERENCE, String.class, true,
                new String[] {"myRole", "partnerRole"}),
            new XMLAttributeImpl(ATTR.PROPERTY, String.class, true, null),
            new XMLAttributeImpl(ATTR.EXPRESSIONLANGUAGE, String.class, true,
                    null),
            new XMLAttributeImpl(ATTR.NM_PROPERTY, String.class, true,
                    null)
        };
        childrenTags = new String[] {
            XMLText.TAG
        };
    }
    
    /** Getter for container attribute.
     * @return  container attribute.
     */
    public String getVariable() {
        return xmlAttrs[VARIABLE].getValue();
    }
    
    /** Setter for container attribute.
     * @param   c   container attribute.
     */
    public void setVariable(String variable) {
    	String oldVariable = getVariable();
    	setAttribute(VARIABLE, variable);
        //if variable is changed we need to clear out cached variable object
        //so that next call to getBPELVariable can find new variable object
        if(variable == null || !variable.equals(oldVariable)) {
        	this.mVariable = null;
        }
    }
    
    /** Getter for part attribute.
     * @return  part attribute.
     */
    public String getPart() {
        return xmlAttrs[PART].getValue();
    }
    
    /** Setter for part attribute.
     * @param   p   part attribute.
     */
    public void setPart(String p) {
        setAttribute(PART, p);
    }
    
    /** Getter for partner attribute.
     * @return  partner attribute.
     */
    public String getPartnerLink() {
        return xmlAttrs[PARTNERLINK].getValue();
    }
    
    /** Setter for partner attribute.
     * @param   p   partner attribute.
     */
    public void setPartnerLink(String partner) {
        String oldPartner = getPartnerLink();
        setAttribute(PARTNERLINK, partner);
        //if partnerLink is changed we need to clear out cached partnerLink object
        //so that next call to getBPELPartnerLink can find new PartnerLink object
        if(partner == null || !partner.equals(oldPartner)) {
        	this.mPartnerLink = null;
        }
    }
    
    /** Getter for serviceReference attribute.
     * @return  serviceReference attribute.
     */
    public String getEndPointReference() {
        return xmlAttrs[ENDPOINT_REFERENCE].getValue();
    }
    
    /** Setter for serviceReference attribute.
     * @param   s   serviceReference attribute.
     */
    public void setEndPointReference(String s) {
        setAttribute(ENDPOINT_REFERENCE, s);
    }
    
    /** Getter for property attribute.
     * @return property attribute.
     */
    public String getProperty() {
        return xmlAttrs[PROPERTY].getValue();
    }
    
    /** Setter for property attribute.
     * @param   p   property attribute.
     */
    public void setProperty(String p) {
        setAttribute(PROPERTY, p);
    }
    
    /** Getter for expression attribute.
     * @return  expression attribute.
     */
    public String getExpression() {
        if(this.expression != null) {
        	return this.expression.getValue();
        }
        
        return null;
    }
    
    /** Setter for expression attribute.
     * @param   e   expression attribute.
     */
    public void setExpression(String e) {
    	XMLText exp = null;
    	
    	if(e != null) {
    		exp = new XMLTextImpl(e);
    	}
    	
    	setExpressionText(exp);
    }
    
    /**
     * Getter for expressionLanguage attribute.
     * @return expressionLanguage
     */
    /** @see com.sun.bpel.model.From#getExpressionLanguage()
     */
    public String getExpressionLanguage() {
    	return xmlAttrs[EXPRESSION_LANGUAGE].getValue();
    }
    
    /**
     * Setter for expressionLanguage attribute.
     * @param e expressionLanguage
     */
    public void setExpressionLanguage(String e) {
    	setAttribute(EXPRESSION_LANGUAGE, e);
    }
    
    /** Adds a child of the element.
     * @param   c   Child.
     */
    public void addChild(XMLNode c) {
        if (c instanceof XMLText) {
            setExpressionText((XMLText) c);
        } else if(c instanceof Literal) {
        	setLiteral((Literal) c);
        }
        else {
            super.addChild(c);
        }
    }
    
    /** Removes a child of the element
     * @param   c   Child.
     */
    public void removeChild(XMLNode c) {
        if (c instanceof XMLText) {
            setExpressionText(null);
        } else if(c instanceof Literal) {
        	setLiteral(null);
        } else {
            super.removeChild(c);
        }
    }
    
    /** Getter for literal data.
     * @return  literal data of the from element.
     */
    public Literal getLiteral() {
        return literal;
    }
    
    /** Setter for literal data.
     * @param   l   literal data.
     */
    public void setLiteral(Literal l) {
        super.replaceChild(literal, l);
        literal = l;
    }
    
    /** Setter for expression data.
     * @param   l   expression data.
     */
    public void setExpressionText(XMLText l) {
        super.replaceChild(expression, l);
        expression = l;
    }
    
    /**
     * Getter for expression text.
     * @return XMLText
     */
    public XMLText getExpressionText() {
    	return this.expression;
    }
    
    
	public PartnerLink getBPELPartnerLink() {
		if(this.mPartnerLink != null) {
			return this.mPartnerLink;
		}
		
		if (BPELHelper.isValueAbsent(getPartnerLink())) {
			return null;
		}
		 
		this.mPartnerLink = BPELHelper.getMatchingPartnerLink(getPartnerLink(), this);
		return this.mPartnerLink;
		
	}


	public void setBPELPartnerLink(PartnerLink partnerLink) {
		this.mPartnerLink = partnerLink;
		
		//update the partnerLink attribute value
		if(partnerLink != null) {
			setPartnerLink(partnerLink.getName());
		} else {
			setPartnerLink(null);
		}
		
	}

	
	
	public Variable getBPELVariable() {
    	if(this.mVariable != null) {
			return this.mVariable;
		}
		
		if (BPELHelper.isValueAbsent(getVariable())) {
			return null;
		}
		
		this.mVariable = BPELHelper.getMatchingVariable(getVariable(), this);
		return this.mVariable;
	}

	public void setBPELVariable(Variable variable) {
		//update the variable attribute value
		if(variable != null) {
			setVariable(variable.getName());
		} else {
			setVariable(null);
		}
		
		this.mVariable = variable;
		
	}
    
	/** @see com.sun.bpel.model.From#getNMProperty()
     */
    public String getNMProperty() {
        return xmlAttrs[NM_PROPERTY].getValue();
    }

    /** @see com.sun.bpel.model.From#setNMProperty(String )
     */
    public void setNMProperty(String  nmProp) {
        setAttribute(NM_PROPERTY, nmProp);
    }

    public MessageProperty getBPELProperty() {
		// TODO Auto-generated method stub
		return null;
	}

	public void setBPELProperty(MessageProperty p) {
		// TODO Auto-generated method stub
		
	}
    
	/** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean accept(Visitor w) {
        BPELVisitor v = morphVisitor(w);
        if (hasChildren()) {
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
        } else {
            if (!v.visit(this)) {
                return false;
            }
        }
        return true;
    }
}
