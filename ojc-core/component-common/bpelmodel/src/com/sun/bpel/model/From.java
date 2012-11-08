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
 * @(#)From.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import com.sun.bpel.xml.common.model.XMLText;
import com.sun.wsdl4j.ext.bpel.MessageProperty;



/**
 * Describes the &lt;from&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface From extends BPELElement, 
                              PartnerLinkReference,
                              VariableReference {
    /** Tag for this element. */
    public static final String TAG = "from";
    
    /** Describes attributes for this element.
     */
    public interface ATTR extends PartnerLinkReference.ATTR, 
                                  VariableReference.ATTR {
        
    	/** "part" attribute token */
        public static final String PART = "part";
        
        /** "serviceReference" attribute token */
        public static final String ENDPOINT_REFERENCE = "endpointReference";
        
        /** "property" attribute token */
        public static final String PROPERTY = "property";
        
        /** "expressionLanguage" attribute token */
        public static final String EXPRESSIONLANGUAGE = "expressionLanguage";
        
        /** NM property is a JBI extension normalized message property  */
        public static final String NM_PROPERTY = "nmProperty";
    }
    
    /** Ordinal position for variable attribute */
    public static final int VARIABLE = 0;
    
    /** Ordinal position for part attributes */
    public static final int PART = VARIABLE + 1;
        
    /** Ordinal position for partnerLink attribute */
    public static final int PARTNERLINK = PART + 1;
    
    /** Ordinal position for serviceReference attribute */
    public static final int ENDPOINT_REFERENCE = PARTNERLINK + 1;
    
    /** Ordinal position for property attribute */
    public static final int PROPERTY = ENDPOINT_REFERENCE + 1;
    
    /** Ordinal position for expressionLanguage attribute */
    public static final int EXPRESSION_LANGUAGE = PROPERTY + 1;
    
    /** Ordinal position for NM property. NM property is a JBI extension normalized message property  */
    public static final int NM_PROPERTY = EXPRESSION_LANGUAGE + 1;
    
    /** Getter for part attribute.
     * @return  part attribute.
     */
    String getPart();
    
    /** Setter for part attribute.
     * @param   p   part attribute.
     */
    void setPart(String p);
    
    /** Getter for serviceReference attribute.
     * @return  serviceReference attribute.
     */
    String getEndPointReference();
    
    /** Setter for serviceReference attribute.
     * @param   s   serviceReference attribute.
     */
    void setEndPointReference(String s);
    
    /** Getter for property attribute.
     * @return property attribute.
     */
    String getProperty();
    
    /** Setter for property attribute.
     * @param   p   property attribute.
     */
    void setProperty(String p);
    
    /** Getter for property attribute.
     * @return property attribute.
     */
    MessageProperty getBPELProperty();
    
    /** Setter for property attribute.
     * @param   p   property attribute.
     */
    void setBPELProperty(MessageProperty p);
    
    
    /** Getter for expression attribute.
     * @return  expression attribute.
     */
    String getExpression();
    
    /** Setter for expression attribute.
     * @param   e   expression attribute.
     */
    void setExpression(String e);
    
    /**
     * Getter for expressionLanguage attribute.
     * @return expressionLanguage
     */
    String getExpressionLanguage();
    
    /**
     * Setter for expressionLanguage attribute.
     * @param e expressionLanguage
     */
    void setExpressionLanguage(String e);
    
    /** Getter for literal sub-element.
     * @return  sub-element of the from element.
     */
    Literal getLiteral();
    
    /** Setter for Literal sub-element.
     * @param   l   Literal sub-element.
     */
    void setLiteral(Literal l);
    
    /**
     * Setter for expression text.
     * @param t XMLText
     */
    void setExpressionText(XMLText t);
    
    /**
     * Getter for expression text.
     * @return XMLText
     */
    XMLText getExpressionText();
    
    String getNMProperty();
    
    void setNMProperty(String  nmProp);
    
    /**
     * Get the list of xpath variables
     * @return
     *//*
    List getXPathVariablesList();
    
    *//**
     * Set the list of xpath variables
     * @param list
     *//*
    void setXPathVariablesList(List list);*/
}
