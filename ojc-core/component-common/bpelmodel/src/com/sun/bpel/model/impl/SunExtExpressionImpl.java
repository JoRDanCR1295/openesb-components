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

import com.sun.bpel.model.SunExtExpression;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.model.wsdlmodel.impl.XMLTextImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLText;
import com.sun.bpel.xml.common.visitor.Visitor;


/**
 * Implements the &lt;from&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class SunExtExpressionImpl extends BPELElementImpl implements SunExtExpression {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 3492731034037825454L;
    
    /** Holds expression text data for this element. */
    private XMLText expression;
    
    /** Creates a new instance of FromImpl */
    public SunExtExpressionImpl() {
        super();
        initSunExtExpression();
    }
    
    /** Creates a new instance of FromImpl.
     * @param   d   Owner document.
     */
    public SunExtExpressionImpl(XMLDocument d) {
        super(d);
        initSunExtExpression();
    }
    
    private void initSunExtExpression() {
        setLocalName(SunExtExpression.TAG);
        xmlAttrs = new XMLAttribute[] {
                new XMLAttributeImpl(ATTR.EXPRESSION_LANGUAGE, String.class, true, null),
                new XMLAttributeImpl(ATTR.INPUT_VARS, String.class, true, null),
                new XMLAttributeImpl(ATTR.OUTPUT_VARS, String.class, true, null)
            };
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
        expression = null;
    	if(e != null) {
    	    expression = new XMLTextImpl(e);
    	}
    }
    
    /** Getter for property expressionLanguage.
     * @return Value of property expressionLanguage.
     *
     */
    public String getExpressionLanguage() {
        return xmlAttrs[EXPRESSION_LANGUAGE].getValue();
    }
    
    /** Setter for property expressionLanguage.
     * @param expressionLanguage New value of property.
     *
     */
    public void setExpressionLanguage(String expressionLanguage) {
        setAttribute(EXPRESSION_LANGUAGE, expressionLanguage);
    }

    /** Getter for property inputVars.
     * @return Value of property inputVars.
     *
     */
    public String getInputVars() {
        return xmlAttrs[INPUT_VAR].getValue();
    }
    
    /** Setter for property inputVar.
     * @param inputVar New value of property.
     *
     */
    public void setInputVars(String inputVars) {
        setAttribute(INPUT_VAR, inputVars);
    }

    /** Getter for property outputVars.
     * @return Value of property outputVars.
     *
     */
    public String getOutputVars() {
        return xmlAttrs[OUTPUT_VAR].getValue();
    }
    
    /** Setter for property outputVars.
     * @param outputVars New value of property.
     *
     */
    public void setOutputVars(String outputVars) {
        setAttribute(OUTPUT_VAR, outputVars);
    }
    
    /** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean accept(Visitor w) {
        BPELVisitor v = morphVisitor(w);
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
    
}
