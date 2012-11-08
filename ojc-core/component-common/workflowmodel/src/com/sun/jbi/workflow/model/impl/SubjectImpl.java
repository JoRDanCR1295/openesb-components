/*
 * SubjectImpl.java
 * 
 * Created on Jun 27, 2007, 2:21:15 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.workflow.model.impl;

import javax.wsdl.Part;

import com.sun.jbi.workflow.model.EmailNotification;
import com.sun.jbi.workflow.model.ModelElement;
import com.sun.jbi.workflow.model.ModelException;
import com.sun.jbi.workflow.model.PortTypeModelElement;
import com.sun.jbi.workflow.model.Subject;
import com.sun.jbi.workflow.model.xmlbeans.TExpression;

/**
 *
 * @author radval
 */
public class SubjectImpl extends ExpressionImpl implements Subject{
    
    private Part mPart;
    
    private static String SUBJECT_PART = "subject";
    
    public SubjectImpl(TExpression subject, ModelElement parent) {
        super(subject, parent);
    }

    public String getPart() {
        return SUBJECT_PART;
    }

    public Part getWSDLPart() throws ModelException {
    	if(this.mPart == null) {
    		this.mPart = Util.getPart(this);
    	}
    	
    	return this.mPart;
    }

    public PortTypeModelElement getPortTypeElement() {
        // TODO Auto-generated method stub
        if  (getParent() instanceof EmailNotification) {
            return (EmailNotification) getParent();
        }
        return null;
    }
}
