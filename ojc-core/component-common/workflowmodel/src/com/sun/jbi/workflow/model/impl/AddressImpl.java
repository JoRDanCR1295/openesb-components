/*
 * AddressImpl.java
 * 
 * Created on May 22, 2007, 1:45:11 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.workflow.model.impl;

import com.sun.jbi.workflow.model.Address;
import com.sun.jbi.workflow.model.ModelElement;
import com.sun.jbi.workflow.model.xmlbeans.TExpression;

/**
 * 
 * 
 */
public class AddressImpl extends ExpressionImpl implements Address {

    public AddressImpl(TExpression address, ModelElement parent) {
        super(address, parent);
    }
}
