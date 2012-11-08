/*
 * NotificationImpl.java
 * 
 * Created on May 22, 2007, 1:03:16 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.workflow.model.impl;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;

import com.sun.jbi.workflow.model.Address;
import com.sun.jbi.workflow.model.EmailNotification;
import com.sun.jbi.workflow.model.ModelElement;
import com.sun.jbi.workflow.model.ModelException;
import com.sun.jbi.workflow.model.Notification;
import com.sun.jbi.workflow.model.PortTypeModelElement;
import com.sun.jbi.workflow.model.xmlbeans.TExpression;
import com.sun.jbi.workflow.model.xmlbeans.TNotification;

/**
 *
 * 
 */
public class EmailNotificationImpl extends NotificationImpl implements EmailNotification {
	
    private Operation mOperation;
    
    private Part mPart;
    
    private static String ADDRESS_PART = "addresses";    
        
    public EmailNotificationImpl(TNotification notification, ModelElement parent) {
        super(notification, parent);
    }
    
    public String getName() {
        return this.mNotificationType.getName();
    }

    public Operation getWSDLOperation() throws ModelException {
        if (mOperation != null) {
            return mOperation;
        }

        this.mOperation = Util.getWSDLOperation(getPortType (), this.mNotificationType.getEmail().getOperation(), this);
        return mOperation;
    }

    
    public PortType getPortType() throws ModelException {
        PortType portType = Util.getPortType(mNotificationType.getEmail().getPortType(), this);
        return portType;
    }
    
    public synchronized List<Address> getAddresses() {
        List<Address> recipients = new ArrayList<Address>();
        List<TExpression> rList = this.mNotificationType.getEmail().getAddressList();
        Iterator<TExpression> it = rList.iterator();

        while(it.hasNext()) {
            TExpression r = it.next();
            Address recipient = new AddressImpl(r, this);
            recipients.add(recipient);
        }

        return recipients;
    }

    public Notification.Type getType() {
        // TODO Auto-generated method stub
        return Notification.Type.Email;
    }

    public String getOptName() {
        // TODO Auto-generated method stub
        return mNotificationType.getEmail().getOperation();
    }

    public QName getPortTypeQName() {
        // TODO Auto-generated method stub
        return mNotificationType.getEmail().getPortType();
    }

    public String getPart() {
        // TODO Auto-generated method stub
        return ADDRESS_PART;
    }

    public Part getWSDLPart() throws ModelException {
        // TODO Auto-generated method stub
        if(this.mPart == null) {
            this.mPart = Util.getPart(this);
        }
        
        return this.mPart;
    }

    public PortTypeModelElement getPortTypeElement() {
        // TODO Auto-generated method stub
        return this;
    }
        
}
