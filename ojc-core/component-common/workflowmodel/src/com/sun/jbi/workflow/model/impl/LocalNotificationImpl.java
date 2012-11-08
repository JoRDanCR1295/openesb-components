/*
 * LocalNotificationImpl.java
 * 
 * Created on Jun 27, 2007, 2:44:46 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.workflow.model.impl;

import com.sun.jbi.workflow.model.LocalNotification;
import com.sun.jbi.workflow.model.ModelElement;
import com.sun.jbi.workflow.model.Notification;
import com.sun.jbi.workflow.model.xmlbeans.TLocalNotification;
import javax.xml.namespace.QName;
import org.w3c.dom.Node;

/**
 *
 * @author radval
 */
public class LocalNotificationImpl extends ModelElementImpl implements LocalNotification {

    private TLocalNotification mLocalNotification;

    public LocalNotificationImpl(TLocalNotification localNotification, ModelElement parent) {
        super(localNotification, parent);
        this.mLocalNotification = localNotification;
    }

    public String getReference() {
        return this.mLocalNotification.getReference();
    }

    public Notification getReferencedNotification() {
        Notification notification = null;
        String reference = getReference();
        if(reference != null) {           
            notification = Util.findNotification(reference, this);
        }
        return notification;
    }

    

}
