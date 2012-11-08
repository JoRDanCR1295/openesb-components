/*
 * LocalNotification.java
 * 
 * Created on Jun 27, 2007, 2:43:27 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.workflow.model;

import javax.xml.namespace.QName;

/**
 *
 * @author radval
 */
public interface LocalNotification extends ModelElement {

    String getReference();
    
    Notification getReferencedNotification();
    
}
