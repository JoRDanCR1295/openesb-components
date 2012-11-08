/*
 * Notification.java
 * 
 * Created on May 22, 2007, 1:02:29 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.workflow.model;

import javax.xml.namespace.QName;




/**
 *
 * 
 */
public interface Notification extends ModelElement, TaskElement {
    
    public enum Type {Email};
   
    String getName();     

    Type getType ();
    
    QName getQName ();
    
    Subject getSubject();
    
    Message getMessage();    
    
 
}
