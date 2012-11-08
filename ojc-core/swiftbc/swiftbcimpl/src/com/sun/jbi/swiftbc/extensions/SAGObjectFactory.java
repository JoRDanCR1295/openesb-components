/*
 * SAGObjectFactory.java
 *
 * Created on April 7, 2007, 7:25 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.swiftbc.extensions;

/**
 *
 * @author Sun Microsystems, Inc.
 */
public interface SAGObjectFactory {
    
    SwiftMessage getNewMessage();
    SwiftLetter getNewLetter();
    
    SwiftEnvelope getNewEnvelope();
    SwiftRoutingStep getNewRoutingStep();
    
    SwiftNamedItem getNewNamedItem();
    SwiftNamedItemList getNewNamedItemList();
}
