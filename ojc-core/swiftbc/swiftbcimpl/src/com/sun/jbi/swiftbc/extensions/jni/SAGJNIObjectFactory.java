/*
 * SAGJNIObjectFactory.java
 *
 * Created on April 7, 2007, 8:32 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.swiftbc.extensions.jni;
import com.sun.jbi.swiftbc.extensions.SwiftEnvelope;
import com.sun.jbi.swiftbc.extensions.SwiftLetter;
import com.sun.jbi.swiftbc.extensions.SwiftMessage;
import com.sun.jbi.swiftbc.extensions.SwiftNamedItem;
import com.sun.jbi.swiftbc.extensions.SwiftNamedItemList;
import com.sun.jbi.swiftbc.extensions.SwiftRoutingStep;
import com.sun.jbi.swiftbc.extensions.SAGObjectFactory;

/**
 *
 * @author Sun Microsystems, Inc.
 */
public class SAGJNIObjectFactory implements SAGObjectFactory{
    
    /** Creates a new instance of SAGJNIObjectFactory */
    public SAGJNIObjectFactory() {
    }
    
    public SwiftMessage getNewMessage(){
        return new SAGJNIMessage();
    }
    public SwiftLetter getNewLetter(){
        return new SAGJNILetter();
    }
    
    public SwiftEnvelope getNewEnvelope(){
        return new SAGJNIEnvelope();
    }
    public SwiftRoutingStep getNewRoutingStep(){
        return new SAGJNIRoutingStep();
    }
    public SwiftNamedItem getNewNamedItem(){
        return new SAGJNINamedItem();
    }
    public SwiftNamedItemList getNewNamedItemList(){
        return new SAGJNINamedItemList();
    }
    
    
}
