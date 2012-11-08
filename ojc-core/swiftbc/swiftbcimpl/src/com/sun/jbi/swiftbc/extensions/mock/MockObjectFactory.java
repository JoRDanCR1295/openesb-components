/*
 * MockObjectFactory.java
 *
 * Created on April 7, 2007, 8:23 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.swiftbc.extensions.mock;

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
public class MockObjectFactory implements SAGObjectFactory{
    
    /** Creates a new instance of MockObjectFactory */
    public MockObjectFactory() {
    }
    public SwiftMessage getNewMessage(){
        return new MockMessage();
    }
    public SwiftLetter getNewLetter(){
        return new MockLetter();
    }
    
    public SwiftEnvelope getNewEnvelope(){
        return new MockEnvelope();
    }
    public SwiftRoutingStep getNewRoutingStep(){
        return new MockRoutingStep();
    }
    public SwiftNamedItem getNewNamedItem(){
        return new MockNamedItem();
    }
    public SwiftNamedItemList getNewNamedItemList(){
        return new MockNamedItemList();
    }
}
