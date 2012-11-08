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
public class SAGObjectFactoryFactory{
    
    /** Creates a new instance of SAGObjectFactory */
    public SAGObjectFactoryFactory() {
    }
    
    public SAGObjectFactory getObjectFactory(){
        return new com.sun.jbi.swiftbc.extensions.jni.SAGJNIObjectFactory();
    }
 
}
