/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb.pojose.core.anno.processor.impl;

import org.glassfish.openesb.pojose.api.annotation.Provider;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyProviderAnnotation;

/**
 *
 * @author gpatil
 */
public class WrapperProvider implements ProxyProviderAnnotation {
    private Provider provider;
    
    public WrapperProvider(Provider p){
        this.provider = p;
    }
    
    public String interfaceQN() {
        return this.interfaceQN();
    }

    public String name() {
        return this.name();
    }

    public String serviceQN() {
        return this.serviceQN();
    }

    // **** Non API class methods.
    
    public Provider getProviderAnnotation(){
        return this.provider;
    }
}
