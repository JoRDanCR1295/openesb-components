/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb.pojose.core.anno.processor.impl;

import org.glassfish.openesb.pojose.api.annotation.POJO;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyPOJOAnnotation;

/**
 *
 * @author gpatil
 */
public class WrapperPOJO implements ProxyPOJOAnnotation {
    private POJO pojo;
    
    public WrapperPOJO(POJO pojo){
        this.pojo = pojo;
    }
    
    public String interfaceNS() {
        return this.interfaceNS();
    }

    public String interfaceName() {
        return this.interfaceName();
    }

    public String name() {
        return this.name();
    }

    public String serviceNS() {
        return this.serviceNS();
    }

    public String serviceName() {
        return this.serviceName();
    }
    
    // **** Non API class methods.
    
    public POJO getPOJOAnnotation(){
        return this.pojo;
    }
}
