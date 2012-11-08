/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb.pojose.core.anno.processor.impl;

import org.glassfish.openesb.pojose.api.annotation.OnFault;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyOnFaultAnnotation;

/**
 *
 * @author gpatil
 */
public class WrapperOnFault implements ProxyOnFaultAnnotation{
    private OnFault or;
    
    public WrapperOnFault(OnFault or){
        this.or = or;
    }

    public OnFault getAnnotation(){
        return this.or;
    }
}
