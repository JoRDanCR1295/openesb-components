/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb.pojose.core.anno.processor.impl;

import org.glassfish.openesb.pojose.api.annotation.OnError;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyOnErrorAnnotation;

/**
 *
 * @author gpatil
 */
public class WrapperOnError implements ProxyOnErrorAnnotation{
    private OnError or;
    
    public WrapperOnError(OnError or){
        this.or = or;
    }

    public OnError getAnnotation(){
        return this.or;
    }
}
