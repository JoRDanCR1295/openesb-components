/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb.pojose.core.anno.processor.impl;

import org.glassfish.openesb.pojose.api.annotation.OnDone;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyOnDoneAnnotation;

/**
 *
 * @author gpatil
 */
public class WrapperOnDone implements ProxyOnDoneAnnotation{

    private OnDone od;
    
    public WrapperOnDone(OnDone od){
        this.od = od;
    }

    public OnDone getAnnotation(){
        return this.od;
    }
}
