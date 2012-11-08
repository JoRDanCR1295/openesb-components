/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb.pojose.core.anno.processor.impl;

import org.glassfish.openesb.pojose.api.annotation.OnReply;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyOnReplyAnnotation;

/**
 *
 * @author gpatil
 */
public class WrapperOnReply implements ProxyOnReplyAnnotation{
    private OnReply or;
    
    public WrapperOnReply(OnReply or){
        this.or = or;
    }

    public OnReply getAnnotation(){
        return this.or;
    }
}
