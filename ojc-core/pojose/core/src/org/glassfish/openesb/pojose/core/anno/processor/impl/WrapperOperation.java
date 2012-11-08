/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb.pojose.core.anno.processor.impl;

import org.glassfish.openesb.pojose.api.annotation.Operation;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyOperationAnnotation;

/**
 *
 * @author gpatil
 */
public class WrapperOperation implements ProxyOperationAnnotation{
    private Operation o;
    
    public WrapperOperation(Operation o){
        this.o = o;
    }
    public String name() {
        return this.o.name();
    }

    public String outMessageTypeQN(){
        return this.o.outMessageTypeQN();
    }
    
    /**
     * @deprecated 11/06/08. Use outMessageTypeQN
     * @return
     */
    public String outMessageType() {
        return this.o.outMessageType();
    }

    /**
     * @deprecated 11/06/08. Use outMessageTypeQN
     * @return
     */    
    public String outMessageTypeNS() {
        return this.o.outMessageTypeNS();
    }
    
    // ***** Non interface methods    
    
    public Operation getAnnotation(){
        return this.o;
    }
}
