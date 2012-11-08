/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb.pojose.core.anno.processor.impl;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.transform.Source;
import org.glassfish.openesb.pojose.api.annotation.OnDone;
import org.glassfish.openesb.pojose.api.annotation.OnError;
import org.glassfish.openesb.pojose.api.annotation.OnFault;
import org.glassfish.openesb.pojose.api.annotation.OnReply;
import org.glassfish.openesb.pojose.api.annotation.Operation;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyMethod;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyOnDoneAnnotation;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyOnErrorAnnotation;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyOnFaultAnnotation;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyOnReplyAnnotation;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyOperationAnnotation;
import org.w3c.dom.Node;

/**
 *
 * @author gpatil
 */
public class WrapperMethod implements ProxyMethod {
    private Method m;
    private static final Class BYTE_ARRAY_CLASS = new byte[0].getClass();    
    
    public WrapperMethod(Method m){
        this.m = m;
    }

    public ProxyOperationAnnotation getAnnotationOperation() {
        Operation o = this.m.getAnnotation(Operation.class);
        if (o != null){
            return new WrapperOperation(o);
        } else {
            return null;
        }
    }

    public String getName() {
        return this.m.getName();
    }

    public ParamType getFirstParameterType() {
        ParamType ret = null;
        
        Class[] paramTypes = m.getParameterTypes();
        if ((paramTypes != null) && (paramTypes.length > 0)){
            if (paramTypes[0] == Void.TYPE) {
                ret = ProxyMethod.ParamType.Void;
            } else if (paramTypes[0] == Object.class) {
                ret = ProxyMethod.ParamType.Object;
            } else if (paramTypes[0] == BYTE_ARRAY_CLASS) {
                ret = ProxyMethod.ParamType.ByteArray;
            } else if (paramTypes[0] == String.class) {
                ret = ProxyMethod.ParamType.String;
            } else if (paramTypes[0] == Node.class) {
                ret = ProxyMethod.ParamType.Node;
            } else if (paramTypes[0] == Source.class) {
                ret = ProxyMethod.ParamType.Source;
            } else if (paramTypes[0] == NormalizedMessage.class) {
                ret = ProxyMethod.ParamType.NormalizedMessage;
            } else if (paramTypes[0] == MessageExchange.class) {
                ret = ProxyMethod.ParamType.MessageExchange;
            }
        }

        return ret;
    }

    public ParamType getReturnParameterType() {
        ParamType ret = null;
        Class retType = m.getReturnType();
        if (retType == Void.TYPE) {
            ret = ProxyMethod.ParamType.Void;
        } else if (retType == Object.class) {
            ret = ProxyMethod.ParamType.Object;
        } else if (retType == BYTE_ARRAY_CLASS) {
            ret = ProxyMethod.ParamType.ByteArray;
        } else if (retType == String.class) {
            ret = ProxyMethod.ParamType.String;
        } else if (retType == Node.class) {
            ret = ProxyMethod.ParamType.Node;
        } else if (retType == Source.class) {
            ret = ProxyMethod.ParamType.Source;
        } else if (retType == NormalizedMessage.class) {
            ret = ProxyMethod.ParamType.NormalizedMessage;
        } else if (retType == MessageExchange.class) {
            ret = ProxyMethod.ParamType.MessageExchange;
        } 
        return ret;
    }

    public ProxyOnReplyAnnotation getOnReplyAnnotation() {
        OnReply or = this.m.getAnnotation(OnReply.class);
        if (or != null){
            return new WrapperOnReply(or);
        }
        return null;
    }

    public ProxyOperationAnnotation getOperationAnnotation() {
        Operation or = this.m.getAnnotation(Operation.class);
        if (or != null){
            return new WrapperOperation(or);
        }
        return null;
    }        

    public ProxyOnDoneAnnotation getOnDoneAnnotation() {
        OnDone or = this.m.getAnnotation(OnDone.class);
        if (or != null){
            return new WrapperOnDone(or);
        }
        return null;
    }

    public ProxyOnErrorAnnotation getOnErrorAnnotation() {
        OnError or = this.m.getAnnotation(OnError.class);
        if (or != null){
            return new WrapperOnError(or);
        }
        return null;
    }

    public ProxyOnFaultAnnotation getOnFaultAnnotation() {
        OnFault or = this.m.getAnnotation(OnFault.class);
        if (or != null){
            return new WrapperOnFault(or);
        }
        return null;
    }
        
    public boolean isPublic() {
        return Modifier.isPublic(this.m.getModifiers());
    }

    public boolean isStatic() {
        return Modifier.isStatic(this.m.getModifiers());
    }
    
    // ***** Non interface methods
    public Method getMethod(){
        return this.m;
    }
    
}
