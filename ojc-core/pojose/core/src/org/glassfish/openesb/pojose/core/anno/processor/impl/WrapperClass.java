/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb.pojose.core.anno.processor.impl;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.glassfish.openesb.pojose.api.annotation.ConsumerEndpoint;
import org.glassfish.openesb.pojose.api.annotation.Endpoint;
import org.glassfish.openesb.pojose.api.annotation.POJO;
import org.glassfish.openesb.pojose.api.annotation.Provider;
import org.glassfish.openesb.pojose.core.anno.processor.*;

/**
 * Implementation of ProxyClass wrapping Class class.
 * Another implemention may use ClassFile API instead actual Class.
 * 
 * @author gpatil
 */
public class WrapperClass implements ProxyClass{
    private Class cls;
    private List<ProxyMethod> ml;
    
    public WrapperClass(Class cls){
        this.cls = cls;
    }

    /**
     * Return POJO annotation wrapper class.
     * 
     * @return
     */
    public ProxyPOJOAnnotation getAnnotationPOJO() {
        POJO pojo = (POJO) this.cls.getAnnotation(POJO.class);
        if (pojo != null){
            return new WrapperPOJO(pojo);
        }
        return null;
    }

    public ProxyProviderAnnotation getAnnotationProvider() {
        Provider pr = (Provider) this.cls.getAnnotation(Provider.class);
        if (pr != null){
            return new WrapperProvider(pr);
        }
        return null;
    }
    
    public synchronized List<ProxyMethod> getMethods() {
        if (ml == null){
            Method[] ms = this.cls.getMethods();
            ml = new ArrayList<ProxyMethod>();
            for (Method m : ms){
                ml.add(new WrapperMethod(m));
            }
        }
        return ml;
    }

    public String getName() {
        return this.cls.getName();
    }
    
    // ***** non API methods.
    public Class getPOJOClass(){
        return this.cls;
    }
    
    public Map<String, Endpoint> getField2EndpointMap(){
        Map<String, Endpoint> ret = new HashMap<String, Endpoint>();
        String name  = null;
        Endpoint ep = null;
        Field[] fs = cls.getDeclaredFields();
        for (Field f : fs){
            ep = null;
            name = f.getName();
            ep = f.getAnnotation(Endpoint.class);
            if (ep != null){
                ret.put(name, ep);
            }
        }
        
        return ret;
    }

    public Map<String, ConsumerEndpoint> getField2ConsumerEndpointMap(){
        Map<String, ConsumerEndpoint> ret = new HashMap<String, ConsumerEndpoint>();
        String name  = null;
        ConsumerEndpoint ep = null;
        Field[] fs = cls.getDeclaredFields();
        for (Field f : fs){
            ep = null;
            name = f.getName();
            ep = f.getAnnotation(ConsumerEndpoint.class);
            if (ep != null){
                ret.put(name, ep);
            }
        }
        
        return ret;
    }
}
