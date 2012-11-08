package com.sun.jbi.restbc.jbiadapter.inbound;

import java.util.HashSet;
import java.util.Set;

import javax.ws.rs.core.Application;

/**
 * DefaultJaxrsPojoApplication.java
 *
 * @author Edward Chou
 */
public class DefaultJaxrsPojoApplication extends Application {

    /* (non-Javadoc)
     * @see javax.ws.rs.core.Application#getClasses()
     */
    @Override
    public Set<Class<?>> getClasses() {
        Set<Class<?>> classes = new HashSet<Class<?>> ();
        classes.add(DefaultJaxrsPojo.class);
        return classes;
    }

}
