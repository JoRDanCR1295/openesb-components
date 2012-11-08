package com.sun.jbi.restbc.jbiadapter.inbound;

import javax.servlet.ServletException;

import com.sun.jersey.spi.container.servlet.ServletContainer;

/**
 * JaxrsServletContainer.java
 *
 * @author Edward Chou
 */
public class JaxrsServletContainer extends ServletContainer {
    
    private ClassLoader cl;
    
    public JaxrsServletContainer(ClassLoader cl) {
        this.cl = cl;
    }
    
    @Override
    public void init() throws ServletException {
        ClassLoader oldCl = Thread.currentThread().getContextClassLoader();
        Thread.currentThread().setContextClassLoader(cl);
        super.init();
        Thread.currentThread().setContextClassLoader(oldCl);
    }
    
}
