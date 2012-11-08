/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.jbi.xfire;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;

import org.codehaus.xfire.handler.LocateBindingHandler;
import org.codehaus.xfire.service.Service;
import org.codehaus.xfire.soap.SoapTransport;
import org.codehaus.xfire.soap.handler.SoapBodyHandler;
import org.codehaus.xfire.transport.AbstractTransport;
import org.codehaus.xfire.transport.Channel;
import org.codehaus.xfire.transport.DefaultEndpoint;
import org.codehaus.xfire.wsdl11.WSDL11Transport;

/**
 * Simple ejb transport, similar to local transport, but without soap encoding.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class EjbTransport extends AbstractTransport implements WSDL11Transport, SoapTransport {
    
    /** The Constant EJB_BINDING. */
    public static final String EJB_BINDING = "uri://schemas.imola.it/jbi/wsdl-extensions/ejb/";

    private static final String KNOWN_URI_SCHEME = "ejb://";

    private static final String EJB = "EJB";

    /** The logger for this class and its instances. */
    private static final Logger LOG
            = LoggerFactory.getLogger(EjbTransport.class);   

    /** The Constant URI_PREFIX. */
    private static final String URI_PREFIX = "urn:xfire:transport:ejb:";
           
    /**
     * Instantiates a new ejb transport.
     */
    public EjbTransport() {
        addInHandler(new LocateBindingHandler());
        addInHandler(new SoapBodyHandler());   
    }

    /**
     * Returns the transport name.
     * 
     * @return the name (always <code>EJB</code> constant)
     * 
     * @see org.codehaus.xfire.wsdl11.WSDL11Transport#getName()
     */
    public String getName() {
        return EJB;
    }
    
    /**
     * Returns the service URL.
     * 
     * @param service the service
     * 
     * @return the service url
     * 
     * @see org.codehaus.xfire.wsdl11.WSDL11Transport#getServiceURL(org.codehaus.xfire.service.Service)
     */
    public String getServiceURL(Service service) {
        return KNOWN_URI_SCHEME + service.getName();
    }

    
    /**
     * Creates a new Channel.
     * 
     * @param uri
     *            the channel uri
     * 
     * @return the channel (always a <code>EjbChannel</code>
     * 
     * @see org.codehaus.xfire.transport.AbstractTransport#createNewChannel(java.lang.String)
     */
    protected Channel createNewChannel(String uri) {
        LOG.debug("Creating new channel for uri: " + uri);
        EjbChannel c = new EjbChannel(uri, this);
        c.setEndpoint(new DefaultEndpoint());
        return c;
    }

    
    /**
     * Returns the prefix uri (the <code>URI_PREFIX</code> constant).
     * 
     * @return the prefix uri
     * 
     * @see org.codehaus.xfire.transport.AbstractTransport#getUriPrefix()
     */
    protected String getUriPrefix() {
        return URI_PREFIX; 
    }

    
    /**
     * Return the supported binding array. Supports only the
     * <code>EJB_BINDING</code> binding
     * 
     * @return the supported binding array
     * 
     * @see org.codehaus.xfire.transport.AbstractTransport#getSupportedBindings()
     */
    public String[] getSupportedBindings()
    {
        return new String[] {EJB_BINDING};
    }
    
    /**
     * Return the known uri scheme (only the <code>KNOWN_URI_SCHEME</code> constant value.
     * 
     * @return the
     * 
     * @see org.codehaus.xfire.transport.AbstractTransport#getKnownUriSchemes()
     */
    public String[] getKnownUriSchemes() {
        return new String[] { KNOWN_URI_SCHEME };
    }

    /**
     * True if the object is of the same instance (EjbTransport).
     * 
     * @param o the object
     * 
     * @return true  if the object is of the same instance. 
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object o) {
        return o instanceof EjbTransport;
    }
     
}
