/*
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.jbi.xfire;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import javax.jbi.component.ComponentContext;
import org.codehaus.xfire.handler.LocateBindingHandler;
import org.codehaus.xfire.service.Service;
import org.codehaus.xfire.soap.SoapTransport;
import org.codehaus.xfire.soap.handler.SoapBodyHandler;
import org.codehaus.xfire.transport.AbstractTransport;
import org.codehaus.xfire.transport.Channel;
import org.codehaus.xfire.transport.DefaultEndpoint;
import org.codehaus.xfire.wsdl11.WSDL11Transport;

/**
 * Simple jbi transport, similar to local transport, but without soap encoding.
 * #TODO Questa classe dovrebbe sparire se decidono di mettere la classe in
 * common
 */
public class JbiTransport extends AbstractTransport
        implements WSDL11Transport, SoapTransport {

    public static final String JBI_BINDING
            = "http://java.sun.com/xml/ns/jbi/binding/service+engine";

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(JbiTransport.class);

    private static final String URI_PREFIX = "urn:xfire:transport:jbi:";

    /**
     * The hash code value returned by the instances of this class.
     *
     * @see #hashCode()
     */
    private static final int HASH_CODE = 61;

    private ComponentContext context;

    public JbiTransport(ComponentContext context) {
        addInHandler(new LocateBindingHandler());
        addInHandler(new SoapBodyHandler());
        this.context = context;
    }

    public String getName() {
        return "JBI";
    }

    public String getServiceURL(Service service) {
        return "jbi://" + service.getName();
    }

    protected Channel createNewChannel(String uri) {
        JbiChannel c;

        if (LOG.isDebugEnabled()) {
            LOG.debug("Creating new channel for uri: " + uri);
        }
        c = new JbiChannel(uri, this);
        c.setEndpoint(new DefaultEndpoint());
        return c;
    }

    protected String getUriPrefix() {
        return URI_PREFIX;
    }

    @Override
    public String[] getSupportedBindings() {                    // Overridden
        return new String[] { JBI_BINDING };
    }

    public String[] getKnownUriSchemes() {
        return new String[] { "jbi://" };
    }

    public ComponentContext getContext() {
        return context;
    }

    /**
     * Indicates whether some other object is "equal to" this one.
     *
     * @param   obj   the reference object with which to compare.
     * @return  <code>true</code> if and only if <code>obj</code> is an instance
     *          of <code>JbiTransport</code> class; <code>false</code>
     *          otherwise.
     * @see     #hashCode()
     */
    @Override
    public boolean equals(Object obj) {                         // Overridden
        return (obj instanceof JbiTransport);
    }

    /**
     * Returns a hash code value for the object.
     *
     * @return  a hash code value for this object.
     * @see     #equals(Object)
     */
    @Override
    public int hashCode() {                                     // Overridden
        return HASH_CODE;
    }
}
