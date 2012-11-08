/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.jbi.wsdl;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;

import java.util.Iterator;
import java.util.List;

import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 * Jbi4Corba wsdl extensions helper class.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public final class Jbi4EjbExtensionUtils {

    /** The logger for this class and its instances. */
    private static final Logger LOG = LoggerFactory
    .getLogger(Jbi4EjbExtensionUtils.class);
    
    /**
     * Instantiates a new jbi4 ejb extension utils.
     */
    private Jbi4EjbExtensionUtils() {}


    /**
     * Gets the <code>Binding</code>.
     * 
     * @param def the WSDL definition
     * @param serviceName the service name
     * @param endpointName the endpoint name
     * 
     * @return the Binding
     */
    public static Binding getBinding(final Definition def,
            final String serviceName, final String endpointName) {
        LOG.debug("getBinding-getServices" + def.getServices().size());
        
        final Service svc = def.getService(QName.valueOf(serviceName));
        
        LOG.debug("getBinding-getService:" + svc);

        if (svc == null) {
            return null;
        }

        final Port port = svc.getPort(endpointName);

        if (port == null) {
            return null;
        } else {
            return port.getBinding();
        }
    }

    /**
     * Gets the <code>PortType</code>.
     * 
     * @param def the WSDL definition
     * @param serviceName the Service Name
     * @param endpointName the Endpoint name
     * 
     * @return the WSDL port type
     */
    public static PortType getPortType(final Definition def,
            final String serviceName, final String endpointName) {
        final Service svc = def.getService(QName.valueOf(serviceName));

        if (svc == null) {
            return null;
        }

        final Port port = svc.getPort(QName.valueOf(endpointName)
                .getLocalPart());

        Binding binding = null;
        if (port == null) {
            return null;
        } else {
            binding = port.getBinding();
        }
        PortType portType = null;
        if (binding != null) {           
            portType = binding.getPortType();
        }
        return portType;
    }    

    /**
     * Gets the <code>Types</code> jbi4ejb extension.
     * 
     * @param def the WSDL definition
     * 
     * @return the <code>Jbi4EjbTypes</code> extensibility element
     */
    @SuppressWarnings("unchecked")
    public static Jbi4EjbTypes getEjbTypes(final Definition def) {
        
        Jbi4EjbTypes ejbTypes = null;

        List extElems = def.getExtensibilityElements();
        
        Iterator extIter = null;
        if (extElems != null) {
            extIter = extElems.iterator();
        }

        while ((extIter != null) && extIter.hasNext() && (ejbTypes == null)) {
            final ExtensibilityElement ee = (ExtensibilityElement) extIter
            .next();
            LOG.debug("Inspecting: " + ee);
            if (Jbi4EjbTypes.class.isInstance(ee)) {
                ejbTypes = (Jbi4EjbTypes) ee;
                LOG.debug("Found a Jbi4EjbTypes instance: " + ejbTypes.getTypesSerialVersionUIDs());
            }
        }
        if (ejbTypes == null) {
            LOG.debug("No jbi4EjbTypes instance found, returning an empty one");
            ejbTypes = new Jbi4EjbTypes(); 
        }
        return ejbTypes;
    }

    /**
     * Gets the <code>Jbi4EjbBinding</code>.
     * 
     * @param def the WSDL definition
     * @param serviceName the service name
     * @param endpointName the endpoint name
     * 
     * @return the <code>Jbi4EjbBinding</code>
     */
    @SuppressWarnings("unchecked")
    public static Jbi4EjbBinding getEjbBinding(final Definition def,
            final String serviceName, final String endpointName) {
        Jbi4EjbBinding ejbBinding = null;
        final Binding binding = getBinding(def, serviceName, endpointName);

        if (binding != null) {
            final List extElems = binding.getExtensibilityElements();

            Iterator extIter = null;
            if (extElems != null) {
                extIter = extElems.iterator();
            }            

            while ((extIter != null) && extIter.hasNext() &&
                    (ejbBinding == null)) {
                final ExtensibilityElement ee = (ExtensibilityElement) extIter
                .next();

                if (Jbi4EjbBinding.class.isInstance(ee)) {
                    ejbBinding = (Jbi4EjbBinding) ee;
                }
            }
        }

        return ejbBinding;
    }

    /**
     * Gets the <code>Jbi4EjbAddress</code>.
     * 
     * @param def the WSDL definition
     * @param serviceName the service name
     * @param endpointName the endpoint name
     * 
     * @return the <code>Jbi4EjbAddress</code>
     */
    @SuppressWarnings("unchecked")
    public static Jbi4EjbAddress getEjbAddress(final Definition def,
            final String serviceName, final String endpointName) {
        Jbi4EjbAddress address = null;
        final Service svc = def.getService(QName.valueOf(serviceName));

        if (svc == null) {
            return null;
        }

        final Port port = svc.getPort(QName.valueOf(endpointName)
                .getLocalPart());

        if (port != null) {
            final List extElems = port.getExtensibilityElements();

            Iterator extIter = null;
            
            if (extElems != null) {
                extIter = extElems.iterator();
            }

            while ((extIter != null) && extIter.hasNext() && (address == null)) {
                final ExtensibilityElement ee = (ExtensibilityElement) extIter
                .next();

                if (Jbi4EjbAddress.class.isInstance(ee)) {
                    address = (Jbi4EjbAddress) ee;
                }
            }
        }
        return address;
    }
                
}
