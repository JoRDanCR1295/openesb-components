 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.endpoint;


import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import org.w3c.dom.DocumentFragment;

/**
 * This Class Implements ServiceEndpoint for resolve the endpoint dynamically 
 * and use it for setting the dynamic corba object in the exchange
 * 
 * @author <a href="mailto:lacquaviva@imolinfo.it">Luca Acquaviva</a>
 */
public class Jbi4CorbaSFServiceEndpoint implements ServiceEndpoint{
    
    /**
     * The Jbi4CorbaEndpont
     **/
    private Jbi4CorbaEndpoint endpoint;
    
    
    /**
     * The String that Contain the IOR
     */
    private String ior;

    /**
     * The DocumentFragment that Contain the EPR
     */
    private DocumentFragment epr;
    
    /**
     * EPR info
     * 
     * /
    private  EPREndpointInfo  eprInfo;

    
    /**
     * Constructor
     */
    public Jbi4CorbaSFServiceEndpoint(Jbi4CorbaEndpoint ep,DocumentFragment epr,String ior){
        this.epr=epr;
        endpoint=ep;
        this.ior=ior;
    }
    
    public DocumentFragment getAsReference(QName arg0) {
        
        return epr;
    }

     /**
     * Return the  Endpointname
     * @return the return
     **/
    public String getEndpointName() {
        return endpoint.getEndpointName();
    }

     /**
     * Return the  Endpointname
     * @the Return
     */
    public QName[] getInterfaces() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    /**
     * Return the  Endpointname
     * @return 
     */
    public QName getServiceName() {
        return endpoint.getServiceName();
    }

    /**
     * Return the Jbi4Corba Endpoint
     */
    public Jbi4CorbaEndpoint getEndpoint() {
        return endpoint;
    }

     /**
     * Return the IOR
     */
    public String getIor() {
       return this.ior;
    }
    
}
