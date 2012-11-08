 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.component;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.jbi.Messages;
import it.imolinfo.jbi4corba.jbi.component.runtime.ComponentRuntime;


import it.imolinfo.jbi4corba.jbi.endpoint.Jbi4CorbaEndpoint;


import it.imolinfo.jbi4corba.jbi.endpoint.Jbi4CorbaSFServiceEndpoint;


import it.imolinfo.jbi4corba.utils.HelperEPRUtils;
import java.io.StringWriter;

import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.servicedesc.ServiceEndpoint;


import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;



import org.w3c.dom.Node;
import org.w3c.dom.NodeList;



/**
 * Jbi4EJB Binding Component. 
 * @see Jbi4CorbaLifeCycle for more details of the generated code.
 *
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4CorbaRuntime extends ComponentRuntime {
    	
    /** The Logger. */
    private static final Logger LOG = LoggerFactory
            .getLogger(Jbi4CorbaRuntime.class);    
    /** **/  
    private static final Messages MESSAGES = 
    	Messages.getMessages(Jbi4CorbaRuntime .class);
    
    /**
     * constructor.
     */
    public Jbi4CorbaRuntime() {
        super();
    }
    
    /**
     * creates the Component specific implementation of the ComponentLifeCycle.
     * 
     * @return the <code>Jbi4EjbLifeCycle</code> instance
     */
    protected ComponentLifeCycle createComponentLifeCycle() {        
        return new Jbi4CorbaLifeCycle(this);
    }
    
    /**
     * creates the Component specific implementation of the ServiceUnitManager.
     * 
     * @return the <code>Jbi4EjbSUManager</code> instance
     */
    protected ServiceUnitManager createServiceUnitManager() {
        return new Jbi4CorbaSUManager(this);
    }
    
    
    /**
     * Resolve Ednpoint for Dynamic PartnerLink
     * @param  epr Document fragment that rappresent the EndpointReference
     * @return the <code>ServiceEndpoint</code> instance
     * 
     */
    @Override
      public javax.jbi.servicedesc.ServiceEndpoint resolveEndpointReference(
        org.w3c.dom.DocumentFragment epr) {
    
    	ServiceEndpoint sendpoint=null;
        ServiceEndpoint resolvedEP=null;
        String ior = null;
        NodeList children = epr.getChildNodes();
        try {

        	if (children != null && children.getLength() > 1) {
        	
        		ior = HelperEPRUtils.getDynamicIORFromEPR(epr);
        		sendpoint = HelperEPRUtils.getEndpointInfo(epr);

           
        	} else if (children.getLength() == 1) {   // "wrapper" element found
           
        	
        		Node child = children.item(0);
            
        		ior = HelperEPRUtils.getDynamicIORFromEPR(child);
        		sendpoint = HelperEPRUtils.getEndpointInfo(child);

        	
        	} else {
           
                    throw new Exception(MESSAGES.getString("CRB000231_Invalid_EPR_Format"));
        	}
                
                
                //Find the Service Endpoint on BUS
                if (ior != null && sendpoint != null) {
               
                    
                    Jbi4CorbaSUManager manager = (Jbi4CorbaSUManager) this.getServiceUnitManager();
                    if (manager != null) {
                        Jbi4CorbaEndpoint endpoint = manager.getDeployedEndpoint(sendpoint); 
            
                        if (endpoint != null) {
                        //ToDo i18N
                        LOG.debug("RESOLVED EPR ==>"+endpoint.getEndpointName());
                        resolvedEP=new Jbi4CorbaSFServiceEndpoint(endpoint,epr,ior);
               
                        }
        
                    }
                    
                } else {
                
                    String msg=MESSAGES.getString("CRB000232_Can't_find_Activate_Endpoint");
                    LOG.info(msg);    
                    
                } 

        } catch (Exception e) {
        	
        	LOG.error(MESSAGES.getString("CRB000232_Invalid_EPR_Format"));
        }
       
       
        
        return resolvedEP;
    }
    
    
    
    
    /**
     * Retrieves a DOM representation containing metadata which describes the
     * service provided by this component, through the given endpoint.
     *
     * @param endpoint the service endpoint.
     * @return the description for the specified service endpoint.
     * @see javax.jbi.Component#getServiceDescription(javax.jbi.servicedesc.ServiceEndpoint)
     */
    
    @Override
    public org.w3c.dom.Document getServiceDescription(ServiceEndpoint ref) {
        LOG.debug("Called getServiceDescription for endpoint: " + ref);
        Jbi4CorbaSUManager manager = (Jbi4CorbaSUManager) this.getServiceUnitManager();
        if (manager != null) {
            Jbi4CorbaEndpoint endpoint = manager.getDeployedEndpoint(ref); 
            if (endpoint != null) {
                if (LOG.isDebugEnabled()) {                    
                    String documentString = null;
                    try {
                        // Set up the output transformer to convert the Document to String
                        TransformerFactory transfac = TransformerFactory.newInstance();
                        Transformer trans = transfac.newTransformer();
                        trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
                        trans.setOutputProperty(OutputKeys.INDENT, "yes");
                        StringWriter sw = new StringWriter();
                        StreamResult result = new StreamResult(sw);
                        DOMSource source = new DOMSource(endpoint.getServiceDescription());
                        trans.transform(source, result);
                        documentString = sw.toString();
                        
                    } catch (TransformerException e) {
                        // Should be a warn, but can be raised only if the debug level is activated.
                        LOG.debug("Error in transforming service description to String for logging pourpose");                        
                    }
                    LOG.debug("Endpoint found, returning:" + documentString);                                       
                }
                return endpoint.getServiceDescription();
            } else {
                LOG.debug("No endpoint found, returning null");
            }
        } 
        // Should never happen.
        return null;            
    }
    
}
