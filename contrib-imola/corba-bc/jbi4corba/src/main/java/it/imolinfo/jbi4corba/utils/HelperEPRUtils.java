 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.utils;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.jbi.Messages;
import it.imolinfo.jbi4corba.jbi.component.runtime.RuntimeHelper;

import java.io.IOException;
import java.io.StringReader;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * An utility  Class for the management of the EndpointReference
 * 
 * @author <a href="mailto:lacquaviva@imolinfo.it">Luca Acquaviva</a>
 */
public class HelperEPRUtils {
    
  /**
   * Logger.
   */
  private static final Logger LOG
          = LoggerFactory.getLogger(HelperEPRUtils.class);
  
	/**
	 * The responsible to translate localized messages.
	 */
	private static final Messages MESSAGES = Messages
			.getMessages(HelperEPRUtils.class);  
  /**
   * WS_ADDRESSING Namespaces 
   **/
  private static final String WS_ADDRESSING_NS = "http://schemas.xmlsoap.org/ws/2004/08/addressing";
  private static final String WS_ADDRESSING_NS2 ="http://www.w3.org/2005/08/addressing";  
    
     /**
      * Extract the IOR from a DocumentFragment
      * @param String DocumentFragment the EPR document faragment
      */
    public static String readAddressFromEPR(final String DocumentFragment) {
        
         
         Document document=null;
         String address="";
	 DocumentBuilderFactory fact = DocumentBuilderFactory.newInstance();
	 try {
		DocumentBuilder bd = fact.newDocumentBuilder();
                
                InputSource source = new InputSource(new StringReader(DocumentFragment)); 
                document=bd.parse(source);
         
                
	} catch (SAXException ex) {
            LOG.debug("Error "+ex );    
        } catch (ParserConfigurationException ex) {
             LOG.debug("Error "+ex );    
        } catch (IOException ex){
             LOG.debug("Error "+ex );    
        }  
         
	
	NodeList ndl = document.getElementsByTagName( "Address" );
        address=ndl.item(0).getTextContent(); 
        //Remove the jbi4Corba: from the address and return only IOR:0000...
        if(address.length()>10){
            address=address.substring(10);
        }
        return address;
        
                
    }
    
     
    
   /**
    * Extract the IOR from a DocumentFragment    * 
    * @param Node the parentnode of the DocumentFragment
    * @return String
    * */
   public static final String getDynamicIORFromEPR(final Node parentNode) throws Exception {
       String IOR = null;
       //URL url = null;
       NodeList children = parentNode.getChildNodes();
   	for(int ii=0; ii< children.getLength(); ii++) {
           Node child = children.item(ii);
           if ("Address".equalsIgnoreCase(child.getLocalName())  &&
               WS_ADDRESSING_NS.equals(child.getNamespaceURI())) {
               IOR = child.getTextContent().trim(); 	
               break;
           } 
           if ("Address".equalsIgnoreCase(child.getLocalName())  &&
                   WS_ADDRESSING_NS2.equals(child.getNamespaceURI())) {
                   IOR = child.getTextContent().trim(); 	
                   break;
               } 
       }
       
        if(IOR.startsWith("jbi4corba:")){
             IOR=IOR.substring(10);
        }else{
            IOR=null;
            throw new Exception("CRB0000250 jbi4Corba BC can't resolve this EPR");
        }    
       //Return the ior without jbi4corba:
       return IOR;
       
   }
   
   /**
    * Extract EndpointInfo as Endpointname and InterfaceName from document fragment
    * This Method Support both WS-ADDRESSING Specifics Format
    * @param Node the ChildNode of the DocumentFragment
    **/
   public static final ServiceEndpoint getEndpointInfo(final Node parentNode) throws Exception {
       ServiceEndpoint endpoint = null;
   	
   	NodeList children = parentNode.getChildNodes();
   	for(int ii=0; ii< children.getLength(); ii++) {
           Node child = children.item(ii);
          
           //WS_Addressing 08_2004
           if ("ServiceName".equalsIgnoreCase(child.getLocalName())  &&
               WS_ADDRESSING_NS.equals(child.getNamespaceURI())) {
           	
               NamedNodeMap attributes = child.getAttributes();
               Node portNode = attributes.getNamedItem("PortName");
               String content = child.getTextContent().trim();
               int index = content.indexOf(":"); 			// locate the namespace prefix
               if (index <= 0) {
                  LOG.debug("Invalid Content"); 
               }
               String prefix = content.substring(0, index);
               String serviceLocalName = content.substring(index + 1);
               String namespaceURI = (child.lookupNamespaceURI(prefix) == null)? parentNode.lookupNamespaceURI(prefix) :
                                     child.lookupNamespaceURI(prefix);
               if (namespaceURI == null) {
            	   LOG.debug("Invalid Content"); 
               }
               String endpointName = portNode != null? portNode.getNodeValue() : null;
               LOG.debug("Endpoint Name:" + endpointName);
               LOG.debug("Serivce Name:" + namespaceURI+" "+serviceLocalName);
               //Find the Endpoint on the bus
               endpoint = RuntimeHelper.getComponentContext().getEndpoint( new QName(namespaceURI, serviceLocalName),endpointName);
              
           }
           
           LOG.debug("=="+child.getLocalName());
           LOG.debug("=="+child.getNamespaceURI());
          
           // WS_Addressing 08_2005    	   
           if ("Metadata".equalsIgnoreCase(child.getLocalName())  &&
        		   WS_ADDRESSING_NS2.equals(child.getNamespaceURI())) {
        	   		
            	   NodeList metadata = child.getChildNodes();
            	   for(int i=0; i< metadata.getLength(); i++) {
            		   Node mchild = metadata.item(i);
            		   if ("ServiceName".equalsIgnoreCase(mchild.getLocalName())) {
                       		
                           NamedNodeMap attributes = mchild.getAttributes();
                           Node portNode = attributes.getNamedItem("EndpointName");
                       		
                           String content = mchild.getTextContent().trim();
                           int index = content.indexOf(":"); 			// locate the namespace prefix
                           if (index <= 0) {
                           
                        	   String msg = MESSAGES.getString("CRB001001_Invalid_content");
                        	   LOG.error(msg);
                           
                           }
                           String prefix = content.substring(0, index);
                           String serviceLocalName = content.substring(index + 1);
                           String namespaceURI = (mchild.lookupNamespaceURI(prefix) == null)? parentNode.lookupNamespaceURI(prefix) :
                                                 mchild.lookupNamespaceURI(prefix);
                           if (namespaceURI == null) {
                        	   String msg = MESSAGES.getString("CRB001001_Invalid_content");
                        	   LOG.error(msg);
                           }
                           
                           String endpointName = portNode != null? portNode.getNodeValue() : null;
                           LOG.debug("Endpoint Name"+endpointName);
                           LOG.debug("Service Name"+namespaceURI+" "+serviceLocalName);
                           //Find the Endpoint on the bus
                           endpoint =RuntimeHelper.getComponentContext().getEndpoint( new QName(namespaceURI, serviceLocalName),endpointName);
                          
                       }
            		   
            		   		   
            	  } 		   
           }	
        	   
           
       }
      
       return endpoint;
   }
   

}
