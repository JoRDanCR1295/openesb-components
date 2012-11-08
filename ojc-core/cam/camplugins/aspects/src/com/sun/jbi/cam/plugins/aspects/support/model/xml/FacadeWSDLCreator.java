/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)FacadeWSDLCreator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 *
 */
package com.sun.jbi.cam.plugins.aspects.support.model.xml;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;

import org.apache.xmlbeans.XmlException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.sun.jbi.cam.common.XMLUtils;
import com.sun.jbi.cam.plugins.aspects.common.XmlConstants;
import com.sun.jbi.cam.plugins.aspects.support.model.FacadeConfiguration;
import com.sun.jbi.cam.plugins.aspects.support.model.PartnerConfiguration;
import com.sun.jbi.cam.plugins.aspects.support.model.ProviderConfiguration;
import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model.PartnerLinkType;
import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model.PartnerLinkTypeImpl;
import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model.Role;
import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model.RoleImpl;
import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model.WSDLModel;
import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model.WSDLModelHelper;
import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model.XSDModel;

/**
 * @author graj
 *
 */
public class FacadeWSDLCreator implements Serializable {
	private static final long serialVersionUID = 1L;
	
    static final String WSDL_PARNERLINK_PREFIX = "pLink";

    /**
     *
     */
    public FacadeWSDLCreator() {
    }


    /**
     * 
     * @param wsdlModel
     * @param wsdlOutputFile
     * @param providerConfiguration
     * @param partnerConfiguration
     * @param facadeConfiguration
     * 
     * @return
     * 
     * @throws WSDLException
     * @throws SAXException
     * @throws IOException
     * @throws MalformedURLException
     * @throws ParserConfigurationException
     * @throws SAXException
     * @throws TransformerConfigurationException
     * @throws TransformerException
     * @throws XmlException
     */
    public WSDLModel addToWSDL(WSDLModel wsdlModel, 
    		File wsdlOutputFile,
            ProviderConfiguration providerConfiguration, 
            PartnerConfiguration partnerConfiguration, 
            FacadeConfiguration facadeConfiguration) 
            throws WSDLException, SAXException,
            IOException, MalformedURLException, ParserConfigurationException,
            SAXException, TransformerConfigurationException, TransformerException, XmlException {
        Definition definition = wsdlModel.getDefinition();
        Service service = null;
        Port port = null;
        Binding binding = null;
        PortType portType = null;
        
        QName serviceQName = providerConfiguration.getServiceQName();
        String portName = providerConfiguration.getPortName();
        
        QName facadeServiceQName = facadeConfiguration.getFacadeServiceQName();
        String facadePortName = facadeConfiguration.getFacadePortName();
        String facadeLocationUri = facadeConfiguration.getLocationURI();
        
        String partnerLinkTypeName = partnerConfiguration.getPartnerLinkTypeName();
        QName partnerLinkQName = partnerConfiguration.getPartnerLinkQName();
        String roleName = partnerConfiguration.getRoleName();

        // ////////////////////////////
        // Find Binding and PortType
        // ////////////////////////////
        Service[] services = wsdlModel.retrieveServices();
        for (Service theService : services) {
            QName thisServiceQName = theService.getQName();
            if (serviceQName.equals(thisServiceQName)) {
                service = theService;
                port = theService.getPort(portName);
                binding = port.getBinding();
                portType = binding.getPortType();
                if((service != null)
                && (port != null)
                && (binding != null)
                && (portType != null)) {
                    break;
                }
            }
        }

        // ////////////////////////////
        // Add the new facade service
        // ////////////////////////////
        if(binding != null) {
            wsdlModel.addNewSOAPServiceForBinding(definition, facadeServiceQName,
                    facadePortName, facadeLocationUri, binding.getQName());
        }

        // ////////////////////////////
        // Add partnerLink to namespace
        // (if it is not there)
        // ////////////////////////////
        String partnerLinkPrefix = WSDL_PARNERLINK_PREFIX;
        Map<String /* prefix */, String /* namespace */> namespaceMap = null;
        namespaceMap = definition.getNamespaces();
        if (false == namespaceMap
                .containsValue(XmlConstants.WSDL_PARTNERLINK_NAMESPACE_VALUE)) {
            definition.addNamespace(partnerLinkPrefix,
                    XmlConstants.WSDL_PARTNERLINK_NAMESPACE_VALUE);
        } else {
            // get the prefix
            for (String keyPrefix : namespaceMap.keySet()) {
                String namespaceURIValue = namespaceMap.get(keyPrefix);
                if (XmlConstants.WSDL_PARTNERLINK_NAMESPACE_VALUE
                        .equals(namespaceURIValue) == true) {
                    partnerLinkPrefix = keyPrefix;
                    break;
                }
            }
        }
        // find out if partnerlink definition is available
        wsdlModel.writeWSDLToFile(definition, wsdlOutputFile.getAbsolutePath());

        // ////////////////////////////
        // Modify WSDL file to add
        // partnerLinkType
        // ////////////////////////////
        List<PartnerLinkType> partnerLinkTypeList = this
                .retrievePartnerLinkType(wsdlOutputFile);
        if((this.containsPartnerLinkTypeElement(definition) == false)
           && (this.containsPartnerLinkTypeAndRoleName(partnerLinkTypeList, partnerLinkTypeName, roleName) == false)){
            this.addPartnerLinkTypeAndRoleForPortType(wsdlOutputFile,
                    partnerLinkPrefix, partnerLinkTypeName, roleName, portType);
        }
        WSDLModel newWSDLModel = null;
        newWSDLModel = new WSDLModelHelper();
        try {
            newWSDLModel.populate(wsdlOutputFile.getAbsolutePath());
        } catch (WSDLException ex) {
            ex.printStackTrace();
        } catch (XmlException ex) {
            ex.printStackTrace();
        }
        return newWSDLModel;
    }

    /**
     *
     * @param wsdlOutputFile
     * @param partnerLinkQName
     * @param roleName
     * @return
     */
    void addPartnerLinkTypeAndRoleForPortType(File wsdlOutputFile,
            String partnerLinkPrefix,
            String partnerLinkTypeName,
            String roleName,
            PortType portType) throws IOException, ParserConfigurationException, SAXException, TransformerConfigurationException, TransformerException {

        if (wsdlOutputFile.exists() == true) {
            XMLUtils utils = new XMLUtils();
            utils.parse(wsdlOutputFile);
            Element definitionElement = utils.getRoot();
            //<partnerLinkType>
            Element partnerLinkTypeElement = utils.createElementNS(XmlConstants.WSDL_PARTNERLINK_NAMESPACE_VALUE, XmlConstants.WSDL_PARTNERLINKTYPE_KEY);
            // name=partnerLinkTypeName
            partnerLinkTypeElement.setAttribute(XmlConstants.WSDL_NAME_KEY, partnerLinkTypeName);
            // <role>
            Element roleElement = utils.createElementNS(XmlConstants.WSDL_PARTNERLINK_NAMESPACE_VALUE, XmlConstants.WSDL_ROLE_KEY);
            // name=roleName
            roleElement.setAttribute(XmlConstants.WSDL_NAME_KEY, roleName);
            // portType="tns:portTypeName"
            QName portTypeName = portType.getQName();
            if(portTypeName.getPrefix().trim().length() > 1) {
                roleElement.setAttribute(XmlConstants.WSDL_PORTTYPE_KEY, portTypeName.getPrefix().trim()+":"+portTypeName.getLocalPart());
            } else {
                roleElement.setAttribute(XmlConstants.WSDL_PORTTYPE_KEY, portTypeName.toString());
            }
            // </role>
            partnerLinkTypeElement.appendChild(roleElement);
            //</partnerLinkType>
            definitionElement.appendChild(partnerLinkTypeElement);
            utils.writeToFile(utils.getDocument(), wsdlOutputFile);
        }

    }

    /**
     *
     * @param definition
     * @return
     */
    boolean containsPartnerLinkTypeElement(Definition definition) {
        boolean result = false;
        List<ExtensibilityElement> elementsList = definition
                .getExtensibilityElements();
        for (ExtensibilityElement element : elementsList) {
            QName elementType = element.getElementType();
            if (elementType
                    .equals(XmlConstants.WSDL_PARTNERLINKTYPE_QNAME_VALUE)) {
                result = true;
                break;
            }
        }
        return result;
    }

    /**
     *
     * @param partnerLinkTypeList
     * @param partnerLinkName
     * @param roleName
     * @return
     */
    boolean containsPartnerLinkTypeAndRoleName(List<PartnerLinkType> partnerLinkTypeList, String partnerLinkName, String roleName) {
        boolean result = false;
        for(PartnerLinkType partnerLinkType : partnerLinkTypeList) {
            if(partnerLinkType.getName().equals(partnerLinkName)
                    && partnerLinkType.getRole().getName().equals(roleName)) {
                result = true;
            }
        }
        return result;
    }

    /**
     * 
     * @param partnerLinkTypeList
     * @param definition
     * @return
     */
    List<PartnerLinkType> fixupNamespaces(List<PartnerLinkType> partnerLinkTypeList, Definition definition) {
        Map<String /*prefix*/, String /*namespaceUri*/> namespaces = definition.getNamespaces();

        QName portTypeQName = null;
        for(PartnerLinkType partnerLinkType : partnerLinkTypeList) {
            portTypeQName = partnerLinkType.getRole().getPortType();
            String localpart = portTypeQName.getLocalPart();
            String prefix = "";
            if(localpart.contains(":")) {
                String arrays[] = localpart.split(":");
                if(arrays.length == 2) {
                    prefix = arrays[0];
                    localpart = arrays[1];
                }
            }
            String namespaceUri = namespaces.get(prefix);
            portTypeQName = new QName(namespaceUri, localpart, prefix);
            partnerLinkType.getRole().setPortType(portTypeQName);
        }

        return partnerLinkTypeList;
    }

    /**
     *
     * @param wsdlFileLocation
     * @return
     * @throws MalformedURLException
     * @throws ParserConfigurationException
     * @throws SAXException
     * @throws IOException
     */
    public List<PartnerLinkType> retrievePartnerLinkType(File wsdlFileLocation)
            throws MalformedURLException, ParserConfigurationException,
            SAXException, IOException {
        List<PartnerLinkType> list = new ArrayList<PartnerLinkType>();
        Document document = null;
        PartnerLinkType partnerLinkType = null;
        Role role = null;
        if (wsdlFileLocation.exists() == true) {
            XMLUtils utils = new XMLUtils();
            utils.parse(wsdlFileLocation);
            NodeList partnerLinkTypeElements = utils.getElementByTagNameNS(
                    XmlConstants.WSDL_PARTNERLINK_NAMESPACE_VALUE,
                    XmlConstants.WSDL_PARTNERLINKTYPE_KEY);
            for (int index = 0; index < partnerLinkTypeElements.getLength(); index++) {
                Element partnerLinkTypeElement = (Element) partnerLinkTypeElements
                        .item(index);
                String partnerLinkTypeName = partnerLinkTypeElement
                        .getAttribute(XmlConstants.WSDL_NAME_KEY);
                NodeList rolesElements = partnerLinkTypeElement
                        .getElementsByTagNameNS(
                                XmlConstants.WSDL_PARTNERLINK_NAMESPACE_VALUE,
                                XmlConstants.WSDL_ROLE_KEY);
                Element roleElement = (Element) rolesElements.item(0);
                String roleName = roleElement
                        .getAttribute(XmlConstants.WSDL_NAME_KEY);
                String portTypeQNameString = roleElement
                        .getAttribute(XmlConstants.WSDL_PORTTYPE_KEY);
                if ((partnerLinkTypeName != null) && (roleName != null)
                        && (portTypeQNameString != null)) {
                    role = new RoleImpl();
                    role.setName(roleName);
                    role.setPortType(QName.valueOf(portTypeQNameString));
                    partnerLinkType = new PartnerLinkTypeImpl();
                    partnerLinkType.setName(partnerLinkTypeName);
                    partnerLinkType.setRole(role);
                    list.add(partnerLinkType);
                }
            }
        }
        return list;
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        String wsdlURI = null;
        wsdlURI =
        "http://webservices.amazon.com/AWSECommerceService/AWSECommerceService.wsdl";
        //wsdlURI = System.getProperty("DRIVER_TEST_DIR") + "/aspectse/cacheGateway/CacheAspectApp/src/cache.wsdl";
        final String FACADE = "facade";
        FacadeWSDLCreator generator = null;
        WSDLModel wsdlModel = null;
        XSDModel xsdHelper = null;
        Definition definition = null;
        String locationURI = "http://localhost:28000/aspects/MyURL";
        String wsdlOutputFileName = "C:/test/myurl.wsdl";
        File testFolder = new File("C:/test");
        File wsdlOutputFile = new File(testFolder, "facade.wsdl");
        wsdlModel = new WSDLModelHelper();

        try {
        	wsdlModel.populate(wsdlURI);
            xsdHelper = wsdlModel.getXsdHelper();
            definition = wsdlModel.getDefinition();
        } catch (WSDLException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (XmlException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        File file = new File(wsdlURI);
        generator = new FacadeWSDLCreator();
        List<PartnerLinkType> list = null;

        try {
            list = generator.retrievePartnerLinkType(file);
            for (PartnerLinkType type : list) {
                System.out.println("name = " + type.getName());
                System.out.println("Role name = " + type.getRole().getName());
                System.out.println("Port Type = "
                        + type.getRole().getPortType());
                QName portTypeQName = type.getRole().getPortType();
                System.out.println("i.e., ( localpart="+portTypeQName.getLocalPart());
                System.out.println(", namespaceUri="+portTypeQName.getNamespaceURI());
                System.out.println(", prefix="+portTypeQName.getPrefix()+"}");
            }
            QName qName = new QName("namespaceURI", "localpart", "prefix");
            System.out.println(qName);

            generator.fixupNamespaces(list, definition);
            for (PartnerLinkType type : list) {
                System.out.println("name = " + type.getName());
                System.out.println("Role name = " + type.getRole().getName());
                System.out.println("Port Type = "
                        + type.getRole().getPortType());
                QName portTypeQName = type.getRole().getPortType();
                System.out.println("i.e., ( localpart="+portTypeQName.getLocalPart());
                System.out.println(", namespaceUri="+portTypeQName.getNamespaceURI());
                System.out.println(", prefix="+portTypeQName.getPrefix()+"}");
            }
        } catch (MalformedURLException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (ParserConfigurationException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (SAXException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

}
