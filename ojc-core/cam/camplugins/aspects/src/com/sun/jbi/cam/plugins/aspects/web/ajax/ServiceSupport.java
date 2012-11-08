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
 * @(#)ServiceSupport.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.web.ajax;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import javax.wsdl.Definition;
import javax.wsdl.Import;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.XmlException;
import org.apache.xmlbeans.impl.xsd2inst.SampleXmlUtil;

import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model.WSDLModel;
import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model.WSDLModelHelper;
import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model.XSDModel;

/**
 * @author graj
 *
 */
public class ServiceSupport implements Serializable {
	private static final long serialVersionUID = 1L;

    private final static String COMMA_COMBO_SEPARATOR = ",";
    private final static String PIPE_TOKEN_SEPARATOR = "|";
    private final static String SEMICOLON_SUB_TOKEN_SEPARATOR= ";";
    
    private final static String NEW_LINE_SEPARATOR= "\n";
    
    
    WSDLModel wsdlModelHelper = null;
    XSDModel xsdModelHelper = null;
    String targetNamespace = null;
    Definition definition = null;
    QName qName = null;
    Service[] servicesList = null;
    Service service = null;
    QName serviceQName = null;
    Port port = null;
    String portName = null;
    Map portTypeMap = null;
    PortType portType = null;
    QName portTypeQName = null;
    Operation operation = null;
    String operationName = null;
    List operationsList = null;
    ExtensibilityElement element = null;
    List elements = null;
    Part[] inputPartList = null;
    Part[] outputPartList = null;
    Part part = null;
    String partName = null;
    QName elementQName = null;
    QName typeQName = null;

    /**
     * No argument Constructor
     */
    public ServiceSupport() {
        // TODO Auto-generated constructor stub
    }

    /**
     * Set it all up
     * @param wsdlUri
     * @throws WSDLException
     * @throws XmlException
     */
    public void setup(String wsdlUri) throws WSDLException, XmlException 
    {
        try {
            wsdlModelHelper = new WSDLModelHelper();
            xsdModelHelper = wsdlModelHelper.getXsdHelper();
            wsdlModelHelper.populate(wsdlUri);
            targetNamespace = wsdlModelHelper.getTargetNamespace();
            definition = wsdlModelHelper.getDefinition();
        } catch (WSDLException e) {
//            wsdlModelHelper = null;
//            xsdModelHelper = null;
            throw e;
        } catch (XmlException e) {
//            wsdlModelHelper = null;
//            xsdModelHelper = null;
            throw e;
        }
    }
    
    /**
     * Retrieve service info for a list os WSDLs
     * @param wsdlUriList
     * @return
     */
    public String retrieveServiceInformation(List<String> wsdlUriList) {
        String concatenatedString = "";
    	for(String wsdlUri : wsdlUriList) {
                String result = this.retrieveServiceInformation(wsdlUri);
                if((result != null) && (result.trim().length() > 0)) {
    		concatenatedString += result;
    		concatenatedString += PIPE_TOKEN_SEPARATOR;
                }
    	}
    	return concatenatedString;
    }

    /**
     * Retrieve the Service Information 
     * @param wsdlUri
     * @return concatenated string
     */
    public String retrieveServiceInformation(String wsdlUri) {
        String concatenatedString = "";
        try {
            setup(wsdlUri);
        } catch (WSDLException e) {
            e.printStackTrace();
            return concatenatedString;
        } catch (XmlException e) {
            e.printStackTrace();
        }
        servicesList = wsdlModelHelper.retrieveServices();
        if (servicesList != null) {
            for (int index = 0; index < servicesList.length; index++) {
                service = servicesList[index];
                if (service != null) {
                    Port[] portsList = wsdlModelHelper.retrieveServicePorts(service);
                    if(portsList != null) {
                        for (int count = 0; count < portsList.length; count++) {
                            port = portsList[count];
                            if ((port != null)
                            && (wsdlModelHelper.hasSOAPBindingPort(port) == true)) {
                                elements = port.getExtensibilityElements();
                                if(elements != null) {
                                    for(Iterator elementIterator = elements.iterator(); elementIterator.hasNext();) {
                                        element = (ExtensibilityElement) elementIterator.next();
                                        if(element != null) {
                                            
                                        } // end (element != null)
                                    } //end for elements.iterator()
                                } // end if(elements != null)
                                String locationURI = wsdlModelHelper.getSOAPLocationURI(port);
                                portType = wsdlModelHelper.getSOAPAbstractPortType(service.getQName(), port.getName());
                                Map<String, List<Import>> importMap  = wsdlModelHelper.getDefinition().getImports();
                                if (portType != null) {
                                    concatenatedString += targetNamespace;
                                    concatenatedString += SEMICOLON_SUB_TOKEN_SEPARATOR;
                                    concatenatedString += service.getQName();
                                    concatenatedString += SEMICOLON_SUB_TOKEN_SEPARATOR;
                                    concatenatedString += port.getName();
                                    concatenatedString += SEMICOLON_SUB_TOKEN_SEPARATOR;
                                    concatenatedString += portType.getQName();
                                    concatenatedString += SEMICOLON_SUB_TOKEN_SEPARATOR;
                                    concatenatedString += locationURI;
                                    concatenatedString += SEMICOLON_SUB_TOKEN_SEPARATOR;
                                    Collection<List<Import>> importListCollection = importMap.values();
                                    for(List<Import> importListElement : importListCollection) {
                                    	for(Import importElement : importListElement) {
                                    		if(importElement != null) {
	                                    		concatenatedString += importElement.getLocationURI();
	                                            concatenatedString += COMMA_COMBO_SEPARATOR;
                                    		} // end (importElement != null)
                                    	} // end for(Import
                                    } // end for(List<Import>
                                } // end (portType != null)
                            } // end if (port != null)
                        } // end for portsList
                    } // end if(portsList != null)
                } // end if (service != null)
            } // end for servicesList
        } // end if (servicesList != null)
        
        return concatenatedString;
    }
    
    /**
     * Retrieve the Operations List 
     * @param wsdlUri
     * @return concatenated string
     */
    public String retrieveOperationsList(String wsdlUri) {
        String concatenatedString = "";
        try {
            setup(wsdlUri);
        } catch (WSDLException e) {
            e.printStackTrace();
            return concatenatedString;
        } catch (XmlException e) {
            e.printStackTrace();
            return concatenatedString;
        }
        servicesList = wsdlModelHelper.retrieveServices();
        if (servicesList != null) {
            for (int index = 0; index < servicesList.length; index++) {
                service = servicesList[index];
                if (service != null) {
                    Port[] portsList = wsdlModelHelper.retrieveServicePorts(service);
                    if(portsList != null) {
                        for (int count = 0; count < portsList.length; count++) {
                            port = portsList[count];
                            if ((port != null)
                            && (wsdlModelHelper.hasSOAPBindingPort(port) == true)) {
                                elements = port.getExtensibilityElements();
                                if(elements != null) {
                                    for(Iterator elementIterator = elements.iterator(); elementIterator.hasNext();) {
                                        element = (ExtensibilityElement) elementIterator.next();
                                        if(element != null) {
                                            
                                        } // end (element != null)
                                    } //end for elements.iterator()
                                } // end if(elements != null)
                                portType = wsdlModelHelper.getSOAPAbstractPortType(service.getQName(), port.getName());
                                if (portType != null) {
                                    operationsList = portType.getOperations();
                                    for (Iterator operationsIterator = operationsList.iterator(); operationsIterator.hasNext() == true;) {
                                        operation = (Operation) operationsIterator.next();
                                        
                                        concatenatedString += targetNamespace;
                                        concatenatedString += SEMICOLON_SUB_TOKEN_SEPARATOR;
                                        concatenatedString += service.getQName();
                                        concatenatedString += SEMICOLON_SUB_TOKEN_SEPARATOR;
                                        concatenatedString += port.getName();
                                        concatenatedString += SEMICOLON_SUB_TOKEN_SEPARATOR;
                                        concatenatedString += portType.getQName();
                                        concatenatedString += PIPE_TOKEN_SEPARATOR;
                                        concatenatedString += operation.getName();
                                        concatenatedString += COMMA_COMBO_SEPARATOR;
                                        
                                    } // end for operationsIterator
                                } // end (portType != null)
                            } // end if (port != null)
                        } // end for portsList
                    } // end if(portsList != null)
                } // end if (service != null)
            } // end for servicesList
        } // end if (servicesList != null)
        
        return concatenatedString;
    }
    
    
    /**
     * Retrieve Message Types
     * @param wsdlUri
     * @param tnsServicePortPortTypeMashup
     * @param operationName
     * @return concatenated string
     */
    public String retrieveMessageTypes(String wsdlUri, String tnsServicePortPortTypeMashup, String operationName) {
        String concatenatedString = "";
        try {
            setup(wsdlUri);
        } catch (WSDLException e) {
            e.printStackTrace();
            return concatenatedString;
        } catch (XmlException e) {
            e.printStackTrace();
            return concatenatedString;
        }
        String[] tokensList = this.parseTokens(tnsServicePortPortTypeMashup,
                SEMICOLON_SUB_TOKEN_SEPARATOR);
        // targetNamespace;serviceQName;portName;portTypeQName
        if(tokensList[0] != null) {
            targetNamespace = tokensList[0];
        }
        if(tokensList[1] != null) {
            serviceQName = QName.valueOf(tokensList[1]);
        }
        if(tokensList[2] != null) {
            portName = tokensList[2];
        }
        if(tokensList[3] != null) {
            portTypeQName = QName.valueOf(tokensList[3]);
        }
        service = definition.getService(serviceQName);
        port = service.getPort(portName);
        portType = wsdlModelHelper.getSOAPAbstractPortType(serviceQName, portName);
        /*
         * Get the specified operation. Note that operation names can
         * be overloaded within a PortType. In case of overloading, the
         * names of the input and output messages can be used to further
         * refine the search.
         *
         * The search criteria will be the operation name parameter and any
         * non-null input or output message name parameters.
         *
         * To exclude the input or output message name from the search criteria,
         * specify a null value for the input or output message name parameter.
         */
        String inputName = null, outputName = null;
        operation = portType.getOperation(operationName, inputName, outputName);
        
        concatenatedString += "Service Name: ";
        concatenatedString += service.getQName();
        concatenatedString += NEW_LINE_SEPARATOR;
        concatenatedString += "Port Name: ";
        concatenatedString += port.getName();
        concatenatedString += NEW_LINE_SEPARATOR;
        concatenatedString += "PortType Name: ";
        concatenatedString += portType.getQName();
        concatenatedString += NEW_LINE_SEPARATOR;
        concatenatedString += "Operation Name: ";
        concatenatedString += operation.getName();
        concatenatedString += NEW_LINE_SEPARATOR;
        
        inputPartList = wsdlModelHelper.getInputParts(operation);
        if(inputPartList != null) {
            for (int inputIndex = 0; inputIndex < inputPartList.length; inputIndex++) {
                part = inputPartList[inputIndex];
                if(part != null) {

                    concatenatedString += "Input Part Name: ";
                    concatenatedString += part.getName();
                    concatenatedString += NEW_LINE_SEPARATOR;
                    
                    if (part.getElementName() != null) {
                        elementQName = part.getElementName();
                        
                        concatenatedString += "Input Element: ";
                        concatenatedString += elementQName;
                        concatenatedString += NEW_LINE_SEPARATOR;
                        concatenatedString += this.expandElement(elementQName);
                        concatenatedString += NEW_LINE_SEPARATOR;
                        
                    } // end if (part.getElementName() != null)
                    if (part.getTypeName() != null) {
                        concatenatedString += "Input Type: ";
                        QName typeQName = part.getTypeName();
                        SchemaType type = this.findSchemaType(typeQName);
                        if(type != null) {
                            if(type.isSimpleType() == true) {
                                concatenatedString += part.getTypeName();
                            } else {
                                concatenatedString += this.expandType(type); 
                            }
                        } else {
                            concatenatedString += part.getTypeName();
                        }
                        concatenatedString += NEW_LINE_SEPARATOR;
                        
                    } // end if (part.getTypeName() != null)
                } // end if(part != null)
            } // end for inputPartList
        } // end if(inputPartList != null)
        outputPartList = wsdlModelHelper.getOutputParts(operation);
        if(outputPartList != null) {
            for (int outputIndex = 0; outputIndex < outputPartList.length; outputIndex++) {
                part = outputPartList[outputIndex];
                if(part != null) {

                    concatenatedString += "Output Part Name: ";
                    concatenatedString += part.getName();
                    concatenatedString += NEW_LINE_SEPARATOR;
                    
                    if (part.getElementName() != null) {
                        elementQName = part.getElementName();
                        
                        concatenatedString += "Output Element: ";
                        concatenatedString += elementQName;
                        concatenatedString += NEW_LINE_SEPARATOR;
                        concatenatedString += this.expandElement(elementQName);
                        concatenatedString += NEW_LINE_SEPARATOR;
                        
                    } // end if (part.getElementName() != null)
                    if (part.getTypeName() != null) {
                        concatenatedString += "Output Type Name: ";
                        QName typeQName = part.getTypeName();
                        SchemaType type = this.findSchemaType(typeQName);
                        if(type != null) {
                            if(type.isSimpleType() == true) {
                                concatenatedString += part.getTypeName();
                            } else {
                                concatenatedString += this.expandType(type); 
                            }
                        } else {
                            concatenatedString += part.getTypeName();
                        }
                        concatenatedString += NEW_LINE_SEPARATOR;
                        
                    } // end if (part.getTypeName() != null)
                } // end if(part != null)
            } // end for outputPartList
        } // end if(outputPartList != null)
        
        return concatenatedString;
    }
    
    /**
     * Find the schema type 
     * @param typeQName
     * @return SchemaType
     */
    SchemaType findSchemaType(QName typeQName) {
        SchemaType schemaType = null;
        if(typeQName == null) {
            return schemaType;
        }
        
        SchemaType[] schemaTypeList = this.xsdModelHelper.getSchemaTypeSystem().globalTypes();
        if(schemaTypeList != null) {
            for(int index = 0; index < schemaTypeList.length; index++) {
                if(schemaTypeList[index] != null) {
                    if(typeQName.equals(schemaTypeList[index].getName()) == true) {
                        schemaType = schemaTypeList[index];
                        break;
                    }
                }
            }
        }
        if(schemaType == null) {
            schemaType = this.xsdModelHelper.getSchemaTypeSystem().findType(typeQName);
        }
        return schemaType;
    }
    
    /**
     * Expand the Element
     * @param elementQName
     * @return concatenated string
     */
    String expandElement(QName elementQName) {
        String concatenatedString = "";
        SchemaType schemaType = this.xsdModelHelper.getExpandedSchemaType(elementQName);
        if(schemaType == null) {
            return concatenatedString;
        }
        
        concatenatedString += "Sample Message for :"+elementQName;
        concatenatedString += NEW_LINE_SEPARATOR;
        concatenatedString += SampleXmlUtil.createSampleForType(schemaType);
        concatenatedString += NEW_LINE_SEPARATOR;
        
        return concatenatedString;
    }

    /**
     * Expand the type
     * @param type
     * @return concatenated string
     */
    String expandType(SchemaType type) {
        String concatenatedString = "";
        if(type == null) {
            return concatenatedString;
        }
        
        concatenatedString += "Sample Message for :"+type.getName();
        concatenatedString += NEW_LINE_SEPARATOR;
        concatenatedString += SampleXmlUtil.createSampleForType(type);
        concatenatedString += NEW_LINE_SEPARATOR;
        
        return concatenatedString;
    }
    
    /**
     * Parse the String tokens.
     * @param the String to parse
     * @param delimiters
     * @return array of Strings, one for each word in subtoken.
     */
    String[] parseTokens(String inputString, String delimiters) {
        List<String> tokensList = new ArrayList<String>();
        String token = null;
        boolean returnDelimiters = false;
        StringTokenizer parser = new StringTokenizer(inputString,
                delimiters,
                returnDelimiters);
        while ( true == parser.hasMoreTokens() ) {
            token = parser.nextToken(delimiters);
            if(token != null) {
                tokensList.add(token);
            } else {
                tokensList.add("");
            }
        }
        return toArray(tokensList, String.class);
    }

    /**
     * We can now use this to produce lists of ints or Strings:
     * List<Integer> ints = Lists.toList(1, 2, 3);
     * List<String> names = Lists.toList("Gopalan", "Suresh", "Raj");
     * 
     * @param <T>
     * @param arr
     * @return
     */
    public static <T> List<T> toList(T... array) {
        List<T> list = new ArrayList<T>();
        for (T arrayElement : array) {
        	list.add(arrayElement);
        }
        return list;
    }
    
    /**
     * Convert Array Collection to String array
     * @param <T>
     * @param collection
     * @param componentType
     * @return
     */
    @SuppressWarnings("unchecked")
    static public <T> T[] toArray(Collection<T> collection,
            Class<T> componentType) {
        // unchecked cast
        T[] array = (T[]) java.lang.reflect.Array.newInstance(componentType,
                collection.size());
        int index = 0;
        for (T value : collection) {
            array[index++] = value;
        }
        return array;
    }
    
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        String concatenatedString = "";
        ServiceSupport support = new ServiceSupport();
        String wsdlUri = null;
        String tnsServicePortPortTypeMashup = null;
        String operationName = null;
        List<String> wsdlUriList = new ArrayList<String>();
        wsdlUriList.add("http://webservices.amazon.com/AWSECommerceService/AWSECommerceService.wsdl");
        wsdlUriList.add("http://www.weather.gov/forecasts/xml/SOAP_server/ndfdXMLserver.php?wsdl");
        wsdlUriList.add("http://schemas.monster.com/current/wsdl/MonsterBusinessGateway.wsdl");
        wsdlUriList.add("http://www.xmethods.net/sd/2001/CurrencyExchangeService.wsdl");
        wsdlUriList.add("http://terraservice.net/TerraService.asmx?WSDL");
        String result = support.retrieveServiceInformation(wsdlUriList);
        System.out.println(result);
        
        //wsdlUri = "http://webservices.amazon.com/AWSECommerceService/AWSECommerceService.wsdl";
//        wsdlUri = "http://www.weather.gov/forecasts/xml/SOAP_server/ndfdXMLserver.php?wsdl";
//        concatenatedString = support.retrieveOperationsList(wsdlUri);
//        System.out.println(concatenatedString);
        //tnsServicePortPortTypeMashup = "http://webservices.amazon.com/AWSECommerceService/2007-02-22;{http://webservices.amazon.com/AWSECommerceService/2007-02-22}AWSECommerceService;AWSECommerceServicePort;{http://webservices.amazon.com/AWSECommerceService/2007-02-22}AWSECommerceServicePortType";
//        tnsServicePortPortTypeMashup = "http://www.weather.gov/forecasts/xml/DWMLgen/wsdl/ndfdXML.wsdl;{http://www.weather.gov/forecasts/xml/DWMLgen/wsdl/ndfdXML.wsdl}ndfdXML;ndfdXMLPort;{http://www.weather.gov/forecasts/xml/DWMLgen/wsdl/ndfdXML.wsdl}ndfdXMLPortType";
        //operationName = "Help";
//        operationName = "NDFDgen";
//        concatenatedString = support.retrieveMessageTypes(wsdlUri, tnsServicePortPortTypeMashup, operationName);
//        System.out.println(concatenatedString);
    }
    /*
        // CurrencyExchangeService
        // wsdlURI =
        // "http://www.xmethods.net/sd/2001/CurrencyExchangeService.wsdl";
        // net.xmethods.services.stockquote.StockQuote
        // wsdlURI
        // ="http://services.xmethods.net/soap/urn:xmethods-delayed-quotes.wsdl";
        // DOTSValidateCanada
        // wsdlURI =
        // "http://ws2.serviceobjects.net/avca/ValidateCanada.asmx?WSDL";
        // wsdlURI =
        // System.getProperty("ALASKA_ROOT") + "/jbi/runtime/Sun/AppServer/domains/domain1/applications/j2ee-modules/eManager/deploy/consuming/0F000000-46AF4899080100-0A121528-01-GovernancePolicyGroup/unit/consuming.wsdl";
        // HouseofDev
        // wsdlURI = "http://ws.houseofdev.com/cfcs/ws.cfc?wsdl";
        // ImageExtractor
        // wsdlURI =
        // "http://www.atomic-x.com/xmlservices/imageextractorws.asmx?wsdl";
        // Financial Services
        // wsdlURI = "http://www.xignite.com/xRealTime.asmx?WSDL";
        // Google Search
        // wsdlURI = "http://api.google.com/GoogleSearch.wsdl";
        // Amazon WebService
        wsdlURI = "http://webservices.amazon.com/AWSECommerceService/AWSECommerceService.wsdl";
        // Amazon AWS Alexa WebService
        // wsdlURI = "http://aws-beta.amazon.com/AWSAlexa/AWSAlexa.wsdl";
        // Amazon Simple Queue Service
        // wsdlURI =
        // "http://webservices.amazon.com/AWSSimpleQueueService/AWSSimpleQueueService.wsdl";
        // StrikeIron Address Verification Premium Web Service - US and Canada
        // wsdlURI = "http://ws.strikeiron.com/DataEnhancement?WSDL";
        // StrikeIron Address Verification Premium Web Service - Global
        // wsdlURI = "http://ws.strikeiron.com/GlobalAddressVerification?WSDL";
        // StrikeIron Address Verification Premium Web Service - France
        // wsdlURI = "http://ws.strikeiron.com/FrenchAddressVerification?WSDL";
        // StrikeIron Address Verification Premium Web Service - UK
        // wsdlURI = "http://ws.strikeiron.com/UKAddressVerification?WSDL";
        // StrikeIron Address Verification Premium Web Service - India
        // wsdlURI = "http://ws.strikeiron.com/IndianAddressVerification?WSDL";
        // StrikeIron Address Verification Premium Web Service - Email
        // Verification
        // wsdlURI = "http://ws.strikeiron.com/EmailVerify?WSDL";
        // Paypal Development and Test sandbox API
        // wsdlURI = "http://www.sandbox.paypal.com/wsdl/PayPalSvc.wsdl";
        // eBay WSDL
        // wsdlURI =
        // "http://developer.ebay.com/webservices/latest/eBaySvc.wsdl";
        // FedEx WSDL
        // wsdlURI = "http://www.xmethods.com/sd/FedExTrackerService.wsdl";
        // Translate IP addresses to city, state, country.
        // wsdlURI = "http://ws2.serviceobjects.net/gpp/GeoPinpoint.asmx?WSDL";
        // GeoPhone allows you to lookup name and postal address information for
        // virtually every U.S. telephone number.
        // wsdlURI = "http://ws2.serviceobjects.net/gp/GeoPhone.asmx?WSDL";
        // returns bank office locations for a given zip and mile radius
        // wsdlURI =
        // "http://www.hosca.com/webservices/bankfinder/bankfinder.asmx?WSDL";
        // This web service allows you to validate partial United States mailing
        // addresses against the U.S. Postal Service database. You can enter
        // street, city, and state information and get various return values
        // including: five-digit standard ZIP code (NNNN), ZIP+
        // wsdlURI =
        // "http://webservices.eraserver.net/zipcoderesolver/zipcoderesolver.asmx?wsdl";
        // Converts Section, Township, Range to Lat Long
        // wsdlURI =
        // "http://sylvanmaps.net/SectionTownshipCalculatorWebService/SectionTownshipToLatLong.asmx?WSDL";
        // Monster WSDL
        // wsdlURI =
        // "http://schemas.monster.com/current/wsdl/MonsterBusinessGateway.wsdl";
        // wsdlURI =
        // "http://schemas.monster.com/BGW3.4/WSDL/MonsterBusinessGateway.wsdl";
        // wsdlURI =
        // "http://schemas.monster.com/BGW3.4/WSDL/MonsterBusinessGateway-wsi.wsdl";
        // CA Traffic Service
        // wsdlURI = "http://www.xmethods.net/sd/2001/CATrafficService.wsdl";
        // Simple Autoloan calculator
        // wsdlURI = "http://upload.eraserver.net/circle24/autoloan.asmx?wsdl";
        // Yahoo Search WSDL simulated
        // wsdlURI =
        // "http://www.pacificspirit.com/Authoring/wsdl/YahooV1Search.wsdl";
        // DHL WSDL
        // wsdlURI = "http://dhl-usa.expediteship.com/DHLShipment.asmx?WSDL";
        // wsdlURI = "http://uddi.org/wsdl/uddi_api_v3_portType.wsdl";
        // wsdlURI =
        // "http://egee-jra1-wm.mi.infn.it/egee-jra1-wm/allocation/wsdl/agreementService.wsdl";

     */ 

}
