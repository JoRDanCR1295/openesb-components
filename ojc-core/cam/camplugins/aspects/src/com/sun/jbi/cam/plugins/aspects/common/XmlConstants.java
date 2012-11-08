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
 * @(#)XmlConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.common;

import javax.xml.namespace.QName;

/**
 * @author graj
 *
 */
public final class XmlConstants {
	public static final String ASPECTMAPXML_ASPECTMAP_KEY = "aspectmap";
	public static final String ASPECTMAPXML_ASPECT_KEY = "aspect";
	public static final String ASPECTMAPXML_EXCHANGETYPE_KEY = "exchangeType";
	public static final String ASPECTMAPXML_ID_KEY = "ID";
	public static final String ASPECTMAPXML_INPUT_KEY = "input";
	public static final String ASPECTMAPXML_PARTNERLINK_KEY = "partnerLink";
	public static final String ASPECTMAPXML_ROLENAME_KEY = "roleName";
	public static final String ASPECTMAPXML_SERVICENAME_KEY = "serviceName";
	public static final String ASPECTMAPXML_PORTNAME_KEY = "portName";
	public static final String ASPECTMAPXML_PORTTYPE_KEY = "portType";
	public static final String ASPECTMAPXML_OPERATION_KEY = "operation";
	public static final String ASPECTMAPXML_MESSAGETYPE_KEY = "messageType";
	public static final String ASPECTMAPXML_FILE_KEY = "file";
    public static final String ASPECTMAPXML_TRANSFORMJBI_KEY = "transformJBI";
    public static final String ASPECTMAPXML_OUTPUT_KEY = "output";
    public static final String ASPECTMAPXML_ADVICE_KEY = "advice";
    public static final String ASPECTMAPXML_TYPE_KEY = "type";
    public static final String ASPECTMAPXML_CONFIGURATIONFILE_KEY = "configurationFile";
    public static final String ASPECTMAPXML_ORDER_KEY = "order";
    
    public static final String JBIXML_JBI_KEY = "jbi";
    public static final String JBIXML_VERSION_KEY = "version";
    public static final String JBIXML_SERVICES_KEY = "services";
    public static final String JBIXML_BINDINGCOMPONENT_KEY = "binding-component";
    public static final String JBIXML_PROVIDES_KEY = "provides";
    public static final String JBIXML_CONSUMES_KEY = "consumes";
    public static final String JBIXML_INTERFACENAME_KEY = "interface-name";
    public static final String JBIXML_SERVICENAME_KEY = "service-name";
    public static final String JBIXML_ENDPOINTNAME_KEY = "endpoint-name";
    public static final String JBIXML_LINKTYPE_KEY = "link-type";
    
    public static final String JBIXML_VERSION_VALUE = "1.0";
    public static final String JBIXML_XMLSCHEMANAMESPACE_VALUE = "http://www.w3.org/2001/XMLSchema-instance";
    public static final String JBIXML_JBISCHEMANAMESPACE_VALUE = "http://java.sun.com/xml/ns/jbi";
    public static final String JBIXML_JBISCHEMALOCATION_VALUE = "jbi.xsd";
    

    public static final String WSDL_PARTNERLINKTYPE_KEY = "partnerLinkType";
    public static final String WSDL_ROLE_KEY = "role";
    public static final String WSDL_NAME_KEY = "name";
    public static final String WSDL_PORTTYPE_KEY = "portType";
    
    public static final String WSDL_NAMESPACE_VALUE = "http://schemas.xmlsoap.org/wsdl/";
    public static final String WSDL_PARTNERLINK_NAMESPACE_VALUE = "http://docs.oasis-open.org/wsbpel/2.0/plnktype";
	public static final QName WSDL_PARTNERLINKTYPE_QNAME_VALUE = new QName(WSDL_PARTNERLINK_NAMESPACE_VALUE, WSDL_PARTNERLINKTYPE_KEY);
	public static final QName WSDL_ROLE_QNAME_VALUE = new QName(WSDL_PARTNERLINK_NAMESPACE_VALUE, WSDL_ROLE_KEY);
    
	public static final String FACADEINFO_FACADE_KEY = "facade";
	public static final String FACADEINFO_CONFIG_KEY = "config";
	public static final String FACADEINFO_PROPERTY_KEY = "property";
	public static final String FACADEINFO_NAME_KEY = "name";
	public static final String FACADEINFO_VALUE_KEY = "value";
	public static final String FACADEINFO_TARGETNAMESPACE_KEY = "targetNameSpace";
	public static final String FACADEINFO_SERVICENAME_KEY = "serviceName";
	public static final String FACADEINFO_PORTNAME_KEY = "portName";
	public static final String FACADEINFO_SOAPADDRESS_KEY = "soap:address";
	
	public static final String CATALOG_CATALOG_KEY = "catalog";
	public static final String CATALOG_SERVICES_KEY = "services";
	public static final String CATALOG_BASELOCATION_KEY = "baseLocation";
	public static final String CATALOG_SERVICE_KEY = "service";
	public static final String CATALOG_NAME_KEY = "name";
	public static final String CATALOG_SERVICEGROUPNAME_KEY = "serviceGroupName";
	public static final String CATALOG_TARGETNAMESPACE_KEY = "targetNamespace";
	public static final String CATALOG_PORT_KEY = "port";
	public static final String CATALOG_LOCATIONURI_KEY = "locationURI";
	public static final String CATALOG_PORTTYPE_KEY = "portType";
	public static final String CATALOG_FOLDERNAME_KEY = "folderName";
	public static final String CATALOG_WSDLFILES_KEY = "wsdlFiles";
	public static final String CATALOG_XSDFILES_KEY = "xsdFiles";
	public static final String CATALOG_SERVICEWSDLFILE_KEY = "serviceWSDLFile";
	
	public static final String ASPECTPOLICYGROUP_ASPECTPOLICYGROUPS_KEY = "aspectPolicyGroups";
	public static final String ASPECTPOLICYGROUP_POLICYGROUPCOLLECTION_KEY = "policyGroupCollection";
    public static final String ASPECTPOLICYGROUP_BASELOCATION_KEY = "baseLocation";
    public static final String ASPECTPOLICYGROUP_POLICYGROUP_KEY = "policyGroup";
    public static final String ASPECTPOLICYGROUP_FOLDERNAME_KEY = "folderName";
    public static final String ASPECTPOLICYGROUP_NAME_KEY = "name";
    public static final String ASPECTPOLICYGROUP_POLICYGROUPNAME_KEY = "policyGroupName";
    public static final String ASPECTPOLICYGROUP_INPUTSTRING_KEY = "inputString";
    public static final String ASPECTPOLICYGROUP_FACADESTRING_KEY = "facadeString";
    public static final String ASPECTPOLICYGROUP_ASPECTSTRING_KEY = "aspectString";
	public static final String ASPECTPOLICYGROUP_POLICYGROUPNAME_STARTSWITH_KEY = "policyGroupName";
	public static final String ASPECTPOLICYGROUP_EXPORT_OPERATION_STARTSWITH_KEY = "export_operation";
	public static final String ASPECTPOLICYGROUP_FACADE_INDEXOF_KEY = "_facade_";
	public static final String ASPECTPOLICYGROUP_EXPORT_ASP_STARTSWITH_KEY = "export_asp";
	public static final String ASPECTPOLICYGROUP_EXPORT_KEY = "export_";
	
	
}
