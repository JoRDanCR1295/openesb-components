/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: ExtConstants.java,v 1.1 2008/01/20 16:40:07 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk.model;

import javax.xml.namespace.QName;

public interface ExtConstants {
    public static final String NS_URI_WSDL = "http://schemas.xmlsoap.org/wsdl/";
    public static final String NS_URI_XMLNS = "http://www.w3.org/2000/xmlns/";
    /** wsdl extension namespace processed by this wsdl extension model */
    public static final String NS_URI = "http://java.sun.com/jbi/wsdl-extensions/sample/asterisk-bc/";
    public static final String NS_DEF_PREFIX = "asteriskbc";
    // wsdl extension element names.
    public static final String EL_BINDING_EXT = "binding";
    public static final String EL_OPERATION_EXT = "operation";
    public static final String EL_INPUT_EXT = "input";
    public static final String EL_OUTPUT_EXT = "output";
    public static final String EL_FAULT_EXT = "fault";
    public static final String EL_PORT_EXT = "address";
    
    // Qualified wsdl extension element names.
    public static final QName QN_BINDING_EXT = new QName(NS_URI, EL_BINDING_EXT);
    public static final QName QN_OPERATION_EXT = new QName(NS_URI, EL_OPERATION_EXT);
    public static final QName QN_INPUT_EXT = new QName(NS_URI, EL_INPUT_EXT);
    public static final QName QN_OUTPUT_EXT = new QName(NS_URI, EL_OUTPUT_EXT);
    public static final QName QN_FAULT_EXT = new QName(NS_URI, EL_FAULT_EXT);
    public static final QName QN_PORT_EXT = new QName(NS_URI, EL_PORT_EXT);
    
    //TODO: define any additional extension element attribute names here.
    // wsdl extension elements attribute names.
    public static final String ATTR_EVENTTYPES = "EventTypes";
    public static final String ATTR_ACTION = "action";
    public static final String ATTR_ADDRESS = "address";
    public static final String ATTR_USERNAME = "username";
    public static final String ATTR_PASSWORD = "password";
    public static final String ATTR_PORT = "port";
    
}
