package net.openesb.jbi.restbc.jbiadapter.wsdl;

import javax.xml.namespace.QName;

import com.ibm.wsdl.Constants;

/**
 * RestConstants.java
 *
 * @author Edward Chou
 */
public class RestConstants {
    
    public RestConstants() {
    }
    
    // Namespaces
    public static final String NS_URI_REST = "http://schemas.sun.com/jbi/wsdl-extensions/rest/";

    // Local element names
    public static final String ELEM_ADDRESS = "address";
    
    // Qualified element names
    public static final QName QNAME_BINDING = new QName(NS_URI_REST, Constants.ELEM_BINDING);
    public static final QName QNAME_OPERATION = new QName(NS_URI_REST, Constants.ELEM_OPERATION);
    public static final QName QNAME_ADDRESS = new QName(NS_URI_REST, ELEM_ADDRESS);
    
}
