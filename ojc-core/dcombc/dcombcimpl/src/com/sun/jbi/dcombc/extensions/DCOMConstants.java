/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/

package com.sun.jbi.dcombc.extensions;

import javax.xml.namespace.QName;

import com.ibm.wsdl.Constants;

/**
 * 
 * @author Chandrakanth Belde
 */
public class DCOMConstants {
	/**
	 *
	 */
    public DCOMConstants() {
		//
    }

    // Namespaces
    public static final String NS_URI_DCOM = "http://schemas.sun.com/jbi/wsdl-extensions/dcom/";

    // Local element names
    public static final String ELEM_ADDRESS = "address";

    public static final String ELEM_MESSAGE = "message";

    // Qualified element names
    public static final QName QNAME_BINDING = new QName(NS_URI_DCOM, Constants.ELEM_BINDING);

    public static final QName QNAME_OPERATION = new QName(NS_URI_DCOM, Constants.ELEM_OPERATION);

    public static final QName QNAME_ADDRESS = new QName(NS_URI_DCOM, ELEM_ADDRESS);

    public static final QName QNAME_MESSAGE = new QName(NS_URI_DCOM, ELEM_MESSAGE);

    // Transaction Support
    public static final String NO_TRANSACTION = "NoTransaction";

    public static final String XA_TRANSACTION = "XATransaction";
}
