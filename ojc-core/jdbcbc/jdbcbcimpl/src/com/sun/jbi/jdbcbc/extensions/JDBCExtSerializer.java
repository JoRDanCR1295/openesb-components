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
 * @(#)JDBCExtSerializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc.extensions;

import java.io.PrintWriter;
import java.io.Serializable;
import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.xml.namespace.QName;
import org.w3c.dom.Element;
import com.ibm.wsdl.util.xml.DOMUtils;


/**
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class JDBCExtSerializer implements ExtensionSerializer,
    ExtensionDeserializer, Serializable {
    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/** Creates a new instance of JDBCExtSerializer */
    public JDBCExtSerializer() {
    }

    /**
     *
     * @param registry
     */
    protected void registerSerializer(final ExtensionRegistry registry) {
        registry.registerSerializer(Binding.class, JDBCConstants.QNAME_BINDING,
            this);
        registry.registerDeserializer(Binding.class,
            JDBCConstants.QNAME_BINDING, this);
        registry.mapExtensionTypes(Binding.class, JDBCConstants.QNAME_BINDING,
            JDBCBinding.class);
        registry.registerSerializer(BindingOperation.class,
            JDBCConstants.QNAME_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class,
            JDBCConstants.QNAME_OPERATION, this);
        registry.mapExtensionTypes(BindingOperation.class,
            JDBCConstants.QNAME_OPERATION, JDBCOperation.class);

        registry.registerSerializer(BindingInput.class,
            JDBCConstants.QNAME_OPERATION_INPUT, this);
        registry.registerDeserializer(BindingInput.class,
            JDBCConstants.QNAME_OPERATION_INPUT, this);
        registry.mapExtensionTypes(BindingInput.class,
            JDBCConstants.QNAME_OPERATION_INPUT, JDBCOperationInput.class);
        registry.registerSerializer(BindingOutput.class,
            JDBCConstants.QNAME_OPERATION_OUTPUT, this);
        registry.registerDeserializer(BindingOutput.class,
            JDBCConstants.QNAME_OPERATION_OUTPUT, this);
        registry.mapExtensionTypes(BindingOutput.class,
            JDBCConstants.QNAME_OPERATION_OUTPUT, JDBCOperationOutput.class);

        registry.registerSerializer(Port.class, JDBCConstants.QNAME_ADDRESS,
            this);
        registry.registerDeserializer(Port.class, JDBCConstants.QNAME_ADDRESS,
            this);
        registry.mapExtensionTypes(Port.class, JDBCConstants.QNAME_ADDRESS,
            JDBCAddress.class);
    }

    /**
     * @param parentType
     * @param elementType
     * @param extension
     * @param pw
     * @param def
     * @param extReg
     * @throws WSDLException
     */
    //@Override
    public void marshall(final Class parentType, final QName elementType,
        final ExtensibilityElement extension, final PrintWriter pw, final Definition def,
        final ExtensionRegistry extReg) throws WSDLException {


    }

    /**
     * @param parentType
     * @param elementType
     * @param el
     * @param def
     * @param extReg
     * @return 
     * @throws WSDLException
     */
    //@Override
    public ExtensibilityElement unmarshall(final Class parentType, final QName elementType,
        final Element el, final Definition def, final ExtensionRegistry extReg)
        throws WSDLException {
        ExtensibilityElement returnValue = null;

        if (JDBCConstants.QNAME_BINDING.equals(elementType)) {
            final JDBCBinding jdbcBinding = new JDBCBinding();
            returnValue = jdbcBinding;
        } else if (JDBCConstants.QNAME_OPERATION.equals(elementType)) {
            final JDBCOperation jdbcOperation = new JDBCOperation();
            returnValue = jdbcOperation;
        } else if (JDBCConstants.QNAME_OPERATION_INPUT.equals(elementType)) {
            final JDBCOperationInput input = new JDBCOperationInput();

            //	JDBCSql jdbcSql = new JDBCSql();
            final String operationType = DOMUtils.getAttribute(el,
                    JDBCOperationInput.ATTR_OPERATION_TYPE);

            if (operationType != null) {
                input.setOperationType(operationType);
            }

            final String numberOfRecords = DOMUtils.getAttribute(el,
                    JDBCOperationInput.ATTR_NUMBER_OF_RECORDS);

            if ((numberOfRecords == null) || (numberOfRecords.equals("")) ) {
                input.setNumberOfRecords(-1);
            } else {
                input.setNumberOfRecords(Integer.parseInt(numberOfRecords));
            }

            input.setParamOrder(DOMUtils.getAttribute(el,
                    JDBCOperationInput.ATTR_PARAM_ORDER));
            input.setSql(DOMUtils.getAttribute(el, JDBCOperationInput.ATTR_SQL));
            input.setTableName(DOMUtils.getAttribute(el,
                    JDBCOperationInput.TABLE_NAME));
            input.setPKName(DOMUtils.getAttribute(el, JDBCOperationInput.PK_NAME));
            input.setMarkColumnName(DOMUtils.getAttribute(el,
                    JDBCOperationInput.MARK_COLUMN_NAME));
            input.setMoveRowToTableName(DOMUtils.getAttribute(el,
                    JDBCOperationInput.MOVE_TABLE_NAME));
            input.setPollingPostProcessing(DOMUtils.getAttribute(el,
                    JDBCOperationInput.POLLING_POST_PROCESS));
            input.setMarkColumnValue(DOMUtils.getAttribute(el,
                    JDBCOperationInput.MARK_COLUMN_VALUE));
            input.setTransaction(DOMUtils.getAttribute(el,
                    JDBCOperationInput.TRANSACTION));

			final String pollmilliseconds = DOMUtils.getAttribute(el,
                    JDBCOperationInput.POLLMILLISECONDS);

            if ((pollmilliseconds == null) || (pollmilliseconds.equals("")) ) {
                input.setPollMilliSeconds(10000);
            } else {
                input.setPollMilliSeconds(Integer.parseInt(pollmilliseconds));
            }
			
			input.setJDBCSql(input);

            //	unmarshallJDBCInpputChildren(el, input);
            returnValue = input;
        } else if (JDBCConstants.QNAME_OPERATION_OUTPUT.equals(elementType)) {
            final JDBCOperationOutput output = new JDBCOperationOutput();

            final String returnPartName = DOMUtils.getAttribute(el,
                    JDBCOperationOutput.ATTR_RETURN_PART_NAME);

            if (returnPartName != null) {
                output.setReturnPartName(returnPartName);
            }

            returnValue = output;
        } else if (JDBCConstants.QNAME_ADDRESS.equals(elementType)) {
            final JDBCAddress jdbcAddress = new JDBCAddress();

            final String jndiName = DOMUtils.getAttribute(el,
                    JDBCAddress.ATTR_JNDI_NAME);
			final String driver = DOMUtils.getAttribute(el,
					JDBCAddress.ATTR_DRIVER_NAME);
			final String url = DOMUtils.getAttribute(el,
                    JDBCAddress.ATTR_URL_NAME);
			final String user = DOMUtils.getAttribute(el,
                    JDBCAddress.ATTR_USER_NAME);
			final String password = DOMUtils.getAttribute(el,
                    JDBCAddress.ATTR_PASSWORD_NAME);

            if (jndiName != null) {
                jdbcAddress.setJndiName(jndiName);
            }
			if (driver != null) {
                jdbcAddress.setDriverClass(driver);
            }
			if (url != null) {
                jdbcAddress.setDBUrl(url);
            }
			if (user != null) {
                jdbcAddress.setUserName(user);
            }
			if (password != null) {
                jdbcAddress.setPassword(password);
            }
            returnValue = jdbcAddress;
        }

        return returnValue;
    }

    /**
     *
     * @param e1
     * @param input
     */

    /*        public void unmarshallJDBCInpputChildren(Element e1,
                            JDBCOperationInput input) {
                    NodeList childNodes = e1.getChildNodes();
                    JDBCSql mJdbcSql = new JDBCSql();
                    Element jdbcEle = null;
                    for (int i = 0; i < childNodes.getLength(); i++) {
                            Node node = childNodes.item(i);
                            if (node instanceof Element) {
                                    Element elem = (Element) node;
                                    String localName = elem.getLocalName();
                                    if (localName.equals(JDBCSql.ATTR_SQL)) {
                                            jdbcEle = elem;
                                            break;
                                    }
                            }
                    }
                    mJdbcSql.setParamOrder(DOMUtils.getAttribute(jdbcEle,
                                    JDBCSql.ATTR_PARAM_ORDER));
                    mJdbcSql.setSql(DOMUtils.getAttribute(jdbcEle, JDBCSql.ATTR_SQL));
                    mJdbcSql.setTableName(DOMUtils.getAttribute(jdbcEle, JDBCSql.TABLE_NAME));
                    mJdbcSql.setPKName(DOMUtils.getAttribute(jdbcEle, JDBCSql.PK_NAME));
                    mJdbcSql.setMarkColumnName(DOMUtils.getAttribute(jdbcEle, JDBCSql.MARK_COLUMN_NAME));
                    mJdbcSql.setMoveRowToTableName(DOMUtils.getAttribute(jdbcEle, JDBCSql.MOVE_TABLE_NAME));
                    mJdbcSql.setPollingPostProcessing(DOMUtils.getAttribute(jdbcEle, JDBCSql.POLLING_POST_PROCESS));
                    mJdbcSql.setMarkColumnValue(DOMUtils.getAttribute(jdbcEle, JDBCSql.MARK_COLUMN_VALUE));
                    mJdbcSql.setTransaction(DOMUtils.getAttribute(jdbcEle, JDBCSql.TRANSACTION));
    
                    input.setJDBCSql(mJdbcSql);
            }
            */
}
