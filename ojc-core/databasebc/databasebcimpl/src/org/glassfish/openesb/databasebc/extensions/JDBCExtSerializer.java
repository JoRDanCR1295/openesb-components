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

package org.glassfish.openesb.databasebc.extensions;

import java.io.PrintWriter;
import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Map;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
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

import com.sun.jbi.internationalization.Messages;

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

    // Pattern for finding application variable tokens
    private static final String ENV_VAR_REGEX = "\\$\\{([a-zA-Z0-9\\.\\-\\_^\\{\\}]+)\\}";
    private static final Pattern mPattern = Pattern.compile(ENV_VAR_REGEX);

    // environment variable configurations
    protected final Map<String, String[]> mEnvVariableMap = new HashMap<String, String[]>();

    private static final Messages mMessages = Messages.getMessages(JDBCExtSerializer.class);

    public JDBCExtSerializer() {
    }

	/** Creates a new instance of JDBCExtSerializer */
    public JDBCExtSerializer(Map<String, String[]> envVariableMap) {
        this();
        mEnvVariableMap.putAll(envVariableMap);
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

        registry.registerSerializer(BindingInput.class,
            JDBCConstants.QNAME_SP_OPERATION_INPUT, this);
        registry.registerDeserializer(BindingInput.class,
            JDBCConstants.QNAME_SP_OPERATION_INPUT, this);
        registry.mapExtensionTypes(BindingInput.class,
            JDBCConstants.QNAME_SP_OPERATION_INPUT, SPOperationInput.class);
        registry.registerSerializer(BindingOutput.class,
            JDBCConstants.QNAME_SP_OPERATION_OUTPUT, this);
        registry.registerDeserializer(BindingOutput.class,
            JDBCConstants.QNAME_SP_OPERATION_OUTPUT, this);
        registry.mapExtensionTypes(BindingOutput.class,
            JDBCConstants.QNAME_SP_OPERATION_OUTPUT, SPOperationOutput.class);

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
            final String operationType = getAttrAndResolveEnvVar(el,
                    JDBCOperationInput.ATTR_OPERATION_TYPE);

            if (operationType != null) {
                input.setOperationType(operationType);
            }

            final String numberOfRecords = getAttrAndResolveEnvVar(el,
                    JDBCOperationInput.ATTR_NUMBER_OF_RECORDS);

            if ((numberOfRecords == null) || (numberOfRecords.equals("")) ) {
                input.setNumberOfRecords(-1);
            } else {
                input.setNumberOfRecords(Integer.parseInt(numberOfRecords));
            }

            input.setParamOrder(getAttrAndResolveEnvVar(el,
                    JDBCOperationInput.ATTR_PARAM_ORDER));
            input.setSql(getAttrAndResolveEnvVar(el, JDBCOperationInput.ATTR_SQL));
            input.setTableName(getAttrAndResolveEnvVar(el,
                    JDBCOperationInput.TABLE_NAME));
            input.setPKName(getAttrAndResolveEnvVar(el, JDBCOperationInput.PK_NAME));
            input.setMarkColumnName(getAttrAndResolveEnvVar(el,
                    JDBCOperationInput.MARK_COLUMN_NAME));
            input.setMoveRowToTableName(getAttrAndResolveEnvVar(el,
                    JDBCOperationInput.MOVE_TABLE_NAME));
            input.setPollingPostProcessing(getAttrAndResolveEnvVar(el,
                    JDBCOperationInput.POLLING_POST_PROCESS));
            input.setMarkColumnValue(getAttrAndResolveEnvVar(el,
                    JDBCOperationInput.MARK_COLUMN_VALUE));
            input.setTransaction(getAttrAndResolveEnvVar(el,
                    JDBCOperationInput.TRANSACTION));
            /*
	* Modified by Logicoy for [ task #20 ] DB BC - Insert statement should return primary key auto-increment value inserted
	*/
            input.setGeneratedKey(getAttrAndResolveEnvVar(el,
                    JDBCOperationInput.ATTR_GENERATED_KEY));
            

			final String pollmilliseconds = getAttrAndResolveEnvVar(el,
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

            final String returnPartName = getAttrAndResolveEnvVar(el,
                    JDBCOperationOutput.ATTR_RETURN_PART_NAME);

            if (returnPartName != null) {
                output.setReturnPartName(returnPartName);
            }

            returnValue = output;
        } else if (JDBCConstants.QNAME_SP_OPERATION_INPUT.equals(elementType)) {
            final SPOperationInput input = new SPOperationInput();

            final String operationType = DOMUtils.getAttribute(el,
                    SPOperationInput.ATTR_OPERATION_TYPE);

            if (operationType != null) {
                input.setOperationType(operationType);
            }

            input.setProcedureName(DOMUtils.getAttribute(el, SPOperationInput.ATTR_SPNAME));
            Logger.getAnonymousLogger().log(Level.INFO, "The execString is:" + SPOperationInput.ATTR_SP_EXEC_STRING);
            input.setExecutionString(DOMUtils.getAttribute(el, SPOperationInput.ATTR_SP_EXEC_STRING));
            Logger.getAnonymousLogger().log(Level.INFO, "The value of execString is:" + DOMUtils.getAttribute(el, SPOperationInput.ATTR_SP_EXEC_STRING));
            input.setTransaction(DOMUtils.getAttribute(el, SPOperationInput.TRANSACTION));
             input.setSPInput(input);
            returnValue = input;
        } else if (JDBCConstants.QNAME_SP_OPERATION_OUTPUT.equals(elementType)) {
            final SPOperationOutput output = new SPOperationOutput();

            final String returnPartName = DOMUtils.getAttribute(el,
                    SPOperationOutput.ATTR_RETURN_PART_NAME);

            if (returnPartName != null) {
                output.setReturnPartName(returnPartName);
            }

            returnValue = output;
        
        } else if (JDBCConstants.QNAME_ADDRESS.equals(elementType)) {
            final JDBCAddress jdbcAddress = new JDBCAddress();

            final String jndiName = getAttrAndResolveEnvVar(el,
                    JDBCAddress.ATTR_JNDI_NAME);
			final String driver = getAttrAndResolveEnvVar(el,
					JDBCAddress.ATTR_DRIVER_NAME);
			final String url = getAttrAndResolveEnvVar(el,
                    JDBCAddress.ATTR_URL_NAME);
			final String user = getAttrAndResolveEnvVar(el,
                    JDBCAddress.ATTR_USER_NAME);
			final String password = getAttrAndResolveEnvVar(el,
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

    public Map<String, String[]> getEnvVariableMap() {
        return Collections.unmodifiableMap(mEnvVariableMap);
    }

	protected Object[] getEnvVariableNames(String attrName, String attrVal)
			throws Exception {
		String tokenName = null;
		Matcher m = mPattern.matcher(attrVal);
		Vector refs = new Vector();
		while (m.find()) {
			tokenName = m.group(1);
			if (tokenName == null || tokenName.trim().length() == 0) {
				throw new Exception(mMessages.getString("DBBC-E001001.Invalid_Token_Name", tokenName));
			}
			refs.add(tokenName);
		}

		if (attrVal.indexOf("${}") >= 0) {
			throw new Exception(mMessages.getString(
													"DBBC-E001002.Invalid_Empty_Token_Name", 
													new Object[] { attrVal,
													attrName }));
		}

		return refs.toArray();
	}

	protected String getAttrAndResolveEnvVar(Element el, String attrName)
			throws WSDLException {
		String attrVal = DOMUtils.getAttribute(el, attrName);
		if (attrVal != null) {
			try {
				if (hasMigrationEnvVarRef(attrVal)) {
					// attribute contains env var reference(s)
					String token = attrVal;
					Object[] vars = getEnvVariableNames(attrName, attrVal);
					if (vars != null) {
						for (int i = 0; i < vars.length; i++) {
							String[] varDesc = (String[]) mEnvVariableMap
									.get(vars[i]);
							if (varDesc == null || varDesc.length != 2) {
								throw new WSDLException(
										"INVALID_WSDL",
										mMessages.getString(
															"DBBC-E001003.Invalid_Env_Var_Ref_No_Def",
															new Object[] { vars[i],
															attrVal,
															attrName }));
							} else {
								// check if the de-referenced value has ${ in it
								String varVal = varDesc[0];
								if (varVal == null) {
									throw new WSDLException(
											"INVALID_WSDL",
											mMessages.getString(
																"DBBC-E001004.Invalid_Env_Var_Value_Null",
																new Object[] {
																vars[i],
																attrName }));
								}
								if (varVal.indexOf("${") >= 0) {
									throw new WSDLException(
											"INVALID_WSDL",
											mMessages.getString(
																"DBBC-E001005.Invalid_Var_Value_Contains_Var_Ref",
																new Object[] {
																attrName,
																attrVal,
																vars[i],
																varVal }));
								}
								attrVal = attrVal.replace("${" + vars[i] + "}",
										varVal);
							}
						}
					}
				}
			} catch (WSDLException e) {
				throw e;
			} catch (Exception e) {
				throw new WSDLException("INVALID_WSDL", e.getMessage());
			}
		}
		return attrVal;
	}

    protected boolean hasMigrationEnvVarRef(String attrVal) throws Exception {
        return mPattern.matcher(attrVal).find();
    }
}
