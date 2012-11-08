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

package com.sun.jbi.imsbc.extensions;

import java.io.Serializable;
import java.util.Collections;
import java.io.PrintWriter;
import java.text.MessageFormat;
import java.util.Map;
import java.util.Vector;
import java.util.HashMap;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.Port;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOutput;
import javax.wsdl.BindingOperation;
import javax.xml.namespace.QName;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.util.xml.DOMUtils;

import org.w3c.dom.Element;

import com.sun.jbi.imsbc.IMSException;
import com.sun.jbi.internationalization.Messages;

/**
 * @author Sun Microsystems
 */
public class IMSExtSerializer implements ExtensionSerializer, ExtensionDeserializer, Serializable {
   
	private static final long serialVersionUID = 1L;

    public static final String ATTR_IRM_LEN = "irmLen";
    
    public static final String ATTR_IRM_ID = "irmId";
    
    public static final String ATTR_IRM_TIMER = "irmTimer";
    
    public static final String ATTR_IRM_SOCKET = "irmSocket";
    
    public static final String ATTR_IRM_CLIENTID = "irmClientId";
    
    public static final String ATTR_IRM_MOD = "irmMod";
    
    public static final String ATTR_IRM_COMMITMODE = "irmCommitMode";
    
    public static final String ATTR_IRM_SYNCLEVEL = "irmSyncLevel";
    
    public static final String ATTR_IRM_ACK = "irmAck";
    
    public static final String ATTR_IRM_FLOW = "irmFlow";
    
    public static final String ATTR_IRM_TRANCODE = "irmTranCode";
    
    public static final String ATTR_IRM_TRANCODESRC = "irmTranCodeSrc";
    
    public static final String ATTR_IRM_DESTID = "irmDestId";
    
    public static final String ATTR_IRM_LTERM = "irmLterm";
    
    public static final String ATTR_IRM_RACFGRPNAME = "irmRacfGrpName";
    
    public static final String ATTR_IRM_RACFUSERID = "irmRacfUserId";
    
    public static final String ATTR_IRM_RACFPWD = "irmRacfPwd";
    
    public static final String ATTR_IRM_HEADERENCODING = "irmHeaderEncod";
    
    public static final String ATTR_IRM_SENDDATAENCODING = "sendDataEncod";
    
    public static final String ATTR_IRM_REPLYDATAENCODING = "replyDataEncod";

    public static final String ATTR_USE_TYPE = "use";

    public static final String ATTR_ENCODING_STYLE = "encodingStyle";

    // Pattern for finding application variable tokens
    private static final String ENV_VAR_REGEX = "\\$\\{([a-zA-Z0-9\\.\\-\\_^\\{\\}]+)\\}";
    private static final Pattern mPattern = Pattern.compile(ENV_VAR_REGEX);
    
    // environment variable configurations
    protected final Map<String, String[]> mEnvVariableMap =
            new HashMap<String, String[]>();
    
    private static final Messages mMessages = Messages.getMessages(IMSExtSerializer.class);
    
	
	 /**
     *  Creates a new instance of IMSExtSerializer
     */
    public IMSExtSerializer() {
    }
    
    public IMSExtSerializer(Map<String, String[]> envVariableMap) {
        this();
        mEnvVariableMap.putAll(envVariableMap);
    }
	
    /**
     * Contruction of Register Serializer class
     * 
     * @param ExtensionRegistry
     */
    public void registerSerializer(ExtensionRegistry registry) {

        // Register and map IMS Binding
        registry.registerSerializer(Binding.class, IMSConstants.QNAME_BINDING, this);
        registry.registerDeserializer(Binding.class, IMSConstants.QNAME_BINDING, this);
        registry.mapExtensionTypes(Binding.class, IMSConstants.QNAME_BINDING, IMSBinding.class);

        // Register and map IMS Operation
        registry.registerSerializer(BindingOperation.class, IMSConstants.QNAME_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class, IMSConstants.QNAME_OPERATION, this);
        registry.mapExtensionTypes(BindingOperation.class, IMSConstants.QNAME_OPERATION, IMSOperation.class);

        // Register and map IMS Input
        registry.registerSerializer(BindingInput.class, IMSConstants.QNAME_MESSAGE, this);
        registry.registerDeserializer(BindingInput.class, IMSConstants.QNAME_MESSAGE, this);
        registry.mapExtensionTypes(BindingInput.class, IMSConstants.QNAME_MESSAGE, IMSInput.class);

        // Register and map IMS Output
        registry.registerSerializer(BindingOutput.class, IMSConstants.QNAME_MESSAGE, this);
        registry.registerDeserializer(BindingOutput.class, IMSConstants.QNAME_MESSAGE, this);
        registry.mapExtensionTypes(BindingOutput.class, IMSConstants.QNAME_MESSAGE, IMSOutput.class);

        // Register and map IMS Address
        registry.registerSerializer(Port.class, IMSConstants.QNAME_ADDRESS, this);
        registry.registerDeserializer(Port.class, IMSConstants.QNAME_ADDRESS, this);
        registry.mapExtensionTypes(Port.class, IMSConstants.QNAME_ADDRESS, IMSAddress.class);
    }

    /**
     * Marshall the wsdl extensability elements
     * 
     * @param Class
     * @param QName
     * @param ExtensibilityElement
     * @param PrintWriter
     * @param Definition
     * @param ExtensionRegistry
     * @throws WSDLException
     */
    public void marshall(Class parentType,
                         QName elementType,
                         ExtensibilityElement extension,
                         PrintWriter pw,
                         javax.wsdl.Definition def,
                         ExtensionRegistry extReg) throws WSDLException {
        if (extension == null) {
            return;
        }

        if (extension instanceof IMSBinding) {
            IMSBinding imsBinding = (IMSBinding) extension;
            pw.print("      <ims:binding");
            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            pw.println("/>");
        } else if (extension instanceof IMSOperation) {
            IMSOperation imsOperation = (IMSOperation) extension;
            pw.print("      <ims:operation");
            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }

            pw.println("/>");
        } else if (extension instanceof IMSAddress) {
            IMSAddress imsAddress = (IMSAddress) extension;
            pw.print("      <ims:address");
            
			if(imsAddress.getServerLocation() != null) {
				DOMUtils.printAttribute(IMSAddress.ATTR_LOCTN, imsAddress.getServerLocation(), pw);
			}	
            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            pw.println("/>");
        } else if (extension instanceof IMSMessage) {
            IMSMessage imsMessage = (IMSMessage) extension;
            pw.print("      <ims:message");

            if(imsMessage.getIrmAck() != null){
            	DOMUtils.printAttribute(ATTR_IRM_ACK, imsMessage.getIrmAck(), pw);
            }
            if(imsMessage.getIrmClientId() != null){
            	DOMUtils.printAttribute(ATTR_IRM_CLIENTID, imsMessage.getIrmClientId(), pw);
            }
            if(imsMessage.getIrmCommitMode() != null){
            	DOMUtils.printAttribute(ATTR_IRM_COMMITMODE, imsMessage.getIrmCommitMode(), pw);
            }
            if(imsMessage.getIrmDestId() != null){
            	DOMUtils.printAttribute(ATTR_IRM_DESTID, imsMessage.getIrmDestId(), pw);
            }
            if(imsMessage.getIrmFlow() != null){
            	DOMUtils.printAttribute(ATTR_IRM_FLOW, imsMessage.getIrmFlow(), pw);
            }
            if(imsMessage.getIrmDestId() != null){
            	DOMUtils.printAttribute(ATTR_IRM_DESTID, imsMessage.getIrmDestId(), pw);
            }
            if(imsMessage.getIrmHeaderEncod() != null){
            	DOMUtils.printAttribute(ATTR_IRM_HEADERENCODING, imsMessage.getIrmHeaderEncod(), pw);
            }
            if(imsMessage.getSendDataEncod() != null){
            	DOMUtils.printAttribute(ATTR_IRM_SENDDATAENCODING, imsMessage.getSendDataEncod(), pw);
            } 
            if(imsMessage.getReplyDataEncod() != null){
            	DOMUtils.printAttribute(ATTR_IRM_REPLYDATAENCODING, imsMessage.getReplyDataEncod(), pw);
            }               
            if(imsMessage.getIrmId() != null){
            	DOMUtils.printAttribute(ATTR_IRM_ID, imsMessage.getIrmId(), pw);
            }
            if(String.valueOf(imsMessage.getIrmLen()) != null){
            	Integer irmLen = Integer.valueOf(imsMessage.getIrmLen());
            	DOMUtils.printAttribute(ATTR_IRM_LEN, irmLen.toString(), pw);
            }            
            if(imsMessage.getIrmLterm() != null){
            	DOMUtils.printAttribute(ATTR_IRM_LTERM, imsMessage.getIrmLterm(), pw);
            }
            if(imsMessage.getIrmMod() != null){
            	DOMUtils.printAttribute(ATTR_IRM_MOD, imsMessage.getIrmMod(), pw);
            } 
            if(imsMessage.getIrmRacfGrpName() != null){
            	DOMUtils.printAttribute(ATTR_IRM_RACFGRPNAME, imsMessage.getIrmRacfGrpName(), pw);
            }
            if(imsMessage.getIrmRacfPwd() != null){
            	DOMUtils.printAttribute(ATTR_IRM_RACFPWD, imsMessage.getIrmRacfPwd(), pw);
            }  
            if(imsMessage.getIrmRacfUserId() != null){
            	DOMUtils.printAttribute(ATTR_IRM_RACFUSERID, imsMessage.getIrmRacfUserId(), pw);
            }
            if(imsMessage.getIrmSocket() != null){
            	DOMUtils.printAttribute(ATTR_IRM_SOCKET, imsMessage.getIrmSocket(), pw);
            } 
            if(imsMessage.getIrmSyncLevel() != null){
            	DOMUtils.printAttribute(ATTR_IRM_SYNCLEVEL, imsMessage.getIrmSyncLevel(), pw);
            }
            if(imsMessage.getIrmTimer() != null){
            	DOMUtils.printAttribute(ATTR_IRM_TIMER, imsMessage.getIrmTimer(), pw);
            }
            if(imsMessage.getIrmTranCode() != null){
            	DOMUtils.printAttribute(ATTR_IRM_TRANCODE, imsMessage.getIrmTranCode(), pw);
            }  
            if(imsMessage.getIrmTranCodeSrc() != null){
            	DOMUtils.printAttribute(ATTR_IRM_TRANCODESRC, imsMessage.getIrmTranCodeSrc(), pw);
            }              
            if (imsMessage.getUseType() != null) {
                DOMUtils.printAttribute(ATTR_USE_TYPE, imsMessage.getUseType(), pw);
            }
            if (imsMessage.getEncodingStyle() != null) {
                DOMUtils.printAttribute(ATTR_ENCODING_STYLE, imsMessage.getEncodingStyle(), pw);
            }
            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }

            pw.println("/>");
        }
    }

    /**
     * Unmarshall and element and return the extension type
     * 
     * @param Class
     * @param QName
     * @param Element
     * @param Definition
     * @param ExtensionRegistry
     * @return ExtensibilityElement
     * @throws WSDLException
     */

    public javax.wsdl.extensions.ExtensibilityElement unmarshall(Class parentType,
                                                                 QName elementType,
                                                                 Element el,
                                                                 Definition def,
                                                                 ExtensionRegistry extReg) throws WSDLException {

        ExtensibilityElement returnValue = null;

        if (IMSConstants.QNAME_BINDING.equals(elementType)) {
            IMSBinding imsBinding = new IMSBinding();
            returnValue = imsBinding;
        } else if (IMSConstants.QNAME_OPERATION.equals(elementType)) {
            IMSOperation imsOperation = new IMSOperation();
            returnValue = imsOperation;
        } else if (IMSConstants.QNAME_ADDRESS.equals(elementType)) {
            IMSAddress imsAddress = new IMSAddress(); 
            String hostURL = getAttrAndResolveEnvVar(el, IMSAddress.ATTR_LOCTN);
            if (nonEmptyString(hostURL)) {
                imsAddress.setServerLocation(hostURL);
            }
            returnValue = imsAddress;
        } else if (IMSConstants.QNAME_MESSAGE.equals(elementType)) {
            IMSMessage imsMessage = new IMSMessage();

            String irmLen = getAttrAndResolveEnvVar(el, ATTR_IRM_LEN);
            if (nonEmptyString(irmLen)) {
            	Integer mlen = new Integer(irmLen);
                imsMessage.setIrmLen(mlen);
            }   
            String irmId = getAttrAndResolveEnvVar(el, ATTR_IRM_ID);
            if (nonEmptyString(irmId)) {
                imsMessage.setIrmId(irmId);
            }   
            String irmTimer = getAttrAndResolveEnvVar(el, ATTR_IRM_TIMER);
            if (nonEmptyString(irmTimer)) {
                imsMessage.setIrmTimer(irmTimer);
            }  
            String irmSocket = getAttrAndResolveEnvVar(el, ATTR_IRM_SOCKET);
            if (nonEmptyString(irmSocket)) {
                imsMessage.setIrmSocket(irmSocket);
            }     
            String irmClientId = getAttrAndResolveEnvVar(el, ATTR_IRM_CLIENTID);
            if (nonEmptyString(irmClientId)) {
                imsMessage.setIrmClientId(irmClientId);
            } 
            String irmMod = getAttrAndResolveEnvVar(el, ATTR_IRM_MOD);
            if (nonEmptyString(irmMod)) {
                imsMessage.setIrmMod(irmMod);
            }
            String irmCommitMode = getAttrAndResolveEnvVar(el, ATTR_IRM_COMMITMODE);
            if (nonEmptyString(irmCommitMode)) {
                imsMessage.setIrmCommitMode(irmCommitMode);
            } 
            String irmSyncLevel = getAttrAndResolveEnvVar(el, ATTR_IRM_SYNCLEVEL);
            if (nonEmptyString(irmSyncLevel)) {
                imsMessage.setIrmSyncLevel(irmSyncLevel);
            }  
            String irmAck = getAttrAndResolveEnvVar(el, ATTR_IRM_ACK);
            if (nonEmptyString(irmAck)) {
                imsMessage.setIrmAck(irmAck);
            }  
            String irmFlow = getAttrAndResolveEnvVar(el, ATTR_IRM_FLOW);
            if (nonEmptyString(irmFlow)) {
                imsMessage.setIrmFlow(irmFlow);
            }    
            String irmTranCode = getAttrAndResolveEnvVar(el, ATTR_IRM_TRANCODE);
            if (nonEmptyString(irmTranCode)) {
                imsMessage.setIrmTranCode(irmTranCode);
            }  
            String irmTranCodeSrc = getAttrAndResolveEnvVar(el, ATTR_IRM_TRANCODESRC);
            if (nonEmptyString(irmTranCodeSrc)) {
                imsMessage.setIrmTranCodeSrc(irmTranCodeSrc);
            }             
            String irmDestId = getAttrAndResolveEnvVar(el, ATTR_IRM_DESTID);
            if (nonEmptyString(irmDestId)) {
                imsMessage.setIrmDestId(irmDestId);
            } 
            String irmLterm = getAttrAndResolveEnvVar(el, ATTR_IRM_LTERM);
            if (nonEmptyString(irmLterm)) {
                imsMessage.getIrmLterm();
            } 
            String irmRacfGrpName = getAttrAndResolveEnvVar(el, ATTR_IRM_RACFGRPNAME);
            if (nonEmptyString(irmRacfGrpName)) {
                imsMessage.setIrmRacfGrpName(irmRacfGrpName);
            } 
            String irmRacfUserId = getAttrAndResolveEnvVar(el, ATTR_IRM_RACFUSERID);
            if (nonEmptyString(irmRacfUserId)) {
                imsMessage.setIrmRacfUserId(irmRacfUserId);
            }
            String irmRacfPwd = getAttrAndResolveEnvVar(el, ATTR_IRM_RACFPWD);
            if (nonEmptyString(irmRacfPwd)) {
                imsMessage.setIrmRacfPwd(irmRacfPwd);
            } 
            String irmHeaderEncod = getAttrAndResolveEnvVar(el, ATTR_IRM_HEADERENCODING);
            if (nonEmptyString(irmHeaderEncod)) {
                imsMessage.setIrmHeaderEncod(irmHeaderEncod);
            } 
            String sendDataEncod = getAttrAndResolveEnvVar(el, ATTR_IRM_SENDDATAENCODING);
            if (nonEmptyString(sendDataEncod)) {
                imsMessage.setSendDataEncod(sendDataEncod);
            } 
            String replyDataEncod = getAttrAndResolveEnvVar(el, ATTR_IRM_REPLYDATAENCODING);
            if (nonEmptyString(replyDataEncod)) {
                imsMessage.setReplyDataEncod(replyDataEncod);
            }                
            String useType = getAttrAndResolveEnvVar(el, ATTR_USE_TYPE);
            if (nonEmptyString(useType)) {
                imsMessage.setUseType(useType);
            }
            String encodingStyle = getAttrAndResolveEnvVar(el, ATTR_ENCODING_STYLE);
            if (nonEmptyString(encodingStyle)) {
                imsMessage.setEncodingStyle(encodingStyle);
            }

            returnValue = imsMessage;
        }
        return returnValue;
    }

    private boolean nonEmptyString(String strToTest) {
        boolean nonEmpty = false;
        if (strToTest != null && strToTest.length() > 0) {
            nonEmpty = true;
        }
        return nonEmpty;
    }
    
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
				throw new Exception(mMessages.getString("IMSBC-E001002.Invalid_Token_Name", tokenName));
			}
			refs.add(tokenName);
		}

		if (attrVal.indexOf("${}") >= 0) {
			throw new Exception(mMessages.getString(
													"IMSBC-E001003.Invalid_Empty_Token_Name", 
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
															"IMSBC-E001004.Invalid_Env_Var_Ref_No_Def",
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
																"IMSBC-E001005.Invalid_Env_Var_Value_Null",
																new Object[] {
																vars[i],
																attrName }));
								}
								if (varVal.indexOf("${") >= 0) {
									throw new WSDLException(
											"INVALID_WSDL",
											mMessages.getString(
																"IMSBC-E001006.Invalid_Var_Value_Contains_Var_Ref",
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
				/*
                 * if (hasMigrationEnvVarRef(attrVal)) { // still has ref un-resolved throw new
                 * WSDLException( "INVALID_WSDL", mMessages .getString(
                 * "IMSBC.Invalid_attr_value_contains_unresolvable_ref", new Object[] { attrVal,
                 * attrName })); }
                 */
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
