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
 * @(#)FTPBCExtPreprocessDeserializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.extensions;

import com.sun.jbi.internationalization.Messages;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;

import javax.xml.namespace.QName;

import com.ibm.wsdl.util.xml.DOMUtils;
import org.w3c.dom.Element;

import java.util.Map;

/**
 *
 * @author jfu
 */
public class FTPBCExtPreprocessDeserializer extends FTPExtSerializer {
    private static final Messages mMessages = Messages.getMessages(FTPBCExtPreprocessDeserializer.class);
    
    public FTPBCExtPreprocessDeserializer(Map appVariableMap) {
        super(appVariableMap);
    }
    
    public javax.wsdl.extensions.ExtensibilityElement unmarshall(Class parentType,
            QName elementType,
            Element el,
            Definition def,
            ExtensionRegistry extReg)
            throws javax.wsdl.WSDLException {
        
        ExtensibilityElement returnValue = null;
        String tmp = null;
        
        if (FTPConstants.QNAME_BINDING.equals(elementType)) {
            FTPBinding fileBinding = new FTPBinding();
            returnValue = fileBinding;
        } else if (FTPConstants.QNAME_OPERATION.equals(elementType)) {
            FTPOperation fileOperation = new FTPOperation();
            returnValue = fileOperation;
        } else if (FTPConstants.QNAME_TRANSFER.equals(elementType)) {
            FTPTransfer ftpTransfer = new FTPTransfer();
            
            collectAppVars(el, FTPTransfer.FTP_SENDTO_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            
            tmp = DOMUtils.getAttribute(el, FTPTransfer.FTP_APPEND_PROPERTY);
            if (tmp != null) {
                ftpTransfer.setAppend(tmp.equals("true"));
            }
            
            tmp = DOMUtils.getAttribute(el, FTPTransfer.FTP_SENDTO_HAS_PATTS_PROPERTY);
            if (tmp != null) {
                ftpTransfer.setSendToHasPatterns(tmp.equals("true"));
            }
            
            collectAppVars(el, FTPTransfer.FTP_RECEIVEFROM_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            
            tmp = DOMUtils.getAttribute(el, FTPTransfer.FTP_RECEIVEFROM_HAS_PATTS_PROPERTY);
            if (tmp != null) {
                ftpTransfer.setReceiveFromHasRegexs(tmp.equals("true"));
            }
            
            collectAppVars(el, FTPTransfer.FTP_POLLINTERVAL_PROPERTY, FTPConstants.APP_VAR_TYPE_NUMBER);
            collectAppVars(el, FTPTransfer.FTP_PRE_SEND_CMD_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            collectAppVars(el, FTPTransfer.FTP_PRE_SEND_LOC_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            
            tmp = DOMUtils.getAttribute(el, FTPTransfer.FTP_PRE_SEND_LOC_HAS_PATTS_PROPERTY);
            if (tmp != null) {
                ftpTransfer.setPreSendLocHasPatterns(tmp.equals("true"));
            }
            
            collectAppVars(el, FTPTransfer.FTP_PRE_RECEIVE_CMD_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            collectAppVars(el, FTPTransfer.FTP_PRE_RECEIVE_LOC_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            
            tmp = DOMUtils.getAttribute(el, FTPTransfer.FTP_PRE_RECEIVE_LOC_HAS_PATTS_PROPERTY);
            if (tmp != null) {
                ftpTransfer.setPreReceiveLocHasPatterns(tmp.equals("true"));
            }
            
            collectAppVars(el, FTPTransfer.FTP_POST_SEND_CMD_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            collectAppVars(el, FTPTransfer.FTP_POST_SEND_LOC_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            
            tmp = DOMUtils.getAttribute(el, FTPTransfer.FTP_POST_SEND_LOC_HAS_PATTS_PROPERTY);
            if (tmp != null) {
                ftpTransfer.setPostSendLocHasPatterns(tmp.equals("true"));
            }
            
            collectAppVars(el, FTPTransfer.FTP_POST_RECEIVE_CMD_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            collectAppVars(el, FTPTransfer.FTP_POST_RECEIVE_LOC_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            
            tmp = DOMUtils.getAttribute(el, FTPTransfer.FTP_POST_RECEIVE_LOC_HAS_PATTS_PROPERTY);
            if (tmp != null) {
                ftpTransfer.setPostReceiveLocHasPatterns(tmp.equals("true"));
            }
            
            collectAppVars(el, FTPTransfer.FTP_USE_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            collectAppVars(el, FTPTransfer.FTP_ENCODINGSTYLE_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            collectAppVars(el, FTPTransfer.FTP_PART_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            
            tmp = DOMUtils.getAttribute(el, FTPTransfer.FTP_MSG_CORRELATE_ENABLED_PROPERTY);
            if (tmp != null) {
                ftpTransfer.setMessageCorrelate(tmp.equals("true"));
            }
            
            returnValue = ftpTransfer;
        } else if (FTPConstants.QNAME_MESSAGE.equals(elementType)) {
            FTPMessage ftpMessage = new FTPMessage();
            
            tmp = DOMUtils.getAttribute(el, FTPMessage.FTP_POLLINTERVAL_PROPERTY);
            if (tmp != null) {
                int interval = 5000;
                try {
                    interval = Integer.parseInt(tmp);
                } catch (Exception e) {
                    
                }
                ftpMessage.setPollInterval(interval);
            }
            
            collectAppVars(el, FTPMessage.FTP_USE_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            collectAppVars(el, FTPMessage.FTP_ENCODINGSTYLE_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            collectAppVars(el, FTPMessage.FTP_PART_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            collectAppVars(el, FTPMessage.FTP_MSG_REPO_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            collectAppVars(el, FTPMessage.FTP_SYMETRIC_MSG_NAME_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            collectAppVars(el, FTPMessage.FTP_SYMETRIC_MSG_NAME_PREFIX_IB_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            collectAppVars(el, FTPMessage.FTP_SYMETRIC_MSG_NAME_PREFIX_OB_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            
            tmp = DOMUtils.getAttribute(el, FTPMessage.FTP_PROTECT_ENABLED_PROPERTY);
            if (tmp != null) {
                ftpMessage.setProtectEnabled(tmp.equals("true"));
            }
            
            tmp = DOMUtils.getAttribute(el, FTPMessage.FTP_ARCHIVE_ENABLED_PROPERTY);
            if (tmp != null) {
                ftpMessage.setArchiveEnabled(tmp.equals("true"));
            }
            
            tmp = DOMUtils.getAttribute(el, FTPMessage.FTP_STAGING_ENABLED_PROPERTY);
            if (tmp != null) {
                ftpMessage.setStagingEnabled(tmp.equals("true"));
            }
            
            tmp = DOMUtils.getAttribute(el, FTPMessage.FTP_MSG_CORRELATE_ENABLED_PROPERTY);
            if (tmp != null) {
                ftpMessage.setMessageCorrelate(tmp.equals("true"));
            }
            
            returnValue = ftpMessage;
        } else if (FTPConstants.QNAME_ADDRESS.equals(elementType)) {
            FTPAddress ftpAddress = new FTPAddress();
            
            collectAppVars(el, FTPAddress.ADDR_URL_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);

            collectAppVars(el, FTPAddress.FTP_SEC_TYPE_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);

            tmp = DOMUtils.getAttribute(el, FTPAddress.FTP_ENABLE_CCC_PROPERTY);
            if (tmp != null) {
                ftpAddress.setEnableCCC(tmp.equals("true"));
            }
            
            collectAppVars(el, FTPAddress.FTP_KSTOR_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            collectAppVars(el, FTPAddress.FTP_KSTOR_PASSWD_PROPERTY, FTPConstants.APP_VAR_TYPE_PASSWORD);
            collectAppVars(el, FTPAddress.FTP_KEY_ALIAS_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            collectAppVars(el, FTPAddress.FTP_KEY_PASSWD_PROPERTY, FTPConstants.APP_VAR_TYPE_PASSWORD);
            collectAppVars(el, FTPAddress.FTP_TSTOR_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            collectAppVars(el, FTPAddress.FTP_TSTOR_PASSWD_PROPERTY, FTPConstants.APP_VAR_TYPE_PASSWORD);

            collectAppVars(el, FTPAddress.FTP_DIRLSTSTYLE_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            
            tmp = DOMUtils.getAttribute(el, FTPAddress.FTP_USE_UD_HEURISTICS_PROPERTY);
            if (tmp != null) {
                ftpAddress.setUseUserDefinedHeuristics(tmp.equals("true"));
            }
            
            collectAppVars(el, FTPAddress.FTP_UD_DIRLSTSTYLE_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            collectAppVars(el, FTPAddress.FTP_UD_HEURISTICS_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            collectAppVars(el, FTPAddress.FTP_TRANSMODE_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            collectAppVars(el, FTPAddress.FTP_CMD_CH_TIMEOUT_PROPERTY, FTPConstants.APP_VAR_TYPE_NUMBER);
            collectAppVars(el, FTPAddress.FTP_DATA_CH_TIMEOUT_PROPERTY, FTPConstants.APP_VAR_TYPE_NUMBER);
            
            collectAppVars(el, FTPAddress.FTP_CNTRL_CH_ENCODING_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);
            collectAppVars(el, FTPAddress.FTP_PERSIST_BASE_LOC_PROPERTY, FTPConstants.APP_VAR_TYPE_STRING);

            returnValue = ftpAddress;
        }
        return returnValue;
    }
    
    protected void collectAppVars(Element el, String attrName, String appVarType) throws WSDLException {
        String s = DOMUtils.getAttribute(el, attrName);
        if (s != null) {
            try {
                if (hasMigrationEnvVarRef(s)) {
                    Object[] appVariableNames = getAppVariableNames(attrName, s);
                    if ( appVariableNames != null ) {
                        for ( int i = 0; i < appVariableNames.length; i++ ) {
                            if (!mAppVariableMap.containsKey(appVariableNames[i])) {
                                //mAppVariableMap.put(appVariableNames[i], new String[] {null, appVarType});
                            }
                        }
                    }
                }
            } catch (Exception e) {
                throw new WSDLException("INVALID_WSDL", e.getMessage());
            }
        }
    }
}
