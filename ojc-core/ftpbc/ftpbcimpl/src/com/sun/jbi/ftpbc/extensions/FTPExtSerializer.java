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
 * @(#)FTPExtSerializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc.extensions;

import com.sun.jbi.internationalization.Messages;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.Definition;
import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOutput;
import javax.wsdl.BindingOperation;
import javax.wsdl.Port;
import javax.wsdl.WSDLException;

import javax.xml.namespace.QName;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.util.xml.DOMUtils;
import org.w3c.dom.Element;

import java.io.Serializable;
import java.io.PrintWriter;
import java.util.Map;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *
 * @author jim.fu@sun.com
 */
public class FTPExtSerializer implements ExtensionSerializer, ExtensionDeserializer, Serializable {

    private static final long serialVersionUID = 1L;
    private static final String APP_VAR_REGEX = "\\$\\{([a-zA-Z0-9\\.\\-\\_^\\{\\}]+)\\}";
    // application variable configurations
    protected Map mAppVariableMap;
    private static final Messages mMessages = Messages.getMessages(FTPExtSerializer.class);
    private static final Pattern mPattern = Pattern.compile(APP_VAR_REGEX);

    public FTPExtSerializer(Map appVariableMap) {
        mAppVariableMap = appVariableMap;
    }

    /**
     * Registers the serializers / deserializers
     */
    public void registerSerializer(ExtensionRegistry registry) {
        registry.registerSerializer(Binding.class, FTPConstants.QNAME_BINDING, this);
        registry.registerDeserializer(Binding.class, FTPConstants.QNAME_BINDING, this);
        registry.mapExtensionTypes(Binding.class, FTPConstants.QNAME_BINDING, FTPBinding.class);

        registry.registerSerializer(BindingOperation.class, FTPConstants.QNAME_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class, FTPConstants.QNAME_OPERATION, this);
        registry.mapExtensionTypes(BindingOperation.class, FTPConstants.QNAME_OPERATION, FTPOperation.class);

        registry.registerSerializer(BindingInput.class, FTPConstants.QNAME_TRANSFER, this);
        registry.registerDeserializer(BindingInput.class, FTPConstants.QNAME_TRANSFER, this);
        registry.mapExtensionTypes(BindingInput.class, FTPConstants.QNAME_TRANSFER, FTPTransfer.class);

        registry.registerSerializer(BindingInput.class, FTPConstants.QNAME_MESSAGE, this);
        registry.registerDeserializer(BindingInput.class, FTPConstants.QNAME_MESSAGE, this);
        registry.mapExtensionTypes(BindingInput.class, FTPConstants.QNAME_MESSAGE, FTPMessage.class);

        registry.registerSerializer(BindingOutput.class, FTPConstants.QNAME_TRANSFER, this);
        registry.registerDeserializer(BindingOutput.class, FTPConstants.QNAME_TRANSFER, this);
        registry.mapExtensionTypes(BindingOutput.class, FTPConstants.QNAME_TRANSFER, FTPTransfer.class);

        registry.registerSerializer(BindingOutput.class, FTPConstants.QNAME_MESSAGE, this);
        registry.registerDeserializer(BindingOutput.class, FTPConstants.QNAME_MESSAGE, this);
        registry.mapExtensionTypes(BindingOutput.class, FTPConstants.QNAME_MESSAGE, FTPMessage.class);

        registry.registerSerializer(Port.class, FTPConstants.QNAME_ADDRESS, this);
        registry.registerDeserializer(Port.class, FTPConstants.QNAME_ADDRESS, this);
        registry.mapExtensionTypes(Port.class, FTPConstants.QNAME_ADDRESS, FTPAddress.class);
    }

    public Map getAppVariableMap() {
        return mAppVariableMap;
    }

    public void marshall(Class parentType,
            QName elementType,
            ExtensibilityElement extension,
            PrintWriter pw,
            javax.wsdl.Definition def,
            ExtensionRegistry extReg) throws WSDLException {
        if (extension == null) {
            return;
        }

        if (extension instanceof FTPBinding) {
            pw.print("      <ftp:binding");
            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            pw.println("/>");
        } else if (extension instanceof FTPOperation) {
            pw.print("      <ftp:operation");
            pw.println("/>");
        } else if (extension instanceof FTPTransfer) {
            FTPTransfer ftpTransfer = (FTPTransfer) extension;
            pw.print("      <ftp:transfer");

            if (ftpTransfer.getSendTo() != null) {
                DOMUtils.printAttribute(FTPTransfer.FTP_SENDTO_PROPERTY,
                        ftpTransfer.getSendTo(),
                        pw);
            }

            DOMUtils.printAttribute(FTPTransfer.FTP_APPEND_PROPERTY,
                    ftpTransfer.getAppend() ? "true" : "false",
                    pw);

            DOMUtils.printAttribute(FTPTransfer.FTP_SENDTO_HAS_PATTS_PROPERTY,
                    ftpTransfer.getSendToHasPatterns() ? "true" : "false",
                    pw);

            if (ftpTransfer.getReceiveFrom() != null) {
                DOMUtils.printAttribute(FTPTransfer.FTP_RECEIVEFROM_PROPERTY,
                        ftpTransfer.getReceiveFrom(),
                        pw);
            }

            DOMUtils.printAttribute(FTPTransfer.FTP_RECEIVEFROM_HAS_PATTS_PROPERTY,
                    ftpTransfer.getReceiveFromHasRegexs() ? "true" : "false",
                    pw);

            DOMUtils.printAttribute(FTPTransfer.FTP_POLLINTERVAL_PROPERTY,
                    ftpTransfer.getPollIntervalStr(),
                    pw);


            if (ftpTransfer.getPreSendCommand() != null) {
                DOMUtils.printAttribute(FTPTransfer.FTP_PRE_SEND_CMD_PROPERTY,
                        ftpTransfer.getPreSendCommand(),
                        pw);
            }

            if (ftpTransfer.getPreSendLoc() != null) {
                DOMUtils.printAttribute(FTPTransfer.FTP_PRE_SEND_LOC_PROPERTY,
                        ftpTransfer.getPreSendLoc(),
                        pw);
            }

            DOMUtils.printAttribute(FTPTransfer.FTP_PRE_SEND_LOC_HAS_PATTS_PROPERTY,
                    ftpTransfer.getPreSendLocHasPatterns() ? "true" : "false",
                    pw);

            if (ftpTransfer.getPreReceiveCommand() != null) {
                DOMUtils.printAttribute(FTPTransfer.FTP_PRE_RECEIVE_CMD_PROPERTY,
                        ftpTransfer.getPreReceiveCommand(),
                        pw);
            }

            if (ftpTransfer.getPreReceiveLoc() != null) {
                DOMUtils.printAttribute(FTPTransfer.FTP_PRE_RECEIVE_LOC_PROPERTY,
                        ftpTransfer.getPreReceiveLoc(),
                        pw);
            }

            DOMUtils.printAttribute(FTPTransfer.FTP_PRE_RECEIVE_LOC_HAS_PATTS_PROPERTY,
                    ftpTransfer.getPreReceiveLocHasPatterns() ? "true" : "false",
                    pw);

            if (ftpTransfer.getPostSendCommand() != null) {
                DOMUtils.printAttribute(FTPTransfer.FTP_POST_SEND_CMD_PROPERTY,
                        ftpTransfer.getPostSendCommand(),
                        pw);
            }

            if (ftpTransfer.getPostSendLoc() != null) {
                DOMUtils.printAttribute(FTPTransfer.FTP_POST_SEND_LOC_PROPERTY,
                        ftpTransfer.getPostSendLoc(),
                        pw);
            }

            DOMUtils.printAttribute(FTPTransfer.FTP_POST_SEND_LOC_HAS_PATTS_PROPERTY,
                    ftpTransfer.getPostSendLocHasPatterns() ? "true" : "false",
                    pw);

            if (ftpTransfer.getPostReceiveCommand() != null) {
                DOMUtils.printAttribute(FTPTransfer.FTP_POST_RECEIVE_CMD_PROPERTY,
                        ftpTransfer.getPostReceiveCommand(),
                        pw);
            }

            if (ftpTransfer.getPostReceiveLoc() != null) {
                DOMUtils.printAttribute(FTPTransfer.FTP_POST_RECEIVE_LOC_PROPERTY,
                        ftpTransfer.getPostReceiveLoc(),
                        pw);
            }

            DOMUtils.printAttribute(FTPTransfer.FTP_POST_RECEIVE_LOC_HAS_PATTS_PROPERTY,
                    ftpTransfer.getPostReceiveLocHasPatterns() ? "true" : "false",
                    pw);

            if (ftpTransfer.getUse() != null) {
                DOMUtils.printAttribute(FTPTransfer.FTP_USE_PROPERTY,
                        ftpTransfer.getUse(),
                        pw);
            }

            if (ftpTransfer.getEncodingStyle() != null) {
                DOMUtils.printAttribute(FTPTransfer.FTP_ENCODINGSTYLE_PROPERTY,
                        ftpTransfer.getEncodingStyle(),
                        pw);
            }

            if (ftpTransfer.getPart() != null) {
                DOMUtils.printAttribute(FTPTransfer.FTP_PART_PROPERTY,
                        ftpTransfer.getPart(),
                        pw);
            }

            if (ftpTransfer.getFileType() != null) {
                DOMUtils.printAttribute(FTPTransfer.FTP_FILETYPE_PROPERTY,
                        ftpTransfer.getFileType(),
                        pw);
            }

            if (ftpTransfer.getForwardAsAttachment()) {
                DOMUtils.printAttribute(FTPTransfer.FTP_FORWARD_AS_ATT_PROPERTY,
                        "true",
                        pw);
            }

            if (ftpTransfer.getCharacterEncoding() != null) {
                DOMUtils.printAttribute(FTPTransfer.FTP_CHARENCODING_PROPERTY,
                        ftpTransfer.getCharacterEncoding(),
                        pw);
            }

            DOMUtils.printAttribute(FTPTransfer.FTP_MSG_CORRELATE_ENABLED_PROPERTY,
                    ftpTransfer.getMessageCorrelate() ? "true" : "false",
                    pw);

            pw.println("/>");
        } else if (extension instanceof FTPMessage) {

            FTPMessage ftpMessage = (FTPMessage) extension;
            pw.print("      <ftp:message ");

            DOMUtils.printAttribute(FTPMessage.FTP_POLLINTERVAL_PROPERTY,
                    "" + ftpMessage.getPollInterval(),
                    pw);

            if (ftpMessage.getUse() != null) {
                DOMUtils.printAttribute(FTPMessage.FTP_USE_PROPERTY,
                        ftpMessage.getUse(),
                        pw);
            }

            if (ftpMessage.getEncodingStyle() != null) {
                DOMUtils.printAttribute(FTPMessage.FTP_ENCODINGSTYLE_PROPERTY,
                        ftpMessage.getEncodingStyle(),
                        pw);
            }
            if (ftpMessage.getPart() != null) {
                DOMUtils.printAttribute(FTPMessage.FTP_PART_PROPERTY,
                        ftpMessage.getPart(),
                        pw);
            }

            if (ftpMessage.getFileType() != null) {
                DOMUtils.printAttribute(FTPMessage.FTP_FILETYPE_PROPERTY,
                        ftpMessage.getFileType(),
                        pw);
            }

            if (ftpMessage.getForwardAsAttachment()) {
                DOMUtils.printAttribute(FTPMessage.FTP_FORWARD_AS_ATT_PROPERTY,
                        "true",
                        pw);
            }

            if (ftpMessage.getCharacterEncoding() != null) {
                DOMUtils.printAttribute(FTPMessage.FTP_CHARENCODING_PROPERTY,
                        ftpMessage.getCharacterEncoding(),
                        pw);
            }

            if (ftpMessage.getMessageRepo() != null) {
                DOMUtils.printAttribute(FTPMessage.FTP_MSG_REPO_PROPERTY,
                        ftpMessage.getMessageRepo(),
                        pw);
            }

            if (ftpMessage.getMessageName() != null) {
                DOMUtils.printAttribute(FTPMessage.FTP_SYMETRIC_MSG_NAME_PROPERTY,
                        ftpMessage.getMessageName(),
                        pw);
            }

            if (ftpMessage.getMessageNamePrefixIB() != null) {
                DOMUtils.printAttribute(FTPMessage.FTP_SYMETRIC_MSG_NAME_PREFIX_IB_PROPERTY,
                        ftpMessage.getMessageNamePrefixIB(),
                        pw);
            }

            if (ftpMessage.getMessageNamePrefixOB() != null) {
                DOMUtils.printAttribute(FTPMessage.FTP_SYMETRIC_MSG_NAME_PREFIX_OB_PROPERTY,
                        ftpMessage.getMessageNamePrefixOB(),
                        pw);
            }

            DOMUtils.printAttribute(FTPMessage.FTP_PROTECT_ENABLED_PROPERTY,
                    ftpMessage.getProtectEnabled() ? "true" : "false",
                    pw);

            DOMUtils.printAttribute(FTPMessage.FTP_ARCHIVE_ENABLED_PROPERTY,
                    ftpMessage.getArchiveEnabled() ? "true" : "false",
                    pw);

            DOMUtils.printAttribute(FTPMessage.FTP_STAGING_ENABLED_PROPERTY,
                    ftpMessage.getStagingEnabled() ? "true" : "false",
                    pw);

            DOMUtils.printAttribute(FTPMessage.FTP_MSG_CORRELATE_ENABLED_PROPERTY,
                    ftpMessage.getMessageCorrelate() ? "true" : "false",
                    pw);

            pw.println("/>");
        } else if (extension instanceof FTPAddress) {
            FTPAddress ftpAddress = (FTPAddress) extension;
            pw.print("      <ftp:address");

            if (ftpAddress.getURL() != null) {
                DOMUtils.printAttribute(FTPAddress.ADDR_URL_PROPERTY,
                        ftpAddress.getURL(),
                        pw);
            }

            // new attrs for FTP/TLS
            if (ftpAddress.getSecureFTPType() != null) {
                DOMUtils.printAttribute(FTPAddress.FTP_SEC_TYPE_PROPERTY,
                        ftpAddress.getSecureFTPType(),
                        pw);
            }

            if (ftpAddress.getEnableCCC()) {
                DOMUtils.printAttribute(FTPAddress.FTP_ENABLE_CCC_PROPERTY,
                        ftpAddress.getEnableCCC() ? "true" : "false",
                        pw);
            }

            if (ftpAddress.getKeyStore() != null) {
                DOMUtils.printAttribute(FTPAddress.FTP_KSTOR_PROPERTY,
                        ftpAddress.getKeyStore(),
                        pw);
            }

            if (ftpAddress.getKeyStorePassword() != null) {
                DOMUtils.printAttribute(FTPAddress.FTP_KSTOR_PASSWD_PROPERTY,
                        ftpAddress.getKeyStorePassword(),
                        pw);
            }

            if (ftpAddress.getKeyAlias() != null) {
                DOMUtils.printAttribute(FTPAddress.FTP_KEY_ALIAS_PROPERTY,
                        ftpAddress.getKeyAlias(),
                        pw);
            }

            if (ftpAddress.getKeyPassword() != null) {
                DOMUtils.printAttribute(FTPAddress.FTP_KEY_PASSWD_PROPERTY,
                        ftpAddress.getKeyPassword(),
                        pw);
            }

            if (ftpAddress.getTrustStore() != null) {
                DOMUtils.printAttribute(FTPAddress.FTP_TSTOR_PROPERTY,
                        ftpAddress.getTrustStore(),
                        pw);
            }

            if (ftpAddress.getTrustStorePassword() != null) {
                DOMUtils.printAttribute(FTPAddress.FTP_TSTOR_PASSWD_PROPERTY,
                        ftpAddress.getTrustStorePassword(),
                        pw);
            }

            if (ftpAddress.getUser() != null) {
                DOMUtils.printAttribute(FTPAddress.ADDR_USER_PROPERTY,
                        ftpAddress.getUser(),
                        pw);
            }

            if (ftpAddress.getPassword() != null) {
                DOMUtils.printAttribute(FTPAddress.ADDR_PASSWORD_PROPERTY,
                        ftpAddress.getPassword(),
                        pw);
            }

            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED,
                        required.toString(),
                        def,
                        pw);
            }

            if (ftpAddress.getDirListStyle() != null) {
                DOMUtils.printAttribute(ftpAddress.FTP_DIRLSTSTYLE_PROPERTY,
                        ftpAddress.getDirListStyle(),
                        pw);
            }

            DOMUtils.printAttribute(ftpAddress.FTP_USE_UD_HEURISTICS_PROPERTY,
                    ftpAddress.getUseUserDefinedHeuristics() ? "true" : "false",
                    pw);

            if (ftpAddress.getUserDefDirListStyle() != null) {
                DOMUtils.printAttribute(ftpAddress.FTP_UD_DIRLSTSTYLE_PROPERTY,
                        ftpAddress.getUserDefDirListStyle(),
                        pw);
            }

            if (ftpAddress.getUserDefDirListHeuristics() != null) {
                DOMUtils.printAttribute(ftpAddress.FTP_UD_HEURISTICS_PROPERTY,
                        ftpAddress.getUserDefDirListHeuristics(),
                        pw);
            }

            if (ftpAddress.getTransferMode() != null) {
                DOMUtils.printAttribute(ftpAddress.FTP_TRANSMODE_PROPERTY,
                        ftpAddress.getTransferMode(),
                        pw);
            }

            if (ftpAddress.getCmdChannelTimeoutStr() != null) {
                DOMUtils.printAttribute(ftpAddress.FTP_CMD_CH_TIMEOUT_PROPERTY,
                        ftpAddress.getCmdChannelTimeoutStr(),
                        pw);
            }

            if (ftpAddress.getDataChannelTimeoutStr() != null) {
                DOMUtils.printAttribute(ftpAddress.FTP_DATA_CH_TIMEOUT_PROPERTY,
                        ftpAddress.getDataChannelTimeoutStr(),
                        pw);
            }

            if (ftpAddress.getControlChannelEncoding() != null) {
                DOMUtils.printAttribute(ftpAddress.FTP_CNTRL_CH_ENCODING_PROPERTY,
                        ftpAddress.getControlChannelEncoding(),
                        pw);
            }

            if (ftpAddress.getPersistenceBaseLocation() != null && ftpAddress.getPersistenceBaseLocation().trim().length() > 0) {
                DOMUtils.printAttribute(ftpAddress.FTP_PERSIST_BASE_LOC_PROPERTY,
                        ftpAddress.getPersistenceBaseLocation(),
                        pw);
            }

            pw.println("/>");
        }
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

            ftpTransfer.setSendTo(getAttrAndResolveAppVar(el, FTPTransfer.FTP_SENDTO_PROPERTY));
            // when deployed under glassfish v3 (fuji container)
            // an attribute can assume an empty string as value
            // in this case, treat it as null, and the corresponding
            // attribute should fall back to the default (if there is a default)
            tmp = getAttrAndResolveAppVar(el, FTPTransfer.FTP_APPEND_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) {
                ftpTransfer.setAppend(tmp.equals("true"));
            }

            tmp = getAttrAndResolveAppVar(el, FTPTransfer.FTP_SENDTO_HAS_PATTS_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) {
                ftpTransfer.setSendToHasPatterns(tmp.equals("true"));
            }

            ftpTransfer.setReceiveFrom(getAttrAndResolveAppVar(el, FTPTransfer.FTP_RECEIVEFROM_PROPERTY));

            tmp = getAttrAndResolveAppVar(el, FTPTransfer.FTP_RECEIVEFROM_HAS_PATTS_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) {
                ftpTransfer.setReceiveFromHasRegexs(tmp.equals("true"));
            }

            tmp = getAttrAndResolveAppVar(el, FTPTransfer.FTP_POLLINTERVAL_PROPERTY);

            if (tmp != null && tmp.trim().length() > 0) {
                ftpTransfer.setPollIntervalStr(tmp.trim());
            } else {
                ftpTransfer.setPollIntervalStr("5000");
            }

            ftpTransfer.setPreSendCommand(getAttrAndResolveAppVar(el, FTPTransfer.FTP_PRE_SEND_CMD_PROPERTY));

            ftpTransfer.setPreSendLoc(getAttrAndResolveAppVar(el, FTPTransfer.FTP_PRE_SEND_LOC_PROPERTY));

            tmp = getAttrAndResolveAppVar(el, FTPTransfer.FTP_PRE_SEND_LOC_HAS_PATTS_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) {
                ftpTransfer.setPreSendLocHasPatterns(tmp.equals("true"));
            }

            ftpTransfer.setPreReceiveCommand(getAttrAndResolveAppVar(el, FTPTransfer.FTP_PRE_RECEIVE_CMD_PROPERTY));
            ftpTransfer.setPreReceiveLoc(getAttrAndResolveAppVar(el, FTPTransfer.FTP_PRE_RECEIVE_LOC_PROPERTY));

            tmp = getAttrAndResolveAppVar(el, FTPTransfer.FTP_PRE_RECEIVE_LOC_HAS_PATTS_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) {
                ftpTransfer.setPreReceiveLocHasPatterns(tmp.equals("true"));
            }

            ftpTransfer.setPostSendCommand(getAttrAndResolveAppVar(el, FTPTransfer.FTP_POST_SEND_CMD_PROPERTY));
            ftpTransfer.setPostSendLoc(getAttrAndResolveAppVar(el, FTPTransfer.FTP_POST_SEND_LOC_PROPERTY));

            tmp = getAttrAndResolveAppVar(el, FTPTransfer.FTP_POST_SEND_LOC_HAS_PATTS_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) {
                ftpTransfer.setPostSendLocHasPatterns(tmp.equals("true"));
            }

            ftpTransfer.setPostReceiveCommand(getAttrAndResolveAppVar(el, FTPTransfer.FTP_POST_RECEIVE_CMD_PROPERTY));
            ftpTransfer.setPostReceiveLoc(getAttrAndResolveAppVar(el, FTPTransfer.FTP_POST_RECEIVE_LOC_PROPERTY));

            tmp = getAttrAndResolveAppVar(el, FTPTransfer.FTP_POST_RECEIVE_LOC_HAS_PATTS_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) {
                ftpTransfer.setPostReceiveLocHasPatterns(tmp.equals("true"));
            }

            ftpTransfer.setUse(getAttrAndResolveAppVar(el, FTPTransfer.FTP_USE_PROPERTY));
            ftpTransfer.setEncodingStyle(getAttrAndResolveAppVar(el, FTPTransfer.FTP_ENCODINGSTYLE_PROPERTY));
            ftpTransfer.setPart(getAttrAndResolveAppVar(el, FTPTransfer.FTP_PART_PROPERTY));

            tmp = getAttrAndResolveAppVar(el, FTPTransfer.FTP_FILETYPE_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) {
                ftpTransfer.setFileType(tmp.trim());
            }

            tmp = getAttrAndResolveAppVar(el, FTPTransfer.FTP_FORWARD_AS_ATT_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) {
                ftpTransfer.setForwardAsAttachment(tmp.equals("true"));
            }

            tmp = getAttrAndResolveAppVar(el, FTPTransfer.FTP_CHARENCODING_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) {
                ftpTransfer.setCharacterEncoding(tmp.trim());
            }

            tmp = getAttrAndResolveAppVar(el, FTPTransfer.FTP_MSG_CORRELATE_ENABLED_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) {
                ftpTransfer.setMessageCorrelate(tmp.equals("true"));
            }

            returnValue = ftpTransfer;
        } else if (FTPConstants.QNAME_MESSAGE.equals(elementType)) {
            FTPMessage ftpMessage = new FTPMessage();

            tmp = getAttrAndResolveAppVar(el, FTPMessage.FTP_POLLINTERVAL_PROPERTY);

            if (tmp != null) {
                // an empty str as poll interval
                // will result in default (5000) used
                int interval = 5000;
                try {
                    interval = Integer.parseInt(tmp);
                } catch (Exception e) {
                }
                ftpMessage.setPollInterval(interval);
            }

            tmp = getAttrAndResolveAppVar(el, FTPMessage.FTP_USE_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) // blank value will result in default
            // for use attribute - "literal"
            {
                ftpMessage.setUse(tmp.trim());
            }

            ftpMessage.setEncodingStyle(getAttrAndResolveAppVar(el, FTPMessage.FTP_ENCODINGSTYLE_PROPERTY));

            tmp = getAttrAndResolveAppVar(el, FTPMessage.FTP_FILETYPE_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) {
                ftpMessage.setFileType(tmp.trim());
            }

            tmp = getAttrAndResolveAppVar(el, FTPMessage.FTP_FORWARD_AS_ATT_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) {
                ftpMessage.setForwardAsAttachment(tmp.equals("true"));
            }

            tmp = getAttrAndResolveAppVar(el, FTPMessage.FTP_CHARENCODING_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) {
                ftpMessage.setCharacterEncoding(tmp.trim());
            }

            ftpMessage.setPart(getAttrAndResolveAppVar(el, FTPMessage.FTP_PART_PROPERTY));
            ftpMessage.setMessageRepo(getAttrAndResolveAppVar(el, FTPMessage.FTP_MSG_REPO_PROPERTY));
            ftpMessage.setMessageName(getAttrAndResolveAppVar(el, FTPMessage.FTP_SYMETRIC_MSG_NAME_PROPERTY));
            ftpMessage.setMessageNamePrefixIB(getAttrAndResolveAppVar(el, FTPMessage.FTP_SYMETRIC_MSG_NAME_PREFIX_IB_PROPERTY));
            ftpMessage.setMessageNamePrefixOB(getAttrAndResolveAppVar(el, FTPMessage.FTP_SYMETRIC_MSG_NAME_PREFIX_OB_PROPERTY));

            tmp = getAttrAndResolveAppVar(el, FTPMessage.FTP_PROTECT_ENABLED_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) {
                ftpMessage.setProtectEnabled(tmp.equals("true"));
            }

            tmp = getAttrAndResolveAppVar(el, FTPMessage.FTP_ARCHIVE_ENABLED_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) {
                ftpMessage.setArchiveEnabled(tmp.equals("true"));
            }

            tmp = getAttrAndResolveAppVar(el, FTPMessage.FTP_STAGING_ENABLED_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) {
                ftpMessage.setStagingEnabled(tmp.equals("true"));
            }

            tmp = getAttrAndResolveAppVar(el, FTPMessage.FTP_MSG_CORRELATE_ENABLED_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) {
                ftpMessage.setMessageCorrelate(tmp.equals("true"));
            }

            returnValue = ftpMessage;
        } else if (FTPConstants.QNAME_ADDRESS.equals(elementType)) {
            FTPAddress ftpAddress = new FTPAddress();

            ftpAddress.setURL(getAttrAndResolveAppVar(el, FTPAddress.ADDR_URL_PROPERTY));
            ftpAddress.setUser(getAttrAndResolveAppVar(el, FTPAddress.ADDR_USER_PROPERTY));
            ftpAddress.setPassword(getAttrAndResolveAppVar(el, FTPAddress.ADDR_PASSWORD_PROPERTY));

            // new attrs for FTP/TLS
            tmp = getAttrAndResolveAppVar(el, FTPAddress.FTP_SEC_TYPE_PROPERTY);
            // null and empty attr will leave
            // SecureFTPType property with default: which is "None"
            if (tmp != null && tmp.trim().length() > 0) {
                ftpAddress.setSecureFTPType(tmp.trim());
            }

            tmp = getAttrAndResolveAppVar(el, FTPAddress.FTP_ENABLE_CCC_PROPERTY);

            if (tmp != null && tmp.trim().length() > 0) {
                ftpAddress.setEnableCCC(tmp.equals("true"));
            }

            ftpAddress.setKeyStore(getAttrAndResolveAppVar(el, FTPAddress.FTP_KSTOR_PROPERTY));

            ftpAddress.setKeyStorePassword(getAttrAndResolveAppVar(el, FTPAddress.FTP_KSTOR_PASSWD_PROPERTY));

            ftpAddress.setKeyAlias(getAttrAndResolveAppVar(el, FTPAddress.FTP_KEY_ALIAS_PROPERTY));

            ftpAddress.setKeyPassword(getAttrAndResolveAppVar(el, FTPAddress.FTP_KEY_PASSWD_PROPERTY));

            ftpAddress.setTrustStore(getAttrAndResolveAppVar(el, FTPAddress.FTP_TSTOR_PROPERTY));

            ftpAddress.setTrustStorePassword(getAttrAndResolveAppVar(el, FTPAddress.FTP_TSTOR_PASSWD_PROPERTY));

            ftpAddress.setDirListStyle(getAttrAndResolveAppVar(el, FTPAddress.FTP_DIRLSTSTYLE_PROPERTY));

            tmp = getAttrAndResolveAppVar(el, FTPAddress.FTP_USE_UD_HEURISTICS_PROPERTY);
            if (tmp != null && tmp.trim().length() > 0) {
                ftpAddress.setUseUserDefinedHeuristics(tmp.equals("true"));
            }

            ftpAddress.setUserDefDirListStyle(getAttrAndResolveAppVar(el, FTPAddress.FTP_UD_DIRLSTSTYLE_PROPERTY));
            ftpAddress.setUserDefDirListHeuristics(getAttrAndResolveAppVar(el, FTPAddress.FTP_UD_HEURISTICS_PROPERTY));

            tmp = getAttrAndResolveAppVar(el, FTPAddress.FTP_TRANSMODE_PROPERTY);
            // fall back to default "Binary" when user left "Transfer Mode" blank
            if (tmp != null && tmp.trim().length() > 0) {
                ftpAddress.setTransferMode(tmp.trim());
            }

            tmp = getAttrAndResolveAppVar(el, FTPAddress.FTP_CMD_CH_TIMEOUT_PROPERTY);
            // set default timeout 45000 when user left "CmdChannelTimeout" blank
            if (tmp != null && tmp.trim().length() > 0) {
                ftpAddress.setCmdChannelTimeoutStr(tmp.trim());
            }

            tmp = getAttrAndResolveAppVar(el, FTPAddress.FTP_DATA_CH_TIMEOUT_PROPERTY);
            // set default timeout 45000 when user left "DataChannelTimeout" blank
            if (tmp != null && tmp.trim().length() > 0) {
                ftpAddress.setDataChannelTimeoutStr(tmp.trim());
            }

            ftpAddress.setControlChannelEncoding(getAttrAndResolveAppVar(el, FTPAddress.FTP_CNTRL_CH_ENCODING_PROPERTY));
            ftpAddress.setPersistenceBaseLocation(getAttrAndResolveAppVar(el, FTPAddress.FTP_PERSIST_BASE_LOC_PROPERTY));

            returnValue = ftpAddress;
        }

        return returnValue;
    }

    String removeExtraEscapeCharacter(String delim) {
        String returnValue = delim;

        try {
            byte[] returnBytes = new byte[delim.length()];
            byte[] delimBytes = delim.getBytes("UTF-8");  // UTF-8
            int len = delim.length();
            boolean found = false;
            int index = 0;
            for (int ii = 0; ii < len && ii + 1 < len; ii++) {
                if (delimBytes[ii] == '\\') {
                    if (delimBytes[ii + 1] == 'r') {
                        returnBytes[index] = '\r';
                        ii++;
                        index++;
                        found = true;
                        continue;
                    }
                    if (delimBytes[ii + 1] == 'n') {
                        returnBytes[index] = '\n';
                        ii++;
                        index++;
                        found = true;
                        continue;
                    }
                    if (delimBytes[ii + 1] == 't') {
                        returnBytes[index] = '\t';
                        ii++;
                        index++;
                        found = true;
                        continue;
                    }
                    if (delimBytes[ii + 1] == 'b') {
                        returnBytes[index] = '\b';
                        ii++;
                        index++;
                        found = true;
                        continue;
                    }

                    if (delimBytes[ii + 1] == 'f') {
                        returnBytes[index] = '\f';
                        if (++ii >= len) {
                            break;
                        }
                        index++;
                        found = true;
                        continue;
                    }
                } else {
                    returnBytes[index] = delimBytes[ii];
                    index++;
                }
            }
            if (found) {
                returnValue = new String(returnBytes, 0, index, "UTF-8");
            }
        } catch (Exception e) {
            // Only support UTF-8
        }

        return returnValue;
    }

    // before "Application Variable" it is called "Environment Variable"
    public static final boolean hasMigrationEnvVarRef(String attrVal) {
        return mPattern.matcher(attrVal).find();
    }

    public static final Object[] getAppVariableNames(String attrName, String attrVal) throws WSDLException {
        String tokenName = null;
        Matcher m = mPattern.matcher(attrVal);
        Vector refs = new Vector();
        while (m.find()) {
            tokenName = m.group(1);
            if (tokenName == null || tokenName.trim().length() == 0) {
                throw new WSDLException("INVALID_WSDL", mMessages.getString("FTPBC-E001013.FES_Invalid_token_name", tokenName));
            }
            refs.add(tokenName);
        }

        if (attrVal.indexOf("${}") >= 0) {
            throw new WSDLException("INVALID_WSDL", mMessages.getString("FTPBC-E001014.FES_Invalid_empty_token_name", new Object[]{attrVal, attrName}));
        }

        return refs.toArray();
    }

    public static final String resolveVars(String name, String val, Map vars) throws WSDLException {
        String result = val;
        if (result != null && vars != null) {
            if (hasMigrationEnvVarRef(val)) {
                // attribute contains env var reference(s)
                Object[] refs = getAppVariableNames(name, val);
                for (int i = 0; i < refs.length; i++) {
                    String[] varDesc = (String[]) vars.get(refs[i]);
                    if (varDesc == null || varDesc.length != 2) {
                        throw new WSDLException("INVALID_WSDL", mMessages.getString("FTPBC-E001015.FES_Invalid_app_var_ref_no_def", new Object[]{refs[i], val, name}));
                    } else {
                        // check if the de-referenced value has ${ in it
                        String varVal = varDesc[0];
                        if (varVal == null) {
                            throw new WSDLException("INVALID_WSDL",
                                    mMessages.getString("FTPBC-E001060.FES_Invalid_attr_value_NULL", new Object[]{refs[i], name}));
                        }
                        if (varVal.indexOf("${") >= 0) {
                            throw new WSDLException("INVALID_WSDL", mMessages.getString("FTPBC-E001016.FES_Invalid_var_value_contains_var_ref", new Object[]{name, val, refs[i], varVal}));
                        }
                        result = result.replace("${" + refs[i] + "}", varVal);
                    }
                }
            }
        }
        if (result != null && hasMigrationEnvVarRef(result)) {
            // still has ref un-resolved
            throw new WSDLException("INVALID_WSDL", mMessages.getString("FTPBC-E001017.FES_Invalid_attr_value_contains_unresolvable_ref", new Object[]{val, name}));
        }
        return result;
    }

    protected String getAttrAndResolveAppVar(Element el, String attrName) throws WSDLException {
        return resolveVars(attrName, DOMUtils.getAttribute(el, attrName), mAppVariableMap);
    }
}
