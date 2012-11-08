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
 * @(#)FileExtPreprocessDeserializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.extensions;

import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

import com.ibm.wsdl.util.xml.DOMUtils;

import org.w3c.dom.Element;

import com.sun.jbi.filebc.util.WSDLUtilities;
import com.sun.jbi.internationalization.Messages;

import java.util.Map;

/**
 *
 * @author Sherry Weng
 * @author Qian Fu jim.fu@sun.com
 */
public class FileExtPreprocessDeserializer extends FileExtSerializer {

    private static final Messages mMessages = Messages.getMessages(FileExtPreprocessDeserializer.class);

    // default constructor
    public FileExtPreprocessDeserializer() {
        super();
    }

    public FileExtPreprocessDeserializer(Map envVariableMap) {
        super(envVariableMap);
    }

    public javax.wsdl.extensions.ExtensibilityElement unmarshall(Class parentType,
            QName elementType,
            Element el,
            Definition def,
            ExtensionRegistry extReg)
            throws javax.wsdl.WSDLException {
        ExtensibilityElement returnValue = null;
        if (FileConstants.QNAME_MESSAGE.equals(elementType)) {
            FileMessage fileMessage = new FileMessage();

            collectEnvVars(el, ATTR_FILE_USE, FileConstants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_FILE_TYPE, FileConstants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_FILE_ENCODING_STYLE, FileConstants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_FILE_NAME, FileConstants.ENV_VAR_TYPE_STRING);
            String fileNameIsPattern = DOMUtils.getAttribute(el, ATTR_FILE_NAME_IS_PATTERN);
            if (fileNameIsPattern != null) {
                fileMessage.setFileNameIsPattern(Boolean.valueOf(fileNameIsPattern));
            }

            collectEnvVars(el, ATTR_POLLING_INTERVAL, FileConstants.ENV_VAR_TYPE_NUMBER);
            collectEnvVars(el, ATTR_MAX_BYTES_PER_RECORD, FileConstants.ENV_VAR_TYPE_NUMBER);

            String attr = DOMUtils.getAttribute(el, ATTR_ADD_EOL);
            if (attr != null) {
                fileMessage.setAddEOL(Boolean.valueOf(attr));
            }

            attr = DOMUtils.getAttribute(el, ATTR_REMOVE_EOL);
            if (attr != null) {
                fileMessage.setRemoveEOL(Boolean.valueOf(attr));
            }

            attr = DOMUtils.getAttribute(el, ATTR_MULTIPLE_RECORDS_PER_FILE);
            if (attr != null) {
                fileMessage.setMultipleRecordsPerFile(Boolean.valueOf(attr));
            }

            collectEnvVars(el, ATTR_RECORD_DELIM, FileConstants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_PART, FileConstants.ENV_VAR_TYPE_STRING);

            attr = DOMUtils.getAttribute(el, ATTR_PROTECT_ENABLED);
            if (attr != null) {
                fileMessage.setProtect(Boolean.valueOf(attr));
            }
            collectEnvVars(el, ATTR_PROTECT_DIR, FileConstants.ENV_VAR_TYPE_STRING);
            attr = DOMUtils.getAttribute(el, ATTR_ARCHIVE_ENABLED);
            if (attr != null) {
                fileMessage.setArchive(Boolean.valueOf(attr));
            }
            collectEnvVars(el, ATTR_ARCHIVE_DIR, FileConstants.ENV_VAR_TYPE_STRING);
            attr = DOMUtils.getAttribute(el, ATTR_ARCHIVE_ENABLED);
            if (attr != null) {
                fileMessage.setStage(Boolean.valueOf(attr));
            }
            collectEnvVars(el, ATTR_STAGE_DIR, FileConstants.ENV_VAR_TYPE_STRING);
            returnValue = fileMessage;
        } else if (FileConstants.QNAME_ADDRESS.equals(elementType)) {
            FileAddress fileAddress = new FileAddress();

            String relativePath = DOMUtils.getAttribute(el, ATTR_RELATIVE_PATH);
            if (relativePath != null) {
                fileAddress.setRelativePath(Boolean.valueOf(relativePath));
            }

            collectEnvVars(el, ATTR_DIRECTORY, FileConstants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_PATH_RELATIVE_TO, FileConstants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_LOCK_NAME, FileConstants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_WORK_AREA, FileConstants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_SEQ_NAME, FileConstants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_PERSIST_BASE_LOC, FileConstants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_POLL_RECURSIVE_EXCLUDE, FileConstants.ENV_VAR_TYPE_STRING);

            returnValue = fileAddress;
        }

        return returnValue;
    }

    protected void collectEnvVars(Element el, String attrName, String envVarType) throws WSDLException {
        String s = DOMUtils.getAttribute(el, attrName);
        if (s != null) {
            try {
                if (WSDLUtilities.hasMigrationAppVarRef(s)) {
                    String token = s;
                    Object[] envVariableNames = WSDLUtilities.getAppVariableNames(s);
                    if (envVariableNames != null) {
                        for (int i = 0; i < envVariableNames.length; i++) {
                            if (!mEnvVariableMap.containsKey(envVariableNames[i])) {
                                mEnvVariableMap.put(envVariableNames[i], new String[]{null, envVarType});
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
