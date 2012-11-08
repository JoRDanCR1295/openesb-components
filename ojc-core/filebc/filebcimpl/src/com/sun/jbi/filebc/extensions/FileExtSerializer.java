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
 * @(#)FileExtSerializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.extensions;

import java.io.Serializable;
import java.io.PrintWriter;
import java.util.Map;

import com.sun.jbi.internationalization.Messages;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.wsdl.extensions.ExtensibilityElement;
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

import com.sun.jbi.filebc.util.WSDLUtilities;

/**
 *
 * @author Sherry Weng
 * @author Jim Fu
 */
public class FileExtSerializer implements ExtensionSerializer, ExtensionDeserializer, Serializable {

    private static final long serialVersionUID = 1L;
    private static final Messages mMessages = Messages.getMessages(FileExtSerializer.class);
    public static final String ATTR_FILE_TYPE = "fileType";
    public static final String ATTR_FILE_USE = "use";
    public static final String ATTR_FILE_ENCODING_STYLE = "encodingStyle";
    public static final String ATTR_DIRECTORY = "fileDirectory";
    public static final String ATTR_FILE_NAME_IS_PATTERN = "fileNameIsPattern";
    public static final String ATTR_FILE_NAME_IS_REGEX = "fileNameIsRegex";
    public static final String ATTR_FILE_NAME = "fileName";
    public static final String ATTR_CHARACTOR_ENCODING = "charset";
    public static final String ATTR_POLLING_INTERVAL = "pollingInterval";
    public static final String ATTR_REMOVE_EOL = "removeEOL";
    public static final String ATTR_ADD_EOL = "addEOL";
    public static final String ATTR_MULTIPLE_RECORDS_PER_FILE = "multipleRecordsPerFile";
    public static final String ATTR_RECORD_DELIM = "recordDelimiter";
    public static final String ATTR_MAX_BYTES_PER_RECORD = "maxBytesPerRecord";
    public static final String ATTR_PART = "part";
    public static final String ATTR_RELATIVE_PATH = "relativePath";
    public static final String ATTR_PATH_RELATIVE_TO = "pathRelativeTo";
    public static final String ATTR_PROTECT_ENABLED = "protect";
    public static final String ATTR_ARCHIVE_ENABLED = "archive";
    public static final String ATTR_STAGING_ENABLED = "stage";
    public static final String ATTR_PROTECT_DIR = "protectDirectory";
    public static final String ATTR_ARCHIVE_DIR = "archiveDirectory";
    public static final String ATTR_STAGE_DIR = "stageDirectory";
    public static final String ATTR_PROTECT_DIR_IS_RELATIVE = "protectDirIsRelative";
    public static final String ATTR_ARCHIVE_DIR_IS_RELATIVE = "archiveDirIsRelative";
    public static final String ATTR_STAGE_DIR_IS_RELATIVE = "stageDirIsRelative";
    public static final String ATTR_FORWARD_AS_ATTACHMENT = "forwardAsAttachment";
    public static final String ATTR_DELETE_FILE_ON_READ = "deleteFileOnRead";
    // added for clustering and persisted sequence numbering
    public static final String ATTR_LOCK_NAME = "lockName";
    public static final String ATTR_WORK_AREA = "workArea";
    public static final String ATTR_SEQ_NAME = "seqName";
    public static final String ATTR_PERSIST_BASE_LOC = "persistenceBaseLoc";
    // added for polling recursively
    public static final String ATTR_POLL_RECURSIVE = "recursive";
    public static final String ATTR_POLL_RECURSIVE_EXCLUDE = "recursiveExclude";
    // file:operation verb attribute
    public static final String ATTR_VERB = "verb";
    // environment variables
    protected Map mEnvVariableMap;
    /** Creates a new instance of FileExtSerializer */
    public FileExtSerializer() {
    }

    public FileExtSerializer(Map envVariableMap) {
        this();
        mEnvVariableMap = envVariableMap;
    }

    /**
     * Registers the serializers / deserializers
     */
    public void registerSerializer(ExtensionRegistry registry) {
        registry.registerSerializer(Binding.class, FileConstants.QNAME_BINDING, this);
        registry.registerDeserializer(Binding.class, FileConstants.QNAME_BINDING, this);
        registry.mapExtensionTypes(Binding.class, FileConstants.QNAME_BINDING, FileBinding.class);

        registry.registerSerializer(BindingOperation.class, FileConstants.QNAME_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class, FileConstants.QNAME_OPERATION, this);
        registry.mapExtensionTypes(BindingOperation.class, FileConstants.QNAME_OPERATION, FileOperation.class);

        registry.registerSerializer(BindingInput.class, FileConstants.QNAME_MESSAGE, this);
        registry.registerDeserializer(BindingInput.class, FileConstants.QNAME_MESSAGE, this);
        registry.mapExtensionTypes(BindingInput.class, FileConstants.QNAME_MESSAGE, FileMessage.class);

        registry.registerSerializer(BindingOutput.class, FileConstants.QNAME_MESSAGE, this);
        registry.registerDeserializer(BindingOutput.class, FileConstants.QNAME_MESSAGE, this);
        registry.mapExtensionTypes(BindingOutput.class, FileConstants.QNAME_MESSAGE, FileMessage.class);

        registry.registerSerializer(Port.class, FileConstants.QNAME_ADDRESS, this);
        registry.registerDeserializer(Port.class, FileConstants.QNAME_ADDRESS, this);
        registry.mapExtensionTypes(Port.class, FileConstants.QNAME_ADDRESS, FileAddress.class);
    }

    public Map getEnvVariableMap() {
        return mEnvVariableMap;
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

        if (extension instanceof FileBinding) {
            FileBinding fileBinding = (FileBinding) extension;
            pw.print("      <file:binding");
            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            pw.println("/>");
        } else if (extension instanceof FileOperation) {
            FileOperation fileOperation = (FileOperation) extension;
            pw.print("      <file:operation");
            pw.println("/>");
        } else if (extension instanceof FileMessage) {
            FileMessage fileMessage = (FileMessage) extension;
            pw.print("      <file:message");
            if (fileMessage.getFileUseType() != null) {
                DOMUtils.printAttribute(ATTR_FILE_USE,
                        fileMessage.getFileUseType(),
                        pw);
            }

            if (fileMessage.getFileType() != null) {
                DOMUtils.printAttribute(ATTR_FILE_TYPE,
                        fileMessage.getFileType(),
                        pw);
            }

            if (fileMessage.getFileEncodingStyle() != null) {
                DOMUtils.printAttribute(ATTR_FILE_ENCODING_STYLE,
                        fileMessage.getFileEncodingStyle(),
                        pw);
            }

            if (fileMessage.getFileName() != null) {
                DOMUtils.printAttribute(ATTR_FILE_NAME,
                        fileMessage.getFileName(),
                        pw);
            }

            if (fileMessage.getFileNameIsPattern() != null) {
                DOMUtils.printAttribute(ATTR_FILE_NAME_IS_PATTERN,
                        fileMessage.getFileNameIsPattern().toString(),
                        pw);
            }

            if (fileMessage.getFileNameIsRegex() != null) {
                DOMUtils.printAttribute(ATTR_FILE_NAME_IS_REGEX,
                        fileMessage.getFileNameIsRegex().toString(),
                        pw);
            }

            if (fileMessage.getPollingInterval() != null) {
                DOMUtils.printAttribute(ATTR_POLLING_INTERVAL,
                        fileMessage.getPollingInterval().toString(),
                        pw);
            }

            if (fileMessage.getMaxBytesPerRecord() != null) {
                DOMUtils.printAttribute(ATTR_MAX_BYTES_PER_RECORD,
                        fileMessage.getMaxBytesPerRecord().toString(),
                        pw);
            }

            if (fileMessage.getAddEOL() != null) {
                DOMUtils.printAttribute(ATTR_ADD_EOL,
                        fileMessage.getAddEOL().toString(),
                        pw);
            }

            if (fileMessage.getRemoveEOL() != null) {
                DOMUtils.printAttribute(ATTR_REMOVE_EOL,
                        fileMessage.getRemoveEOL().toString(),
                        pw);
            }

            if (fileMessage.getMultipleRecordsPerFile() != null) {
                DOMUtils.printAttribute(ATTR_MULTIPLE_RECORDS_PER_FILE,
                        fileMessage.getMultipleRecordsPerFile().toString(),
                        pw);
            }

            if (fileMessage.getRecordDelimiter() != null) {
                DOMUtils.printAttribute(ATTR_RECORD_DELIM,
                        fileMessage.getRecordDelimiter(),
                        pw);
            }

            if (fileMessage.getPart() != null) {
                DOMUtils.printAttribute(ATTR_PART,
                        fileMessage.getPart(),
                        pw);
            }

            if (fileMessage.getProtect() != null) {
                DOMUtils.printAttribute(ATTR_PROTECT_ENABLED,
                        fileMessage.getProtect().toString(),
                        pw);
            }

            if (fileMessage.getProtectDirectory() != null) {
                DOMUtils.printAttribute(ATTR_PROTECT_DIR,
                        fileMessage.getProtectDirectory(),
                        pw);
            }

            if (fileMessage.getProtectDirIsRelative() != null) {
                DOMUtils.printAttribute(ATTR_PROTECT_DIR_IS_RELATIVE,
                        fileMessage.getProtectDirIsRelative().toString(),
                        pw);
            }

            if (fileMessage.getArchive() != null) {
                DOMUtils.printAttribute(ATTR_ARCHIVE_ENABLED,
                        fileMessage.getArchive().toString(),
                        pw);
            }

            if (fileMessage.getArchiveDirectory() != null) {
                DOMUtils.printAttribute(ATTR_ARCHIVE_DIR,
                        fileMessage.getArchiveDirectory(),
                        pw);
            }

            if (fileMessage.getArchiveDirIsRelative() != null) {
                DOMUtils.printAttribute(ATTR_ARCHIVE_DIR_IS_RELATIVE,
                        fileMessage.getArchiveDirIsRelative().toString(),
                        pw);
            }

            if (fileMessage.getStage() != null) {
                DOMUtils.printAttribute(ATTR_STAGING_ENABLED,
                        fileMessage.getStage().toString(),
                        pw);
            }

            if (fileMessage.getStageDirectory() != null) {
                DOMUtils.printAttribute(ATTR_STAGE_DIR,
                        fileMessage.getStageDirectory(),
                        pw);
            }

            if (fileMessage.getStageDirIsRelative() != null) {
                DOMUtils.printAttribute(ATTR_STAGE_DIR_IS_RELATIVE,
                        fileMessage.getStageDirIsRelative().toString(),
                        pw);
            }

            if (extension.getRequired() != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED,
                        extension.getRequired().toString(),
                        def,
                        pw);
            }

            pw.println("/>");
        } else if (extension instanceof FileAddress) {
            FileAddress fileAddress = (FileAddress) extension;
            pw.print("      <file:address");

            if (fileAddress.getRelativePath() != null) {
                DOMUtils.printAttribute(ATTR_RELATIVE_PATH,
                        fileAddress.getRelativePath().toString(),
                        pw);
            }

            if (fileAddress.getFileDirectory() != null) {
                DOMUtils.printAttribute(ATTR_DIRECTORY,
                        fileAddress.getFileDirectory(),
                        pw);
            }

            if (fileAddress.getPathRelativeTo() != null) {
                DOMUtils.printAttribute(ATTR_PATH_RELATIVE_TO,
                        fileAddress.getPathRelativeTo(),
                        pw);
            }

            if (fileAddress.getLockName() != null) {
                DOMUtils.printAttribute(ATTR_LOCK_NAME,
                        fileAddress.getLockName(),
                        pw);
            }

            if (fileAddress.getWorkArea() != null) {
                DOMUtils.printAttribute(ATTR_WORK_AREA,
                        fileAddress.getWorkArea(),
                        pw);
            }

            if (fileAddress.getSeqName() != null) {
                DOMUtils.printAttribute(ATTR_SEQ_NAME,
                        fileAddress.getSeqName(),
                        pw);
            }

            if (fileAddress.getPersistenceBaseLoc() != null) {
                DOMUtils.printAttribute(ATTR_PERSIST_BASE_LOC,
                        fileAddress.getPersistenceBaseLoc(),
                        pw);
            }

            if (fileAddress.getRecursive() != null) {
                DOMUtils.printAttribute(ATTR_POLL_RECURSIVE,
                        fileAddress.getRecursive().toString(),
                        pw);
            }

            if (fileAddress.getExcludeRegex() != null && fileAddress.getExcludeRegex().trim().length() > 0) {
                DOMUtils.printAttribute(ATTR_POLL_RECURSIVE_EXCLUDE,
                        fileAddress.getExcludeRegex(),
                        pw);
            }

            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED,
                        required.toString(),
                        def,
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
        if (FileConstants.QNAME_BINDING.equals(elementType)) {
            FileBinding fileBinding = new FileBinding();
            returnValue = fileBinding;
        } else if (FileConstants.QNAME_OPERATION.equals(elementType)) {
            FileOperation fileOperation = new FileOperation();
            fileOperation.setVerb(DOMUtils.getAttribute(el, ATTR_VERB));
            returnValue = fileOperation;
        } else if (FileConstants.QNAME_MESSAGE.equals(elementType)) {
            FileMessage fileMessage = new FileMessage();

            String attr = getAttrAndResolveEnvVar(el, ATTR_FILE_USE);

            if (attr != null && attr.trim().length() > 0) {
                fileMessage.setFileUseType(attr);
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_FILE_TYPE);

            if (attr != null && attr.trim().length() > 0) {
                fileMessage.setFileType(attr);
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_FILE_ENCODING_STYLE);

            if (attr != null && attr.trim().length() > 0) {
                fileMessage.setFileEncodingStyle(attr);
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_FILE_NAME);

            if (attr != null && attr.trim().length() > 0) {
                fileMessage.setFileName(attr);
            }

            String fileNameIsPattern = DOMUtils.getAttribute(el, ATTR_FILE_NAME_IS_PATTERN);
            if (fileNameIsPattern != null && fileNameIsPattern.trim().length() > 0) {
                fileMessage.setFileNameIsPattern(Boolean.valueOf(fileNameIsPattern));
            }

            String fileNameIsRegex = DOMUtils.getAttribute(el, ATTR_FILE_NAME_IS_REGEX);
            if (fileNameIsRegex != null && fileNameIsRegex.trim().length() > 0) {
                fileMessage.setFileNameIsRegex(Boolean.valueOf(fileNameIsRegex));
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_POLLING_INTERVAL);

            if (attr != null && attr.trim().length() > 0) {
                try {
                    fileMessage.setPollingInterval(new Long(attr));
                } catch (NumberFormatException e) {
                    throw new WSDLException("INVALID_WSDL",
                            mMessages.getString("FILEBC-E00221.FES_Invalid_number_value", new Object[]{attr, ATTR_POLLING_INTERVAL}));
                } catch (Exception e) {
                    throw new WSDLException("INVALID_WSDL", e.getMessage());
                }
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_CHARACTOR_ENCODING);

            if (attr != null && attr.trim().length() > 0) {
                // The escape character '\' is always
                // escaped by the DOM parser.
                // We are removing the extra escape character here for now
                // until it becomes more clear why that is the case.
                attr = removeExtraEscapeCharacter(attr);
                fileMessage.setCharacterEncoding(attr);
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_MAX_BYTES_PER_RECORD);

            if (attr != null && attr.trim().length() > 0) {
                try {
                    fileMessage.setMaxBytesPerRecord(new Long(attr));
                } catch (NumberFormatException e) {
                    throw new WSDLException("INVALID_WSDL",
                            mMessages.getString("FILEBC-E00221.FES_Invalid_number_value", new Object[]{attr, ATTR_MAX_BYTES_PER_RECORD}));
                } catch (Exception e) {
                    throw new WSDLException("INVALID_WSDL", e.getMessage());
                }
            }

            String addEOL = DOMUtils.getAttribute(el, ATTR_ADD_EOL);
            if (addEOL != null && addEOL.trim().length() > 0) {
                fileMessage.setAddEOL(Boolean.valueOf(addEOL));
            }

            String removeEOL = DOMUtils.getAttribute(el, ATTR_REMOVE_EOL);
            if (removeEOL != null && removeEOL.trim().length() > 0) {
                fileMessage.setRemoveEOL(Boolean.valueOf(removeEOL));
            }

            String multipleRecordsPerFile = DOMUtils.getAttribute(el, ATTR_MULTIPLE_RECORDS_PER_FILE);
            if (multipleRecordsPerFile != null && multipleRecordsPerFile.trim().length() > 0) {
                fileMessage.setMultipleRecordsPerFile(Boolean.valueOf(multipleRecordsPerFile));
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_RECORD_DELIM);

            if (attr != null && attr.trim().length() > 0) {
                // The escape character '\' is always
                // escaped by the DOM parser.
                // We are removing the extra escape character here for now
                // until it becomes more clear why that is the case.
                attr = removeExtraEscapeCharacter(attr);
                fileMessage.setRecordDelimiter(attr);
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_PART);

            if (attr != null) {
                fileMessage.setPart(attr);
            }

            attr = DOMUtils.getAttribute(el, ATTR_PROTECT_ENABLED);
            if (attr != null && attr.trim().length() > 0) {
                fileMessage.setProtect(Boolean.valueOf(attr));
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_PROTECT_DIR);
            if (attr != null && attr.trim().length() > 0) {
                fileMessage.setProtectDirectory(attr);
            }

            attr = DOMUtils.getAttribute(el, ATTR_PROTECT_DIR_IS_RELATIVE);
            if (attr != null && attr.trim().length() > 0) {
                fileMessage.setProtectDirIsRelative(Boolean.valueOf(attr));
            }

            attr = DOMUtils.getAttribute(el, ATTR_ARCHIVE_ENABLED);
            if (attr != null && attr.trim().length() > 0) {
                fileMessage.setArchive(Boolean.valueOf(attr));
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_ARCHIVE_DIR);
            if (attr != null && attr.trim().length() > 0) {
                fileMessage.setArchiveDirectory(attr);
            }

            attr = DOMUtils.getAttribute(el, ATTR_ARCHIVE_DIR_IS_RELATIVE);
            if (attr != null && attr.trim().length() > 0) {
                fileMessage.setArchiveDirIsRelative(Boolean.valueOf(attr));
            }

            attr = DOMUtils.getAttribute(el, ATTR_STAGING_ENABLED);
            if (attr != null && attr.trim().length() > 0) {
                fileMessage.setStage(Boolean.valueOf(attr));
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_STAGE_DIR);
            if (attr != null && attr.trim().length() > 0) {
                fileMessage.setStageDirectory(attr);
            }

            attr = DOMUtils.getAttribute(el, ATTR_STAGE_DIR_IS_RELATIVE);
            if (attr != null && attr.trim().length() > 0) {
                fileMessage.setStageDirIsRelative(Boolean.valueOf(attr));
            }

            attr = DOMUtils.getAttribute(el, ATTR_FORWARD_AS_ATTACHMENT);
            if (attr != null && attr.trim().length() > 0) {
                fileMessage.setForwardAsAttachment(Boolean.valueOf(attr));
            }

            attr = DOMUtils.getAttribute(el, ATTR_DELETE_FILE_ON_READ);
            if (attr != null && attr.trim().length() > 0) {
                fileMessage.setDeleteFileOnRead(Boolean.valueOf(attr));
            }

            returnValue = fileMessage;
        } else if (FileConstants.QNAME_ADDRESS.equals(elementType)) {
            FileAddress fileAddress = new FileAddress();

            String relativePath = DOMUtils.getAttribute(el, ATTR_RELATIVE_PATH);

            if (relativePath != null && relativePath.trim().length() > 0) {
                fileAddress.setRelativePath(Boolean.valueOf(relativePath));
            }

            String attr = getAttrAndResolveEnvVar(el, ATTR_DIRECTORY);

            if (attr != null && attr.trim().length() > 0) {
                fileAddress.setFileDirectory(attr);
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_PATH_RELATIVE_TO);

            if (attr != null && attr.trim().length() > 0) {
                fileAddress.setPathRelativeTo(attr);
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_LOCK_NAME);

            if (attr != null && attr.trim().length() > 0) {
                // when lockName is not explicitly specified - should assume default
                // fileAddress already has default when constructed
                fileAddress.setLockName(attr);
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_WORK_AREA);

            if (attr != null && attr.trim().length() > 0) {
                fileAddress.setWorkArea(attr);
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_SEQ_NAME);

            if (attr != null && attr.trim().length() > 0) {
                fileAddress.setSeqName(attr);
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_PERSIST_BASE_LOC);

            if (attr != null && attr.trim().length() > 0) {
                fileAddress.setPersistenceBaseLoc(attr);
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_POLL_RECURSIVE);

            if (attr != null && attr.trim().length() > 0) {
                fileAddress.setRecursive(Boolean.valueOf(attr.trim().toLowerCase()));
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_POLL_RECURSIVE_EXCLUDE);

            if (attr != null && attr.trim().length() > 0) {
                fileAddress.setExcludeRegex(attr.trim());
            }

            returnValue = fileAddress;
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
    protected String getAttrAndResolveEnvVar(Element el, String attrName) throws WSDLException {
        String attrVal = DOMUtils.getAttribute(el, attrName);
        if (attrVal != null) {
            if (attrVal.indexOf("${}") >= 0) {
                throw new WSDLException("INVALID_WSDL", mMessages.getString("FILEBC-E00222.FES_Invalid_empty_token_name", new Object[]{attrVal, attrName}));
            }
            try {
                if (WSDLUtilities.hasMigrationAppVarRef(attrVal)) {
                    // attribute contains app var reference(s)
                    Object[] vars = WSDLUtilities.getAppVariableNames(attrVal);
                    if (vars != null) {
                        for (int i = 0; i < vars.length; i++) {
                            String[] varDesc = (String[]) mEnvVariableMap.get(vars[i]);
                            if (varDesc == null || varDesc.length != 2) {
                                throw new WSDLException("INVALID_WSDL",
                                        mMessages.getString("FILEBC-E00223.FES_Invalid_env_var_ref_no_def", new Object[]{vars[i], attrVal, attrName}));
                            } else {
                                // check if the de-referenced value has ${ in it
                                String varVal = varDesc[0];
                                if (varVal == null) {
                                    throw new WSDLException("INVALID_WSDL",
                                            mMessages.getString("FILEBC-E00224.FES_Invalid_env_var_value_null", new Object[]{vars[i], attrName}));
                                }
                                if (varVal.indexOf("${") >= 0) {
                                    throw new WSDLException("INVALID_WSDL",
                                            mMessages.getString("FILEBC-E00225.FES_Invalid_var_value_contains_var_ref", new Object[]{attrName, attrVal, vars[i], varVal}));
                                }
                                attrVal = attrVal.replace("${" + vars[i] + "}", varVal);
                            }
                        }
                    } else {
                        // detected app var reference, but extracted non from the attr value
                    }
                    if (WSDLUtilities.hasMigrationAppVarRef(attrVal)) {
                        // still has ref un-resolved
                        throw new WSDLException("INVALID_WSDL",
                                mMessages.getString("FILEBC-E00226.FES_Invalid_attr_value_contains_unresolvable_ref", new Object[]{attrVal, attrName}));
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

}
