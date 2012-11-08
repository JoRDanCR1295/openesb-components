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
 * @(#)FileMessage.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.extensions;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 * @author Sherry Weng
 */
public class FileMessage implements ExtensibilityElement, Serializable, Cloneable {

    private static final long serialVersionUID = 1L;
    public static final String FILE_TYPE_TEXT = "text";
    public static final String FILE_TYPE_BINARY = "binary";
    public static final String FILE_USE_TYPE_LITERAL = "literal";
    public static final String FILE_USE_TYPE_ENCODED = "encoded";
    // default working directory name
    public static final String FILE_PROTECT_DIR_DEFAULT = "protect";
    public static final String FILE_ARCHIVE_DIR_DEFAULT = "archive";
    public static final String FILE_STAGING_DIR_DEFAULT = "stage";
    public static final String DELIM_LINE_FEED = "LINE FEED";
    private QName fieldElementType = FileConstants.QNAME_MESSAGE;
    private String fileEncodingStyle;
    private String fileType = FILE_TYPE_TEXT;           // default
    private String fileUseType = FILE_USE_TYPE_LITERAL; // default
    private String fileName;
    private String part;
    private String recordDelimiter;
    private Long maxBytesPerRecord;
    private Long pollingInterval;
    private Boolean fieldRequired = Boolean.FALSE;
    private Boolean addEOL = Boolean.FALSE;
    private Boolean fileNameIsPattern = Boolean.FALSE;
    private Boolean fileNameIsRegex = Boolean.FALSE;
    private Boolean multipleRecordsPerFile = Boolean.FALSE;
    private Boolean removeEOL = Boolean.FALSE;
    private String charEncoding;
    // all pre/post operations are not enabled to be backward compatible 
    // with original file bc behavor
    private Boolean mOverwriteProtectEnabled = Boolean.FALSE;
    private Boolean mArchiveProcessedEnabled = Boolean.TRUE;
    private Boolean mStagingMessageWriteEnabled = Boolean.FALSE;
//    private String mOverwriteProtectDirectory = FILE_PROTECT_DIR_DEFAULT;
    private String mOverwriteProtectDirectory;
    private String mArchiveDirectory = FILE_ARCHIVE_DIR_DEFAULT;
    private String mStagingDirectory = FILE_STAGING_DIR_DEFAULT;
    // relative is the default
    private Boolean mOverwriteProtectDirIsRelative = Boolean.TRUE;
    private Boolean mArchiveDirIsRelative = Boolean.TRUE;
    private Boolean mStagingDirIsRelative = Boolean.TRUE;
    private boolean forwardAsAttachment = Boolean.FALSE;
    private boolean deleteFileOnRead = Boolean.FALSE;

    public FileMessage() {
    }

    /**
     * Set the extensibility element type
     * @param elementType the type
     */
    public void setElementType(QName elementType) {
        fieldElementType = elementType;
    }

    /**
     * Get the extensibility element type
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return fieldElementType;
    }

    /**
     * Set whether required (for wsdl:required)
     */
    public void setRequired(Boolean required) {
        fieldRequired = required;
    }

    /**
     * Get whether required (for wsdl:required)
     */
    public Boolean getRequired() {
        return fieldRequired;
    }

    public void setFileType(String val) {
        fileType = val;
    }

    public String getFileType() {
        return fileType;
    }

    public void setFileUseType(String val) {
        fileUseType = val;
    }

    public String getFileUseType() {
        return fileUseType;
    }

    public void setFileEncodingStyle(String val) {
        fileEncodingStyle = val;
    }

    public String getFileEncodingStyle() {
        return fileEncodingStyle;
    }

    public void setFileName(String val) {
        fileName = val;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileNameIsPattern(Boolean val) {
        fileNameIsPattern = val;
    }

    public Boolean getFileNameIsPattern() {
        return fileNameIsPattern;
    }

    public void setFileNameIsRegex(Boolean val) {
        fileNameIsRegex = val;
    }

    public Boolean getFileNameIsRegex() {
        return fileNameIsRegex;
    }

    public void setPollingInterval(Long val) {
        pollingInterval = val;
    }

    public Long getPollingInterval() {
        return pollingInterval;
    }

    public void setCharacterEncoding(String val) {
        charEncoding = val;
    }

    public String getCharacterEncoding() {
        return charEncoding;
    }

    public void setMaxBytesPerRecord(Long val) {
        maxBytesPerRecord = val;
    }

    public Long getMaxBytesPerRecord() {
        return maxBytesPerRecord;
    }

    public void setRecordDelimiter(String val) {
        if (DELIM_LINE_FEED.equals(val)) {
            val = System.getProperty("line.separator");
        }
        recordDelimiter = val;
    }

    public String getRecordDelimiter() {
        return recordDelimiter;
    }

    public void setAddEOL(Boolean val) {
        addEOL = val;
    }

    public Boolean getAddEOL() {
        return addEOL;
    }

    public void setRemoveEOL(Boolean val) {
        removeEOL = val;
    }

    public Boolean getRemoveEOL() {
        return removeEOL;
    }

    public void setMultipleRecordsPerFile(Boolean val) {
        multipleRecordsPerFile = val;
    }

    public Boolean getMultipleRecordsPerFile() {
        return multipleRecordsPerFile;
    }

    public void setPart(String val) {
        part = val;
    }

    public String getPart() {
        return part;
    }

    public void setProtect(Boolean b) {
        mOverwriteProtectEnabled = b;
    }

    public Boolean getProtect() {
        return mOverwriteProtectEnabled;
    }

    public void setArchive(Boolean b) {
        mArchiveProcessedEnabled = b;
    }

    public Boolean getArchive() {
        return mArchiveProcessedEnabled;
    }

    public void setStage(Boolean b) {
        mStagingMessageWriteEnabled = b;
    }

    public Boolean getStage() {
        return mStagingMessageWriteEnabled;
    }

    public void setProtectDirectory(String s) {
        mOverwriteProtectDirectory = s;
    }

    public String getProtectDirectory() {
        return mOverwriteProtectDirectory;
    }

    public void setProtectDirIsRelative(Boolean b) {
        mOverwriteProtectDirIsRelative = b;
    }

    public Boolean getProtectDirIsRelative() {
        return mOverwriteProtectDirIsRelative;
    }

    public void setArchiveDirectory(String s) {
        mArchiveDirectory = s;
    }

    public String getArchiveDirectory() {
        return mArchiveDirectory;
    }

    public void setArchiveDirIsRelative(Boolean b) {
        mArchiveDirIsRelative = b;
    }

    public Boolean getArchiveDirIsRelative() {
        return mArchiveDirIsRelative;
    }

    public void setStageDirectory(String s) {
        mStagingDirectory = s;
    }

    public String getStageDirectory() {
        return mStagingDirectory;
    }

    public void setStageDirIsRelative(Boolean b) {
        mStagingDirIsRelative = b;
    }

    public Boolean getStageDirIsRelative() {
        return mStagingDirIsRelative;
    }

    @Override
    public FileMessage clone() throws CloneNotSupportedException {
        return (FileMessage) super.clone();
    }

    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nFile Message (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);
        strBuf.append("\nUse=" + fileUseType);
        strBuf.append("\nFile Type=" + fileType);
        strBuf.append("\nEncoding Style=" + fileEncodingStyle);
        strBuf.append("\nFile Name is pattern=" + fileNameIsPattern);
        strBuf.append("\nFile Name is regular expression=" + fileNameIsRegex);
        strBuf.append("\nFile Name=" + fileName);
        strBuf.append("\nPollingInterval=" + ((pollingInterval != null) ? pollingInterval.toString() : "null"));
        strBuf.append("\nMultiple Records Per File=" + ((multipleRecordsPerFile != null) ? multipleRecordsPerFile.toString() : "FALSE"));
        strBuf.append("\nMax Bytes Per Record =" + ((maxBytesPerRecord != null) ? maxBytesPerRecord.toString() : "null"));
        strBuf.append("\nAdd EOL=" + ((addEOL != null) ? addEOL.toString() : "FALSE"));
        strBuf.append("\nRemove EOL=" + ((removeEOL != null) ? removeEOL.toString() : "FALSE"));
        strBuf.append("\nRecord Delimiter=" + recordDelimiter);
        strBuf.append("\nProtect Enabled=" + mOverwriteProtectEnabled);
        strBuf.append("\nProtect Directory=" + mOverwriteProtectDirectory);
        strBuf.append("\nProtect Directory Is Relative=" + mOverwriteProtectDirIsRelative);
        strBuf.append("\nArchive Enabled=" + mArchiveProcessedEnabled);
        strBuf.append("\nArchive Directory=" + mArchiveDirectory);
        strBuf.append("\nArchive Directory Is Relative=" + mArchiveDirIsRelative);
        strBuf.append("\nStage Enabled=" + mStagingMessageWriteEnabled);
        strBuf.append("\nStage Directory=" + mStagingDirectory);
        strBuf.append("\nStage Directory Is Relative=" + mStagingDirIsRelative);

        return strBuf.toString();
    }

    public boolean isForwardAsAttachment() {
        return forwardAsAttachment;
    }

    public void setForwardAsAttachment(boolean forwardAsAttachment) {
        this.forwardAsAttachment = forwardAsAttachment;
    }

    /**
     * @return the deleteOnRead
     */
    public boolean isDeleteFileOnRead() {
        return deleteFileOnRead;
    }

    /**
     * @param deleteOnRead the deleteOnRead to set
     */
    public void setDeleteFileOnRead(boolean deleteOnRead) {
        this.deleteFileOnRead = deleteOnRead;
    }
}
